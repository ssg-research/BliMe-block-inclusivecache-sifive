/*
 * Copyright 2019 SiFive, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You should have received a copy of LICENSE.Apache2 along with
 * this software. If not, you may obtain a copy at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sifive.blocks.inclusivecache

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{DescribedSRAM, BlindedMem}

import scala.math.{max, min}

abstract class BankedStoreAddress(val inner: Boolean, params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val noop = Bool() // do not actually use the SRAMs, just block their use
  val way  = UInt(width = params.wayBits)
  val set  = UInt(width = params.setBits)
  val beat = UInt(width = if (inner) params.innerBeatBits else params.outerBeatBits)
  val mask = UInt(width = if (inner) params.innerMaskBits else params.outerMaskBits)
}

trait BankedStoreRW
{
  val write = Bool()
}

class BankedStoreOuterAddress(params: InclusiveCacheParameters) extends BankedStoreAddress(false, params)
{
  val blindmask_phase = Bool()
}
class BankedStoreInnerAddress(params: InclusiveCacheParameters) extends BankedStoreAddress(true, params)
class BankedStoreInnerAddressRW(params: InclusiveCacheParameters) extends BankedStoreInnerAddress(params) with BankedStoreRW

abstract class BankedStoreData(val inner: Boolean, params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val data = if (inner) UInt(width = params.inner.manager.beatBytes*9)
             else       UInt(width = params.outer.manager.beatBytes*8)
}

class BankedStoreOuterData(params: InclusiveCacheParameters) extends BankedStoreData(false, params)
class BankedStoreInnerData(params: InclusiveCacheParameters) extends BankedStoreData(true,  params)
class BankedStoreInnerPoison(params: InclusiveCacheParameters) extends BankedStoreInnerData(params)
class BankedStoreOuterPoison(params: InclusiveCacheParameters) extends BankedStoreOuterData(params)
class BankedStoreInnerDecoded(params: InclusiveCacheParameters) extends BankedStoreInnerData(params)
class BankedStoreOuterDecoded(params: InclusiveCacheParameters) extends BankedStoreOuterData(params)

class BankedStore(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    val sinkC_adr = Decoupled(new BankedStoreInnerAddress(params)).flip
    val sinkC_dat = new BankedStoreInnerPoison(params).flip
    val sinkD_adr = Decoupled(new BankedStoreOuterAddress(params)).flip
    val sinkD_dat = new BankedStoreOuterPoison(params).flip
    val sourceC_adr = Decoupled(new BankedStoreOuterAddress(params)).flip
    val sourceC_dat = new BankedStoreOuterDecoded(params)
    val sourceD_radr = Decoupled(new BankedStoreInnerAddress(params)).flip
    val sourceD_rdat = new BankedStoreInnerDecoded(params)
    val sourceD_wadr = Decoupled(new BankedStoreInnerAddress(params)).flip
    val sourceD_wdat = new BankedStoreInnerPoison(params).flip
  }

  val innerBytes = params.inner.manager.beatBytes
  val outerBytes = params.outer.manager.beatBytes
  val rowBytes = params.micro.portFactor * max(innerBytes, outerBytes)
  require (rowBytes < params.cache.sizeBytes)
  val rowEntries = params.cache.sizeBytes / rowBytes
  val rowBits = log2Ceil(rowEntries)
  val numBanks = rowBytes / params.micro.writeBytes
  val codeBits = 8*params.micro.writeBytes
  require (codeBits % 64 == 0)

  val cc_banks = Seq.tabulate(numBanks) {
    i =>
      DescribedSRAM(
        name = s"cc_banks_$i",
        desc = "Banked Store",
        size = rowEntries,
        data = UInt(width = codeBits)
      )
  }
  val cc_blindmask_banks = Seq.tabulate(numBanks) {
    i =>
      DescribedSRAM(
        name = s"cc_banks_$i",
        desc = "Banked Store",
        size = rowEntries,
        data = UInt(width = codeBits/8)
      )
  }
  // These constraints apply on the port priorities:
  //  sourceC > sinkD     outgoing Release > incoming Grant      (we start eviction+refill concurrently)
  //  sinkC > sourceC     incoming ProbeAck > outgoing ProbeAck  (we delay probeack writeback by 1 cycle for QoR)
  //  sinkC > sourceDr    incoming ProbeAck > SourceD read       (we delay probeack writeback by 1 cycle for QoR)
  //  sourceDw > sourceDr modified data visible on next cycle    (needed to ensure SourceD forward progress)
  //  sinkC > sourceC     inner ProbeAck > outer ProbeAck        (make wormhole routing possible [not yet implemented])
  //  sinkC&D > sourceD*  beat arrival > beat read|update        (make wormhole routing possible [not yet implemented])

  // Combining these restrictions yields a priority scheme of:
  //  sinkC > sourceC > sinkD > sourceDw > sourceDr
  //          ^^^^^^^^^^^^^^^ outer interface

  // Requests have different port widths, but we don't want to allow cutting in line.
  // Suppose we have requests A > B > C requesting ports --A-, --BB, ---C.
  // The correct arbitration is to allow --A- only, not --AC.
  // Obviously --A-, BB--, ---C should still be resolved to BBAC.

  class Request extends Bundle {
    val wen      = Bool()
    val index    = UInt(width = rowBits)
    val bankSel  = UInt(width = numBanks)
    val bankSum  = UInt(width = numBanks) // OR of all higher priority bankSels
    val bankEn   = UInt(width = numBanks) // ports actually activated by request
    val data     = Vec(numBanks, UInt(width = codeBits + codeBits/8))
    val inner    = Bool()
    val blindmask_phase = Bool()
  }

  def req[T <: BankedStoreAddress](b: DecoupledIO[T], write: Bool, d: UInt, blindmask_phase: Bool): Request = {
    val beatBytes = if (b.bits.inner) innerBytes else outerBytes
    val ports = beatBytes / params.micro.writeBytes
    val bankBits = log2Ceil(numBanks / ports)
    val words = Seq.tabulate(ports) { i =>
      val data = d((i + 1) * 8 * params.micro.writeBytes - 1, i * 8 * params.micro.writeBytes)

      if (b.bits.inner) {
        val blindmasks_beat_offset = beatBytes*8
        val blindmask = d((i + 1) * params.micro.writeBytes - 1 + blindmasks_beat_offset, i * params.micro.writeBytes + blindmasks_beat_offset)

        Cat(blindmask, data)
      } else {
        data
      }
    }
    val a = Cat(b.bits.way, b.bits.set, Mux(blindmask_phase, b.bits.beat, b.bits.beat)) // FIXME: size hack: set blindmask transfer size to cacheBlockBytes and change beat here to beat>>3
    val m = b.bits.mask
    val out = Wire(new Request)

    val select = UIntToOH(a(bankBits-1, 0), numBanks/ports)
    val ready  = Cat(Seq.tabulate(numBanks/ports) { i => !(out.bankSum((i+1)*ports-1, i*ports) & m).orR } .reverse)
    b.ready := ready(a(bankBits-1, 0))

    out.wen      := write
    out.index    := a >> bankBits
    out.bankSel  := Mux(b.valid, FillInterleaved(ports, select) & Fill(numBanks/ports, m), UInt(0))
    out.bankEn   := Mux(b.bits.noop, UInt(0), out.bankSel & FillInterleaved(ports, ready))
    Vec(Seq.fill(numBanks/ports) { words }.flatten).zipWithIndex.map { case (w,i) => 
      out.data(i) := w
    }
    out.inner    := b.bits.inner.asBool
    out.blindmask_phase := blindmask_phase

    out
  }

  val innerData = UInt(0, width = innerBytes*9)
  val outerData = UInt(0, width = outerBytes*9)
  val W = Bool(true)
  val R = Bool(false)

  val sinkC_req    = req(io.sinkC_adr,    W, io.sinkC_dat.data    , false.B)
  val sinkD_req    = req(io.sinkD_adr,    W, io.sinkD_dat.data    , io.sinkD_adr.bits.blindmask_phase)
  val sourceC_req  = req(io.sourceC_adr,  R, outerData            , io.sourceC_adr.bits.blindmask_phase)
  val sourceD_rreq = req(io.sourceD_radr, R, innerData            , false.B)
  val sourceD_wreq = req(io.sourceD_wadr, W, io.sourceD_wdat.data , false.B)

  // See the comments above for why this prioritization is used
  val reqs = Seq(sinkC_req, sourceC_req, sinkD_req, sourceD_wreq, sourceD_rreq)

  // Connect priorities; note that even if a request does not go through due to failing
  // to obtain a needed subbank, it still blocks overlapping lower priority requests.
  reqs.foldLeft(UInt(0)) { case (sum, req) =>
    req.bankSum := sum
    req.bankSel | sum
  }
  // Access the banks
  val regout = Vec((cc_banks zip cc_blindmask_banks).zipWithIndex.map { case ((b_data, b_bmask), i) =>
    val en  = reqs.map(_.bankEn(i)).reduce(_||_)
    val sel = reqs.map(_.bankSel(i))
    val wen = PriorityMux(sel, reqs.map(_.wen))
    val idx = PriorityMux(sel, reqs.map(_.index))
    val data= PriorityMux(sel, reqs.map(_.data(i)))
    val inner = PriorityMux(sel, reqs.map(_.inner))
    val blindmask_phase = PriorityMux(sel, reqs.map(_.blindmask_phase))

    val blindedmem_read = Wire(BlindedMem(UInt(),UInt()))
    blindedmem_read.bits := b_data.read(idx, !wen && en)
    blindedmem_read.blindmask := b_bmask.read(idx, !wen && en)

    when (wen && en) {
      when (inner) {
        b_data.write(idx, data(codeBits-1, 0)) 
        b_bmask.write(idx, data(codeBits + codeBits/8 - 1, codeBits)) 
      }
      .otherwise   {
        when (blindmask_phase) { b_bmask.write(idx, data) }
        .otherwise             { b_data.write(idx, data) }
      }
    }
    RegEnable(blindedmem_read, RegNext(!wen && en))
  })

  val regsel_sourceC = RegNext(RegNext(sourceC_req.bankEn))
  val regsel_sourceD = RegNext(RegNext(sourceD_rreq.bankEn))
  val delayed_blindmask_phase_C = RegNext(RegNext(sourceC_req.blindmask_phase))

  val decodeC_bits = regout.zipWithIndex.map {
    case (r, i) => Mux(regsel_sourceC(i), r.bits, UInt(0))
  }.grouped(outerBytes/params.micro.writeBytes).toList.transpose.map(s => s.reduce(_|_))

  val decodeC_blindmask = regout.zipWithIndex.map {
    case (r, i) => Mux(regsel_sourceC(i), r.blindmask, UInt(0))
  }.grouped(outerBytes/params.micro.writeBytes).toList.transpose.map(s => s.reduce(_|_))

  io.sourceC_dat.data := Mux(delayed_blindmask_phase_C, Cat(decodeC_blindmask.reverse), Cat(decodeC_bits.reverse))

  val decodeD_bits = regout.zipWithIndex.map {
    // Intentionally not Mux1H and/or an indexed-mux b/c we want it 0 when !sel to save decode power
    case (r, i) => Mux(regsel_sourceD(i), r.bits, UInt(0))
  }.grouped(innerBytes/params.micro.writeBytes).toList.transpose.map(s => s.reduce(_|_))

  val decodeD_blindmask = regout.zipWithIndex.map {
    // Intentionally not Mux1H and/or an indexed-mux b/c we want it 0 when !sel to save decode power
    case (r, i) => Mux(regsel_sourceD(i), r.blindmask, UInt(0))
  }.grouped(innerBytes/params.micro.writeBytes).toList.transpose.map(s => s.reduce(_|_))

  io.sourceD_rdat.data := Cat(Cat(decodeD_blindmask.reverse), Cat(decodeD_bits.reverse))

  private def banks = cc_banks.map("\"" + _.pathName + "\"").mkString(",")
  def json: String = s"""{"widthBytes":${params.micro.writeBytes},"mem":[${banks}]}"""
}
