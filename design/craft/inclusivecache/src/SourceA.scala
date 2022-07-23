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
import freechips.rocketchip.tilelink._

// debugging:
import freechips.rocketchip.subsystem.ExtMem

class SourceARequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val tag    = UInt(width = params.tagBits)
  val set    = UInt(width = params.setBits)
  val param  = UInt(width = 3)
  val source = UInt(width = params.outer.bundle.sourceBits)
  val block  = Bool()
  val blindmask_phase = Bool()
}

class SourceA(params: InclusiveCacheParameters) extends Module
{
  val io = new Bundle {
    val req = Decoupled(new SourceARequest(params)).flip
    val a = Decoupled(new TLBundleA(params.outer.bundle))
  }

  // ready must be a register, because we derive valid from ready
  require (!params.micro.outerBuf.a.pipe && params.micro.outerBuf.a.isDefined)

  val a = Wire(io.a)
  io.a <> params.micro.outerBuf.a(a)

  io.req.ready := a.ready
  a.valid := io.req.valid
  params.ccover(a.valid && !a.ready, "SOURCEA_STALL", "Backpressured when issuing an Acquire")

  a.bits.opcode  := Mux(io.req.bits.block, TLMessages.AcquireBlock, TLMessages.AcquirePerm)
  a.bits.param   := io.req.bits.param
  a.bits.size    := Mux(io.req.bits.blindmask_phase, UInt(params.offsetBits), UInt(params.offsetBits)) // FIXME size hack
  a.bits.source  := io.req.bits.source
  a.bits.address := Mux(io.req.bits.blindmask_phase, params.expandBlindmaskAddr(io.req.bits.tag, io.req.bits.set, UInt(0)), params.expandDataAddr(io.req.bits.tag, io.req.bits.set, UInt(0)))
  a.bits.mask    := ~UInt(0, width = params.outer.manager.beatBytes)
  a.bits.data    := UInt(0)

  // debugging:
  when (a.fire) {
    val orig_addr = params.expandAddress(io.req.bits.tag, io.req.bits.set, UInt(0))
    when (params.withinMainMem(orig_addr)) {
      val blindmaskAddr = params.expandBlindmaskAddr(io.req.bits.tag, io.req.bits.set, UInt(0))
      // printf("A withinMainMem true: orig_addr = %x, blindmaskAddr = %x\n", orig_addr, blindmaskAddr)
      printf("A %x %x %x | orig_addr = %x, blindmaskAddr = %x\n", io.req.bits.blindmask_phase, a.bits.address, a.bits.size, orig_addr, blindmaskAddr)
    } .otherwise {
      printf("A withinMainMem false: orig_addr = %x\n", orig_addr)
      printf("\tExtMem-base = %x, ExtMem-size = %x\n", UInt(params.p(ExtMem).get.master.base), UInt(params.p(ExtMem).get.master.size))
    }
  }
}
