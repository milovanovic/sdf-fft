package dspblocks.utils.common

import chisel3._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

class SplitterAND2(beatBytes: Int) extends LazyModule()(Parameters.empty) {

  val streamNode = AXI4StreamNexusNode(
    masterFn = { seq =>
      seq.reduce({ (a: AXI4StreamMasterPortParameters, b: AXI4StreamMasterPortParameters) =>
        AXI4StreamMasterPortParameters(a.masterParams.union(b.masterParams))
      })
    },
    slaveFn = { seq =>
      seq.reduce({ (_: AXI4StreamSlavePortParameters, b: AXI4StreamSlavePortParameters) =>
        AXI4StreamSlavePortParameters(b.slaveParams.union(b.slaveParams))
      })
    }
  )
  //val streamNode = AXI4StreamIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.in.length == 1, "Only one input to splitter allowed")
    require(
      streamNode.out.length <= beatBytes * 4,
      "Number of outputs should be equal or smaller then number of bits in mask register"
    )

    val (in, _) = streamNode.in.head

    val outs = streamNode.out.map { case (out, edge) => out.ready }
    val readyAND = outs.reduce(_ && _)
    val ready = Wire(Bool())
    ready := readyAND
    in.ready := ready

    streamNode.out.zipWithIndex.foreach {
      case ((out, edge), i) =>
        out.valid := in.valid && ready
        out.bits := in.bits
    }
  }
}
