// import Mill dependency
import mill._
import mill.define.Sources
import mill.modules.Util
import mill.scalalib.TestModule.ScalaTest
import scalalib._
// support BSP
import mill.bsp._
import os._

def scalaVersionString: String = "2.13.10"
def chiselVersionString: String = "3.5.6"

trait SbtScalaModule extends SbtModule {
  def millSourcePath = os.pwd
  def scalaVersion = scalaVersionString
  def scalacOptions = Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
    "-unchecked",
    "-Ymacro-annotations"
  )
  def ivyDeps = T{Agg(
    ivy"org.scala-lang:scala-reflect:$scalaVersionString",
    ivy"org.json4s::json4s-jackson:3.6.6",
    ivy"org.scalatest::scalatest:3.2.0"
)}
}
trait ChiselScalaModule extends SbtScalaModule { m =>
  override def scalacOptions = super.scalacOptions() ++ Seq("-P:chiselplugin:genBundleElements")
  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:$chiselVersionString",
  )
  def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:$chiselVersionString",
  )
  object test extends SbtModuleTests with TestModule.ScalaTest {
    def ivyDeps = m.ivyDeps() ++ Agg(
      ivy"edu.berkeley.cs::chisel-iotesters:2.5.1"
    )
  }
}
object sdf_fft extends ChiselScalaModule {
  def moduleDeps = Seq(
    rocketchip, 
    rocket_dsp_utils
  )
}

object rocketchip extends SbtScalaModule {
  override def millSourcePath = os.pwd / "generators" / "rocket-chip"
  def moduleDeps = Seq(
    hardfloat, rocketMacros, cde
  )
  object rocketMacros extends SbtScalaModule {
    override def millSourcePath = os.pwd / "generators" / "rocket-chip" / "macros"
  }
}
object hardfloat extends ChiselScalaModule {
  override def millSourcePath = os.pwd / "generators" / "rocket-chip" / "hardfloat"
}
object cde extends SbtScalaModule {
  override def millSourcePath = os.pwd / "tools" / "cde"
  def sources = T.sources{
    super.sources() ++ Seq(PathRef(millSourcePath  / "cde" / "src" / "chipsalliance" / "rocketchip"))
  }
}
object rocket_dsp_utils extends SbtScalaModule {
  override def millSourcePath = os.pwd / "tools" / "rocket-dsp-utils"
  def moduleDeps = Seq(
    rocketchip, cde, dsptools
  )
}
object dsptools extends ChiselScalaModule {
  override def millSourcePath = os.pwd / "tools" / "dsptools"
  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:$chiselVersionString",
    ivy"org.scalatest::scalatest:3.2.+",
    ivy"org.typelevel::spire:0.17.0",
    ivy"org.scalanlp::breeze:1.1",
    ivy"junit:junit:4.13",
    ivy"org.scalacheck::scalacheck:1.14.3",
    ivy"edu.berkeley.cs::chisel-iotesters:2.5.1"
  )
}