package dspblocks.utils.dsp.windowing

import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import fixedpoint._
import dsptools._
import dsptools.numbers._
import dspblocks.sdffft.{DIFDecimType, DITDecimType, DecimType}

case class WindowingParams[T <: Data](
  protoIQ:     DspComplex[T], // input data type
  numPoints:   Int, // number of window coefficents
  protoWin:    T, // window coefficients data type
  decimType:   DecimType, // use DIT or DIF version, to know how to generate addresses for reading
  runTime:     Boolean, // use run time configurable number of points (include fftSize register)
  numMulPipes: Int, // number of pipeline registers after multiplication operator
  fftDirReg:   Boolean, // include register for defining fft direction (fft or ifft)/ when ifft is enabled passthrough data
  windowFunc:  WindowFunctionType, // when constWindow is set then this parameter denotes constant window function
  // otherwise it represents window function used to initialize SRAM/Block RAM in run-time configurable mode
  memoryFile:  String, // name of the file where window coefficents are stored
  dirName:     String, // directory name where file with windowing coefficents is saved
  constWindow: Boolean, // predefined window function stored in ROM is used, no SRAM/Block RAM
  trimType:    TrimType, // define trim type after multiplication
  windFuncSeq: Option[Seq[WindowFunctionType]]) {}

object WindowingParams {
  def fixed(
    dataWidth:   Int = 16,
    binPoint:    Int = 14,
    numPoints:   Int = 1024,
    decimType:   DecimType = DIFDecimType,
    runTime:     Boolean = true,
    numMulPipes: Int = 1,
    fftDirReg:   Boolean = false,
    windowFunc:  WindowFunctionType = WindowFunctionTypes.None(),
    memoryFile:  String = "",
    dirName:     String = "windowing_coefficents",
    constWindow: Boolean = false,
    trimType:    TrimType = Convergent,
    windFuncSeq: Option[Seq[WindowFunctionType]] = None
  ): WindowingParams[FixedPoint] = {
    val protoIQ = DspComplex(FixedPoint(dataWidth.W, binPoint.BP))
    val protoWin = FixedPoint(windowFunc.dataWidth.W, (windowFunc.dataWidth - 2).BP)

    WindowingParams(
      numPoints = numPoints,
      protoIQ = protoIQ,
      protoWin = protoWin,
      decimType = decimType,
      runTime = runTime,
      numMulPipes = numMulPipes,
      fftDirReg = fftDirReg,
      windowFunc = windowFunc,
      dirName = dirName,
      memoryFile = memoryFile,
      constWindow = constWindow,
      trimType = trimType,
      windFuncSeq = windFuncSeq
    )
  }
}
