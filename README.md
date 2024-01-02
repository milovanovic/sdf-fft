# SDF-FFT Design Generator
The SDF-FFT Generator is a highly parametrizable Single-Path-Delay-Feedback (SDF) Fast Fourier Transform (FFT) hardware accelerator.


## Prerequisites

The following software packages should be installed prior to running this project:
* [sbt](http://www.scala-sbt.org) or
* [mill](https://mill-build.com)
* [Verilator](http://www.veripool.org/wiki/verilator)

## Setup

Proposed design generator is intended to be used inside [chipyard](https://github.com/ucb-bar/chipyard) environment as one of the generators located inside `generators/dsp-blocks`. Anyhow, if you want to use this repository standalone then follow instructions below:

*  Clone this repository.
*  Switch directory.
*  Initialize all tools and submodules.
*  Compile code, generate verilog or run tests.

```
git clone --recurse-submodules https://github.com/milovanovic/sdf-fft.git
cd sdf-fft
sbt test or ./mill sdf_fft.test
``

## Documentation

* doc/sdf_fft_generator.md - detailed documentation about design generator
* doc/images - design block diagrams, SQNR analysis graphs

Much more useful information about this work can be found inside ["A Highly Parametrizable Chisel HCL Generator of Single-Path Delay Feedback FFT Processors"](https://ieeexplore.ieee.org/document/8889581) paper published on International Conference on Microelectronics, MIEL 2019.

If you are using SDF-FFT generator for research, please cite it by the following publication:

    @INPROCEEDINGS{sdffft,
      author={Milovanović, V. M. and Petrović, M. L.},
      booktitle={2019 IEEE 31st International Conference on Microelectronics (MIEL)},
      title={A Highly Parametrizable Chisel HCL Generator of Single-Path Delay Feedback FFT Processors},
      year={2019},
      pages={247-250},
      doi={10.1109/MIEL.2019.8889581}}

## Guide For New Contributors

If you are trying to make a contribution to this project, please guide following:
1. You can contribute by submitting pull requests from your fork to the upstream repository.
2. If you need help on making a pull request, follow this [guide](https://docs.github.com/en/github/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests).
3. To understand how to compile and test from the source code, follow the instructions inside setup section.

