
# A Collection of Digital Signal Processing Block Generators

This repository contains a variety of Digital Signal Processing (DSP) accelerators designed in a configurable and modular fashion. Currently available are listed below:
* **sdf-fft** - A Single-Path-Delay-Feedback (SDF) Fast Fourier Transform (FFT) hardware accelerators

## Prerequisites

The following software packages should be installed prior to running this project:
* [sbt](http://www.scala-sbt.org)
* [Verilator](http://www.veripool.org/wiki/verilator)

## Documentation

To access detailed documentation for each design generator, please refer to the directory `doc/name_of_the_generator/` (e.g. `doc/sdf-fft`) .

## Setup

This project is intended to be used and run inside [chipyard](https://github.com/ucb-bar/chipyard) environment. However, if you wish to use this repository independently, please adhere to the instructions below:

* Clone the repository
* Navigate to the directory
* Initialize all necessary tools and submodules
* Compile the code, generate Verilog, or execute tests
```
git clone https://github.com/ucb-bar/dsp-blocks.git
cd dsp-blocks
./scripts/init_submodules_and_build_sbt.sh
sbt compile
```

#### Note

The script `init_submodules_and_build_sbt.sh` is responsible for setting up all necessary tools and generators essential for running this project. Additionally, it configures `build.sbt` with accurately defined dependencies, aligning with the `chipyard 1.10.1` release. Users have the flexibility to modify versions by adjusting the corresponding checkout commits within the same script.

Conversely, the script `remove_submodules.sh` executes commands that reverse the actions performed by `init_submodules_and_build_sbt.sh`.

Please have in mind that modifying the commit may result in potential issues related to tool dependencies.

## Guide For New Contributors

If you are trying to make a contribution to this project, please guide following:
1. You can contribute by submitting pull requests from your fork to the upstream repository.
2. If you need help on making a pull request, follow this [guide](https://docs.github.com/en/github/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests).
3. To understand how to compile and test from the source code, follow the instructions inside setup section.
