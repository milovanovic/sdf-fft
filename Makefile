# SPDX-License-Identifier: Apache-2.0

base_dir ?= $(abspath .)
build_dir ?= $(base_dir)/generated-rtl

SBT ?= sbt

all: clean gen_rtl

gen_rtl:
	cd $(base_dir) && $(SBT) "runMain fft.SDFFFTApp"
	
.PHONY: clean
clean:
	if [ -d $(build_dir) ]; then cd $(build_dir) && rm -f *.*;fi
