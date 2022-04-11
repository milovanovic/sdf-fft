
base_dir ?= $(abspath .)
target_dir_radix2 ?= $(base_dir)/generated-rtl/radix2
target_dir_radix22 ?= $(base_dir)/generated-rtl/radix22

target_list = $(target_dir_radix2) $(target_dir_radix22)

SBT ?= sbt
all: clean gen_all_single_file clean_fir_json

gen_all_single_file: clean gen_rtl_radix2_single_v_file gen_rtl_radix22_single_v_file clean_fir_json

gen_all_multiple_files:  clean gen_rtl_radix2_multiple_v_files gen_rtl_radix22_multiple_v_files clean_fir_json

gen_rtl_radix2_single_v_file:
	bash generate_verilog.sh generate_verilog_radix2 0 0
	bash generate_verilog.sh generate_verilog_radix2 1 0

gen_rtl_radix2_multiple_v_files:
	bash generate_verilog.sh generate_verilog_radix2 0 1
	bash generate_verilog.sh generate_verilog_radix2 1 1

gen_rtl_radix22_single_v_file:
	bash generate_verilog.sh generate_verilog_radix22 0 0
	bash generate_verilog.sh generate_verilog_radix22 1 0

gen_rtl_radix22_multiple_v_files:
	bash generate_verilog.sh generate_verilog_radix22 0 1
	bash generate_verilog.sh generate_verilog_radix22 1 1

clean_fir_json:
	for target_dir in $(target_list); do if [ -d "$$target_dir" ]; then cd "$$target_dir" && rm  -f **/*.fir **/*.anno.json;fi done

.PHONY: clean
clean:
	for target_dir in $(target_list); do if [ -d "$$target_dir" ]; then cd "$$target_dir" && rm  -f **/*.*;fi done
