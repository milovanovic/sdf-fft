#!/bin/bash
echo $0
full_path=$(realpath $0)
dir_path=$(dirname $full_path)

target_dir_radix2=$dir_path/generated-rtl/radix2
target_dir_radix22=$dir_path/generated-rtl/radix22

fft_size_array=(32 64 128 256 512 1024)
#fft_size_array=(32)
word_size_array=(8 12 16)
#word_size_array=(16)


generate_verilog_radix2 () {
  bitreverse=${1:-1}
  separate_verilog=${2:-1}

  for fft_size in "${fft_size_array[@]}"
  do
    for width in "${word_size_array[@]}"
    do
      fft_dir_name=$target_dir_radix2/sdffft_size_${fft_size}_width_${width}_bitreverse_${bitreverse}
      cd $dir_path && sbt "runMain fft.SDFFFTApp $fft_dir_name $width $fft_size $bitreverse 2 ${separate_verilog}"
      if [ -d $fft_dir_name ]; then mv $dir_path/mem.conf $fft_dir_name/fft_mem.conf;fi
    done
  done
}


generate_verilog_radix22 () {
  bitreverse=${1:-1}
  separate_verilog=${2:-1}

  for fft_size in "${fft_size_array[@]}"
  do
    for width in "${word_size_array[@]}"
    do
      fft_dir_name=$target_dir_radix22/sdffft_size_${fft_size}_width_${width}_bitreverse_${bitreverse}
      cd $dir_path && sbt "runMain fft.SDFFFTApp $fft_dir_name  $width $fft_size $bitreverse 2^2 ${separate_verilog}"
      if [ -d $fft_dir_name  ]; then mv $dir_path/mem.conf $fft_dir_name/fft_mem.conf;fi
    done
  done
}
"$@"



