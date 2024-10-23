#!/usr/bin/env bash
# from https://sharats.me/posts/shell-script-best-practices/

set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]]; then
    set -o xtrace
fi
# https://gitlab.com/risingprismtv/single-gpu-passthrough/-/wikis/6)-Preparation-and-placing-of-the-ROM-file

# sudo rmmod nvidia_uvm
# sudo rmmod nvidia_drm
# sudo rmmod nvidia_modeset
# sudo rmmod nvidia
sudo rmmod i2c_nvidia_gpu


sudo nvflash --save vbios.rom

sudo modprobe nvidia
sudo modprobe nvidia_uvm
sudo modprobe nvidia_drm
sudo modprobe nvidia_modeset
sudo modprobe i2c_nvidia_gpu
