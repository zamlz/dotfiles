#!/bin/sh

# This script is simply to disable the Address Space Layout Randomization
# present in the kernel. It should be run as ROOT

# This is known to help AMD Ryzen CPUs stop segfaulting
# in large parallel processes

# The script itself will only attempt at runtime,
# but a permanent solution is commented below

# To disable it at runtime
echo 0 > /proc/sys/kernel/randomize_va_space

# To permanently disable it
# Add the following line to /etc/sysctl.conf
#
#   kernel.randomize_va_space = 0
#
