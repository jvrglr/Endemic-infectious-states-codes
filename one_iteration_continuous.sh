#!/bin/bash

jjcontrol=$1
h=$2
R0=$3
echo $jjcontrol $h $R0
#time echo $Pth $R $locks $jjcontrol| ./exe.x #If I add time, the information is on the error files

echo $jjcontrol $h $R0 | ./exe_field_var_h_b_cont.x
