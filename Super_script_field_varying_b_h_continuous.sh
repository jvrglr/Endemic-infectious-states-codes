#!/bin/bash
rm exe_field_var_h_b_cont.x

#SIR CHECK --> gfortran -static -fbounds-check dranxor2.f M_declarations.f08 M_functions.f08  M_subroutines.f08 main.f08 -o exe.x
#SIR--> 
gfortran -static -O2 dranxor2.f M_declarations.f08 M_functions.f08  M_subroutines.f08 main.f08 -o exe_field_var_h_b_cont.x #SIR

#rm data/field.dat
#echo "#h tf" > data/field.dat
mkdir out
mkdir err
mkdir data
city=("Results")
#alphas=("0")
mkdir ${city}

#R0s=("2.05 2.1 2.15 2.2 2.25 2.3 2.35 2.4 2.45 2.5 2.55 2.6 2.65 2.7 2.75 2.8 2.85 2.9 2.95 3.0")
#R0s=("0.0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.75 0.8 0.85 0.9 0.95 1.0 1.05 1.1 1.15 1.2 1.25 1.3 1.35 1.4 1.45 1.5 1.55 1.6 1.65 1.7 1.75 1.8 1.85 1.9 1.95 2.0")
R0s=("0.0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.75 0.8 0.85 0.9 0.95 1.0")
#R0s=("1.0 1.05 1.1 1.15 1.2 1.25 1.3 1.35 1.4 1.45 1.5 1.55 1.6 1.65 1.7 1.75 1.8 1.85 1.9 1.95 2.0")

hs=("0.0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.75 0.8 0.85 0.9 0.95 1.0 1.05 1.1 1.15 1.2 1.25 1.3 1.35 1.4 1.45 1.5 1.55 1.6 1.65 1.7 1.75 1.8 1.85 1.9 1.95 2.0")
R0s=("0.8")
hs=("0.2")

realizations=1000	 #Number of realizations to execute. Before was 100
    for R0 in $R0s;do
        for h in $hs;do
            for  ((jjcontrol=1;jjcontrol<=$realizations;jjcontrol++)); do	
                echo "n=" $jjcontrol "xs=" $R0 "h=" $h
                time ./one_iteration_continuous.sh $jjcontrol $h $R0 #For Local
                #run -t 5 -e err/e_b${R0}_h${h}_n${jjcontrol}_noT.err -o out/o_b${R0}_h${h}_n${jjcontrol}_noT.out ./one_iteration_continuous.sh $jjcontrol $h $R0
            done
            #python Extract_plot_data.py $L2
        done
    done

