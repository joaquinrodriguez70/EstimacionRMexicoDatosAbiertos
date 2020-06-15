#!/bin/bash
Rscript estimacionR.r
Rscript estimacionRMX.r
#Rscript estimacionRMXSintomas.r
#Rscript estimacionRSintomas.r 
mv *.csv ./backup 
mv *.zip ./backup
