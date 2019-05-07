#!/bin/bash

cd ~/GitHub/R/data_extractors
for dir in $(ls -d */)
do
    cd $dir
    R CMD BATCH run.R
    rm run.Rout
    cd ../
done
   
    
