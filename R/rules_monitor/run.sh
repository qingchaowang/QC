#!/bin/bash

export DISPLAY=:99

cd ~/GitHub/R/rules_monitor
for dir in $(ls -d */)
do
    cd $dir
    R CMD BATCH run.R
    cd ../
done
   
    
