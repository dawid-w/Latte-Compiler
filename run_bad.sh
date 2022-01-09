#!/bin/bash

make

for filename in bad/*.lat; do    
    ./latc_llvm $filename &> tmp.txt
    status=$?    

    if [[ $status = 0 ]]
    then
        echo $filename
    fi 
        
    rm tmp.txt
done