#!/bin/bash

make
clear

pass=0
failed=0

for filename in good/*.lat; do    
    ./latc $filename > tmp.txt

    if [[ $? != 0 ]]
    then
        echo "Failed:"
        echo $filename
        printf  "\n\n\n"
        ((failed++))
    else
        name=$(echo "$filename" | cut -f 1 -d '.')

        lli "$name.ll" > tmp.output
        cmp --silent "$name.output" tmp.output
        
        if [[ $? != 0 ]]
        then
            echo "---- $name -----"
            echo "------ Expected: ------"
            cat "$name.output"
            echo "-------- Got: ---------"
            cat "tmp.output"
            echo "-----------------------"
            printf  "\n\n\n"
            ((failed++))
        else
            ((pass++))
        fi
    fi

    rm -f "$name.ll"
    rm -f "$name.bc"
    rm -f tmp.output  
    rm -f tmp.txt
done


echo "--------------- Results ---------------"
echo "Pass:     $pass"
echo "Failed:   $failed"