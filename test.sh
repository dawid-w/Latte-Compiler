#!/bin/bash

make

pass=0
failed=0

cd lib &&  clang -c -emit-llvm runtime.c && cd ..

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

        llvm-as "$name.ll"
        llvm-link -o out.bc "$name.bc" lib/runtime.bc
        
        if test -f "$name.input"; then
            lli out.bc < "$name.input" > tmp.output
        else
            lli out.bc > tmp.output
        fi

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

        rm -f "$name.ll"
        rm -f "$name.bc"
    fi

    rm -f "$name.ll"
    rm -f "$name.bc"
    rm -f tmp.output  
    rm -f tmp.txt
    rm -f out.bc
done

make clean

echo "--------------- Results ---------------"
echo "Pass:     $pass"
echo "Failed:   $failed"

