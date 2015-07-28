#!/bin/bash

MAX=0 IFS=
while read -r line
do
    [ ${#line} -le 16 ] && printf '%s\n' "$line"
done < ODS6.txt

