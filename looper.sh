#!/bin/bash
# TODO readline
# rlwrap?
history -r looper.history
echo -n "> "
while read -e INPUT
do
    if [[ $? == 1 ]]
    then
        history -w looper.history
        echo -e "Exiting"
        exit 0
    fi
    history -s "$INPUT"
    echo $INPUT

    echo -n "> "
done