#!/usr/bin/env sh

get_program() {
    IFS=':'
    for path in $PATH; do
        ls $path 2> /dev/null | while read -r program; do
            echo $path/$program;
        done;
    done | fzf --reverse --prompt "Program Launcher: "
}

program=$(get_program)
if [ -n "${program}" ]; then
    nohup "$program" > /dev/null 2>&1 &
    sleep 0.1
fi
