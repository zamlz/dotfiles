#!/bin/bash
for y in $(seq 0 524287)
  do
  for x in $(seq 0 7)
  do
    a=$(expr $y \* 8 + $x)
    echo -ne "$a \\u$a "
  done
  echo
done
