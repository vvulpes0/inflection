#!/bin/sh

base="${1##*/}"

./cm "$1" < "${1%trn}tst" > Hypotheses/"${base%trn}tst"
