#!/bin/sh
echo 'This is the Interactive Perl shell'
rlwrap -A -pgreen -S"perl> " perl -wnE'say eval()//$@'