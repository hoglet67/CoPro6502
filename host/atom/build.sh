#!/bin/bash

rm -f TUBE

echo Assembling
ca65 -l AtomHost.lst -o AtomHost.o AtomHost.asm

echo Linking
ld65 AtomHost.o -o TUBE -C AtomHost.lkr 

echo Cleaning
rm -f *.o
