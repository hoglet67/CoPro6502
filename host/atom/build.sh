#!/bin/bash

cd src

rm -f TUBE

echo Assembling
ca65 -l AtomHost.lst -o AtomHost.o AtomHost.asm

echo Linking
ld65 AtomHost.o -o TUBE -C AtomHost.lkr 

echo CRC = `../tools/crc16 TUBE | tr "a-z" "A-Z"`

echo Cleaning
rm -f *.o

cd ..

echo Packaging

rm -rf TUBE
mkdir TUBE
cp -a README.txt TUBE
cp -a src/TUBE TUBE
cp -a demos/* TUBE
zip -qr atom_tube.zip TUBE
