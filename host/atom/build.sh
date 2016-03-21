#!/bin/bash

rm -rf TUBE
mkdir TUBE

cd src

for version in TUBE TUBED1 TUBED2 TUBED3
do

rm -f $version

echo Assembling $version
ca65 -l AtomHost_$version.lst -o AtomHost_$version.o AtomHost_$version.asm

echo Linking $version
ld65 AtomHost_$version.o -o $version -C AtomHost.lkr 

echo CRC = `../tools/crc16 $version | tr "a-z" "A-Z"`

cp -a $version ../TUBE

echo Cleaning $version
rm -f AtomHost_$version.o $version

done

cd ..

echo Packaging

cp -a demos/* TUBE
cp -a README.txt TUBE
rm -f atom_tube.zip
zip -qr atom_tube.zip TUBE
