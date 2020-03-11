#!/bin/bash

for i in {1..8}
do 
    mv tarefa-$i/tarefa$i.exe tarefa-$i/tarefa-$i-11212400.exe
    #gfortran tarefa-$i/tarefa-$i-11212400.f90 -o tarefa-$i/tarefa-$i-11212400.exe
done