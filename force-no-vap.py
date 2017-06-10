#!/usr/bin/env python

import imp
from sys import argv

readgro=imp.load_source('readgro','/home/scha0275/program-workshop/python2.7/readgro.py')

name=argv[1]

info=readgro.groread(name,0)

atlist1=[]
atlist2=[]

for i,mol in enumerate(info["atnum"]):
    if info["xyz"][i][2] > 15.0:
        atlist1.append(mol)
    if info["xyz"][i][2] < 8.0:
        atlist2.append(mol)

atlist1=[str(t).strip() for t in atlist1]
atlist2=[str(t).strip() for t in atlist2]
ndx=open('index.ndx','a')

s=''
ndx.write('[ g1 ]\n')
for a in atlist1:
    s+=a+' '
    if len(s) > 70:
        ndx.write(s+'\n')
        s=''
ndx.write(a+'\n')

s=''
ndx.write('[ g2 ]\n')
for a in atlist2:
    s+=a+' '
    if len(s) > 70:
        ndx.write(s+'\n')
        s=''
ndx.write(a+'\n')

ndx.close()
