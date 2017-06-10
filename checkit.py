#!/share/apps/amber16/miniconda/bin/python2.7

import sys
import os

thefile=sys.argv[1]

afile=open(thefile,'r')

lines=afile.readlines()

lines=[thing.strip() for thing in lines]

x=[]
y=[]

for thing in lines:
    try:
        x.append(float(thing.split()[0]))
        y.append(float(thing.split()[1]))

    except:
        pass

avg0=0.
n0=0
avg1=0.
n1=0

for thing in x:
    if abs(thing) < 1.5:
        avg0+=y[x.index(thing)]
        n0+=1

    if abs(thing) > 4.:
        avg1+=y[x.index(thing)]
        n1+=1

print avg1/float(n1) - avg0/float(n0)

