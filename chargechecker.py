#!/share/apps/amber16/miniconda/bin/python2.7

import math
import os
import sys

infile=sys.argv[1]
thefile=open(infile,'r')
lines=thefile.readlines()
thefile.close()

lines=[thing.strip() for thing in lines]

state=False
charges=[]

for thing in lines:
    try:
        if thing.split()[0] == '@<TRIPOS>BOND':
            state=False
        if state:
            charges.append(float(thing.split()[-1]))
        if thing.split()[0] == '@<TRIPOS>ATOM':
            state=True
    except:
        pass

charge=sum(charges)

print charge
