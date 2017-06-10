#!/share/apps/amber16/miniconda/bin/python2.7

import numpy
import os
import sys

infile=sys.argv[1]
shouldbe=float(sys.argv[2])
thefile=open(infile,'r')
outfile=open('somemolefile.mol2','w')
lines=thefile.readlines()
thefile.close()

state=False
charges=[]
newcharges=[]
i=0

for thing in lines:
    try:
        if thing.split()[0] == '@<TRIPOS>BOND':
            state=False
            c=float(shouldbe)/sum(charges)
            print c
            newcharges=[c*charge for charge in charges]
            print 'old:',sum(charges)
            print 'new:',sum(newcharges)
            while j < i:
                outfile.write("%72s %9.6f\n" % (lines[j][:71],newcharges[j-k]))
                j+=1
        if state:
            charges.append(float(thing.split()[-1]))
        if thing.split()[0] == '@<TRIPOS>ATOM':
            state=True
            outfile.write(thing)
            j=i+1
            k=i+1
            
    except:
        pass
    if state == False:
        outfile.write(thing)
    i+=1

outfile.close()
