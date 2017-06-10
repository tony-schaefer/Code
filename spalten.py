#!/share/apps/amber16/miniconda/bin/python2.7

#add the $HESS namespace to a .inp file

import sys
import os

if len(sys.argv) < 2:
    print "spalten splits the Hessian matrix out of a hessian .dat file and puts it in a .inp file"
    print "usage: './spalten.py some.inp hessian.dat'"
    print "spalten.inp is created"
    sys.exit(0)

hess=sys.argv[2] #hessian .dat file
inp=sys.argv[1] #some .inp file

hessfile=open(hess,'r')
hesslines=hessfile.readlines()
hessfile.close()

inpfile=open(inp,'r')
inplines=inpfile.readlines()
inpfile.close()

outfile=open('spalten.inp','w')

state=False
for sline in inplines:
    try:
        if sline.split()[0] == '$DATA':
            for tline in hesslines:
                try:
                    if tline.split()[0] == '$HESS':
                        state=True
                    if state:
                        outfile.write(tline)
                    if tline.split()[0] == '$END':
                        state=False
                except:
                    pass
    except:
        pass
    outfile.write(sline)

