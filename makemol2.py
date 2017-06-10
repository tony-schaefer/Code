#!/share/apps/amber16/miniconda/bin/python2.7

import math
import os
import sys

import imp
readgro=imp.load_source('readgro','/home/scha0275/program-workshop/python2.7/readgro.py')

infile=sys.argv[1]
thefile=open(infile,'r')
lines=thefile.readlines()
thefile.close()

lines=[thing.strip() for thing in lines]

state=False
charges=[]

for thing in lines:
    try:
        if thing == '[ bonds ]':
            state=False
        if state:
            charges.append(float(thing.split()[-2]))
        if thing == '[ atoms ]':
            state=True
    except:
        pass

charge=sum(charges)

print charge

infile=sys.argv[2]

fileinfo=readgro.groread(infile,0)

fileinfo['xyz']=[[10.*k for k in j] for j in fileinfo['xyz']]

outfile=open('mymol2.mol2','w')

outfile.write('@<TRIPOS>MOLECULE\n')
outfile.write(' '.join(set(fileinfo['molres']))+'\n')
outfile.write("%5i\n" % (fileinfo['natoms']))


outfile.write('@<TRIPOS>ATOM\n')

for i in range(0,fileinfo['natoms'],1):
    outfile.write(" %6i %-4s         %7.4f    %7.4f    %7.4f %-4s       1  1        %6.4f\n" % (fileinfo['atnum'][i], fileinfo['atres'][i], fileinfo['xyz'][i][0], fileinfo['xyz'][i][1], fileinfo['xyz'][i][2], fileinfo['molres'][i], charges[i]))

outfile.close()
