#!/usr/bin/env python

from sys import argv

filename=argv[1]

infile=open(filename,'r')
lines=infile.readlines()
infile.close()

lines=[l.strip().split() for l in lines]

data=[]

for l in lines:
    try:
        data.append([float(l[0]),sum([float(i) for i in l[1:]])])
    except:
        pass

outfile=open('histo.sum','w')
for p in data:
    outfile.write("%8.5f  %8.5f\n" % (p[0],p[1]))
outfile.close()
