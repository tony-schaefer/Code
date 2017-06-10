#!/share/apps/amber16/miniconda/bin/python2.7

import sys

infile=open(sys.argv[1],'r')
inlines=infile.readlines()
outfile=open('bsFlop.xvg','w+')

xi=[]
pmf=[]
err=[]

for line in inlines:
    if line.strip().startswith('@') or line.strip().startswith('#'):
        outfile.write(line)
    else:
        xi.append(float(line.split()[0]))
        pmf.append(float(line.split()[1]))
        err.append(float(line.split()[2]))
          
xi=[-coord for coord in xi]

pmf=[val-pmf[len(pmf)-1] for val in pmf]

i=0

while i < len(pmf):
    outfile.write("%E   %E   %E\n" % (xi[i],pmf[i],err[i]))
    i+=1

infile.close()
outfile.close()
