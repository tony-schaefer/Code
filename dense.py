#!/usr/bin/env python

from imp import load_source as load
from sys import argv
from time import clock as timer

gro = load('gro','/home/scha0275/program-workshop/python2.7/readgro.py')

amass={'H':1.008, 'C':12.011, 'N':14.0067, 'O': 15.9994, 'F':18.9984, 'NA': 22.98977, 'CL': 35.453}

bins = 50
ftime=-1
itime=0
axis=2
outfile='dense.out'

for i,tem in enumerate(argv):
    if tem == '-f':
        grofile=argv[i+1]
    if tem == '-ftime':
        ftime=int(argv[i+1])
    if tem == '-itime':
        itime=int(argv[i+1])
    if tem == '-bins':
        bins=int(argv[i+1])
    if tem == '-axis':
        axis={'x':0,'y':1,'z':2,'0':0,'1':1,'2':2}[argv[i+1]]
    if tem == '-o':
        outfile=argv[i+1]

bins = bins + 1
clock = 0
outer_clock=0
io_time=0.0
loop_time=0.0

while True:

    if ftime <= outer_clock and ftime > 0:
        break

    if outer_clock % 50 == 0:
        print outer_clock

    if outer_clock >= itime:
        a=timer()
        info=gro.groread(grofile,outer_clock)
        io_time+=timer()-a
    
        if not info:
            break

        if clock == 0:
            vol=info['box'][0]*info['box'][1]*info['box'][2]/float(bins-1)
            binmass={}
            for thing in set(info['molres']):
                binmass[thing]=[0.0 for i in range(0,bins)]
            
            binpos=[0.0]
            dz=info['box'][axis]/float(bins-1)
            for b in range(1,bins):
                binpos.append(binpos[-1]+dz)


        a=timer()
        for j,xyz in enumerate(info['xyz']):
            for i in range(1, bins):
                if xyz[axis] <= 0.0:
                    xyz[axis]+=info['box'][axis]
                if xyz[axis] >= info['box'][axis]:
                    xyz[axis]-=info['box'][axis]
    
                if xyz[axis] > binpos[i-1] and xyz[axis] < binpos[i]:
                    if info['molres'][j].strip() in amass:
                        ele=info['molres'][j].strip()
                    elif info['atres'][j].strip()[0:2] == 'CL':
                        ele=info['atres'][j].strip()[0:2]
                    else: 
                        ele=info['atres'][j].strip()[0:1]
                
                    binmass[info['molres'][j]][i]+=amass[ele]
                    break
        loop_time+=timer()-a
    
        clock+=1
    
    outer_clock+=1

for res in binmass:
    binmass[res]=[mass/(vol*0.602214*float(clock)) for mass in binmass[res]] 

outfile=open(outfile,'w')
writestr=''
a=timer()
for i in range(1,bins):
    b=binpos[i]-dz/2.0
    writestr+=("%12.7f " % b)
    for res in binmass:
        writestr+=("  %12.7f " % binmass[res][i])
    writestr+='\n'
outfile.write(writestr)
outfile.close()

io_time+=timer()-a

print 'time spent reading:', io_time
print 'time spent processing:', loop_time
