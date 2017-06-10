#!/share/apps/amber16/miniconda/bin/python2.7

import matplotlib.pyplot as plot
import sys

print "plotter to Rule The World"

x=[]
y=[]
color=[]
cmd=[]

theplan=open(sys.argv[1],'r')
stuff=theplan.readlines()
theplan.close()

internal=['hexscatter()','pointscatter()']

def pointscatter():
    for line in stuff:
        try:
            x.append(float(line.split()[0]))
            y.append(float(line.split()[1]))
        except:
            cmd.append(line)
    for thing in cmd:
        try:
            if thing.split()[0] not in internal: 
                exec thing.lstrip() 
        except:
            pass
def hexscatter():
    for line in stuff:
        try:
            if line.split()[2] != '#FFFFFF':
                x.append(float(line.split()[0]))
                y.append(float(line.split()[1]))
                color.append(line.split()[2])
        except:
            cmd.append(line)
    for thing in cmd:
        try:
            if thing.split()[0] not in internal: 
                exec thing.lstrip() 
        except:
            pass
try:
    if stuff[0].strip() not in internal:
        print "unacceptable file format"
        print "first line must be one of", [cmd for cmd in internal]
    exec stuff[0].lstrip()
    print "What are we going to do today, Brain?"
    print "Same thing we do every day. We're going to "+stuff[0].strip()[:-2]+" to rule "+sys.argv[1]+"!"
except:
    print "could not interpret",stuff[0]
    print "first line must be one of", [cmd for cmd in internal]
    
plot.show() 

