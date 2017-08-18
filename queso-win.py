#!/usr/bin/env python

import os
import time
import sys
from imp import load_source

queso_mod=load_source('queso_mod','/home/scha0275/program-workshop/python2.7/queso.py')

class colorme:
    red='\033[31m'
    green='\033[32m'
    yellow='\033[93m'
    blue='\033[34m'
    purple='\033[35m'
    cyan='\033[96m'
    white='\033[97m'
    black='\033[98m'
    gray='\033[90m'
    lightgrey='\033[37m'
    orange='\033[33m'

savetype=sys.argv[1]
#save scheme

proc = sys.argv[2]
# number of slots the job takes
try:
    startwin = int(sys.argv[3])
except:
    startwin = 1
try:
    stopwin = int(sys.argv[4])
except:
    stopwin = -1

try:
    clstart = int(sys.argv[5])
except:
    clstart = -1
# starting Cl

try:
    clstop = int(sys.argv[6])
except:
    clstop = -1
# stop Cl

win = startwin

if clstop == -1:
    dires=os.listdir('.')
    for thing in dires:
        if '-' in thing:
            try:
                if int(thing.split('-')[-1]) > clstop or clstop == -1:
                    clstop=int(thing.split('-')[-1])
                if int(thing.split('-')[-1]) <= clstop:
                    clstart=int(thing.split('-')[-1])
            except:
                pass
    num = int(clstop)-int(clstart)
    cl=clstop
    cls=clstop
else:
    cls = clstop
    num = int(clstop)-int(clstart)
    cl = int(clstop)

while int(cl) >= int(cls) - int(num):

    if win > stopwin and stopwin > 0:
        break

    dire = "window-" + str(win) + "-" + str(cl)

    if os.path.isdir(dire):
#
        salsa=queso_mod.queso(proc,savetype)

        os.chdir(dire)

        if salsa.count(',') == 0:
            time.sleep(0.7)

        #os.system('/home/scha0275/program-workshop/queso-tests/./queso-test.sh ' + str(proc)+ " " + str(dire.split("-")[1]) + "-" + str(dire.split("-")[2])+" "+salsa)
        os.system('/home/scha0275/backups/./queso-qgmx '+str(proc)+" w-"+ str(dire.split("-")[1])+"-"+str(dire.split("-")[2])+" "+salsa)
            
        os.chdir("../")
       
        win = win + 1

    else:
    
        win = startwin

	cl = int(cl)-1
        
queso_mod.que_test(savetype)

