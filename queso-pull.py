#!/usr/bin/env python

import os
import time
import sys
from imp import load_source

queso_mod=load_source('queso_mod','/home/scha0275/program-workshop/python2.7/queso.py')

class colorme:
    red='\033[91m'
    green='\033[92m'
    yellow='\033[93m'
    blue='\033[94m'
    purple='\033[95m'
    cyan='\033[96m'

savetype=sys.argv[1]
# saving scheme

proc = sys.argv[2]
# number of slots the job takes
try:
    clstart = int(sys.argv[3])
except:
    clstart = -1
# starting Cl

try:
    clstop = int(sys.argv[4])
except:
    clstop = -1
# stop Cl

if clstop == -1:
    dires=sorted([ d for d in os.listdir('.') if 'pull-' in d ])
    clstop=int(dires[0].split('-')[-1])
    if clstart == -1:
        clstart=int(dires[-1].split('-')[-1])
    else:
        clstop=int(dires[-1].split('-')[-1])
    num = int(clstop)-int(clstart)
    cl=clstop
    cls=clstop
else:
    cls = clstop
    num = int(clstop)-int(clstart)
    cl = int(clstop)

cl_list=range(max([clstart,clstop]), min([clstop-1,clstart-1]), -1)

savetype = sys.argv[1]
    # rules for saving space

for cl in cl_list:

    dire = "pull-" + str(cl)

    if os.path.isdir(dire):
        salsa=queso_mod.queso(proc,savetype)

        os.chdir(dire)

        if not os.path.isfile("pull.mdp"):
            os.system("cp ../pull.mdp .")

        if salsa.count(',') == 0:
            time.sleep(0.3)

        os.system('/home/scha0275/backups/./queso-qgmx '+str(proc)+" p-"+ str(dire.split("-")[1])+" "+salsa)
	
        os.chdir("../")
       
    else:
    
        break

queso_mod.que_test(savetype)

