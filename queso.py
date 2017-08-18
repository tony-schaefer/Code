#!/usr/bin/env python

import os
import time
import sys

class colorme:
    """Spruce up bland terminal output with ease!"""
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


def que_test(a):
    """queue_test(save type)
    test to see if any nodes are not in a normal state
    if save type is 'chem331', the chem331 queue will be checked
    else, the all queue will be checked"""

    qstat = os.popen('qstat -f ').read()

    quetest = qstat.split('\n')

    if a == "chem331":
        queue="chem331"
    else:
        queue="all"

    name = []
    test = []
    state = []
    i=0

    for line in quetest:
        if line.split(".")[0] == queue:
            test.append(line)
            name.append(line.split()[0])
            try:
                state.append(line.split()[5])
            except:
                state.append("up")
            i=i+1

    for i,tem in enumerate(name):
        if state[i] != "up":
            sys.stderr.write("%s%s has state %s\n" % (colorme.red, tem, state[i]))

def queso(proc,savetype):
    """queso( number of processors, save type )
    save types:
        none - use any available space
        save - try to save 32 processors; may be on different nodes; this is the default
        node - try to save one whole node
        scratch - save space on nodes 12-14
        givescratch - only submit to nodes 12-14
        chem331 - submit to the chem331 queue
        tony - don't submit to certain nodes and don't submit to scratch nodes
        cole - don't submit to different certain nodes and not scratch nodes
        matt - don't submit to node 12"""

    trytosave=32

    qstat = os.popen('qstat -f ').read()

    quetest = qstat.split('\n')

    name = []
    cap = []
    current = []
    space = []
    test = []
    state = []
# these are lists
    procinfo = ""
    i=0
    tspace=0

    if savetype == "chem331":
        queue="chem331"
        savetype='none'
    else:
        queue="all"

    if savetype != "none":
        saving=0
    else:
        saving=trytosave
# these are things

    for line in quetest:
# read qstat
        if line.split(".")[0] == queue:
            test.append(line)
# keep track of what's read about the nodes
            name.append(line.split()[0])
# name of the node is the first thing for lines that start with 'all.' (see bottom)
            procinfo = line.split()[2]
# info about what's running on that node comes a little farther down
            current.append(procinfo.split("/")[1])
# seperate that info so it can be used
# current is how many slots are currently being used on that node
            cap.append(procinfo.split("/")[2])
# cap is how many slots the node has
            space.append(int(cap[i])-int(current[i]))
# make a list of how much space is left on each node
            try:
                state.append(line.split()[5])
            except:
                state.append("up")
# see if the node is up, or if something is wrong
            i=i+1
    try:
        # nodes 12, 13, and 14 have more hard drive space than other nodes
        # this is under a try in case we are looking at the chem331 queue
        scratch_nodes=[name.index('all.q@compute-0-12.local'),name.index('all.q@compute-0-13.local'),name.index('all.q@compute-0-14.local')]
        scratch=scratch_nodes
        not_tony=[0,1,2,3,7,8,9,10]+scratch_nodes
        not_cole=[0,1,7,8,9,10,11]+scratch_nodes
        not_matt=[name.index('all.q@compute-0-12.local')]

        if savetype == "scratch":
            for i in scratch:
                state[i]='save'
            savetype="none"

        if savetype == "givescratch":
            for i in range(0,len(state)):
                if i not in scratch:
                    state[i]='save'
            savetype='none'

        if savetype == "tony":
            for i in not_tony:
                state[i]="save"
            savetype='none'

        if savetype == "cole":
            for i  in not_cole:
                state[i]="save"
            savetype='none'

        if savetype == "matt":
            for i in not_matt:
                state[i]="save"
            savetype='none'

        if savetype == "emptyonly":
            for i in range(0,len(state)):
                if int(space[i]) != int(cap[i]):
                    state[i]="save"

    except:
        pass
    lis =[]
    tspace=sum(space)-int(proc)
# make see how much space there is minus a bit to make sure everthing doesn't get filled

    i=0
    m=-1
    maxi=max(space)

    while i < len(name) and savetype == "node":
        if state[i] == "up" and int(space[i]) == int(cap[i]):
            print "keeping",name[i],"empty"
            saving=saving+int(space[i])
            savetype = "con" 
        i+=1

    if savetype != 'none':
        i=0
        while i < len(name) and saving <= trytosave:
            if state[i] == "up" and space[i] >= maxi:
# savetype 'none' makes no attempt to save space for other jobs
                m=i
                maxi=int(space[i])
        
            i+=1
       
            if i == len(name):  
                try:
                    if space[m] > 0 and saving < trytosave:
                        saving = saving + int(space[m])
                        state[m]="save"
                        space[m]=0
                        maxi=max(space)
                        i=0
                    else:
                        break
                except:
                    break
            if i == len(name) or saving >= trytosave: 
                break 
       
    i=0  
    m=-1
    maxi=0
    while i < len(name):
        if int(space[i]) > int(maxi) and state[i] == "up":
            m=i
            maxi=space[i]
        i+=1
# find the emptiest node and remember which one it is

    try:
        if int(space[int(m)]) >= int(proc) and state[m] == "up":
            space[int(m)]=int(space[int(m)])-int(proc)
            current[int(m)]=int(current[int(m)])+int(proc)
            salsa=name[int(m)]
# submit job to emptiest node
        else:
            i=0
            while i < len(name):
                if state[i] == "up":
                    lis.append(name[i])
                i+=1
        
            salsa=','.join(map(str,lis))
# or submit to nodes that aren't reserved (there's space, but not enough)

    except:
        i=0
        while i < len(name):
            if state[i] == "up":
                lis.append(name[i])
            i+=1
        thing=','.join(map(str,lis))
        salsa=thing

    if salsa == "":
        salsa=queue+".q"

    return salsa

# or submit to one of the nodes that isn't reserved (in except because m would not be defined in this case)

# sample qstat -f info included below:
#---------------------------------------------------------------------------------
#all.q@compute-0-4.local       BIP   0/17/32       15.70     linux-x64     
#  49479 0.60520 w-42-1337  scha0275     r     05/01/2016 25:37:45    17        
#---------------------------------------------------------------------------------
#
#name of this node is all.q@compute-0-4.local
#cap is 32, current is 17, space is 15, state is "up"
#everything else is ignored
#

if __name__ == "__main__":

    savetype=sys.argv[1]

    proc=sys.argv[2]

    time.sleep(0.3)

    salsa=queso(proc,savetype)

    que_test(savetype)

    print salsa

