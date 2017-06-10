#!/share/apps/python/bin/python2.7

import os
import time
import sys

class colorme:
    red='\033[91m'
    green='\033[92m'
    yellow='\033[93m'
    blue='\033[94m'
    purple='\033[95m'
    cyan='\033[96m'

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
    clstart=int(dires[-1].split('-')[-1])
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
trytosave=32
if savetype != "none":
    saving=0
else:
    saving=64
# these are things

for line in quetest:
# read queso.test
    if line.split(".")[0] == "all":
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
            print "node",i,"has state",state[i]
        except:
            state.append("up")
# don't want to submit stuff to nodes that are throwing a fit
        i=i+1

if savetype == "scratch":
#    state[0]='save'
    state[4]='save'
    state[5]='save'
    state[6]='save'
    savetype="none"

for cl in cl_list:

    dire = "pull-" + str(cl)

    if os.path.isdir(dire):

        lis =[]
        tspace=sum(space)-int(proc)
# make see how much space there is minus a bit to make sure everthing doesn't get filled

# set to 64 to easily bypass the space-saving code

        i=0
        m=-1
        maxi=max(space)

        while i < len(name) and savetype == "node":
            if state[i] == "up" and int(space[i]) is int(cap[i]):
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
    	                    print "saving",space[m],"on",name[m] 
                            state[m]="save"
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
                    i=i+1
                salsa=','.join(map(str,lis))
# or submit to nodes that aren't reserved (there's space, but not enough)

        except:
            i=0
            while i < len(name):
                if state[i] == "up":
                    lis.append(name[i])
                i=i+1
            thing=','.join(map(str,lis))
            salsa=thing
# or submit to one of the nodes that isn't reserved (in except because m would not be defined in this case)

# sample qstat -f info included below:
#---------------------------------------------------------------------------------
#all.q@compute-0-4.local       BIP   0/17/32        5.70     linux-x64     
#  49479 0.60520 w-42-1337  scha0275     r     05/01/2016 25:37:45    17        
#---------------------------------------------------------------------------------
#
#name of this node is all.q@compute-0-4.local
#cap is 32, current is 17, space is 15
#everything else is ignored
#

        os.chdir(dire)

        if not os.path.isfile("pull.mdp"):
            os.system("cp ../pull.mdp .")

        if salsa.count(',') == 0: 
            time.sleep(1.5)
        
        os.system('/home/scha0275/backups/./queso-qgmx '+str(proc)+" p-"+ str(dire.split("-")[1])+" "+salsa)
	
        os.chdir("../")
       
    else:
    
        break

i=0
for thing in state:
    if thing not in ["up","save"]:
        print colorme.red+ name[i],"has state",state[i]
    i+=1

if os.path.isfile("queso.test"):
    os.remove("queso.test")
