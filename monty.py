#!/bin/python2

import sys
import os
import random
import math

help="false"

if "-h" in sys.argv:
    help="true"

try:
    sys.argv[1]
except:
    help="true"

if help == "true":
    print ""
    print "       catnip is used to put a residue from one file into another"
    print ""
    print "-c   : input system coordinate file (.gro) to which molecules are added"
    print "-oc  : [ optional | catnip.gro ] output system coordinate file"
    print "-p   : input topology file (.top) which corresponds to the system coordinate file"
    print "-op  : [ optional | catnip.top ] output topology file (.top)"
    print "-ci  : input molecule file (.top) which is put in the system file"
    print "-dl  : [ optional | some.file ] File listing residues that can be removed, if necessary,"
    print "       to make space for new molecules. One RES per line. Also, 'no RES' prevents a"
    print "       hardcoded residue from being removed."
    print "-n   : [ optional | prompted ] number of new molecules to put into the system"
    print "-g   : [ optional | prompted ] group name, number, or 'interface RES1 RES2 or ceil'"
    print "-ndx : [ optional | index.ndx ] specify index file"
    print "-h   : help screen"
    sys.exit()
# useful info ^

deleted=0
i=0
outbox="catnip.gro"
outtop="catnip.top"
ndxfile="index.ndx"
# default outs
delgroups=[]
indgroups=[]
groupies=[]
thisgroup=[]
r3=[0,1,2]
g1=6.02314
g2=3.14159
catdel=[]


def sqdist(this,that):
    xd=min([(this[0]-that[0])**2,(abs(this[0]-that[0])-box[0])**2])
    yd=min([(this[1]-that[1])**2,(abs(this[1]-that[1])-box[1])**2])
    zd=min([(this[2]-that[2])**2,(abs(this[2]-that[2])-box[2])**2])
    return xd+yd+zd 
# squared distance
 
def distance(this,that):
    return math.sqrt(sqdist(this,that))
# square root of squared distance
  
def move(this,that):
# move this molecule away from that molecule
    a=sqdist(xyz[boxnum.index(rngesus)],this)
    b=sqdist(xyz[boxnum.index(rngesus)],that)
    c=sqdist(this,that)
    try:
        psi=math.acos((c-a-b)/(-2.*math.sqrt(a*b)))
    except:
        psi = 0
    if abs(psi) > 0.3:
        try:
            dx=(that[0]-this[0])/distance(this,that)
            dy=(that[1]-this[1])/distance(this,that)
            dz=(that[2]-this[2])/distance(this,that)
        except:
            dx=1.0
            dy=0.0
            dz=0.0
        dx=0.4*dx
        dy=0.4*dy
        dz=0.4*dz
        dxyz=[dx,dy,dz]
        tryxyz=[[oldxyz[i-1][j]+dxyz[j]+xyz[boxnum.index(rngesus)][j] for j in r3] for i in newat]
        return tryxyz
# translate it away
    else:
        a1=that[0]-this[0]
        a2=that[1]-this[1]
        a3=that[2]-this[2]
        b1=that[0]-xyz[boxnum.index(rngesus)][0]
        b2=that[1]-xyz[boxnum.index(rngesus)][1]
        b3=that[2]-xyz[boxnum.index(rngesus)][2]
        vx=(a2*b3)-(a3*b2)
        vy=-(a1*b3)+(a3*b1)
        vz=(a1*b2)-(a2*b1)
        n=math.sqrt(vx**2+vy**2+vz**2)
        try:
            vx=vx/n
            vy=vy/n
            vz=vz/n
        except:
            vx=1.0
            vy=0.0
            vz=0.0
        if psi > 0:
            R=[[math.cos(0.3)+vx*vx*(1-math.cos(0.3)),vx*vy*(1-math.cos(0.3))-vz*math.sin(0.3),vx*vx*(1-math.cos(0.3))+vy*math.sin(0.3)],[vx*vy*(1-math.cos(0.3))+vz*math.sin(0.3),math.cos(0.3)+vy*vy*(1-math.cos(0.3)),vy*vz*(1-math.cos(0.3))-vx*math.sin(0.3)],[vx*vz*(1-math.cos(0.3))-vy*math.sin(0.3),vy*vz*(1-math.cos(0.3))+vx*math.sin(0.3),math.cos(0.3)+vz*vz*(1-math.cos(0.3))]]
        else:
            R=[[math.cos(-0.3)+vx*vx*(1-math.cos(-0.3)),vx*vy*(1-math.cos(-0.3))-vz*math.sin(-0.3),vx*vx*(1-math.cos(-0.3))+vy*math.sin(-0.3)],[vx*vy*(1-math.cos(-0.3))+vz*math.sin(-0.3),math.cos(-0.3)+vy*vy*(1-math.cos(-0.3)),vy*vz*(1-math.cos(-0.3))-vx*math.sin(-0.3)],[vx*vz*(1-math.cos(-0.3))-vy*math.sin(-0.3),vy*vz*(1-math.cos(-0.3))+vx*math.sin(-0.3),math.cos(-0.3)+vz*vz*(1-math.cos(-0.3))]]
        i=0
        while i < len(oldxyz):
            dx=oldxyz[i][0]*R[0][0]+oldxyz[i][1]*R[0][1]+oldxyz[i][2]*R[0][2] 
            dy=oldxyz[i][0]*R[1][0]+oldxyz[i][1]*R[1][1]+oldxyz[i][2]*R[1][2]
            dz=oldxyz[i][0]*R[2][0]+oldxyz[i][1]*R[2][1]+oldxyz[i][2]*R[2][2]
            oldxyz[i]=[dx,dy,dz]
            i+=1 
             
        tryxyz=[[oldxyz[i-1][j]+xyz[boxnum.index(rngesus)][j] for j in r3] for i in newat]
        return tryxyz
# rotate it

while i < len(sys.argv):
    if sys.argv[i] == "-c":
        box=sys.argv[i+1]
    if sys.argv[i] == "-oc":
        outbox=sys.argv[i+1]
    if sys.argv[i] == "-p":
        topfile=sys.argv[i+1]
    if sys.argv[i] == "-op":
        outtop=sys.argv[i+1]
    if sys.argv[i] == "-ci":
        molfile=sys.argv[i+1]
    if sys.argv[i] == "-dl":
        dfile=sys.argv[i+1]
    if sys.argv[i] == "-n":
        tries=int(sys.argv[i+1])
    if sys.argv[i] == "-g":
        group=sys.argv[i+1]
        if group == "interface":
            g1=sys.argv[i+2]
            g2=sys.argv[i+3]
    if sys.argv[i] == "-ndx":
        ndxfile=sys.argv[i+1]
  
    i+=1
# read command line stuff 
  
try:
    tries
except:
    tries=raw_input("How many would you like added? ")

tries=int(tries)
# if not given a command line option, it will ask how many to add

if tries < 1:
    print "must add at least once"
    sys.exit()

try:
    if os.path.isfile(dfile):
        delfile=open(dfile,"r+")
        for line in delfile:
            thing = line.split(',')
            for thang in thing:
                if thang not in delgroups:
                    delgroups.append(thang) 
        delfile.close()
# read file for deleting stuff
    else:
        delgroups=dfile.split(",")
# or read commandline stuff
except:
    pass

if "SOL" not in delgroups:
    delgroups.append("SOL")
if "CHX" not in delgroups:
    delgroups.append("CHX")
if "CHCL" not in delgroups:
    delgroups.append("CHCL")
for thing in delgroups:
    if thing.split()[0] == "no":
        while thing.split()[1] in delgroups:
            delgroups.remove(thing.split()[1])
    while delgroups.count(thing) > 1:
        delgroups.remove(thing)
# add or remove things from the list

print "instructions for removable molecules:",delgroups

if not os.path.isfile("index.ndx"):
    print "making new index..."
    os.system("echo q | gmx make_ndx -f "+box+" &>/dev/null")
# make new index if there isn't one

index=open(ndxfile,"rw+")

groups=-1

print "reading index file..."

for line in index:
    try:
        if line.split()[0] == '[':
            groupies.append([])
            groupies[groups]=thisgroup
            indgroups.append(line)
            thisgroup=[]
            groups=groups+1
        else:
            thisgroup=thisgroup+(line.split())
    except:
        pass
# read index for group name and the atoms in each group

indgroups=[thing[2:-3] for thing in indgroups]
# remove brackets

index.close()

try:
    group
except:
    for thing in indgroups:
        print indgroups.index(thing),thing
    group = raw_input("Where would you like to place the new molecules? ")
    if group == "interface":
        g1=raw_input("group one: ")
        g2=raw_input("group two: ") 
try:
    group=indgroups[int(group)]
except:
    pass
try: 
    g1=indgroups[int(g1)]
except:
    pass
try:
    g2=indgroups[int(g2)]
except:
    pass
# making sure you know what group you want removed 

boxfile=open(box,"r+")

molnum=[]
boxmol=[]
boxele=[]
boxnum=[]
xyz=[]
box=[]

print "reading box file..."

for line in boxfile:
    try:
        line.split()[2]
        molnum.append(int(line[:5].split()[0]))
        boxmol.append(str(line[5:9].split()[0]))
        boxele.append(str(line[12:15].split()[0]))
        boxnum.append(int(line[15:20].split()[0]))
        xyz.append(line[21:45].split())
    except:
        try:
            boxatoms=int(line.split()[0])
        except:
            pass

box=line.split()
box=[float(i) for i in box]
xyz=[[float(j) for j in i] for i in xyz]
# read system box stuff

boxfile.close()

if group == "interface":
    i=0
    com1=0
    com2=0
    groups=groups+1
    while i < boxatoms:
        if boxmol[i] == g1:
            com1=com1+xyz[i][2]
        if boxmol[i] == g2:
            com2=com2+xyz[i][2]
        i+=1
    try:
        com1=com1/boxmol.count(g1)
    except:
        if g1 == "ceil":
            com1=box[2]
        else:
            com1=0.
    try:
        com2=com2/boxmol.count(g2)
    except:
        if g2 == "ceil":
            com2=box[2]
        else:
            com2=0.

    top=max([com1,com2])
    bot=min([com1,com2])
    indgroups.append("interface")
    thing=[]
    for i in boxnum:
        if xyz[i-1][2] < top and xyz[i-1][2] > bot:
            thing.append(i)
    groupies.append([])
    groupies[groups]=thing
# find interface if needed

llptc=open(molfile,"r+")

newnum=[]
newmol=[]
newele=[]
newat=[]
newxyz=[]

print "reading new molecule file..."

for line in llptc:
    try:
        line.split()[2]
        newnum.append(int(line[:5].split()[0]))
        newmol.append(str(line[5:9].split()[0]))
        newele.append(str(line[12:15].split()[0]))
        newat.append(int(line[15:20].split()[0]))
        newxyz.append(line[21:45].split())
    except:
        try:
            newatoms=int(line.split()[0])
        except:
            pass

llptc.close()

added=0

while added < tries:

    added=added+1

    rngesus=int(groupies[indgroups.index(group)][random.randrange(0,len(groupies[indgroups.index(group)]))])
# pick the chosen one

    print "placing new molecule",added,"by",rngesus

    oldxyz=[]
    
    newxyz=[[float(j) for j in i] for i in newxyz]

    cx=0.
    cy=0.
    cz=0.

    for thing in newxyz:
        cx=cx+thing[0]
        cy=cy+thing[1]
        cz=cz+thing[2]

    for i in newat:
        oldxyz.append([newxyz[i-1][0]-cx/float(len(newxyz)),newxyz[i-1][1]-cy/float(len(newxyz)),newxyz[i-1][2]-cz/float(len(newxyz))])

    tryxyz=[[xyz[boxnum.index(rngesus)][j]+oldxyz[i-1][j] for j in r3] for i in newat]

    attempt=0

    print "adjusting orientation..."

    for i in tryxyz:
        for j in xyz:
            if sqdist(i,j) < 0.015 and boxmol[xyz.index(j)] not in delgroups:
                attempt=attempt+1
                tryxyz=move(i,j)
            if attempt > 20:
                break
        if attempt > 20:
            break
# it's got 20 tries to move the molecule away, but it probably won't use them because it's not smart
      
    print "removing molecules..."

    for j in xyz:
        for i in tryxyz:
            if sqdist(i,j) < 0.015 and boxmol[xyz.index(j)] in delgroups and molnum[xyz.index(j)] not in catdel:
                catdel.append(molnum[xyz.index(j)])
                deleted=deleted+molnum.count(molnum[xyz.index(j)])

# the next part of the code adds ~5 seconds per molecule added with a 20k atom system

#    print "taking names..."

#    for k in boxnum:
#        if molnum[boxnum.index(k)] in catdel and molnum[boxnum.index(k)] in groupies[indgroups.index(group)]:
#            pass
#        else:
#            if molnum[boxnum.index(k)] in groupies[indgroups.index(group)]:
#            groupies[indgroups.index(group)]=[kept for kept in groupies[indgroups.index(group)] if not kept in catdel]
#            groupies[indgroups.index(group)].remove(molnum[boxnum.index(k)])

    mols=molnum[len(molnum)-1]
    for i in newat:
         molnum.append(newnum[newat.index(i)]+mols)
         boxmol.append(newmol[newat.index(i)])
         boxele.append(newele[newat.index(i)])
         boxnum.append(i+boxatoms)
         xyz.append(tryxyz[newat.index(i)])
        
    boxatoms=boxatoms+newatoms           
# update number of atoms in the box

    for k in newlist:
        newcount[newlist.index(k)]+=len(stuff[newlist.index(k)])

print "writing output files..."

catcoord=open(outbox,"w")
thetop=open(topfile,"r")
cattop=open(outtop,"w")

for line in thetop:
    cattop.write(str(line))
    try:
        if line.split()[0]+line.split()[1]+line.split()[2] == "[molecules]":
            break
# copy everything from the old topology up until [ molecules ]
    except:
        pass

thetop.close()

nowatoms=int(boxatoms-deleted)

catcoord.write("catnip conffile\n")
catcoord.write(str(nowatoms)+"\n")
# write new number of atoms to system

nextmol=1
nextatom=0
molsofthat=0

for k in boxnum:
    if molnum[boxnum.index(k)] not in catdel:
        try: 
            if molnum[boxnum.index(k)] != molnum[boxnum.index(k-1)]:
                nextmol+=1
                molsofthat+=1
# renumber atoms and molecules so they are all consecutive
            if boxmol[boxnum.index(k)] != boxmol[boxnum.index(k-1)] or k is boxnum[len(boxnum)-1]:
                if k is boxnum[len(boxnum)-1]:
                    molsofthat+=1
                print boxmol[boxnum.index(k-1)]
                cattop.write(boxmol[boxnum.index(k-1)]+"    "+str(molsofthat)+"\n")
                molsofthat=0
# write new number of things to topology

        except:
            pass
        nextatom+=1
        catcoord.write("%5i%-4s  %4s%5i  %6.3f  %6.3f  %6.3f\n" % (nextmol,boxmol[boxnum.index(k)],boxele[boxnum.index(k)],nextatom,xyz[boxnum.index(k)][0],xyz[boxnum.index(k)][1],xyz[boxnum.index(k)][2]))
# write all atoms to the new file

catcoord.close()

cattop.close()

os.system("echo '    '"+str(box[0])+"'    '"+str(box[1])+"'    '"+str(box[2])+" >> "+outbox)

# echo the last line so vmd can open the new file?

