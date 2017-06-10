#!/usr/bin/env python

import sys

inp=sys.argv[1]
with open(inp,'r') as i:
    tops=i.readlines()

natom=0
ntypes=0
nbonh=0
mbona=0
ntheth=0
mtheta=0
nphih=0
mphia=0
nhparm=0
nnb=0
nres=0
nbona=0
ntheta=0
nphia=0
numbnd=0
numang=0
nptra=0
natyp=0
nphb=0
ifpert=0
nbper=0
ngper=0
ndper=0
mbper=0
mbper=0
mgper=0
mgper=0
mdper=0
ifbox=0
nmxrs=0
ifcap=0
numextra=0
ncopy=0

newtop=open('parmesean.top','w')
ready=False

for mol in tops:
    nmols=int(mol.split()[1])
    topfile=mol.split()[0]
    with open(topfile,'r') as i:
        parm=i.readlines()

    l=''.join(parm).split('%')

    for i,tem in enumerate(l):
        if tem =='FLAG TITLE':
            

