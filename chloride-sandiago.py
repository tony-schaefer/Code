#!/usr/bin/env python

import os

dires=os.listdir('.')

for d in dires:
    if os.path.isdir(d):
        pullxfile=d+'/pullx.xvg'
        pfile=open(pullxfile,'r')
        plines=pfile.readlines()
        pfile.close()
        plines=[l.strip().split() for l in plines]
        ls=[]
        for l in plines:
            try:
                ls.append(float(l[1]))
            except:
                pass
        print d
        print min(ls)
        print max(ls)
