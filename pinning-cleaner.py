#!/usr/bin/env python

import os

qstat = os.popen('qstat -u "*"').read()

run_jids = [i.split()[0] for i in qstat.split('\n') if i]

run_jids.append('unpinned')

nodes=os.listdir('/home/scha0275/.pinning')

for node in nodes:
    with open('/home/scha0275/.pinning/'+node,'r+') as pin:
        pinlines=pin.readlines()
        newlines=[]
        for ln in pinlines:
            if ln.split("'")[1] not in run_jids:
                print 'removed',ln.split("'")[1],'from',node
                newlines.append(ln.split("'")[0]+"'unpinned'"+ln.split("'")[2])
            else:
                newlines.append(ln)
        pin.seek(0)
        pin.write(''.join(newlines))
        pin.truncate()
 
