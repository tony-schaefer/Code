#!/usr/bin/env python

import os
from numpy import array

s=raw_input('this will remove all backup traj_comp.xtc files; press enter to confirm')

try:
    os.remove('/home/scha0275/program-workshop/library.list')
except:
    pass
prevdir=os.getcwd()
os.chdir('/home/scha0275/')

def addtolibrary():
    l=array(os.listdir('.'))
    for thing in l:
        if not os.path.isdir(thing) and not thing.startswith('#'):
            with open('/home/scha0275/program-workshop/library.list','a') as phil:
                cwd=os.getcwd()+'/'
                phil.write(cwd+thing+'\n')
        if thing.startswith('#traj_comp.xtc.'):
            os.remove(thing)
            print cwd+thing, 'removed'
        if os.path.isdir(thing):
            os.chdir(thing)
            a=addtolibrary()
            os.chdir('..')
    return True

b=addtolibrary()
os.chdir(prevdir)
