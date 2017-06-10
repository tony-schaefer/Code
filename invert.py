#!/usr/bin/env python

from imp import load_source as load
import sys

gro = load('gro','/home/scha0275/program-workshop/python2.7/readgro.py')

grofile=sys.argv[1]

info=gro.groread(grofile,0)

info['xyz']=[[info['box'][i]-xyz[i] for i in range(0,3)] for xyz in info['xyz']]

gro.growrite('new.gro',info)

