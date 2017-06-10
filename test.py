#!/usr/bin/env python

from imp import load_source as load

gro = load('gro','/home/scha0275/program-workshop/python2.7/readgro.py')

info=gro.groread('../coordinate-files/confout.gro',0)

gro.growrite('new.gro',info)


