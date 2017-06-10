#!/share/apps/python/bin/python2.7

import os
import re

list = []

space = 0.3

try:
    os.system("/home/scha0275/backups/./pickme.e <../nvt/nvt_equib.gro")
except:
    os.system("/home/scha0275/backups/./pickme.e <../nvt/confout.gro")

cl = raw_input("What is the last Cl? ")

d = 5

while d >= 5:

    if not os.path.isdir("../"+str(d)+"-windows"):

        os.system("mkdir ../"+str(d)+"-windows")

	break
 
    else:

        d = d + 1

if os.path.isfile("../pull/pull-"+cl+"/traj_comp.xtc"):

    os.system("cp ../pull/pull-"+cl+"/traj_comp.xtc .")
    os.system("cp ../pull/pull-"+cl+"/Run.tpr .")
    os.system("cp ../pull/pull-"+cl+"/*.top .")
    os.system("cp ../pull/pull-"+cl+"/index.ndx .")

    os.system("echo 0 | gmx trjconv -quiet -f traj_comp.xtc -s Run.tpr -o conf.gro -n -sep")

    i = 0

    win = 1

    while os.path.isfile("conf"+str(i)+".gro"):

        print "Processing configuration",str(i)+"..."

        os.system("gmx distance -quiet -select 'com of resname SOL plus group 'CL_spec'' -oxyz -oall dist"+str(i)+".xvg -s Run.tpr -n -f conf"+str(i)+".gro &>/dev/null")

        distfile = open("dist"+str(i)+".xvg","r+")

        for line in distfile:

            if re.match('^@',line) or re.match('^#',line):

	        pass

            else:
            
                dist = line.split()[3]
 
        distfile.close()

        if i is 0:

            lastdist = dist

	if abs(float(dist)-float(lastdist)) > space or i is 0:

            os.system("mkdir ../"+str(d)+"-windows/window-"+str(win)+"-"+str(cl))
            os.system("cp conf"+str(i)+".gro ../"+str(d)+"-windows/window-"+str(win)+"-"+str(cl)+"/input.gro")
	    os.system("cp index.ndx ../"+str(d)+"-windows/window-"+str(win)+"-"+str(cl)+"/index.ndx")
            os.system("cp /home/scha0275/backups/nvt-equil.mdp ../"+str(d)+"-windows/window-"+str(win)+"-"+str(cl)+"/")
            os.system("cp *.top ../"+str(d)+"-windows/window-"+str(win)+"-"+str(cl)+"/")
            list.append(i)
            win = win + 1
            lastdist=dist

	i = i + 1

    os.system("rm conf*")

    os.system("rm dist*")

    os.system("rm \#*")

    print "making",win - 1,"windows:"

    for val in list:

        print val

else:

    print "no traj_comp.xtc found"
