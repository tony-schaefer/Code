#!/usr/bin/env python

from sys import argv
from os import system, environ, getenv, getpid, popen
from time import sleep
from linecache import getline
#if this ever breaks, I wouldn't bother fixing it; it's too stupid and impossible to get it to work 100% of the time
#add a delay between your job submissions to make it work more often
#maybe you want to because performance is better idk

starting=False
pinned="pinned"
unpinned="unpinned"
# i forget quotes sometimes

jobtype='gmx'
# assume job is an gmx job
def sleepcycle(jobid):
    pid=getpid()
    sleep(float(int(jobid) % 10))
    sleep(float(pid % 10))
    return None

try:
    from socket import gethostname
    nodename=gethostname()
#this might not work depending on where the code actually get executed (idk), you can pass the hostname on the command line
except:
    pass

for thing in argv[1:]:
    if thing.lower() == 'starting':
        starting=True
        system('/home/scha0275/program-workshop/python2.7/pinning-cleaner.py')
        #job is starting, figure out what procs to bind it to
    if thing.lower() == 'ending':
        starting=False
        #job is ending, update scratch files to say nothing is bound to these procs anymore
    if thing.lower().startswith('procs='):
        procs=int(thing.split('=')[1])
        #for starting, get me this many procs
    if thing.lower().startswith('jid='):
        jobid=thing.split('=')[1]
        #\$JOB_ID for scratch files
    if thing.lower().startswith('type='):
        jobtype=thing.split('=')[1]
        #type=gmx means it's a gromacs job, benefits a lot from hyperthreading, so spread procs out over more cores 
    if thing.lower().startswith('host='):
        nodename=thing.split('=')[1]
        #compute name the job will run on (don't need this, but is here for testing)
    if thing.lower().startswith('cmd='):
        cmdfile=thing.split('=')[1]
        #file with a list of commands to execute, 1 command per line

#example: echo "gmx mdrun -nt $NUMPROC > cmd.file"
#example: echo "mpirun pmemd.MPI >> cmd.file"
#example: qsub << eof 
#example: et cetera 
#example: ./pynscheduling.py starting jid=\$JOB_ID procs=$NUMPROC cmd=cmd.file type=gmx host=\$HOSTNAME
#example: >> eof

with open('pyn.log','a') as pyn:
    pyn.write('looking at pins on '+nodename+'\n')

if starting:
    mypins=[]
#########i'm almost sure the following code works as intended
# k i'm actually not very sure
# only like 55% sure
    a=sleepcycle(jobid)

    ready=False

    while not ready:

        running=popen("top -d 0 -n 1 -b").read()
# check what's running using top
        jobs=running.split('\n')

        for j,tem in enumerate(jobs):
            if tem.startswith("PID"):
                start=j+1
                break

        jobs=[thing.split()[-1] for thing in jobs[j:] if thing != '' and thing.split()[7] == 'R']

        if jobs.count("pynscheduling.p") + jobs.count("top") > 1:
        # see if more than one instance of this program is running
            with open('pyn.log','a') as pyn:
                pyn.write('top or another instance of this program is running:\n')
                for job in jobs:
                    pyn.write(job+'\n')
                pyn.write("performin' sleep cycle\n")
            a=sleepcycle(jobid)
        else:
            ready=True

    flags=[' ']
    #idk what to do if you aren't running in parallel
    i=0
    pinthese=[]
    #list of procs that the mpirun/mdrun commands will be bound to
    j=0
    diditwork=False
    while diditwork is False:
        mypins=['unpinned' for i in range(0,32)]
        # note - this is not the same as saying all processors are unpinned
        # this is "mypins[0]='unpinned'"
        # to say that it is unpinned, we would need "mypins[0]=['unpinned']"
        # these are totally different 
        # this was written this way to be as clear as possible
        # jk
        with open('pyn.log','a') as pyn:
            pyn.write('the current pinning status of the node is as follows:\n')
        for i in range(1,33):
            l=getline('/home/scha0275/.pinning/'+nodename,i)
            exec l.strip()
            if l == '': 
                with open('pyn.log','a') as pyn:
                #this has happened when the hard drives fill up
                #replace the file
                    pyn.write('there is something wrong with the pinning scratch file for this node\n')
                    pyn.write('one or more lines in the scratch file is blank\n')
                    diditwork=True
            with open('pyn.log','a') as pyn:
                pyn.write(str(i-1)+" "+mypins[i-1][0]+"\n")
        with open('/home/scha0275/.pinning/'+nodename,'r') as f:
            data=f.read()
        with open('/home/scha0275/.pinning/'+nodename,'w') as f:
            f.write('a=sleepcycle(jobid)\n'+data)
# file should only have 32 lines which is a list of what's currently pinned 
# mypins=[['63214'],...['unpinned']]
#            ^job id          ^ not pinned; index is processor id or whatever it's called \_('')_/ 
    #read scratch file that contains info about what's bound to what proc

        foundpins=False
        tried=0
#        while foundpins:
        if jobtype == 'mpi' and 2*(procs//2) != procs:
            with open('pyn.log','a') as p:
                p.write('mpi job is more convenient with an even number of processors')
                p.write("I'll let it pass this time\n")
                procs+=1
        while not foundpins:
            tried+=1
            if tried > 30:
                break
            for n in range(0,len(mypins)):
                try:
                    if n <= 16-(procs//2) and jobtype == 'mpi':
                        templist=range(n,n+procs//2)+range(n+16,16+n+procs//2)
                        if all(mypins[j] == ['unpinned'] for j in templist):
                            pinthese=templist
                # no hyperthreading with this ^, won't bother getting gromacs to follow this

                    if n <= 32-procs and jobtype != 'mpi':
# mpi jobs are run per core and not per proc, so this resource allocating thing won't be as space-efficient 
                        templist=range(n,n+procs)
                        if n+procs <= len(mypins):
                            if all(mypins[j] == ['unpinned'] for j in templist):
                                pinthese=templist
                                break
                # maybe hyperthreading with this ^
                except:
                    pass

                if n >= 32-procs:
                    flags=[' ',' ']
                    break
                
            if jobtype == 'gmx' and len(pinthese) > 0:
                if pinthese[0] <= 15 and pinthese[-1] <= 15:
                    foundpins=True
                    pinoffset=pinthese[0]*2
                    flags=['-pin on','-pinoffset',pinoffset,'-pinstride',2]
                    #gmx flags for thread 0
                    with open('pyn.log','a') as pyn:
                        pyn.write('found a good place to pin this gromacs job\n')
                        for p in pinthese:
                            pyn.write(str(p)+"\n")
                    break
                elif pinthese[0] > 15:
                    pinoffset=(pinthese[0]-16)*2+1
                    flags=['-pin on','-pinoffset',pinoffset,'-pinstride',2]
                    foundpins=True
                    #gmx flags for thread 1
                    with open('pyn.log','a') as pyn:
                        pyn.write('found a good place to pin this gromacs job\n')
                        for p in pinthese:
                            pyn.write(str(p)+"\n")
                    break
                else:
                    mypins[pinthese[0]]=['pass']
                    #don't want jobs getting split across sockets. it's slow because the cache isn't shared well or something. idk. it's just slower.
        
            if jobtype == 'mpi' and len(pinthese) > 0:
                rankfile=open('rankpyle','w')
                r=0
                for thing in pinthese[:len(pinthese)//2]:
                    rankfile.write('rank '+str(r)+'='+nodename+' slot='+str(thing)+'\n')
                    r+=1
                rankfile.close()
                #write rankfile
                flags=['-rf','rankpyle']
                foundpins=True
                with open('pyn.log','a') as pyn:
                    pyn.write('found a good place to pin this mpi job\n')
                    for p in pinthese:
                        pyn.write(str(p)+"\n")
                break

            if jobtype == 'g09':
                flags=','.join([str(k) for k in pinthese])
                foundpins=True
                with open('pyn.log','a') as pyn:
                    pyn.write('found a good place to pin this gaussian job\n')
                    for p in pinthese:
                        pyn.write(str(p)+"\n")
                break

        if len(pinthese) > 0:
            for thing in pinthese:
                mypins[thing]=[jobid]

            with open('/home/scha0275/.pinning/'+nodename,'w') as phil:
                for i,tem in enumerate(mypins):
                    phil.write('mypins['+str(i)+"]= ['"+str(tem[0]).replace('pass','unpinned')+"']\n")
                oldpins=mypins
                with open('/home/scha0275/.pinning/'+nodename,'r') as phil:
                    for line in phil:
                        exec line.strip()

                for i,tem in enumerate(oldpins):
                    if tem != ['pass'] and tem != mypins[i]:
                        diditwork=False
                        with open('pyn.log','a') as pyn:
                            pyn.write('there was a problem with marking these as pinned; trying again\n')
                        a=sleepcycle(jobid)
                else:
                    with open('pyn.log','a') as pyn:
                        pyn.write('these should have marked as pinned, moving on to running commands\n')
                    diditwork=True
        else:
            diditwork=True
            flags=[' ']
            with open('pyn.log','a') as pyn:
                pyn.write("wasn't able to find a good place to pin this\n")
        #update pin scratch file using sed because it might be faster, idk
   
    if jobtype == 'mpi' and len(pinthese) == 0:
        flags=['-np',str(int(procs))]

    environ['whospynning'] = str(-1)

    if jobtype != 'g09':
        flags=[str(thing) for thing in flags]
        flagstr=' '.join(flags)
        with open('pyn.log','a') as pyn:
            pyn.write('using these flags: '+flagstr+'\n')
    #convert flags to something that can be used on the command line

    cmdf=open(cmdfile,'r+')
    cmdl=cmdf.readlines()
    cmdf.close()
    #read the commands we are supposed to execute
    cmdl=[thing.strip() for thing in cmdl]
   
    for thing in cmdl:

        cmde=thing.split()

        for thang in cmde:

            if jobtype == 'g09' and len(pinthese) > 0:
                if thang.strip().endswith('.com'):
                    with open('pyn.log','a') as pyn:
                        pyn.write('will add '+flags+' to '+thang+'\n')
                    comfile=open(thang,'r+')
                    comlines=comfile.readlines()
                    comfile.seek(0)
                    if any(j.startswith('%cpu') for j in comlines):
                        a=0
                        for thing in comlines: 
                            if thing.startswith('%cpu'):
                                break
                            a+=1
                        comlines=comlines[:a]+['%cpu='+flags+'\n']+comlines[a+1:]
                    else:
                        comlines=['%cpu='+flags+'\n']+comlines
                    comfile.write(''.join(comlines))
                    comfile.truncate()
                    comfile.close()
#put %cpu=n,n+1,n+2... in the com file
        try:
            a=cmde.index('mpirun')
            #find where mpirun is
            flagstring=flagstr
            if jobtype != 'mpi':
                flagstring=" "
        except:
            try:
                a=cmde.index('mdrun')
                #find where mdrun is
                flagstring=flagstr
                if jobtype != 'gmx':
                    flagstring=" "
            except:
                a=0
                flagstring=" "
                #if neither of those are in the command, idk what to do, so i don't try to pin
        blankstr=""
        for i in range(0,a+1,1):
            blankstr+=cmde[i]+' '
        blankstr+=flagstring+' '
        for i in range(a+1,len(cmde),1):
            blankstr+=cmde[i]+' '
        with open('pyn.log','a') as pyn:
            pyn.write('executing command: '+blankstr+'\n')
        system(blankstr)
        #rebuild the command with the pinning instructions added and execute

    starting=False

if starting is False:
    
    a=sleepcycle(jobid)
    ready=False

    while not ready:

        running=popen("top -d 0 -n 1 -b").read()

        jobs=running.split('\n')

        for j,tem in enumerate(jobs):
            if tem.startswith("PID"):
                start=j+1
            break

        jobs=[thing.split()[-1] for thing in jobs[j:] if thing != '' and thing.split()[7] == 'R']

        if jobs.count("pynscheduling.p") + jobs.count("top") > 1:
            with open('pyn.log','a') as pyn:
                pyn.write('top or another instance of this program is running:\n')
                for job in jobs:
                    pyn.write(job+'\n')
                pyn.write("performing sleep cycle\n")
            a=sleepcycle(jobid)
        else:
            ready=True
    
    system('sed -i -e "s/'+"'"+str(jobid)+"'"+'/'+"'"+"unpinned"+"'"+'/g" /home/scha0275/.pinning/'+nodename)
