#!/share/apps/amber16/miniconda/bin/python2.7

from sys import argv
from os import system, environ, getenv
from time import sleep
#if this ever breaks, don't bother fixing it; it's too stupid and impossible to get it to work 100% of the time
#add a delay between your job submissions to make it work more often
#maybe you want to because performance is better idk

starting=False
pinned="pinned"
unpinned="unpinned"
# i forget quotes sometimes

gromacstypes=['gmx']
jobtype='mpi'
# assume job is an mpi job

try:
    from socket import gethostname
    nodename=gethostname()
#this might not work depending on where the code actually get executed (idk), you can pass the hostname on the command line
except:
    pass

for thing in argv[1:]:
    if thing.lower() == 'starting':
        starting=True
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
        #compute name the job will run on (in case the socket thing above didn't work, i haven't actually tested it)
    if thing.lower().startswith('cmd='):
        cmdfile=thing.split('=')[1]
        #file with a list of commands to execute, 1 command per line

#example: echo "gmx mdrun -nt $NUMPROC > cmd.file"
#example: qsub << eof 
#example: et cetera 
#example: ./pynscheduling.py starting jid=\$JOB_ID procs=$NUMPROC cmd=cmd.file type=gmx host=\$HOSTNAME
#example: >> eof
#example sleep 5


if starting:
    mypins=[]
#########i'm almost sure the following code works as intended
# k i'm actually not very sure
# only like 50% sure
    if not getenv('whospynning',False):
        environ['whospynning']='-1'
    while environ['whospynning'] != '-1':
        sleep(10)
    while environ['whospynning'] != str(jobid):
        sleep(5)
        environ['whospynning'] = str(jobid)
    while environ['whospynning'] != str(jobid):
        sleep(5)
        environ['whospynning'] = str(jobid)
# sleep if another instance of the program is running on the host

    flags=[' ']
    #idk what to do if you aren't running in parallel
    i=0
    pinthese=[]
    #list of procs that the mpirun/mdrun commands will be bound to
    j=0
    diditwork=False
    while diditwork is False:
        pinlist=open('/home/scha0275/.pinning/'+nodename,'r+')
        filelines=pinlist.readlines()
        for thing in filelines:
            mypins.append('')
            exec thing.strip()
        pinlist.close()
# file should only have 32 lines which is a list of what's currently pinned 
# mypins=[['63214'],...['unpinned']]
#            ^job id          ^ not pinned; index is processor id or whatever it's called \_('')_/ 
    #read scratch file that contains info about what's bound to what proc
# there's a sed command that flags the procs in the scratch file as pinned. if that didn't work, try stuff over again
        if jobtype not in gromacstypes and procs >= 2:
        #job is not gmx means job is mpirun 
            while j < (32-procs/2):
                if procs > 2:
                #floor divides don't work with < 4, but why would you use 3?
                    templist=mypins[j:j+(procs//4)]+mypins[j+16:j+16+(procs//4)]
                    #pick consecutive cores
                    #http://bit.ly/2j8X46E or ask Tony for what this loading looks like
                    for k in range(0,len(templist)-1,1):
                        if templist[k][0] != 'unpinned':
                            break
                        elif k == len(templist)-1:
                            pinstride=1
                            pinoffset=2*j
                            pinthese=range(j,j+(procs//4)+2,1)+range(j+16,j+(procs//4)+18,1)
#rewrite this parth with 6 procs in mind
                            flags=['--bind-to-socket','-bysocket','-slotlist',','.join(str(slot) for slot in range(j,(j+procs//2)))]
                    if flags != [' ']:
                        break    
                        #quit once we found a place to put this stuff
                else:
                    #similar to above, but without floors (//) and stuff
                    templist=[mypins[j]]+[mypins[j+16]]
                    for k in range(0,len(templist),1):
                        if templist[k][0] != 'unpinned':
                            break
                        elif k == len(templist)-1:
                            pinstride=1
                            pinoffset=2*j
                            pinthese=[j,j+16]
                            flags=['--bind-to-socket','-bysocket','-slotlist',','.join(str(slot) for slot in range(j,(j+procs//2)))]
                    if flags != [' ']:
                        break
                j+=1

        elif jobtype in gromacstypes and procs >=2:
        #gromacs mdrun benefits from hyperthreading
            while j < (32-procs/2):
                if mypins[j][0] == unpinned and j < (16-procs)+1:
                #check thread 0 to see if it has $procs consecutive procs
                    for k in range(j,j+procs,1):
                        if mypins[k][0] != 'unpinned':
                            break
                        elif k == j+procs-1:
                            pinstride=2
                            pinoffset=2*j
                            pinthese=range(j,j+procs)
                            flags=['-pinstride',pinstride,'-pinoffset',pinoffset,'-pin on']
                    if flags != [' ']:    
                        break
                        #quit once it finds something
                elif mypins[j][0] == unpinned and j < (32-procs)+1 and j > 15:
                #check thread 1 ""
                    for k in range(j,j+procs,1):
                        if mypins[k][0] != unpinned:
                            break
                        elif k == j+procs-1:
                            pinstride=2
                            pinoffset=2*(j-16)+1
                            pinthese=range(j,j+procs)
                            flags=['-pinstride',pinstride,'-pinoffset',pinoffset,'-pin on']
                    if flags != [' ']:
                        break
                    elif j < (16-procs/2) and j%2 == 0:
#check cores, this loading won't benefit from hyperthreading, see spreadsheet/Tony
                        if  procs > 2:
                            templist=mypins[j:j+(procs//4)]+mypins[j+16:j+16+(procs//4)]
                            for k in range(0,len(templist),1):
# rewrite with 6 procs in mind
                                if templist[k][0] != 'unpinned':
                                    break
                                elif k == len(templist)-1:
                                    pinstride=1
                                    pinoffset=2*j
                                    pinthese=range(j,j+(procs//4)+1,1)+range(j+16,j+17+(procs//4),1)
                                    flags=['-pinstride',pinstride,'-pinoffset',pinoffset,'-pin on']
                        else:
                        # 2 core method, no floors
                            templist=[mypins[j]]+[mypins[j+16]]
                            for k in range(0,len(templist),1):
                                if templist[k][0] != unpinned:
                                    break
                                elif k == len(templist)-1:
                                    pinstride=1
                                    pinoffset=2*j
                                    pinthese=[j,j+16]
                                    flags=['-pinstride',pinstride,'-pinoffset',pinoffset,'-pin on']

                j+=1
        else:
            flags=[' ',' ']
            #we didn't find anything
            logfile=open('log.file','w')
            logfile.write('no procs could be scheduled\n')
            logfile.close()
            #keep track of when things didn't work

        if len(pinthese) > 0:
            for thing in pinthese:
                system('echo '+str(thing)+' >> log.file')
                print str(jobid)
                a=system('sed -i -e "s/mypins\['+str(thing)+'\]= \[\'unpinned/mypins\['+str(thing)+'\]= \[\''+str(jobid)+'/g" /home/scha0275/.pinning/'+nodename)
                system('cat /home/scha0275/.pinning/'+nodename)
                if a != 0:
                    diditwork=False
                    sleep(5)
                    system('echo sed did not work >> log.file')
                    system('sed -i -e "s/'+str(jobid)+'/unpinned/g" /home/scha0275/.pinning/'+nodename)
                    break
                    #listing procs as in-use didn't work, get rid of anything we said we were going to use and go try to find new procs
                else:
                    system('echo sed worked >> log.file')
                    diditwork=True
        else:
            diditwork=True
            flags=[' ']
        #update pin scratch file using sed because it might be faster, idk
    
    environ['whospynning'] = str(-1)

    flags=[str(thing) for thing in flags]
    flagstr=' '.join(flags)
    #convert flags to something that can be used on the command line
    cmdf=open(cmdfile,'r')
    cmdl=cmdf.readlines()
    cmdf.close()
    #read the commands we are supposed to execute
    cmdl=[thing.strip() for thing in cmdl]
    
    for thing in cmdl:
        cmde=thing.split()
        try:
            a=cmde.index('mpirun')
            #find where mpirun is
        except:
            try:
                a=cmde.index('mdrun')
                #find where mdrun is
            except:
                a=0
                flagstr=" "
                #if neither of those are in the command, idk what to do, so i don't try to pin
        blankstr=""
        for i in range(0,a+1,1):
            blankstr+=cmde[i]+' '
        blankstr+=flagstr+' '
        for i in range(a+1,len(cmde),1):
            blankstr+=cmde[i]+' '
        system(blankstr)
        #rebuild the command with the pinning instructions added and execute

    starting=False

if starting is False:
    a=system('sed -i "s/\''+str(jobid)+'\'/\'unpinned\'/g" /home/scha0275/.pinning/'+nodename)
    if a != 0:
        system("echo ending sed didn't work "+str(jobid)+" >> log.file")

#list the procs as available when done
