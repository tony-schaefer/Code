#!/share/apps/amber16/miniconda/bin/python2.7

import sys

logfile=open(sys.argv[1],'r')
loglines=logfile.readlines()
logfile.close()

comfile=open('new.com','w+')

atoms=0
elem={'1':'H','2':'He','3':'Li','4':'Be','5':'B','6':'C','7':'N','8':'O','9':'F','10':'Ne','11':'Na','12':'Mg','13':'Al','14':'Si','15':'P','16':'S','17':'Cl','18':'Ar','19':'K','20':'Ca','26':'Fe','35':'Br','53':'I'}
readytoread=-6
writelist=[]
frame=0

loglines=[thing.strip() for thing in loglines]

for thing in loglines:
    try:
        if thing.split()[-1] == "orientation:":
            readytoread=-5
            writelist=[]
    except:
        pass
    if readytoread > -6:
        readytoread+=1
    if thing.startswith('-') and readytoread >= 1:
        readytoread=-6
        atoms=0
    if readytoread == 1:
        frame+=1
    if readytoread >= 1:
        atoms+=1
        center=elem[thing.split()[1]]
        x=float(thing.split()[3])
        y=float(thing.split()[4])
        z=float(thing.split()[5])
        writelist.extend(("%-2s      %10.7f     %10.7f     %10.7f\n" % (center,x,y,z)))
 

comfile.write(''.join(writelist))
comfile.write('\n')
comfile.close()

