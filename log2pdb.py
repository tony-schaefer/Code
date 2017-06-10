#!/share/apps/amber16/miniconda/bin/python2.7

import sys

fullwritelist=[]
frame=0
fullfile=open('full.pdb','w')
for thing in sys.argv[1:]:

    logfile=open(thing,'r')
    loglines=logfile.readlines()
    logfile.close()

    pdbfile=open('log2pdb.pdb','w+')

    atoms=0
    elem={'1':'H','2':'He','3':'Li','4':'Be','5':'B','6':'C','7':'N','8':'O','9':'F','10':'Ne','11':'Na','12':'Mg','13':'Al','14':'Si','15':'P','16':'S','17':'Cl','18':'Ar','19':'K','20':'Ca','26':'Fe','35':'Br','53':'I'}
    readytoread=-6
    writelist=[]
    partwritelist=[]
    a=128
    n=0

    loglines=[i.strip() for i in loglines]

    for thang in loglines:
        if thang.endswith("orientation:"):
            readytoread=-5
        if readytoread > -6:
            readytoread+=1
        if thang.startswith('-') and readytoread >= 1:
            readytoread=-6
            atoms=0
            pdbfile.write(''.join(writelist))
            pdbfile.write('ENDMDL\n')
            fullwritelist.extend("MODEL "+str(frame)+'\n')
            fullwritelist.extend(writelist)
            fullwritelist.extend('ENDMDL\n')
            partwritelist.append([])
            partwritelist[-1]=partwritelist[-1]+["MODEL "+str(frame)+'\n']
            partwritelist[-1]=partwritelist[-1]+[''.join(writelist)]
            partwritelist[-1]=partwritelist[-1]+['ENDMDL\n']
            writelist=[]
        if readytoread == 1:
            frame+=1
            pdbfile.write("MODEL "+str(frame)+'\n')
        if readytoread >= 1:
            atoms+=1
            center=elem[thang.split()[1]]
            x=float(thang.split()[3])
            y=float(thang.split()[4])
            z=float(thang.split()[5])
            writelist.extend(("HETATM %4i %2s   UNK     1      %6.3f  %6.3f  %6.3f  1.00  0.00          %2s  \n" % (atoms,center,x,y,z,center)))

        if thang.startswith('-') and readytoread < -7:
            readytoread=-6
            nrgfile=open('file.nrg','w+')
            nrgfile.write(''.join(writelist))
            nrgfile.close()
            writelist=[]
        if readytoread < -8:
            writelist.extend("%10s    %10s \n" %(thang.split()[-1],thang.split()[1]))
            if float(thang.split()[1]) == 0.0:
                n=0
            a+=n
        if readytoread < -6:
            readytoread+=-1
        if thang == "Summary of reaction path following":
            readytoread=-7
    pdbfile.close()

b=partwritelist[a:]
b.reverse()

partwritefile=open('partfile.pdb','w+')
for thing in b:
    partwritefile.write(''.join(thing))
for thing in partwritelist[:a]:
    partwritefile.write(''.join(thing))
partwritefile.close()

fullfile.write(''.join(fullwritelist))
fullfile.close()

