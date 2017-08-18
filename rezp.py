#!/share/apps/amber16/miniconda/bin/python2.7

import sys
import os

i=0
givehelp=False
charge="0"

if len(sys.argv[:]) < 3:
    givehelp=True

if givehelp:
    print "make a mol2 file from a pdb and qm output and for am1-bcc"
    print "-f : input pdb file, atoms must be in same order as qm out"
    print "-o : output mol2 file"
    print "-e : qm output for ESP"
    print "-c : (0) net charge"
    print "-h : help"
    sys.exit()

while i < len(sys.argv):
    if sys.argv[i][:2] == "-h":
        givehelp=True
    if sys.argv[i] == "-f":
        inputfile=sys.argv[i+1]
    if sys.argv[i] == "-o":
        outputfile=sys.argv[i+1]
    if sys.argv[i] == "-e":
        esp=sys.argv[i+1]
    if sys.argv[i] == "-c":
        charge=sys.argv[i+1]
    i+=1

os.system("antechamber -i "+inputfile+" -fi pdb -fo mol2 -o am1bcc.mol2 -c bcc -s 2 -nc "+charge)
os.system("espgen -i "+esp+" -o "+esp+".dat")
os.system("cp am1bcc.mol2 "+outputfile[:-5]+"-am1bcc.mol2")

am1bcc=open("am1bcc.mol2","r")

state="start"
atoms=0
atnum=[]
newmol2=[]

for line in am1bcc:
    if state == "tripos":
        state="charges"
    try:
        if line.split()[0] == "@<TRIPOS>ATOM":
            state="tripos"
    except:
        pass
    try:
        if line.split()[0] == "@<TRIPOS>BOND":
            state="bond"
    except:
        pass
    if state == "charges" and line[:1] != "@":
        newmol2.append(line[:72])
        atoms+=1
        if line[50:51] == "h":
            atnum.append("1")
        if line[50:51] == "c":
            atnum.append("6")
        if line[50:51] == "n":
            atnum.append("7")
        if line[50:51] == "o":
            atnum.append("8")
        if line[50:51] == "f":
            atnum.append("9")
        if line[50:51] == "s":
            atnum.append("16")
    else:
        newmol2.append(line[:-1])

am1bcc.close()

respin=open("rezp.in","w")

respin.write("REZP\n")
respin.write(" &cntrl\n")
respin.write(" nmol=1,\n")
respin.write(" qwt=0.0005,\n")
respin.write(" /\n")
respin.write("    1.0\n")
respin.write("REZP\n")
respin.write(("%5s%5s\n" % (charge,str(atoms))))
ls=[]
for j in atnum:
    ls.extend(("%5s    0\n" % j))
respin.write(''.join([k for k in ls]))
respin.write("\n")

respin.close()

os.system("resp -O -i rezp.in -o rezp.out -p rezp.pch -t rezp.chg -e "+esp+".dat")

rezpcharge=[]
pch=open("rezp.pch","r")
for line in pch:
    try:
        if state == "atoms":
            rezpcharge.append(line.split()[3])
    except:
        break
    try:
        if line.split()[0] == "NO":
            state="atoms"
    except:
        pass
    
pch.close()

ls=[]

mol2file=open(outputfile,"w")
state="start"
i=0

for thing in newmol2:
    if state == "tripos":
        state="charges"
    try:
        if thing.split()[0] == "@<TRIPOS>ATOM":
            state="tripos"
    except:
        pass
    try:
        if thing.split()[0] == "@<TRIPOS>BOND":
            state="bond"
    except:
        pass
    if state == "charges":
        this=("%72s%9f" %(thing,float(rezpcharge[i])))
        ls.append(this)
        i+=1
    elif state != "charges":
        ls.append(thing)

mol2file.write('\n'.join([thing for thing in ls]))
mol2file.close()

extra=["rezp.in","rezp.out","rezp.pch","rezp.chg",esp+".dat"]

#for afile in extra:
#    os.remove(afile)
