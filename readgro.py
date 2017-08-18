def groread(filename,frame): #read frame from gro trajectory
    '''groread(filename, frame number), returns dictionary if frame is read successfully, False otherwise\n The dictionary contains:\n header:header \n natoms:number of atoms \n molnum:list of molecule numbers \n molres:list of molecule names \n atres:list of atom names \n atnum:list of atom numbers \n xyz:list of position vectors (as a list of lists) \n box:rectangular box dimensions'''
    outstuff={"header":'header',"natoms":0,"molnum":[],"molres":[],"atres":[],"atnum":[],"xyz":[],"box":[0.,0.,0.]} #initialize output dictionary
    
    from linecache import getline

    header = getline(filename,1).strip() #grap header 
    natoms = int(getline(filename,2).strip()) #grap number of atoms

    outstuff['header']=header

    outstuff["natoms"]=natoms

    start=frame*(natoms+3)+3 #starting line for atoms is this
    stop=start+natoms #last line of atoms is this

    try:
        for i in range(start,stop):
            line=getline(filename,i) # get the line and split it up
            outstuff["molnum"].append(int(line[0:5]))
            outstuff["molres"].append(line[5:9])
            outstuff["atres"].append(line[11:15])
            outstuff["atnum"].append(int(line[15:20]))
            outstuff["xyz"].append([float(j) for j in line[22:44].split()])

        outstuff["box"]=[float(j) for j in getline(filename,stop).split()] # get box dimensions 

        return outstuff
    
    except:
        return False # return False if something doesn't work

def growrite(filename,info):
    '''growrite(filename, info), returns True if file is written, False otherwise; "info" is a dictionary containing the following:\n header:header \n natoms:number of atoms \n molnum:list of molecule numbers \n molres:list of molecule names \n atres:list of atom names \n atnum:list of atom numbers \n xyz:list of position vectors (as a list of lists) \n box:rectangular box dimensions'''
    try:
        with open(filename,'w') as gro:
            gro.write("%s\n" %(info['header']))
            gro.write("%5i \n" % (info['natoms']))
            writelist=[]
            for i in range(0,len(info['atnum'])):
                writelist.extend("%5i%-4s  %4s%5i  %6.3f  %6.3f  %6.3f\n" %(info['molnum'][i], info['molres'][i], info['atres'][i], info['atnum'][i], info['xyz'][i][0], info['xyz'][i][1], info['xyz'][i][2]))
            gro.write(''.join(writelist))
            gro.write("%10.5f %10.5f %10.5f\n" % (info['box'][0], info['box'][1], info['box'][2]))
        return True
    except:
        return False

def pdbread(filename,frame): #read frame from gro trajectory
    '''pdbread(filename, frame number), returns dictionary if frame is read successfully, False otherwise\n The dictionary contains:\n natoms:number of atoms \n molnum:list of molecule numbers \n molres:list of molecule names \n atres:list of atom names \n atnum:list of atom numbers \n xyz:list of position vectors in nm (as a list of lists) \n box: CRYST1 info'''
    outstuff={"natoms":0,"molnum":[],"molres":[],"atres":[],"atnum":[],"xyz":[],"box":[0.,0.,0.]} #initialize output dictionary
    
    with open(filename,'r') as pdb:
        lines=pdb.readlines()

    ends=[]
    for i,line in enumerate(lines):
        if line.startswith('END'):
            ends.append(i)

    if len(ends) <= 1:
        ends=[0,len(lines)-1]

    outstuff['natoms']=0

    try:
        for line in lines[ends[frame]:ends[frame+1]]:
            if line.startswith('ATOM') or line.startswith('HETATM'):
                outstuff['natoms']+=1
                outstuff["atnum"].append(int(line[6:11]))
                outstuff["atres"].append(line[13:16])
                outstuff["molres"].append(line[17:20])
                outstuff["molnum"].append(int(line[22:26]))
                outstuff["xyz"].append([float(j)/10.0 for j in line[32:54].split()])
        
            if line.startswith('CRYST'):
                outstuff["box"]=[j for j in line.strip().split()[1:]] # get box dimensions 
                for i,tem in enumerate(outstuff["box"]):
                    try:
                        outstuff["box"][i]=float(tem)
                    except:
                        pass

        return outstuff
    
    except:
        return False # return False if something doesn't work

