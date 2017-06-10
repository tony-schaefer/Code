#!/share/apps/amber16/miniconda/bin/python2.7

from numpy import array,append

n=2
k=0
primelist=array([])
shortlist=array([])

while True:
    j=0
    isprime=True
    for num in primelist:
        if n % num == 0:
            isprime=False
            break
    if isprime:
        primelist=append(primelist,n)
        shortlist=append(shortlist,str(n))
        if len(shortlist) > 1000:
            print '\n'.join(shortlist)
            shortlist=array([])
    n+=1
