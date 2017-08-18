#!/share/apps/amber16/miniconda/bin/python2.7

import sys

def chbase(b1,b2,innum):
    """chbase( starting base, final base, number in starting base)
        for bases 1-10, Arabic numerals are used
        for bases 11-36, the uppercase ISO basic Latin alphabet is added
        for bases 37-62, the lowercase ISO basic Latin alphabet is added"""
    numbers=['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
    
    if innum.startswith('-'):
        innum=innum[1:]
        starting='-'
    else:
        starting=''

    if any( numbers.index(n) > b1 or n not in numbers for n in list(innum) ):
        print innum,'not in base',b1
        print b1,'consists of',''.join(numbers[:b1])
        sys.exit()
    if b1 > len(numbers):
        print 'base cannot exceed '+str(len(numbers))
        sys.exit(1)
    elif b1 < 1:
        print 'base must be greater than 0'
        sys.exit(1)
    if b2 > len(numbers):
        print 'base cannot exceed '+str(len(numbers))
        sys.exit(1)
    elif b2 < 1:
        print 'base must be greater than 0'
        sys.exit(1)

    innum=str(innum)

    inlist=[numbers.index(dig) for dig in innum]

    i=len(inlist)-1
    n=0
    b10=0
    while i >= 0:
        b10+=(inlist[n])*(b1**i)
        i-=1
        n+=1

    bglist=[]
    q=[b10]
    r=[]

    if b2 == 1:
        i=0
        while i < b10:
            r.append(1)
            i+=1
        if b10 == 0:
            return ''
    else:

        while True:
            a=q[-1]//b2
            r.append(q[-1]%b2)
            q.append(a)
            if a == 0:
                break
   
    r.reverse()
    
    bg=starting+''.join(numbers[num] for num in r)

    return bg

if __name__ == "__main__":

    print 'enter starting base'
    b1=int(raw_input(''))
    print 'enter a number in that base'
    innum=raw_input('')
    print 'enter target base'
    b2=int(raw_input(''))

    print innum+' in base '+str(b1)+' is '+chbase(b1,b2,innum)+' in base '+str(b2)
