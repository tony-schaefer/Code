def listdir(d):
    from os import listdir, remove

    remove('~/.listdir')

    l=listdir(d)

    with open('~./listdir','a') as f:
        for i in l:
            f.write(i+'\n')

def numfiles(d):
    from os import listdir, remove

    remove('~/.listdir')

    l=listdir(d)

    with open('~./listdir','a') as f:
        f.write(len(l))
    
