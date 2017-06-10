#!/usr/bin/python
#path to python interpreter

import random
 
#def pyrate():
if 1:
    while 1:
# do this forever
        normal=raw_input("")
# read user input
        words=normal.split()
# split into words
        if "asdfasdf" in words:
            break
# asdfasdf stops the pyrating
        i=0
        while i < len(words):
# change words to pyraty words
            if words[i] == "hi" or words[i] == "hello" or words[i] == "hey" or words[i] == "yo":
                hi=random.randint(0,2)
                if hi == 0: words[i]="ahoy"
                if hi == 1: words[i]="oy"
                if hi == 2: words[i]="yo-ho-ho"
            if words[i][-3:] == "ing" and len(words[i]) > 4:
                words[i]=words[i][:-3]+"in'"
            if words[i][-4:-1] == "ing" and len(words[i]) > 5:
                words[i]=words[i][:-4]+"in'"+words[i][-1:]
            if words[i] == "is" or words[i] == "am" or words[i].lower() == "are":
                words[i]="be"
            if words[i].lower() == "yes" or words[i].lower() == "yeah" or words[i].lower() == "yea" or words[i].lower() == "ya" or words[i].lower() == "yup" or words[i].lower() == "ok" or words[i].lower() == "okay" or words[i].lower() == "yep":
                yes=random.randint(0,2)
                if yes == 0: words[i]="arr"
                if yes == 1: words[i]="aye"
                if yes == 2: words[i]="yarr"    
            if words[i] == "what's":
                words[i]=words[i][:-2]+" be"
            try:
                if words[i][-2:] == "'s" and words[i+2][-4:-1] == "ing":
                   words[i]=words[i][:-2]+" be"
            except:
                pass
            try:
                if words[i][-2:] == "'s" and words[i+2][-3:] == "ing":
                   words[i]=words[i][:-2]+" be"
            except:
                pass
            try:
                if words[i][-2:] == "'s" and words[i+1][-4:-1] == "ing":
                   words[i]=words[i][:-2]+" be"
            except:
                pass
            try:
                if words[i][-2:] == "'s" and words[i+1][-3:] == "ing":
                   words[i]=words[i][:-2]+" be"
            except:
                pass
            if words[i].lower() == "i'm":
                words[i]="me be"
            if words[i] == "you":
                words[i]="ye"
            if words[i] == "wow":
                words[i]="shiver me timbers!"
            if words[i] == "old":
                words[i]="barnacle-covered"
            try:
                if words[i] == "to":
                    words[i+1]="t'"+words[i+1]
                    words.remove("to")
            except:
                pass
            words[i]=words[i].replace("shit","crap-basket")
            if words[i] == "ass":
                words[i]="arse"
            if words[i].lower() == "i":
                words[i]="me"
            if words[i] == "fair" or words[i] == "honest":
                words[i]="square"
            if words[i] == "flag":
                words[i]="colors"
            if i == 0:
                words[i]=words[i][:1].upper()+words[i][1:]
            try:
                if words[i-1][-1:] == "." and words[i-1][-2:] != "..":
                    words[i]=words[i][:1].upper()+words[i][1:]
            except:
                pass
            try:
                if words[i-1][-1:] == "?" or words[i-1][-1:] =="!":
                    words[i]=words[i][:1].upper()+words[i][1:]
            except:
                pass
            i+=1
        print " ".join([word for word in words])
