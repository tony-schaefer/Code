#!/share/apps/python/bin/python2.7

import sys
import math

calc = "z="
ansfile = open("/home/scha0275/program-workshop/.ans.out","rw+")
anslines = ansfile.readlines()

for line in anslines:
    exec line

try:
    ans = float(ans)
except:
    ans = 0.0

for thing in sys.argv:
    if str(thing) != sys.argv[0]:
        calc+=str(thing)

def acos(thing):
    return math.acos(thing)
def sin(thing):
    return math.asin(thing)
def cos(thing):
    return math.atan(thing)
def cos(thing):
    return math.cos(thing)
def sin(thing):
    return math.sin(thing)
def tan(thing):
    return math.tan(thing)
def ln(thing):
    return math.log(thing)
def log(thing):
    return math.log10(thing)
def sqrt(thing):
    return math.sqrt(thing)

calc = calc.replace("x","*")
calc = calc.replace("^","**")
calc = calc.replace("v","(")
calc = calc.replace("b",")")
calc = calc.replace("pi","math.pi")
calc = calc.replace("e","math.e")

print calc[2:]

#try:
if 2 > 1:
    exec calc

    print z

    if not ansfile.closed:
        for line in anslines:
            if line[:4] == "ans=":
                ansfile.write("ans="+str(z))
            else:
                ansfile.write(line)
        ansfile.close()
#except:
#    print "unable to perform operation"
    if not ansfile.closed:
        ansfile.close()
