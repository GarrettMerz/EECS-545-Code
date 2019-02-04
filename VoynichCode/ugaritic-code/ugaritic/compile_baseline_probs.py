#!/usr/bin/python2.5

import sys, string

em_lines = sys.stdin.readlines()
ug_lines = open("ugaritic.data").readlines()

d = {}
d2 = {}

for i in range(len(em_lines)):
    em = em_lines[i].strip()
    ug = ug_lines[i].split()[1].replace("-","").replace("#","").strip()
    if len(em) != len(ug):
        print em,ug
        continue
    for j in range(len(em)):
        c1 = ug[j]
        c2 = em[j]
        
        if not d.has_key(c1):
            d[c1] = {}
        if not d[c1].has_key(c2):
            d[c1][c2] = 0
        d[c1][c2] = d[c1][c2] + 1
        
        if not d2.has_key(c2):
            d2[c2] = {}
        if not d2[c2].has_key(c1):
            d2[c2][c1] = 0
        d2[c2][c1] = d2[c2][c1] + 1 

print "Hebrew -> Ugaritic"
for h in d2.keys():
    if d.has_key(h):
        a = d[h].items()
        sm = 0.0
        for (u,n) in a:
            sm = sm + n
        a = map(lambda (u,n): (u,n/sm), a)
        a.sort(key=lambda x: x[1], reverse=True)
        print h,a
print
print "Ugaritic -> Hebrew"
for u in d.keys():
    a = filter(lambda (h,n): not h in string.digits, d[u].items())
    sm = 0.0
    for (h,n) in a:
        sm = sm + n
    a = map(lambda (h,n): (h,n/sm), a)
    a.sort(key=lambda x: x[1], reverse=True)
    a = map(lambda (h,n): h + ("%.2f" % (n))[1:], a)
    print u + "\t" + " ".join(a)
