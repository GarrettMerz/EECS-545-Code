#!/usr/bin/python

import sys
from math import *

use_uglm = True

uglm = {}

for line in open("/afs/csail.mit.edu/u/b/bsnyder/scratch/ugaritic/ugaritic.data"):
    fields = line.rstrip().split("\t")
    ug = fields[0].split()[-1].replace("#","").replace("-","")
    uglm["END"] = uglm.get("END",0.0) + 1.0
    for u in ug:
        uglm[u] = uglm.get(u,0.0) + 1.0

n = sum(uglm.values())
for u in uglm.keys():
    uglm[u] = uglm[u] / n


def get_noncog_prob(ug):
    if use_uglm:
        probs = map(lambda u: uglm[u], ug)
        return reduce(lambda x, y: x * y, probs, uglm["END"])
    else:
        ug_len = float(len(ug))
        return exp (ug_len * (log(1) - log(31)))

probs = map(float,open("viterbi_probs").readlines())

i = 0
for line in open("/afs/csail.mit.edu/u/b/bsnyder/scratch/ugaritic/ugaritic.data"):
    fields = line.rstrip().split("\t")
    ug = fields[0].split()[-1].replace("#","").replace("-","")
    # print ug
    noncog_prob = get_noncog_prob(ug)
    cog_prob = probs[i]
    if len(fields) > 1:
        cog_str = "!!!"
    else:
        cog_str = "XXX"
    
    print cog_str,`(cog_prob/noncog_prob)`
    i += 1