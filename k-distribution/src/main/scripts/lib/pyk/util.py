#!/usr/bin/env python3

from   datetime import datetime
import hashlib
import sys
import time

_lastTime = time.time()

def combineDicts(*dicts):
    if len(dicts) == 0:
        return {}
    if len(dicts) == 1:
        return dicts[0]
    dict1 = dicts[0]
    dict2 = dicts[1]
    restDicts = dicts[2:]
    if dict1 is None or dict2 is None:
        return None
    intersecting_keys = set(dict1.keys()).intersection(set(dict2.keys()))
    for key in intersecting_keys:
        if dict1[key] != dict2[key]:
            return None
    newDict = { key : dict1[key] for key in dict1 }
    for key in dict2.keys():
        newDict[key] = dict2[key]
    return combineDicts(newDict, *restDicts)

def findCommonItems(l1, l2):
    common = []
    for i in l1:
        if i in l2:
            common.append(i)
    newL1 = []
    newL2 = []
    for i in l1:
        if not i in common:
            newL1.append(i)
    for i in l2:
        if not i in common:
            newL2.append(i)
    return (common, newL1, newL2)

def dedupe(ls):
    newL = []
    for l in ls:
        if l not in newL:
            newL.append(l)
    return newL

def notif(msg):
    global _lastTime
    curTime   = time.time()
    diffTime  = curTime - _lastTime
    _lastTime = curTime
    sys.stderr.write('== ' + sys.argv[0].split('/')[-1] + ' [+' + '{0:8.2f}'.format(diffTime) + ']: ' + msg + '\n')
    sys.stderr.flush()

def warning(msg):
    notif('[WARNING] ' + msg)

def fatal(msg, exitCode = 1):
    notif('[FATAL] ' + msg)
    raise('Quitting')

def genFileTimestamp(comment = '//'):
    return comment + ' This file generated by: ' + sys.argv[0] + '\n' + comment + ' ' + str(datetime.now()) + '\n'

def getAppliedAxiomList(debugLogFile):
    axioms      = []
    next_axioms = []
    with open(debugLogFile, 'r') as logFile:
        for line in logFile:
            if line.find('DebugTransition') > 0:
                if line.find('after  apply axioms:') > 0:
                    next_axioms.append(line[line.find('after  apply axioms:') + len('after  apply axioms:'):])
                elif len(next_axioms) > 0:
                    axioms.append(next_axioms)
                    next_axioms = []
    return axioms

def strHash(k):
    hash = hashlib.sha256()
    hash.update(str(k).encode('utf-8'))
    return str(hash.hexdigest())