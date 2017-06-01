import string
import copy


def getCareAboutPos(careAbout, r):
    # returns the position of the header
    # corresponding to this type of response
    # which we care about!
    d = {}
    for i in xrange(len(r)):
        if r[i] in careAbout:
            d[i] = r[i]
    return d


# def getResps(ids, r):


def getKidsResponses(file, careAbout):
    data = open(file, "r")
    l = data.readline()
    # resps = dict()
    ids = dict()
    lastStart = 0
    c = 0
    allResps = []
    # l = l.replace("\r")
    r = l.split(",")
    # r = r.split("\r")
    # print r
    for i in r:
        if "\r" in i:
            allResps.append(r[lastStart:c + 1])
            lastStart = c
        c += 1

    #retD = {}
    vals = {}

    for k in xrange(len(allResps)):
        if k == 0:
            ids = getCareAboutPos(careAbout, allResps[k])
        else:
            #relies on subject being in
            #first column!
            sub = allResps[k][0]
            sub = sub.split("\r")[1]
            #print sub
            if sub not in vals:
                vals[sub] = []
            for key in ids.keys():
                val = allResps[k][key]
                if val != "":

                    vals[sub].append(val)


    return vals



def getMonkeyTsimaneResponses(file, careAbout, subset={}):
    data = open(file, "r")
    l = data.readline()
    # resps = dict()
    r = l.split(",")
    if len(subset.keys()) > 0:
        k = subset.keys()[0]
        ind_sub = int(r.index(k))
    else:
        ind_sub = -1
    ind = int(r.index(careAbout))
    l = data.readline()
    vals = {}
    while l != "":
        r = l.split(",")
        if (ind_sub == -1) or (r[ind_sub] in subset[k]):
            part = r[0]
            cr = r[ind]
            if part not in vals:
                vals[part] = []
            if cr != "":
                vals[part].append(cr)

        l = data.readline()



    return vals


def getCounts(resps, dictMap):
    allP = {}
    for r in resps:
        tmp = ""
        for a in r:
            paren = dictMap[a]
            tmp += paren + ","
        tmp = tuple(tmp[:len(tmp) - 1].split(","))
        if tmp not in allP:
            allP[tmp] = 0

        allP[tmp] += 1
    return allP


def getCountData(file, careAbout, which, subset={}):
    if which == "Kids":
        careAbout = [careAbout]
        resps = getKidsResponses(file, careAbout)
    else:
        resps = getMonkeyTsimaneResponses(file, careAbout, subset)
    dictMap = {"A": "[", "B": "]", "C": "(", "D": ")"}
    allC = []
    for key in resps.keys():
        cs = getCounts(resps[key], dictMap)
        allC.append(cs)
    # counts = getCounts(resps, dictMap)
    return allC


if __name__ == "__main__":
    file = "stevesdata/RecursionMonkey.csv"
    careAbout = "Order pressed"

    count = getCountData(file, careAbout, "blorb", subset={"Experiment" :
                                                         ["Experiment 2"]})
