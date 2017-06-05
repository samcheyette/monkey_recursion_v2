import string 
import random
import copy
import os
from context import *

def isValidCenterEmbed(parens, matching_set):
    if len(parens) == 0:
        return True
    else:
        if (((parens[0], parens[len(parens) - 1]) not in matching_set)
            or parens[0] in parens[1:] or parens[len(parens) - 1] in parens[:len(parens) - 1]):
            return False
        else:
            return isValidCenterEmbed(parens[1:len(parens)-1], matching_set)

def simulateData(parens, matching_set, randomValidLsts = [], pC=1.0,pT=0.0, N=12):
    allParens = {}
    for i in xrange(len(parens)):
        for j in xrange(len(parens)):
            for k in xrange(len(parens)):
                for l in xrange(len(parens)):
                    ps = (parens[i], parens[j], parens[k], parens[l])
                    if isValidCenterEmbed(ps, matching_set):
                        allParens[ps] = int((pC / 2.0) * N)

                    elif pT > 0.0 and ps in randomValidLsts:
                        allParens[ps] = int((pT/float((len(randomValidLsts)))) * N)


                    elif pC + pT < 1.0:
                        allParens[ps] = int((1.0 - pT - pC) * N)


    return allParens


def get_distribution(hyps, samp=2500):
    dct={}

    for hp in hyps:
        h = hp[0]
        p = hp[1]
        for s in xrange(samp):
            out = h()
            if out not in dct:
                dct[out] = 0.0
            dct[out] += p/float(samp)

    return dct


def get_traces(hyps, samp=2500):
    dct={}

    for hp in hyps:
        h = hp[0]
        p = hp[1]
        for s in xrange(samp):
            call = h()
            out = h.history[:4]
            if out not in dct:
                dct[out] = 0.0
            dct[out] += p/float(samp)

    return dct


def get_probs_from_htype(htype, samp=2500):
    dct = {"CE":0.0, "CR":0.0, "TR":0.0}
    open_set = ["(", "["]
    closed_set = [")", "]"]
    matching_set = [("(", ")"), ("[", "]")]
    ind =0 
    for _ in xrange(samp):
        C = Context(open_set, closed_set, matching_set)
        C.set_params(mem_error=0.0)
        for h in htype:
            if h in "([])":
                C.pick_specific(h)
            elif h == "O":
                C.pick_open()
            elif h == "C":
                C.pick_closed()
            elif h == "M":
                C.pick_match()
            else:
                assert(False)
        ret = tuple(C.slots)
        if len(ret) == 4:
            if ret in [("[", "(", ")", "]"), 
                ("(", "[", "]", ")")]:
                dct["CE"] += 1
            elif ret in [("[", "(", "]", ")"), 
                ("(", "[", ")", "]")]:
                dct["CR"] += 1
            elif ret in [("[", "]", "(", ")"), 
                ("(", ")", "[", "]")]:
                dct["TR"] += 1
            ind += 1


    if ind > 0:
        dct["CE"] = dct["CE"]/float(ind)
        dct["CR"] = dct["CR"]/float(ind)
        dct["TR"] = dct["TR"]/float(ind)

    return dct

def output1(outhyps, who, start, 
            dat, mod_out, mod_distr, trace):

    out = ""
    app = False

    if outhyps != None:
        if os.path.isfile(outhyps):
            out_f = open(outhyps, "a+")
        else:
            out = "Who, Participant, HorM, which, var, P\n"
            #out += "P_CE, P_CR, P_TR\n"
            out_f = open(outhyps, "w+")

    paren_map = {'(': 'W', '[': 'X', ']': 'Y', ')': 'Z'}

    norm = float(sum([dat[k] for k in dat]))

    for d in set(dat.keys() + mod_distr.keys()):
        conv = "".join([paren_map[j] for j in d])

        p_dat = 0.0
        if d in dat:
            p_dat = dat[d]/norm
        out += ("%s, %s, %s, %s, %s, %f\n" %
                 (who, start, "human", "distribution",
                         conv, p_dat))

        p_mod = 0.0
        if d in mod_distr:
            p_mod = mod_distr[d]
        out += ("%s, %s, %s, %s, %s, %f\n" %
                 (who, start, "model", "distribution",
                         conv, p_mod))
    for d in trace:
        conv = ""
        for z in d:
            if z in paren_map:
                conv += paren_map[z]
            else:
                conv += z

        p_dat = 0.0

        p_htype=get_probs_from_htype(d)
        pce = p_htype["CE"]
        pcr = p_htype["CR"]
        ptr = p_htype["TR"]
        otr =1.0 - pce - pcr - ptr

        p_mod = 0.0
        if d in trace:
            p_mod = trace[d]
        out += ("%s, %s, %s, %s, %s, %f\n" %
                 (who, start, "model", "trace",
                         conv, p_mod))

        p_mod = 0.0
        if d in trace:
            p_mod = trace[d]
        out += ("%s, %s, %s, %s, %s, %f\n" %
                 (who, start, "CE", "trace",
                         conv, pce))
        out += ("%s, %s, %s, %s, %s, %f\n" %
                 (who, start, "CR", "trace",
                         conv, pcr))
        out += ("%s, %s, %s, %s, %s, %f\n" %
                 (who, start, "TR", "trace",
                         conv,  ptr))
        out += ("%s, %s, %s, %s, %s, %f\n" %
                 (who, start, "OT", "trace",
                         conv,  otr))


    if outhyps != None:
        out_f.write(out)
        out_f.close()


if __name__ == "__main__":
    ht = "([CC"
    ph = get_probs_from_htype(ht)
    print ph
    ht = "([MC"
    ph = get_probs_from_htype(ht)
    print ph
    ht = "OOMM"
    ph = get_probs_from_htype(ht)
    print ph
    ht = "(OCM"
    ph = get_probs_from_htype(ht)
    print ph 
    ht = "()OM"
    ph = get_probs_from_htype(ht)
    print ph 