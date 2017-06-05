
import random
import copy
from context import *
from cleanStevesData_byParticipant2 import *
import math

matching_set = [('(', ')'), ('[', ']')]
open_set = ["(", "["]
closed_set = [")", "]"]
all_p = ['[', '(', ')', ']']

types = ['O', 'C', 'M'] + all_p



def get_pos_paren(d, length=1):
	dcts = [{} for i in xrange(4-length+1)]

	all_conj = copy.copy(all_p)
	while True:
		if length == 1:
			break
		else:
			new_conj = []
			for c in all_conj:
				for k in all_p:
					new_c = c + k
					new_conj.append(new_c)
			length -= 1
			all_conj = copy.copy(new_conj)
	for dct in dcts:
		for p in all_conj:
			dct[p] = 0

	for k in d:
		for i in xrange(len(dcts)):
			dct = dcts[i]
			which = "".join(list(k[i:i+length]))
			dct[which] += d[k]

	return dcts

def get_hyps(prims, length=4):
	if length == 1:
		return prims

	else:
		new_hs = []
		for h in get_hyps(prims, length-1):
			for p in prims:
				new_hs.append(h + p)
		return copy.deepcopy(new_hs)

def find_match(m, available):
    for r in available:
        if (m, r) in matching_set or (r, m) in matching_set:
            return r
    return None

def get_hyp_gen(hyp, available,
				 open_available, 
				 closed_available, 
				 sofar=""):
	
	if len(hyp) == 0:
		return {"":1.0}

	else:
		h = hyp[0]
		poss = {}
		if h in "([])" and h in available:
			poss[h] = 1.0

		elif h == "O" and len(open_available)>0:
			for o in open_available:
				poss[o] = 1/float(len(open_available))

		elif h == "C" and len(closed_available)>0:
			for o in closed_available:
				poss[o] = 1/float(len(closed_available))

		elif h == "M" and len(closed_available) > 0:
			for s in sofar[::-1]:
				match = find_match(s, available)
				if (match in closed_available):
					poss[match] = 1.0
					break

		if len(poss.keys()) == 0:
			poss["*"] = 1.0


		ret_dcts = {}
		for key in poss:
			new_av = copy.copy(available)
			new_opav = copy.copy(open_available)
			new_clav = copy.copy(closed_available)
			new_sofar = sofar + key
			if key in new_av:
				new_av.remove(key)
				if key in new_opav:
					new_opav.remove(key)
				else:
					new_clav.remove(key)

			from_here = get_hyp_gen(hyp[1:], 
									new_av,
									new_opav,
									new_clav,
									new_sofar)
			prob_key = poss[key]
			for k in from_here:
				prob_here =from_here[k]
				if key + k not in ret_dcts:
					ret_dcts[key + k] = 0.0
				ret_dcts[key+k] += prob_here * prob_key

		return ret_dcts


def get_hyps_gen(hyps):
	r_dct = {}
	for hyp in hyps:
		hyp_gen = get_hyp_gen(hyp, 
			copy.copy(all_p),
			copy.copy(open_set), 
			copy.copy(closed_set))

		r_dct[hyp] = copy.deepcopy(hyp_gen)
	return r_dct

def sim(p1, p2):
	s = 0
	if len(p1) == len(p2):
		for p in xrange(len(p1)):
			if p1[p] == p2[p]:
				s += 1
	return s

def prob_h_given_d(hyp_gen, data, sm=1e-10):
	n = sum([data[d] for d in data])
	#hyp_gen = hypothesis[hypothesis.keys()[0]]

	#n_choose_k = (lambda tup: 
			#		math.factorial(tup[0])/
				#	(float(math.factorial(tup[0] -tup[1])) *
				#		math.factorial(tup[1])))

	p_h = 1.0
	hyp_gen_use = copy.deepcopy(hyp_gen)
	sum_val = float(sum([data[k] for k in data]))
	z = 0.0
	for d in data:
		if d not in hyp_gen_use:
			hyp_gen_use[d] = 0.0
		hyp_gen_use[d] += sm/sum_val

	for h in hyp_gen_use:
		z += hyp_gen_use[h]

	for d in data:
		#p_h += math.log(sm)
		p_h += math.log(hyp_gen_use[d]/z) * (data[d]) 
		#p_h += math.log(hyp_gen[d]) * data[d] 

	return p_h

def logsumexp(v):
    m = max(v)
    return m+math.log(sum(map( lambda x: math.exp(x-m), v)))

def run():
	#at_p = get_pos_paren(data, length=1)
	hypotheses = get_hyps(types, length_hyps)
	out_hyps = get_hyps_gen(hypotheses)
	all_data_bef =getCountData(file, careAbout, who, subset)
	all_data = []
	for pers in all_data_bef:
		new_dct = {}
		for key in pers:
			new_dct["".join(list(key))] = pers[key]
		all_data.append(copy.deepcopy(new_dct))

	
	for d in all_data:
		print "*"*50
		hyps = []
		for k in out_hyps.keys():
			h = out_hyps[k]
			prob_h_d = prob_h_given_d(h, d, sm=0.1)
			hyps.append((k, copy.deepcopy(h), prob_h_d))

		z = logsumexp([x[2] for x in hyps])

		hyps = sorted(copy.deepcopy(hyps), 
			key=lambda tup:-tup[2])
		hyps = [(x[0], x[1], math.exp(x[2]-z)) for x in hyps][:top_n]

		hyps = hyps[:top_n]
		print d
		for h in hyps:
			print h[0],h[2]

			print

		print "*" * 50
		print




if __name__ == "__main__":
	length_hyps=4
	top_n = 10
	who = "Monkeys"
	careAbout = "Order pressed"
	if who == "Kids":
		file = "stevesdata/RecursionKids.csv"
		start = 0 
		subset= {}

	elif who == "Monkeys":
		file = "stevesdata/RecursionMonkey.csv"
		#outhyps = "output/MonkeyHyps.csv"
		start = 100
		#subset = {}
		subset={"Exposure": "2"}

	else:
		file =  "stevesdata/RecursionTsimane.csv"
		start = 1000
		subset= {}



	run()

