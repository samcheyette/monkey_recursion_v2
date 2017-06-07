
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



def add_noise_to_hyp(hyp_out, p):
	#for now assumes deviation (dev) is 1
	new_hyps = {}
	for h in hyp_out:
		lst=[]

		old_hyp_out = hyp_out[h]
		new_hyps[h] = (1.0 - p) * old_hyp_out
		for i in xrange(len(h)):
			for j in all_p:
				if j != h[i]:
					new_hyp = h[:i] + j + h[i+1:]
					lst.append((new_hyp, old_hyp_out))

		for lp in lst:
			if lp[0] not in new_hyps:
				new_hyps[lp[0]] = 0.0
			new_hyps[lp[0]] += p * lp[1]/len(lst)


	return new_hyps
		

def add_noise(out_hyps, p):
	new_hyps={}

	for h in out_hyps.keys():
		#print h, out_hyps[h]
		out_cop = copy.deepcopy(out_hyps[h])
		new_hyps[h] = add_noise_to_hyp(out_cop, p)

	return new_hyps


def filter_hyps(out_hyps, thresh=2.0):
	keep = {}
	for h in out_hyps:
		badness = 0.0
		for k in out_hyps[h]:
			badness += float(k.count("*")) * out_hyps[h][k]



		if badness < thresh:
			keep[h] = out_hyps[h]
	return keep


def mixture_hyps(out_hyps):
	mix_hyps = {}
	for h in out_hyps:
		mix_hyps[h] = out_hyps[h]
	for h1 in out_hyps:
		for h2 in out_hyps:
			if h1 != h2 and ((h2 + ' ' + h1) not in out_hyps):
				nh = h1 + ' ' + h2
				new_dct= {}
				for k in out_hyps[h1]:
					if k not in new_dct:
						new_dct[k] = 0.0
					new_dct[k] += out_hyps[h1][k] * 0.5

				for k in out_hyps[h2]:
					if k not in new_dct:
						new_dct[k] = 0.0
					new_dct[k] += out_hyps[h2][k] * 0.5

				mix_hyps[nh] = copy.deepcopy(new_dct)

	return mix_hyps
							

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

	p_h = 0.0
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
		p_h += (math.log(hyp_gen_use[d]) - math.log(z)) * (data[d]) 
		#p_h += math.log(hyp_gen[d]) * data[d] 

	return p_h

def logsumexp(v):
    m = max(v)
    return m+math.log(sum(map( lambda x: math.exp(x-m), v)))

def normalize(hyps, top_n):

	z = logsumexp([x[2] for x in hyps])

	hyps = sorted(copy.deepcopy(hyps), 
		key=lambda tup:-tup[2])
	hyps = [(x[0], x[1], math.exp(x[2]-z)) for x in hyps][:top_n]

	hyps = hyps[:top_n]
	return hyps



		
def compute_post_mix(hs, out_hyps, data):
	total_dist = {}
	prob_h = 1.0/float(len(hs))
	unique = 0
	seen = set()

	for h in hs:
		if h not in seen:
			seen.add(h)
			unique += 1
		for k in out_hyps[h]:
			if k not in total_dist:
				total_dist[k] = 0.0
			total_dist[k] += prob_h * out_hyps[h][k]

	p_d_given_h = prob_h_given_d(total_dist, data, sm=1e-2)
	
	p_h = unique * math.log(1.0/float(len(out_hyps)))
	p_h_given_d = p_d_given_h + p_h


	return p_h_given_d
