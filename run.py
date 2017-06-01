from LOTlib.DataAndObjects import FunctionData
from LOTlib.Inference.Samplers.MetropolisHastings import MHSampler
from model_parentheses import *
from LOTlib.TopN import TopN
from LOTlib.Miscellaneous import logsumexp, qq
from math import exp, log
import time
from cleanStevesData_byParticipant2 import *

def run():
	fdata_orig = [ FunctionData(input=(), 
			output=dat, alpha=ALPHA) ]
	h0 = MyHypothesis(**{"alpha":ALPHA, 
						"open_set":open_set,
						"closed_set": closed_set, 
						"matching_set": matching_set})
	tn = TopN(N=TOP)


	ind =0 
	print "... ..." * 10
	print

	norm_best = sum([dat[k] for k in dat])
	t_start = time.time()
	for _ in xrange(NTIMES):
		for h in MHSampler(h0, fdata_orig, steps=NSAMP):
			tn.add(h)
			if not (ind % 100):
				print "#################"
				t_lapsed = time.time() - t_start
				print t_lapsed, ind/t_lapsed

				best = tn.best()
				print ind
				print best
				print exp(best.posterior_score)
				print
				dct = {}
				for x in xrange(250):
				    best_out = best()
				    if x < 5:
				        print best.history[:4]
				        print best_out
				    if best_out not in dct:
				        dct[best_out] = 0.0
				    dct[best_out] += norm_best/250.0

				print
				tot_dct = 0
				tot_dat = 0
				dif =0 
				for d in dat:
					dct_d = 0
					if d in dct:
						dct_d = dct[d]
					tot_dct += dct_d
					tot_dat += dat[d]
					dif += (dct_d - dat[d])**2
					print d, dat[d], dct_d
				print "other", 0, tot_dat - tot_dct 
				print "dif", dif

				print "#################"
				if not ind%1000:
					print
					print "*-"*50
					print "*-"*50

					print h
					print exp(h.posterior_score)
					for _ in xrange(5):
						xzh = h()
						print h.history[:4]
						print xzh

					print "*-"*50
					print "*-"*50

				print

			ind += 1

	print "... >>> " * 10
	print

	posts = [(h, 
			h.compute_posterior(fdata_orig,
			 **{"nsamples":4096})) for h in
			  tn.get_all(sorted=True)]


	posts_only = [tup[1] for tup in posts]
	z = logsumexp(posts_only)
	pp = ([(tup[0], exp(tup[1] - z)) for tup in posts])
	sort_post_probs = sorted(pp, key=lambda tup: 1 - tup[1])

	for p in sort_post_probs[:5]:
		print p[0]
		print p[1]
		print
	print ">>> >>> " * 10
	print

	return sort_post_probs


def analyze(probs, num):

	#need to renormalize
	probs = probs[:num]
	posts_only = [tup[1] for tup in probs]
	z = logsumexp(posts_only)
	pp = ([(tup[0], exp(tup[1] - z)) for tup in probs])
	sort_post_probs = sorted(pp, key=lambda tup: 1 - tup[1])

	distribution = get_distribution(sort_post_probs)
	trace = get_traces(sort_post_probs)

	for d in distribution:
		if distribution[d] > 0.001:
			print d, distribution[d]

	print
	for d in trace:
		if trace[d] > 0.001:
			print d, trace[d]

	return distribution, trace


if __name__ == "__main__":

	########GLOBALS########
	NSAMP = 20000
	NTIMES= 5
	ALPHA = 0.1
	TOP = 25
	who = "Tsimane"
	careAbout = "Order pressed"
	outhyps = "output/all_data6.csv"
	#outhyps = None

	all_parens = ["(", "[", "]", ")"]
	open_set = ["(", "["]
	closed_set = [")", "]"]
	matching_set = [("(", ")"), ("[", "]")]


	if who == "Kids":
		file = "stevesdata/RecursionKids.csv"
		start = 0 
		subset= {}
		#outhyps = "output/KidsHyps.csv"

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

	all_data =getCountData(file, careAbout, who, subset)

	#dat = simulateData(all_parens, matching_set,
				#	randomValidLsts=[('[', '(', ']', ')'), 
						#			 ('(', '[', ')', ']')], 
					# pC=0.8, pT=0.2, N=100)
	########################




	for dat in all_data:
		if start != 100:
			print dat
			mod_out = run()

			n=10
			analysis = analyze(mod_out, n)
			distr = analysis[0]
			trace = analysis[1]

			t = time.time()
			output1(outhyps, who, start,dat,
					 mod_out, distr, trace)
		#while time.time() < t + 7.5:
		#	pass

		start = start + 1



