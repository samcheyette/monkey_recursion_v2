from HMM_helpers import *


def run(top_n):
	#at_p = get_pos_paren(data, length=1)
	hypotheses = get_hyps(types, length_hyps)
	out_hyps = get_hyps_gen(hypotheses)
	out_hyps = filter_hyps(out_hyps, thresh=0.5)
	#out_hyps = add_noise(out_hyps, 0.2)
	all_data_bef = getCountData(file, careAbout, who, subset)

	all_data = []

	out_hyps = mixture_hyps(out_hyps)

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
			prob_h_d +=float(k.count(" ") + 1) * math.log(0.5)
			hyps.append((k, copy.deepcopy(h), prob_h_d))


		hyps = normalize(hyps, top_n)

		print d
		for h in hyps:
			print h[0],h[2]
			print

		print "*" * 50
		print


def propose(h, hyp_dist, noise, num_noise):




	r = random.randint(0, len(hyp_dist)-1)
	rand_sub = random.random()
	rand_add = random.random()
	h_new = copy.copy(h)

	if rand_sub < 0.5 and len(h) > 0:
		rem1 = random.randint(0, len(h_new)-1)
		h_new = h_new[0:rem1] + h_new[rem1+1:]

	if rand_add < 0.5 or len(h_new) == 0:
		h_new.append(hyp_dist[r])


	noise = noise + (random.randint(0, 2) - 1)
	noise = min(num_noise, max(0, noise))
	rand_ret = random.random()
	#if rand_ret < 0.5:
	return h_new, noise
	#else:
		#return propose(h_new, hyp_dist)


def run_MCMC(top_n):
	#at_p = get_pos_paren(data, length=1)
	hypotheses = get_hyps(types, length_hyps)

	out_hyps = get_hyps_gen(hypotheses)
	out_hyps = filter_hyps(out_hyps, thresh=filter_thresh)
	
	ret_mcmc = {}
	ret_distr = {}



	#out_hyps = add_noise(out_hyps, 0.1)
	out_hyps_noise = [copy.deepcopy(out_hyps)]

	for i in xrange(1, num_noise+1):
		noise_hyp = add_noise(copy.deepcopy(out_hyps), 
								i/(2*float(num_noise)))
		out_hyps_noise.append(noise_hyp)



	all_data_bef = getCountData(file, careAbout, who, subset)
	all_data = []
	all_part_data = []
	all_distr_data = []


	for pers in all_data_bef:
		new_dct = {}
		for key in pers:
			new_dct["".join(list(key))] = pers[key]
		all_data.append(copy.deepcopy(new_dct))


	for d in all_data:
		print d
		step = 0
		h = []
		h_post = 0.0
		noise = 0
		best_noise = noise
		for k in out_hyps:
			ret_mcmc[k] = 0.0
		norm_retmcmc = 0.0

		for k in hypotheses:
			ret_distr[k] = 0.0


		while step < MCMC_STEPS:
			prop_n = propose(h, out_hyps.keys(), noise, num_noise)
			prop = prop_n[0]
			noise = prop_n[1]
			out_hyps = out_hyps_noise[noise]
			prop_post = 0.0
			if h_post == 0.0:
				h = prop
				h_post = math.exp(compute_post_mix(h, out_hyps, d))
				best = copy.copy(h)
				best_post = h_post
				best_noise = noise
			else:
				prop_post = math.exp(compute_post_mix(prop, out_hyps, d))
				acc = min(1.0, prop_post / h_post)
				if random.random() < acc:
					h = copy.copy(prop)
					h_post = prop_post
					if h_post > best_post:
						best_post = h_post
						best_noise = noise
						best = sorted(copy.copy(h))

			if not(step % 1000):
				print step

				#print h, h_post
				print best_noise/(2*float(num_noise))
				print best, best_post
			#print prop, prop_post
				print

			step += 1

			if step > 0.5 * MCMC_STEPS:
				for k in h:
					ret_mcmc[k] += 1.0/float(len(h))
					norm_retmcmc += 1.0/float(len(h))


					distr_add = out_hyps[k]
					for out in distr_add.keys():

						ret_distr[out] += distr_add[out]/float(len(h))


		for k in ret_distr:
			ret_distr[k] = ret_distr[k]/ float(norm_retmcmc)

		for k in ret_mcmc:
			ret_mcmc[k] = ret_mcmc[k]/float(norm_retmcmc)


		all_part_data.append((copy.deepcopy(ret_mcmc)))
		all_distr_data.append((copy.deepcopy(ret_distr)))

	return all_part_data, all_distr_data, all_data

def output(models_traces, models_distr, hum_data,
		 filter_thresh):
	hypotheses = get_hyps(types, length_hyps)

	out_hyps = get_hyps_gen(hypotheses)
	out_hyps = filter_hyps(out_hyps, thresh=filter_thresh)

	for mod in models_traces:
		whch = 0
		m = mod[mod.keys()[0]]

		for subj in m:
			print mod.keys()[0], whch
			ce = ["([])", "[()]"]
			cr = ["([)]", "[(])"]
			tr = ["[]()", "()[]"]
			#normalized by probaility props
			prop_ce = 0.0
			prop_cr = 0.0
			prop_tr = 0.0
			prop_ot = 0.0
			for alg in out_hyps:
				p = 0.0
				distr_alg = out_hyps[alg]
				if alg in subj and subj[alg] > 0.01:
					p = subj[alg]
					#print alg, p
					#print distr_alg


				for d in distr_alg:
					if d in ce:
						prop_ce += p * distr_alg[d]
					elif d in cr:
						prop_cr += p * distr_alg[d]
					elif d in tr:
						prop_tr += p * distr_alg[d]
					else:
						prop_ot += p * distr_alg[d]

			print

			whch += 1


			#print

if __name__ == "__main__":
	length_hyps=4
	top_n = 10
	MCMC_STEPS = 250
	num_noise = 10
	filter_thresh = 0.5

	acceptance_par = 10e-4
	whos = ["Kids", "Monkeys", "Tsimane"]

	all_mods_traces = []
	all_mods_distr = []

	for who in whos:
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



		model_res = run_MCMC(top_n)
		traces = model_res[0]
		distr= model_res[1]
		hum_data = model_res[2]

		all_mods_traces.append({who:copy.deepcopy(traces)})

	output(all_mods_traces, all_mods_distr, 
			hum_data, filter_thresh)



