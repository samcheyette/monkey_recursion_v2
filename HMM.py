
import random
import copy

matching = [('(', ')'), ('[', ']')]
all_p = ['[', '(', ')', ']']

def find_match(m):
    #takes a parenthesis and a list of all
    #remaining items on the board
    #and returns None if there are no matches
    #and the match if it exists
    for r in all_p:
        if (m, r) in matching or (r, m) in matching:
            return r
    return ""

def pick_open():
	if random.random() < 0.5:
		return "("
	else:
		return "["

def pick_closed():
	if random.random() < 0.5:
		return ")"
	else:
		return "]"

def pick_match():
	match = ""
	for c in context[::-1]:
		match = find_match(c)

	return match


states = ['OPEN', 'CLOSED', 'MATCH']
 
observations = ('[', '(', ')',']')
 
start_probability = {'OPEN': 0.6, 'CLOSED': 0.4, 'MATCH':0.0}
 
transition_probability = {
   'OPEN' : {'OPEN': 0.3, 'CLOSED': 0.3, 'MATCH':0.4},
   'CLOSED' : {'OPEN': 0.4, 'CLOSED': 0.2, 'MATCH':0.4},
   'MATCH' : {'OPEN': 0.4, 'CLOSED': 0.2, 'MATCH':0.4},

   }


emission_probability = {
   'OPEN' : lambda: pick_open(),
   'CLOSED' : lambda: pick_closed(),#,
   'MATCH': lambda: pick_match()
   }

#def find_match(context):

def prob_to_lst(ps, granularity):
	lst = []
	for k in ps:
		p = ps[k]
		#if 'float' in str(type(p)):
		lst += [k for _ in xrange(int(granularity * p))]

	return lst

if __name__ == "__main__":
	nsamp = 1000000
	lst = []
	gran = 10
	s_lst = prob_to_lst(start_probability,gran)


	t_p = {}
	for k in transition_probability:
		t_p[k] = prob_to_lst(transition_probability[k], gran)


	ind = 0
	while ind < nsamp:
		context = ""
		program = []
		r = random.randint(0, len(s_lst)-1)
		state = s_lst[r]
		#state = start

		while len(context) < 4:
			emit = str(emission_probability[state]())
			program.append(state)
			context += str(emit)
			state = t_p[state][random.randint(0, gran-1)]

			#if len(context) >= 4:
				#context=context[len(context)-4:]
		ind += 1

		print ind
		print program
		print context
		print