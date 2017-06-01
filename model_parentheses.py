from LOTlib.Grammar import Grammar

from context import Context
from LOTlib.Primitives import primitive
import random
from scipy.stats import beta
from helpers import *
# Context().__init__()

from LOTlib.Grammar import Grammar


@primitive
def flip_(p):
    return p > random.random()

@primitive
def gt_(x, y):
    return x > y

@primitive
def index_(x,y,z):
    if len(x.slots) > y:
        return (x.slots[y] == z)
    else:
        return False

@primitive
def and_(x, y):
    return x & y

@primitive
def or_(x, y):
    return x | y

@primitive
def not_(x):
    return (not x)


grammar = Grammar()

grammar.add_rule('S2', '[%s]', ['op'], 1.0)
grammar.add_rule('S2', '%s + [%s]', ['S2', 'op'], 1.0)
grammar.add_rule('S2', '[%s] + %s', ['op', 'S2'], 1.0)


grammar.add_rule('START', '%s.set_params(mem_error=%s) + %s', 
                ['C', 'PROB', 'S2'], 1.0)
#grammar.add_rule('START', '%s.set_params(mem_error=0.0) + %s', 
                #['C','S2'], 1.0)

grammar.add_rule('op', 
                '%s.pick_specific("%s")',
                 ['C', 'PAREN'], 1.0)

grammar.add_rule('op', '%s.pick_open()', ['C'], 1.0)
grammar.add_rule('op', '%s.pick_closed()', ['C'], 1.0)
grammar.add_rule('op', '%s.pick_match(inner=True)', ['C'], 1.0)

grammar.add_rule('PAREN', "(", None, 1.0)
grammar.add_rule('PAREN', ")", None, 1.0)
grammar.add_rule('PAREN', "[", None, 1.0)
grammar.add_rule('PAREN', "]", None, 1.0)


grammar.add_rule('op', 
    '%s.ifelse_(%s, lambda: %s, lambda: %s)',
      ['C', 'BOOL', 'op', 'op'], 0.25)

grammar.add_rule('op', 
    '%s.ifelse_(%s, lambda: %s, lambda: %s, lambda: %s, lambda: %s)',
      ['C', 'BOOL', 'op', 'op', 'op', 'op'], 0.125)



grammar.add_rule('BOOL', 
                    'flip_(%s)', ['PROB'], 1.0)
grammar.add_rule('BOOL', 
                    'gt_(len(%s.slots), %s)', 
                        ['C', 'INT'], 1.0)
#grammar.add_rule('BOOL', 'index_(%s, %s, %s)', ['C', 'INT','PAREN'], 1.0)
grammar.add_rule('BOOL', 'index_(%s, %s,"%s")', 
        ['C', 'INT', 'PAREN'],1.0)

#grammar.add_rule('BOOL', 
             #       'and_(%s, %s)', 
                      #  ['BOOL', 'BOOL'], 0.25)
#grammar.add_rule('BOOL', 
                    #'or_(%s, %s)', 
               #         ['BOOL', 'BOOL'], 0.25)
grammar.add_rule('BOOL', 
                    'not_(%s)', 
                        ['BOOL'], 0.5)
#grammar.add_rule('op', '%s.pick_random()', ['C'], 1.0)

#grammar.add_rule('op', '%s.pick_inner_match(p=%s)', ['C', 'PROB'], 1.0)
#grammar.add_rule('op', '%s.pick_outer_match(p=%s)', ['C', 'PROB'], 1.0)
print grammar

#bias towards "no noise"
noise_bias = 1
#grammar.add_rule('PROB', '0.0', None, 1.0)
prob_grain = 10
for k in xrange(0, prob_grain):
    p = k/float(prob_grain)
    #pr = beta.pdf(p, 1, 9)
    pr =  1.0
    grammar.add_rule('PROB',str(p), None, pr)

for k in xrange(0,4):
    grammar.add_rule('INT', str(k), None, 1.0)



from LOTlib.Hypotheses.LOTHypothesis import LOTHypothesis
from LOTlib.Miscellaneous import nicelog

from LOTlib.Hypotheses.Likelihoods.StochasticLikelihood import StochasticLikelihood
import copy
from math import log, factorial

from LOTlib.Miscellaneous import Infinity, beta, attrmem

# define a
class MyHypothesis(StochasticLikelihood, LOTHypothesis):
    def __init__(self, **kwargs):
        if 'alpha' in kwargs:
            self.alpha = kwargs['alpha']
        else:
            self.alpha = 0.0

        if "open_set" in kwargs:
            self.open_set = kwargs['open_set']
        if "closed_set" in kwargs:
            self.closed_set = kwargs['closed_set']
        if "matching_set" in kwargs:
            self.matching_set = kwargs['matching_set']


        LOTHypothesis.__init__(self, grammar=grammar,  maxnodes=400,
                                   display="lambda C: %s", 
                                   prior_temperature=1.0,
                                   likelihood_temperature=1.0,**kwargs)


    def __call__(self):  # , max_length=4):
        ret_vals = []
        C = Context(open_set=self.open_set, 
                closed_set=self.closed_set, 
                matching_set = self.matching_set)

        self.fvalue(C)

        exp =  len(self.open_set) + len(self.closed_set)
        ret = C.slots[:exp]
        #if len(ret) < exp:
           # for _ in xrange(exp - len(ret)):
            #    p_all = open_set + closed_set
              #  r = random.randint(0, exp-1)
                #ret.append(p_all[r])
        
        self.history=C.trace

        return tuple(ret)


    @attrmem('likelihood')
    def compute_likelihood(self, data, 
                shortcut=-Infinity, nsamples=512, 
                    **kwargs):
        # For each input, if we don't see its input (via llcounts), recompute it through simulation

        seen = {} # hash of data inputs to llcounts for that input
        ll = 0.0
        sm = self.alpha
        for datum in data:
            if datum.input not in seen:
                seen[datum.input] = self.make_ll_counts(datum.input, nsamples=nsamples)

            ll += self.compute_single_likelihood(datum, seen[datum.input], sm=sm) / self.likelihood_temperature

            if ll < shortcut:
                return -Infinity

        return ll

if __name__ == "__main__":
    from LOTlib.DataAndObjects import FunctionData
    from LOTlib.Inference.Samplers.MetropolisHastings import MHSampler

    t = 1
    nsamp = 3000
    ntimes=5
    alpha = 0.1

    all_parens = ["(", "[", "]", ")"]

    open_set = ["(", "["]
    closed_set = [")", "]"]
    matching_set = [("(", ")"), ("[", "]")]


    #C= Context(open_set, closed_set, matching_set)


    #dat = simulateData(all_parens, matching_set)
    #dat = {tuple(copy.copy(all_parens)):25,
         #("(", "[", "[", ")"):9,
         #("(", "(", "[", "]"):6,
        # ("(", "[", "]", "]"):2,
         #("(", "(", "]", ")"):5,
         #("(", "[", ")", "]"):8}

    dat = {}
    for _ in xrange(100):
        c_sim = Context(open_set, closed_set, matching_set)
        c_sim.set_params(mem_error=0.0)
        c_sim.pick_specific("(")

        c_sim.pick_open()
        if random.random() < 0.5:
            c_sim.pick_match(inner=True)
        else:
            c_sim.pick_closed()
        c_sim.pick_match(inner=True)
        #c_sim.pick_closed(ret_error=0.0)

        ret = c_sim.slots
        #while len(ret) < 4:
         #   ret += all_parens[random.randint(0,3)]
        ret = tuple(ret)

        #if len(ret) == 4:
        if ret not in dat:
            dat[ret] = 0
        dat[ret] += 1

    print dat

    fdata_orig = [ FunctionData(input=(),
         output=dat, alpha=alpha) ]
    #n_enum = int(log(nsamp*2, 2)+1)

    ind =0 
    print 
    h0 = MyHypothesis(**{"alpha":alpha, 
                            "open_set":open_set,
                             "closed_set": closed_set, 
                             "matching_set": matching_set})

    for _ in xrange(ntimes):
        for h in MHSampler(h0, fdata_orig, steps=nsamp):
            if not (ind % 100):
                print ind
                print h
                print h.posterior_score
                print h.likelihood
                print h.prior
                dct = {}
                for x in xrange(100):
                    h_out= h()
                    if x < 5:
                        print h.history[:4]
                        print h_out
                    if h_out not in dct:
                        dct[h_out] = 0
                    dct[h_out] += 1
                print dct

                print
            ind += 1
