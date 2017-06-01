import random
import copy

class Context():

    def __init__(self, open_set, closed_set, 
            matching_set):
        #self.visible_set = []
        self.open_set = open_set
        self.closed_set = closed_set
        self.matching = matching_set
        self.all = self.open_set + self.closed_set
        self.slots = []
        self.available = copy.copy(self.all)
        self.open_available = copy.copy(self.open_set)
        self.closed_available = copy.copy(self.closed_set)
        self.trace=""

    def set_params(self, mem_error):
        self.mem_error = mem_error
        return []

    def degrade_memory(self):
        for c in copy.copy(self.all):
            if ((self.mem_error > random.random())
                 and c not in self.available):
                self.available.append(c)
                if c in self.open_set:
                    assert(c not in self.open_available)
                    self.open_available.append(c)
                else:
                    assert(c not in self.closed_available)
                    self.closed_available.append(c)



    def is_open(self, r):
        if r in self.open_set:
            return True


    def is_closed(self, r):
        return (not self.is_open(r))


    def find_match(self, m, use):
        #takes a parenthesis and a list of all
        #remaining items on the board
        #and returns None if there are no matches
        #and the match if it exists
        for r in use:
            if (m, r) in self.matching or (r, m) in self.matching:
                return r
        return None

    def place(self, which, force=False): 
        #force=True: place regardless of whether or not
        #available
        if (which in self.open_set and 
            (which in self.available or force)):
            assert(which in self.open_available or force)

            if which in self.open_available:
                self.open_available.remove(which)
                self.available.remove(which)
            else:
                assert(force)

        elif (which in self.available or force):
            assert(which in self.closed_available or force)

            if which in self.closed_available:
                self.closed_available.remove(which)
                self.available.remove(which)

            else:
                assert(force)
        else:
            assert(False)

        self.slots.append(which)
        self.degrade_memory()


    def pick_specific(self, which):
        #if ret_error > random.random():
            #if which in self.open_set:
                #self.pick_open(type_error)
            #else:
               # self.pick_closed(type_error)

        #else:
        if which in self.available:
            self.place(which)
            self.trace += str(which)

    def pick_closed(self):
        #if ret_error > random.random():
            #self.pick_random()
        #else:
        if len(self.closed_available) > 0:
            r = random.randint(0, len(self.closed_available)-1)
            self.place(self.closed_available[r])
            self.trace += "C"
                

    def pick_open(self):
        if len(self.open_available) > 0:
            r = random.randint(0, len(self.open_available)-1)
            self.place(self.open_available[r])
            self.trace += "O"



    def pick_random(self):
        if len(self.available) > 0:
            r = random.randint(0, len(self.available)-1)
            self.place(self.available[r])
            self.trace += "R"



    def find_match(self, m):
        #takes a parenthesis and a list of all
        #remaining items on the board
        #and returns None if there are no matches
        #and the match if it exists
        for r in self.available:
            if (m, r) in self.matching or (r, m) in self.matching:
                return r
        return None

    def pick_match(self, inner=False):
        for s in self.slots[::-1]:
            match = self.find_match(s)
            if (match in self.available and 
                (not(inner) or match in self.closed_set)):
                self.place(match)
                self.trace += "M"
                break

    def ifelse_(self, bool, func1, func2, 
                func3=None, func4=None):
        if bool:
            func1()
            if func3 != None:
                func3()
        else:
            func2()
            if func4 != None:
                func4()
