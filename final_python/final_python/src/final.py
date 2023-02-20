from prolog_structures import Rule, RuleBody, Term, Function, Variable, Atom, Number
from typing import List
from functools import reduce

import sys
import random

class Not_unifiable(Exception):
	pass

'''
Please read prolog_structures.py for data structures
that represent Prolog terms, rules, and goals.
'''
class Interpreter:
	def __init__(self):
		pass

	'''
	Example
	occurs_check (v, t) where v is of type Variable, t is of type Term.
	occurs_check (v, t) returns true if the Prolog Variable v occurs in t.
	Please see the lecture note Control in Prolog to revisit the concept of
	occurs-check.
	'''
	def occurs_check (self, v : Variable, t : Term) -> bool:
		if isinstance(t, Variable):
			return v == t
		elif isinstance(t, Function):
			for t in t.terms:
				if self.occurs_check(v, t):
					return True
			return False
		return False


	'''
	Problem 1
	variables_of_term (t) where t is of type Term.
	variables_of_clause (c) where c is of type Rule.

	The function should return the Variables contained in a term or a rule
	using Python set.

	The result must be saved in a Python set. The type of each element (a Prolog Variable)
	in the set is Variable.
	'''
	def variables_of_term (self, t : Term) -> set :
		if isinstance(t, Variable):
			return set(([t]))
		elif isinstance(t, Function):
			x = set()
			for t in t.terms:
				if isinstance(t, Variable):
					x.add(t)
			return x
		return set()

	def variables_of_clause (self, c : Rule) -> set :
		x = self.variables_of_term(c.head)
		for i in c.body.terms:
			x.union(self.variables_of_term(i))
		
		return x


	'''
	Problem 2
	substitute_in_term (s, t) where s is of type dictionary and t is of type Term
	substitute_in_clause (s, t) where s is of type dictionary and c is of type Rule,

	The value of type dict should be a Python dictionary whose keys are of type Variable
	and values are of type Term. It is a map from variables to terms.

	The function should return t_ obtained by applying substitution s to t.

	Please use Python dictionary to represent a subsititution map.
	'''
	def substitute_in_term (self, s : dict, t : Term) -> Term:
		if s =={}:
			return t
		elif isinstance(t, Function):
			term_helper = []
			for x in t.terms:
				term_helper.append(self.substitute_in_term(s,x))
			return Function(t.relation, term_helper)
		elif isinstance(t, Variable):
			term_helper = s.get(t,t)
			return term_helper
		elif isinstance(t, Atom):
			term_helper = s.get(t,t)
			return term_helper
		elif isinstance(t, Number):
			term_helper = s.get(t.value,t.value)
			term_helper = str(term_helper)
			return Number(term_helper)

	def substitute_in_clause (self, s : dict, c : Rule) -> Rule:
		if s == {}:
			return c
		head_helper = self.substitute_in_term(s, c.head)
		term_helper = []
		for x in c.body.terms:
			term_helper.append(self.substitute_in_term(s,x))
		return Rule(head_helper, RuleBody(term_helper))


	'''
	Problem 3
	unify (t1, t2) where t1 is of type term and t2 is of type Term.
	The function should return a substitution map of type dict,
	which is a unifier of the given terms. You may find the pseudocode
	of unify in the lecture note Control in Prolog useful.

	The function should raise the exception raise Not_unfifiable (),
	if the given terms are not unifiable.

	Please use Python dictionary to represent a subsititution map.
	'''
	def funct (self, s, tup):
		x = tup[0]
		y = tup[1]
		return self.unify_helper(x,y,s)

	def unify_helper(self, prevx, prevy, s:dict):
		x = self.substitute_in_term(s, prevx)
		y = self.substitute_in_term(s, prevy)
		if isinstance(x, Variable) and (self.occurs_check(x,y) == False):
			Snew = {x:y}
			for key,value in s.items():
				ValNew = self.substitute_in_term(Snew, value)
				s[key] = ValNew
			s[x] = y
			return s
		elif isinstance(y, Variable) and (self.occurs_check(x,y) == False):
			Snew = {y:x}
			for key,value in s.items():
				ValNew = self.substitute_in_term(Snew, value)
				s[key] = ValNew
			s[y] = x
			return s
		elif x == y:
			return s
		elif isinstance(x, Function) and isinstance(y, Function):
			if x.relation != y.relation:
				raise Not_unifiable
			TermsOfX = []
			for i in x.terms:
				TermsOfX.append(i)
			TermsOfY = []
			for i in y.terms:
				TermsOfY.append(i)
			if len(TermsOfY) != len(TermsOfX):
				raise Not_unifiable
			tupes = []
			for i in range(len(TermsOfX)):
				tupes.append((TermsOfX[i],TermsOfY[i]))
			return(reduce(self.funct, tupes, s))
		else:
			raise Not_unifiable
	def unify (self, t1: Term, t2: Term) -> dict:
		return self.unify_helper(t1,t2,{})


	fresh_counter = 0
	def fresh(self) -> Variable:
		self.fresh_counter += 1
		return Variable("_G" + str(self.fresh_counter))
	def freshen(self, c: Rule) -> Rule:
		c_vars = self.variables_of_clause(c)
		theta = {}
		for c_var in c_vars:
			theta[c_var] = self.fresh()

		return self.substitute_in_clause(theta, c)


	'''
	Problem 4
	Following the Abstract interpreter pseudocode in the lecture note Control in Prolog to implement
	a nondeterministic Prolog interpreter.

	nondet_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of Terms (results), which is an instance of the original goal and is
	a logical consequence of the program. See the tests cases (in src/main.py) as examples.
	'''
	def nondet_query (self, program : List[Rule], pgoal : List[Term]) -> List[Term]:
		while(1):
			goal= pgoal[:]
			res = goal[:]
			while(res):
				x = random.randint(0, len(res)-1)
				y = res[x]
				unify = []
				for i in range(len(program)):
					NewY = self.freshen(program[i])
					try:
						self.unify(y,NewY.head)
						unify.append(NewY)
					except Not_unifiable:
						pass
				if unify == []:
					break
				k = random.randint(0, len(unify)-1)
				w = self.unify(y, unify[k].head)
				res.remove(res[x])
				for term in unify[k].body.terms:
					res.append(term)
				for term in range(len(goal)):
					new = self.substitute_in_term(w, goal[term])
					goal[term] = new
				for term in range(len(res)):
					new = self.substitute_in_term(w, res[term])
					res[term] = new
			if res == []:
				return goal


	'''
	Challenge Problem

	det_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of term lists (results). Each of these results is
	an instance of the original goal and is a logical consequence of the program.
	If the given goal is not a logical consequence of the program, then the result
	is an empty list. See the test cases (in src/main.py) as examples.
	'''
	def det_query (self, program : List[Rule], pgoal : List[Term]) -> List[List[Term]]:
		return [pgoal]
