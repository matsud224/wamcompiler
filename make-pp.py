insts=[['put-variable-temporary', 'x,a'],
 ['put-variable-permanent', 'y,a'],
 ['put-value-temporary', 'x,a'],
 ['put-value-permanent', 'y,a'],
 ['put-unsafe-value', 'y,a'],
 ['put-structure', 'f,a'],
 ['put-list', 'a'],
 ['put-constant', 'c,a'],
 ['set-variable-temporary', 'x'],
 ['set-variable-permanent', 'y'],
 ['set-value-temporary', 'x'],
 ['set-value-permanent', 'y'],
 ['set-local-value-temporary', 'x'],
 ['set-local-value-permanent', 'y'],
 ['set-constant', 'c'],
 ['set-void', 'n'],
 ['get-variable-temporary', 'x,a'],
 ['get-variable-permanent', 'y,a'],
 ['get-value-temporary', 'x,a'],
 ['get-value-permanent', 'y,a'],
 ['get-structure', 'f,a'],
 ['get-list', 'a'],
 ['get-constant', 'c,a'],
 ['unify-variable-temporary', 'x'],
 ['unify-variable-permanent', 'y'],
 ['unify-value-temporary', 'x'],
 ['unify-value-permanent', 'y'],
 ['unify-local-value-temporary', 'x'],
 ['unify-local-value-permanent', 'y'],
 ['unify-constant', 'c'],
 ['unify-void', 'n'],
 ['call', 'p,n'],
 ['execute', 'p'],
 ['try-me-else', 'l'],
 ['retry-me-else', 'l'],
 ['trust-me', 'n'],
 ['try', 'l'],
 ['retry', 'l'],
 ['trust', 'l'],
 ['switch-on-term', 'v,c,l,s'],
 ['switch-on-constant', 'alist'],
 ['switch-on-structure', 'alist'],
 ['get-level', 'y'],
 ['cut', 'y']]


def makeformat(args):
	str=""
	for a in args:
		if a=="x":
			str=str+"X~A,"
		elif a=="y":
			str=str+"Y~A,"
		elif a=="a":
			str=str+"A~A,"
		else:
			str=str+"~A,"
	return str.rstrip(',')

def nth(n):
	return '(ca' + 'd'*n + 'r inst)'

def makeargs(n):
	str=""
	for i in range(1,n+1):
		str=str+nth(i)+" "
	return str.rstrip(' ')


for i in insts:
	args=i[1].split(',')
	print('({0}\n(format t "{0} {1}~%" {2}))'.format(i[0],makeformat(args),makeargs(len(args))))
	
 

