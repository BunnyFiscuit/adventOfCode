import string

bags = open("2022/inputs/3.txt", "r").read().splitlines()
splitted = [(x[:int(len(x)/2)], x[int(len(x)/2):]) for x in [list(b) for b in bags]]

alphs = list(string.ascii_lowercase) + list(string.ascii_uppercase)

solA = [alphs.index(list(x)[0]) + 1 for x in [set.intersection(set(a),set(b)) for (a,b) in splitted]]
print("part 1:", sum(solA))

solB = [alphs.index(list(x)[0]) + 1 for x in [set.intersection(set(a), set(b), set(c)) for (a,b,c) in [bags[i:i+3] for i in range(0, len(bags), 3)]]]
print("part 2:", sum(solB))