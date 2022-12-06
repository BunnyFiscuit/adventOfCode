lines = open("2022/inputs/2.txt", "r").read().splitlines()

def cValue(val):
  return 0 if val == 'A' or val == 'X' else 1 if val == 'B' or val == 'Y' else 2 

tuples = [(cValue(a), cValue(b)) for (a,b) in [(l.split(" ")[0],l.split(" ")[1]) for l in lines]]

def outcome(elf, you):
  return 6 if (elf +1) % 3 == you else 3 if elf == you else 0

outcomes = [outcome(elf, you) + you + 1 for (elf, you) in tuples]
print("part 1:", sum(outcomes))


def needed_play(elf, you):
  res = you * 3;  
  possible = {
    0: 2 if elf == 0 else 0 if elf == 1 else 1,
    3: elf,
    6: 1 if elf == 0 else 2 if elf == 1 else 0
  }
  return possible[res]

outcomes_two = [outcome(elf, needed_play(elf, you)) + (needed_play(elf,you)) + 1 for (elf, you) in tuples]
print("part 2:", sum(outcomes_two))