lines = open("2022/inputs/1.txt", "r").read().splitlines()

grouped = []
curr = []
for cal in lines:
  if (cal != ""):
    curr.append(int(cal))
  else:
    grouped.append(sum(curr.copy()))
    curr = []

grouped.sort()
grouped.reverse()

print("part 1:", grouped[0], "\npart 2:", sum(grouped[0:3]))