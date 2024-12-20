# vim: set ts=4 sts=4 sw=4 expandtab :

#
# Main cli func
#
def main(args):
    loglevel = args.verbose or 0
    dicttext = args.dictionary.read()
    gridtext = args.gridfile.read()
    dictionary = [l.strip() for l in dicttext.splitlines() if l.strip()]
    grid = [list(line) for line in gridtext.splitlines()]
    if loglevel>0: print("Dict =\n%s\n" % "\n".join(dictionary[:3]+["..."]+dictionary[-3:]))
    if loglevel>0: print("Grid =\n%s\n" % gridtext)
    run(dictionary, grid, loglevel)

#
# Run !
#
# 1. Find slots - where to place words
# 2. Find crossing between slots
# 3. Index the dictionary by word size, position and letter
# 4. Compute letter frequency
# 5. Start solver
#
def run(dictionary, grid, loglevel):
    slots = find_slots(grid)
    if loglevel>1: print("Enumerated %d slots" % len(slots))
    if loglevel>2: print("Slots = %s" % repr(slots))
    constrained = constrain_slots(slots)
    if loglevel>2: print("Constraints = %s" % repr(constrained))
    tree = make_tree(dictionary)
    if loglevel>1: print("Computed indexed dictionary tree")
    if loglevel>2: print("Tree = %s" % repr(tree))
    frequencies = make_frequencies(tree)
    if loglevel>1: print("Computed letter frequencies")
    if loglevel>2: print("Frequencies = %s" % repr(frequencies))
    solve(dictionary, grid, constrained, tree, frequencies)

#
# Find slots
#
# A slot is any location that can be filled with a word.
# Highly unoptimized, for maintainability
#
# Given :  Point = tuple(int,int)
# Given :  Slot = list[Point]
# Input :  list[list[char]]
# Output : list[Slot]
#
# Example, for grid
# #########
# ##.....##
# ####.#.##
# ####.####
# #########
#
# Returns [
#  [(2,1),(3,1),(4,1),(5,1),(6,1)],
#  [(4,1),(4,2),(4,3)],
#  [(6,1),(6,2)]
# ]
#
def find_slots(grid):
    w = len(grid[0])
    h = len(grid)
    slots = []
    slot = []
    for y in range(h):
        for x in range(w):
            cell = grid[y][x]
            if (cell != '#') and (cell != " "):
                slot.append( (x,y) )
            elif len(slot):
                slots.append(slot)
                slot = []
        if len(slot):
            slots.append(slot)
            slot = []
    for x in range(w):
        for y in range(h):
            cell = grid[y][x]
            if (cell != '#') and (cell != " "):
                slot.append( (x,y) )
            elif len(slot):
                slots.append(slot)
                slot = []
        if len(slot):
            slots.append(slot)
            slot = []
    return slots

#
# Constrain slots
#
# Compute each crossing slot, and crossing positions, for each initial slot
#
# Given :  Point = tuple(int,int)
# Given :  Slot = list[Point]
# Given :  Position = int
# Given :  Crossing = tuple(Position,Slot,Position)
# Input :  list[Slot]
# Output : list[tuple(Slot,list[Crossing])]
#
# Example, for input [
#  [(2,1),(3,1),(4,1),(5,1),(6,1)],
#  [(4,1),(4,2),(4,3)],
#  [(6,1),(6,2)]
# ]
#
# Returns [
#  ( [(2,1),(3,1),(4,1),(5,1),(6,1)], [
#    (2, [(4,1),(4,2),(4,3)], 0),
#    (4, [(6,1),(6,2)], 0)
#  ] ),
#  ( [(4,1),(4,2),(4,3)], [
#    (0, [(2,1),(3,1),(4,1),(5,1),(6,1)], 2),
#  ] ),
#  ( [(6,1),(6,2)], {
#    (0, [(2,1),(3,1),(4,1),(5,1),(6,1)], 4),
#  ] )
# ]
#
def constrain_slots(slots):
    constrained = []
    for a in slots:
        crossings = []
        for b in slots:
            if b is not a:
                intersection = set(a) & set(b)
                if intersection:
                    common = list(intersection)[0]
                    pos_a = a.index(common)
                    pos_b = b.index(common)
                    crossings.append( (pos_a, b, pos_b) )
        constrained.append( (a, crossings) )
    return constrained

#
# Compute a dictionary tree
#
# The tree is an index by word size, position and letter
# Example, for input FLEXED
# Returns a dict containing at least
# {6:{3:{"X":["FLEXED"]}}}
#
# Input :  list[str]
# Output : dict{int,dict{int,dict{char,list[str]}}}
#
def make_tree(dictionary):
    tree = {}
    for word in dictionary:
        word_size = len(word)
        for position, letter in enumerate(list(word)):
            if word_size not in tree: tree[word_size] = {}
            byposition = tree[word_size]
            if position not in byposition: byposition[position] = {}
            byletter = byposition[position]
            if letter not in byletter: byletter[letter] = []
            byletter[letter].append(word)
    return tree

#
# Compute tree frequencies
#
# The frequency is the percentage of occurence of a given letter amongst all other
# (for an already given word_size and position pair)
#
# The frequency tree is indexed by word size, position and letter
# Example, for input FLEXED
# Returns a dict containing at least (0.01 is a non representative example percentage here)
# {6:{3:{"X":0.01}}}
#
# Input :  list[str]
# Output : dict{int,dict{int,dict{char,float}}}
#
def make_frequencies(tree):
    frequencies = {}
    for word_size, byposition in tree.items():
        frequencies[word_size] = {}
        for position, byletter in byposition.items():
            frequencies[word_size][position] = {}
            total = 0
            for letter, words in byletter.items():
                total += len(words)
            for letter, words in byletter.items():
                frequencies[word_size][position][letter] = float(len(words))/float(total)
    return frequencies

#
# Start the solver.
#
# The algorithm is a complete walk of the solution tree.
# With infinite time it will find all existing solutions or a proof
# of non-solvability.
#
# The caller should however keep in mind that this is a
# https://en.wikipedia.org/wiki/Constraint_satisfaction_problem
# and we're looking for *one* solution in an *acceptable* amount of time.
# 
# This algorithm makes the following choices :
# 1. While there is at least one slot with one non-filled blank '.'
# 2. Order slots by the following heuristic
# 2.1. The more known letters the better, i.e. "EX.UIS.TE" seems easier than "T....D"
# 2.2. The less unknown letters the better, i.e. "EX.T" seems easier than "EXT....."
# 2.3. The less candidates the better, i.e. "...ZZ" s easier than "E...T"
# 2.4. The more crossings the better, i.e. choose to fail fast
# 3. Sort the candidates by frequency score
# 4. For each candidate, in order
# 4.1. Apply the candidate to newgrid
# 4.2. Non blocking : one candidate must exist for each crossing_slot in newgrid
#      NB: This is a sort of Breadth First Search for non-solvability
# 4.3. Recurse at 1.
#      NB: This is a sort of Depth First Search for solutions
#
def solve(dictionary, grid, constrained, tree, frequencies):
    pass

def is_filled(slot, grid):
    for (x,y) in slot:
        cell = grid[y][x]
        if '.' == cell:
            return False
    return True

#
# Command line launcher
#
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose",
        action="count",
        help="Dictionary file")
    parser.add_argument("-d", "--dictionary",
        type=argparse.FileType('r', encoding='utf-8'),
        help="Dictionary file")
    parser.add_argument("gridfile",
        type=argparse.FileType('r', encoding='utf-8'),
        help="Input crosswords grid file")
    args = parser.parse_args()
    main(args)

