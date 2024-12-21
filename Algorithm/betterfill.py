# vim: set ts=4 sts=4 sw=4 expandtab :

TRACE = 5

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
    if loglevel>TRACE: print("Slots = %s" % repr(slots))
    constrained = constrain_slots(slots)
    if loglevel>TRACE: print("Constraints = %s" % repr(constrained))
    tree = make_tree(dictionary)
    if loglevel>1: print("Computed indexed dictionary tree")
    if loglevel>TRACE: print("Tree = %s" % repr(tree))
    frequencies = make_frequencies(tree)
    if loglevel>1: print("Computed letter frequencies")
    if loglevel>TRACE: print("Frequencies = %s" % repr(frequencies))
    solve(dictionary, grid, constrained, tree, frequencies, loglevel)

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
                if len(slot) >= 2:
                    slots.append(slot)
                slot = []
        if len(slot):
            if len(slot) >= 2:
                slots.append(slot)
            slot = []
    for x in range(w):
        for y in range(h):
            cell = grid[y][x]
            if (cell != '#') and (cell != " "):
                slot.append( (x,y) )
            elif len(slot):
                if len(slot) >= 2:
                    slots.append(slot)
                slot = []
        if len(slot):
            if len(slot) >= 2:
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
    for word_size in tree:
        byposition = tree[word_size]
        for position in byposition:
            byletter = byposition[position]
            for letter in byletter:
                byletter[letter] = set(byletter[letter])
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
def solve(dictionary, grid, constrained, tree, frequencies, loglevel):
    # 1. While there is at least one slot with one non-filled blank '.'
    remaining = [constraint for constraint in constrained 
        if not is_filled(constraint[0], grid)]
    if not remaining: return solution(grid)
    # 2. Order slots by the following heuristic
    [a,i,slot_a] = [remaining[0],0,remaining[0][0]]
    state_a = get_slot(slot_a, grid)
    known_a = known_letters(state_a)
    candidates_a = None
    for j,b in enumerate(remaining[1:]):
        slot_b = b[0]
        state_b = get_slot(slot_b, grid)
        known_b = known_letters(state_b)
        # 2.1. The more known letters the better
        # 2.2. The less unknown letters the better
        if ( (known_b > known_a) or
             ( (known_b == known_a) and 
               (len(b) < len(a)) ) ):
            [a,i,slot_a,state_a,known_a] = [b,j,slot_b,state_b,known_b]
            candidates_a = None
        # 2.3. The less candidates the better
        # 2.4. The more crossings the better
        elif (known_b == known_a) and (len(b) == len(a)):
            candidates_a = candidates_a or candidates_for(state_a,
                tree, dictionary)
            candidates_b = candidates_for(state_b,
                tree, dictionary)
            if ( (len(candidates_b) < len(candidates_a)) or
                 ( (len(candidates_b) == len(candidates_a)) and
                   (len(b[1]) > len(a[1])) ) ):
                [a,i,slot_a,state_a,known_a] = [b,j,slot_b,state_b,known_b]
                candidates_a = candidates_b
    candidates_a = candidates_a or candidates_for(state_a,
                tree, dictionary)
    # 3. Sort the candidates by frequency score
    candidates = list(candidates_a)
    candidates.sort(key=lambda c:score_of(c,a,frequencies))
    # 4. For each candidate, in order
    for candidate in candidates:
        # 4.1. Apply the candidate to newgrid
        for (i,(x,y)) in enumerate(slot_a):
            grid[y][x] = candidate[i]
        # 4.2. Non blocking for each crossing_slot in newgrid
        blocking = False
        for crossing in a[1]:
            if not blocking:
                (_, slot_b, _) = crossing
                state_b = get_slot(slot_b, grid)
                found = candidates_for(state_b, tree, dictionary)
                # One candidate must exist
                blocking = len(found) > 0 # XXX, highly inneficient
        # 4.3. Recurse at 1.
        maybe = solve(dictionary, grid, constrained,
            tree, frequencies, loglevel)
        # Restore
        for (i,(x,y)) in enumerate(slot_a):
            grid[y][x] = state_a[i]
        if maybe:
            return maybe

def score_of(candidate, constraint, frequencies):
    (slot, crossings) = constraint
    mapping = {}
    for crossing in crossings:
        (position_a, _, _) = crossing
        mapping[position_a] = crossing
    total = 0.0
    for position, letter in enumerate(candidate):
        if position in mapping:
            (_, slot_b, position_b) = mapping[position]
            b_word_size = len(slot_b)
            if b_word_size in frequencies:
                byposition = frequencies[b_word_size]
                if position_b in byposition:
                    byletter = byposition[position_b]
                    if letter in byletter:
                        total += byletter[letter]
    return total

def known_letters(state):
    count = 0
    for char in state:
        if char != '.':
            count+=1
    return count

def get_slot(slot, grid):
    return [grid[y][x] for (x,y) in slot]

def is_filled(slot, grid):
    for (x,y) in slot:
        cell = grid[y][x]
        if '.' == cell:
            return False
    return True

def candidates_for(state, tree, dictionary):
    applied = False
    result = None
    n = len(state)
    for position,letter in enumerate(state):
        if letter != '.':
            if applied:
                result = result & tree[n][position][letter]
            else:
                result = tree[n][position][letter]
                applied = True
    if applied:
        return result
    else:
        return [w for w in dictionary if len(w) == n]

def solution(grid):
    print("\n".join(["".join(row) for row in grid]))
    return grid

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

