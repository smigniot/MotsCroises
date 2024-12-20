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
# 2. pass
#
def run(dictionary, grid, loglevel):
    slots = find_slots(grid)
    if loglevel>1: print("Slots = %s" % repr(slots))
    constrained = constrain_slots(slots)
    if loglevel>1: print("Constraints = %s" % repr(constrained))
    tree = make_tree(dictionary)
    if loglevel>2: print("Tree = %s" % repr(tree))

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

