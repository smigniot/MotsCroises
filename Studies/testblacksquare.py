from blacksquare import *
import sys


lines = []
for line in sys.stdin:
    l = line.strip("\n")
    if l.strip():
        lines.append(l)

width = max([len(l) for l in lines])
height = len(lines)
emptyrow = " "*width
template = "\n".join([(l+emptyrow)[0:width] for l in lines])

print("Grid (%dx%d) =\n%s" % (width,height,template))

#xw = Crossword(num_rows=height, num_cols=width)
xw = Crossword(max([height,width]))
for y,row in enumerate(template.splitlines()):
    for x,letter in enumerate(row):
        letter = letter.upper()
        if letter == ".":
            letter = EMPTY
        elif (letter < 'A') or (letter > 'Z'):
            letter = BLACK
        xw[(y,x)] = letter

xw.pprint()




