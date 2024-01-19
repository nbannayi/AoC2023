# Advent of Code 2023, day 22 - Sand Slabs.
# Python.

def parse_slabs(input_file): 
    '''Given an input file return a stack of slabs sorted with those closest to ground at top.'''
    # List to be used as a stack.
    slabs = []
    # Load raw data.
    with open(input_file, 'r') as file:
        for line in file:        
            ends = line.strip().split('~')
            cube_1 = [int(c) for c in ends[0].split(',')]
            cube_2 = [int(c) for c in ends[1].split(',')]
            slabs.append([cube_1, cube_2])
    # Sort blocks so that those closest to ground are nearest top.
    slab_levels = [(min(slab[0][2], slab[1][2]), slab) for slab in slabs]
    sorted_slabs = sorted(slab_levels, key=lambda x: x[0], reverse=True)
    slabs = [slab[1] for slab in sorted_slabs]
    return slabs

def init_grid(slabs):
    '''Given a list of slabs initialise a grid with the appropriate dimensions.'''
    max_x = max(max(slabs, key=lambda x: x[0][0])[0][0], max(slabs, key=lambda x: x[1][0])[1][0])    
    max_y = max(max(slabs, key=lambda x: x[0][1])[0][1], max(slabs, key=lambda x: x[1][1])[1][1])       
    max_z = max(max(slabs, key=lambda x: x[0][2])[0][2], max(slabs, key=lambda x: x[1][2])[1][2])    
    grid = [[[0 for _ in range(max_z+1)] for _ in range(max_y+1)] for _ in range(max_x+1)]
    return grid

def drop_slab(slab, grid):
    '''Given a slab drops it into the grid and returns a grid with all blocks filled where they land.'''
    # Firstly given a slab work out all blocks it comprises.
    end_1, end_2 = slab[0], slab[1]
    if end_1[0] != end_2[0]:
        blocks = [[n, end_1[1], end_1[2]] for n in range(end_1[0], end_2[0]+1)]
    elif end_1[1] != end_2[1]:
        blocks = [[end_1[0], n, end_1[2]] for n in range(end_1[1], end_2[1]+1)]
    else:
        blocks = [[end_1[0], end_1[1], n] for n in range(end_1[2], end_2[2]+1)]
    # For each slab move it down as far as possible in grid.
    blocked = False
    offset = 0
    while (not blocked):
        solid_count = 0
        for block in blocks:
            x,y,z = block
            if z-offset == 0:
                solid_count = 1
                break
            else: 
                solid_count += grid[x][y][z-offset]
        if solid_count == 0:            
            offset += 1
        else:
            for block in blocks:
                x,y,z = block
                grid[x][y][z-offset+1] = 1
            blocked = True
    return grid    

def drop_slabs(slabs, grid):
    '''Given a stack of slabs drops them into the grid and returns a grid with all blocks filled where they land.'''
    slabs_stack = list(slabs)
    while(len(slabs_stack) > 0):
        slab = slabs_stack.pop()
        grid = drop_slab(slab, grid)
    return grid

# Part 1.
input_file = "Day22InputExample.txt"
slabs = parse_slabs(input_file)
grid = init_grid(slabs)
new_grid = drop_slabs(slabs, grid)

for z in range(9):
    print("z="+str(z))
    for y in range(3):
        for x in range(3):            
            print(new_grid[x][y][z], end='')
        print()
    print()
