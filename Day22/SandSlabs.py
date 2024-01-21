# Advent of Code 2023, day 22 - Sand Slabs.
# Python.
import copy

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

def get_slab_blocks(slab):
    '''Explode a slab into constituent blocks.'''
    end_1, end_2 = slab[0], slab[1]
    if end_1[0] != end_2[0]:
        blocks = [[n, end_1[1], end_1[2]] for n in range(end_1[0], end_2[0]+1)]
    elif end_1[1] != end_2[1]:
        blocks = [[end_1[0], n, end_1[2]] for n in range(end_1[1], end_2[1]+1)]
    else:
        blocks = [[end_1[0], end_1[1], n] for n in range(end_1[2], end_2[2]+1)]
    return blocks

def drop_slab(slab, grid):
    '''Given a slab drops it into the grid and returns a grid with all blocks filled where they land,'''
    '''and the new slab at the landed location'''
    # Firstly given a slab work out all blocks it comprises.
    blocks = get_slab_blocks(slab)
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
    # Work out landed slab location.
    no_blocks = len(blocks)
    landed_slab = [[blocks[0][0],blocks[0][1],blocks[0][2]-offset+1], 
        [blocks[no_blocks-1][0],blocks[no_blocks-1][1],blocks[no_blocks-1][2]-offset+1]]
    return landed_slab, grid

def can_drop(slab, grid):
    '''Returns true if the slab can drop.'''
    blocks = get_slab_blocks(slab)
    # Get all blocks at the lowest level.
    min_z = sorted(blocks, key=lambda x: x[2])[0][2]
    lowest_blocks = [sublist for sublist in blocks if sublist[2] == min_z]
    # Check if all blocks can move down 1 block.
    can_drop = True
    solid_count = 0
    for block in lowest_blocks:
        x,y,z = block
        if z-1 == 0:
            can_drop = False
            break
        else:
            solid_count += grid[x][y][z-1]
    if solid_count > 0:
        can_drop = False 
    return can_drop
    
def drop_slabs(slabs, grid):
    '''Given a stack of slabs drops them into the grid and returns the landed slabs and a grid with all blocks'''
    '''filled where they land.'''
    landed_slabs = []
    while(len(slabs) > 0):
        slab = slabs.pop()
        landed_slab, grid = drop_slab(slab, grid)
        landed_slabs.append(landed_slab)
    return landed_slabs, grid

def display_grid(grid):
    '''Print the grid in xy slices starting from the bottom'''
    for z in range(9):
        print("z="+str(z))
        for y in range(3):
            for x in range(3):            
                print(grid[x][y][z], end='')
            print()
        print()

def can_disintegrate_slab(slab, landed_slabs, grid):
    '''Check is slab can be disintegrated'''
    # First disintegrate slab in a copy of the grid.    
    grid_copy = copy.deepcopy(grid) 
    for block in get_slab_blocks(slab):
        x,y,z = block
        grid_copy[x][y][z] = 0
    # Now check if anything moves (might need optimising.)
    potential_slabs_above = [sublist for sublist in landed_slabs if slab[1][2]+1 == sublist[0][2]]
    for slab in potential_slabs_above:
        if can_drop(slab, grid_copy):
            return False
    return True

def get_no_disintegratable_slabs(slabs, grid):
    '''Given parsed slabs and grid, return total of how many can be disintegrated.'''
    landed_slabs, grid = drop_slabs(slabs, grid)
    total = 0
    for slab in landed_slabs:
        if can_disintegrate_slab(slab, landed_slabs, grid):
            total += 1
    return total

# Part 1.
input_file = "Day22Input.txt"
slabs = parse_slabs(input_file)
grid = init_grid(slabs)
print("Part 1 answer:", get_no_disintegratable_slabs(slabs, grid))