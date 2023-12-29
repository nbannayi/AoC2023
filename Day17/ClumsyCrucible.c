/* Advent of Code 2023, day 17 - Clumsy Crucible */
/* C */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include "ClumsyCrucible.h"
#include "BlockHeap.h"
#include "Dijkstra.h"

/* Get the input file data into map. */
Block** parseInputFile(const char* inputFile, int* numRows, int* numCols)
{
    FILE *file = fopen(inputFile, "r");

    /* Read the dimensions of the array. */
    fscanf(file, "%d %d", numRows, numCols);
    
    /* Allocate memory for the 2D array. */
    Block** map = (Block**)malloc(*numRows * sizeof(Block*));    
    for (int i = 0; i < *numRows; i++) 
    {
        map[i] = (Block*)malloc(*numCols * sizeof(Block));
    }    

    /* Populate array. */
    for (int i = 0; i < *numRows; i++) 
    {
        for (int j = 0; j < *numCols; j++) 
        {
            char digit;
            fscanf(file, " %c", &digit);
            map[i][j].row = i;
            map[i][j].col = j;
            map[i][j].heatLoss = digit - '0';
        }
    }
    fclose(file);    
    
    return map;
}

/* Display contents of the map. */
void displayMap(Block **map, int numRows, int numCols)
{
    printf("Content of the map:\n");
    for (int i = 0; i < numRows; i++) 
    {
        for (int j = 0; j < numCols; j++) 
        {
            printf("(%d,%d), HL=%d\n", map[i][j].row, map[i][j].col, map[i][j].heatLoss);
        }
        printf("\n");
    }
}

/* Free memory in map */
void cleanup(Block **map, int numRows)
{
    /* Free the allocated memory. */
    if (map != NULL) 
    {
        for (int i = 0; i < numRows; i++) 
        {
            free(map[i]);
        }
        free(map);
    }
}

int main() 
{   
    const char *filename = "Day17InputExample2.txt";
    int numRows, numCols;

    /* Parse input data into a 2D array of blocks. */   
    Block **map = parseInputFile(filename, &numRows, &numCols);

    /* Navigate using Dijkstra. */
    Block **predecessors;
    int **heatLosses;
    Dijkstra_navigate(map, numRows, numCols, map[0][0], heatLosses, predecessors);

    /* Cleanup memory */
    cleanup(map, numRows);

    return 0;
}