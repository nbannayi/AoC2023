#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include "ClumsyCrucible.h"
#include "BlockHeap.h"
#include "Dijkstra.h"

/* Function to display the shortest path */
void Dijkstra_displayShortestPath(Block **predecessors, int startRow, int startCol, int endRow, int endCol) 
{
    printf("Shortest path from (%d, %d) to (%d, %d):\n", startRow, startCol, endRow, endCol);

    int currentRow = endRow;
    int currentCol = endCol;

    /* Create path grid */
    char grid[endCol+1][endCol+1];
    for (int i = 0; i <= endCol; i++)
    {
        for (int j = 0; j <= endCol; j++)
        {
            grid[i][j] = '.';
        }
    }
    grid[endCol][endCol] = '#';

    // Backtrack from the destination to the starting point
    while (currentRow != startRow || currentCol != startCol) {
        printf("(%d, %d) <- ", currentRow, currentCol);

        // Move to the predecessor
        Block predecessor = predecessors[currentRow][currentCol];
        currentRow = predecessor.row;
        currentCol = predecessor.col;
        grid[currentRow][currentCol] = '#';
    }    
    printf("(%d, %d)\n", startRow, startCol);

    /* Display path grid */
    for (int i = 0; i <= endCol; i++)
    {
        for (int j = 0; j <= endCol; j++)
        {
            printf("%c", grid[i][j]);
        }
        printf("\n");
    }
}

/* Output the result (distances array now contains the shortest distances) */
void Dijkstra_displayShortestDistances(int **heatLosses, int numRows, int numCols)
{
    printf("Matrix of lowest heat loss:\n");
    for (int i = 0; i < numRows; i++) 
    {
        for (int j = 0; j < numCols; j++) 
        {
            if (heatLosses[i][j] == INT_MAX) 
            {
                printf("INF ");
            } 
            else 
            {
                printf("%d ", heatLosses[i][j]);
            }
        }
        printf("\n");
    }
}

/* Dijkstra's algorithm. */
void Dijkstra_navigate(Block **map, int numRows, int numCols, Block start, int **heatLosses, Block **predecessors) 
{
    /* Create a heap for the priority queue */
    BlockHeap* queue = BlockHeap_create(numRows * numCols);

    /* Initialize heatloss values for all blocks to infinity */
    heatLosses = (int **)malloc(numRows * sizeof(int *));
    for (int i = 0; i < numRows; i++) 
    {
        heatLosses[i] = (int *)malloc(numCols * sizeof(int));
        for (int j = 0; j < numCols; j++) 
        {
            heatLosses[i][j] = INT_MAX;
        }
    }

    /* Set the heatloss to 0 for first block. */
    heatLosses[start.row][start.col] = 0;

    /* Insert the starting block into the priority queue. */
    BlockHeap_insert(queue, start);

    /* Keep track of consecutive moves in each direction. */
    int consecutiveMoves[4] = {0};  // 0: up, 1: down, 2: left, 3: right

    /* Keep track of predecessors for path reconstruction */
    predecessors = (Block**)malloc(numRows * sizeof(Block*));    
    for (int i = 0; i < numRows; i++) 
    {
        predecessors[i] = (Block*)malloc(numCols * sizeof(Block));        
    }

    /* Main loop */
    while (queue->size >0) 
    {        
        /* Extract the node with the smallest tentative distance from the priority queue. */
        Block currentBlock = BlockHeap_extractMin(queue);

        /* Process neighbours of the current node. */
        for (int dir = 0; dir < 4; dir++) // Assuming 4 directions: up, down, left, right.
        {  
            int newRow = currentBlock.row + (dir == 0 ? -1 : (dir == 1 ? 1 : 0));
            int newCol = currentBlock.col + (dir == 2 ? -1 : (dir == 3 ? 1 : 0));

            /* Check if the neighbour is within bounds. */
            if (newRow >= 0 && newRow < numRows && newCol >= 0 && newCol < numCols) 
            {
                /* Calculate the tentative distance to the neighbour. */
                int tentativeHeatLoss = heatLosses[currentBlock.row][currentBlock.col] + map[newRow][newCol].heatLoss;

                /* Update the distance if it's smaller */
                if (tentativeHeatLoss < heatLosses[newRow][newCol]) 
                {
                    heatLosses[newRow][newCol] = tentativeHeatLoss;

                    /* Update predecessor for path reconstruction */
                    predecessors[newRow][newCol] = currentBlock;

                    /* Insert the updated neighbor into the priority queue */
                    Block neighbour = {newRow, newCol, tentativeHeatLoss};
                    BlockHeap_insert(queue, neighbour);
                }
            }
        }
    }

    Dijkstra_displayShortestPath(predecessors, 0, 0, numRows-1, 9);
    Dijkstra_displayShortestDistances(heatLosses, numRows, numCols);
    Dijkstra_cleanup(predecessors, heatLosses, numRows, numCols);
}

/* Cleanup all memory allocated for Dijkstra */
void Dijkstra_cleanup(Block **predecessors, int **heatLosses, int numRows, int numCols)
{
    /* Free the allocated memory for predecessor values. */
    for (int i = 0; i < numRows; i++) 
    {
        free(predecessors[i]);
        free(heatLosses[i]);
    }
    free(predecessors);
    free(heatLosses);
}