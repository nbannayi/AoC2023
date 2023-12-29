#ifndef DIJKSTRA_H
#define DIJKSTRA_H

#include "ClumsyCrucible.h"

/* Function to display the shortest path */
void Dijkstra_displayShortestPath(Block **predecessors, int startRow, int startCol, int endRow, int endCol);

/* Output the result (distances array now contains the shortest distances) */
void Dijkstra_displayShortestDistances(int **heatLosses, int numRows, int numCols);

/* Dijkstra's algorithm. */
void Dijkstra_navigate(Block **map, int numRows, int numCols, Block start, int **heatLosses, Block **predecessors);

/* Cleanup all memory allocated for Dijkstra */
void Dijkstra_cleanup(Block **predecessors, int **heatLosses, int numRows, int numCols);

#endif