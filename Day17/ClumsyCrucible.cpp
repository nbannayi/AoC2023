#include <iostream>
#include <set>
#include <queue>
#include <stack>
#include <stdio.h>

#include "Node.h"
#include "Move.h"

using namespace std;

// Parse input from file into map (do this bit the old-skool C way!)
int** parseInputFile(const char* inputFile, int* numRows, int* numCols)
{
    FILE *file = fopen(inputFile, "r");

    /* Read the dimensions of the array. */
    fscanf(file, "%d %d", numRows, numCols);
    
    /* Allocate memory for the 2D array. */
    int** heatMap = (int**)malloc(*numRows * sizeof(int*));    
    for (int i = 0; i < *numRows; i++) 
    {
        heatMap[i] = (int*)malloc(*numCols * sizeof(int));
    }    

    /* Populate array. */
    for (int i = 0; i < *numRows; i++) 
    {
        for (int j = 0; j < *numCols; j++) 
        {
            char digit;
            fscanf(file, " %c", &digit);
            heatMap[i][j] = digit - '0';
        }
    }
    fclose(file);    
    
    return heatMap;
}

// Use Dijkstra's algorithm to navigate around the heatmap.
// Pass utlraCrucibles = 0 for part 1, pass as 1 for part 2.
int navigate(int **heatMap, int noRows, int noCols, int goalx, int goaly, int ultraCrucibles)
{
    // Add start nodes to priority queue.
    Node node1(1,0,1,EAST);
    Node node2(0,1,1,SOUTH);
    Move move1(node1,heatMap[0][1]);
    Move move2(node2,heatMap[1][0]);

    // Data structures required for Dijkstra.
    std::set<Node> visitedNodes;
    std::priority_queue<Move> movePriorityQueue;    

    // Push first two moves onto the priority queue.
    movePriorityQueue.push(move1);
    movePriorityQueue.push(move2);

    // Data structure to store the path.
    std::stack<Move> path;

    // Main loop of Dijkstra.
    while (!movePriorityQueue.empty())
    {
        // Get (and remove) top item from priority queue.
        Move currentMove = movePriorityQueue.top();
        movePriorityQueue.pop();

        // Check if we have visited it, if so loop back.        
        Node currentNode = currentMove.getNode(); 

        auto it = visitedNodes.find(currentNode);
        if (it != visitedNodes.end()) 
        {
            continue;
        }

        // Add to visted nodes.
        visitedNodes.insert(currentNode);

        // If we have reached the end, exit.
        if (currentNode.getX() == goalx && currentNode.getY() == goaly)
        {
            if (ultraCrucibles == 0 || (ultraCrucibles = 1 && currentNode.getNoConsecutive() >= 4))
            {
                return currentMove.getHeatloss();
            }            
        }

        // Get all neighbours.
        std::set<Move> neighbours = currentMove.getNeighbours(heatMap, noRows, noCols, ultraCrucibles);
        
        // Add neighbours to queue.
        for (const auto& neighbour : neighbours) 
        {
            movePriorityQueue.push(neighbour);
            path.push(neighbour);
        }        
    }

    return 0;
}

// Advent of Code 2023, day 17 - Clumsy Crucible.
// C/C++.
int main() 
{              
    // Parse input into heatMap.
    const char *filename = "Day17Input.txt";
    int noRows, noCols;

    // Parse input data into a 2D array of blocks.
    int **heatMap = parseInputFile(filename, &noRows, &noCols);

    // Find path 1.
    int result1 = navigate(heatMap, noRows, noCols, noRows-1, noCols-1, 0);    
    cout << "Part 1 answer: " << result1 << endl;

    // Find path 2.
    int result2 = navigate(heatMap, noRows, noCols, noRows-1, noCols-1, 1);    
    cout << "Part 2 answer: " << result2 << endl;
    
    return 0;
}