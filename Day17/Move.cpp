// Move.cpp

#include <set>
#include "Move.h"

using namespace std;

// Constructor definition.
Move::Move(Node node, int heatloss)
    : node(node), heatloss(heatloss) {}

// Getter method definitions.
Node Move::getNode() const 
{
    return node;
}

int Move::getHeatloss() const 
{
    return heatloss;
}

// Get next move with accumulated heatloss from grid.
int Move::getNextMove(int dir, int noRows, int noCols, int **heatmap, int noConsecutive, Move &move)
{
    int newx, newy;
    
    switch (dir)
    {
        case NORTH:
            newx = node.getX();
            newy = node.getY()-1;
            break;
        case SOUTH:
            newx = node.getX();            
            newy = node.getY()+1;
            break;
        case EAST:
            newx = node.getX()+1;
            newy = node.getY();
            break;
        case WEST:
            newx = node.getX()-1;
            newy = node.getY();
            break;
    } 
    if (newx >= 0 && newx < noCols && newy >= 0 && newy < noRows) 
    {
        Node nextNode(newx, newy, noConsecutive, dir);
        move = Move(nextNode, heatloss + heatmap[nextNode.getY()][nextNode.getX()]);
        return 0;
    }

    return 1;
}

// Get a set of all neighbours.
std::set<Move> Move::getNeighbours(int** heatMap, int noRows, int noCols) 
{
    std::set<Move> neighbours;

    int leftDir = (node.getDir() - 1) % 4;
    while (leftDir < 0) leftDir += 4;

    Move leftMove(Node(0, 0, 1, 0), 0); // Default.
    int leftResult = getNextMove(leftDir, noRows, noCols, heatMap, 1, leftMove);
    if (leftResult == 0) 
    {
        neighbours.insert(leftMove);
    }

    int rightDir = (node.getDir() + 1) % 4;    
    Move rightMove(Node(0, 0, 1, 0), 0); // Default.
    int rightResult = getNextMove(rightDir, noRows, noCols, heatMap, 1, rightMove);
    if (rightResult == 0) 
    {
        neighbours.insert(rightMove);
    }

    Move aheadMove(Node(0, 0, 1, 0), 0); // Default.
    if (node.getNoConsecutive() < 3) 
    {
        int aheadResult = getNextMove(node.getDir(), noRows, noCols, heatMap, node.getNoConsecutive()+1, aheadMove);        
        if (aheadResult == 0) 
        {
            neighbours.insert(aheadMove);
        }
    }

    return neighbours;
}

// Overload the less than operator for comparison.
bool Move::operator<(const Move& other) const {
    if (heatloss != other.heatloss) 
        return heatloss > other.heatloss;    
    if (node.getDir() == other.getNode().getDir() && node.getNoConsecutive() != other.getNode().getNoConsecutive()) 
        return node.getNoConsecutive() > other.getNode().getNoConsecutive();
    if (node.getY() != other.getNode().getY()) 
        return node.getY() > other.getNode().getY();
    return node.getX() > other.getNode().getX();
}