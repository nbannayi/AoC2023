// Move.h
#ifndef MOVE_H
#define MOVE_H

#include "Node.h"
#include <iostream>

/// @brief Represents a nove around the map with total heatloss.
class Move
{
    private:
        Node node;
        int heatloss;

    public:    
        // Constructor.
        Move(Node node, int heatloss);
        Move();            

        // Getter methods
        Node getNode() const;
        int getHeatloss() const;

        // Overload the less than operator for comparison
        bool operator<(const Move& other) const;

        int getNextMove(int dir, int noRows, int noCols, int **heatmap, int noConsecutive, Move &move);

        std::set<Move> getNeighbours(int** heatMap, int noRows, int noCols);

        // Overload the << operator to display a Move instance.
        friend std::ostream& operator<<(std::ostream& os, const Move& move) 
        {
            std::string strDir;
            switch (move.node.getDir())
            {
                case NORTH:
                    strDir = "NORTH";
                    break;
                case SOUTH:
                    strDir = "SOUTH";
                    break;
                case WEST:
                    strDir = "WEST";
                    break;
                case EAST:
                    strDir = "EAST";
                    break;
            }
            os << "Move: Node(" << move.node.getX() << ", " << move.node.getY() << "), Dir = " << strDir
               << ", Consecutive = " << move.node.getNoConsecutive() << ", Heatloss = " << move.getHeatloss();
            return os;
        }
};

#endif