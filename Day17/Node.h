// Node.h
#ifndef NODE_H
#define NODE_H

// Direction in which to move.
#define NORTH 0
#define EAST 1
#define SOUTH 2
#define WEST 3

#include <iostream>

// Represents a node in the map.
class Node
{
    private:
        int x;
        int y;
        int dir;
        int noConsecutive;

    public:
        // Constructor
        Node(int x, int y, int noConsecutive, int direction);

        // Getter methods
        int getX() const;
        int getY() const;
        int getDir() const;
        int getNoConsecutive() const;

        // Overload the less than operator for comparison
        bool operator<(const Node& other) const;

        // Overload the << operator to display a Node instance.
        friend std::ostream& operator<<(std::ostream& os, const Node& node) 
        {
            std::string strDir;
            switch (node.getDir())
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
            os << "Node: (" << node.getX() << ", " << node.getY() << "), Dir = " << strDir 
               << ", Consecutive = " << node.getNoConsecutive();
            return os;
        }
};

#endif