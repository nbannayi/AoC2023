// Node.cpp
#include "Node.h"

// Constructor definition.
Node::Node(int x, int y, int noConsecutive, int dir)
    : x(x), y(y), dir(dir), noConsecutive(noConsecutive) {}

// Getter method definitions.
int Node::getX() const {
    return x;
}

int Node::getY() const {
    return y;
}

int Node::getDir() const {
    return dir;
}

int Node::getNoConsecutive() const {
    return noConsecutive;
}

// Overload the less than operator for comparison
bool Node::operator<(const Node& other) const {
    if (x != other.x) 
        return x > other.x;
    if (y != other.y) 
        return y > other.y;
    if (dir != other.dir) 
        return dir > other.dir;
    return noConsecutive > other.noConsecutive;
}