#ifndef CLUMSY_CRUCIBLE_H
#define CLUMSY_CRUCIBLE_H

#define MAX_ROWS 150
#define MAX_COLS 150

/* Define a block in the map */
typedef struct {
    int row;
    int col;
    int heatLoss;
} Block;

#endif