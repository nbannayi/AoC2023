#ifndef BLOCK_HEAP_H
#define BLOCK_HEAP_H

#include "ClumsyCrucible.h"

#define MAX_HEAP_SIZE 20000

/* Priority heap. */
typedef struct {
    Block* array;
    int size;
    int capacity;
} BlockHeap;

/* Create a block heap */
BlockHeap* BlockHeap_create(int capacity);

/* Insert item in block heap */
void BlockHeap_insert(BlockHeap* heap, Block key);

/* Extract minimum item from block heap (by heat loss) */
Block BlockHeap_extractMin(BlockHeap* heap);

/* Display block heap contents. */
void BlockHeap_display(BlockHeap* heap);

/* Free up memory taken up by block heap */
void BlockHeap_cleanup(BlockHeap* heap);

#endif