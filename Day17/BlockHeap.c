#include <stdio.h>
#include <stdlib.h>
#include "BlockHeap.h"
#include "ClumsyCrucible.h"

/* Create a block heap */
BlockHeap* BlockHeap_create(int capacity) 
{    
    BlockHeap* heap = (BlockHeap*)malloc(sizeof(BlockHeap));
    if (heap == NULL) {
        perror("Memory allocation error");
        exit(EXIT_FAILURE);
    }
    heap->array = (Block*)malloc(capacity * sizeof(Block));
    if (heap->array == NULL) {
        perror("Memory allocation error");
        free(heap);
        exit(EXIT_FAILURE);
    }
    heap->size = 0;
    heap->capacity = capacity;
    return heap;
}

/* Swap two items in a block heap */
void BlockHeap_swap(Block* a, Block* b) 
{
    Block temp = *a;
    *a = *b;
    *b = temp;
}

/* Reorder items in block heap */
void BlockHeap_heapifyUp(BlockHeap* heap, int index) 
{
    int parent = (index - 1) / 2;
    while (index > 0 && heap->array[index].heatLoss < heap->array[parent].heatLoss) {
        BlockHeap_swap(&heap->array[index], &heap->array[parent]);
        index = parent;
        parent = (index - 1) / 2;
    }
}

/* Insert item in block heap */
void BlockHeap_insert(BlockHeap* heap, Block key) 
{
    if (heap->size >= heap->capacity) {
        fprintf(stderr, "Heap overflow\n");
        return;
    }
    heap->size++;
    int index = heap->size - 1;
    heap->array[index] = key;
    BlockHeap_heapifyUp(heap, index);
}

/* Extract minimum item from block heap (by heat loss) */
Block BlockHeap_extractMin(BlockHeap* heap) 
{
    if (heap->size <= 0) 
    {
        fprintf(stderr, "Heap underflow\n");
        exit(EXIT_FAILURE);
    }
    if (heap->size == 1) 
    {
        heap->size--;
        return heap->array[0];
    }
    Block root = heap->array[0];
    heap->array[0] = heap->array[heap->size - 1];
    heap->size--;
    // Heapify down.
    int index = 0;
    while (1) 
    {
        int leftChild = 2 * index + 1;
        int rightChild = 2 * index + 2;
        int smallest = index;
        if (leftChild < heap->size && heap->array[leftChild].heatLoss < heap->array[smallest].heatLoss) 
        {
            smallest = leftChild;
        }
        if (rightChild < heap->size && heap->array[rightChild].heatLoss < heap->array[smallest].heatLoss) 
        {
            smallest = rightChild;
        }
        if (smallest != index) 
        {
            BlockHeap_swap(&heap->array[index], &heap->array[smallest]);
            index = smallest;
        } 
        else 
        {
            break;
        }
    }
    return root;
}

/* Display contents of block heap. */
void BlockHeap_display(BlockHeap* heap) 
{
    printf("BlockHeap contents:\n");
    for (int i = 0; i < heap->size; i++) {
        printf("(%d, %d, %d)\n", heap->array[i].row, heap->array[i].col, heap->array[i].heatLoss);
    }
    printf("\n");
}

/* Free up memory taken up by block heap */
void BlockHeap_cleanup(BlockHeap* heap) 
{
    free(heap->array);
    free(heap);
}