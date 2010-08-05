/*! Gemeral purpose memory allocation macros
 *
 *  @file  alloc.h
 *  @brief Memory allocator interface
 *
 *  Legal Notice:
 *
 *  This file ("alloc.h") was released into the public domain
 *  by Sunrise Telephone Systems KK, Tokyo, Japan.
 *
 */

#ifndef ALLOC_H
#define ALLOC_H

#include <stdlib.h>

// allocation funtion
#define ALLOCATE(_size) malloc(_size)

// deallocation function
#define DEALLOCATE(_pointer) free(_pointer)

#endif /* ALLOC_H */

// END OF FILE