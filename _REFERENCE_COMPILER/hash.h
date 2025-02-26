/*! Gemeral purpose 32-bit hash function
 *
 *  @file  hash.h
 *  @brief 32-bit hash function
 *
 *  Legal Notice:
 *
 *  This file ("hash.h") was released into the public domain
 *  by Sunrise Telephone Systems KK, Tokyo, Japan.
 *
 */

#ifndef HASH_H
#define HASH_H

// initial value
#define HASH_INITIAL 0

// iterative value
#define HASH_NEXT_CHAR(_hash,_ch) \
    (_ch + (_hash << 6) + (_hash << 16) - _hash)

// final value
#define HASH_FINAL(_hash) (_hash & 0x7FFFFFFF)

#endif /* HASH_H */

// END OF FILE