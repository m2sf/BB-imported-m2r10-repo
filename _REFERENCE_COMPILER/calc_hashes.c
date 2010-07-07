/*
 *  calc_hashes.c
 *
 *  Utility to calculate hashes of reserved words and built-in identifiers
 *  for use within the Modula-2 R10 reference compiler
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include "hash.h"

int main (int argc, const char **argv) {
    unsigned int i, j, hash;
    unsigned char *ident;
	
    if (argc <= 1) {
        printf("usage: calc_hashes string { string }\n");
        exit(1);
    } // end if

    for (i = 1; i < argc; i++) {
    
        ident = (unsigned char *)argv[i];
                
        j = 0;
        hash = HASH_INITIAL;
        
        while (ident[j] != 0) {
            hash =  HASH_NEXT_CHAR(hash, ident[j]);
            j++;
        } // end while
        
        hash = HASH_FINAL(hash);

        printf("0x%*0X   /* %s */\n", 8, hash, ident);

    } // end for
    
    return 0;
} // end main

// END OF FILE