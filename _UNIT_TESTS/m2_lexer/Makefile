# Modula-2 R10 Compiler (m2r10c)
#
# Makefile for use with the GCC toolchain
#
# This file was contributed by Peter Eiserloh and modified by B.Kowarsch
#
# Copyright (C) 2009, 2010 by P.Eiserloh and B.Kowarsch. All rights reserved.
#
# License:
#
#  Permission is hereby granted to review and test this software for the sole
#  purpose of supporting the effort by the licensor  to implement a reference
#  compiler for  Modula-2 R10.  It is not permissible under any circumstances
#  to  use the software  for the purpose  of creating derivative languages or 
#  dialects.  This permission is valid until 31 December 2010, 24:00h GMT.
#
# Future licensing:
#
#  The licensor undertakes  to release  this software  under a BSD-style open
#  source license  AFTER  the M2R10 language definition has been finalised.


# ---------------------------------------------------------------------------
# A P P L I C A T I O N S
# ---------------------------------------------------------------------------

APP = m2_lexer_test

SRC = \
	../driver.c \
	../../_REFERENCE_COMPILER/KVS.c \
	../../_REFERENCE_COMPILER/m2_reserved_words.c \
	../../_REFERENCE_COMPILER/m2_tokens.c \
	../../_REFERENCE_COMPILER/m2_lexer.c \
	m2_lexer_test.c

LIB =


# ---------------------------------------------------------------------------
# T O O L S
# ---------------------------------------------------------------------------

RM = rm -f

CC = gcc
CFLAGS = -Wall -Wno-comments -g

LD = gcc
LDFLAGS =


# ---------------------------------------------------------------------------
# A U T O M A T I C -- D O  N O T  E D I T  B E L O W  T H I S  L I N E
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# S O U R C E  C L A S S I F I C A T I O N
# ---------------------------------------------------------------------------

C_SOURCE = $(filter %.c,$(SRC))
C_HEADER = *.h


# ---------------------------------------------------------------------------
# O B J E C T  C L A S S I F I C A T I O N
# ---------------------------------------------------------------------------

C_OBJECT = $(C_SOURCE:%.c=%.o)


# ---------------------------------------------------------------------------
# M A I N  T A R G E T S
# ---------------------------------------------------------------------------

.PHONY: all clean tidy summary

all: $(APP)

clean:
	$(RM) $(C_OBJECT)

tidy: clean
	$(RM) depends.mk *.o $(APP)

summary:
	@ echo C Sources = $(C_SOURCE)
	@ echo C Headers = $(C_HEADER)
	@ echo C Objects = $(C_OBJECT)
	@ echo Libraries = $(LIB)


# ---------------------------------------------------------------------------
# B U I L D  T A R G E T S
# ---------------------------------------------------------------------------

%.o : %.c
	$(CC) -c $(CFLAGS) $< -o $@

$(APP) : $(C_OBJECT)
	$(LD) $(LDFLAGS) $(LIB:%=-l%) $^ -o $@


# ---------------------------------------------------------------------------
# D E P E N D E N C I E S
# ---------------------------------------------------------------------------

depends.mk : Makefile $(C_SOURCE) $(C_HEADER)
	$(CC) -MM $(C_SOURCE) > depends.mk

-include depends.mk


# END OF FILE
