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


include ../version.mk

SHELL = /bin/sh

# ---------------------------------------------------------------------------
# A P P L I C A T I O N S
# ---------------------------------------------------------------------------

APPL = m2r10c


CC = gcc
CXX = g++

CFLAGS = -Wall -g
CXXFLAGS = -Wall -g
LDFLAGS =


DEFS =

OBJS = 	\
	KVS.o \
	m2_reserved_words.o \
	m2_tokens.o \
	m2_lexer.o \
	m2_productions.o \
	m2_tokenset.o \
	main.c

#	m2_filenames.o \
#	m2_parser.o \
#	m2_ast.o \
#	m2_te.o


LIBS =


c_sources 	= *.c
headers		= *.h
sources		= $(c_sources) $(headers)


# ---------------------------------------------------------------------------
# O B J E C T   F I L E S
# ---------------------------------------------------------------------------

%.o:%.c
	$(CC) -c $(CFLAGS) $< -o $@


# ---------------------------------------------------------------------------
# M A I N   T A R G E T S
# ---------------------------------------------------------------------------

all: $(APPL)

clean:
	$(RM) *.o

dist:

dist-clean:

install:

uninstall:


# ---------------------------------------------------------------------------
# B U I L D S
# ---------------------------------------------------------------------------

$(APPL): $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o $@


# ---------------------------------------------------------------------------
# D E P E N D E N C I E S
# ---------------------------------------------------------------------------

include depends.mk

depends.mk: Makefile $(sources)
	$(CC) -MM $(c_sources) >depends.mk

# END OF FILE
