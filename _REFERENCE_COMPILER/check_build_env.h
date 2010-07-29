/* Modula-2 R10 Compiler (m2r10c)
 *
 *  chek_build_env.h
 *  Determine build emvironment from predefined compiler macros
 *  Abort compilation in progress if C99 is not enabled
 *
 *  Author: Benjamin Kowarsch
 *
 *  Copyright (C) 2010 B.Kowarsch. All rights reserved.
 *
 *  License:
 *
 *  Permission is hereby granted to review and test this software for the sole
 *  purpose of supporting the effort by the licensor  to implement a reference
 *  compiler for  Modula-2 R10.  It is not permissible under any circumstances
 *  to  use the software  for the purpose  of creating derivative languages or 
 *  dialects.  This permission is valid until 31 December 2010, 24:00h GMT.
 *
 *  Future licensing:
 *
 *  The licensor undertakes  to release  this software  under a BSD-style open
 *  source license  AFTER  the M2R10 language definition has been finalised.
 *  
 */


#ifndef CHECK_BUILD_ENV_H
#define CHECK_BUILD_ENV_H


// ===========================================================================
// Determine host OS and default filesystem
// ===========================================================================

// ---------------------------------------------------------------------------
// Test for AIX
// ---------------------------------------------------------------------------
#if defined(_AIX)

#define HOST_OS_AIX
#define HOST_OS_UNIX
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for Amiga OS
// ---------------------------------------------------------------------------
#elif defined(AMIGA) || defined(__amigaos__)

#define HOST_OS_AMIGA
#define HOST_FILESYSTEM_AMIGA

// ---------------------------------------------------------------------------
// Test for BSD (FreeBSD)
// ---------------------------------------------------------------------------
#elif defined(__FreeBSD__)

#define HOST_OS_BSD
#define HOST_OS_UNIX
#define HOST_OS_FreeBSD
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for BSD (NetBSD)
// ---------------------------------------------------------------------------
#elif defined(__NetBSD__)

#define HOST_OS_BSD
#define HOST_OS_UNIX
#define HOST_OS_NetBSD
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for BSD (OpenBSD)
// ---------------------------------------------------------------------------
#elif defined(__OpenBSD__)

#define HOST_OS_BSD
#define HOST_OS_UNIX
#define HOST_OS_OpenBSD
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for BSD (DragonFly)
// ---------------------------------------------------------------------------
#elif defined(__DragonFly__)

#define HOST_OS_BSD
#define HOST_OS_UNIX
#define HOST_OS_DragonflyBSD
#define HOST_OS_DragonFlyBSD
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for BSD (BSD/OS)
// ---------------------------------------------------------------------------
#elif defined(__bsdi__)

#define HOST_OS_BSD
#define HOST_OS_UNIX
#define HOST_OS_BSDi
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for DG/UX
// ---------------------------------------------------------------------------
#elif defined(DGUX) || defined(__DGUX__) || defined(__dgux__)

#define HOST_OS_UNIX
#define HOST_OS_DGUX
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for GNU Hurd
// ---------------------------------------------------------------------------
#elif defined(__GNU__)

#define HOST_OS_GNU_HURD
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for HP/UX
// ---------------------------------------------------------------------------
#elif defined(hpux) || defined(_hpux) || defined(__hpux)

#define HOST_OS_UNIX
#define HOST_OS_HPUX
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for IRIX
// ---------------------------------------------------------------------------
#elif defined(sgi) || defined(__sgi)

#define HOST_OS_UNIX
#define HOST_OS_IRIX
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for Linux
// ---------------------------------------------------------------------------
#elif defined(linux) || defined(__linux)

#define HOST_OS_UNIX
#define HOST_OS_LINUX
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for Mac OS X
// ---------------------------------------------------------------------------
#elif defined(__APPLE__) || defined(__darwin__)

#define HOST_OS_BSD
#define HOST_OS_UNIX
#define HOST_OS_DARWIN
#define HOST_OS_MACOSX
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test For legacy MacOS (pre-OSX)
// ---------------------------------------------------------------------------
#elif defined(macintosh) || defined (Macintosh)

#define HOST_OS_LEGACY_MACOS
#define HOST_FILESYSTEM_LEGACY_MACOS

// ---------------------------------------------------------------------------
// Test for Minix
// ---------------------------------------------------------------------------
#elif defined(MINIX) || defined (__minix)

#define HOST_OS_UNIX
#define HOST_OS_MINIX
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for MS-DOS
// ---------------------------------------------------------------------------
#elif defined(MSDOS) || defined(__MSDOS__) || defined(__DOS__)

#define HOST_OS_MSDOS
#define HOST_FILESYSTEM_MSDOS

// ---------------------------------------------------------------------------
// Test for MS-Windows
// ---------------------------------------------------------------------------
#elif defined(_WIN32) || defined (__WIN32__) || defined(__WINDOWS__)

#define HOST_OS_WINDOWS
#define HOST_OS_MSWINDOWS
#define HOST_FILESYSTEM_MSDOS

// ---------------------------------------------------------------------------
// Test for OS/2
// ---------------------------------------------------------------------------
#elif defined(OS2) || defined (_OS2) || defined (__OS2__)

#define HOST_OS_OS2
#define HOST_OS_IBM_OS2
#define HOST_FILESYSTEM_MSDOS

// ---------------------------------------------------------------------------
// Test for QNX
// ---------------------------------------------------------------------------
#elif defined(__QNX__) || defined(__QNXNTO__)

#define HOST_OS_UNIX
#define HOST_OS_QNX
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for RSX-11
// ---------------------------------------------------------------------------
#elif defined(RSX11) || defined(__RSX11) || defined(__RSX11__)

#define HOST_OS_RSX11
#define HOST_FILESYSTEM_ODS

// ---------------------------------------------------------------------------
// Test for Solaris
// ---------------------------------------------------------------------------
#elif defined(sun) || defined(__sun)

#define HOST_OS_UNIX
#define HOST_OS_SOLARIS
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for OSF/1 (aka Tru64)
// ---------------------------------------------------------------------------
#elif defined(__osf) || defined(__osf__)

#define HOST_OS_OSF
#define HOST_OS_UNIX
#define HOST_OS_TRU64
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for Ultrix
// ---------------------------------------------------------------------------
#elif defined(ultrix) || defined(__ultrix) || defined(__ultrix__)

#define HOST_OS_BSD
#define HOST_OS_UNIX
#define HOST_OS_ULTRIX
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for VMS (aka OpenVMS)
// ---------------------------------------------------------------------------
#elif defined(VMS) || defined(__VMS)

#define HOST_OS_VMS
#define HOST_FILESYSTEM_ODS

// ---------------------------------------------------------------------------
// Test for XENIX
// ---------------------------------------------------------------------------
#elif defined(M_XENIX)

#define HOST_OS_UNIX
#define HOST_OS_XENIX
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Test for Generic Unix (if all else fails)
// ---------------------------------------------------------------------------
#elif defined(__unix) || defined(__unix__)

#define HOST_OS_UNIX
#define HOST_OS_GENERIC_UNIX
#define HOST_FILESYSTEM_POSIX

// ---------------------------------------------------------------------------
// Unknown host environment
// ---------------------------------------------------------------------------
#else

#define HOST_OS_UNKNOWN
#define HOST_FILESYSTEM_UNKNOWN

#endif /* host environment test */


// ===========================================================================
// Determine host compiler
// ===========================================================================

// ---------------------------------------------------------------------------
// Test for Borland C
// ---------------------------------------------------------------------------
#if defined(__BORLANDC__) || defined(__TURBOC__)

#define HOST_COMPILER_BORLAND
#define C99_SUPPORT_UNKNOWN
#info "Host compiler: Borland C"

// ---------------------------------------------------------------------------
// Test for Clang
// ---------------------------------------------------------------------------
#elif defined(__clang__)

#define HOST_COMPILER_CLANG
#define C99_SUPPORT
#info "Host compiler: Clang"

// ---------------------------------------------------------------------------
// Test for Comeau C
// ---------------------------------------------------------------------------
#elif defined(__COMO__)

#define HOST_COMPILER_COMOC
#define HOST_COMPILER_COMO
#define HOST_COMPILER_COMEAU
#define C99_SUPPORT
#info "Host compiler: Comeau C"

// ---------------------------------------------------------------------------
// Test for DEC C
// ---------------------------------------------------------------------------
#elif defined(__DECC)

#define HOST_COMPILER_DECC
#define HOST_COMPILER_DEC
#define C99_SUPPORT_UNKNOWN
#info "Host compiler: DEC C"

// ---------------------------------------------------------------------------
// Test for CYGWIN GCC
// ---------------------------------------------------------------------------
#elif defined(__CYGWIN__)

#define HOST_COMPILER_CYGWIN
#define C99_SUPPORT
#info "Host compiler: Cygwin GCC"

// ---------------------------------------------------------------------------
// Test for Digital Mars C
// ---------------------------------------------------------------------------
#elif defined(__DMC__)

#define HOST_COMPILER_DMC
#define HOST_COMPILER_DIGITAL_MARS
#define C99_SUPPORT
#info "Host compiler: Digital Mars C"

// ---------------------------------------------------------------------------
// Test for Dignus Systems C
// ---------------------------------------------------------------------------
#elif defined(__SYSC__)

#define HOST_COMPILER_SYSC
#define HOST_COMPILER_DIGNUS
#define NO_C99_SUPPORT
#info "Host compiler: Dignus Systems C"

// ---------------------------------------------------------------------------
// Test for DJGPP
// ---------------------------------------------------------------------------
#elif defined(__DJGPP__)

#define HOST_COMPILER_DJGPP
#define C99_SUPPORT
#info "Host compiler: DJGPP C"

// ---------------------------------------------------------------------------
// Test for Edison Design Group C
// ---------------------------------------------------------------------------
#elif defined(__EDG__)

#define HOST_COMPILER_EDG
#define HOST_COMPILER_EDISON_DESIGN_GROUP
#define C99_SUPPORT
#info "Host compiler: Edison Design Group C"

// ---------------------------------------------------------------------------
// Test for GNU C
// ---------------------------------------------------------------------------
#elif defined(__GNUC__)

#define HOST_COMPILER_GCC
#define HOST_COMPILER_GNU
#define C99_SUPPORT
#info "Host compiler: GNU C"

// ---------------------------------------------------------------------------
// Test for Green Hills C
// ---------------------------------------------------------------------------
#elif defined(__ghs__)

#define HOST_COMPILER_GHS
#define HOST_COMPILER_GREEN_HILLS
#define NO_C99_SUPPORT
#info "Host compiler: Green Hills C"

// ---------------------------------------------------------------------------
// Test for HP C
// ---------------------------------------------------------------------------
#elif defined(__HP_cc)

#define HOST_COMPILER_HPC
#define HOST_COMPILER_HP
#define NO_C99_SUPPORT
#info "Host compiler: HP C"

// ---------------------------------------------------------------------------
// Test for IBM XL C
// ---------------------------------------------------------------------------
#elif defined(__IBMC__) || defined(__xlc__)

#define HOST_COMPILER_XLC
#define HOST_COMPILER_IBMC
#define HOST_COMPILER_IBM
#define C99_SUPPORT
#info "Host compiler: IBM XL C"

// ---------------------------------------------------------------------------
// Test for Imagecraft C
// ---------------------------------------------------------------------------
#elif defined(__IMAGECRAFT__)

#define HOST_COMPILER_IMAGECRAFT
#define NO_C99_SUPPORT
#info "Host compiler: Imagecraft C"

// ---------------------------------------------------------------------------
// Test for Intel C Compiler
// ---------------------------------------------------------------------------
#elif defined(__INTEL_COMPILER) || defined(__ICL)

#define HOST_COMPILER_ICL
#define HOST_COMPILER_INTEL
#define C99_SUPPORT
#info "Host compiler: Intel C"

// ---------------------------------------------------------------------------
// Test for LCC
// ---------------------------------------------------------------------------
#elif defined(__LCC__)

#define HOST_COMPILER_LCC
#define NO_C99_SUPPORT
#info "Host compiler: LCC"

// ---------------------------------------------------------------------------
// Test for LLVM
// ---------------------------------------------------------------------------
#elif defined(__llvm__)

#define HOST_COMPILER_LLVM
#define C99_SUPPORT
#info "Host compiler: LLVM"

// ---------------------------------------------------------------------------
// Test for Metrowerks C
// ---------------------------------------------------------------------------
#elif defined(__MWERKS__)

#define HOST_COMPILER_METROWERKS
#define C99_SUPPORT_UNKNOWN
#info "Host compiler: Metrowerks C"

// ---------------------------------------------------------------------------
// Test for MINGW GCC
// ---------------------------------------------------------------------------
#elif defined(__MINGW32__)

#define HOST_COMPILER_MINGW32
#define C99_SUPPORT
#info "Host compiler: Mingw GCC"

// ---------------------------------------------------------------------------
// Test for MPW C
// ---------------------------------------------------------------------------
#elif defined(MPW_C)

#define HOST_COMPILER_MPW
#define C99_SUPPORT_UNKNOWN
#info "Host compiler: MPW C"

// ---------------------------------------------------------------------------
// Test for MS C
// ---------------------------------------------------------------------------
#elif defined(_MSC_VER)

#define HOST_COMPILER_MSC
#define HOST_COMPILER_MICROSOFT
#define C99_SUPPORT
#info "Host compiler: Microsoft C"

// ---------------------------------------------------------------------------
// Test for Pathscale C
// ---------------------------------------------------------------------------
#elif defined(__PATHCC__)

#define HOST_COMPILER_PATHCC
#define HOST_COMPILER_PATHSCALE
#define C99_SUPPORT
#info "Host compiler: Pathscale C"

// ---------------------------------------------------------------------------
// Test for Pelles C
// ---------------------------------------------------------------------------
#elif defined(__POCC__)

#define HOST_COMPILER_POCC
#define HOST_COMPILER_PELLES
#define C99_SUPPORT
#info "Host compiler: Pelles C"

// ---------------------------------------------------------------------------
// Test for Portland C
// ---------------------------------------------------------------------------
#elif defined(__PGI)

#define HOST_COMPILER_PGCC
#define HOST_COMPILER_PGI
#define HOST_COMPILER_PORTLAND
#define C99_SUPPORT
#info "Host compiler: Portland PGI C"

// ---------------------------------------------------------------------------
// Test for Sun Studio C
// ---------------------------------------------------------------------------
#elif defined(__SUNPRO_C)

#define HOST_COMPILER_SUNC
#define HOST_COMPILER_SUN
#define C99_SUPPORT
#info "Host compiler: Sun Studio C"

// ---------------------------------------------------------------------------
// Test for Tiny C
// ---------------------------------------------------------------------------
#elif defined(__TINYC__)

#define HOST_COMPILER_TCC
#define HOST_COMPILER_TINYC
#define C99_SUPPORT
#info "Host compiler: Tiny C"

// ---------------------------------------------------------------------------
// Test for TendDRA C
// ---------------------------------------------------------------------------
#elif defined(__TenDRA__)

#define HOST_COMPILER_TENDRA
#define C99_SUPPORT_UNKNOWN
#info "Host compiler: TenDRA C"

// ---------------------------------------------------------------------------
// Test for Watcom and OpenWatcom C
// ---------------------------------------------------------------------------
#elif defined(__WATCOMC__)

#define HOST_COMPILER_WATCOM
#define C99_SUPPORT_UNKNOWN
#info "Host compiler: Watcom C"


// ---------------------------------------------------------------------------
// Test for Wind River C
// ---------------------------------------------------------------------------
#elif defined(__DCC__)

#define HOST_COMPILER_DCC
#define HOST_COMPILER_WINDRIVER
#define NO_C99_SUPPORT
#info "Host compiler: Wind River C"

// ---------------------------------------------------------------------------
// Unknown compiler
// ---------------------------------------------------------------------------
#else

#define HOST_COMPILER_UNKNOWN
#define C99_SUPPORT_UNKNOWN
#info "Host compiler: Unknown"

#endif /* compiler test */


// ===========================================================================
// Determine C99 status
// ===========================================================================

// ---------------------------------------------------------------------------
// Test whether C99 is enabled or not
// ---------------------------------------------------------------------------
#if (__STDC_VERSION__ >= 199901L)

#define C99_ENABLED

#elif /* not enabled */

#define C99_NOT_ENABLED
#define C99_DISABLED

#endif /* C99 test */


// ---------------------------------------------------------------------------
// Build environment check and info
// ---------------------------------------------------------------------------
#if defined(C99_ENABLED)

#info "C99 is enabled. Compilation will proceed."

#elif defined(C99_SUPPORT) || defined(C99_SUPPORT_UNKNOWN)

#info "C99 is disabled. Compilation will abort."
#info "You must enable C99 to compile this project."
#fatal "Compilation aborted because C99 is disabled."

#elif defined(NO_C99_SUPPORT)

#info "This compiler does not support C99. Compilation will abort."
#info "You must use a compiler with C99 support to compile this project."
#fatal "Compilation aborted because C99 is not supported."

#endif /* check and info */


#endif /* CHECK_BUILD_ENV_H */

// END OF FILE
