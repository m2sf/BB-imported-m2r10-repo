/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_notifications.h
 *  Notification handling interface
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

#ifndef M2_NOTIFICATIONS_H
#define M2_NOTIFICATIONS_H


// FROM common IMPORT opaque_t, cardinal;
#include "common.h"

// FROM m2_filenames IMPORT m2_filename_t;
#include "m2_filenames.h"

// FROM m2_fileio IMPORT m2_file_t;
#include "m2_fileio.h"


// ---------------------------------------------------------------------------
// Notification codes
// ---------------------------------------------------------------------------

typedef /* m2_notification_t */ enum {
    M2_NOTIFY_ALLOCATION_FAILED,
    M2_NOTIFY_ILLEGAL_CHAR_IN_STRING_LITERAL,
    M2_NOTIFY_SYNTAX_ERROR
    
    // TO DO: add notifications
    
} m2_notification_t;


// ---------------------------------------------------------------------------
// Notification sources
// ---------------------------------------------------------------------------

typedef /* m2_notifier_t */ enum {
    M2_NOTIFIER_FILEIO,
    M2_NOTIFIER_LEXER,
    M2_NOTIFIER_PARSER,
    M2_NOTIFIER_CODEGEN
} m2_notifier_t;


// ---------------------------------------------------------------------------
// Notification handler type
// ---------------------------------------------------------------------------
//
// Notification handlers must match the signature of notification handler type
// <m2_notification_f>.  Notification handler parameters are as follows:
//
// o  notification code
// o  filename to which the notification relates
// o  file position to which the notification relates
// o  notifying component that is sending the notification
// o  arbitrary detail information provided by notifying component

typedef void (*m2_notification_f)(m2_notification_t,
                                  m2_filename_t,
                                  m2_file_pos_t,
                                  m2_notifier_t,
                                  opaque_t);


#endif /* M2_NOTIFICATIONS_H */

// END OF FILE