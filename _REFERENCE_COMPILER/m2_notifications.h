/*! Modula-2 R10 Compiler (m2r10c)
 *
 *  @file  m2_notifications.h
 *  @brief Notification handling interface
 *
 *  @b Author: Benjamin Kowarsch
 *
 *  @b Copyright: (C) 2010 B.Kowarsch. All rights reserved.
 *
 *  @b License:
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


// FROM common IMPORT cardinal;
#include "common.h"


// --------------------------------------------------------------------------
// Notification codes
// --------------------------------------------------------------------------

typedef /* m2_notification_t */ enum {
    M2_NOTIFICATION_ALLOCATION_FAILED,
    M2_NOTIFICATION_SYNTAX_ERROR
    
    // TO DO: add notifications
    
} m2_notification_t;


// --------------------------------------------------------------------------
// Notification handler type
// --------------------------------------------------------------------------

typedef void (*m2_notification_f)(m2_notification_t, const char*, cardinal);


#endif /* M2_NOTIFICATIONS_H */

// END OF FILE