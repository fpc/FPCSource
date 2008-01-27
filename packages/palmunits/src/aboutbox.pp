{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: AboutBox.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines About Box routines
 *
 * History:
 *    October 25th, 1995   Created by Christopher Raff
 *
 *****************************************************************************)

unit aboutbox;

interface

uses  palmos,coretraps;

// WARNING!!! This routine is for the private use of Palm applications.
// It is released with the public headers so that the sample apps
// released with the SDK can be compiled by developers.

procedure AbtShowAbout(creator: UInt32); syscall sysTrapAbtShowAbout;

implementation

end.
