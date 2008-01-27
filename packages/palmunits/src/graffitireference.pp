{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1996-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: GraffitiReference.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines the Graffiti Reference routines.
 *
 * History:
 *    June 25, 1996  Created by Roger Flores
 *    06/25/96 rsf   Created by Roger Flores
 *    07/30/99 kwk   Moved all reference types other than referenceDefault
 *                   into GraffitiReference.c
 *
 *****************************************************************************)

unit graffitireference;

interface

uses palmos, coretraps;

type
  ReferenceType = Enum;

const
  referenceDefault = $ff; // based on graffiti mode

(************************************************************
 * Graffiti Reference procedures
 *************************************************************)

procedure SysGraffitiReferenceDialog(referenceType: ReferenceType); syscall sysTrapSysGraffitiReferenceDialog;

implementation

end.
