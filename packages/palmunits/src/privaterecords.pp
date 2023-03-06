{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1996-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: PrivateRecords.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This header file defines a generic private record maintainance dialogs, etc.
 *
 * History:
 *    6/23/99. Created by Craig Skinner
 *
 *****************************************************************************)

{$IFNDEF FPC_DOTTEDUNITS}
unit privaterecords;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses PalmApi.Palmos, PalmApi.Coretraps;
{$ELSE FPC_DOTTEDUNITS}
uses palmos, coretraps;
{$ENDIF FPC_DOTTEDUNITS}

// Defines needed for hidden record visual determination.
type
  privateRecordViewEnum = Enum;

const
  showPrivateRecords = $00;
  maskPrivateRecords = Succ(showPrivateRecords);
  hidePrivateRecords = Succ(maskPrivateRecords);

//-----------------------------------------------------------------------
// Prototypes
//-----------------------------------------------------------------------

function SecSelectViewStatus: privateRecordViewEnum; syscall sysTrapSecSelectViewStatus;

function SecVerifyPW(newSecLevel: privateRecordViewEnum): Boolean; syscall sysTrapSecVerifyPW;

implementation

end.
