{$MACRO ON}

{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: TelephonyMgrUI.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *       This is the header
 *
 * History:
 *       May 23, 2000      ARO   Initial Release
 *       May,30, 2000      PPL   Add Pin code notification
 *       Nov 11, 2000      PPL   Remove unused staff
 *       Dec 03, 2000      LFe add flags
 *
 *****************************************************************************)

unit telephonymgrui;

interface

uses palmos;

(***********************************************************************
 * Structure definition - to move to a public header file
 ***********************************************************************)

const
  kTelNotifyErrorDetailsVersion = 1;

  telNotifyErrorEvent     = Rsc('terr');
  telNotifyEnterCodeEvent = Rsc('tpin');

  kTelTryAgainBit         = $00000001;
  kTelNoSetUpButtonBit    = $00000002;
  kTelAutoTryAgainBit     = $00000004;
  kTelAutoSetUpButtonBit  = $00000008;

type
  TelNotifyErrorDetailsType = record
    version: UInt16;
    error: Err;
    ioFlags: UInt32;
    messageP: PChar;
  end;

  TelNotifyErrorDetailsTag = TelNotifyErrorDetailsType;
  TelNotifyErrorDetailsPtr = ^TelNotifyErrorDetailsType;

implementation

end.
