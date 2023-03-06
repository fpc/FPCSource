{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Password.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Password include file
 *
 * History:
 *    4/1/95 - created by Roger Flores
 *
 *****************************************************************************)

{$IFNDEF FPC_DOTTEDUNITS}
unit password;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses PalmApi.Coretraps;
{$ELSE FPC_DOTTEDUNITS}
uses coretraps;
{$ENDIF FPC_DOTTEDUNITS}

const
  pwdLength = 32;
  pwdEncryptionKeyLength = 64;

function PwdExists: Boolean; syscall sysTrapPwdExists;

function PwdVerify(Astring: PAnsiChar): Boolean; syscall sysTrapPwdVerify;

procedure PwdSet(oldPassword, newPassword: PAnsiChar); syscall sysTrapPwdSet;

procedure PwdRemove; syscall sysTrapPwdRemove;

implementation

end.
