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

unit password;

interface

uses coretraps;

const
  pwdLength = 32;
  pwdEncryptionKeyLength = 64;

function PwdExists: Boolean; syscall sysTrapPwdExists;

function PwdVerify(Astring: PChar): Boolean; syscall sysTrapPwdVerify;

procedure PwdSet(oldPassword, newPassword: PChar); syscall sysTrapPwdSet;

procedure PwdRemove; syscall sysTrapPwdRemove;

implementation

end.
