{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: PhoneLookup.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines phone number lookup structures and routines.
 *
 * History:
 *    July 23, 1996  Created by Art Lamb
 *    March 24, 2000 Ludovic Ferrandis: Add custom API
 *
 *****************************************************************************)

unit phonelookup;

interface

uses coretraps, field, applaunchcmd;

procedure PhoneNumberLookup(var fldP: FieldType); syscall sysTrapPhoneNumberLookup;

procedure PhoneNumberLookupCustom(var fldP: FieldType; params: AddrLookupParamsPtr; useClipboard: Boolean); syscall sysTrapPhoneNumberLookupCustom;

implementation

end.
