{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1997-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: FontSelect.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines the font selector routine.
 *
 * History:
 *    September 10, 1997   Created by Art Lamb
 *
 *****************************************************************************)

unit fontselect_;

interface

uses coretraps, font;

function FontSelect(fontID: FontID): FontID; syscall sysTrapFontSelect;

implementation

end.
