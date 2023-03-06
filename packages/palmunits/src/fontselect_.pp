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

{$IFNDEF FPC_DOTTEDUNITS}
unit fontselect_;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses PalmApi.Coretraps, PalmApi.Font;
{$ELSE FPC_DOTTEDUNITS}
uses coretraps, font;
{$ENDIF FPC_DOTTEDUNITS}

function FontSelect(fontID: FontID): FontID; syscall sysTrapFontSelect;

implementation

end.
