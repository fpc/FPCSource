(******************************************************************************
 *
 * Copyright (c) 1996-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Localize.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Functions to localize data.
 *
 * History:
 *    8/28/96  Roger - Initial version
 *
 *****************************************************************************)

unit localize;

interface

uses palmos, coretraps;

// The number format (thousands separator and decimal point).  This defines
// how numbers are formatted and not neccessarily currency numbers (i.e. Switzerland).
type
  NumberFormatType = Enum;

const
  nfCommaPeriod = 0;
  nfPeriodComma = Succ(nfCommaPeriod);
  nfSpaceComma = Succ(nfPeriodComma);
  nfApostrophePeriod = Succ(nfSpaceComma);
  nfApostropheComma = Succ(nfApostrophePeriod);

procedure LocGetNumberSeparators(numberFormat: NumberFormatType; thousandSeparator, decimalSeparator: PChar); syscall sysTrapLocGetNumberSeparators;

implementation

end.
