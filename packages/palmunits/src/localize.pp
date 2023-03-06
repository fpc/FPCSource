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

{$IFNDEF FPC_DOTTEDUNITS}
unit localize;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses PalmApi.Palmos, PalmApi.Coretraps;
{$ELSE FPC_DOTTEDUNITS}
uses palmos, coretraps;
{$ENDIF FPC_DOTTEDUNITS}

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

procedure LocGetNumberSeparators(numberFormat: NumberFormatType; thousandSeparator, decimalSeparator: PAnsiChar); syscall sysTrapLocGetNumberSeparators;

implementation

end.
