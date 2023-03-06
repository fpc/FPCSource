{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SelDay.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines the date picker month object's  structures
 *   and routines.
 *
 * History:
 *    November 10, 1994 Created by Roger Flores
 *
 *****************************************************************************)

{$IFNDEF FPC_DOTTEDUNITS}
unit selday;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses PalmApi.Palmos, PalmApi.Coretraps, PalmApi.Datetime, PalmApi.Day;
{$ELSE FPC_DOTTEDUNITS}
uses palmos, coretraps, datetime, day;
{$ENDIF FPC_DOTTEDUNITS}

const
  daySelectorMinYear = firstYear;
  daySelectorMaxYear = lastYear;

function SelectDayV10(var month, day, year: Int16; const title: PAnsiChar): Boolean; syscall sysTrapSelectDayV10;

function SelectDay(const selectDayBy: SelectDayType; var month, day, year: Int16; const title: PAnsiChar): Boolean; syscall sysTrapSelectDay;

implementation

end.
