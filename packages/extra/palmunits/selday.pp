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

unit selday;

interface

uses palmos, coretraps, datetime, day;

const
  daySelectorMinYear = firstYear;
  daySelectorMaxYear = lastYear;

function SelectDayV10(var month, day, year: Int16; const title: PChar): Boolean; syscall sysTrapSelectDayV10;

function SelectDay(const selectDayBy: SelectDayType; var month, day, year: Int16; const title: PChar): Boolean; syscall sysTrapSelectDay;

implementation

end.
