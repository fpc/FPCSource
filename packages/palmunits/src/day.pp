(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Day.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines the date picker month object's  structures
 *   and routines.
 *
 * History:
 *    May 31, 1995   Created by Roger Flores
 *
 *****************************************************************************)
unit day;

interface

uses palmos, coretraps, rect, datetime, control;

type
  SelectDayType = Enum;

const
  selectDayByDay = 0;   // return d/m/y
  selectDayByWeek = 1;  // return d/m/y with d as same day of the week
  selectDayByMonth = 2; // return d/m/y with d as same day of the month

type
  DaySelectorType = record
    bounds: RectangleType;
    visible: Boolean;
    reserved1: UInt8;
    visibleMonth: Int16; // month actually displayed
    visibleYear: Int16;  // year actually displayed
    selected: DateTimeType;
    selectDayBy: SelectDayType;
    reserved2: UInt8;
  end;
  DaySelectorPtr = ^DaySelectorType;

procedure DayDrawDaySelector(const selectorP: DaySelectorPtr); syscall sysTrapDayDrawDaySelector;

function DayHandleEvent(const selectorP: DaySelectorPtr; const pEvent: EventPtr): Boolean; syscall sysTrapDayHandleEvent;

procedure DayDrawDays(const selectorP: DaySelectorPtr); syscall sysTrapDayDrawDays;

implementation

end.
