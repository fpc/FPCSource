(******************************************************************************
 *
 * Copyright (c) 1996-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: ScrollBar.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines scroll bar structures and routines.
 *
 * History:
 *    Feb 6, 1996 Created by Art Lamb
 *
 *****************************************************************************)

unit scrollbar;

interface

uses palmos, coretraps, control;

type
  ScrollBarRegionType = Enum;

const
  sclUpArrow = 0;
  sclDownArrow = Succ(sclUpArrow);
  sclUpPage = Succ(sclDownArrow);
  sclDownPage = Succ(sclUpPage);
  sclCar = Succ(sclDownPage);

type
  ScrollBarAttrType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_SCROLLBARS} // These fields will not be available in the next OS release!
    Bits: UInt16;
{
    UInt16 usable       :1; // Set if part of ui
    UInt16 visible      :1; // Set if drawn, used internally
    UInt16 hilighted    :1; // Set if region is hilighted
    UInt16 shown        :1; // Set if drawn and maxValue > minValue
    UInt16 activeRegion :4; // ScrollBarRegionType
    UInt16 reserved     :8; // Reserved for future use
}
  {$endif}
  end;
  ScrollBarAttrTag = ScrollBarAttrType;

  ScrollBarType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_SCROLLBARS} // These fields will not be available in the next OS release!
    bounds: RectangleType;
    id: UInt16;
    attr: ScrollBarAttrType;
    value: Int16;
    minValue: Int16;
    maxValue: Int16;
    pageSize: Int16;
    penPosInCar: Int16;
    savePos: Int16;
  {$endif}
  end;

  ScrollBarPtr = ^ScrollBarType;

procedure SclGetScrollBar(const bar: ScrollBarPtr; var valueP, minP, maxP, pageSizeP: Int16); syscall sysTrapSclGetScrollBar;

procedure SclSetScrollBar(bar: ScrollBarPtr; value: Int16; const min, max, pageSize: Int16); syscall sysTrapSclSetScrollBar;

procedure SclDrawScrollBar(bar: ScrollBarPtr); syscall sysTrapSclDrawScrollBar;

function SclHandleEvent(bar: ScrollBarPtr; const event: EventPtr): Boolean; syscall sysTrapSclHandleEvent;

implementation

end.
