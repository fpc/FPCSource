(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: List.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines list structures and routines.
 *
 * History:
 *    November 3, 1994  Created by Roger Flores
 *       Name  Date     Description
 *       ----  ----     -----------
 *       bob   2/9/99   fixed const stuff
 *
 *****************************************************************************)

unit list;

interface

uses palmos, coretraps, rect, font, window, control;

const
  noListSelection = -1;

//-------------------------------------------------------------------
// List structures
//-------------------------------------------------------------------

type
  ListAttrType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_LISTS} // These fields will not be available in the next OS release!
    Bits: UInt16;
{
    UInt16 usable      :1; // set if part of ui
    UInt16 enabled     :1; // set if interactable (not grayed out)
    UInt16 visible     :1; // set if drawn
    UInt16 poppedUp    :1; // set if choices displayed in popup win.
    UInt16 hasScrollBar:1; // set if the list has a scroll bar
    UInt16 search      :1; // set if incremental search is enabled
    UInt16 reserved    :2;
}
  {$endif}
  end;
  ListAttrTag = ^ListAttrType;

// Load data callback routine prototype
type
  ListDrawDataFuncType = procedure (itemNum: Int16; bounds: RectanglePtr; var itemsText: PChar);
  ListDrawDataFuncPtr = ListDrawDataFuncType;

type
  ListType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_LISTS} // These fields will not be available in the next OS release!
    id: UInt16;
    bounds: RectangleType;
    attr: ListAttrType;
    itemsText: ^PChar;
    numItems: Int16;                        // number of choices in the list
    currentItem: Int16;                     // currently display choice
    topItem: Int16;                         // top item visible when poped up
    font: FontID;                           // font used to draw list
    reserved: UInt8;
    popupWin: WinHandle;                    // used only by popup lists
    drawItemsCallback: ListDrawDataFuncPtr; // 0 indicates no function
  {$endif}
  end;
  ListPtr = ^ListType;

//-------------------------------------------------------------------
// List routines
//-------------------------------------------------------------------

procedure LstDrawList(listP: ListPtr); syscall sysTrapLstDrawList;

procedure LstEraseList(listP: ListPtr); syscall sysTrapLstEraseList;

function LstGetSelection(const listP: ListPtr): Int16; syscall sysTrapLstGetSelection;

function LstGetSelectionText(const listP: ListPtr; itemNum: Int16): PChar; syscall sysTrapLstGetSelectionText;

function LstHandleEvent(listP: ListPtr; const eventP: EventPtr): Boolean; syscall sysTrapLstHandleEvent;

procedure LstSetHeight(listP: ListPtr; visibleItems: Int16); syscall sysTrapLstSetHeight;

procedure LstSetPosition(listP: ListPtr; x, y: Coord); syscall sysTrapLstSetPosition;

procedure LstSetSelection(listP: ListPtr; itemNum: Int16); syscall sysTrapLstSetSelection;

procedure LstSetListChoices(listP: ListPtr; var itemsText: PChar; numItems: Int16); syscall sysTrapLstSetListChoices;

procedure LstSetDrawFunction(listP: ListPtr; func: ListDrawDataFuncPtr); syscall sysTrapLstSetDrawFunction;

procedure LstSetTopItem(listP: ListPtr; itemNum: Int16); syscall sysTrapLstSetTopItem;

procedure LstMakeItemVisible(listP: ListPtr; itemNum: Int16); syscall sysTrapLstMakeItemVisible;

function LstGetNumberOfItems(const listP: ListPtr): Int16; syscall sysTrapLstGetNumberOfItems;

function LstPopupList(listP: ListPtr): Int16; syscall sysTrapLstPopupList;

function LstScrollList(listP: ListPtr; direction: WinDirectionType; itemCount: Int16): Boolean; syscall sysTrapLstScrollList;

function LstGetVisibleItems(const listP: ListPtr): Int16; syscall sysTrapLstGetVisibleItems;

function LstNewList(formPP: PointerPtr; id: UInt16; x, y, width, height: Coord;
                    font: FontID; visibleItems, triggerId: Int16): Err; syscall sysTrapLstNewList;

function LstGetTopItem(const listP: ListPtr): Int16; syscall sysTrapLstGetTopItem;

implementation

end.
