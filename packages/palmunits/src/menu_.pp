(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Menu.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines menu structures and routines.
 *
 * History:
 *    November 18, 1994 Created by Roger Flores
 *       Name  Date     Description
 *       ----  ----     -----------
 *       gap   09/29/99 Added gsiWasEnabled to MenuCmdBarType
 *
 *****************************************************************************)

unit menu_;

interface

uses palmos, coretraps, errorbase, control, event_;

// Errors returned by Menu routines

const
  menuErrNoMenu       = menuErrorClass or 1;
  menuErrNotFound     = menuErrorClass or 2;
  menuErrSameId       = menuErrorClass or 3;
  menuErrTooManyItems = menuErrorClass or 4;
  menuErrOutOfMemory  = menuErrorClass or 5;

// Command bar structures

type
  MenuCmdBarResultType = Enum;

const
  menuCmdBarResultNone = 0;                                // send nothing (this'd be quite unusual but is allowed)
  menuCmdBarResultChar = Succ(menuCmdBarResultNone);       // char to send (with commandKeyMask bit set)
  menuCmdBarResultMenuItem = Succ(menuCmdBarResultChar);   // id of the menu item
  menuCmdBarResultNotify = Succ(menuCmdBarResultMenuItem); // Nofication Manager notification type

// maximum length of the prompt string to display in the command bar
  menuCmdBarMaxTextLength = 20;

type
  MenuCmdBarButtonType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_MENUS} // These fields will not be available in the next OS release!
    bitmapId: UInt16;
    name: array [0..menuCmdBarMaxTextLength-1] of Char;
    resultType: MenuCmdBarResultType;
    reserved: UInt8; // alignment padding
    result: UInt32;
  {$endif}
  end;
  MenuCmdBarButtonTag = MenuCmdBarButtonType;

  MenuCmdBarType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_MENUS} // These fields will not be available in the next OS release!
    bitsBehind: WinHandle;
    timeoutTick: Int32; // tick to disappear on
    top: Coord;
    numButtons: Int16;
    insPtWasEnabled: Boolean;
    gsiWasEnabled: Boolean;
    feedbackMode: Boolean; // set when just displaying confirmation feedback
    buttonsData: ^MenuCmdBarButtonType;
  {$endif}
  end;

// to tell MenuCmdBarAddButton where to add the button: on right or left.
const
  menuCmdBarOnRight = 0;
  menuCmdBarOnLeft  = $ff;

////Menu-specific

  noMenuSelection = -1;
  noMenuItemSelection = -1;
  separatorItemSelection = -2;

// cause codes for menuOpen Event
  menuButtonCause  = 0;
  menuCommandCause = 1;

// To match Apple's ResEdit the first byte of a menu item's text can
// be a special char indicating a special menu item.
  MenuSeparatorChar = '-';

type
  MenuItemType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_MENUS} // These fields will not be available in the next OS release!
    id: UInt16;    // id of the menu item
    command: Char;   // command key
    bits: UInt8;
{
    UInt8      hidden: 1;  // true if menu item is hidden
    UInt8      reserved: 7;
}
    itemStr: PChar;   // string to be displayed
  {$endif}
  end;
  MenuItemTag = MenuItemType;

  MenuPullDownType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_MENUS} // These fields will not be available in the next OS release!
    menuWin: WinHandle; // window of pull-down menu
    bounds: RectangleType; // bounds of the pulldown
    bitsBehind: WinHandle; // saving bits behind pull-down menu
    titleBounds: RectangleType; // bounds of the title in menu bar
    title: PChar; // menu title displayed in menu bar
    bits: UInt16;
{
    UInt16     hidden: 1;  // true if pulldown is hidden
    UInt16     numItems: 15; // number of items in the menu
}
    items: ^MenuItemType; // array of menu items
  {$endif}
  end;
  MenuPullDownTag = MenuPullDownType;
  MenuPullDownPtr = ^MenuPullDownType;

  MenuBarAttrType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_MENUS} // These fields will not be available in the next OS release!
    bits: UInt16;
{
    UInt16 visible   :1;   // Set if menu bar is drawn
    UInt16 commandPending :1;   // Set if next key is a command
    UInt16 insPtEnabled :1;   // Set if insPt was on when menu was drawn
    UInt16 needsRecalc :1;    // if set then recalc menu dimensions
    UInt16 attnIndicatorIsAllowed :1;   // set if attn indicator was allowed when menu was drawn
    UInt16 reserved         :11;        // reserved for future use
}
  {$endif}
  end;
  MenuBarAttrTag = MenuBarAttrType;

  MenuBarType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_MENUS} // These fields will not be available in the next OS release!
    barWin: WinHandle; // window of menu bar
    bitsBehind: WinHandle; // saving bits behind menu bar
    savedActiveWin: WinHandle;
    bitsBehindStatus: WinHandle;
    attr: MenuBarAttrType;
    curMenu: Int16; // current menu or -1 if none
    curItem: Int16; // current item in curMenu, -1 if none
    commandTick: Int32;
    numMenus: Int16; // number of menus
    menus: MenuPullDownPtr; // array of menus
  {$endif}
  end;
  MenuBarTag = MenuBarType;
  MenuBarPtr = ^MenuBarType;

function MenuInit(resourceId: UInt16): MenuBarPtr; syscall sysTrapMenuInit;

function MenuGetActiveMenu: MenuBarPtr; syscall sysTrapMenuGetActiveMenu;

function MenuSetActiveMenu(menuP: MenuBarPtr): MenuBarPtr; syscall sysTrapMenuSetActiveMenu;

procedure MenuDispose(menuP: MenuBarPtr); syscall sysTrapMenuDispose;

function MenuHandleEvent(menuP: MenuBarPtr; var event: EventType; var error: UInt16): Boolean; syscall sysTrapMenuHandleEvent;

procedure MenuDrawMenu(menuP: MenuBarPtr); syscall sysTrapMenuDrawMenu;

procedure MenuEraseStatus(menuP: MenuBarPtr); syscall sysTrapMenuEraseStatus;

procedure MenuSetActiveMenuRscID(resourceId: UInt16); syscall sysTrapMenuSetActiveMenuRscID;

function MenuCmdBarAddButton(where: UInt8; bitmapId: UInt16; resultType: MenuCmdBarResultType;
                             result_: UInt32; nameP: PChar): Err; syscall sysTrapMenuCmdBarAddButton;

function MenuCmdBarGetButtonData(buttonIndex: Int16; var bitmapIdP: UInt16; var resultTypeP: MenuCmdBarResultType;
                                 var resultP: UInt32; nameP: PChar): Boolean; syscall sysTrapMenuCmdBarGetButtonData;

procedure MenuCmdBarDisplay; syscall sysTrapMenuCmdBarDisplay;

function MenuShowItem(id: UInt16): Boolean; syscall sysTrapMenuShowItem;

function MenuHideItem(id: UInt16): Boolean; syscall sysTrapMenuHideItem;

function MenuAddItem(positionId, id: UInt16; cmd: Char; const textP: PChar): Err; syscall sysTrapMenuAddItem;

implementation

end.
