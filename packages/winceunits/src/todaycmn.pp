{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007-2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
//*****************************************************************************/
//*                                                                           */
//*  TodayCmn.h                                                               */
//*                                                                           */
//*  Today screen common header file                                          */
//*                                                                           */
//*****************************************************************************/

//
// Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit TodayCmn;

interface

uses Windows;

// list item types
type
     _TODAYLISTITEMTYPE = (tlitOwnerInfo := 0,  // name, company
                           tlitAppointments,   // today's appointments, events
                           tlitMail,           // message information
                           tlitTasks,          // overdue and upcoming tasks
                           tlitCustom,         // other
                           tlitReserved1,      // reserved value
                           tlitReserved2,      // reserved value
                           tlitNil);           // sentinel

     TODAYLISTITEMTYPE = _TODAYLISTITEMTYPE;


const
      MAX_ITEMNAME    = 32;

type
     TODAY_SELECTABILITY = (TODAY_SELECTABILITY_NOTSELECTABLE := 0,
                            TODAY_SELECTABILITY_SELECTABLE,
                            TODAY_SELECTABILITY_CUSTOMSELECTION,
                            TODAY_SELECTABILITY_LAST);

// information for a single today item
type
     _TODAYLISTITEM = record
       szName:array[0..MAX_ITEMNAME-1] of WideChar;
       tlit:TODAYLISTITEMTYPE;
       dwOrder:DWORD;
       cyp:DWORD;
       fEnabled:BOOL;
       fOptions:BOOL;
       grfFlags:DWORD;
       szDLLPath:array[0..MAX_PATH-1] of WideChar;
       hinstDLL:HINST;
       hwndCustom:HWND;
       fSizeOnDraw:BOOL;
       prgbCachedData:LPBYTE;
       cbCachedData:DWORD;
       dwSelectability:DWORD;
     end;
     TODAYLISTITEM = _TODAYLISTITEM;
     LPTODAYLISTITEM = ^_TODAYLISTITEM;

const
      TODAYM_GETCOLOR            = WM_USER + 100; // wParam == TODAYCOLOR_*
      TODAYCOLOR_TEXT            = $10000004;
      TODAYCOLOR_HIGHLIGHT       = $10000022;
      TODAYCOLOR_HIGHLIGHTEDTEXT = $10000023;


const
      TODAYM_DRAWWATERMARK       = WM_USER + 101; // lParam == TODAYDRAWWATERMARKINFO*

type
     TODAYDRAWWATERMARKINFO = record
       _hdc:HDC;
       rc:RECT;
       _hwnd:HWND;
     end;
     LPTODAYDRAWWATERMARKINFO = ^TODAYDRAWWATERMARKINFO;


const
      TODAYM_TOOKSELECTION       = WM_USER + 102;


const
      TODAYM_REQUESTREFRESH      = WM_USER + 103;
      TODAYREFRESH_NOHEIGHTCHANGE = $00000001;


// the following block of message id #'s is reserved for internal use
const
      TODAYM_RESERVEDSTART      = WM_USER + 300;
      TODAYM_RESERVEDEND        = WM_USER + 399;

      
// maximum number of today items
const
      k_cTodayItemsMax          = 30;


// custom DLL resources
const
      IDI_ICON                  = 128;
      IDD_TODAY_CUSTOM          = 500;


// custom DLL functions
const
      ORDINAL_INITIALIZEITEM    = 240;

type
     PFNCUSTOMINITIALIZEITEM = function(ptli:LPTODAYLISTITEM; hwndParent:HWND):HWND; cdecl;
     TInitializeCustomItemProc = PFNCUSTOMINITIALIZEITEM;

const
      ORDINAL_OPTIONSDIALOGPROC = 241;

type
     PFNCUSTOMOPTIONSDLGPROC = function(_hDlg:HWND; _message:UINT; wParam:UINT; lParam:LONG):BOOL; cdecl;
     TCustomItemOptionsDlgProc = PFNCUSTOMOPTIONSDLGPROC;


// custom DLL messages
const
      WM_TODAYCUSTOM_CLEARCACHE           = WM_USER+242;
      WM_TODAYCUSTOM_QUERYREFRESHCACHE    = WM_USER+243;
      WM_TODAYCUSTOM_RECEIVEDSELECTION    = WM_USER+244;
      WM_TODAYCUSTOM_LOSTSELECTION        = WM_USER+245;
      WM_TODAYCUSTOM_USERNAVIGATION       = WM_USER+246;
      WM_TODAYCUSTOM_ACTION               = WM_USER+247;
      WM_TODAYCUSTOM_CLOSEDATABASES       = WM_USER+248;

implementation

end.