{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{  Declarations for commctrl WinCE API

}

{exported functions list = to do,
 * please remove functions done *
 
     Exports

       ordinal    name

            73    AddMRUData
            66    AddMRUStringA
            67    AddMRUStringW
            35    CenterWindow
            39    CommandBands_AddAdornments
            37    CommandBands_AddBands
            36    CommandBands_Create
            38    CommandBands_GetCommandBar
            41    CommandBands_GetRestoreInformation
            40    CommandBands_Show
            10    CommandBar_AddAdornments
             5    CommandBar_AddBitmap
             3    CommandBar_Create
            43    CommandBar_DrawMenuBar
            11    CommandBar_GetItemWindow
             9    CommandBar_GetMenu
            12    CommandBar_Height
             6    CommandBar_InsertComboBox
             7    CommandBar_InsertControl
             8    CommandBar_InsertMenubar
            42    CommandBar_InsertMenubarEx
             4    CommandBar_Show
            63    CreateMRUListA
            64    CreateMRUListW
            15    CreateToolbar
            50    DPA_Create
            61    DPA_DeleteAllPtrs
            60    DPA_DeletePtr
            31    DPA_Destroy
            56    DPA_DestroyCallback
            32    DPA_GetPtr
            62    DPA_GetPtrIndex
            54    DPA_Grow
            51    DPA_InsertPtr
            59    DPA_Search
            53    DPA_SetPtr
            52    DPA_Sort
            46    DSA_Clone
            23    DSA_Create
            30    DSA_DeleteAllItems
            29    DSA_DeleteItem
            24    DSA_Destroy
            47    DSA_DestroyCallback
            48    DSA_EnumCallback
            25    DSA_GetItem
            26    DSA_GetItemPtr
            27    DSA_InsertItem
            45    DSA_Search
            28    DSA_SetItem
            58    DSA_SetRange
            57    DSA_Sort
            68    DelMRUString
            49    DoReaderMode
            71    EnumMRUListA
            72    EnumMRUListW
            74    FindMRUData
            69    FindMRUStringA
            70    FindMRUStringW
            65    FreeMRUList
            22    InvertRect
            55    IsCapEditAvailable
            13    IsCommandBarMessage
                  ListView_SetItemSpacing
            75    PopulateComboWithMRU
            76    PopulateMenuWithMRU
            80    SHCreateDefaultGradient
            79    SHDrawGradient
            81    SHDrawText
            77    SHGetSysColor
            78    SHGetSystemMetrics
            82    SHSetSysColors
            83    SHSetSystemMetrics
            34    StrToIntW
            33    Str_SetPtrW
}

{$ifdef read_interface}

//*****************************************************************************
// consts
//*****************************************************************************
const
  ComctlDLL     = 'commctrl';

  COMCTL32_VERSION=$020c;

  // DATETIMEPICK CONTROL
  DATETIMEPICK_CLASS  ='SysDateTimePick32';
  DTM_FIRST           = $1000;
  DTM_GETSYSTEMTIME   = DTM_FIRST + 1;
  DTM_SETSYSTEMTIME   = DTM_FIRST + 2;
  DTM_GETRANGE        = DTM_FIRST + 3;
  DTM_SETRANGE        = DTM_FIRST + 4;
  DTM_SETFORMATA      = DTM_FIRST + 5;
  DTM_SETMCCOLOR      = DTM_FIRST + 6;
  DTM_GETMCCOLOR      = DTM_FIRST + 7;
  DTM_GETMONTHCAL     = DTM_FIRST + 8;
  DTM_SETMCFONT       = DTM_FIRST + 9;
  DTM_GETMCFONT       = DTM_FIRST + 10;
  DTM_SETFORMATW      = DTM_FIRST + 50;


  DTS_UPDOWN                 = $0001;
  DTS_SHOWNONE               = $0002;
  DTS_SHORTDATEFORMAT        = $0000;
  DTS_LONGDATEFORMAT         = $0004;
  DTS_SHORTDATECENTURYFORMAT = $000C;
  DTS_TIMEFORMAT             = $0009;
  DTS_APPCANPARSE            = $0010;
  DTS_RIGHTALIGN             = $0020;
  DTS_NONEBUTTON             = $0080;

  DTN_FIRST                  =-760;
  DTN_DATETIMECHANGE  = DTN_FIRST + 1;
  DTN_USERSTRINGA     = DTN_FIRST + 2;
  DTN_WMKEYDOWNA      = DTN_FIRST + 3;
  DTN_FORMATA         = DTN_FIRST + 4;
  DTN_FORMATQUERYA    = DTN_FIRST + 5;
  DTN_DROPDOWN        = DTN_FIRST + 6;
  DTN_CLOSEUP         = DTN_FIRST + 7;
  DTN_USERSTRINGW     = DTN_FIRST + 15;
  DTN_WMKEYDOWNW      = DTN_FIRST + 16;
  DTN_FORMATW         = DTN_FIRST + 17;
  DTN_FORMATQUERYW    = DTN_FIRST + 18;

  GDTR_MIN            = $0001;
  GDTR_MAX            = $0002;

  GDT_ERROR           = -1;
  GDT_VALID           = 0;
  GDT_NONE            = 1;

  // Pocket PC  special controls
  WC_CAPEDIT    = 'CAPEDIT';
  WC_TSTATIC    = 'TTSTATIC';
  WC_TBUTTON    = 'TTBUTTON';

  // for FE, single byte character edit
  WC_SBEDIT     = 'sbedit';


//*****************************************************************************
// types
//*****************************************************************************

type
  // DATETIMEPICK CONTROL



  tagNMDATETIMECHANGE = record
    nmhdr   : NMHDR;
    dwFlags : DWORD;
    st      : SYSTEMTIME;
  end;
  NMDATETIMECHANGE=tagNMDATETIMECHANGE;
  TNMDATETIMECHANGE=tagNMDATETIMECHANGE;
  LPNMDATETIMECHANGE=^tagNMDATETIMECHANGE;

  tagNMDATETIMESTRINGA = record
    nmhdr         : NMHDR;
    pszUserString : LPCSTR;
    st            : SYSTEMTIME;
    dwFlags       : DWORD;
  end;
  NMDATETIMESTRINGA=tagNMDATETIMESTRINGA;
  TNMDATETIMESTRINGA=tagNMDATETIMESTRINGA;
  LPNMDATETIMESTRINGA=^tagNMDATETIMESTRINGA;

  tagNMDATETIMESTRINGW = record
    nmhdr         : NMHDR;
    pszUserString : LPCWSTR;
    st            : SYSTEMTIME;
    dwFlags       : DWORD;
  end;
  NMDATETIMESTRINGW=tagNMDATETIMESTRINGW;
  TNMDATETIMESTRINGW=tagNMDATETIMESTRINGW;
  LPNMDATETIMESTRINGW=^tagNMDATETIMESTRINGW;


  tagNMDATETIMEWMKEYDOWNA = record
    nmhdr     : NMHDR;
    nVirtKey  : integer;
    pszFormat : LPCSTR;
    st        : SYSTEMTIME;
  end;
  NMDATETIMEWMKEYDOWNA=tagNMDATETIMEWMKEYDOWNA;
  TNMDATETIMEWMKEYDOWNA=tagNMDATETIMEWMKEYDOWNA;
  LPNMDATETIMEWMKEYDOWNA=^tagNMDATETIMEWMKEYDOWNA;

  tagNMDATETIMEWMKEYDOWNW = record
    nmhdr     : NMHDR;
    nVirtKey  : integer;
    pszFormat : LPCWSTR;
    st        : SYSTEMTIME;
  end;
  NMDATETIMEWMKEYDOWNW=tagNMDATETIMEWMKEYDOWNW;
  TNMDATETIMEWMKEYDOWNW=tagNMDATETIMEWMKEYDOWNW;
  LPNMDATETIMEWMKEYDOWNW=^tagNMDATETIMEWMKEYDOWNW;


  tagNMDATETIMEFORMATA = record
    nmhdr      : NMHDR;
    pszFormat  : LPCSTR;
    st         : SYSTEMTIME;
    pszDisplay : LPCSTR;
    szDisplay  : Array[0..63] of CHAR;
  end;
  NMDATETIMEFORMATA=tagNMDATETIMEFORMATA;
  TNMDATETIMEFORMATA=tagNMDATETIMEFORMATA;
  LPNMDATETIMEFORMATA=^NMDATETIMEFORMATA;

  tagNMDATETIMEFORMATW = record
    nmhdr      : NMHDR;
    pszFormat  : LPCWSTR;
    st         : SYSTEMTIME;
    pszDisplay : LPCWSTR;
    szDisplay  : Array[0..63] of WCHAR;
  end;
  NMDATETIMEFORMATW=tagNMDATETIMEFORMATW;
  TNMDATETIMEFORMATW=tagNMDATETIMEFORMATW;
  LPNMDATETIMEFORMATW=^NMDATETIMEFORMATW;


  tagNMDATETIMEFORMATQUERYA = record
    nmhdr      : NMHDR;
    pszFormat  : LPCSTR;
    szMax      : SIZE;
  end;
  NMDATETIMEFORMATQUERYA=tagNMDATETIMEFORMATQUERYA;
  TNMDATETIMEFORMATQUERYA=tagNMDATETIMEFORMATQUERYA;
  LPNMDATETIMEFORMATQUERYA=^tagNMDATETIMEFORMATQUERYA;

  tagNMDATETIMEFORMATQUERYW = record
    nmhdr      : NMHDR;
    pszFormat  : LPCWSTR;
    szMax      : SIZE;
  end;
  NMDATETIMEFORMATQUERYW=tagNMDATETIMEFORMATQUERYW;
  TNMDATETIMEFORMATQUERYW=tagNMDATETIMEFORMATQUERYW;
  LPNMDATETIMEFORMATQUERYW=^tagNMDATETIMEFORMATQUERYW;


//*****************************************************************************
// functions
//*****************************************************************************

function CreatePropertySheetPage(lppsp:LPCPROPSHEETPAGE):HPROPSHEETPAGE; external ComctlDLL name 'CreatePropertySheetPageW';
function CreatePropertySheetPageW(lppsp:LPCPROPSHEETPAGE):HPROPSHEETPAGE; external ComctlDLL name 'CreatePropertySheetPageW';
function CreateStatusWindow(style:LONG; lpszText:LPCWSTR; hwndParent:HWND; wID:UINT):HWND; external ComctlDll name 'CreateStatusWindowW';
function CreateStatusWindowW(style:LONG; lpszText:LPCWSTR; hwndParent:HWND; wID:UINT):HWND; external ComctlDll name 'CreateStatusWindowW';
function CreateToolbarEx(hwnd:HWND; ws:DWORD; wID:UINT; nBitmaps:longint; hBMInst:HINST;wBMID:UINT; lpButtons:LPCTBBUTTON; iNumButtons:longint; dxButton:longint; dyButton:longint;dxBitmap:longint;
  dyBitmap:longint; uStructSize:UINT):HWND; external ComctlDLL name 'CreateToolbarEx';
function CreateUpDownControl(dwStyle:DWORD; x:longint; y:longint; cx:longint; cy:longint;hParent:HWND; nID:longint; hInst:HINST; hBuddy:HWND; nUpper:longint;nLower:longint; nPos:longint):HWND; external ComctlDLL name 'CreateUpDownControl';
function DestroyPropertySheetPage(hPSPage:HPROPSHEETPAGE):WINBOOL; external ComctlDLL name 'DestroyPropertySheetPage';
procedure DrawStatusText(hDC:HDC; lprc:LPRECT; pszText:LPCWSTR; uFlags:UINT); external ComctlDLL name 'DrawStatusTextW';
procedure DrawStatusTextW(hDC:HDC; lprc:LPRECT; pszText:LPCWSTR; uFlags:UINT); external ComctlDLL name 'DrawStatusTextW';
procedure InitCommonControls; external ComctlDLL name 'InitCommonControls';
function InitCommonControlsEx(_para1:LPINITCOMMONCONTROLSEX):WINBOOL; external ComctlDLL name 'InitCommonControlsEx';
function PropertySheet(lppsph:LPCPROPSHEETHEADER):longint; external ComctlDll name 'PropertySheetW';
function PropertySheetW(lppsph:LPCPROPSHEETHEADER):longint; external ComctlDll name 'PropertySheetW';

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}


