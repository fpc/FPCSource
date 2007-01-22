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

  COMCTL32_VERSION=$020e;

  // Common control shared messages
  CCM_FIRST = $2000;
  CCM_LAST  = CCM_FIRST + $200;

  CCM_SETBKCOLOR = CCM_FIRST + $1; // lParam is bkColor
  CCM_SETVERSION = CCM_FIRST + $7;
  CCM_GETVERSION = CCM_FIRST + $8;

  //InitCommonControlEx
  //I_IMAGENONE           = -2; // Desktop listview uses this same value for I_IMAGENONE when (_WIN32_IE >= 0x0501)

  //ICC_LISTVIEW_CLASSES  = $00000001; // listview, header
  //ICC_TREEVIEW_CLASSES  = $00000002; // treeview, tooltips
  //ICC_BAR_CLASSES       = $00000004; // toolbar, statusbar, trackbar, tooltips
  //ICC_TAB_CLASSES       = $00000008; // tab, tooltips
  //ICC_UPDOWN_CLASS      = $00000010; // updown
  //ICC_PROGRESS_CLASS    = $00000020; // progress
  ICC_ANIMATE_CLASS     = $00000080; // animate
  //ICC_WIN95_CLASSES     = $0000007F;
  //ICC_DATE_CLASSES      = $00000100; // month picker, date picker, time picker, updown
  //ICC_COOL_CLASSES      = $00000400; // rebar (coolbar) control
  //ICC_INTERNET_CLASSES  = $00000800; // IP Address control
  //ICC_TOOLTIP_CLASSES   = $00001000; // Tooltip static & button
  //ICC_CAPEDIT_CLASS     = $00002000; // All-caps edit control
  //ICC_FE_CLASSES        = $40000000; // FE specific input subclasses

  LVM_FIRST             =  $1000;      // ListView messages
  TV_FIRST              =  $1100;      // TreeView messages
  HDM_FIRST             =  $1200;      // Header messages

  //Ranges for control message IDs
  LVN_FIRST             =  (-100);       // listview
  LVN_LAST              =  (-199);
  HDN_FIRST             =  (-300);       // header
  HDN_LAST              =  (-399);
  TVN_FIRST             =  (-400);       // treeview
  TVN_LAST              =  (-499);
  TTN_FIRST             =  (-520);       // tooltips
  TTN_LAST              =  (-549);
  TCN_FIRST             =  (-550);       // tab control
  TCN_LAST              =  (-580);
  // Shell reserved               (0U-580U) -  (0U-589U)
  CDN_FIRST             =  (-601);        // common dialog (new)
  CDN_LAST              =  (-699);
  TBN_FIRST             =  (-700);        // toolbar
  TBN_LAST              =  (-720);
  UDN_FIRST             =  (-721);        // updown
  UDN_LAST              =  (-740);
  MCN_FIRST             =  (-750);        // monthcal
  MCN_LAST              =  (-759);
  RBN_FIRST             =  (-831);        // rebar
  RBN_LAST              =  (-859);
  IPN_FIRST             =  (-860);        // internet address
  IPN_LAST              =  (-879);        // internet address
  SBN_FIRST             =  (-880);        // status bar
  SBN_LAST              =  (-900);
  SHN_FIRST             =  (-1400);       // Shell reserved
  SHN_LAST              =  (-1500);

  MSGF_COMMCTRL_BEGINDRAG     = $4200;
  MSGF_COMMCTRL_SIZEHEADER    = $4201;
  MSGF_COMMCTRL_DRAGSELECT    = $4202;

  //Generic WM_NOTIFY notification codes
  NM_FIRST              =  0;
  NM_LAST               =  -99;
  //NM_OUTOFMEMORY        =  (NM_FIRST-1);
  //NM_CLICK              =  (NM_FIRST-2);
  //NM_DBLCLK             =  (NM_FIRST-3);
  //NM_RETURN             =  (NM_FIRST-4);
  //NM_RCLICK             =  (NM_FIRST-5);
  //NM_RDBLCLK            =  (NM_FIRST-6);
  //NM_SETFOCUS           =  (NM_FIRST-7);
  //NM_KILLFOCUS          =  (NM_FIRST-8);
  NM_CUSTOMDRAW         =  (NM_FIRST-12);
  NM_HOVER              =  (NM_FIRST-13);
  NM_NCHITTEST          =  (NM_FIRST-14);
  NM_KEYDOWN            =  (NM_FIRST-15);
  NM_RECOGNIZEGESTURE   =  (NM_FIRST-50);

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
  DTM_SETFORMAT       = DTM_SETFORMATW;


  DTS_UPDOWN                 = $0001;
  DTS_SHOWNONE               = $0002;
  DTS_SHORTDATEFORMAT        = $0000;
  DTS_LONGDATEFORMAT         = $0004;
  DTS_SHORTDATECENTURYFORMAT = $000C;
  DTS_TIMEFORMAT             = $0009;
  DTS_APPCANPARSE            = $0010;
  DTS_RIGHTALIGN             = $0020;
  DTS_NONEBUTTON             = $0080;

  //datetimepick
  DTN_FIRST                  =-760;
  DTN_LAST                   =-799;
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

  //TOOLBAR CONTROL
  //TOOLBARCLASSNAMEW   = 'ToolbarWindow32';
  //TOOLBARCLASSNAME    = TOOLBARCLASSNAMEW;

  TBIF_IMAGE          =  $00000001;
  TBIF_TEXT           =  $00000002;
  TBIF_STATE          =  $00000004;
  TBIF_STYLE          =  $00000008;
  TBIF_LPARAM         =  $00000010;
  TBIF_COMMAND        =  $00000020;
  TBIF_SIZE           =  $00000040;

  // BUTTONINFO APIs
  TB_GETBUTTONINFOW   = (WM_USER + 63);
  TB_SETBUTTONINFOW   = (WM_USER + 64);
  TB_GETBUTTONINFO    = TB_GETBUTTONINFOW;
  TB_SETBUTTONINFO    = TB_SETBUTTONINFOW;

  TB_INSERTBUTTONW    = (WM_USER + 67);
  TB_ADDBUTTONSW      = (WM_USER + 68);
  TB_HITTEST          = (WM_USER + 69);

  //TB_INSERTBUTTON     = TB_INSERTBUTTONW;
  //TB_ADDBUTTONS       = TB_ADDBUTTONSW;

  TB_SETDRAWTEXTFLAGS = (WM_USER + 70);

  //TBN_GETBUTTONINFOW  = (TBN_FIRST-20);
  //TBN_BEGINDRAG       = (TBN_FIRST-1);
  //TBN_ENDDRAG         = (TBN_FIRST-2);
  TBN_DROPDOWN        = (TBN_FIRST - 10);
  TBN_DRAGOUT         = (TBN_FIRST - 14);
  //TBN_GETBUTTONINFO   = TBN_GETBUTTONINFOW;

  // custom draw return flags
  // values under 0x00010000 are reserved for global custom draw values.
  // above that are for specific controls
  CDRF_DODEFAULT          = $00000000;
  CDRF_NEWFONT            = $00000002;
  CDRF_SKIPDEFAULT        = $00000004;
  CDRF_NOTIFYPOSTPAINT    = $00000010;
  CDRF_NOTIFYITEMDRAW     = $00000020;
  CDRF_NOTIFYSUBITEMDRAW  = $00000020;  // flags are the same, we can distinguish by context
  CDRF_NOTIFYPOSTERASE    = $00000040;

  // drawstage flags
  // values under = $00010000 are reserved for global custom draw values.
  // above that are for specific controls
  CDDS_PREPAINT           = $00000001;
  CDDS_POSTPAINT          = $00000002;
  CDDS_PREERASE           = $00000003;
  CDDS_POSTERASE          = $00000004;
  // the = $000010000 bit means it's individual item specific
  CDDS_ITEM               = $00010000;
  CDDS_ITEMPREPAINT       = CDDS_ITEM or CDDS_PREPAINT;
  CDDS_ITEMPOSTPAINT      = CDDS_ITEM or CDDS_POSTPAINT;
  CDDS_ITEMPREERASE       = CDDS_ITEM or CDDS_PREERASE;
  CDDS_ITEMPOSTERASE      = CDDS_ITEM or CDDS_POSTERASE;
  CDDS_SUBITEM            = $00020000;

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

  tagNMDATETIMESTRINGW = record
    nmhdr         : NMHDR;
    pszUserString : LPCWSTR;
    st            : SYSTEMTIME;
    dwFlags       : DWORD;
  end;
  NMDATETIMESTRINGW=tagNMDATETIMESTRINGW;
  TNMDATETIMESTRINGW=tagNMDATETIMESTRINGW;
  LPNMDATETIMESTRINGW=^tagNMDATETIMESTRINGW;
  NMDATETIMESTRING=tagNMDATETIMESTRINGW;
  TNMDATETIMESTRING=tagNMDATETIMESTRINGW;
  PNMDATETIMESTRING=^tagNMDATETIMESTRINGW;

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

  //Generic structure for a key
  tagNMKEY = Record
    hdr   : NMHDR;
    wVKey : WORD;
    flags : UINT;
  end;
  NMKEY=tagNMKEY;
  LPNMKEY=^NMKEY;

  //Generic WM_NOTIFY notification structures
  tagNMMOUSE = Record
    hdr        : NMHDR;
    dwItemSpec : DWORD;
    dwItemData : DWORD;
    pt         : POINT;
  end;
  NMMOUSE=tagNMMOUSE;
  LPNMMOUSE=^NMMOUSE;
  PNMMOUSE=^NMMOUSE;
  NMCLICK=NMMOUSE;
  LPNMCLICK=LPNMMOUSE;

  //TOOLBAR CONTROL
  TBBUTTONINFOW = record
    cbSize    : UINT;
    dwMask    : DWORD;
    idCommand : Longint;
    iImage    : Longint;
    fsState   : BYTE;
    fsStyle   : BYTE;
    cx        : WORD;
    lParam    : DWORD;
    pszText   : LPWSTR;
    cchText   : Longint;
  end;
  LPTBBUTTONINFOW=^TBBUTTONINFOW;
  TBBUTTONINFO=TBBUTTONINFOW;
  TTBButtonInfo=TBBUTTONINFO;

  tagNMCUSTOMDRAWINFO = packed record
    hdr: TNMHdr;
    dwDrawStage: DWORD;
    hdc: HDC;
    rc: TRect;
    dwItemSpec: DWORD;
    uItemState: UINT;
    lItemlParam: LPARAM;
  end;
  PNMCustomDraw = ^TNMCustomDraw;
  TNMCustomDraw = tagNMCUSTOMDRAWINFO;
  
  tagNMLVCUSTOMDRAW = packed record
    nmcd: TNMCustomDraw;
    clrText: COLORREF;
    clrTextBk: COLORREF;
    iSubItem: Integer;
  end;
  PNMLVCustomDraw = ^TNMLVCustomDraw;
  TNMLVCustomDraw = tagNMLVCUSTOMDRAW;
  
  tagNMLVODSTATECHANGE = packed record
    hdr: TNMHdr;
    iFrom: Integer;
    iTo: Integer;
    uNewState: UINT;
    uOldState: UINT;
  end;
  PNMLVODStateChange = ^TNMLVODStateChange;
  TNMLVODStateChange = tagNMLVODSTATECHANGE;
  
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


