{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 by Marco van de Voort
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Original copyright statement follows.

/****************************************************************************
*                                                                           *
* HtmlHelp.h                                                                *
*                                                                           *
* Copyright (c) 1996-1997, Microsoft Corp. All rights reserved.             *
*                                                                           *
****************************************************************************/
}

unit htmlhelp;

interface
{$ifdef FPC_OS_UNICODE}
  {$define UNICODE}
{$endif}

Uses Windows;

const
// Commands to pass to HtmlHelp()

        HH_DISPLAY_TOPIC         = $0000;
        HH_HELP_FINDER           = $0000;  // WinHelp equivalent
        HH_DISPLAY_TOC           = $0001;  
        HH_DISPLAY_INDEX         = $0002;  
        HH_DISPLAY_SEARCH        = $0003;  
        HH_SET_WIN_TYPE          = $0004;
        HH_GET_WIN_TYPE          = $0005;
        HH_GET_WIN_HANDLE        = $0006;
        HH_ENUM_INFO_TYPE        = $0007;  // Get Info type name, call repeatedly to enumerate, -1 at end
        HH_SET_INFO_TYPE         = $0008;  // Add Info type to filter.
        HH_SYNC                  = $0009;
        HH_RESERVED1             = $000A;
        HH_RESERVED2             = $000B;
        HH_RESERVED3             = $000C;
        HH_KEYWORD_LOOKUP        = $000D;
        HH_DISPLAY_TEXT_POPUP    = $000E;  // display string resource id or text in a popup window
        HH_HELP_CONTEXT          = $000F;  // display mapped numeric value in dwData
        HH_TP_HELP_CONTEXTMENU   = $0010;  // text popup help, same as WinHelp HELP_CONTEXTlMENU
        HH_TP_HELP_WM_HELP       = $0011;  // text popup help, same as WinHelp HELP_WM_HELP
        HH_CLOSE_ALL             = $0012;  // close all windows opened directly or indirectly by the caller
        HH_ALINK_LOOKUP          = $0013;  // ALink version of HH_KEYWORD_LOOKUP
        HH_GET_LAST_ERROR        = $0014;  // not currently implemented // See HHERROR.h
        HH_ENUM_CATEGORY         = $0015;	// Get category name, call repeatedly to enumerate, -1 at end
        HH_ENUM_CATEGORY_IT      = $0016;  // Get category info type members, call repeatedly to enumerate, -1 at end
        HH_RESET_IT_FILTER       = $0017;  // Clear the info type filter of all info types.
        HH_SET_INCLUSIVE_FILTER  = $0018;  // set inclusive filtering method for untyped topics to be included in display
        HH_SET_EXCLUSIVE_FILTER  = $0019;  // set exclusive filtering method for untyped topics to be excluded from display
        HH_INITIALIZE            = $001C;  // Initializes the help system.
        HH_UNINITIALIZE          = $001D;  // Uninitializes the help system.
        HH_SET_QUERYSERVICE      = $001E;  // Set the Host IQueryService interface
        HH_PRETRANSLATEMESSAGE   = $00fd;  // Pumps messages. (NULL, NULL, MSG*).
        HH_SET_GLOBAL_PROPERTY   = $00fc;  // Set a global property. (NULL, NULL, HH_GPROP)
        HH_SAFE_DISPLAY_TOPIC    = $0020;  // private addition to the interface for InternetExplorer.

        HHWIN_PROP_TAB_AUTOHIDESHOW = (1 shl 0);    // Automatically hide/show tri-pane window
        HHWIN_PROP_ONTOP            = (1 shl 1);    // Top-most window
        HHWIN_PROP_NOTITLEBAR       = (1 shl 2);    // no title bar
        HHWIN_PROP_NODEF_STYLES     = (1 shl 3);    // no default window styles (only HH_WINTYPE.dwStyles)
        HHWIN_PROP_NODEF_EXSTYLES   = (1 shl 4);    // no default extended window styles (only HH_WINTYPE.dwExStyles)
        HHWIN_PROP_TRI_PANE         = (1 shl 5);    // use a tri-pane window
        HHWIN_PROP_NOTB_TEXT        = (1 shl 6);    // no text on toolbar buttons
        HHWIN_PROP_POST_QUIT        = (1 shl 7);    // post WM_QUIT message when window closes
        HHWIN_PROP_AUTO_SYNC        = (1 shl 8);    // automatically ssync contents and index
        HHWIN_PROP_TRACKING         = (1 shl 9);    // send tracking notification messages
        HHWIN_PROP_TAB_SEARCH       = (1 shl 10);   // include search tab in navigation pane
        HHWIN_PROP_TAB_HISTORY      = (1 shl 11);   // include history tab in navigation pane
        HHWIN_PROP_TAB_FAVORITES    = (1 shl 12);   // include favorites tab in navigation pane
        HHWIN_PROP_CHANGE_TITLE     = (1 shl 13);   // Put current HTML title in title bar
        HHWIN_PROP_NAV_ONLY_WIN     = (1 shl 14);   // Only display the navigation window
        HHWIN_PROP_NO_TOOLBAR       = (1 shl 15);   // Don't display a toolbar
        HHWIN_PROP_MENU             = (1 shl 16);   // Menu
        HHWIN_PROP_TAB_ADVSEARCH    = (1 shl 17);   // Advanced FTS UI.
        HHWIN_PROP_USER_POS         = (1 shl 18);   // After initial creation, user controls window size/position
        HHWIN_PROP_TAB_CUSTOM1      = (1 shl 19);   // Use custom tab #1
        HHWIN_PROP_TAB_CUSTOM2      = (1 shl 20);   // Use custom tab #2
        HHWIN_PROP_TAB_CUSTOM3      = (1 shl 21);   // Use custom tab #3
        HHWIN_PROP_TAB_CUSTOM4      = (1 shl 22);   // Use custom tab #4
        HHWIN_PROP_TAB_CUSTOM5      = (1 shl 23);   // Use custom tab #5
        HHWIN_PROP_TAB_CUSTOM6      = (1 shl 24);   // Use custom tab #6
        HHWIN_PROP_TAB_CUSTOM7      = (1 shl 25);   // Use custom tab #7
        HHWIN_PROP_TAB_CUSTOM8      = (1 shl 26);   // Use custom tab #8
        HHWIN_PROP_TAB_CUSTOM9      = (1 shl 27);   // Use custom tab #9
        HHWIN_TB_MARGIN             = (1 shl 28);   // the window type has a margin

        HHWIN_PARAM_PROPERTIES      = (1 shl 1);    // valid fsWinProperties
        HHWIN_PARAM_STYLES          = (1 shl 2);    // valid dwStyles
        HHWIN_PARAM_EXSTYLES        = (1 shl 3);    // valid dwExStyles
        HHWIN_PARAM_RECT            = (1 shl 4);    // valid rcWindowPos
        HHWIN_PARAM_NAV_WIDTH       = (1 shl 5);    // valid iNavWidth
        HHWIN_PARAM_SHOWSTATE       = (1 shl 6);    // valid nShowState
        HHWIN_PARAM_INFOTYPES       = (1 shl 7);    // valid apInfoTypes
        HHWIN_PARAM_TB_FLAGS        = (1 shl 8);    // valid fsToolBarFlags
        HHWIN_PARAM_EXPANSION       = (1 shl 9);    // valid fNotExpanded
        HHWIN_PARAM_TABPOS          = (1 shl 10);   // valid tabpos
        HHWIN_PARAM_TABORDER        = (1 shl 11);   // valid taborder
        HHWIN_PARAM_HISTORY_COUNT   = (1 shl 12);   // valid cHistory
        HHWIN_PARAM_CUR_TAB         = (1 shl 13);   // valid curNavType

        HHWIN_BUTTON_EXPAND         = (1 shl 1);    // Expand/contract button
        HHWIN_BUTTON_BACK           = (1 shl 2);    // Back button
        HHWIN_BUTTON_FORWARD        = (1 shl 3);    // Forward button
        HHWIN_BUTTON_STOP           = (1 shl 4);    // Stop button
        HHWIN_BUTTON_REFRESH        = (1 shl 5);    // Refresh button
        HHWIN_BUTTON_HOME           = (1 shl 6);    // Home button
        HHWIN_BUTTON_BROWSE_FWD     = (1 shl 7);    // not implemented
        HHWIN_BUTTON_BROWSE_BCK     = (1 shl 8);    // not implemented
        HHWIN_BUTTON_NOTES          = (1 shl 9);    // not implemented
        HHWIN_BUTTON_CONTENTS       = (1 shl 10);   // not implemented
        HHWIN_BUTTON_SYNC           = (1 shl 11);   // Sync button
        HHWIN_BUTTON_OPTIONS        = (1 shl 12);   // Options button
        HHWIN_BUTTON_PRINT          = (1 shl 13);   // Print button
        HHWIN_BUTTON_INDEX          = (1 shl 14);   // not implemented
        HHWIN_BUTTON_SEARCH         = (1 shl 15);   // not implemented
        HHWIN_BUTTON_HISTORY        = (1 shl 16);   // not implemented
        HHWIN_BUTTON_FAVORITES      = (1 shl 17);   // not implemented
        HHWIN_BUTTON_JUMP1          = (1 shl 18);
        HHWIN_BUTTON_JUMP2          = (1 shl 19);
        HHWIN_BUTTON_ZOOM           = (1 shl 20);
        HHWIN_BUTTON_TOC_NEXT       = (1 shl 21);
        HHWIN_BUTTON_TOC_PREV       = (1 shl 22);


// Button IDs

        IDTB_EXPAND             = 200; 
        IDTB_CONTRACT           = 201; 
        IDTB_STOP               = 202; 
        IDTB_REFRESH            = 203; 
        IDTB_BACK               = 204; 
        IDTB_HOME               = 205; 
        IDTB_SYNC               = 206; 
        IDTB_PRINT              = 207; 
        IDTB_OPTIONS            = 208; 
        IDTB_FORWARD            = 209; 
        IDTB_NOTES              = 210;  // not implemented
        IDTB_BROWSE_FWD         = 211; 
        IDTB_BROWSE_BACK        = 212; 
        IDTB_CONTENTS           = 213;  // not implemented
        IDTB_INDEX              = 214;  // not implemented
        IDTB_SEARCH             = 215;  // not implemented
        IDTB_HISTORY            = 216;  // not implemented
        IDTB_FAVORITES          = 217;  // not implemented
        IDTB_JUMP1              = 218; 
        IDTB_JUMP2              = 219;
        IDTB_CUSTOMIZE          = 221;
        IDTB_ZOOM               = 222;
        IDTB_TOC_NEXT           = 223;
        IDTB_TOC_PREV           = 224;

// Notification codes

        HHN_FIRST       = longword (0)-longword(860); // (0U-860U)
        HHN_LAST        = longword (0)-longword(879); // (0U-879U)

        HHN_NAVCOMPLETE   = (HHN_FIRST-0);
        HHN_TRACK         = (HHN_FIRST-1);
        HHN_WINDOW_CREATE = (HHN_FIRST-2);

        HHWIN_DEF_BUTTONS           =       (HHWIN_BUTTON_EXPAND  or
                                             HHWIN_BUTTON_BACK    or
                                             HHWIN_BUTTON_OPTIONS or
                                             HHWIN_BUTTON_PRINT);

        HHWIN_NAVTYPE_TOC          = 0;
        HHWIN_NAVTYPE_INDEX        = 1;
        HHWIN_NAVTYPE_SEARCH       = 2;
        HHWIN_NAVTYPE_FAVORITES    = 3;
        HHWIN_NAVTYPE_HISTORY      = 4; // not implemented
        HHWIN_NAVTYPE_AUTHOR       = 5;
        HHWIN_NAVTYPE_CUSTOM_FIRST = 11;
        IT_INCLUSIVE               = 0;
        IT_EXCLUSIVE               = 1;
        IT_HIDDEN                  = 2;

        HHWIN_NAVTAB_TOP           = 0;
        HHWIN_NAVTAB_LEFT          = 1;
        HHWIN_NAVTAB_BOTTOM        = 2;

        HH_MAX_TABS                = 19;  // maximum number of tabs

        HH_TAB_CONTENTS            = 0;
        HH_TAB_INDEX               = 1;
        HH_TAB_SEARCH              = 2;
        HH_TAB_FAVORITES           = 3;
        HH_TAB_HISTORY             = 4;
        HH_TAB_AUTHOR              = 5;

        HH_TAB_CUSTOM_FIRST        = 11;
        HH_TAB_CUSTOM_LAST         = HH_MAX_TABS;

        HH_MAX_TABS_CUSTOM         = (HH_TAB_CUSTOM_LAST - HH_TAB_CUSTOM_FIRST + 1);

// HH_DISPLAY_SEARCH Command Related Structures and Constants

        HH_FTS_DEFAULT_PROXIMITY   = (-1);

        HHACT_TAB_CONTENTS         = 0;
        HHACT_TAB_INDEX            = 1;
        HHACT_TAB_SEARCH           = 2;
        HHACT_TAB_HISTORY          = 3;
        HHACT_TAB_FAVORITES        = 4;

        HHACT_EXPAND               = 5;
        HHACT_CONTRACT             = 6;
        HHACT_BACK                 = 7;
        HHACT_FORWARD              = 8;
        HHACT_STOP                 = 9;
        HHACT_REFRESH              = 10;
        HHACT_HOME                 = 11;
        HHACT_SYNC                 = 12;
        HHACT_OPTIONS              = 13;
        HHACT_PRINT                = 14;
        HHACT_HIGHLIGHT            = 15;
        HHACT_CUSTOMIZE            = 16;
        HHACT_JUMP1                = 17;
        HHACT_JUMP2                = 18;
        HHACT_ZOOM                 = 19;
        HHACT_TOC_NEXT             = 20;
        HHACT_TOC_PREV             = 21;
        HHACT_NOTES                = 22;

        HHACT_LAST_ENUM            = 23;

  const
    External_library='hhctrl.ocx'; {Setup as you need}

  { Pointers to basic pascal types, inserted by h2pas conversion program.}

Type

     PtagHHN_NOTIFY = ^tagHHN_NOTIFY;
     tagHHN_NOTIFY = packed record
          hdr : NMHDR;
          pszUrl : PCSTR;                 { Multi-byte, null-terminated string }
       end;
     HHN_NOTIFY = tagHHN_NOTIFY;
     PHHN_NOTIFY = ^HHN_NOTIFY;

     PtagHH_POPUP = ^tagHH_POPUP;
     tagHH_POPUP = packed record
          cbStruct : longint;             { sizeof this structure }
          hinst : HINST {ANCE};              { instance handle for string resource }
          idString : UINT;                { string resource id, or text id if pszFile is specified in HtmlHelp call }
          pszText : LPCTSTR;              { used if idString is zero }
          pt : POINT;                     { top center of popup window }
          clrForeground : COLORREF;       { use -1 for default }
          clrBackground : COLORREF;       { use -1 for default }
          rcMargins : RECT;               { amount of space between edges of window and text, -1 for each member to ignore }
          pszFont : LPCTSTR;              { facename, point size, char set, BOLD ITALIC UNDERLINE }
       end;
     HH_POPUP = tagHH_POPUP;
     PHH_POPUP = ^HH_POPUP;

     PtagHH_AKLINK = ^tagHH_AKLINK;
     tagHH_AKLINK = packed record
          cbStruct : longint;              { sizeof this structure }
          fReserved : BOOL;                { must be FALSE (really!) }
          pszKeywords : LPCTSTR;           { semi-colon separated keywords }
          pszUrl : LPCTSTR;                { URL to jump to if no keywords found (may be NULL) }
          pszMsgText : LPCTSTR;            { Message text to display in MessageBox if pszUrl is NULL and no keyword match }
          pszMsgTitle : LPCTSTR;           { Message text to display in MessageBox if pszUrl is NULL and no keyword match }
          pszWindow : LPCTSTR;             { Window to display URL in }
          fIndexOnFail : BOOL;             { Displays index if keyword lookup fails. }
       end;
     HH_AKLINK = tagHH_AKLINK;
     PHH_AKLINK = ^HH_AKLINK;

     PtagHH_ENUM_IT = ^tagHH_ENUM_IT;
     tagHH_ENUM_IT = packed record
          cbStruct : longint;              { size of this structure }
          iType : longint;                 { the type of the information type ie. Inclusive, Exclusive, or Hidden }
          pszCatName : LPCSTR;             { Set to the name of the Category to enumerate the info types in a category; else NULL }
          pszITName : LPCSTR;              { volitile pointer to the name of the infotype. Allocated by call. Caller responsible for freeing }
          pszITDescription : LPCSTR;       { volitile pointer to the description of the infotype. }
       end;
     HH_ENUM_IT = tagHH_ENUM_IT;
     PHH_ENUM_IT = PtagHH_ENUM_IT;
     PPHH_ENUM_IT = ^PHH_ENUM_IT;

     PtagHH_ENUM_CAT = ^tagHH_ENUM_CAT;
     tagHH_ENUM_CAT = packed record
          cbStruct : longint;                  { size of this structure }
          pszCatName : LPCSTR;                 { volitile pointer to the category name }
          pszCatDescription : LPCSTR;          { volitile pointer to the category description }
       end;
     HH_ENUM_CAT = tagHH_ENUM_CAT;
     PHH_ENUM_CAT = PtagHH_ENUM_CAT;
     PPHH_ENUM_CAT = ^PHH_ENUM_CAT;

     PtagHH_SET_INFOTYPE = ^tagHH_SET_INFOTYPE;
     tagHH_SET_INFOTYPE = packed record
          cbStruct : longint;             { the size of this structure }
          pszCatName : LPCSTR;            { the name of the category, if any, the InfoType is a member of. }
          pszInfoTypeName : LPCSTR;       { the name of the info type to add to the filter }
       end;
     HH_SET_INFOTYPE = tagHH_SET_INFOTYPE;
     PHH_SET_INFOTYPE = PtagHH_SET_INFOTYPE;
     PPHH_SET_INFOTYPE = ^PHH_SET_INFOTYPE;

     HH_INFOTYPE = DWORD;
     PPHH_INFOTYPE = ^PHH_INFOTYPE;
     PHH_INFOTYPE = HH_INFOTYPE;

     PtagHH_FTS_QUERY = ^tagHH_FTS_QUERY;
     tagHH_FTS_QUERY = packed record
          cbStruct : longint;            { Sizeof structure in bytes. }
          fUniCodeStrings : BOOL;        { TRUE if all strings are unicode. }
          pszSearchQuery : LPCTSTR;      { String containing the search query. }
          iProximity : LONG;             { Word proximity. }
          fStemmedSearch : BOOL;         { TRUE for StemmedSearch only. }
          fTitleOnly : BOOL;             { TRUE for Title search only. }
          fExecute : BOOL;               { TRUE to initiate the search. }
          pszWindow : LPCTSTR;           { Window to display in }
       end;
     HH_FTS_QUERY = tagHH_FTS_QUERY;
     PHH_FTS_QUERY = ^HH_FTS_QUERY;

     PtagHH_WINTYPE = ^tagHH_WINTYPE;
     tagHH_WINTYPE = packed record                                   { HH_WINTYPE Structure }
          cbStruct : longint;                                        { IN: size of this structure including all Information Types }
          fUniCodeStrings : BOOL;                                    { IN/OUT: TRUE if all strings are in UNICODE }
          pszType : LPCTSTR;                                         { IN/OUT: Name of a type of window }
          fsValidMembers : DWORD;                                    { IN: Bit flag of valid members (HHWIN_PARAM_) }
          fsWinProperties : DWORD;                                   { IN/OUT: Properties/attributes of the window (HHWIN_) }
          pszCaption : LPCTSTR;                                      { IN/OUT: Window title }
          dwStyles : DWORD;                                          { IN/OUT: Window styles }
          dwExStyles : DWORD;                                        { IN/OUT: Extended Window styles }
          rcWindowPos : RECT;                                        { IN: Starting position, OUT: current position }
          nShowState : longint;                                      { IN: show state (e.g., SW_SHOW) }
          hwndHelp : HWND;                                           { OUT: window handle }
          hwndCaller : HWND;                                         { OUT: who called this window }
          paInfoTypes : PHH_INFOTYPE;                                { IN: Pointer to an array of Information Types }
          { The following members are only valid if HHWIN_PROP_TRI_PANE is set }
          hwndToolBar : HWND;                                        { OUT: toolbar window in tri-pane window }
          hwndNavigation : HWND;                                     { OUT: navigation window in tri-pane window }
          hwndHTML : HWND;                                           { OUT: window displaying HTML in tri-pane window }
          iNavWidth : longint;                                       { IN/OUT: width of navigation window }
          rcHTML : RECT;                                             { OUT: HTML window coordinates }
          pszToc : LPCTSTR;                                          { IN: Location of the table of contents file }
          pszIndex : LPCTSTR;                                        { IN: Location of the index file }
          pszFile : LPCTSTR;                                         { IN: Default location of the html file }
          pszHome : LPCTSTR;                                         { IN/OUT: html file to display when Home button is clicked }
          fsToolBarFlags : DWORD;                                    { IN: flags controling the appearance of the toolbar }
          fNotExpanded : BOOL;                                       { IN: TRUE/FALSE to contract or expand, OUT: current state }
          curNavType : longint;                                      { IN/OUT: UI to display in the navigational pane }
          tabpos : longint;                                          { IN/OUT: HHWIN_NAVTAB_TOP, HHWIN_NAVTAB_LEFT, or HHWIN_NAVTAB_BOTTOM }
          idNotify : longint;                                        { IN: ID to use for WM_NOTIFY messages }
          tabOrder : array[0..(HH_MAX_TABS+1)-1] of BYTE;            { IN/OUT: tab order: Contents, Index, Search, History, Favorites, Reserved 1-5, Custom tabs }
          cHistory : longint;                                        { IN/OUT: number of history items to keep (default is 30) }
          pszJump1 : LPCTSTR;                                        { Text for HHWIN_BUTTON_JUMP1 }
          pszJump2 : LPCTSTR;                                        { Text for HHWIN_BUTTON_JUMP2 }
          pszUrlJump1 : LPCTSTR;                                     { URL for HHWIN_BUTTON_JUMP1 }
          pszUrlJump2 : LPCTSTR;                                     { URL for HHWIN_BUTTON_JUMP2 }
          rcMinSize : RECT;                                          { Minimum size for window (ignored in version 1) }
          cbInfoTypes : longint;                                     { size of paInfoTypes; }
          pszCustomTabs : LPCTSTR;                                   { multiple zero-terminated strings }
       end;
     HH_WINTYPE = tagHH_WINTYPE;
     PHH_WINTYPE = PtagHH_WINTYPE;
     PPHH_WINTYPE = ^PHH_WINTYPE;

     PtagHHNTRACK = ^tagHHNTRACK;
     tagHHNTRACK = packed record
          hdr : NMHDR;
          pszCurUrl : PCSTR;               { Multi-byte, null-terminated string }
          idAction : longint;              { HHACT_ value }
          phhWinType : PHH_WINTYPE;        { Current window type structure }
       end;
     HHNTRACK = tagHHNTRACK;
     PHHNTRACK = ^HHNTRACK;

  function HtmlHelpA(hwndCaller:HWND; pszFile:LPCSTR; uCommand:UINT; dwData:DWORD_PTR):HWND;stdcall;external External_library name 'HtmlHelpA';
  function HtmlHelpW(hwndCaller:HWND; pszFile:LPCWSTR; uCommand:UINT; dwData:DWORD_PTR):HWND;stdcall;external External_library name 'HtmlHelpW';
 {$ifdef Unicode}
  function HtmlHelp(hwndCaller:HWND; pszFile:LPCWSTR; uCommand:UINT; dwData:DWORD_PTR):HWND;stdcall;external External_library name 'HtmlHelpW';
 {$else}
  function HtmlHelp(hwndCaller:HWND; pszFile:LPCSTR; uCommand:UINT; dwData:DWORD_PTR):HWND;stdcall;external External_library name 'HtmlHelpA';
 {$endif}

type HH_GPROPID = (

    HH_GPROPID_SINGLETHREAD=1,      // VARIANT_BOOL: True for single thread
    HH_GPROPID_TOOLBAR_MARGIN=2,    // long: Provides a left/right margin around the toolbar.
    HH_GPROPID_UI_LANGUAGE=3,       // long: LangId of the UI.
    HH_GPROPID_CURRENT_SUBSET=4,    // BSTR: Current subset.
    HH_GPROPID_CONTENT_LANGUAGE=5   // long: LandId for desired content.
    );

  {$packrecords 8}

  type
     PtagHH_GLOBAL_PROPERTY = ^tagHH_GLOBAL_PROPERTY;
     tagHH_GLOBAL_PROPERTY = packed record
          id : HH_GPROPID;
          _var : VARIANT;
       end;
     HH_GLOBAL_PROPERTY = tagHH_GLOBAL_PROPERTY;
     PHH_GLOBAL_PROPERTY = ^HH_GLOBAL_PROPERTY;

implementation

end.
