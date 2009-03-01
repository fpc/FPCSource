{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

********************************************************************* }

//**********************************************************************/
//*                                                                    */
//*      IMM.H - Input Method Manager definitions                      */
//*                                                                    */
//**********************************************************************/

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit imm;

{$CALLING cdecl}

interface

uses Windows;

type
     HIMC  = DWORD;
     HIMCC = DWORD;

const
      NULLIMC	= HIMC(0);

type
     LPHKL = ^HKL;
     LPUINT = ^UINT;


type
     tagCOMPOSITIONFORM = record
       dwStyle:DWORD;
       ptCurrentPos:POINT;
       rcArea:RECT;
     end;
     COMPOSITIONFORM = tagCOMPOSITIONFORM;
     PCOMPOSITIONFORM = ^COMPOSITIONFORM;
     NPCOMPOSITIONFORM = ^COMPOSITIONFORM;
     LPCOMPOSITIONFORM = ^COMPOSITIONFORM;

type
     tagCANDIDATEFORM  = record
       dwIndex:DWORD;
       dwStyle:DWORD;
       ptCurrentPos:POINT;
       rcArea:RECT;
     end;
     CANDIDATEFORM = tagCANDIDATEFORM;
     PCANDIDATEFORM = ^CANDIDATEFORM;
     NPCANDIDATEFORM = ^CANDIDATEFORM;
     LPCANDIDATEFORM = ^CANDIDATEFORM;


type
     tagCANDIDATELIST = record
       dwSize:DWORD;
       dwStyle:DWORD;
       dwCount:DWORD;
       dwSelection:DWORD;
       dwPageStart:DWORD;
       dwPageSize:DWORD;
       dwOffset:array[0..0] of DWORD;
     end;
     CANDIDATELIST = tagCANDIDATELIST;
     PCANDIDATELIST = ^CANDIDATELIST;
     NPCANDIDATELIST = ^CANDIDATELIST;
     LPCANDIDATELIST = ^CANDIDATELIST;

type
     tagREGISTERWORDA = record
       lpReading:LPSTR;
       lpWord:LPSTR;
     end;
     REGISTERWORDA = tagREGISTERWORDA;
     PREGISTERWORDA = ^REGISTERWORDA;
     NPREGISTERWORDA = ^REGISTERWORDA;
     LPREGISTERWORDA = ^REGISTERWORDA;

type
     tagREGISTERWORDW = record
       lpReading:LPWSTR;
       lpWord:LPWSTR;
     end;
     REGISTERWORDW = tagREGISTERWORDW;
     PREGISTERWORDW = ^REGISTERWORDW;
     NPREGISTERWORDW = ^REGISTERWORDW;
     LPREGISTERWORDW = ^REGISTERWORDW;

{$IFDEF UNICODE}
type
     REGISTERWORD = REGISTERWORDW;
     PREGISTERWORD = PREGISTERWORDW;
     NPREGISTERWORD = NPREGISTERWORDW;
     LPREGISTERWORD = LPREGISTERWORDW;
{$ELSE UNICODE}
type
     REGISTERWORD = REGISTERWORDA;
     PREGISTERWORD = PREGISTERWORDA;
     NPREGISTERWORD = NPREGISTERWORDA;
     LPREGISTERWORD = LPREGISTERWORDA;
{$ENDIF UNICODE}

const
      STYLE_DESCRIPTION_SIZE  = 32;

type
     tagSTYLEBUFA = record
       dwStyle:DWORD;
       szDescription:array[0..STYLE_DESCRIPTION_SIZE-1] of char;
     end;
     STYLEBUFA = tagSTYLEBUFA;
     PSTYLEBUFA = ^STYLEBUFA;
     NPSTYLEBUFA = ^STYLEBUFA;
     LPSTYLEBUFA = ^STYLEBUFA;

type
     tagSTYLEBUFW = record
       dwStyle:DWORD;
       szDescription:array[0..STYLE_DESCRIPTION_SIZE-1] of WCHAR;
     end;
     STYLEBUFW = tagSTYLEBUFW;
     PSTYLEBUFW = ^STYLEBUFW;
     NPSTYLEBUFW = ^STYLEBUFW;
     LPSTYLEBUFW = ^STYLEBUFW;

{$IFDEF UNICODE}
type
     STYLEBUF = STYLEBUFW;
     PSTYLEBUF = PSTYLEBUFW;
     NPSTYLEBUF = NPSTYLEBUFW;
     LPSTYLEBUF = LPSTYLEBUFW;
{$ELSE UNICODE}
type
     STYLEBUF = STYLEBUFA;
     PSTYLEBUF = PSTYLEBUFA;
     NPSTYLEBUF = NPSTYLEBUFA;
     LPSTYLEBUF = LPSTYLEBUFA;
{$ENDIF UNICODE}

const
      IMEMENUITEM_STRING_SIZE = 80;

type
     tagIMEMENUITEMINFOA = record
       cbSize:UINT;
       fType:UINT;
       fState:UINT;
       wID:UINT;
       hbmpChecked:HBITMAP;
       hbmpUnchecked:HBITMAP;
       dwItemData:DWORD;
       szString:array[0..IMEMENUITEM_STRING_SIZE-1] of char;
       hbmpItem:HBITMAP;
     end;
     IMEMENUITEMINFOA = tagIMEMENUITEMINFOA;
     PIMEMENUITEMINFOA = ^IMEMENUITEMINFOA;
     NPIMEMENUITEMINFOA = ^IMEMENUITEMINFOA;
     LPIMEMENUITEMINFOA = ^IMEMENUITEMINFOA;

type
     tagIMEMENUITEMINFOW = record
       cbSize:UINT;
       fType:UINT;
       fState:UINT;
       wID:UINT;
       hbmpChecked:HBITMAP;
       hbmpUnchecked:HBITMAP;
       dwItemData:DWORD;
       szString:array[0..IMEMENUITEM_STRING_SIZE-1] of WCHAR;
       hbmpItem:HBITMAP;
     end;
     IMEMENUITEMINFOW = tagIMEMENUITEMINFOW;
     PIMEMENUITEMINFOW = ^IMEMENUITEMINFOW;
     NPIMEMENUITEMINFOW = ^IMEMENUITEMINFOW;
     LPIMEMENUITEMINFOW = ^IMEMENUITEMINFOW;

{$IFDEF UNICODE}
type
     IMEMENUITEMINFO = IMEMENUITEMINFOW;
     PIMEMENUITEMINFO = PIMEMENUITEMINFOW;
     NPIMEMENUITEMINFO = NPIMEMENUITEMINFOW;
     LPIMEMENUITEMINFO = LPIMEMENUITEMINFOW;
{$ELSE UNICODE}
type
     IMEMENUITEMINFO = IMEMENUITEMINFOA;
     PIMEMENUITEMINFO = PIMEMENUITEMINFOA;
     NPIMEMENUITEMINFO = NPIMEMENUITEMINFOA;
     LPIMEMENUITEMINFO = LPIMEMENUITEMINFOA;
{$ENDIF UNICODE}

type
     tagIMECHARPOSITION = record
       dwSize:DWORD;
       dwCharPos:DWORD;
       pt:POINT;
       cLineHeight:UINT;
       rcDocument:RECT;
     end;
     IMECHARPOSITION = tagIMECHARPOSITION;
     PIMECHARPOSITION = ^IMECHARPOSITION;
     NPIMECHARPOSITION = ^IMECHARPOSITION;
     LPIMECHARPOSITION = ^IMECHARPOSITION;


// prototype of IMM API

const
      ImmDLL = 'coredll.dll';

{$IFNDEF WINCE}
function ImmInstallIMEW(lpszIMEFileName:LPCWSTR; lpszLayoutText:LPCWSTR):HKL; external ImmDLL name 'ImmInstallIMEW';

{$IFDEF UNICODE}
function ImmInstallIME(lpszIMEFileName:LPCWSTR; lpszLayoutText:LPCWSTR):HKL; external ImmDLL name 'ImmInstallIMEW';
{$ELSE UNICODE}
function ImmInstallIME(lpszIMEFileName:LPCWSTR; lpszLayoutText:LPCWSTR):HKL; external ImmDLL name 'ImmInstallIMEA';
{$ENDIF UNICODE}

{$ENDIF WINCE}

function ImmGetDefaultIMEWnd(_hwnd:HWND):HWND; external ImmDLL name 'ImmGetDefaultIMEWnd'; // 459

function ImmGetDescriptionW(_hkl:HKL; lpszDescription:LPWSTR; uBufLen:UINT):UINT; external ImmDLL name 'ImmGetDescriptionW'; // 45A
{$IFDEF UNICODE}
function ImmGetDescription(_hkl:HKL; lpszDescription:LPWSTR; uBufLen:UINT):UINT; external ImmDLL name 'ImmGetDescriptionW';
{$ELSE UNICODE}
function ImmGetDescription(_hkl:HKL; lpszDescription:LPSTR; uBufLen:UINT):UINT; external ImmDLL name 'ImmGetDescriptionA';
{$ENDIF UNICODE}

function ImmGetIMEFileNameW(_hkl:HKL; lpszFileName:LPWSTR; uBufLen:UINT):UINT; external ImmDLL name 'ImmGetIMEFileNameW'; // 477
{$IFDEF UNICODE}
function ImmGetIMEFileName(_hkl:HKL; lpszFileName:LPWSTR; uBufLen:UINT):UINT; external ImmDLL name 'ImmGetIMEFileNameW';
{$ELSE UNICODE}
function ImmGetIMEFileName(_hkl:HKL; lpszFileName:LPSTR; uBufLen:UINT):UINT; external ImmDLL name 'ImmGetIMEFileNameA';
{$ENDIF UNICODE}

function ImmGetProperty(_hkl:HKL; fdwIndex:DWORD):DWORD; external ImmDLL name 'ImmGetProperty'; // 460

function ImmIsIME(_hkl:HKL):BOOL; external ImmDLL name 'ImmIsIME'; // 448 

function ImmSimulateHotKey(_hwnd:HWND; dwHotKeyID:DWORD):BOOL; external ImmDLL name 'ImmSimulateHotKey'; // 472

function ImmCreateContext:HIMC; external ImmDLL name 'ImmCreateContext'; // 44A
function ImmDestroyContext(_himc:HIMC):BOOL; external ImmDLL name 'ImmDestroyContext'; // 44B
function ImmGetContext(_hwnd:HWND):HIMC; external ImmDLL name 'ImmGetContext'; // 440
function ImmReleaseContext(_hwnd:HWND; _himc:HIMC):BOOL; external ImmDLL name 'ImmReleaseContext'; // 445
function ImmAssociateContext(_hwnd:HWND; _himc:HIMC):HIMC; external ImmDLL name 'ImmAssociateContext'; // 44C
function ImmAssociateContextEx(_hwnd:HWND; _himc:HIMC; dwFlags:DWORD):BOOL; external ImmDLL name 'ImmAssociateContextEx'; // 476

function ImmGetCompositionStringW(_himc:HIMC; dwIndex:DWORD; lpBuf:LPVOID; dwBufLen:DWORD):LONG; external ImmDLL name 'ImmGetCompositionStringW'; // 447
{$IFDEF UNICODE}
function ImmGetCompositionString(_himc:HIMC; dwIndex:DWORD; lpBuf:LPVOID; dwBufLen:DWORD):LONG; external ImmDLL name 'ImmGetCompositionStringW';
{$ELSE UNICODE}
function ImmGetCompositionString(_himc:HIMC; dwIndex:DWORD; lpBuf:LPVOID; dwBufLen:DWORD):LONG; external ImmDLL name 'ImmGetCompositionStringA';
{$ENDIF UNICODE}

function ImmSetCompositionStringW(_himc:HIMC;
                                  dwIndex:DWORD;
                                  lpComp:LPCVOID;
                                  dwCompLen:DWORD;
                                  lpRead:LPCVOID;
                                  dwReadLen:DWORD):BOOL; external ImmDLL name 'ImmSetCompositionStringW'; // 46B
{$IFDEF UNICODE}
function ImmSetCompositionString(_himc:HIMC;
                                 dwIndex:DWORD;
                                 lpComp:LPCVOID;
                                 dwCompLen:DWORD;
                                 lpRead:LPCVOID;
                                 dwReadLen:DWORD):BOOL; external ImmDLL name 'ImmSetCompositionStringW';
{$ELSE UNICODE}
function ImmSetCompositionString(_himc:HIMC;
                                 dwIndex:DWORD;
                                 lpComp:LPCVOID;
                                 dwCompLen:DWORD;
                                 lpRead:LPCVOID;
                                 dwReadLen:DWORD):BOOL; external ImmDLL name 'ImmSetCompositionStringA';
{$ENDIF UNICODE}

function ImmGetCandidateListCountW(_himc:HIMC; lpdwListCount:LPDWORD):DWORD; external ImmDLL name 'ImmGetCandidateListCountW'; // 454
{$IFDEF UNICODE}
function ImmGetCandidateListCount(_himc:HIMC; lpdwListCount:LPDWORD):DWORD; external ImmDLL name 'ImmGetCandidateListCountW';
{$ELSE UNICODE}
function ImmGetCandidateListCount(_himc:HIMC; lpdwListCount:LPDWORD):DWORD; external ImmDLL name 'ImmGetCandidateListCountA';
{$ENDIF UNICODE}

function ImmGetCandidateListW(_himc:HIMC; deIndex:DWORD; lpCandList:LPCANDIDATELIST; dwBufLen:DWORD):DWORD; external ImmDLL name 'ImmGetCandidateListW'; // 453
{$IFDEF UNICODE}
function ImmGetCandidateList(_himc:HIMC; deIndex:DWORD; lpCandList:LPCANDIDATELIST; dwBufLen:DWORD):DWORD; external ImmDLL name 'ImmGetCandidateListW'; 
{$ELSE UNICODE}
function ImmGetCandidateList(_himc:HIMC; deIndex:DWORD; lpCandList:LPCANDIDATELIST; dwBufLen:DWORD):DWORD; external ImmDLL name 'ImmGetCandidateListA';
{$ENDIF UNICODE}

function ImmGetGuideLineW(_himc:HIMC; deIndex:DWORD; lpBuf:LPWSTR; dwBufLen:DWORD):DWORD; external ImmDLL name 'ImmGetGuideLineW'; // 45B
{$IFDEF UNICODE}
function ImmGetGuideLine(_himc:HIMC; deIndex:DWORD; lpBuf:LPWSTR; dwBufLen:DWORD):DWORD; external ImmDLL name 'ImmGetGuideLineW';
{$ELSE UNICODE}
function ImmGetGuideLine(_himc:HIMC; deIndex:DWORD; lpBuf:LPSTR; dwBufLen:DWORD):DWORD; external ImmDLL name 'ImmGetGuideLineA';
{$ENDIF UNICODE}

function ImmGetConversionStatus(_himc:HIMC; lpfdwConversion:LPDWORD; lpfdwSentence:LPDWORD):BOOL; external ImmDLL name 'ImmGetConversionStatus'; // 441 ImmGetConversionStatus
function ImmSetConversionStatus(_himc:HIMC; fdwConversion:DWORD; fdwSentence:DWORD):BOOL; external ImmDLL name 'ImmSetConversionStatus'; // 446
function ImmGetOpenStatus(_himc:HIMC):BOOL; external ImmDLL name 'ImmGetOpenStatus'; // 45F
function ImmSetOpenStatus(_himc:HIMC; fOpen:BOOL):BOOL; external ImmDLL name 'ImmSetOpenStatus'; // 46F

function ImmGetCompositionFontW(_himc:HIMC; lplf:LPLOGFONTW):BOOL; external ImmDLL name 'ImmGetCompositionFontW'; // 456
{$IFDEF UNICODE}
function ImmGetCompositionFont(_himc:HIMC; lplf:LPLOGFONTW):BOOL; external ImmDLL name 'ImmGetCompositionFontW';
{$ELSE UNICODE}
function ImmGetCompositionFont(_himc:HIMC; lplf:LPLOGFONTA):BOOL; external ImmDLL name 'ImmGetCompositionFontA';
{$ENDIF UNICODE}

function ImmSetCompositionFontW(_himc:HIMC; lplf:LPLOGFONTW):BOOL; external ImmDLL name 'ImmSetCompositionFontW'; // 46A
{$IFDEF UNICODE}
function ImmSetCompositionFont(_himc:HIMC; lplf:LPLOGFONTW):BOOL; external ImmDLL name 'ImmSetCompositionFontW';
{$ELSE UNICODE}
function ImmSetCompositionFont(_himc:HIMC; lplf:LPLOGFONT):BOOL; external ImmDLL name 'ImmSetCompositionFontA';
{$ENDIF UNICODE}

function ImmConfigureIMEW(_hkl:HKL; _hwnd:HWND; dwMode:DWORD; lpData:LPVOID):BOOL; external ImmDLL name 'ImmConfigureIMEW'; // 44D
{$IFDEF UNICODE}
function ImmConfigureIME(_hkl:HKL; _hwnd:HWND; dwMode:DWORD; lpData:LPVOID):BOOL; external ImmDLL name 'ImmConfigureIMEW';
{$ELSE UNICODE}
function ImmConfigureIME(_hkl:HKL; _hwnd:HWND; dwMode:DWORD; lpData:LPVOID):BOOL; external ImmDLL name 'ImmConfigureIMEA';
{$ENDIF UNICODE}

function ImmEscapeW(_hkl:HKL; _himc:HIMC; uEscape:UINT; lpData:LPVOID):LRESULT; external ImmDLL name 'ImmEscapeW'; // 451
{$IFDEF UNICODE}
function ImmEscape(_hkl:HKL; _himc:HIMC; uEscape:UINT; lpData:LPVOID):LRESULT; external ImmDLL name 'ImmEscapeW';
{$ELSE UNICODE}
function ImmEscape(_hkl:HKL; _himc:HIMC; uEscape:UINT; lpData:LPVOID):LRESULT; external ImmDLL name 'ImmEscapeA';
{$ENDIF UNICODE}

function ImmGetConversionListW(_hkl:HKL;
                               _himc:HIMC;
                               lpSrc:LPCWSTR;
                               lpDest:LPCANDIDATELIST;
                               dwBufLen:DWORD;
                               uFlag:UINT):DWORD; external ImmDLL name 'ImmGetConversionListW'; // 458
{$IFDEF UNICODE}
function ImmGetConversionList(_hkl:HKL;
                              _himc:HIMC;
                              lpSrc:LPCWSTR;
                              lpDest:LPCANDIDATELIST;
                              dwBufLen:DWORD;
                              uFlag:UINT):DWORD; external ImmDLL name 'ImmGetConversionListW';
{$ELSE UNICODE}
function ImmGetConversionList(_hkl:HKL;
                              _himc:HIMC;
                              lpSrc:LPCSTR;
                              lpDest:LPCANDIDATELIST;
                              dwBufLen:DWORD;
                              uFlag:UINT):DWORD; external ImmDLL name 'ImmGetConversionListA';
{$ENDIF UNICODE}

function ImmNotifyIME(_himc:HIMC; dwAction:DWORD; dwIndex:DWORD; dwValue:DWORD):BOOL; external ImmDLL name 'ImmNotifyIME'; // 442

function ImmActivateLayout(hSelKL:HKL):BOOL; external ImmDLL name 'ImmActivateLayout'; // ?7BB

function ImmSendNotification:BOOL; external ImmDLL name 'ImmSendNotification'; // ?7BC

function ImmGetStatusWindowPos(_himc:HIMC; lpptPos:LPPOINT):BOOL; external ImmDLL name 'ImmGetStatusWindowPos'; // 471
function ImmSetStatusWindowPos(_himc:HIMC; lpptPos:LPPOINT):BOOL; external ImmDLL name 'ImmSetStatusWindowPos'; // 470
function ImmGetCompositionWindow(_himc:HIMC; lpCompForm:LPCOMPOSITIONFORM):BOOL; external ImmDLL name 'ImmGetCompositionWindow'; // 457
function ImmSetCompositionWindow(_himc:HIMC; lpCompForm:LPCOMPOSITIONFORM):BOOL; external ImmDLL name 'ImmSetCompositionWindow'; // 46C
function ImmGetCandidateWindow(_himc:HIMC; dwIndex:DWORD; lpCandidate:LPCANDIDATEFORM):BOOL; external ImmDLL name 'ImmGetCandidateWindow'; // 455
function ImmSetCandidateWindow(_himc:HIMC; lpCandidate:LPCANDIDATEFORM):BOOL; external ImmDLL name 'ImmSetCandidateWindow'; // 469 

function ImmIsUIMessageW(hWndIME:HWND; msg:UINT; wParam:WPARAM; lParam:LPARAM):BOOL; external ImmDLL name 'ImmIsUIMessageW'; // 462
{$IFDEF UNICODE}
function ImmIsUIMessage(hWndIME:HWND; msg:UINT; wParam:WPARAM; lParam:LPARAM):BOOL; external ImmDLL name 'ImmIsUIMessageW';
{$ELSE UNICODE}
function ImmIsUIMessage(hWndIME:HWND; msg:UINT; wParam:WPARAM; lParam:LPARAM):BOOL; external ImmDLL name 'ImmIsUIMessageA';
{$ENDIF UNICODE}

function ImmGetVirtualKey(_hwnd:HWND):UINT; external ImmDLL name 'ImmGetVirtualKey'; // 478

type
     REGISTERWORDENUMPROCA = function(lpszReading:LPCSTR; dwStyle:DWORD; lpszString:LPCSTR; lpData:LPVOID):longint; cdecl;
     REGISTERWORDENUMPROCW = function(lpszReading:LPCWSTR; dwStyle:DWORD; lpszString:LPCWSTR; lpData:LPVOID):longint; cdecl;
{$IFDEF UNICODE}
     REGISTERWORDENUMPROC = REGISTERWORDENUMPROCW;
{$ELSE UNICODE}
     REGISTERWORDENUMPROC = REGISTERWORDENUMPROCA;
{$ENDIF UNICODE}

function ImmRegisterWordW(_hkl:HKL; lpszReading:LPCWSTR; dwStyle:DWORD; lpszRegister:LPCWSTR):BOOL; external ImmDLL name 'ImmRegisterWordW'; // 466
{$IFDEF UNICODE}
function ImmRegisterWord(_hkl:HKL; lpszReading:LPCWSTR; dwStyle:DWORD; lpszRegister:LPCWSTR):BOOL; external ImmDLL name 'ImmRegisterWordW';
{$ELSE UNICODE}
function ImmRegisterWord(_hkl:HKL; lpszReading:LPCSTR; dwStyle:DWORD; lpszRegister:LPCSTR):BOOL; external ImmDLL name 'ImmRegisterWordA';
{$ENDIF UNICODE}

function ImmUnregisterWordW(_hkl:HKL; lpszReading:LPCWSTR; dwStyle:DWORD; lpszUnregister:LPCWSTR):BOOL; external ImmDLL name 'ImmUnregisterWordW'; // 475 ImmUnregisterWordW
{$IFDEF UNICODE}
function ImmUnregisterWord(_hkl:HKL; lpszReading:LPCWSTR; dwStyle:DWORD; lpszUnregister:LPCWSTR):BOOL; external ImmDLL name 'ImmUnregisterWordW';
{$ELSE UNICODE}
function ImmUnregisterWord(_hkl:HKL; lpszReading:LPCSTR; dwStyle:DWORD; lpszUnregister:LPCSTR):BOOL; external ImmDLL name 'ImmUnregisterWordA';
{$ENDIF UNICODE}

function ImmGetRegisterWordStyleW(_hkl:HKL; nItem:UINT; _lpStyleBuf:LPSTYLEBUFW):UINT; external ImmDLL name 'ImmGetRegisterWordStyleW'; // 461
{$IFDEF UNICODE}
function ImmGetRegisterWordStyle(_hkl:HKL; nItem:UINT; _lpStyleBuf:LPSTYLEBUFW):UINT; external ImmDLL name 'ImmGetRegisterWordStyleW';
{$ELSE UNICODE}
function ImmGetRegisterWordStyle(_hkl:HKL; nItem:UINT; _lpStyleBuf:LPSTYLEBUFA):UINT; external ImmDLL name 'ImmGetRegisterWordStyleA';
{$ENDIF UNICODE}

function ImmEnumRegisterWordW(_hkl:HKL;
                              lpfnEnumProc:REGISTERWORDENUMPROCW;
                              lpszReading:LPCWSTR;
                              dwStyle:DWORD;
                              lpszRegister:LPCWSTR;
                              lpData:LPVOID):UINT; external ImmDLL name 'ImmEnumRegisterWordW'; // 450
{$IFDEF UNICODE}
function ImmEnumRegisterWord(_hkl:HKL;
                             lpfnEnumProc:REGISTERWORDENUMPROCW;
                             lpszReading:LPCWSTR;
                             dwStyle:DWORD;
                             lpszRegister:LPCWSTR;
                             lpData:LPVOID):UINT; external ImmDLL name 'ImmEnumRegisterWordW';
{$ELSE UNICODE}
function ImmEnumRegisterWord(_hkl:HKL;
                             lpfnEnumProc:REGISTERWORDENUMPROCA;
                             lpszReading:LPCSTR;
                             dwStyle:DWORD;
                             lpszRegister:LPCSTR;
                             lpData:LPVOID):UINT; external ImmDLL name 'ImmEnumRegisterWordA';
{$ENDIF UNICODE}

function ImmDisableIME(idThread:DWORD):BOOL; external ImmDLL name 'ImmDisableIME'; // 443

{$IFNDEF WINCE}
function ImmGetImeMenuItemsA(_himc:HIMC;
                             dwFlags:DWORD;
                             dwType:DWORD;
                             lpImeParentMenu:LPIMEMENUITEMINFOA;
                             lpImeMenu:LPIMEMENUITEMINFOA;
                             dwSize:DWORD):DWORD; external ImmDLL name 'ImmGetImeMenuItemsA';
{$ENDIF WINCE}

function ImmGetImeMenuItemsW(_himc:HIMC;
                             dwFlags:DWORD;
                             dwType:DWORD;
                             lpImeParentMenu:LPIMEMENUITEMINFOW;
                             lpImeMenu:LPIMEMENUITEMINFOW;
                             dwSize:DWORD):DWORD; external ImmDLL name 'ImmGetImeMenuItemsW'; // 479
{$IFDEF UNICODE}
function ImmGetImeMenuItems(_himc:HIMC;
                            dwFlags:DWORD;
                            dwType:DWORD;
                            lpImeParentMenu:LPIMEMENUITEMINFO;
                            lpImeMenu:LPIMEMENUITEMINFO;
                            dwSize:DWORD):DWORD; external ImmDLL name 'ImmGetImeMenuItemsW';
{$ELSE UNICODE}
function ImmGetImeMenuItems(_himc:HIMC;
                            dwFlags:DWORD;
                            dwType:DWORD;
                            lpImeParentMenu:LPIMEMENUITEMINFO;
                            lpImeMenu:LPIMEMENUITEMINFO;
                            dwSize:DWORD):DWORD; external ImmDLL name 'ImmGetImeMenuItemsA';
{$ENDIF UNICODE}


// wParam for WM_IME_CONTROL
const
      IMC_GETCANDIDATEPOS             = $0007;
      IMC_SETCANDIDATEPOS             = $0008;
      IMC_GETCOMPOSITIONFONT          = $0009;
      IMC_SETCOMPOSITIONFONT          = $000A;
      IMC_GETCOMPOSITIONWINDOW        = $000B;
      IMC_SETCOMPOSITIONWINDOW        = $000C;
      IMC_GETSTATUSWINDOWPOS          = $000F;
      IMC_SETSTATUSWINDOWPOS          = $0010;
      IMC_CLOSESTATUSWINDOW           = $0021;
      IMC_OPENSTATUSWINDOW            = $0022;



// dwAction for ImmNotifyIME
const
      NI_OPENCANDIDATE                = $0010;
      NI_CLOSECANDIDATE               = $0011;
      NI_SELECTCANDIDATESTR           = $0012;
      NI_CHANGECANDIDATELIST          = $0013;
      NI_FINALIZECONVERSIONRESULT     = $0014;
      NI_COMPOSITIONSTR               = $0015;
      NI_SETCANDIDATE_PAGESTART       = $0016;
      NI_SETCANDIDATE_PAGESIZE        = $0017;


// lParam for WM_IME_SETCONTEXT
      ISC_SHOWUICANDIDATEWINDOW       = $00000001;
      ISC_SHOWUICOMPOSITIONWINDOW     = $80000000;
      ISC_SHOWUIGUIDELINE             = $40000000;
      ISC_SHOWUIALLCANDIDATEWINDOW    = $0000000F;
      ISC_SHOWUIALL                   = $C000000F;


// dwIndex for ImmNotifyIME/NI_COMPOSITIONSTR
      CPS_COMPLETE                    = $0001;
      CPS_CONVERT                     = $0002;
      CPS_REVERT                      = $0003;
      CPS_CANCEL                      = $0004;


// the modifiers of hot key
      MOD_ALT                         = $0001;
      MOD_CONTROL                     = $0002;
      MOD_SHIFT                       = $0004;


      MOD_LEFT                        = $8000;
      MOD_RIGHT                       = $4000;

      MOD_ON_KEYUP                    = $0800;
      MOD_IGNORE_ALL_MODIFIER         = $0400;


// Windows for Simplified Chinese Edition hot key ID from 0x10 - 0x2F
      IME_CHOTKEY_FIRST						                 = $10;
      IME_CHOTKEY_IME_NONIME_TOGGLE           = $10;
      IME_CHOTKEY_SHAPE_TOGGLE                = $11;
      IME_CHOTKEY_SYMBOL_TOGGLE               = $12;
      IME_CHOTKEY_LAST						                  = $2f;

// Windows for Japanese Edition hot key ID from 0x30 - 0x4F
      IME_JHOTKEY_FIRST						                 = $30;
      IME_JHOTKEY_CLOSE_OPEN                  = $30;
      IME_JHOTKEY_LAST						                  = $4f;

// Windows for Korean Edition hot key ID from 0x50 - 0x6F
      IME_KHOTKEY_FIRST						                 = $50;
      IME_KHOTKEY_SHAPE_TOGGLE                = $50;
      IME_KHOTKEY_HANJACONVERT                = $51;
      IME_KHOTKEY_ENGLISH                     = $52;
      IME_KHOTKEY_LAST						                  = $6f;

// Windows for Traditional Chinese Edition hot key ID from 0x70 - 0x8F
      IME_THOTKEY_FIRST						                 = $70;
      IME_THOTKEY_IME_NONIME_TOGGLE           = $70;
      IME_THOTKEY_SHAPE_TOGGLE                = $71;
      IME_THOTKEY_SYMBOL_TOGGLE               = $72;
      IME_THOTKEY_LAST						                  = $8f;

// direct switch hot key ID from 0x100 - 0x11F
      IME_HOTKEY_DSWITCH_FIRST                = $100;
      IME_HOTKEY_DSWITCH_LAST                 = $11F;

// IME private hot key from 0x200 - 0x21F
      IME_HOTKEY_PRIVATE_FIRST                = $200;
      IME_ITHOTKEY_RESEND_RESULTSTR           = $200;
      IME_ITHOTKEY_PREVIOUS_COMPOSITION       = $201;
      IME_ITHOTKEY_UISTYLE_TOGGLE             = $202;
      IME_HOTKEY_PRIVATE_LAST                 = $21F;


// dwSystemInfoFlags bits

// parameter of ImmGetCompositionString
      GCS_COMPREADSTR                 = $0001;
      GCS_COMPREADATTR                = $0002;
      GCS_COMPREADCLAUSE              = $0004;
      GCS_COMPSTR                     = $0008;
      GCS_COMPATTR                    = $0010;
      GCS_COMPCLAUSE                  = $0020;
      GCS_CURSORPOS                   = $0080;
      GCS_DELTASTART                  = $0100;
      GCS_RESULTREADSTR               = $0200;
      GCS_RESULTREADCLAUSE            = $0400;
      GCS_RESULTSTR                   = $0800;
      GCS_RESULTCLAUSE                = $1000;

// style bit flags for WM_IME_COMPOSITION
      CS_INSERTCHAR                   = $2000;
      CS_NOMOVECARET                  = $4000;



// bits of fdwInit of INPUTCONTEXT
// IME version constants
      IMEVER_0310                     = $0003000A;
      IMEVER_0400                     = $00040000;


// IME property bits
      IME_PROP_END_UNLOAD             = $00000001;
      IME_PROP_KBD_CHAR_FIRST         = $00000002;
      IME_PROP_IGNORE_UPKEYS          = $00000004;
      IME_PROP_NEED_ALTKEY            = $00000008;
      IME_PROP_NO_KEYS_ON_CLOSE       = $00000010;
      IME_PROP_AT_CARET               = $00010000;
      IME_PROP_SPECIAL_UI             = $00020000;
      IME_PROP_CANDLIST_START_FROM_1  = $00040000;
      IME_PROP_UNICODE                = $00080000;
      IME_PROP_COMPLETE_ON_UNSELECT   = $00100000;
// IME property bits, anyone adding a new bit must update this
      IME_PROP_ALL                    = $001F001F;


// IME UICapability bits
      UI_CAP_2700                     = $00000001;
      UI_CAP_ROT90                    = $00000002;
      UI_CAP_ROTANY                   = $00000004;


// ImmSetCompositionString Capability bits
      SCS_CAP_COMPSTR                 = $00000001;
      SCS_CAP_MAKEREAD                = $00000002;
      SCS_CAP_SETRECONVERTSTRING      = $00000004;


// IME WM_IME_SELECT inheritance Capability bits
      SELECT_CAP_CONVERSION           = $00000001;
      SELECT_CAP_SENTENCE             = $00000002;


// ID for deIndex of ImmGetGuideLine
      GGL_LEVEL                       = $00000001;
      GGL_INDEX                       = $00000002;
      GGL_STRING                      = $00000003;
      GGL_PRIVATE                     = $00000004;


// ID for dwLevel of GUIDELINE Structure
      GL_LEVEL_NOGUIDELINE            = $00000000;
      GL_LEVEL_FATAL                  = $00000001;
      GL_LEVEL_ERROR                  = $00000002;
      GL_LEVEL_WARNING                = $00000003;
      GL_LEVEL_INFORMATION            = $00000004;


// ID for dwIndex of GUIDELINE Structure
      GL_ID_UNKNOWN                   = $00000000;
      GL_ID_NOMODULE                  = $00000001;
      GL_ID_NODICTIONARY              = $00000010;
      GL_ID_CANNOTSAVE                = $00000011;
      GL_ID_NOCONVERT                 = $00000020;
      GL_ID_TYPINGERROR               = $00000021;
      GL_ID_TOOMANYSTROKE             = $00000022;
      GL_ID_READINGCONFLICT           = $00000023;
      GL_ID_INPUTREADING              = $00000024;
      GL_ID_INPUTRADICAL              = $00000025;
      GL_ID_INPUTCODE                 = $00000026;
      GL_ID_INPUTSYMBOL               = $00000027;
      GL_ID_CHOOSECANDIDATE           = $00000028;
      GL_ID_REVERSECONVERSION         = $00000029;
      GL_ID_PRIVATE_FIRST             = $00008000;
      GL_ID_PRIVATE_LAST              = $0000FFFF;


// ID for dwIndex of ImmGetProperty
      IGP_GETIMEVERSION               = DWORD(-4);
      IGP_PROPERTY                    = $00000004;
      IGP_CONVERSION                  = $00000008;
      IGP_SENTENCE                    = $0000000c;
      IGP_UI                          = $00000010;
      IGP_SETCOMPSTR                  = $00000014;
      IGP_SELECT                      = $00000018;
      IGP_PRIVATEDATASIZE				         = $0000001c;
// last property index, anyone adding a new property index must update this
      IGP_LAST                        = IGP_PRIVATEDATASIZE;


// dwIndex for ImmSetCompositionString API
      SCS_SETSTR                      = GCS_COMPREADSTR or GCS_COMPSTR;
      SCS_CHANGEATTR                  = GCS_COMPREADATTR or GCS_COMPATTR;
      SCS_CHANGECLAUSE                = GCS_COMPREADCLAUSE or GCS_COMPCLAUSE;
      SCS_SETRECONVERTSTRING          = $00010000;
      SCS_QUERYRECONVERTSTRING        = $00020000;


// attribute for COMPOSITIONSTRING Structure
      ATTR_INPUT                      = $00;
      ATTR_TARGET_CONVERTED           = $01;
      ATTR_CONVERTED                  = $02;
      ATTR_TARGET_NOTCONVERTED        = $03;
      ATTR_INPUT_ERROR                = $04;


// bit field for IMC_SETCOMPOSITIONWINDOW, IMC_SETCANDIDATEWINDOW
      CFS_DEFAULT                     = $0000;
      CFS_RECT                        = $0001;
      CFS_POINT                       = $0002;
      CFS_FORCE_POSITION              = $0020;
      CFS_CANDIDATEPOS                = $0040;
      CFS_EXCLUDE                     = $0080;


// conversion direction for ImmGetConversionList
      GCL_CONVERSION                  = $0001;
      GCL_REVERSECONVERSION           = $0002;
      GCL_REVERSE_LENGTH              = $0003;


// bit field for conversion mode
      IME_CMODE_ALPHANUMERIC          = $0000;
      IME_CMODE_NATIVE                = $0001;
      IME_CMODE_CHINESE               = IME_CMODE_NATIVE;
// IME_CMODE_HANGEUL is old name of IME_CMODE_HANGUL. It will be gone eventually.
      IME_CMODE_HANGEUL               = IME_CMODE_NATIVE;
      IME_CMODE_HANGUL                = IME_CMODE_NATIVE;
      IME_CMODE_JAPANESE              = IME_CMODE_NATIVE;
      IME_CMODE_KATAKANA              = $0002;  // only effect under IME_CMODE_NATIVE
      IME_CMODE_LANGUAGE              = $0003;
      IME_CMODE_FULLSHAPE             = $0008;
      IME_CMODE_ROMAN                 = $0010;
      IME_CMODE_CHARCODE              = $0020;
      IME_CMODE_HANJACONVERT          = $0040;
      IME_CMODE_SOFTKBD               = $0080;
      IME_CMODE_NOCONVERSION          = $0100;
      IME_CMODE_EUDC                  = $0200;
      IME_CMODE_SYMBOL                = $0400;
      IME_CMODE_PASSWORD				          = $80000000;


      IME_SMODE_NONE                  = $0000;
      IME_SMODE_PLAURALCLAUSE         = $0001;
      IME_SMODE_SINGLECONVERT         = $0002;
      IME_SMODE_AUTOMATIC             = $0004;
      IME_SMODE_PHRASEPREDICT         = $0008;


// style of candidate
      IME_CAND_UNKNOWN                = $0000;
      IME_CAND_READ                   = $0001;
      IME_CAND_CODE                   = $0002;
      IME_CAND_MEANING                = $0003;
      IME_CAND_RADICAL                = $0004;
      IME_CAND_STROKE                 = $0005;


// wParam of report message WM_IME_NOTIFY
      IMN_CLOSESTATUSWINDOW           = $0001;
      IMN_OPENSTATUSWINDOW            = $0002;
      IMN_CHANGECANDIDATE             = $0003;
      IMN_CLOSECANDIDATE              = $0004;
      IMN_OPENCANDIDATE               = $0005;
      IMN_SETCONVERSIONMODE           = $0006;
      IMN_SETSENTENCEMODE             = $0007;
      IMN_SETOPENSTATUS               = $0008;
      IMN_SETCANDIDATEPOS             = $0009;
      IMN_SETCOMPOSITIONFONT          = $000A;
      IMN_SETCOMPOSITIONWINDOW        = $000B;
      IMN_SETSTATUSWINDOWPOS          = $000C;
      IMN_GUIDELINE                   = $000D;
      IMN_PRIVATE                     = $000E;


// error code of ImmGetCompositionString
      IMM_ERROR_NODATA                = -1;
      IMM_ERROR_GENERAL               = -2;


// dialog mode of ImmConfigureIME
      IME_CONFIG_GENERAL              = 1;
      IME_CONFIG_REGISTERWORD         = 2;
      IME_CONFIG_SELECTDICTIONARY     = 3;


// dialog mode of ImmEscape
      IME_ESC_QUERY_SUPPORT           = $0003;
      IME_ESC_RESERVED_FIRST          = $0004;
      IME_ESC_RESERVED_LAST           = $07FF;
      IME_ESC_PRIVATE_FIRST           = $0800;
      IME_ESC_PRIVATE_LAST            = $0FFF;
      IME_ESC_SEQUENCE_TO_INTERNAL    = $1001;
      IME_ESC_GET_EUDC_DICTIONARY     = $1003;
      IME_ESC_SET_EUDC_DICTIONARY     = $1004;
      IME_ESC_MAX_KEY                 = $1005;
      IME_ESC_IME_NAME                = $1006;
      IME_ESC_SYNC_HOTKEY             = $1007;
      IME_ESC_HANJA_MODE              = $1008;
      IME_ESC_AUTOMATA                = $1009;
      IME_ESC_PRIVATE_HOTKEY          = $100a;


// style of word registration
      IME_REGWORD_STYLE_EUDC          = $00000001;
      IME_REGWORD_STYLE_USER_FIRST    = $80000000;
      IME_REGWORD_STYLE_USER_LAST     = $FFFFFFFF;

// dwFlags for ImmAssociateContextEx
      IACE_CHILDREN                   = $0001;
      IACE_DEFAULT                    = $0010;
      IACE_IGNORENOCONTEXT            = $0020;

// dwFlags for ImmGetImeMenuItems
      IGIMIF_RIGHTMENU                = $0001;

// dwType for ImmGetImeMenuItems
      IGIMII_CMODE                    = $0001;
      IGIMII_SMODE                    = $0002;
      IGIMII_CONFIGURE                = $0004;
      IGIMII_TOOLS                    = $0008;
      IGIMII_HELP                     = $0010;
      IGIMII_OTHER                    = $0020;
      IGIMII_INPUTTOOLS               = $0040;

// fType of IMEMENUITEMINFO structure
      IMFT_RADIOCHECK = $00001;
      IMFT_SEPARATOR  = $00002;
      IMFT_SUBMENU    = $00004;

// fState of IMEMENUITEMINFO structure
      IMFS_GRAYED          = MF_GRAYED;
      IMFS_DISABLED        = MFS_DISABLED;
      IMFS_CHECKED         = MFS_CHECKED;
      IMFS_HILITE          = MFS_HILITE;
      IMFS_ENABLED         = MFS_ENABLED;
      IMFS_UNCHECKED       = MFS_UNCHECKED;
      IMFS_UNHILITE        = MFS_UNHILITE;
      IMFS_DEFAULT         = MFS_DEFAULT;



// type of soft keyboard
// for Windows Traditional Chinese Edition
      SOFTKEYBOARD_TYPE_T1            = $0001;
// for Windows Simplified Chinese Edition
      SOFTKEYBOARD_TYPE_C1            = $0002;



//	Windows CE immp.h merged into imm.h

type
     tagCOMPOSITIONSTRING = record
       dwSize:DWORD;
       dwCompReadAttrLen:DWORD;
       dwCompReadAttrOffset:DWORD;
       dwCompReadClauseLen:DWORD;
       dwCompReadClauseOffset:DWORD;
       dwCompReadStrLen:DWORD;
       dwCompReadStrOffset:DWORD;
       dwCompAttrLen:DWORD;
       dwCompAttrOffset:DWORD;
       dwCompClauseLen:DWORD;
       dwCompClauseOffset:DWORD;
       dwCompStrLen:DWORD;
       dwCompStrOffset:DWORD;
       dwCursorPos:DWORD;
       dwDeltaStart:DWORD;
       dwResultReadClauseLen:DWORD;
       dwResultReadClauseOffset:DWORD;
       dwResultReadStrLen:DWORD;
       dwResultReadStrOffset:DWORD;
       dwResultClauseLen:DWORD;
       dwResultClauseOffset:DWORD;
       dwResultStrLen:DWORD;
       dwResultStrOffset:DWORD;
       dwPrivateSize:DWORD;
       dwPrivateOffset:DWORD;
     end;
     COMPOSITIONSTRING = tagCOMPOSITIONSTRING;
     PCOMPOSITIONSTRING = ^tagCOMPOSITIONSTRING;
     NPCOMPOSITIONSTRING = ^tagCOMPOSITIONSTRING;
     LPCOMPOSITIONSTRING = ^tagCOMPOSITIONSTRING;


type
     tagGUIDELINE = record
       dwSize:DWORD;
       dwLevel:DWORD;
       dwIndex:DWORD;
       dwStrLen:DWORD;
       dwStrOffset:DWORD;
       dwPrivateSize:DWORD;
       dwPrivateOffset:DWORD;
     end;
     GUIDELINE = tagGUIDELINE;
     PGUIDELINE = ^tagGUIDELINE;
     NPGUIDELINE = ^tagGUIDELINE;
     LPGUIDELINE = ^tagGUIDELINE;

type
     tagRECONVERTSTRING = record
       dwSize:DWORD;
       dwVersion:DWORD;
       dwStrLen:DWORD;
       dwStrOffset:DWORD;
       dwCompStrLen:DWORD;
       dwCompStrOffset:DWORD;
       dwTargetStrLen:DWORD;
       dwTargetStrOffset:DWORD;
     end;
     RECONVERTSTRING = tagRECONVERTSTRING;
     PRECONVERTSTRING = ^tagRECONVERTSTRING;
     NPRECONVERTSTRING = ^tagRECONVERTSTRING;
     LPRECONVERTSTRING = ^tagRECONVERTSTRING;

type
     tagCANDIDATEINFO = record
       dwSize:DWORD;
       dwCount:DWORD;
       dwOffset:array[0..31] of DWORD;
       dwPrivateSize:DWORD;
       dwPrivateOffset:DWORD;
     end;
     CANDIDATEINFO = tagCANDIDATEINFO;
     PCANDIDATEINFO = ^tagCANDIDATEINFO;
     NPCANDIDATEINFO = ^tagCANDIDATEINFO;
     LPCANDIDATEINFO = ^tagCANDIDATEINFO;


type
     INPUTCONTEXTLOGFONT = record
       case longint of
         0: (A:LOGFONTA);
         1: (W:LOGFONTW);
     end;
     tagINPUTCONTEXT = record
       _hwnd:HWND;
       fOpen:BOOL;
					  fdwClient:DWORD;		//	Windows CE addition.
	      hwndImeInUse:HWND;	//	Windows CE addition.
       ptStatusWndPos:POINT;
       ptSoftKbdPos:POINT;
       fdwConversion:DWORD;
       fdwSentence:DWORD;
       lfFont:INPUTCONTEXTLOGFONT;
(*
    union   {
        LOGFONTA        A;
        LOGFONTW        W;
    } lfFont;
*)
       cfCompForm:COMPOSITIONFORM;
       cfCandForm:array[0..3] of CANDIDATEFORM;
       hCompStr:HIMCC;
       hCandInfo:HIMCC;
       hGuideLine:HIMCC;
       hPrivate:HIMCC;
       dwNumMsgBuf:DWORD;
       hMsgBuf:HIMCC;
       fdwInit:DWORD;
       dwReserve:array[0..2] of DWORD;
       uSavedVKey:UINT;
       fChgMsg:BOOL;
       fdwFlags:DWORD;
       fdw31Compat:DWORD;
       dwRefCount:DWORD;

       pImeModeSaver:PVOID;
       fdwDirty:DWORD;
     end;
     INPUTCONTEXT = tagINPUTCONTEXT;
     PINPUTCONTEXT = ^tagINPUTCONTEXT;
     NPINPUTCONTEXT = ^tagINPUTCONTEXT;
     LPINPUTCONTEXT = ^tagINPUTCONTEXT;


type
     tagIMEINFO = record
       dwPrivateDataSize:DWORD;
       fdwProperty:DWORD;
       fdwConversionCaps:DWORD;
       fdwSentenceCaps:DWORD;
       fdwUICaps:DWORD;
       fdwSCSCaps:DWORD;
       fdwSelectCaps:DWORD;
     end;
     IMEINFO = tagIMEINFO;
     PIMEINFO = ^tagIMEINFO;
     NPIMEINFO = ^tagIMEINFO;
     LPIMEINFO = ^tagIMEINFO;


type
     tagSOFTKBDDATA = record
       uCount:UINT;
       wCode:array[0..0,0..255] of word;
     end;
     SOFTKBDDATA = tagSOFTKBDDATA;
     PSOFTKBDDATA = ^tagSOFTKBDDATA;
     NPSOFTKBDDATA = ^tagSOFTKBDDATA;
     LPSOFTKBDDATA = ^tagSOFTKBDDATA;


function ImmGetHotKey(dwHotKeyID:DWORD; lpuModifiers:LPUINT; lpuVKey:LPUINT; _lphkl:LPHKL):BOOL; external ImmDLL name 'ImmGetHotKey'; // 46E
function ImmSetHotKey(dwHotKeyID:DWORD; uModifiers:UINT; uVKey:UINT; _hkl:HKL):BOOL; external ImmDLL name 'ImmSetHotKey'; // 46D 
function ImmGenerateMessage(_himc:HIMC):BOOL; external ImmDLL name 'ImmGenerateMessage'; // 452 

{$IFNDEF WINCE}
function ImmRequestMessageA(HIMC, WPARAM, LPARAM):LRESULT; external ImmDLL name 'ImmRequestMessageA';
{$ENDIF WINCE}
function ImmRequestMessageW(_himc:HIMC; wParam:WPARAM; lParam:LPARAM):LRESULT; external ImmDLL name 'ImmRequestMessageW'; // 480
{$IFDEF UNICODE}
function ImmRequestMessage(_himc:HIMC; wParam:WPARAM; lParam:LPARAM):LRESULT; external ImmDLL name 'ImmRequestMessageW';
{$ELSE UNICODE}
function ImmRequestMessage(_himc:HIMC; wParam:WPARAM; lParam:LPARAM):LRESULT; external ImmDLL name 'ImmRequestMessageA';
{$ENDIF UNICODE}

//
// Prototype of soft keyboard APIs
//

{$IFNDEF WINCE}
function ImmCreateSoftKeyboard(param1:UINT; _hwnd:HWND; paran3:longint; param4:longint):HWND; external ImmDLL name 'ImmCreateSoftKeyboard';
function ImmDestroySoftKeyboard(_hwnd:HWND):BOOL; external ImmDLL name 'ImmDestroySoftKeyboard';
function ImmShowSoftKeyboard(_hwnd:HWND; param2:longint):BOOL; external ImmDLL name 'ImmShowSoftKeyboard';
{$ENDIF WINCE}

function ImmLockIMC(_himc:HIMC):LPINPUTCONTEXT; external ImmDLL name 'ImmLockIMC'; // 463
function ImmUnlockIMC(_himc:HIMC):BOOL; external ImmDLL name 'ImmUnlockIMC'; // 473 
function ImmGetIMCLockCount(_himc:HIMC):DWORD; external ImmDLL name 'ImmGetIMCLockCount'; // 45E 

function ImmCreateIMCC(dwSize:DWORD):HIMCC; external ImmDLL name 'ImmCreateIMCC'; // 44E 
function ImmDestroyIMCC(_himcc:HIMCC):HIMCC; external ImmDLL name 'ImmDestroyIMCC'; // 44F
function ImmLockIMCC(_himcc:HIMCC):LPVOID; external ImmDLL name 'ImmLockIMCC'; // 464
function ImmUnlockIMCC(_himcc:HIMCC):BOOL; external ImmDLL name 'ImmUnlockIMCC'; // 474
function ImmGetIMCCLockCount(_himcc:HIMCC):DWORD; external ImmDLL name 'ImmGetIMCCLockCount'; // 45C
function ImmReSizeIMCC(_himcc:HIMCC; dwSize:DWORD):HIMCC; external ImmDLL name 'ImmReSizeIMCC'; // 465
function ImmGetIMCCSize(_himcc:HIMCC):DWORD; external ImmDLL name 'ImmGetIMCCSize'; // 45D


const
// the window extra offset
      IMMGWL_IMC                      = 0;
      IMMGWL_PRIVATE                  = SizeOf(LONG);


// 0x11 - 0x20 is reserved for soft keyboard
// wParam for WM_IME_SYSTEM
      IMS_DESTROYWINDOW               = $0001;
      IMS_IME31COMPATIBLE             = $0002;
      IMS_SETOPENSTATUS               = $0003;
      IMS_SETACTIVECONTEXT            = $0004;
      IMS_CHANGE_SHOWSTAT             = $0005;
      IMS_WINDOWPOS                   = $0006;

      IMS_SENDIMEMSG                  = $0007;
      IMS_SENDIMEMSGEX                = $0008;
      IMS_SETCANDIDATEPOS             = $0009;
      IMS_SETCOMPOSITIONFONT          = $000A;
      IMS_SETCOMPOSITIONWINDOW        = $000B;
      IMS_CHECKENABLE                 = $000C;
      IMS_CONFIGUREIME                = $000D;
      IMS_CONTROLIMEMSG               = $000E;
      IMS_SETOPENCLOSE                = $000F;
      IMS_ISACTIVATED                 = $0010;
      IMS_UNLOADTHREADLAYOUT          = $0011;
      IMS_LCHGREQUEST                 = $0012;
      IMS_SETSOFTKBDONOFF             = $0013;
      IMS_GETCONVERSIONMODE           = $0014;
      IMS_IMEHELP                     = $0015;

      IMS_IMENT35SENDAPPMSG           = $0016;
      IMS_ACTIVATECONTEXT             = $0017;
      IMS_DEACTIVATECONTEXT           = $0018;
      IMS_ACTIVATETHREADLAYOUT        = $0019;
      IMS_CLOSEPROPERTYWINDOW         = $001a;
      IMS_OPENPROPERTYWINDOW          = $001b;

      IMS_GETIMEMENU                  = $001c;
      IMS_ENDIMEMENU                  = $001d;

      IMS_SENDNOTIFICATION            = $001f;
// IMS_SENDNOTIFICATION dirty bits for INPUTCONTEXT
      IMSS_UPDATE_OPEN               	= $0001;
      IMSS_UPDATE_CONVERSION         	= $0002;
      IMSS_UPDATE_SENTENCE           	= $0004;
      IMSS_INIT_OPEN                 	= $0100;

// These two msgs are CE only & on desktop IMS_XXX msgs range till
// 0x0024 - so these two msgs are defined from 0x0030.
      IMS_SETCONVERSIONSTATUS         = $0030;
      IMS_SETSENTENCEMODE             = $0031;

// for NI_CONTEXTUPDATED
      IMC_GETCONVERSIONMODE           = $0001;
      IMC_SETCONVERSIONMODE           = $0002;
      IMC_GETSENTENCEMODE             = $0003;
      IMC_SETSENTENCEMODE             = $0004;
      IMC_GETOPENSTATUS               = $0005;
      IMC_SETOPENSTATUS               = $0006;
// wParam for WM_IME_CONTROL to the soft keyboard
      IMC_GETSOFTKBDFONT              = $0011;
      IMC_SETSOFTKBDFONT              = $0012;
      IMC_GETSOFTKBDPOS               = $0013;
      IMC_SETSOFTKBDPOS               = $0014;
      IMC_GETSOFTKBDSUBTYPE           = $0015;
      IMC_SETSOFTKBDSUBTYPE           = $0016;
      IMC_SETSOFTKBDDATA              = $0018;


      NI_CONTEXTUPDATED               = $0003;
// the return bits of ImmProcessHotKey
      IPHK_HOTKEY                     = $0001;
      IPHK_PROCESSBYIME               = $0002;
      IPHK_CHECKCTRL                  = $0004;
// NT only
      IPHK_SKIPTHISKEY                = $0010;


      MOD_WIN                         = $0008;
      IME_INVALID_HOTKEY              = $ffffffff;
      IME_SYSINFO_WINLOGON            = $0001;
      IME_SYSINFO_WOW16               = $0002;
      GCS_COMP                        = GCS_COMPSTR or GCS_COMPATTR or GCS_COMPCLAUSE;
      GCS_COMPREAD                    = GCS_COMPREADSTR or GCS_COMPREADATTR or GCS_COMPREADCLAUSE;
      GCS_RESULT                      = GCS_RESULTSTR or GCS_RESULTCLAUSE;
      GCS_RESULTREAD                  = GCS_RESULTREADSTR or GCS_RESULTREADCLAUSE;
      INIT_STATUSWNDPOS               = $00000001;
      INIT_CONVERSION                 = $00000002;
      INIT_SENTENCE                   = $00000004;
      INIT_LOGFONT                    = $00000008;
      INIT_COMPFORM                   = $00000010;
      INIT_SOFTKBDPOS                 = $00000020;


// fdw31Compat of INPUTCONTEXT
      F31COMPAT_NOKEYTOIME     = $00000001;
      F31COMPAT_MCWHIDDEN      = $00000002;
      F31COMPAT_MCWVERTICAL    = $00000004;
      F31COMPAT_CALLFROMWINNLS = $00000008;
      F31COMPAT_SAVECTRL       = $00010000;
      F31COMPAT_PROCESSEVENT   = $00020000;
      F31COMPAT_ECSETCFS       = $00040000;


// the return value of ImmGetAppIMECompatFlags
      IMECOMPAT_UNSYNC31IMEMSG = $00000001;
// the meaning of this bit depend on the same bit in
// IMELinkHdr.ctCountry.fdFlags
      IMECOMPAT_DUMMYTASK      = $00000002;
// For Japanese and Hangeul versions, this bit on
// indicates no dummy task is needed
      IMECOMPAT_NODUMMYTASK    = IMECOMPAT_DUMMYTASK;
// For Chinese and PRC versions, this bit on indicates
// a dummy tasked is needed
      IMECOMPAT_NEEDDUMMYTASK         = IMECOMPAT_DUMMYTASK;
      IMECOMPAT_POSTDUMMY             = $00000004;
      IMECOMPAT_ECNOFLUSH             = $00000008;
      IMECOMPAT_NOINPUTLANGCHGTODLG   = $00000010;
      IMECOMPAT_ECREDRAWPARENT        = $00000020;
      IMECOMPAT_SENDOLDSBM            = $00000040;
      IMECOMPAT_UNSYNC31IMEMSG2       = $00000080;
      IMECOMPAT_NOIMEMSGINTERTASK     = $00000100;
      IMECOMPAT_USEXWANSUNG           = $00000200;
      IMECOMPAT_JXWFORATOK            = $00000400;
      IMECOMPAT_NOIME                 = $00000800;
      IMECOMPAT_NOKBDHOOK             = $00001000;
      IMECOMPAT_APPWNDREMOVEIMEMSGS   = $00002000;
      IMECOMPAT_LSTRCMP31COMPATIBLE   = $00004000;
      IMECOMPAT_USEALTSTKFORSHLEXEC   = $00008000;
      IMECOMPAT_NOVKPROCESSKEY        = $00010000;
      IMECOMPAT_NOYIELDWMCHAR         = $00020000;
      IMECOMPAT_SENDSC_RESTORE        = $00040000;
      IMECOMPAT_NOSENDLANGCHG         = $00080000;
      IMECOMPAT_FORCEUNSYNC31IMEMSG   = $00100000;
      IMECOMPAT_CONSOLEIMEPROCESS     = $00200000;

      IMGTF_CANT_SWITCH_LAYOUT        = $00000001;
      IMGTF_CANT_UNLOAD_IME           = $00000002;

      UI_CAP_SOFTKBD                  = $00010000;
// all IME UICapability bits, anyone adding a new bit must update this mask
      UI_CAP_ALL                      = $00010007;
// all ImmSetCompositionString Capability bits, anyone adding a new bit must update this mask
      SCS_CAP_ALL                     = $00000007;
// all IME WM_IME_SELECT inheritance Capability bits, anyone adding a new bit must update this mask
      SELECT_CAP_ALL                  = $00000003;
      CFS_SCREEN                      = $0004;
      CFS_VERTICAL                    = $0008;
      CFS_HIDDEN                      = $0010;
// all conversion mode bits, anyone adding a new bit must update this mask
      IME_CMODE_ALL                   = $0FFF;
// all sentence mode bits, anyone adding a new bit must update this mask
      IME_SMODE_ALL                   = $001F;
      IMN_SOFTKBDDESTROYED            = $0011;



{$IFNDEF WINCE}
// protype of IME APIs
function ImeInquire(param1:LPIMEINFO; lpszUIClass:LPWSTR; dwSystemInfoFlags:DWORD):BOOL;
function ImeConfigure(param1:HKL; param2:HWND; param3:DWORD; param4:LPVOID):BOOL;
function ImeConversionListW(param1:HIMC; param2:LPCWSTR; param3:LPCANDIDATELIST; dwBufLen:DWORD; uFlag:UINT):DWORD;
function ImeDestroy(param1:UINT):BOOL;
function ImeEscape(param1:HIMC; param2:UINT; param3:LPVOID):LRESULT;
function ImeProcessKey(param1:HIMC; param2:UINT; param3:LPARAM; param4:LPBYTE):BOOL;
function ImeSelect(param1:HIMC; param2:BOOL):BOOL;
function ImeSetActiveContext(param1:HIMC; param2:BOOL):BOOL;
function ImeToAsciiEx(uVirtKey:UINT; uScaCode:UINT; lpbKeyState:LPBYTE; lpdwTransBuf:LPDWORD; fuState:UINT; _himc:HIMC):UINT;
function NotifyIME(_himc:HIMC; param2:DWORD; param3:DWORD; param4:DWORD):BOOL;
function ImeRegisterWord(param1:LPCWSTR; param2:DWORD; param3:LPCWSTR):BOOL;
function ImeUnregisterWord(param1:LPCWSTR; param2:DWORD; param3:LPCWSTR):BOOL;
function ImeGetRegisterWordStyle(nItem:UINT; param2:LPSTYLEBUF):UINT;
function ImeEnumRegisterWord(param1:REGISTERWORDENUMPROC; param2:LPCWSTR; param3:DWORD; param4:LPCWSTR; param2:LPVOID):UINT;
function ImeSetCompositionString(_himc:HIMC; dwIndex:DWORD; lpComp:LPCVOID; param4:DWORD; lpRead:LPCVOID; param6:DWORD):BOOL;
function ImeGetImeMenuItems(_himc:HIMC; dwFlags:DWORD; dwType:DWORD; lpParentMenu:LPIMEMENUITEMINFO; lpMenu:LPIMEMENUITEMINFO; dwSize:DWORD):DWORD;
{$ENDIF WINCE}



//	Windows CE additions


function ImmIsValidIMC(_himc:HIMC):BOOL; external ImmDLL name 'ImmIsValidIMC'; // ?

//*****************************************
// Messages From Parent window
//  wParam : 0
//  lParam : appropriate value for Message
//*****************************************
const
      CLM_START       = $00001000;

      CLM_SETCANDLIST    = CLM_START + 0; // lParam : LPCANDIDATELIST
      CLM_SETWNDPOS      = CLM_START + 1; // lParam : MAKELONG(POINT)
      CLM_GETCURSEL      = CLM_START + 2; // lParam : 0 ; return : current selection
      CLM_GETPAGESTART   = CLM_START + 3; // lParam : 0 ; return : current page start

// Internal for CANDLIST window
//*****************************************
// Special window styles for candidate list
//*****************************************
const
      CLS_BUTTONS     = $00000001;
      CLS_FRACTION    = $00000002;
      CLS_ACTIVATE    = $00000004;
      WS_NOTIFY       = $00000008;


//*****************************************
// Notification from Candidate list window
//  wParam for WM_CAND_NOTIFY message
//*****************************************
const
      CLN_NOTIFYCODE  = $0100;
      CLN_DETERMINE   = CLN_NOTIFYCODE + 0;
      CLN_SELECTION   = CLN_NOTIFYCODE + 1;
      CLN_PAGESTART   = CLN_NOTIFYCODE + 2;


//*****************************************
// Structure for Globaled candidate proc
//*****************************************
type
     _MYCANDLIST = record
       ParentWnd:HWND;  // Parent window handle.
       CandList:LPCANDIDATELIST;   // Candidate list structure.
     end;
     MYCANDLIST = _MYCANDLIST;


{/*
 * Handwriting pad specific structures
 */}
type
     _tagHWXRESULT = record
       cbCount:word;          // Number of candidates in the list
       iSelection:word;      // Current selection
       iPosition:word;       // cursor position
       chCandidate:array[0..0] of TCHAR;   // The array of candidates
     end;
     HWXRESULT = _tagHWXRESULT;
     PHWXRESULT = ^_tagHWXRESULT;

// Internal window style
const
      ES_COMPWND                  = $00004000;

// Internal defines and API
const
      WM_IME_CONTROL_EDIT         = $00001000;
      IMC_EDIT_CONVERT            = $00000001;
      IMC_EDIT_DETERMINE          = $00000002;
      IMC_EDIT_CANCELCONVERT      = $00000003;
      IMC_EDIT_GETSTATUS          = $00000004;
      IMC_EDIT_UNDO               = $00000005;
      IMC_EDIT_CANUNDO            = $00000006;
      IMC_EDIT_EMPTYUNDOBUFFER    = $00000007;
      IMC_EDIT_REPLACESEL         = $00000008;
      IMC_EDIT_CLEARALL           = $00000009;
      IMC_EDIT_SETIMC             = $0000000A;
      IMC_EDIT_GETCANDIDATEPOS    = $0000000B;
      IMC_EDIT_UPDATEPROP         = $0000000C;
      IMC_EDIT_HALFWIDTH          = $0000000D;
      IMC_EDIT_UPDATECLIST        = $0000000E;

// Edit control is in input mode, no IME is involved
      IMCR_INPUT                  = $0001;
// Edit control is in conversion mode.
      IMCR_CONVERT                = $0002;
// Edit control is in conversion mode, the current clauses is un-converted
// and the cursor is within current clause
      IMCR_UNCONVERT              = $0003;
// Edit control is in composition mode, but not in conversion mode.
      IMCR_COMPOSITION            = $0004;

// For set cursor posision on composition string ( Internal )
      IME_ESC_SETCURSOR           = $100A;


      SIP_QUERY_STATE             = $2000;
      SIP_SET_STATE               = $2001;
      SIP_STATE_HDKB              = $0001;
      SIP_STATE_NOHDKB			         = $0002;

      SIP_QUERY_RCMASK            = $3000;
      SIP_SET_RCMASK              = $3001;

      SIP_SET_OPENCANDWND			      = $5000;
      SIP_SET_CLOSECANDWND		      = $5001;

      SIP_QUERY_HALFWIDTH			      = $6000;
      SIP_SET_HALFWIDTH			        = $6001;
      SIP_WIDTHSETTING_HALF       = $01;

      SIP_QUERY_MODE				          = $8000;
      SIP_SET_MODE				            = $8001;
      SIP_MODE_MINIMAL		          = $0000;
      SIP_MODE_REDUCED		          = $0001;
      SIP_MODE_FULL				           = $0002;
      SIP_MODE_FULL_HIDDEN		      = $0003;

      SIP_NOTIFY_FOCUS_CHANGE     = $FFFB;
      SIP_RELEASE_STICKY_KEYS     = $FFFC;
      SIP_CONFIG_CHANGED          = $FFFD;
      SIP_SHOW_WARNING			         = $FFFE;
      SIP_KILL_MENU				           = $FFFF;




// wIndex value of ImmSIPanelState
      SIP_QUERY_LOCATION          = $1000;
      SIP_SET_LOCATION            = $1001;
      SIP_INPUT_ATTRIBUTES		      = $7000;
      SIP_INPUT_NUMERIC 			       = $00000004;
      SIP_INPUT_ALPHANUMERIC		    = $00000008;
      SIP_INPUT_HIRAGANA			       = $00000010;
      SIP_INPUT_KATAKANA			       = $00000100;
      SIP_INPUT_EVERYTHING		      = $00000000;
      SIP_INPUT_PASSWORD          = $00000020;
      SIP_INPUT_HALFWIDTH			      = $00000001;
      SIP_INPUT_FULLWIDTH			      = $00000002;

function ImmSIPanelState(dwCmd:UINT; pValue:LPVOID):BOOL; external ImmDLL name 'ImmSIPanelState'; // 467


const
// wParam of report message WM_IME_REQUEST
      IMR_COMPOSITIONWINDOW           = $0001;
      IMR_CANDIDATEWINDOW             = $0002;
      IMR_COMPOSITIONFONT             = $0003;
      IMR_RECONVERTSTRING             = $0004;
      IMR_CONFIRMRECONVERTSTRING      = $0005;
      IMR_QUERYCHARPOSITION           = $0006;
      IMR_DOCUMENTFEED                = $0007;
// reserved value
      IMR_RESERVED0x1000              = $1000;

{
#ifdef WINCEOEM
#include <pimm.h>	// internal defines
#endif
}



// Additional function declared in pwinuser.h from WM 6.0 Platform Builder.

function ImmEnableIME(param1:DWORD):BOOL; external ImmDLL name 'ImmEnableIME'; // 444

function ImmGetKeyboardLayout(dwThreadId:DWORD):HKL; external ImmDLL name 'ImmGetKeyboardLayout'; // 449

implementation

end.