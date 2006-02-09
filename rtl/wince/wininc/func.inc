{
    This file is part of the Free Pascal run time library.
    This unit contains the record definition for the Win32 API
    Copyright (c) 1999-2000 by Florian KLaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{
  Functions.h

  Declarations for all the Windows32 API Functions

  Copyright (C) 1996, 1997 Free Software Foundation, Inc.

  Author: Scott Christley <scottc@net-community.com>

  This file is part of the Windows32 API Library.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  If you are interested in a warranty or support for this source code,
  contact Scott Christley <scottc@net-community.com> for more information.

  You should have received a copy of the GNU Library General Public
  License along with this library; see the file COPYING.LIB.
  If not, write to the Free Software Foundation,
  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  Changes :

  08/22/2005 update for wince4.2 port, orinaudo@gmail.com
}

{$ifdef read_interface}
//begin common win32 & wince
function AbortDoc(_para1:HDC):longint; external GdiDLL name 'AbortDoc';
function AdjustWindowRectEx(lpRect:LPRECT; dwStyle:DWORD; bMenu:WINBOOL; dwExStyle:DWORD):WINBOOL; external UserDLLCore name 'AdjustWindowRectEx';
function BeginDeferWindowPos(nNumWindows:longint):HDWP; external UserDLLCore name 'BeginDeferWindowPos';
function BeginPaint(hWnd:HWND; lpPaint:LPPAINTSTRUCT):HDC; external UserDLLCore name 'BeginPaint';
function BitBlt(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:HDC; _para7:longint; _para8:longint; _para9:DWORD):WINBOOL; external GdiDLL name 'BitBlt';
function BringWindowToTop(hWnd:HWND):WINBOOL; external UserDLLCore name 'BringWindowToTop';
function CallNextHookEx(hhk:HHOOK; nCode:longint; wParam:WPARAM; lParam:LPARAM):LRESULT; external UserDLLCore name 'CallNextHookEx';
function CheckMenuItem(hMenu:HMENU; uIDCheckItem:UINT; uCheck:UINT):DWORD; external UserDLLCore name 'CheckMenuItem';
function CheckMenuRadioItem(_para1:HMENU; _para2:UINT; _para3:UINT; _para4:UINT; _para5:UINT):WINBOOL; external UserDLLCore name 'CheckMenuRadioItem';
function CheckRadioButton(hDlg:HWND; nIDFirstButton:longint; nIDLastButton:longint; nIDCheckButton:longint):WINBOOL; external UserDLLCore name 'CheckRadioButton';
function ClearCommBreak(hFile:HANDLE):WINBOOL; external KernelDLL name 'ClearCommBreak';
function ClearCommError(hFile:HANDLE; lpErrors:LPDWORD; lpStat:LPCOMSTAT):WINBOOL; external KernelDLL name 'ClearCommError';
function ClientToScreen(hWnd:HWND; lpPoint:LPPOINT):WINBOOL; external UserDLLCore name 'ClientToScreen';
function ClipCursor(lpRect:LPRECT):WINBOOL; external UserDLLCore name 'ClipCursor';
function CloseClipboard:WINBOOL; external UserDLLCore name 'CloseClipboard';
function CloseEnhMetaFile(_para1:HDC):HENHMETAFILE; external GdiDLL name 'CloseEnhMetaFile';
function CloseHandle(hObject:HANDLE):WINBOOL; external KernelDLL name 'CloseHandle';
function CombineRgn(_para1:HRGN; _para2:HRGN; _para3:HRGN; _para4:longint):longint; external GdiDLL name 'CombineRgn';
function CommDlgExtendedError : DWORD; external ComdlgDLL name 'CommDlgExtendedError';
//to move to ascfun
function CommDlg_OpenSave_GetSpecA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
//to move to unifun
function CommDlg_OpenSave_GetSpecW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
{$ifndef Unicode}
//to move to ascdef
function CommDlg_OpenSave_GetSpec(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
{$endif Unicode}
//to move to ascfun
function CommDlg_OpenSave_GetFilePathA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
//to move to unifun
function CommDlg_OpenSave_GetFilePathW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
{$ifndef Unicode}
//to move to ascdef
function CommDlg_OpenSave_GetFilePath(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
{$endif Unicode}
//to move to ascfun
function CommDlg_OpenSave_GetFolderPathA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
//to move to unifun
function CommDlg_OpenSave_GetFolderPathW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
{$ifndef Unicode}
//to move to ascdef
function CommDlg_OpenSave_GetFolderPath(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
{$endif Unicode}
//begin moved to unidef
//function CommDlg_OpenSave_GetSpec(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
//function CommDlg_OpenSave_GetFilePath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
//function CommDlg_OpenSave_GetFolderPath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
//end moved to unidef
function CommDlg_OpenSave_GetFolderIDList(_hdlg:HWND;_pidl:LPVOID;_cbmax : longint) : LRESULT;
function CommDlg_OpenSave_SetControlText(_hdlg:HWND;_id : longint;_text : LPSTR) : LRESULT;
function CommDlg_OpenSave_HideControl(_hdlg:HWND;_id : longint) : LRESULT;
function CommDlg_OpenSave_SetDefExt(_hdlg:HWND;_pszext : LPSTR) : LRESULT;
function CompareFileTime(lpFileTime1:LPFILETIME; lpFileTime2:LPFILETIME):LONG; external KernelDLL name 'CompareFileTime';
function ContinueDebugEvent(dwProcessId:DWORD; dwThreadId:DWORD; dwContinueStatus:DWORD):WINBOOL; external KernelDLL name 'ContinueDebugEvent';
function ConvertDefaultLocale(Locale:LCID):LCID; external KernelDLL name 'ConvertDefaultLocale';
procedure CopyMemory(Destination:PVOID; Source:pointer; Length:DWORD);
function CopyRect(lprcDst:LPRECT; var lprcSrc:RECT):WINBOOL; external UserDLLCore name 'CopyRect';
function CountClipboardFormats:longint; external UserDLLCore name 'CountClipboardFormats';
function CreateBitmap(_para1:longint; _para2:longint; _para3:UINT; _para4:UINT; _para5:pointer):HBITMAP; external GdiDLL name 'CreateBitmap';
function CreateCaret(hWnd:HWND; hBitmap:HBITMAP; nWidth:longint; nHeight:longint):WINBOOL; external UserDLLCore name 'CreateCaret';
function CreateCompatibleBitmap(_para1:HDC; _para2:longint; _para3:longint):HBITMAP; external GdiDLL name 'CreateCompatibleBitmap';
function CreateCompatibleDC(_para1:HDC):HDC; external GdiDLL name 'CreateCompatibleDC';
function CreateDIBPatternBrushPt(_para1:pointer; _para2:UINT):HBRUSH; external GdiDLL name 'CreateDIBPatternBrushPt';
function CreateDIBSection(_para1:HDC; var _para2:BITMAPINFO; _para3:UINT; var _para4:pointer; _para5:HANDLE;_para6:DWORD):HBITMAP; external GdiDLL name 'CreateDIBSection';
function CreateIconIndirect(piconinfo:PICONINFO):HICON; external UserDLLCore name 'CreateIconIndirect';
function CreateMenu:HMENU; external UserDLLCore name 'CreateMenu';
function CreatePopupMenu:HMENU; external UserDLLCore name 'CreatePopupMenu';
function CreatePalette(var _para1:LOGPALETTE):HPALETTE; external GdiDLL name 'CreatePalette';
function CreatePatternBrush(_para1:HBITMAP):HBRUSH; external GdiDLL name 'CreatePatternBrush';
function CreatePen(_para1:longint; _para2:longint; _para3:COLORREF):HPEN; external GdiDLL name 'CreatePen';
function CreatePenIndirect(var _para1:LOGPEN):HPEN; external GdiDLL name 'CreatePenIndirect';
function CreateRectRgn(_para1:longint; _para2:longint; _para3:longint; _para4:longint):HRGN; external GdiDLL name 'CreateRectRgn';
function CreateRectRgnIndirect(var _para1:RECT):HRGN; external GdiDLL name 'CreateRectRgnIndirect';
function CreateSolidBrush(_para1:COLORREF):HBRUSH; external GdiDLL name 'CreateSolidBrush';
function CreateToolbarEx(hwnd:HWND; ws:DWORD; wID:UINT; nBitmaps:longint; hBMInst:HINST;wBMID:UINT; lpButtons:LPCTBBUTTON; iNumButtons:longint; dxButton:longint; dyButton:longint;dxBitmap:longint;
  dyBitmap:longint; uStructSize:UINT):HWND; external ComctlDLL name 'CreateToolbarEx';
function CreateUpDownControl(dwStyle:DWORD; x:longint; y:longint; cx:longint; cy:longint;hParent:HWND; nID:longint; hInst:HINST; hBuddy:HWND; nUpper:longint;nLower:longint; nPos:longint):HWND; external ComctlDLL name 'CreateUpDownControl';
function DebugActiveProcess(dwProcessId:DWORD):WINBOOL; external KernelDLL name 'DebugActiveProcess';
function DeferWindowPos(hWinPosInfo:HDWP; hWnd:HWND; hWndInsertAfter:HWND; x:longint; y:longint;cx:longint; cy:longint; uFlags:UINT):HDWP; external UserDLLCore name 'DeferWindowPos';
procedure DeleteCriticalSection(lpCriticalSection:LPCRITICAL_SECTION); external KernelDLL name 'DeleteCriticalSection';
function DeleteDC(_para1:HDC):WINBOOL; external GdiDLL name 'DeleteDC';
function DeleteEnhMetaFile(_para1:HENHMETAFILE):WINBOOL; external GdiDLL name 'DeleteEnhMetaFile';
function DeleteMenu(hMenu:HMENU; uPosition:UINT; uFlags:UINT):WINBOOL; external UserDLLCore name 'DeleteMenu';
function DeleteObject(_para1:HGDIOBJ):WINBOOL; external GdiDLL name 'DeleteObject';
function DestroyAcceleratorTable(hAccel:HACCEL):WINBOOL; external UserDLLCore name 'DestroyAcceleratorTable';
function DestroyCaret:WINBOOL; external UserDLLCore name 'DestroyCaret';
function DestroyIcon(hIcon:HICON):WINBOOL; external UserDLLCore name 'DestroyIcon';
function DestroyMenu(hMenu:HMENU):WINBOOL; external UserDLLCore name 'DestroyMenu';
function DestroyPropertySheetPage(hPSPage:HPROPSHEETPAGE):WINBOOL; external ComctlDLL name 'DestroyPropertySheetPage';
function DestroyWindow(hWnd:HWND):WINBOOL; external UserDLLCore name 'DestroyWindow';
function DeviceIoControl(hDevice:HANDLE; dwIoControlCode:DWORD; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID;nOutBufferSize:DWORD; lpBytesReturned:LPDWORD; lpOverlapped:LPOVERLAPPED):WINBOOL; external KernelDLL name 'DeviceIoControl';
function DisableThreadLibraryCalls(hLibModule:HMODULE):WINBOOL; external KernelDLL name 'DisableThreadLibraryCalls';
function DrawEdge(hdc:HDC; qrc:LPRECT; edge:UINT; grfFlags:UINT):WINBOOL; external UserDLLCore name 'DrawEdge';
function DrawFocusRect(hDC:HDC; var lprc:RECT):WINBOOL; external UserDLLCore name 'DrawFocusRect';
function DrawFrameControl(_para1:HDC; _para2:LPRECT; _para3:UINT; _para4:UINT):WINBOOL; external UserDLLCore name 'DrawFrameControl';
function DrawIconEx(hdc:HDC; xLeft:longint; yTop:longint; hIcon:HICON; cxWidth:longint;cyWidth:longint; istepIfAniCur:UINT; hbrFlickerFreeDraw:HBRUSH; diFlags:UINT):WINBOOL; external UserDLLCore name 'DrawIconEx';
function DrawMenuBar(hWnd:HWND):WINBOOL; external UserDLLCore name 'DrawMenuBar';
function DuplicateHandle(hSourceProcessHandle:HANDLE; hSourceHandle:HANDLE; hTargetProcessHandle:HANDLE; lpTargetHandle:LPHANDLE; dwDesiredAccess:DWORD;bInheritHandle:WINBOOL; dwOptions:DWORD):WINBOOL; external KernelDLL name 'DuplicateHandle';
function Ellipse(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint):WINBOOL; external GdiDLL name 'Ellipse';
function EqualRgn(_para1:HRGN; _para2:HRGN):WINBOOL; external GdiDLL name 'EqualRgn';
function EmptyClipboard:WINBOOL; external UserDLLCore name 'EmptyClipboard';
function EnableMenuItem(hMenu:HMENU; uIDEnableItem:UINT; uEnable:UINT):WINBOOL; external UserDLLCore name 'EnableMenuItem';
function EnableWindow(hWnd:HWND; bEnable:WINBOOL):WINBOOL; external UserDLLCore name 'EnableWindow';
function EndDeferWindowPos(hWinPosInfo:HDWP):WINBOOL; external UserDLLCore name 'EndDeferWindowPos';
function EndDialog(hDlg:HWND; nResult:longint):WINBOOL; external UserDLLCore name 'EndDialog';
function EndDoc(_para1:HDC):longint; external GdiDLL name 'EndDoc';
function EndPage(_para1:HDC):longint; external GdiDLL name 'EndPage';
function EndPaint(hWnd:HWND; lpPaint:LPPAINTSTRUCT):WINBOOL; external UserDLLCore name 'EndPaint';
procedure EnterCriticalSection(lpCriticalSection:LPCRITICAL_SECTION); external KernelDLL name 'EnterCriticalSection';
function EnumClipboardFormats(format:UINT):UINT; external UserDLLCore name 'EnumClipboardFormats';
function EnumWindows(lpEnumFunc:ENUMWINDOWSPROC; lParam:LPARAM):WINBOOL; external UserDLLCore name 'EnumWindows';
function EqualRect(var lprc1:RECT; var lprc2:RECT):WINBOOL; external UserDLLCore name 'EqualRect';
function EscapeCommFunction(hFile:HANDLE; dwFunc:DWORD):WINBOOL; external KernelDLL name 'EscapeCommFunction';
procedure ExitThread(dwExitCode:DWORD); external KernelDLL name 'ExitThread';
function ExitWindowsEx(uFlags:UINT; dwReserved:DWORD):WINBOOL; external UserDLLAyg name 'ExitWindowsEx';
function ExtCreateRegion(var _para1:XFORM; _para2:DWORD; var _para3:RGNDATA):HRGN; external GdiDLL name 'ExtCreateRegion';
function ExtEscape(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR; _para5:longint;_para6:LPSTR):longint; external GdiDLL name 'ExtEscape';
function ExcludeClipRect(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint):longint; external GdiDLL name 'ExcludeClipRect';
function FileTimeToLocalFileTime(lpFileTime:LPFILETIME; lpLocalFileTime:LPFILETIME):WINBOOL; external KernelDLL name 'FileTimeToLocalFileTime';
function FileTimeToSystemTime(lpFileTime:LPFILETIME; lpSystemTime:LPSYSTEMTIME):WINBOOL; external KernelDLL name 'FileTimeToSystemTime';
procedure FillMemory(Destination:PVOID; Length:DWORD; Fill:BYTE);
function FillRect(hDC:HDC; const lprc:RECT; hbr:HBRUSH):longint; external UserDLLCore name 'FillRect';
function FindClose(hFindFile:HANDLE):WINBOOL; external KernelDLL name 'FindClose';
function FindCloseChangeNotification(hChangeHandle:HANDLE):WINBOOL; external KernelDLL name 'FindCloseChangeNotification';
function FindNextChangeNotification(hChangeHandle:HANDLE):WINBOOL; external KernelDLL name 'FindNextChangeNotification';
function FlushFileBuffers(hFile:HANDLE):WINBOOL; external KernelDLL name 'FlushFileBuffers';
function FlushInstructionCache(hProcess:HANDLE; lpBaseAddress:LPCVOID; dwSize:DWORD):WINBOOL; external KernelDLL name 'FlushInstructionCache';
function FlushViewOfFile(lpBaseAddress:LPCVOID; dwNumberOfBytesToFlush:DWORD):WINBOOL; external KernelDLL name 'FlushViewOfFile';
function FreeLibrary(hLibModule:HMODULE):WINBOOL; external KernelDLL name 'FreeLibrary';
procedure FreeLibraryAndExitThread(hLibModule:HMODULE; dwExitCode:DWORD); external KernelDLL name 'FreeLibraryAndExitThread';
function GetActiveWindow:HWND; external UserDLLCore name 'GetActiveWindow';
function GetACP:UINT; external KernelDLL name 'GetACP';
function GetAsyncKeyState(vKey:longint):SHORT; external UserDLLCore name 'GetAsyncKeyState';
function GetBkColor(_para1:HDC):COLORREF; external GdiDLL name 'GetBkColor';
function GetBkMode(_para1:HDC):longint; external GdiDLL name 'GetBkMode';
function GetCapture:HWND; external UserDLLCore name 'GetCapture';
function GetCaretBlinkTime:UINT; external UserDLLCore name 'GetCaretBlinkTime';
function GetCaretPos(lpPoint:LPPOINT):WINBOOL; external UserDLLCore name 'GetCaretPos';
function GetClientRect(hWnd:HWND; lpRect:LPRECT):WINBOOL; external UserDLLCore name 'GetClientRect';
function GetClipboardOwner:HWND; external UserDLLCore name 'GetClipboardOwner';
function GetClipboardData(uFormat:UINT):HANDLE; external UserDLLCore name 'GetClipboardData';
function GetClipBox(_para1:HDC; _para2:LPRECT):longint; external GdiDLL name 'GetClipBox';
function GetClipCursor(lpRect:LPRECT):WINBOOL; external UserDLLCore name 'GetClipCursor';
function GetClipRgn(_para1:HDC; _para2:HRGN):longint; external GdiDLL name 'GetClipRgn';
function GetCommProperties(hFile:HANDLE; lpCommProp:LPCOMMPROP):WINBOOL; external KernelDLL name 'GetCommProperties';
function GetCommModemStatus(hFile:HANDLE; lpModemStat:PDWORD):WINBOOL; external KernelDLL name 'GetCommModemStatus';
function GetCommState(hFile:HANDLE; lpDCB:PDCB):WINBOOL; external KernelDLL name 'GetCommState';
function GetCommTimeouts(hFile:HANDLE; lpCommTimeouts:PCOMMTIMEOUTS):WINBOOL; external KernelDLL name 'GetCommTimeouts';
function GetCPInfo(_para1:UINT; _para2:LPCPINFO):WINBOOL; external KernelDLL name 'GetCPInfo';
function GetCursor:HCURSOR; external UserDLLCore name 'GetCursor';
function GetCursorPos(lpPoint:LPPOINT):WINBOOL; external UserDLLCore name 'GetCursorPos';
function GetCurrentObject(_para1:HDC; _para2:UINT):HGDIOBJ; external GdiDLL name 'GetCurrentObject';
function GetCurrentPositionEx(_para1:HDC; _para2:LPPOINT):WINBOOL; external GdiDLL name 'GetCurrentPositionEx';
function GetDC(hWnd:HWND):HDC; external UserDLLCore name 'GetDC';
function GetDCEx(hWnd:HWND; hrgnClip:HRGN; flags:DWORD):HDC; external UserDLLCore name 'GetDCEx';
function GetDesktopWindow:HWND; external UserDLLCore name 'GetDesktopWindow';
function GetDeviceCaps(_para1:HDC; _para2:longint):longint; external GdiDLL name 'GetDeviceCaps';
function GetDialogBaseUnits:longint; external UserDLLCore name 'GetDialogBaseUnits';
function GetDIBColorTable(_para1:HDC; _para2:UINT; _para3:UINT; var _para4:RGBQUAD):UINT; external GdiDLL name 'GetDIBColorTable';
function GetDlgCtrlID(hWnd:HWND):longint; external UserDLLCore name 'GetDlgCtrlID';
function GetDlgItem(hDlg:HWND; nIDDlgItem:longint):HWND; external UserDLLCore name 'GetDlgItem';
function GetDlgItemInt(hDlg:HWND; nIDDlgItem:longint; var lpTranslated:WINBOOL; bSigned:WINBOOL):UINT; external UserDLLCore name 'GetDlgItemInt';
function GetDoubleClickTime:UINT; external UserDLLCore name 'GetDoubleClickTime';
function GetExitCodeProcess(hProcess:HANDLE; lpExitCode:LPDWORD):WINBOOL; external KernelDLL name 'GetExitCodeProcess';
function GetExitCodeThread(hThread:HANDLE; lpExitCode:LPDWORD):WINBOOL; external KernelDLL name 'GetExitCodeThread';
function GetFocus:HWND; external UserDLLCore name 'GetFocus';
function GetForegroundWindow:HWND; external UserDLLCore name 'GetForegroundWindow';
function GetFileInformationByHandle(hFile:HANDLE; lpFileInformation:LPBY_HANDLE_FILE_INFORMATION):WINBOOL; external KernelDLL name 'GetFileInformationByHandle';
function GetFileTime(hFile:HANDLE; lpCreationTime:LPFILETIME; lpLastAccessTime:LPFILETIME; lpLastWriteTime:LPFILETIME):WINBOOL; external KernelDLL name 'GetFileTime';
function GetFileSize(hFile:HANDLE; lpFileSizeHigh:LPDWORD):DWORD; external KernelDLL name 'GetFileSize';
function GetKeyboardLayout(dwLayout:DWORD):HKL; external UserDLLCore name 'GetKeyboardLayout';
function GetKeyboardLayoutList(nBuff:longint; var lpList:HKL):UINT; external UserDLLCore name 'GetKeyboardLayoutList';
function GetKeyboardType(nTypeFlag:longint):longint; external UserDLLCore name 'GetKeyboardType';
function GetKeyState(nVirtKey:longint):SHORT; external UserDLLCore name 'GetKeyState';
function GetLastError:DWORD; external KernelDLL name 'GetLastError';
procedure GetLocalTime(lpSystemTime:LPSYSTEMTIME); external KernelDLL name 'GetLocalTime';
function GetMessagePos:DWORD; external UserDLLCore name 'GetMessagePos';
function GetNearestColor(_para1:HDC; _para2:COLORREF):COLORREF; external GdiDLL name 'GetNearestColor';
function GetNearestPaletteIndex(_para1:HPALETTE; _para2:COLORREF):UINT; external GdiDLL name 'GetNearestPaletteIndex';
function GetNextDlgGroupItem(hDlg:HWND; hCtl:HWND; bPrevious:WINBOOL):HWND; external UserDLLCore name 'GetNextDlgGroupItem';
function GetNextDlgTabItem(hDlg:HWND; hCtl:HWND; bPrevious:WINBOOL):HWND; external UserDLLCore name 'GetNextDlgTabItem';
function GetObjectType(h:HGDIOBJ):DWORD; external GdiDLL name 'GetObjectType';
function GetOEMCP:UINT; external KernelDLL name 'GetOEMCP';
function GetOpenClipboardWindow:HWND; external UserDLLCore name 'GetOpenClipboardWindow';
function GetPaletteEntries(_para1:HPALETTE; _para2:UINT; _para3:UINT; _para4:LPPALETTEENTRY):UINT; external GdiDLL name 'GetPaletteEntries';
function GetParent(hWnd:HWND):HWND; external UserDLLCore name 'GetParent';
function GetPriorityClipboardFormat(var paFormatPriorityList:UINT; cFormats:longint):longint; external UserDLLCore name 'GetPriorityClipboardFormat';
function GetPixel(_para1:HDC; _para2:longint; _para3:longint):COLORREF; external GdiDLL name 'GetPixel';
function GetProcessHeap:HANDLE; external KernelDLL name 'GetProcessHeap';
function GetQueueStatus(flags:UINT):DWORD; external UserDLLCore name 'GetQueueStatus';
function GetRegionData(_para1:HRGN; _para2:DWORD; _para3:LPRGNDATA):DWORD; external GdiDLL name 'GetRegionData';
function GetRgnBox(_para1:HRGN; _para2:LPRECT):longint; external GdiDLL name 'GetRgnBox';
function GetScrollInfo(_para1:HWND; _para2:longint; _para3:LPSCROLLINFO):WINBOOL; external UserDLLCore name 'GetScrollInfo';
function GetStockObject(_para1:longint):HGDIOBJ; external GdiDLL name 'GetStockObject';
function GetSubMenu(hMenu:HMENU; nPos:longint):HMENU; external UserDLLCore name 'GetSubMenu';
function GetSysColor(nIndex:longint):DWORD; external UserDLLCore name 'GetSysColor';
function GetSysColorBrush(nIndex:longint):HBRUSH; external UserDLLCore name 'GetSysColorBrush';
function GetSystemDefaultLangID:LANGID; external KernelDLL name 'GetSystemDefaultLangID';
function GetSystemDefaultLCID:LCID; external KernelDLL name 'GetSystemDefaultLCID';
procedure GetSystemInfo(lpSystemInfo:LPSYSTEM_INFO); external KernelDLL name 'GetSystemInfo';
function GetSystemMetrics(nIndex:longint):longint; external UserDLLCore name 'GetSystemMetrics';
procedure GetSystemTime(lpSystemTime:LPSYSTEMTIME); external KernelDLL name 'GetSystemTime';
function GetSystemPaletteEntries(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPPALETTEENTRY):UINT; external GdiDLL name 'GetSystemPaletteEntries';
function GetTextAlign(_para1:HDC):UINT; external GdiDLL name 'GetTextAlign';
function GetTextColor(_para1:HDC):COLORREF; external GdiDLL name 'GetTextColor';
function GetThreadContext(hThread:HANDLE; lpContext:LPCONTEXT):WINBOOL; external KernelDLL name 'GetThreadContext';
function GetThreadPriority(hThread:HANDLE):longint; external KernelDLL name 'GetThreadPriority';
function GetThreadTimes(hThread:HANDLE; lpCreationTime:LPFILETIME; lpExitTime:LPFILETIME; lpKernelTime:LPFILETIME; lpUserTime:LPFILETIME):WINBOOL; external KernelDLL name 'GetThreadTimes';
function GetTickCount:DWORD; external KernelDLL name 'GetTickCount';
function GetTimeZoneInformation(lpTimeZoneInformation:LPTIME_ZONE_INFORMATION):DWORD; external KernelDLL name 'GetTimeZoneInformation';
function GetUpdateRect(hWnd:HWND; lpRect:LPRECT; bErase:WINBOOL):WINBOOL; external UserDLLCore name 'GetUpdateRect';
function GetUpdateRgn(hWnd:HWND; hRgn:HRGN; bErase:WINBOOL):longint; external UserDLLCore name 'GetUpdateRgn';
function GetUserDefaultLangID:LANGID; external KernelDLL name 'GetUserDefaultLangID';
function GetUserDefaultLCID:LCID; external KernelDLL name 'GetUserDefaultLCID';
function GetWindow(hWnd:HWND; uCmd:UINT):HWND; external UserDLLCore name 'GetWindow';
function GetWindowDC(hWnd:HWND):HDC; external UserDLLCore name 'GetWindowDC';
function GetWindowRgn(hWnd:HWND; hRgn:HRGN):longint; external UserDLLCore name 'GetWindowRgn';
function GetWindowRect(hWnd:HWND; lpRect:LPRECT):WINBOOL; external UserDLLCore name 'GetWindowRect';
function GetWindowThreadProcessId(hWnd:HWND; lpdwProcessId:LPDWORD):DWORD; external UserDLLCore name 'GetWindowThreadProcessId';
function GlobalAllocPtr(flags,cb:DWord):Pointer;
function GlobalFreePtr(lp:Pointer):Pointer;
function GlobalDiscard(hglbMem:HGLOBAL):HGLOBAL;
function GlobalLockPtr(lp:pointer):Pointer;
function GlobalPtrHandle(lp:pointer):Pointer;
function GlobalUnlockPtr(lp:pointer):Pointer;
function GlobalDeleteAtom(nAtom:ATOM):ATOM; external KernelDLL name 'GlobalDeleteAtom';
procedure GlobalMemoryStatus(lpBuffer:LPMEMORYSTATUS); external KernelDLL name 'GlobalMemoryStatus';
function Header_DeleteItem(hwndHD:HWND;index : longint) : WINBOOL;
function Header_GetItem(hwndHD:HWND;index:longint;var hdi : HD_ITEM) : WINBOOL;
function Header_GetItemCount(hwndHD : HWND) : longint;
function Header_InsertItem(hwndHD:HWND;index : longint;var hdi : HD_ITEM) : longint;
function Header_Layout(hwndHD:HWND;var layout : HD_LAYOUT) : WINBOOL;
function Header_SetItem(hwndHD:HWND;index : longint;var hdi : HD_ITEM) : WINBOOL;
function HeapAlloc(hHeap:HANDLE; dwFlags:DWORD; dwBytes:DWORD):LPVOID; external KernelDLL name 'HeapAlloc';
function HeapAllocTrace(hHeap:HANDLE; dwFlags:DWORD; dwBytes:DWORD; dwLineNum:DWORD; szFileName:PCHAR):LPVOID; external KernelDLL name 'HeapAllocTrace';  //+winbase
function HeapCreate(flOptions:DWORD; dwInitialSize:DWORD; dwMaximumSize:DWORD):HANDLE; external KernelDLL name 'HeapCreate';
function HeapDestroy(hHeap:HANDLE):WINBOOL; external KernelDLL name 'HeapDestroy';
function HeapFree(hHeap:HANDLE; dwFlags:DWORD; lpMem:LPVOID):WINBOOL; external KernelDLL name 'HeapFree';
function HeapReAlloc(hHeap:HANDLE; dwFlags:DWORD; lpMem:LPVOID; dwBytes:DWORD):LPVOID; external KernelDLL name 'HeapReAlloc';
function HeapSize(hHeap:HANDLE; dwFlags:DWORD; lpMem:LPCVOID):DWORD; external KernelDLL name 'HeapSize';
function HeapValidate(hHeap:HANDLE; dwFlags:DWORD; lpMem:LPCVOID):WINBOOL; external KernelDLL name 'HeapValidate';
function HideCaret(hWnd:HWND):WINBOOL; external UserDLLCore name 'HideCaret';
function ImageList_Add(himl:HIMAGELIST; hbmImage:HBITMAP; hbmMask:HBITMAP):longint; external ComctlDLLCore name 'ImageList_Add';
function ImageList_AddIcon(himl:HIMAGELIST; hicon:HICON):longint;
function ImageList_AddMasked(himl:HIMAGELIST; hbmImage:HBITMAP; crMask:COLORREF):longint; external ComctlDLLCore name 'ImageList_AddMasked';
function ImageList_BeginDrag(himlTrack:HIMAGELIST; iTrack:longint; dxHotspot:longint; dyHotspot:longint):WINBOOL; external ComctlDLLCore name 'ImageList_BeginDrag';
function ImageList_Create(cx:longint; cy:longint; flags:UINT; cInitial:longint; cGrow:longint):HIMAGELIST; external ComctlDLLCore name 'ImageList_Create';
function ImageList_Destroy(himl:HIMAGELIST):WINBOOL; external ComctlDLLCore name 'ImageList_Destroy';
function ImageList_DragEnter(hwndLock:HWND; x:longint; y:longint):WINBOOL; external ComctlDLLCore name 'ImageList_DragEnter';
function ImageList_DragLeave(hwndLock:HWND):WINBOOL; external ComctlDLLCore name 'ImageList_DragLeave';
function ImageList_DragMove(x:longint; y:longint):WINBOOL; external ComctlDLLCore name 'ImageList_DragMove';
function ImageList_DragShowNolock(fShow:WINBOOL):WINBOOL; external ComctlDLLCore name 'ImageList_DragShowNolock';
function ImageList_Draw(himl:HIMAGELIST; i:longint; hdcDst:HDC; x:longint; y:longint;fStyle:UINT):WINBOOL; external ComctlDLLCore name 'ImageList_Draw';
function ImageList_DrawEx(himl:HIMAGELIST; i:longint; hdcDst:HDC; x:longint; y:longint;dx:longint; dy:longint; rgbBk:COLORREF; rgbFg:COLORREF; fStyle:UINT):WINBOOL; external ComctlDLLCore name 'ImageList_DrawEx';
function ImageList_DrawIndirect(pimldp:PIMAGELISTDRAWPARAMS):WINBOOL; external ComctlDLLCore name 'ImageList_DrawIndirect'; //+commctrl
procedure ImageList_EndDrag; external ComctlDLLCore name 'ImageList_EndDrag';
function ImageList_GetBkColor(himl:HIMAGELIST):COLORREF; external ComctlDLLCore name 'ImageList_GetBkColor';
function ImageList_GetDragImage(ppt:LPPOINT; pptHotspot:LPPOINT):HIMAGELIST; external ComctlDLLCore name 'ImageList_GetDragImage';
function ImageList_GetIcon(himl:HIMAGELIST; i:longint; flags:UINT):HICON; external ComctlDLLCore name 'ImageList_GetIcon';
function ImageList_GetIconSize(himl:HIMAGELIST; var cx:longint; var cy:longint):WINBOOL; external ComctlDLLCore name 'ImageList_GetIconSize';
function ImageList_GetImageCount(himl:HIMAGELIST):longint; external ComctlDLLCore name 'ImageList_GetImageCount';
function ImageList_GetImageInfo(himl:HIMAGELIST; i:longint; var pImageInfo:IMAGEINFO):WINBOOL; external ComctlDLLCore name 'ImageList_GetImageInfo';
function ImageList_Merge(himl1:HIMAGELIST; i1:longint; himl2:HIMAGELIST; i2:longint; dx:longint;dy:longint):HIMAGELIST; external ComctlDLLCore name 'ImageList_Merge';
function ImageList_Replace(himl:HIMAGELIST; i:longint; hbmImage:HBITMAP; hbmMask:HBITMAP):WINBOOL; external ComctlDLLCore name 'ImageList_Replace';
function ImageList_ReplaceIcon(himl:HIMAGELIST; i:longint; hicon:HICON):longint; external ComctlDLLCore name 'ImageList_ReplaceIcon';
function ImageList_Remove(himl:HIMAGELIST; i:longint):WINBOOL; external ComctlDLLCore name 'ImageList_Remove';
function ImageList_SetBkColor(himl:HIMAGELIST; clrBk:COLORREF):COLORREF; external ComctlDLLCore name 'ImageList_SetBkColor';
function ImageList_SetDragCursorImage(himlDrag:HIMAGELIST; iDrag:longint; dxHotspot:longint; dyHotspot:longint):WINBOOL; external ComctlDLLCore name 'ImageList_SetDragCursorImage';
function ImageList_SetIconSize(himl:HIMAGELIST; cx:longint; cy:longint):WINBOOL; external ComctlDLLCore name 'ImageList_SetIconSize';
function ImageList_SetImageCount(himl: HIMAGELIST; uNewCount: UINT): longint; external ComctlDLLCore name 'ImageList_SetImageCount';
function ImageList_SetOverlayImage(himl:HIMAGELIST; iImage:longint; iOverlay:longint):WINBOOL; external ComctlDLLCore name 'ImageList_SetOverlayImage';
function InflateRect(lprc:LPRECT; dx:longint; dy:longint):WINBOOL; external UserDLLCore name 'InflateRect';
procedure InitCommonControls; external ComctlDLL name 'InitCommonControls'; 
//function InitCommonControlsEx(_para1:LPINITCOMMONCONTROLSEX):WINBOOL; external ComctlDLL name 'InitCommonControlsEx'; //- conflict with other def
procedure InitializeCriticalSection(lpCriticalSection:LPCRITICAL_SECTION); external KernelDLL name 'InitializeCriticalSection';
function InSendMessage:WINBOOL; external UserDLLCore name 'InSendMessage';
function InterlockedIncrement(lpAddend:LPLONG):LONG; external KernelDLL name 'InterlockedIncrement';
function InterlockedDecrement(lpAddend:LPLONG):LONG; external KernelDLL name 'InterlockedDecrement';
function InterlockedCompareExchange( var Destination:LPLONG; Exchange:LONG; Comperand:LONG):LONG; external KernelDLL name 'InterlockedCompareExchange';
function InterlockedExchange(Target:LPLONG; Value:LONG):LONG; external KernelDLL name 'InterlockedExchange';
function InterlockedExchangeAdd( Addend:LPLONG; Value:LONG):LONG; external KernelDLL name 'InterlockedExchangeAdd';
function InterlockedTestExchange( Target:LPLONG; oldValue:LONG; newValue:LONG):LONG; external KernelDLL name 'InterlockedTestExchange';
function IntersectClipRect(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint):longint; external GdiDLL name 'IntersectClipRect';
function IntersectRect(lprcDst:LPRECT; var lprcSrc1:RECT; var lprcSrc2:RECT):WINBOOL; external UserDLLCore name 'IntersectRect';
function InvalidateRect(hWnd:HWND; var lpRect:RECT; bErase:WINBOOL):WINBOOL; external UserDLLCore name 'InvalidateRect';
function InvalidateRect(hWnd:HWND;lpRect:LPRECT; bErase:WINBOOL):WINBOOL; external UserDLLCore name 'InvalidateRect';
function InvalidateRgn(hWnd:HWND; hRgn:HRGN; bErase:WINBOOL):WINBOOL; external UserDLLCore name 'InvalidateRgn';
function InvertRect(hDC:HDC; var lprc:RECT):WINBOOL; external UserDLLCore name 'InvertRect';
function IsBadReadPtr(lp:LPVOID; ucb:UINT):WINBOOL; external Kerneldll name 'IsBadReadPtr';
function IsBadWritePtr(lp:LPVOID; ucb:UINT):WINBOOL; external Kerneldll name 'IsBadWritePtr';
function IsBadCodePtr(lpfn:FARPROC):WINBOOL; external Kerneldll name 'IsBadCodePtr';
function IsChild(hWndParent:HWND; hWnd:HWND):WINBOOL; external UserDLLCore name 'IsChild';
function IsClipboardFormatAvailable(format:UINT):WINBOOL; external UserDLLCore name 'IsClipboardFormatAvailable';
function IsDBCSLeadByte(TestChar:BYTE):WINBOOL; external KernelDLL name 'IsDBCSLeadByte';
function IsDBCSLeadByteEx(CodePage:UINT; TestChar:BYTE):WINBOOL; external KernelDLL name 'IsDBCSLeadByteEx';
function IsRectEmpty(var lprc:RECT):WINBOOL; external UserDLLCore name 'IsRectEmpty';
function IsValidCodePage(CodePage:UINT):WINBOOL; external KernelDLL name 'IsValidCodePage';
function IsValidLocale(Locale:LCID; dwFlags:DWORD):WINBOOL; external KernelDLL name 'IsValidLocale';
function IsWindow(hWnd:HWND):WINBOOL; external UserDLLCore name 'IsWindow';
function IsWindowEnabled(hWnd:HWND):WINBOOL; external UserDLLCore name 'IsWindowEnabled';
function IsWindowVisible(hWnd:HWND):WINBOOL; external UserDLLCore name 'IsWindowVisible';
procedure keybd_event(bVk:BYTE; bScan:BYTE; dwFlags:DWORD; dwExtraInfo:DWORD); external UserDLLCore name 'keybd_event';
function KillTimer(hWnd:HWND; uIDEvent:UINT):WINBOOL; external UserDLLCore name 'KillTimer';
procedure LeaveCriticalSection(lpCriticalSection:LPCRITICAL_SECTION); external KernelDLL name 'LeaveCriticalSection';
function LineTo(_para1:HDC; _para2:longint; _para3:longint):WINBOOL; external GdiDLL name 'LineTo';
function ListView_Arrange(hwndLV:HWND;code : UINT) : LRESULT;
function ListView_CreateDragImage(hwnd:HWND;i : longint;lpptUpLeft : LPPOINT) : LRESULT;
function ListView_DeleteAllItems(hwnd : HWND) : LRESULT;
function ListView_DeleteColumn(hwnd:HWND;iCol : longint) : LRESULT;
function ListView_DeleteItem(hwnd:HWND;iItem : longint) : LRESULT;
function ListView_EditLabel(hwndLV:HWND;i : longint) : LRESULT;
function ListView_EnsureVisible(hwndLV:HWND;i,fPartialOK : longint) : LRESULT;
function ListView_FindItem(hwnd:HWND;iStart : longint;var lvfi : LV_FINDINFO) : longint;
function ListView_GetBkColor(hwnd : HWND) : LRESULT;
function ListView_GetCallbackMask(hwnd : HWND) : LRESULT;
function ListView_GetColumn(hwnd:HWND;iCol : longint;var col : LV_COLUMN) : LRESULT;
function ListView_GetColumnWidth(hwnd:HWND;iCol : longint) : LRESULT;
function ListView_GetCountPerPage(hwndLV : HWND) : LRESULT;
function ListView_GetEditControl(hwndLV : HWND) : LRESULT;
function ListView_GetImageList(hwnd:HWND;iImageList : wINT) : LRESULT;
function ListView_GetISearchString(hwndLV:HWND;lpsz : LPTSTR) : LRESULT;
function ListView_GetItem(hwnd:HWND;var item : LV_ITEM) : LRESULT;
function ListView_GetItemCount(hwnd : HWND) : LRESULT;
function ListView_GetItemPosition(hwndLV:HWND;i : longint;var pt : POINT) : longint;
function ListView_GetItemSpacing(hwndLV:HWND;fSmall : longint) : LRESULT;
function ListView_GetItemState(hwndLV:HWND;i,mask : longint) : LRESULT;
function ListView_GetNextItem(hwnd:HWND; iStart, flags : longint) : LRESULT;
function ListView_GetOrigin(hwndLV:HWND;var pt : POINT) : LRESULT;
function ListView_GetSelectedCount(hwndLV : HWND) : LRESULT;
function ListView_GetStringWidth(hwndLV:HWND;psz : LPCTSTR) : LRESULT;
function ListView_GetTextBkColor(hwnd : HWND) : LRESULT;
function ListView_GetTextColor(hwnd : HWND) : LRESULT;
function ListView_GetTopIndex(hwndLV : HWND) : LRESULT;
function ListView_GetViewRect(hwnd:HWND;var rc : RECT) : LRESULT;
function ListView_HitTest(hwndLV:HWND;var info : LV_HITTESTINFO) : LRESULT;
function ListView_InsertColumn(hwnd:HWND;iCol : longint;var col : LV_COLUMN) : LRESULT;
function ListView_InsertItem(hwnd:HWND;var item : LV_ITEM) : LRESULT;
function ListView_RedrawItems(hwndLV:HWND;iFirst,iLast : longint) : LRESULT;
function ListView_Scroll(hwndLV:HWND;dx,dy : longint) : LRESULT;
function ListView_SetBkColor(hwnd:HWND;clrBk : COLORREF) : LRESULT;
function ListView_SetCallbackMask(hwnd:HWND;mask : UINT) : LRESULT;
function ListView_SetColumn(hwnd:HWND;iCol : longint; var col : LV_COLUMN) : LRESULT;
function ListView_SetColumnWidth(hwnd:HWND;iCol,cx : longint) : LRESULT;
function ListView_SetImageList(hwnd:HWND;himl : longint;iImageList : HIMAGELIST) : LRESULT;
function ListView_SetItem(hwnd:HWND;var item : LV_ITEM) : LRESULT;
function ListView_SetItemCount(hwndLV:HWND;cItems : longint) : LRESULT;
function ListView_SetItemPosition(hwndLV:HWND;i,x,y : longint) : LRESULT;
function ListView_SetItemPosition32(hwndLV:HWND;i,x,y : longint) : LRESULT;
function ListView_SetItemState(hwndLV:HWND; i, data, mask:longint) : LRESULT;
function ListView_SetItemText(hwndLV:HWND; i, iSubItem_:longint;pszText_ : LPTSTR) : LRESULT;
function ListView_SetTextBkColor(hwnd:HWND;clrTextBk : COLORREF) : LRESULT;
function ListView_SetTextColor(hwnd:HWND;clrText : COLORREF) : LRESULT;
function ListView_SortItems(hwndLV:HWND;_pfnCompare:PFNLVCOMPARE;_lPrm : LPARAM) : LRESULT;
function ListView_Update(hwndLV:HWND;i : longint) : LRESULT;
function LoadResource(hModule:HINST; hResInfo:HRSRC):HGLOBAL; external KernelDLL name 'LoadResource';
function LocalAlloc(uFlags:UINT; uBytes:UINT):HLOCAL; external KernelDLL name 'LocalAlloc';
function LocalDiscard(hlocMem:HLOCAL):HLOCAL;
function LocalFileTimeToFileTime(lpLocalFileTime:LPFILETIME; lpFileTime:LPFILETIME):WINBOOL; external KernelDLL name 'LocalFileTimeToFileTime';
function LocalFree(hMem:HLOCAL):HLOCAL; external KernelDLL name 'LocalFree';
function LocalReAlloc(hMem:HLOCAL; uBytes:UINT; uFlags:UINT):HLOCAL; external KernelDLL name 'LocalReAlloc';
function LocalSize(hMem:HLOCAL):UINT; external KernelDLL name 'LocalSize';
function MapDialogRect(hDlg:HWND; lpRect:LPRECT):WINBOOL; external UserDLLCore name 'MapDialogRect';
function MapWindowPoints(hWndFrom:HWND; hWndTo:HWND; lpPoints:LPPOINT; cPoints:UINT):longint; external UserDLLCore name 'MapWindowPoints';
function MaskBlt(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:HDC; _para7:longint; _para8:longint; _para9:HBITMAP; _para10:longint;_para11:longint; _para12:DWORD):WINBOOL; external GdiDLL name 'MaskBlt';
function MessageBeep(uType:UINT):WINBOOL; external UserDLLCore name 'MessageBeep';
procedure mouse_event(dwFlags:DWORD; dx:DWORD; dy:DWORD; cButtons:DWORD; dwExtraInfo:DWORD); external UserDLLCore name 'mouse_event';
function MoveToEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external GdiDLL name 'MoveToEx';
procedure MoveMemory(Destination:PVOID; Source:pointer; Length:DWORD);
function MoveWindow(hWnd:HWND; X:longint; Y:longint; nWidth:longint; nHeight:longint;bRepaint:WINBOOL):WINBOOL; external UserDLLCore name 'MoveWindow';
function MsgWaitForMultipleObjectsEx(nCount:DWORD; pHandles:LPHANDLE ; dwMilliseconds:DWORD; dwWakeMask:DWORD; dwFlags:DWORD):DWORD; external UserDLLCore name 'MsgWaitForMultipleObjectsEx';  //+winuser
function MultiByteToWideChar(CodePage:UINT; dwFlags:DWORD; lpMultiByteStr:LPCSTR; cchMultiByte:longint; lpWideCharStr:LPWSTR;cchWideChar:longint):longint; external KernelDLL name 'MultiByteToWideChar';
function OffsetRect(lprc:LPRECT; dx:longint; dy:longint):WINBOOL; external UserDLLCore name 'OffsetRect';
function OffsetRgn(_para1:HRGN; _para2:longint; _para3:longint):longint; external GdiDLL name 'OffsetRgn';
function OpenClipboard(hWndNewOwner:HWND):WINBOOL; external UserDLLCore name 'OpenClipboard';
function OpenProcess(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; dwProcessId:DWORD):HANDLE; external KernelDLL name 'OpenProcess';
function PatBlt(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:DWORD):WINBOOL; external GdiDLL name 'PatBlt';
procedure PostQuitMessage(nExitCode:longint); external UserDLLCore name 'PostQuitMessage';
function PlayEnhMetaFile(_para1:HDC; _para2:HENHMETAFILE; var _para3:RECT):WINBOOL; external GdiDLL name 'PlayEnhMetaFile';
function Polygon(_para1:HDC; _para2:LPPOINT; _para3:longint):WINBOOL; external GdiDLL name 'Polygon';
function Polyline(_para1:HDC; _para2:LPPOINT; _para3:longint):WINBOOL; external GdiDLL name 'Polyline';
function PropSheet_AddPage(hPropSheetDlg : HWND;hpage : HPROPSHEETPAGE) : LRESULT;
function PropSheet_Apply(hPropSheetDlg : HWND) : LRESULT;
function PropSheet_CancelToClose(hPropSheetDlg : HWND) : LRESULT;
function PropSheet_Changed(hPropSheetDlg,hwndPage : HWND) : LRESULT;
function PropSheet_GetCurrentPageHwnd(hDlg : HWND) : LRESULT;
function PropSheet_GetTabControl(hPropSheetDlg : HWND) : LRESULT;
function PropSheet_IsDialogMessage(hDlg : HWND;pMsg : longint) : LRESULT;
function PropSheet_PressButton(hPropSheetDlg : HWND;iButton : longint) : LRESULT;
function PropSheet_QuerySiblings(hPropSheetDlg : HWND;param1,param2 : longint) : LRESULT;
function PropSheet_RebootSystem(hPropSheetDlg : HWND) : LRESULT;
function PropSheet_RemovePage(hPropSheetDlg : HWND;hpage : HPROPSHEETPAGE; index : longint) : LRESULT;
function PropSheet_RestartWindows(hPropSheetDlg : HWND) : LRESULT;
function PropSheet_SetCurSel(hPropSheetDlg : HWND;hpage : HPROPSHEETPAGE; index : longint) : LRESULT;
function PropSheet_SetCurSelByID(hPropSheetDlg : HWND; id : longint) : LRESULT;
function PropSheet_SetFinishText(hPropSheetDlg:HWND;lpszText : LPTSTR) : LRESULT;
function PropSheet_SetTitle(hPropSheetDlg:HWND;dwStyle:DWORD;lpszText : LPCTSTR) : LRESULT;
function PropSheet_SetWizButtons(hPropSheetDlg:HWND;dwFlags : DWORD) : LRESULT;
function PropSheet_UnChanged(hPropSheetDlg:HWND;hwndPage : HWND) : LRESULT;
function PtInRect(lprc:LPRECT; pt:POINT):WINBOOL; external UserDLLCore name 'PtInRect';
function PtInRegion(_para1:HRGN; _para2:longint; _para3:longint):WINBOOL; external GdiDLL name 'PtInRegion';
function PurgeComm(hFile:HANDLE; dwFlags:DWORD):WINBOOL; external KernelDLL name 'PurgeComm';
function QueryPerformanceCounter(lpPerformanceCount:PLARGE_INTEGER):WINBOOL; external Kerneldll name 'QueryPerformanceCounter';
function QueryPerformanceFrequency(lpFrequency:PLARGE_INTEGER):WINBOOL; external Kerneldll name 'QueryPerformanceFrequency';
procedure RaiseException(dwExceptionCode:DWORD; dwExceptionFlags:DWORD; nNumberOfArguments:DWORD; lpArguments:LPDWORD); external KernelDLL name 'RaiseException';
function ReadProcessMemory(hProcess:HANDLE; lpBaseAddress:LPCVOID; lpBuffer:LPVOID; nSize:DWORD; lpNumberOfBytesRead:LPDWORD):WINBOOL; external KernelDLL name 'ReadProcessMemory';
function RealizePalette(_para1:HDC):UINT; external GdiDLL name 'RealizePalette';
function RectInRegion(_para1:HRGN; var _para2:RECT):WINBOOL; external GdiDLL name 'RectInRegion';
function RectVisible(_para1:HDC; var _para2:RECT):WINBOOL; external GdiDLL name 'RectVisible';
function Rectangle(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint):WINBOOL; external GdiDLL name 'Rectangle';
function RedrawWindow(hWnd:HWND; var lprcUpdate:RECT; hrgnUpdate:HRGN; flags:UINT):WINBOOL; external UserDLLCore name 'RedrawWindow';
function RedrawWindow(hWnd:HWND; lprcUpdate:LPRECT; hrgnUpdate:HRGN; flags:UINT):WINBOOL; external UserDLLCore name 'RedrawWindow';
function RegisterHotKey(hWnd:HWND; anID:longint; fsModifiers:UINT; vk:UINT):WINBOOL; external UserDLLCore name 'RegisterHotKey';
function RegCloseKey(hKey:HKEY):LONG; external AdvApiDLLCore name 'RegCloseKey';
function RegFlushKey(hKey:HKEY):LONG; external AdvApiDLLCore name 'RegFlushKey';
function ReleaseCapture:WINBOOL; external UserDLLCore name 'ReleaseCapture';
function ReleaseDC(hWnd:HWND; hDC:HDC):longint; external UserDLLCore name 'ReleaseDC';
function ReleaseSemaphore(hSemaphore:HANDLE; lReleaseCount:LONG; lpPreviousCount:LPLONG):WINBOOL; external KernelDLL name 'ReleaseSemaphore';
function ReleaseMutex(hMutex:HANDLE):WINBOOL; external KernelDLL name 'ReleaseMutex';
function RemoveMenu(hMenu:HMENU; uPosition:UINT; uFlags:UINT):WINBOOL; external UserDLLCore name 'RemoveMenu';
function RestoreDC(_para1:HDC; _para2:longint):WINBOOL; external GdiDLL name 'RestoreDC';
function ResumeThread(hThread:HANDLE):DWORD; external KernelDLL name 'ResumeThread';
function RoundRect(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:longint; _para7:longint):WINBOOL; external GdiDLL name 'RoundRect';
function SaveDC(_para1:HDC):longint; external GdiDLL name 'SaveDC';
function ScreenToClient(hWnd:HWND; lpPoint:LPPOINT):WINBOOL; external UserDLLCore name 'ScreenToClient';
function ScrollDC(hDC:HDC; dx:longint; dy:longint; var lprcScroll:RECT; var lprcClip:RECT;hrgnUpdate:HRGN; lprcUpdate:LPRECT):WINBOOL; external UserDLLCore name 'ScrollDC';
function ScrollWindowEx(hWnd:HWND; dx:longint; dy:longint; var prcScroll:RECT; var prcClip:RECT;hrgnUpdate:HRGN; prcUpdate:LPRECT; flags:UINT):longint; external UserDLLCore name 'ScrollWindowEx';
function SelectClipRgn(_para1:HDC; _para2:HRGN):longint; external GdiDLL name 'SelectClipRgn';
function SelectObject(_para1:HDC; _para2:HGDIOBJ):HGDIOBJ; external GdiDLL name 'SelectObject';
function SelectPalette(_para1:HDC; _para2:HPALETTE; _para3:WINBOOL):HPALETTE; external GdiDLL name 'SelectPalette';
function SNDMSG(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;
function SetAbortProc(_para1:HDC; _para2:TABORTPROC):longint; external GdiDLL name 'SetAbortProc';
function SetActiveWindow(hWnd:HWND):HWND; external UserDLLCore name 'SetActiveWindow';
function SetBkColor(_para1:HDC; _para2:COLORREF):COLORREF; external GdiDLL name 'SetBkColor';
function SetBkMode(_para1:HDC; _para2:longint):longint; external GdiDLL name 'SetBkMode';
function SetBitmapBits(_para1:HBITMAP; _para2:DWORD; _para3:pointer):LONG; external GdiDLL name 'SetBitmapBits';
function SetBrushOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external GdiDLL name 'SetBrushOrgEx';
function SetCapture(hWnd:HWND):HWND; external UserDLLCore name 'SetCapture';
function SetCaretBlinkTime(uMSeconds:UINT):WINBOOL; external UserDLLCore name 'SetCaretBlinkTime';
function SetCaretPos(X:longint; Y:longint):WINBOOL; external UserDLLCore name 'SetCaretPos';
function SetClipboardData(uFormat:UINT; hMem:HANDLE):HANDLE; external UserDLLCore name 'SetClipboardData';
function SetCommBreak(hFile:HANDLE):WINBOOL; external KernelDLL name 'SetCommBreak';
function SetCommMask(hFile:HANDLE; dwEvtMask:DWORD):WINBOOL; external KernelDLL name 'SetCommMask';
function SetCommState(hFile:HANDLE; lpDCB:LPDCB):WINBOOL; external KernelDLL name 'SetCommState';
function SetCommTimeouts(hFile:HANDLE; lpCommTimeouts:LPCOMMTIMEOUTS):WINBOOL; external KernelDLL name 'SetCommTimeouts';
function SetCursor(hCursor:HCURSOR):HCURSOR; external UserDLLCore name 'SetCursor';
function SetCursorPos(X:longint; Y:longint):WINBOOL; external UserDLLCore name 'SetCursorPos';
function SetDIBColorTable(_para1:HDC; _para2:UINT; _para3:UINT; var _para4:RGBQUAD):UINT; external GdiDLL name 'SetDIBColorTable';
function SetDIBitsToDevice(_para1:HDC; _para2:longint; _para3:longint; _para4:DWORD; _para5:DWORD;_para6:longint; _para7:longint; _para8:UINT; _para9:UINT; _para10:pointer;var _para11:BITMAPINFO; _para12:UINT):longint;
  external GdiDLL name 'SetDIBitsToDevice';
function SetDlgItemInt(hDlg:HWND; nIDDlgItem:longint; uValue:UINT; bSigned:WINBOOL):WINBOOL; external UserDLLCore name 'SetDlgItemInt';
function SetEndOfFile(hFile:HANDLE):WINBOOL; external KernelDLL name 'SetEndOfFile';
function SetFilePointer(hFile:HANDLE; lDistanceToMove:LONG; lpDistanceToMoveHigh:PLONG; dwMoveMethod:DWORD):DWORD; external KernelDLL name 'SetFilePointer';
function SetFileTime(hFile:HANDLE; lpCreationTime:LPFILETIME; lpLastAccessTime:LPFILETIME; lpLastWriteTime:LPFILETIME):WINBOOL; external KernelDLL name 'SetFileTime';
function SetFocus(hWnd:HWND):HWND; external UserDLLCore name 'SetFocus';
function SetForegroundWindow(hWnd:HWND):WINBOOL; external UserDLLCore name 'SetForegroundWindow';
procedure SetLastError(dwErrCode:DWORD); external KernelDLL name 'SetLastError';
function SetLocalTime(lpSystemTime:LPSYSTEMTIME):WINBOOL; external KernelDLL name 'SetLocalTime';
function SetPaletteEntries(_para1:HPALETTE; _para2:UINT; _para3:UINT; var _para4:PALETTEENTRY):UINT; external GdiDLL name 'SetPaletteEntries';
function SetParent(hWndChild:HWND; hWndNewParent:HWND):HWND; external UserDLLCore name 'SetParent';
function SetPixel(_para1:HDC; _para2:longint; _para3:longint; _para4:COLORREF):COLORREF; external GdiDLL name 'SetPixel';
function SetRect(lprc:LPRECT; xLeft:longint; yTop:longint; xRight:longint; yBottom:longint):WINBOOL; external UserDLLCore name 'SetRect';
function SetRectEmpty(lprc:LPRECT):WINBOOL; external UserDLLCore name 'SetRectEmpty';
function SetRectRgn(_para1:HRGN; _para2:longint; _para3:longint; _para4:longint; _para5:longint):WINBOOL; external GdiDLL name 'SetRectRgn';
function SetROP2(_para1:HDC; _para2:longint):longint; external GdiDLL name 'SetROP2';
function SetScrollInfo(_para1:HWND; _para2:longint; _para3:LPCSCROLLINFO; _para4:WINBOOL):longint; external UserDLLCore name 'SetScrollInfo';
function SetScrollPos(hWnd:HWND; nBar:longint; nPos:longint; bRedraw:WINBOOL):longint; external UserDLLCore name 'SetScrollPos';
function SetScrollRange(hWnd:HWND; nBar:longint; nMinPos:longint; nMaxPos:longint; bRedraw:WINBOOL):WINBOOL; external UserDLLCore name 'SetScrollRange';
function SetSysColors(cElements:longint; var lpaElements:wINT; var lpaRgbValues:COLORREF):WINBOOL; external UserDLLCore name 'SetSysColors';
function SetSystemTime(lpSystemTime:LPSYSTEMTIME):WINBOOL; external KernelDLL name 'SetSystemTime';
function SetTextColor(_para1:HDC; _para2:COLORREF):COLORREF; external GdiDLL name 'SetTextColor';
function SetTextAlign(_para1:HDC; _para2:UINT):UINT; external GdiDLL name 'SetTextAlign';
function SetTimer(hWnd:HWND; nIDEvent:UINT; uElapse:UINT; lpTimerFunc:TIMERPROC):UINT; external UserDLLCore name 'SetTimer';
function SetTimeZoneInformation(lpTimeZoneInformation:LPTIME_ZONE_INFORMATION):WINBOOL; external KernelDLL name 'SetTimeZoneInformation';
function SetThreadPriority(hThread:HANDLE; nPriority:longint):WINBOOL; external KernelDLL name 'SetThreadPriority';
function SetupComm(hFile:HANDLE; dwInQueue:DWORD; dwOutQueue:DWORD):WINBOOL; external KernelDLL name 'SetupComm';
function SetViewportOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external GdiDLL name 'SetViewportOrgEx';
function SetWindowPos(hWnd:HWND; hWndInsertAfter:HWND; X:longint; Y:longint; cx:longint;cy:longint; uFlags:UINT):WINBOOL; external UserDLLCore name 'SetWindowPos';
function SetWindowRgn(hWnd:HWND; hRgn:HRGN; bRedraw:WINBOOL):longint; external UserDLLCore name 'SetWindowRgn';
procedure SHAddToRecentDocs(_para1:UINT; _para2:LPCVOID); external ShellDLLCore name 'SHAddToRecentDocs';
function SHGetFileInfo(_para1:LPCTSTR; _para2:DWORD; var _para3:SHFILEINFO; _para4:UINT; _para5:UINT):DWORD; external ShellDLLCore name 'SHGetFileInfo';
function SHGetPathFromIDList(_para1:LPCITEMIDLIST; _para2:LPTSTR):WINBOOL; external ShellDLL name 'SHGetPathFromIDList';
function SHGetSpecialFolderLocation(_para1:HWND; _para2:longint; var _para3:LPITEMIDLIST):HRESULT; external ShellDLL name 'SHGetSpecialFolderLocation';
function ShowCursor(bShow:WINBOOL):longint; external UserDLLCore name 'ShowCursor';
function ShowCaret(hWnd:HWND):WINBOOL; external UserDLLCore name 'ShowCaret';
function ShowWindow(hWnd:HWND; nCmdShow:longint):WINBOOL; external UserDLLCore name 'ShowWindow';
function SizeofResource(hModule:HINST; hResInfo:HRSRC):DWORD; external KernelDLL name 'SizeofResource';
procedure Sleep(dwMilliseconds:DWORD); external KernelDLL name 'Sleep';
function StartPage(_para1:HDC):longint; external GdiDLL name 'StartPage';
function StretchBlt(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:HDC; _para7:longint; _para8:longint; _para9:longint; _para10:longint;_para11:DWORD):WINBOOL; external GdiDLL name 'StretchBlt';
function StretchDIBits(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:longint; _para7:longint; _para8:longint; _para9:longint; _para10:pointer;var _para11:BITMAPINFO; _para12:UINT; _para13:DWORD):longint;
  external GdiDLL name 'StretchDIBits';
function SubtractRect(lprcDst:LPRECT; var lprcSrc1:RECT; var lprcSrc2:RECT):WINBOOL; external UserDLLCore name 'SubtractRect';
function SuspendThread(hThread:HANDLE):DWORD; external KernelDLL name 'SuspendThread';
function SystemTimeToFileTime(lpSystemTime:LPSYSTEMTIME; lpFileTime:LPFILETIME):WINBOOL; external KernelDLL name 'SystemTimeToFileTime';
function TabCtrl_GetImageList(hwnd : HWND) : LRESULT;
function TabCtrl_SetImageList(hwnd:HWND;himl : HIMAGELIST) : LRESULT;
function TabCtrl_GetItemCount(hwnd : HWND) : LRESULT;
function TabCtrl_GetItem(hwnd:HWND;iItem : longint;var item : TC_ITEM) : LRESULT;
function TabCtrl_SetItem(hwnd:HWND;iItem : longint;var item : TC_ITEM) : LRESULT;
function TabCtrl_InsertItem(hwnd:HWND;iItem : longint;var item : TC_ITEM) : LRESULT;
function TabCtrl_DeleteItem(hwnd:HWND;i : longint) : LRESULT;
function TabCtrl_DeleteAllItems(hwnd : HWND) : LRESULT;
function TabCtrl_GetItemRect(hwnd:HWND;i : longint;var rc : RECT) : LRESULT;
function TabCtrl_GetCurSel(hwnd : HWND) : LRESULT;
function TabCtrl_SetCurSel(hwnd:HWND;i : longint) : LRESULT;
function TabCtrl_HitTest(hwndTC:HWND;var info : TC_HITTESTINFO) : LRESULT;
function TabCtrl_SetItemExtra(hwndTC:HWND;cb : longint) : LRESULT;
function TabCtrl_AdjustRect(hwnd:HWND;bLarger:WINBOOL;var rc : RECT) : LRESULT;
function TabCtrl_SetItemSize(hwnd:HWND;x,y : longint) : LRESULT;
function TabCtrl_RemoveImage(hwnd:HWND;i : WPARAM) : LRESULT;
function TabCtrl_SetPadding(hwnd:HWND;cx,cy : longint) : LRESULT;
function TabCtrl_GetRowCount(hwnd : HWND) : LRESULT;
function TabCtrl_GetToolTips(hwnd : HWND) : LRESULT;
function TabCtrl_SetToolTips(hwnd:HWND;hwndTT : longint) : LRESULT;
function TabCtrl_GetCurFocus(hwnd : HWND) : LRESULT;
function TabCtrl_SetCurFocus(hwnd:HWND;i : longint) : LRESULT;
function TerminateProcess(hProcess:HANDLE; uExitCode:UINT):WINBOOL; external KernelDLL name 'TerminateProcess';
function TerminateThread(hThread:HANDLE; dwExitCode:DWORD):WINBOOL; external KernelDLL name 'TerminateThread';
function TlsGetValue(dwTlsIndex:DWORD):LPVOID; external KernelDLL name 'TlsGetValue';
function TlsSetValue(dwTlsIndex:DWORD; lpTlsValue:LPVOID):WINBOOL; external KernelDLL name 'TlsSetValue';
function TrackPopupMenuEx(_para1:HMENU; _para2:UINT; _para3:longint; _para4:longint; _para5:HWND;_para6:LPTPMPARAMS):WINBOOL; external UserDLLCore name 'TrackPopupMenuEx';
function TranslateCharsetInfo(var lpSrc:DWORD; lpCs:LPCHARSETINFO; dwFlags:DWORD):WINBOOL; external GdiDLL name 'TranslateCharsetInfo';
function TranslateMessage(lpMsg:LPMSG):WINBOOL; external UserDLLCore name 'TranslateMessage';
function TransmitCommChar(hFile:HANDLE; cChar:char):WINBOOL; external KernelDLL name 'TransmitCommChar';
function TreeView_InsertItem(hwnd:HWND;lpis : LPTV_INSERTSTRUCT) : LRESULT;
function TreeView_DeleteItem(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
function TreeView_DeleteAllItems(hwnd : HWND) : LRESULT;
function TreeView_Expand(hwnd:HWND;hitem:HTREEITEM;code : longint) : LRESULT;
function TreeView_GetCount(hwnd : HWND) : LRESULT;
function TreeView_GetIndent(hwnd : HWND) : LRESULT;
function TreeView_SetIndent(hwnd:HWND;indent : longint) : LRESULT;
function TreeView_GetImageList(hwnd:HWND;iImage : WPARAM) : LRESULT;
function TreeView_SetImageList(hwnd:HWND;himl:HIMAGELIST;iImage : WPARAM) : LRESULT;
function TreeView_GetNextItem(hwnd:HWND;hitem:HTREEITEM;code : longint) : LRESULT;
function TreeView_GetChild(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
function TreeView_GetNextSibling(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
function TreeView_GetPrevSibling(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
function TreeView_GetParent(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
function TreeView_GetFirstVisible(hwnd : HWND) : LRESULT;
function TreeView_GetNextVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
function TreeView_GetPrevVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
function TreeView_GetSelection(hwnd : HWND) : LRESULT;
function TreeView_GetDropHilight(hwnd : HWND) : LRESULT;
function TreeView_GetRoot(hwnd : HWND) : LRESULT;
function TreeView_Select(hwnd:HWND;hitem:HTREEITEM;code : longint) : LRESULT;
function TreeView_SelectItem(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
function TreeView_SelectDropTarget(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
function TreeView_SelectSetFirstVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
function TreeView_GetItem(hwnd:HWND;var item : TV_ITEM) : LRESULT;
function TreeView_SetItem(hwnd:HWND;var item : TV_ITEM) : LRESULT;
function TreeView_EditLabel(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
function TreeView_GetEditControl(hwnd : HWND) : LRESULT;
function TreeView_GetVisibleCount(hwnd : HWND) : LRESULT;
function TreeView_HitTest(hwnd:HWND;lpht : LPTV_HITTESTINFO) : LRESULT;
function TreeView_CreateDragImage(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
function TreeView_SortChildren(hwnd:HWND;hitem:HTREEITEM;recurse : longint) : LRESULT;
function TreeView_EnsureVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
function TreeView_SortChildrenCB(hwnd:HWND;psort:LPTV_SORTCB;recurse : longint) : LRESULT;
function TreeView_EndEditLabelNow(hwnd:HWND;fCancel : longint) : LRESULT;
function TreeView_GetISearchString(hwndTV:HWND;lpsz : LPTSTR) : LRESULT;
function TryEnterCriticalSection(lpCriticalSection:LPCRITICAL_SECTION):WINBOOL; external KernelDLL name 'TryEnterCriticalSection';  //+winbase
function UnhookWindowsHookEx(hhk:HHOOK):WINBOOL; external UserDLLCore name 'UnhookWindowsHookEx';
function UnionRect(lprcDst:LPRECT; var lprcSrc1:RECT; var lprcSrc2:RECT):WINBOOL; external UserDLLCore name 'UnionRect';
function UnmapViewOfFile(lpBaseAddress:LPVOID):WINBOOL; external KernelDLL name 'UnmapViewOfFile';
function UnregisterHotKey(hWnd:HWND; anID:longint):WINBOOL; external UserDLLCore name 'UnregisterHotKey';
function UpdateWindow(hWnd:HWND):WINBOOL; external UserDLLCore name 'UpdateWindow';
function ValidateRect(hWnd:HWND; var lpRect:RECT):WINBOOL; external UserDLLCore name 'ValidateRect';
function ValidateRect(hWnd:HWND;lpRect:LPRECT):WINBOOL; external UserDLLCore name 'ValidateRect';
function ValidateRgn(hWnd:HWND; hRgn:HRGN):WINBOOL; external UserDLLCore name 'ValidateRgn';
function VirtualAlloc(lpAddress:LPVOID; dwSize:DWORD; flAllocationType:DWORD; flProtect:DWORD):LPVOID; external KernelDLL name 'VirtualAlloc';
function VirtualProtect(lpAddress:LPVOID; dwSize:DWORD; flNewProtect:DWORD; lpflOldProtect:PDWORD):WINBOOL; external KernelDLL name 'VirtualProtect';
function VirtualQuery(lpAddress:LPCVOID; lpBuffer:PMEMORY_BASIC_INFORMATION; dwLength:DWORD):DWORD; external KernelDLL name 'VirtualQuery';
function VirtualFree(lpAddress:LPVOID; dwSize:DWORD; dwFreeType:DWORD):WINBOOL; external KernelDLL name 'VirtualFree';
function WriteProcessMemory(hProcess:HANDLE; lpBaseAddress:LPVOID; lpBuffer:LPVOID; nSize:DWORD; lpNumberOfBytesWritten:LPDWORD):WINBOOL; external KernelDLL name 'WriteProcessMemory';
function WaitCommEvent(hFile:HANDLE; lpEvtMask:LPDWORD; lpOverlapped:LPOVERLAPPED):WINBOOL; external KernelDLL name 'WaitCommEvent';
function WaitForDebugEvent(lpDebugEvent:LPDEBUG_EVENT; dwMilliseconds:DWORD):WINBOOL; external KernelDLL name 'WaitForDebugEvent';
function WaitForSingleObject(hHandle:HANDLE; dwMilliseconds:DWORD):DWORD; external KernelDLL name 'WaitForSingleObject';
function WaitForMultipleObjects(nCount:DWORD; lpHandles : PWOHandleArray; bWaitAll:WINBOOL; dwMilliseconds:DWORD):DWORD; external KernelDLL name 'WaitForMultipleObjects';
function WideCharToMultiByte(CodePage:UINT; dwFlags:DWORD; lpWideCharStr:LPCWSTR; cchWideChar:longint; lpMultiByteStr:LPSTR;cchMultiByte:longint; lpDefaultChar:LPCSTR; lpUsedDefaultChar:LPBOOL):longint; external KernelDLL name 'WideCharToMultiByte';
function WNetCloseEnum(hEnum:HANDLE):DWORD; external MprDLLCore name 'WNetCloseEnum';
function WNetDisconnectDialog(hwnd:HWND; dwType:DWORD):DWORD; external MprDLLCore name 'WNetDisconnectDialog';
procedure ZeroMemory(Destination:PVOID; Length:DWORD);

//end common win32 & wince

{$ifdef WINCE}
//begin wince only
function ActivateKeyboardLayout(hkl:HKL; Flags:UINT):HKL; external UserDLLCore name 'ActivateKeyboardLayout';
//redirected to SendDlgItemMessage
function CheckDlgButton(hDlg:HWND; nIDButton:longint; uCheck:UINT):WINBOOL; 
//win32 version redirected
function ChildWindowFromPoint(hWndParent:HWND; Point:POINT):HWND; external UserDLLCore name 'ChildWindowFromPoint';

//redirected to DrawInconEx
function DrawIcon(hDC:HDC; X:longint; Y:longint; hIcon:HICON):WINBOOL; 
function EventModify(hEvent:HANDLE; func:DWORD ):WINBOOL; external KernelDLL name 'EventModify';  //+kfuncs
//redirected to TerminateProcess
procedure ExitProcess(uExitCode:UINT);
//calculated value 
function GetCurrentProcess:HANDLE; 
//calculated value 
function GetCurrentProcessId:DWORD; 
//calculated value 
function GetCurrentThread:HANDLE; 
//calculated value 
function GetCurrentThreadId:DWORD; 
//redirected to LocalAlloc
function GlobalAlloc(uFlags:UINT; dwBytes:DWORD):HGLOBAL; 
//redirected to LocalHandle
function GlobalHandle(pMem:LPCVOID):HGLOBAL; 
//redirected to LocalFree
function GlobalFree(hMem:HGLOBAL):HGLOBAL; 
//redirected to LocalLock
function GlobalLock(hMem:HGLOBAL):LPVOID; 
//redirected to LocalRealloc
function GlobalReAlloc(hMem:HGLOBAL; dwBytes:DWORD; uFlags:UINT):HGLOBAL; 
//redirected to LocalSize
function GlobalSize(hMem:HGLOBAL):DWORD; 
//redirected to LocalUnlock
function GlobalUnlock(hMem:HGLOBAL):WINBOOL; 
//empty
function LocalLock(hMem:HLOCAL):LPVOID; 
//empty
function LocalUnlock(hMem:HLOCAL):WINBOOL; 
//empty
function LocalHandle(pMem:LPCVOID):HLOCAL; 
//redirected to TlsCall
function TlsAlloc:DWORD; 
function TlsCall(p1:DWORD; p2:DWORD):DWORD; external KernelDLL name 'TlsCall';
//redirected to TlsCall
function TlsFree(dwTlsIndex:DWORD):WINBOOL; 
//redirected to MsgWaitForMultipleObjectsEx
function MsgWaitForMultipleObjects(nCount:DWORD; pHandles:LPHANDLE; fWaitAll:WINBOOL; dwMilliseconds:DWORD; dwWakeMask:DWORD):DWORD; 
//redirected to TrackPopupMenuEx
function TrackPopupMenu(hMenu:HMENU; uFlags:UINT; x:longint; y:longint; nReserved:longint;hWnd:HWND; var prcRect:RECT):WINBOOL; 
//redirected to EventModify
function PulseEvent(hEvent:HANDLE):WINBOOL;
//redirected to EventModify
function ResetEvent(hEvent:HANDLE):WINBOOL; 
//redirected to EventModify
function SetEvent(hEvent:HANDLE):WINBOOL; 
//win32 version redirected
function WindowFromPoint(Point:POINT):HWND; external UserDLLCore name 'WindowFromPoint';

// SHGetSpecialFolderPath consts
const
  CSIDL_PROGRAMS           = $0002;
  CSIDL_CONTROLS           = $0003;
  CSIDL_PRINTERS           = $0004;
  CSIDL_PERSONAL           = $0005;
  CSIDL_FAVORITES          = $0006;
  CSIDL_STARTUP            = $0007;
  CSIDL_RECENT             = $0008;
  CSIDL_SENDTO             = $0009;
  CSIDL_BITBUCKET          = $000a;
  CSIDL_STARTMENU          = $000b;
  CSIDL_DESKTOPDIRECTORY   = $0010;
  CSIDL_DRIVES             = $0011;
  CSIDL_NETWORK            = $0012;
  CSIDL_NETHOOD            = $0013;
  CSIDL_FONTS		           = $0014;
  CSIDL_TEMPLATES          = $0015;
  CSIDL_APPDATA            = $001a;
  
function SHGetSpecialFolderPath(hwndOwner: HWND; lpszPath: LPTSTR; nFolder: LongInt; fCreate: BOOL): BOOL;
  external 'coredll' name 'SHGetSpecialFolderPath';

//end wince only
{$endif WINCE}

{$ifdef WIN32}
//begin win32 only
{function AbortPath(_para1:HDC):WINBOOL; external 'gdi32' name 'AbortPath';}
function AccessCheck(pSecurityDescriptor:PSECURITY_DESCRIPTOR; ClientToken:HANDLE; DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; PrivilegeSet:PPRIVILEGE_SET;PrivilegeSetLength:LPDWORD;
  GrantedAccess:LPDWORD; AccessStatus:LPBOOL):WINBOOL; external 'advapi32' name 'AccessCheck';
{$ifdef WIN95}
function ActivateKeyboardLayout(hkl:HKL; Flags:UINT):HKL; external 'user32' name 'ActivateKeyboardLayout';
{$else}
function ActivateKeyboardLayout(hkl:HKL; Flags:UINT):WINBOOL; external 'user32' name 'ActivateKeyboardLayout';
{$endif}
function AddAccessAllowedAce(pAcl:PACL; dwAceRevision:DWORD; AccessMask:DWORD; pSid:PSID):WINBOOL; external 'advapi32' name 'AddAccessAllowedAce';
function AddAccessDeniedAce(pAcl:PACL; dwAceRevision:DWORD; AccessMask:DWORD; pSid:PSID):WINBOOL; external 'advapi32' name 'AddAccessDeniedAce';
function AddAce(pAcl:PACL; dwAceRevision:DWORD; dwStartingAceIndex:DWORD; pAceList:LPVOID; nAceListLength:DWORD):WINBOOL; external 'advapi32' name 'AddAce';
function AddAuditAccessAce(pAcl:PACL; dwAceRevision:DWORD; dwAccessMask:DWORD; pSid:PSID; bAuditSuccess:WINBOOL;bAuditFailure:WINBOOL):WINBOOL; external 'advapi32' name 'AddAuditAccessAce';
function AdjustTokenPrivileges(TokenHandle:HANDLE; DisableAllPrivileges:WINBOOL; NewState:PTOKEN_PRIVILEGES; BufferLength:DWORD; PreviousState:PTOKEN_PRIVILEGES;ReturnLength:PDWORD):WINBOOL; external 'advapi32' name 'AdjustTokenPrivileges';
function AdjustTokenGroups(TokenHandle:HANDLE; ResetToDefault:WINBOOL; NewState:PTOKEN_GROUPS; BufferLength:DWORD; PreviousState:PTOKEN_GROUPS;ReturnLength:PDWORD):WINBOOL; external 'advapi32' name 'AdjustTokenGroups';
function AdjustWindowRect(lpRect:LPRECT; dwStyle:DWORD; bMenu:WINBOOL):WINBOOL; external 'user32' name 'AdjustWindowRect';
function AllocateAndInitializeSid(pIdentifierAuthority:PSID_IDENTIFIER_AUTHORITY; nSubAuthorityCount:BYTE; nSubAuthority0:DWORD; nSubAuthority1:DWORD; nSubAuthority2:DWORD;nSubAuthority3:DWORD; nSubAuthority4:DWORD;
  nSubAuthority5:DWORD; nSubAuthority6:DWORD; nSubAuthority7:DWORD;var pSid:PSID):WINBOOL; external 'advapi32' name 'AllocateAndInitializeSid';
function AllocateLocallyUniqueId(Luid:PLUID):WINBOOL; external 'advapi32' name 'AllocateLocallyUniqueId';
function AllocConsole:WINBOOL; external 'kernel32' name 'AllocConsole';
function AngleArc(_para1:HDC; _para2:longint; _para3:longint; _para4:DWORD; _para5:Single;_para6:Single):WINBOOL; external 'gdi32' name 'AngleArc';
function AnimatePalette(_para1:HPALETTE; _para2:UINT; _para3:UINT; var _para4:PALETTEENTRY):WINBOOL; external 'gdi32' name 'AnimatePalette';
function Animate_Create(hWndP:HWND; id:HMENU;dwStyle:DWORD;hInstance:HINST):HWND;
function Animate_Open(hwnd : HWND;szName : LPTSTR) : LRESULT;
function Animate_Play(hwnd : HWND;from,_to : longint;rep : UINT) : LRESULT;
function Animate_Stop(hwnd : HWND) : LRESULT;
function Animate_Close(hwnd : HWND) : LRESULT;
function Animate_Seek(hwnd : HWND;frame : longint) : LRESULT;
function AnyPopup:WINBOOL; external 'user32' name 'AnyPopup';
function Arc(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:longint; _para7:longint; _para8:longint; _para9:longint):WINBOOL; external 'gdi32' name 'Arc';
function ArcTo(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:longint; _para7:longint; _para8:longint; _para9:longint):WINBOOL; external 'gdi32' name 'ArcTo';
function AreAllAccessesGranted(GrantedAccess:DWORD; DesiredAccess:DWORD):WINBOOL; external 'advapi32' name 'AreAllAccessesGranted';
function AreAnyAccessesGranted(GrantedAccess:DWORD; DesiredAccess:DWORD):WINBOOL; external 'advapi32' name 'AreAnyAccessesGranted';
function AreFileApisANSI:WINBOOL; external 'kernel32' name 'AreFileApisANSI';
function ArrangeIconicWindows(hWnd:HWND):UINT; external 'user32' name 'ArrangeIconicWindows';
function AttachThreadInput(idAttach:DWORD; idAttachTo:DWORD; fAttach:WINBOOL):WINBOOL; external 'user32' name 'AttachThreadInput';
function BackupRead(hFile:HANDLE; lpBuffer:LPBYTE; nNumberOfBytesToRead:DWORD; lpNumberOfBytesRead:LPDWORD; bAbort:WINBOOL;bProcessSecurity:WINBOOL; var lpContext:LPVOID):WINBOOL; external 'kernel32' name 'BackupRead';
function BackupSeek(hFile:HANDLE; dwLowBytesToSeek:DWORD; dwHighBytesToSeek:DWORD; lpdwLowByteSeeked:LPDWORD; lpdwHighByteSeeked:LPDWORD;var lpContext:LPVOID):WINBOOL; external 'kernel32' name 'BackupSeek';
function BackupWrite(hFile:HANDLE; lpBuffer:LPBYTE; nNumberOfBytesToWrite:DWORD; lpNumberOfBytesWritten:LPDWORD; bAbort:WINBOOL;bProcessSecurity:WINBOOL; var lpContext:LPVOID):WINBOOL; external 'kernel32' name 'BackupWrite';
function Beep(dwFreq:DWORD; dwDuration:DWORD):WINBOOL; external 'kernel32' name 'Beep';
function BeginPath(_para1:HDC):WINBOOL; external 'gdi32' name 'BeginPath';
function BroadcastSystemMessage(_para1:DWORD; _para2:LPDWORD; _para3:UINT; _para4:WPARAM; _para5:LPARAM):longint; external 'user32' name 'BroadcastSystemMessage';
function CancelDC(_para1:HDC):WINBOOL; external 'gdi32' name 'CancelDC';
function CascadeWindows(hwndParent:HWND; wHow:UINT; var lpRect:RECT; cKids:UINT; var lpKids:HWND):WORD; external 'user32' name 'CascadeWindows';
function ChangeClipboardChain(hWndRemove:HWND; hWndNewNext:HWND):WINBOOL; external 'user32' name 'ChangeClipboardChain';
//to move in ascfun.inc
function CharNextExA(CodePage:WORD; lpCurrentChar:LPCSTR; dwFlags:DWORD):LPSTR; external 'user32' name 'CharNextExA';
//to move in ascfun.inc
function CharPrevExA(CodePage:WORD; lpStart:LPCSTR; lpCurrentChar:LPCSTR; dwFlags:DWORD):LPSTR; external 'user32' name 'CharPrevExA';
function CheckColorsInGamut(_para1:HDC; _para2:LPVOID; _para3:LPVOID; _para4:DWORD):WINBOOL; external 'gdi32' name 'CheckColorsInGamut';
function CheckDlgButton(hDlg:HWND; nIDButton:longint; uCheck:UINT):WINBOOL; external 'user32' name 'CheckDlgButton';
function ChildWindowFromPoint(hWndParent:HWND; Point:POINT):HWND; { external 'user32' name 'ChildWindowFromPoint';bug 1807 } 
function ChildWindowFromPointEx(_para1:HWND; _para2:POINT; _para3:UINT):HWND; {external 'user32' name 'ChildWindowFromPointEx';}
function ChoosePixelFormat(_para1:HDC; _para2:PPIXELFORMATDESCRIPTOR):longint; external 'gdi32' name 'ChoosePixelFormat';
function Chord(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:longint; _para7:longint; _para8:longint; _para9:longint):WINBOOL; external 'gdi32' name 'Chord';
function CloseDesktop(hDesktop:HDESK):WINBOOL; external 'user32' name 'CloseDesktop';
function CloseEventLog(hEventLog:HANDLE):WINBOOL; external 'advapi32' name 'CloseEventLog';
function CloseFigure(_para1:HDC):WINBOOL; external 'gdi32' name 'CloseFigure';
function CloseMetaFile(_para1:HDC):HMETAFILE; external 'gdi32' name 'CloseMetaFile';
function CloseServiceHandle(hSCObject:SC_HANDLE):WINBOOL; external 'advapi32' name 'CloseServiceHandle';
function CloseWindow(hWnd:HWND):WINBOOL; external 'user32' name 'CloseWindow';
function CloseWindowStation(hWinSta:HWINSTA):WINBOOL; external 'user32' name 'CloseWindowStation';
function ColorMatchToTarget(_para1:HDC; _para2:HDC; _para3:DWORD):WINBOOL; external 'gdi32' name 'ColorMatchToTarget';
function CombineTransform(_para1:LPXFORM; var _para2:XFORM; var _para3:XFORM):WINBOOL; external 'gdi32' name 'CombineTransform';
function ConnectNamedPipe(hNamedPipe:HANDLE; lpOverlapped:LPOVERLAPPED):WINBOOL; external 'kernel32' name 'ConnectNamedPipe';
function ControlService(hService:SC_HANDLE; dwControl:DWORD; lpServiceStatus:LPSERVICE_STATUS):WINBOOL; external 'advapi32' name 'ControlService';
function CopyIcon(hIcon:HICON):HICON; external 'user32' name 'CopyIcon';
function CopyImage(_para1:HANDLE; _para2:UINT; _para3:longint; _para4:longint; _para5:UINT):HICON; external 'user32' name 'CopyImage';
function CopySid(nDestinationSidLength:DWORD; pDestinationSid:PSID; pSourceSid:PSID):WINBOOL; external 'advapi32' name 'CopySid';
function CreateBitmapIndirect(var _para1:BITMAP):HBITMAP; external 'gdi32' name 'CreateBitmapIndirect';
function CreateBrushIndirect(var _para1:LOGBRUSH):HBRUSH; external 'gdi32' name 'CreateBrushIndirect';
function CreateCursor(hInst:HINST; xHotSpot:longint; yHotSpot:longint; nWidth:longint; nHeight:longint;pvANDPlane:pointer; pvXORPlane:pointer):HCURSOR; external 'user32' name 'CreateCursor';
function CreateConsoleScreenBuffer(dwDesiredAccess:DWORD; dwShareMode:DWORD; var lpSecurityAttributes:SECURITY_ATTRIBUTES; dwFlags:DWORD; lpScreenBufferData:LPVOID):HANDLE; external 'kernel32' name 'CreateConsoleScreenBuffer';
function CreateDiscardableBitmap(_para1:HDC; _para2:longint; _para3:longint):HBITMAP; external 'gdi32' name 'CreateDiscardableBitmap';
function CreateDIBitmap(_para1:HDC; var _para2:BITMAPINFOHEADER; _para3:DWORD; _para4:pointer; var _para5:BITMAPINFO;_para6:UINT):HBITMAP; external 'gdi32' name 'CreateDIBitmap';
function CreateDIBPatternBrush(_para1:HGLOBAL; _para2:UINT):HBRUSH; external 'gdi32' name 'CreateDIBPatternBrush';
function CreateEllipticRgn(_para1:longint; _para2:longint; _para3:longint; _para4:longint):HRGN; external 'gdi32' name 'CreateEllipticRgn';
function CreateEllipticRgnIndirect(var _para1:RECT):HRGN; external 'gdi32' name 'CreateEllipticRgnIndirect';
function CreateHalftonePalette(_para1:HDC):HPALETTE; external 'gdi32' name 'CreateHalftonePalette';
function CreateHatchBrush(_para1:longint; _para2:COLORREF):HBRUSH; external 'gdi32' name 'CreateHatchBrush';
function CreateIcon(hInstance:HINST; nWidth:longint; nHeight:longint; cPlanes:BYTE; cBitsPixel:BYTE;var lpbANDbits:BYTE; var lpbXORbits:BYTE):HICON; external 'user32' name 'CreateIcon';
function CreateIconFromResource(presbits:PBYTE; dwResSize:DWORD; fIcon:WINBOOL; dwVer:DWORD):HICON; external 'user32' name 'CreateIconFromResource';
function CreateIconFromResourceEx(presbits:PBYTE; dwResSize:DWORD; fIcon:WINBOOL; dwVer:DWORD; cxDesired:longint;cyDesired:longint; Flags:UINT):HICON; external 'user32' name 'CreateIconFromResourceEx';
function CreateIoCompletionPort(FileHandle:HANDLE; ExistingCompletionPort:HANDLE; CompletionKey:DWORD; NumberOfConcurrentThreads:DWORD):HANDLE; external 'kernel32' name 'CreateIoCompletionPort';
function CreateMappedBitmap(hInstance:HINST; idBitmap:longint; wFlags:UINT; lpColorMap:LPCOLORMAP; iNumMaps:longint):HBITMAP; external 'comctl32' name 'CreateMappedBitmap';
function CreatePipe(hReadPipe:PHANDLE; hWritePipe:PHANDLE; lpPipeAttributes:LPSECURITY_ATTRIBUTES; nSize:DWORD):WINBOOL; external 'kernel32' name 'CreatePipe';
function CreatePolyPolygonRgn(var _para1:POINT; var _para2:wINT; _para3:longint; _para4:longint):HRGN; external 'gdi32' name 'CreatePolyPolygonRgn';
function CreatePolygonRgn(var _para1:POINT; _para2:longint; _para3:longint):HRGN; external 'gdi32' name 'CreatePolygonRgn';
function CreatePrivateObjectSecurity(ParentDescriptor:PSECURITY_DESCRIPTOR; CreatorDescriptor:PSECURITY_DESCRIPTOR; var NewDescriptor:PSECURITY_DESCRIPTOR;
  IsDirectoryObject:WINBOOL; Token:HANDLE;GenericMapping:PGENERIC_MAPPING):WINBOOL; external 'advapi32' name 'CreatePrivateObjectSecurity';
//to move to ascfun.inc
function CreatePropertySheetPageA(lppsp:LPCPROPSHEETPAGE):HPROPSHEETPAGE; external 'comctl32' name 'CreatePropertySheetPageA';
function CreateRemoteThread(hProcess:HANDLE; lpThreadAttributes:LPSECURITY_ATTRIBUTES; dwStackSize:DWORD; lpStartAddress:LPTHREAD_START_ROUTINE; lpParameter:LPVOID;
  dwCreationFlags:DWORD; lpThreadId:LPDWORD):HANDLE; external 'kernel32' name 'CreateRemoteThread';
function CreateRoundRectRgn(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:longint):HRGN; external 'gdi32' name 'CreateRoundRectRgn';
function CreateTapePartition(hDevice:HANDLE; dwPartitionMethod:DWORD; dwCount:DWORD; dwSize:DWORD):DWORD; external 'kernel32' name 'CreateTapePartition';
function DdeAbandonTransaction(_para1:DWORD; _para2:HCONV; _para3:DWORD):BOOL;external 'user32' name 'DdeAbandonTransaction';
function DdeAccessData(_para1:HDDEDATA; _para2:PDWORD):PBYTE;external 'user32' name 'DdeAccessData';
function DdeAddData(_para1:HDDEDATA; _para2:PBYTE; _para3:DWORD; _para4:DWORD):HDDEDATA;external 'user32' name 'DdeAddData';
{ This is only a prototype PM
function DdeCallback(_para1, _para2:UINT; _para3:HCONV; _para4, _para5:HSZ;_para6: HDDEDATA; _para7, _para8:PDWORD):HDDEDATA;external 'user32' name 'DdeCallback';}
function DdeClientTransaction(_para1:PBYTE; _para2:DWORD; _para3:HCONV; _para4:HSZ; _para5:UINT;
           _para6:UINT; _para7:DWORD; _para8:PDWORD):HDDEDATA;external 'user32' name 'DdeClientTransaction';
function DdeCmpStringHandles(_para1:HSZ; _para2:HSZ):longint; external 'user32' name 'DdeCmpStringHandles';
function DdeConnect(_para1:DWORD; _para2:HSZ; _para3:HSZ; var _para4:CONVCONTEXT):HCONV; external 'user32' name 'DdeConnect';
function DdeConnectList(_para1:DWORD; _para2:HSZ; _para3:HSZ; _para4:HCONVLIST; _para5:PCONVCONTEXT):HCONVLIST;external 'user32' name 'DdeConnectList';
function DdeCreateDataHandle(_para1:DWORD; _para2:LPBYTE; _para3:DWORD; _para4:DWORD; _para5:HSZ;_para6:UINT; _para7:UINT):HDDEDATA; external 'user32' name 'DdeCreateDataHandle';
function DdeDisconnect(_para1:HCONV):WINBOOL; external 'user32' name 'DdeDisconnect';
function DdeDisconnectList(_para1:HCONVLIST):BOOL;external 'user32' name 'DdeDisconnectList';
function DdeEnableCallback(_para1:DWORD; _para2:HCONV; _para3:UINT):BOOL;external 'user32' name 'DdeEnableCallback';
function DdeFreeDataHandle(_para1:HDDEDATA):WINBOOL; external 'user32' name 'DdeFreeDataHandle';
function DdeFreeStringHandle(_para1:DWORD;_para2:HSZ):WINBOOL; external 'user32' name 'DdeFreeStringHandle';
function DdeGetData(_para1:HDDEDATA;_para2:LPBYTE; _para3:DWORD; _para4:DWORD):DWORD; external 'user32' name 'DdeGetData';
function DdeGetLastError(_para1:DWORD):UINT; external 'user32' name 'DdeGetLastError';
function DdeImpersonateClient(_para1:HCONV):BOOL;external 'user32' name 'DdeImpersonateClient';
function DdeKeepStringHandle(_para1:DWORD; _para2:HSZ):BOOL;external 'user32' name 'DdeKeepStringHandle';
function DdeNameService(_para1:DWORD; _para2:HSZ; _para3:HSZ; _para4:UINT):HDDEDATA; external 'user32' name 'DdeNameService';
function DdePostAdvise(_para1:DWORD; _para2:HSZ; _para3:HSZ):WINBOOL; external 'user32' name 'DdePostAdvise';
function DdeQueryConvInfo(_para1:HCONV; _para2:DWORD; _para3:PCONVINFO):UINT;external 'user32' name 'DdeQueryConvInfo';
function DdeQueryNextServer(_para1:HCONVLIST; _para2:HCONV):HCONV;external 'user32' name 'DdeQueryNextServer';
function DdeReconnect(_para1:HCONV):HCONV; external 'user32' name 'DdeReconnect';
function DdeSetUserHandle(_para1:HCONV; _para2:DWORD; _para3:DWORD):BOOL;external 'user32' name 'DdeSetUserHandle';
function DdeUnaccessData(_para1:HDDEDATA):BOOL;external 'user32' name 'DdeUnaccessData';
function DdeUninitialize(_para1:DWORD):WINBOOL; external 'user32' name 'DdeUninitialize';
procedure DebugBreak; external 'kernel32' name 'DebugBreak';
function DeleteAce(pAcl:PACL; dwAceIndex:DWORD):WINBOOL; external 'advapi32' name 'DeleteAce';
function DeleteAtom(nAtom:ATOM):ATOM; external 'kernel32' name 'DeleteAtom';
function DeleteColorSpace(_para1:HCOLORSPACE):WINBOOL; external 'gdi32' name 'DeleteColorSpace';
function DeleteMetaFile(_para1:HMETAFILE):WINBOOL; external 'gdi32' name 'DeleteMetaFile';
function DeleteService(hService:SC_HANDLE):WINBOOL; external 'advapi32' name 'DeleteService';
function DeregisterEventSource(hEventLog:HANDLE):WINBOOL; external 'advapi32' name 'DeregisterEventSource';
function DescribePixelFormat(_para1:HDC; _para2:longint; _para3:UINT; _para4:LPPIXELFORMATDESCRIPTOR):longint; external 'gdi32' name 'DescribePixelFormat';
function DestroyCursor(hCursor:HCURSOR):WINBOOL; external 'user32' name 'DestroyCursor';
function DestroyPrivateObjectSecurity(ObjectDescriptor:PSECURITY_DESCRIPTOR):WINBOOL; external 'advapi32' name 'DestroyPrivateObjectSecurity';
function DisconnectNamedPipe(hNamedPipe:HANDLE):WINBOOL; external 'kernel32' name 'DisconnectNamedPipe';
function DosDateTimeToFileTime(wFatDate:WORD; wFatTime:WORD; lpFileTime:LPFILETIME):WINBOOL; external 'kernel32' name 'DosDateTimeToFileTime';
function DPtoLP(_para1:HDC; _para2:LPPOINT; _para3:longint):WINBOOL; external 'gdi32' name 'DPtoLP';
procedure DragAcceptFiles(_para1:HWND; _para2:WINBOOL); external 'shell32' name 'DragAcceptFiles';
function DragDetect(hwnd:HWND; pt:POINT):WINBOOL; {external 'user32' name 'DragDetect';bug 1807 }  
procedure DragFinish(_para1:HDROP); external 'shell32' name 'DragFinish';
function DragObject(_para1:HWND; _para2:HWND; _para3:UINT; _para4:DWORD; _para5:HCURSOR):DWORD; external 'user32' name 'DragObject';
function DragQueryPoint(_para1:HDROP; _para2:LPPOINT):WINBOOL; external 'shell32' name 'DragQueryPoint';
function DrawAnimatedRects(hwnd:HWND; idAni:longint; var lprcFrom:RECT; var lprcTo:RECT):WINBOOL; external 'user32' name 'DrawAnimatedRects';
function DrawCaption(_para1:HWND; _para2:HDC; var _para3:RECT; _para4:UINT):WINBOOL; external 'user32' name 'DrawCaption';
function DrawEscape(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR):longint; external 'gdi32' name 'DrawEscape';
function DrawIcon(hDC:HDC; X:longint; Y:longint; hIcon:HICON):WINBOOL; external 'user32' name 'DrawIcon';
procedure DrawInsert(handParent:HWND; hLB:HWND; nItem:longint); external 'comctl32' name 'DrawInsert';
function DuplicateIcon(_para1:HINST; _para2:HICON):HICON; external 'shell32' name 'DuplicateIcon';
function DuplicateToken(ExistingTokenHandle:HANDLE; ImpersonationLevel:SECURITY_IMPERSONATION_LEVEL; DuplicateTokenHandle:PHANDLE):WINBOOL; external 'advapi32' name 'DuplicateToken';
function EndPath(_para1:HDC):WINBOOL; external 'gdi32' name 'EndPath';
function EnumDesktopWindows(hDesktop:HDESK; lpfn:ENUMWINDOWSPROC; lParam:LPARAM):WINBOOL; external 'user32' name 'EnumDesktopWindows';
function EnumObjects(_para1:HDC; _para2:longint; _para3:ENUMOBJECTSPROC; _para4:LPARAM):longint; external 'gdi32' name 'EnumObjects';
function EraseTape(hDevice:HANDLE; dwEraseType:DWORD; bImmediate:WINBOOL):DWORD; external 'kernel32' name 'EraseTape';
function Escape(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR; _para5:LPVOID):longint; external 'gdi32' name 'Escape';
function EqualSid(pSid1:PSID; pSid2:PSID):WINBOOL; external 'advapi32' name 'EqualSid';
function EqualPrefixSid(pSid1:PSID; pSid2:PSID):WINBOOL; external 'advapi32' name 'EqualPrefixSid';
function EnableScrollBar(hWnd:HWND; wSBflags:UINT; wArrows:UINT):WINBOOL; external 'user32' name 'EnableScrollBar';
function EnumChildWindows(hWndParent:HWND; lpEnumFunc:ENUMWINDOWSPROC; lParam:LPARAM):WINBOOL; external 'user32' name 'EnumChildWindows';
function EnumEnhMetaFile(_para1:HDC; _para2:HENHMETAFILE; _para3:ENHMETAFILEPROC; _para4:LPVOID; var _para5:RECT):WINBOOL; external 'gdi32' name 'EnumEnhMetaFile';
function EnumMetaFile(_para1:HDC; _para2:HMETAFILE; _para3:ENUMMETAFILEPROC; _para4:LPARAM):WINBOOL; external 'gdi32' name 'EnumMetaFile';
function EnumThreadWindows(dwThreadId:DWORD; lpfn:ENUMWINDOWSPROC; lParam:LPARAM):WINBOOL; external 'user32' name 'EnumThreadWindows';
function EnumTaskWindows(hTask:HWND; lpfn:FARPROC; lParam: LPARAM): BOOL;external 'user32' name 'EnumThreadWindows';
function ExcludeUpdateRgn(hDC:HDC; hWnd:HWND):longint; external 'user32' name 'ExcludeUpdateRgn';
procedure ExitProcess(uExitCode:UINT);external 'kernel32' name 'ExitProcess';
function ExtCreatePen(_para1:DWORD; _para2:DWORD; var _para3:LOGBRUSH; _para4:DWORD; var _para5:DWORD):HPEN; external 'gdi32' name 'ExtCreatePen';
function ExtFloodFill(_para1:HDC; _para2:longint; _para3:longint; _para4:COLORREF; _para5:UINT):WINBOOL; external 'gdi32' name 'ExtFloodFill';
function ExtSelectClipRgn(_para1:HDC; _para2:HRGN; _para3:longint):longint; external 'gdi32' name 'ExtSelectClipRgn';
procedure FatalExit(ExitCode:longint); external 'kernel32' name 'FatalExit';
function FileTimeToDosDateTime(lpFileTime:LPFILETIME; lpFatDate:LPWORD; lpFatTime:LPWORD):WINBOOL; external 'kernel32' name 'FileTimeToDosDateTime';
function FillConsoleOutputAttribute(hConsoleOutput:HANDLE; wAttribute:WORD; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfAttrsWritten:LPDWORD):WINBOOL; external 'kernel32' name 'FillConsoleOutputAttribute';
function FillPath(_para1:HDC):WINBOOL; external 'gdi32' name 'FillPath';
function FillRgn(_para1:HDC; _para2:HRGN; _para3:HBRUSH):WINBOOL; external 'gdi32' name 'FillRgn';
function FindFirstFreeAce(pAcl:PACL; var pAce:LPVOID):WINBOOL; external 'advapi32' name 'FindFirstFreeAce';
function FixBrushOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external 'gdi32' name 'FixBrushOrgEx';
function FlashWindow(hWnd:HWND; bInvert:WINBOOL):WINBOOL; external 'user32' name 'FlashWindow';
function FlattenPath(_para1:HDC):WINBOOL; external 'gdi32' name 'FlattenPath';
function FloodFill(_para1:HDC; _para2:longint; _para3:longint; _para4:COLORREF):WINBOOL; external 'gdi32' name 'FloodFill';
function FlushConsoleInputBuffer(hConsoleInput:HANDLE):WINBOOL; external 'kernel32' name 'FlushConsoleInputBuffer';
function FrameRect(hDC:HDC; var lprc:RECT; hbr:HBRUSH):longint; external 'user32' name 'FrameRect';
function FrameRgn(_para1:HDC; _para2:HRGN; _para3:HBRUSH; _para4:longint; _para5:longint):WINBOOL; external 'gdi32' name 'FrameRgn';
function FreeConsole:WINBOOL; external 'kernel32' name 'FreeConsole';
function FreeResource(hResData:HGLOBAL):WINBOOL; external 'kernel32' name 'FreeResource';
function FreeSid(pSid:PSID):PVOID; external 'advapi32' name 'FreeSid';
function GdiComment(_para1:HDC; _para2:UINT; var _para3:BYTE):WINBOOL; external 'gdi32' name 'GdiComment';
function GdiFlush:WINBOOL; external 'gdi32' name 'GdiFlush';
function GdiGetBatchLimit:DWORD; external 'gdi32' name 'GdiGetBatchLimit';
function GdiSetBatchLimit(_para1:DWORD):DWORD; external 'gdi32' name 'GdiSetBatchLimit';
function GenerateConsoleCtrlEvent(dwCtrlEvent:DWORD; dwProcessGroupId:DWORD):WINBOOL; external 'kernel32' name 'GenerateConsoleCtrlEvent';
function GetAce(pAcl:PACL; dwAceIndex:DWORD; var pAce:LPVOID):WINBOOL; external 'advapi32' name 'GetAce';
function GetAclInformation(pAcl:PACL; pAclInformation:LPVOID; nAclInformationLength:DWORD; dwAclInformationClass:ACL_INFORMATION_CLASS):WINBOOL; external 'advapi32' name 'GetAclInformation';
function GetAspectRatioFilterEx(_para1:HDC; _para2:LPSIZE):WINBOOL; external 'gdi32' name 'GetAspectRatioFilterEx';
function GetArcDirection(_para1:HDC):longint; external 'gdi32' name 'GetArcDirection';
function GetBitmapBits(_para1:HBITMAP; _para2:LONG; _para3:LPVOID):LONG; external 'gdi32' name 'GetBitmapBits';
function GetBitmapDimensionEx(_para1:HBITMAP; _para2:LPSIZE):WINBOOL; external 'gdi32' name 'GetBitmapDimensionEx';
function GetBoundsRect(_para1:HDC; _para2:LPRECT; _para3:UINT):UINT; external 'gdi32' name 'GetBoundsRect';
function GetBrushOrgEx(_para1:HDC; _para2:LPPOINT):WINBOOL; external 'gdi32' name 'GetBrushOrgEx';
function GetClassWord(hWnd:HWND; nIndex:longint):WORD; external 'user32' name 'GetClassWord';
function GetClipboardViewer:HWND; external 'user32' name 'GetClipboardViewer';
function GetColorAdjustment(_para1:HDC; _para2:LPCOLORADJUSTMENT):WINBOOL; external 'gdi32' name 'GetColorAdjustment';
function GetColorSpace(_para1:HDC):HANDLE; external 'gdi32' name 'GetColorSpace';
function GetCommConfig(hCommDev:HANDLE; lpCC:LPCOMMCONFIG; lpdwSize:LPDWORD):WINBOOL; external 'kernel32' name 'GetCommConfig';
function GetConsoleCP:UINT; external 'kernel32' name 'GetConsoleCP';
function GetConsoleCursorInfo(hConsoleOutput:HANDLE; lpConsoleCursorInfo:PCONSOLE_CURSOR_INFO):WINBOOL; external 'kernel32' name 'GetConsoleCursorInfo';
function GetConsoleMode(hConsoleHandle:HANDLE; lpMode:LPDWORD):WINBOOL; external 'kernel32' name 'GetConsoleMode';
function GetConsoleOutputCP:UINT; external 'kernel32' name 'GetConsoleOutputCP';
function GetConsoleScreenBufferInfo(hConsoleOutput:HANDLE; lpConsoleScreenBufferInfo:PCONSOLE_SCREEN_BUFFER_INFO):WINBOOL; external 'kernel32' name 'GetConsoleScreenBufferInfo';
function GetCurrentProcess:HANDLE; external 'kernel32' name 'GetCurrentProcess';
function GetCurrentProcessId:DWORD; external 'kernel32' name 'GetCurrentProcessId';
function GetCurrentThread:HANDLE; external 'kernel32' name 'GetCurrentThread';
function GetCurrentThreadId:DWORD; external 'kernel32' name 'GetCurrentThreadId';
function GetDCOrgEx(_para1:HDC; _para2:LPPOINT):WINBOOL; external 'gdi32' name 'GetDCOrgEx';
function GetDeviceGammaRamp(_para1:HDC; _para2:LPVOID):WINBOOL; external 'gdi32' name 'GetDeviceGammaRamp';
function GetDIBits(_para1:HDC; _para2:HBITMAP; _para3:UINT; _para4:UINT; _para5:LPVOID;_para6:LPBITMAPINFO; _para7:UINT):longint; external 'gdi32' name 'GetDIBits';
procedure GetEffectiveClientRect(hWnd:HWND; lprc:LPRECT; lpInfo:LPINT); external 'comctl32' name 'GetEffectiveClientRect';
function GetEnhMetaFileHeader(_para1:HENHMETAFILE; _para2:UINT; _para3:LPENHMETAHEADER):UINT; external 'gdi32' name 'GetEnhMetaFileHeader';
function GetEnhMetaFilePaletteEntries(_para1:HENHMETAFILE; _para2:UINT; _para3:LPPALETTEENTRY):UINT; external 'gdi32' name 'GetEnhMetaFilePaletteEntries';
function GetFileType(hFile:HANDLE):DWORD; external 'kernel32' name 'GetFileType';
function GetFontData(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:LPVOID; _para5:DWORD):DWORD; external 'gdi32' name 'GetFontData';
function GetFontLanguageInfo(_para1:HDC):DWORD; external 'gdi32' name 'GetFontLanguageInfo';
function GetGraphicsMode(_para1:HDC):longint; external 'gdi32' name 'GetGraphicsMode';
function GetHandleInformation(hObject:HANDLE; lpdwFlags:LPDWORD):WINBOOL; external 'kernel32' name 'GetHandleInformation';
function GetIconInfo(hIcon:HICON; piconinfo:PICONINFO):WINBOOL; external 'user32' name 'GetIconInfo';
function GetInputState:WINBOOL; external 'user32' name 'GetInputState';
function GetKeyboardState(lpKeyState:PBYTE):WINBOOL; external 'user32' name 'GetKeyboardState';
function GetKBCodePage:UINT; external 'user32' name 'GetKBCodePage';
function GetKernelObjectSecurity(Handle:HANDLE; RequestedInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external 'advapi32' name 'GetKernelObjectSecurity';
//redirected to internal_GetLargestConsoleWindowSize
function GetLargestConsoleWindowSize(hConsoleOutput:HANDLE):COORD;
function GetLastActivePopup(hWnd:HWND):HWND; external 'user32' name 'GetLastActivePopup';
function GetLengthSid(pSid:PSID):DWORD; external 'advapi32' name 'GetLengthSid';
function GetLogicalDrives:DWORD; external 'kernel32' name 'GetLogicalDrives';
function GetMailslotInfo(hMailslot:HANDLE; lpMaxMessageSize:LPDWORD; lpNextSize:LPDWORD; lpMessageCount:LPDWORD; lpReadTimeout:LPDWORD):WINBOOL; external 'kernel32' name 'GetMailslotInfo';
function GetMapMode(_para1:HDC):longint; external 'gdi32' name 'GetMapMode';
function GetMenu(hWnd:HWND):HMENU; external 'user32' name 'GetMenu';
function GetMenuCheckMarkDimensions:LONG; external 'user32' name 'GetMenuCheckMarkDimensions';
function GetMenuContextHelpId(_para1:HMENU):DWORD; external 'user32' name 'GetMenuContextHelpId';
function GetMenuDefaultItem(hMenu:HMENU; fByPos:UINT; gmdiFlags:UINT):UINT; external 'user32' name 'GetMenuDefaultItem';
function GetMenuItemID(hMenu:HMENU; nPos:longint):UINT; external 'user32' name 'GetMenuItemID';
function GetMenuItemCount(hMenu:HMENU):longint; external 'user32' name 'GetMenuItemCount';
function GetMenuItemRect(hWnd:HWND; hMenu:HMENU; uItem:UINT; lprcItem:LPRECT):WINBOOL; external 'user32' name 'GetMenuItemRect';
function GetMenuState(hMenu:HMENU; uId:UINT; uFlags:UINT):UINT; external 'user32' name 'GetMenuState';
function GetMetaFileBitsEx(_para1:HMETAFILE; _para2:UINT; _para3:LPVOID):UINT; external 'gdi32' name 'GetMetaFileBitsEx';
function GetMetaRgn(_para1:HDC; _para2:HRGN):longint; external 'gdi32' name 'GetMetaRgn';
function GetMessageExtraInfo:LONG; external 'user32' name 'GetMessageExtraInfo';
function GetMessageTime:LONG; external 'user32' name 'GetMessageTime';
function GetMiterLimit(_para1:HDC; _para2:PSingle):WINBOOL; external 'gdi32' name 'GetMiterLimit';
function GetNamedPipeInfo(hNamedPipe:HANDLE; lpFlags:LPDWORD; lpOutBufferSize:LPDWORD; lpInBufferSize:LPDWORD; lpMaxInstances:LPDWORD):WINBOOL; external 'kernel32' name 'GetNamedPipeInfo';
function GetNextWindow(hWnd:HWND; uCmd:UINT):HWND; external 'user32' name 'GetNextWindow';
function GetNumberOfConsoleInputEvents(hConsoleInput:HANDLE; lpNumberOfEvents:PDWORD):WINBOOL; external 'kernel32' name 'GetNumberOfConsoleInputEvents';
function GetNumberOfConsoleMouseButtons(lpNumberOfMouseButtons:LPDWORD):WINBOOL; external 'kernel32' name 'GetNumberOfConsoleMouseButtons';
function GetNumberOfEventLogRecords(hEventLog:HANDLE; NumberOfRecords:PDWORD):WINBOOL; external 'advapi32' name 'GetNumberOfEventLogRecords';
function GetOldestEventLogRecord(hEventLog:HANDLE; OldestRecord:PDWORD):WINBOOL; external 'advapi32' name 'GetOldestEventLogRecord';
function GetPath(_para1:HDC; _para2:LPPOINT; _para3:LPBYTE; _para4:longint):longint; external 'gdi32' name 'GetPath';
function GetPixelFormat(_para1:HDC):longint; external 'gdi32' name 'GetPixelFormat';
function GetPolyFillMode(_para1:HDC):longint; external 'gdi32' name 'GetPolyFillMode';
function GetPriorityClass(hProcess:HANDLE):DWORD; external 'kernel32' name 'GetPriorityClass';
function GetPrivateObjectSecurity(ObjectDescriptor:PSECURITY_DESCRIPTOR; SecurityInformation:SECURITY_INFORMATION; ResultantDescriptor:PSECURITY_DESCRIPTOR;
  DescriptorLength:DWORD; ReturnLength:PDWORD):WINBOOL;external 'advapi32' name 'GetPrivateObjectSecurity';
function GetProcAddress(hModule:HINST; lpProcName:LPCSTR):FARPROC; external 'kernel32' name 'GetProcAddress';
function GetProcessAffinityMask(hProcess:HANDLE; lpProcessAffinityMask:LPDWORD; lpSystemAffinityMask:LPDWORD):WINBOOL; external 'kernel32' name 'GetProcessAffinityMask';
function GetProcessShutdownParameters(lpdwLevel:LPDWORD; lpdwFlags:LPDWORD):WINBOOL; external 'kernel32' name 'GetProcessShutdownParameters';
function GetProcessHeaps(NumberOfHeaps:DWORD; ProcessHeaps:PHANDLE):DWORD; external 'kernel32' name 'GetProcessHeaps';
function GetProcessTimes(hProcess:HANDLE; lpCreationTime:LPFILETIME; lpExitTime:LPFILETIME; lpKernelTime:LPFILETIME; lpUserTime:LPFILETIME):WINBOOL; external 'kernel32' name 'GetProcessTimes';
function GetProcessWindowStation:HWINSTA; external 'user32' name 'GetProcessWindowStation';
function GetProcessWorkingSetSize(hProcess:HANDLE; lpMinimumWorkingSetSize:LPDWORD; lpMaximumWorkingSetSize:LPDWORD):WINBOOL; external 'kernel32' name 'GetProcessWorkingSetSize';
function GetRasterizerCaps(_para1:LPRASTERIZER_STATUS; _para2:UINT):WINBOOL; external 'gdi32' name 'GetRasterizerCaps';
function GetROP2(_para1:HDC):longint; external 'gdi32' name 'GetROP2';
function GetScrollPos(hWnd:HWND; nBar:longint):longint; external 'user32' name 'GetScrollPos';
function GetScrollRange(hWnd:HWND; nBar:longint; lpMinPos:LPINT; lpMaxPos:LPINT):WINBOOL; external 'user32' name 'GetScrollRange';
function GetSecurityDescriptorControl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; pControl:PSECURITY_DESCRIPTOR_CONTROL; lpdwRevision:LPDWORD):WINBOOL; external 'advapi32' name 'GetSecurityDescriptorControl';
function GetSecurityDescriptorDacl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; lpbDaclPresent:LPBOOL; var pDacl:PACL; lpbDaclDefaulted:LPBOOL):WINBOOL; external 'advapi32' name 'GetSecurityDescriptorDacl';
function GetSecurityDescriptorGroup(pSecurityDescriptor:PSECURITY_DESCRIPTOR; var pGroup:PSID; lpbGroupDefaulted:LPBOOL):WINBOOL; external 'advapi32' name 'GetSecurityDescriptorGroup';
function GetSecurityDescriptorLength(pSecurityDescriptor:PSECURITY_DESCRIPTOR):DWORD; external 'advapi32' name 'GetSecurityDescriptorLength';
function GetSecurityDescriptorOwner(pSecurityDescriptor:PSECURITY_DESCRIPTOR; var pOwner:PSID; lpbOwnerDefaulted:LPBOOL):WINBOOL; external 'advapi32' name 'GetSecurityDescriptorOwner';
function GetSecurityDescriptorSacl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; lpbSaclPresent:LPBOOL; var pSacl:PACL; lpbSaclDefaulted:LPBOOL):WINBOOL; external 'advapi32' name 'GetSecurityDescriptorSacl';
function GetSidIdentifierAuthority(pSid:PSID):PSID_IDENTIFIER_AUTHORITY; external 'advapi32' name 'GetSidIdentifierAuthority';
function GetSidLengthRequired(nSubAuthorityCount:UCHAR):DWORD; external 'advapi32' name 'GetSidLengthRequired';
function GetSidSubAuthority(pSid:PSID; nSubAuthority:DWORD):PDWORD; external 'advapi32' name 'GetSidSubAuthority';
function GetSidSubAuthorityCount(pSid:PSID):PUCHAR; external 'advapi32' name 'GetSidSubAuthorityCount';
function GetStdHandle(nStdHandle:DWORD):HANDLE; external 'kernel32' name 'GetStdHandle';
function GetStretchBltMode(_para1:HDC):longint; external 'gdi32' name 'GetStretchBltMode';
function GetSystemPaletteUse(_para1:HDC):UINT; external 'gdi32' name 'GetSystemPaletteUse';
function GetSystemMenu(hWnd:HWND; bRevert:WINBOOL):HMENU; external 'user32' name 'GetSystemMenu';
function GetSystemTimeAdjustment(lpTimeAdjustment:PDWORD; lpTimeIncrement:PDWORD; lpTimeAdjustmentDisabled:PWINBOOL):WINBOOL; external 'kernel32' name 'GetSystemTimeAdjustment';
procedure GetSystemTimeAsFileTime(lpSystemTimeAsFileTime:LPFILETIME); external 'kernel32' name 'GetSystemTimeAsFileTime';
function GetTextCharacterExtra(_para1:HDC):longint; external 'gdi32' name 'GetTextCharacterExtra';
function GetTextCharset(hdc:HDC):longint; external 'gdi32' name 'GetTextCharset';
function GetTextCharsetInfo(hdc:HDC; lpSig:LPFONTSIGNATURE; dwFlags:DWORD):longint; external 'gdi32' name 'GetTextCharsetInfo';
function GetThreadLocale:LCID; external 'kernel32' name 'GetThreadLocale';
function GetTopWindow(hWnd:HWND):HWND; external 'user32' name 'GetTopWindow';
function GetTapeParameters(hDevice:HANDLE; dwOperation:DWORD; lpdwSize:LPDWORD; lpTapeInformation:LPVOID):DWORD; external 'kernel32' name 'GetTapeParameters';
function GetTapePosition(hDevice:HANDLE; dwPositionType:DWORD; lpdwPartition:LPDWORD; lpdwOffsetLow:LPDWORD; lpdwOffsetHigh:LPDWORD):DWORD; external 'kernel32' name 'GetTapePosition';
function GetTapeStatus(hDevice:HANDLE):DWORD; external 'kernel32' name 'GetTapeStatus';
function GetThreadDesktop(dwThreadId:DWORD):HDESK; external 'user32' name 'GetThreadDesktop';
{$ifdef LPLDT_ENTRY}
function GetThreadSelectorEntry(hThread:HANDLE; dwSelector:DWORD; lpSelectorEntry:LPLDT_ENTRY):WINBOOL; external 'kernel32' name 'GetThreadSelectorEntry';
{$endif LPLDT_ENTRY}
function GetTokenInformation(TokenHandle:HANDLE; TokenInformationClass:TOKEN_INFORMATION_CLASS; TokenInformation:LPVOID; TokenInformationLength:DWORD; ReturnLength:PDWORD):WINBOOL; external 'advapi32' name 'GetTokenInformation';
function GetUserObjectSecurity(hObj:HANDLE; pSIRequested:PSECURITY_INFORMATION; pSID:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external 'user32' name 'GetUserObjectSecurity';
function GetVersion:DWORD; external 'kernel32' name 'GetVersion';
function GetViewportExtEx(_para1:HDC; _para2:LPSIZE):WINBOOL; external 'gdi32' name 'GetViewportExtEx';
function GetViewportOrgEx(_para1:HDC; _para2:LPPOINT):WINBOOL; external 'gdi32' name 'GetViewportOrgEx';
function GetWorldTransform(_para1:HDC; _para2:LPXFORM):WINBOOL; external 'gdi32' name 'GetWorldTransform';
function GetWindowContextHelpId(_para1:HWND):DWORD; external 'user32' name 'GetWindowContextHelpId';
function GetWindowExtEx(_para1:HDC; _para2:LPSIZE):WINBOOL; external 'gdi32' name 'GetWindowExtEx';
function GetWindowOrgEx(_para1:HDC; _para2:LPPOINT):WINBOOL; external 'gdi32' name 'GetWindowOrgEx';
function GetWindowPlacement(hWnd:HWND; var lpwndpl:WINDOWPLACEMENT):WINBOOL; external 'user32' name 'GetWindowPlacement';
function GetWindowPlacement(hWnd:HWND; lpwndpl:PWINDOWPLACEMENT):WINBOOL; external 'user32' name 'GetWindowPlacement';
function GetWindowWord(hWnd:HWND; nIndex:longint):WORD; external 'user32' name 'GetWindowWord';
function GetWinMetaFileBits(_para1:HENHMETAFILE; _para2:UINT; _para3:LPBYTE; _para4:wINT; _para5:HDC):UINT; external 'gdi32' name 'GetWinMetaFileBits';
function GlobalAlloc(uFlags:UINT; dwBytes:DWORD):HGLOBAL; external 'kernel32' name 'GlobalAlloc';
function GlobalCompact(dwMinFree:DWORD):UINT; external 'kernel32' name 'GlobalCompact';
procedure GlobalFix(hMem:HGLOBAL); external 'kernel32' name 'GlobalFix';
function GlobalFlags(hMem:HGLOBAL):UINT; external 'kernel32' name 'GlobalFlags';
function GlobalFree(hMem:HGLOBAL):HGLOBAL; external 'kernel32' name 'GlobalFree';
function GlobalHandle(pMem:LPCVOID):HGLOBAL; external 'kernel32' name 'GlobalHandle';
function GlobalLock(hMem:HGLOBAL):LPVOID; external 'kernel32' name 'GlobalLock';
function GlobalReAlloc(hMem:HGLOBAL; dwBytes:DWORD; uFlags:UINT):HGLOBAL; external 'kernel32' name 'GlobalReAlloc';
function GlobalReAllocPtr(lp:Pointer;cbNew,flags:DWord):Pointer;
function GlobalSize(hMem:HGLOBAL):DWORD; external 'kernel32' name 'GlobalSize';
procedure GlobalUnfix(hMem:HGLOBAL); external 'kernel32' name 'GlobalUnfix';
function GlobalUnlock(hMem:HGLOBAL):WINBOOL; external 'kernel32' name 'GlobalUnlock';
function GlobalUnWire(hMem:HGLOBAL):WINBOOL; external 'kernel32' name 'GlobalUnWire';
function GlobalWire(hMem:HGLOBAL):LPVOID; external 'kernel32' name 'GlobalWire';
function HeapCompact(hHeap:HANDLE; dwFlags:DWORD):UINT; external 'kernel32' name 'HeapCompact';
function HeapLock(hHeap:HANDLE):WINBOOL; external 'kernel32' name 'HeapLock';
function HeapUnlock(hHeap:HANDLE):WINBOOL; external 'kernel32' name 'HeapUnlock';
function HeapWalk(hHeap:HANDLE; lpEntry:LPPROCESS_HEAP_ENTRY):WINBOOL; external 'kernel32' name 'HeapWalk';
function HiliteMenuItem(hWnd:HWND; hMenu:HMENU; uIDHiliteItem:UINT; uHilite:UINT):WINBOOL; external 'user32' name 'HiliteMenuItem';
function ImpersonateNamedPipeClient(hNamedPipe:HANDLE):WINBOOL; external 'advapi32' name 'ImpersonateNamedPipeClient';
function ImpersonateLoggedOnUser(hToken:HANDLE):WINBOOL; external 'advapi32' name 'ImpersonateLoggedOnUser';
function ImpersonateSelf(ImpersonationLevel:SECURITY_IMPERSONATION_LEVEL):WINBOOL; external 'advapi32' name 'ImpersonateSelf';
function InitAtomTable(nSize:DWORD):WINBOOL; external 'kernel32' name 'InitAtomTable';
function InitializeAcl(pAcl:PACL; nAclLength:DWORD; dwAclRevision:DWORD):WINBOOL; external 'advapi32' name 'InitializeAcl';
function InitializeSecurityDescriptor(pSecurityDescriptor:PSECURITY_DESCRIPTOR; dwRevision:DWORD):WINBOOL; external 'advapi32' name 'InitializeSecurityDescriptor';
function InitializeSid(Sid:PSID; pIdentifierAuthority:PSID_IDENTIFIER_AUTHORITY; nSubAuthorityCount:BYTE):WINBOOL; external 'advapi32' name 'InitializeSid';
function InvertRgn(_para1:HDC; _para2:HRGN):WINBOOL; external 'gdi32' name 'InvertRgn';
function IsBadHugeReadPtr(lp:pointer; ucb:UINT):WINBOOL; external 'kernel32' name 'IsBadHugeReadPtr';
function IsBadHugeWritePtr(lp:LPVOID; ucb:UINT):WINBOOL; external 'kernel32' name 'IsBadHugeWritePtr';
function IsDlgButtonChecked(hDlg:HWND; nIDButton:longint):UINT; external 'user32' name 'IsDlgButtonChecked';
function IsIconic(hWnd:HWND):WINBOOL; external 'user32' name 'IsIconic';
function IsMenu(hMenu:HMENU):WINBOOL; external 'user32' name 'IsMenu';
function IsTextUnicode(lpBuffer:LPVOID; cb:longint; lpi:LPINT):WINBOOL; external 'advapi32' name 'IsTextUnicode';
function IsValidAcl(pAcl:PACL):WINBOOL; external 'advapi32' name 'IsValidAcl';
function IsValidSecurityDescriptor(pSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL; external 'advapi32' name 'IsValidSecurityDescriptor';
function IsValidSid(pSid:PSID):WINBOOL; external 'advapi32' name 'IsValidSid';
function IsWindowUnicode(hWnd:HWND):WINBOOL; external 'user32' name 'IsWindowUnicode';
function IsZoomed(hWnd:HWND):WINBOOL; external 'user32' name 'IsZoomed';
function LBItemFromPt(hLB:HWND; pt:POINT; bAutoScroll:WINBOOL):longint; { external 'comctl32' name 'LBItemFromPt';}
function LineDDA(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:LINEDDAPROC;_para6:LPARAM):WINBOOL; external 'gdi32' name 'LineDDA';
function LoadModule(lpModuleName:LPCSTR; lpParameterBlock:LPVOID):DWORD; external 'kernel32' name 'LoadModule';
function LocalCompact(uMinFree:UINT):UINT; external 'kernel32' name 'LocalCompact';
function LocalFlags(hMem:HLOCAL):UINT; external 'kernel32' name 'LocalFlags';
function LocalHandle(pMem:LPCVOID):HLOCAL; external 'kernel32' name 'LocalHandle';
function LocalLock(hMem:HLOCAL):LPVOID; external 'kernel32' name 'LocalLock';
function LocalShrink(hMem:HLOCAL; cbNewSize:UINT):UINT; external 'kernel32' name 'LocalShrink';
function LocalUnlock(hMem:HLOCAL):WINBOOL; external 'kernel32' name 'LocalUnlock';
function LockFile(hFile:HANDLE; dwFileOffsetLow:DWORD; dwFileOffsetHigh:DWORD; nNumberOfBytesToLockLow:DWORD; nNumberOfBytesToLockHigh:DWORD):WINBOOL; external 'kernel32' name 'LockFile';
function LockFileEx(hFile:HANDLE; dwFlags:DWORD; dwReserved:DWORD; nNumberOfBytesToLockLow:DWORD; nNumberOfBytesToLockHigh:DWORD;lpOverlapped:LPOVERLAPPED):WINBOOL; external 'kernel32' name 'LockFileEx';
function LockResource(hResData:HGLOBAL):LPVOID; external 'kernel32' name 'LockResource';
function LockServiceDatabase(hSCManager:SC_HANDLE):SC_LOCK; external 'advapi32' name 'LockServiceDatabase';
function LockWindowUpdate(hWndLock:HWND):WINBOOL; external 'user32' name 'LockWindowUpdate';
function LookupIconIdFromDirectory(presbits:PBYTE; fIcon:WINBOOL):longint; external 'user32' name 'LookupIconIdFromDirectory';
function LookupIconIdFromDirectoryEx(presbits:PBYTE; fIcon:WINBOOL; cxDesired:longint; cyDesired:longint; Flags:UINT):longint; external 'user32' name 'LookupIconIdFromDirectoryEx';
function LPtoDP(_para1:HDC; _para2:LPPOINT; _para3:longint):WINBOOL; external 'gdi32' name 'LPtoDP';
function MakeAbsoluteSD(pSelfRelativeSecurityDescriptor:PSECURITY_DESCRIPTOR; pAbsoluteSecurityDescriptor:PSECURITY_DESCRIPTOR; lpdwAbsoluteSecurityDescriptorSize:LPDWORD; pDacl:PACL; lpdwDaclSize:LPDWORD;pSacl:PACL;
  lpdwSaclSize:LPDWORD; pOwner:PSID; lpdwOwnerSize:LPDWORD; pPrimaryGroup:PSID;lpdwPrimaryGroupSize:LPDWORD):WINBOOL; external 'advapi32' name 'MakeAbsoluteSD';
function MakeDragList(hLB:HWND):WINBOOL; external 'comctl32' name 'MakeDragList';
function MakeSelfRelativeSD(pAbsoluteSecurityDescriptor:PSECURITY_DESCRIPTOR; pSelfRelativeSecurityDescriptor:PSECURITY_DESCRIPTOR; lpdwBufferLength:LPDWORD):WINBOOL; external 'advapi32' name 'MakeSelfRelativeSD';
procedure MapGenericMask(AccessMask:PDWORD; GenericMapping:PGENERIC_MAPPING); external 'advapi32' name 'MapGenericMask';
function MapViewOfFile(hFileMappingObject:HANDLE; dwDesiredAccess:DWORD; dwFileOffsetHigh:DWORD; dwFileOffsetLow:DWORD; dwNumberOfBytesToMap:DWORD):LPVOID; external 'kernel32' name 'MapViewOfFile';
function MapViewOfFileEx(hFileMappingObject:HANDLE; dwDesiredAccess:DWORD; dwFileOffsetHigh:DWORD; dwFileOffsetLow:DWORD; dwNumberOfBytesToMap:DWORD;lpBaseAddress:LPVOID):LPVOID; external 'kernel32' name 'MapViewOfFileEx';
procedure MenuHelp(uMsg:UINT; wParam:WPARAM; lParam:LPARAM; hMainMenu:HMENU; hInst:HINST;hwndStatus:HWND; var lpwIDs:UINT); external 'comctl32' name 'MenuHelp';
function MenuItemFromPoint(hWnd:HWND; hMenu:HMENU; ptScreen:POINT):longint; {external 'user32' name 'MenuItemFromPoint';bug 1807 }
function ModifyWorldTransform(_para1:HDC; var _para2:XFORM; _para3:DWORD):WINBOOL; external 'gdi32' name 'ModifyWorldTransform';
function MsgWaitForMultipleObjects(nCount:DWORD; pHandles:LPHANDLE; fWaitAll:WINBOOL; dwMilliseconds:DWORD; dwWakeMask:DWORD):DWORD; external 'user32' name 'MsgWaitForMultipleObjects';
function MulDiv(nNumber:longint; nNumerator:longint; nDenominator:longint):longint; external 'kernel32' name 'MulDiv';
function NotifyBootConfigStatus(BootAcceptable:WINBOOL):WINBOOL; external 'advapi32' name 'NotifyBootConfigStatus';
function NotifyChangeEventLog(hEventLog:HANDLE; hEvent:HANDLE):WINBOOL; external 'advapi32' name 'NotifyChangeEventLog';
function OemKeyScan(wOemChar:WORD):DWORD; external 'user32' name 'OemKeyScan';
function OffsetClipRgn(_para1:HDC; _para2:longint; _para3:longint):longint; external 'gdi32' name 'OffsetClipRgn';
function OffsetViewportOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external 'gdi32' name 'OffsetViewportOrgEx';
function OffsetWindowOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external 'gdi32' name 'OffsetWindowOrgEx';
function OpenFile(lpFileName:LPCSTR; lpReOpenBuff:LPOFSTRUCT; uStyle:UINT):HFILE; external 'kernel32' name 'OpenFile';
function OpenIcon(hWnd:HWND):WINBOOL; external 'user32' name 'OpenIcon';
function OpenInputDesktop(dwFlags:DWORD; fInherit:WINBOOL; dwDesiredAccess:DWORD):HDESK; external 'user32' name 'OpenInputDesktop';
function OpenProcessToken(ProcessHandle:HANDLE; DesiredAccess:DWORD; TokenHandle:PHANDLE):WINBOOL; external 'advapi32' name 'OpenProcessToken';
function OpenThreadToken(ThreadHandle:HANDLE; DesiredAccess:DWORD; OpenAsSelf:WINBOOL; TokenHandle:PHANDLE):WINBOOL; external 'advapi32' name 'OpenThreadToken';
function PaintDesktop(hdc:HDC):WINBOOL; external 'user32' name 'PaintDesktop';
function PaintRgn(_para1:HDC; _para2:HRGN):WINBOOL; external 'gdi32' name 'PaintRgn';
function PathToRegion(_para1:HDC):HRGN; external 'gdi32' name 'PathToRegion';
function PeekNamedPipe(hNamedPipe:HANDLE; lpBuffer:LPVOID; nBufferSize:DWORD; lpBytesRead:LPDWORD; lpTotalBytesAvail:LPDWORD;lpBytesLeftThisMessage:LPDWORD):WINBOOL; external 'kernel32' name 'PeekNamedPipe';
function Pie(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:longint; _para7:longint; _para8:longint; _para9:longint):WINBOOL; external 'gdi32' name 'Pie';
function PlayEnhMetaFileRecord(_para1:HDC; _para2:LPHANDLETABLE; var _para3:ENHMETARECORD; _para4:UINT):WINBOOL; external 'gdi32' name 'PlayEnhMetaFileRecord';
function PlayMetaFile(_para1:HDC; _para2:HMETAFILE):WINBOOL; external 'gdi32' name 'PlayMetaFile';
function PlayMetaFileRecord(_para1:HDC; _para2:LPHANDLETABLE; _para3:LPMETARECORD; _para4:UINT):WINBOOL; external 'gdi32' name 'PlayMetaFileRecord';
function PlgBlt(_para1:HDC; var _para2:POINT; _para3:HDC; _para4:longint; _para5:longint;_para6:longint; _para7:longint; _para8:HBITMAP; _para9:longint; _para10:longint):WINBOOL; external 'gdi32' name 'PlgBlt';
function PolyBezier(_para1:HDC; _para2:LPPOINT; _para3:DWORD):WINBOOL; external 'gdi32' name 'PolyBezier';
function PolyBezierTo(_para1:HDC; _para2:POINT; _para3:DWORD):WINBOOL; external 'gdi32' name 'PolyBezierTo';
function PolyDraw(_para1:HDC; var _para2:POINT; var _para3:BYTE; _para4:longint):WINBOOL; external 'gdi32' name 'PolyDraw';
function PolyPolygon(_para1:HDC; var _para2:POINT; var _para3:wINT; _para4:longint):WINBOOL; external 'gdi32' name 'PolyPolygon';
function PolyPolyline(_para1:HDC; var _para2:POINT; var _para3:DWORD; _para4:DWORD):WINBOOL; external 'gdi32' name 'PolyPolyline';
function PolylineTo(_para1:HDC; _para2:LPPOINT; _para3:DWORD):WINBOOL; external 'gdi32' name 'PolylineTo';
function PrepareTape(hDevice:HANDLE; dwOperation:DWORD; bImmediate:WINBOOL):DWORD; external 'kernel32' name 'PrepareTape';
function PrivilegeCheck(ClientToken:HANDLE; RequiredPrivileges:PPRIVILEGE_SET; pfResult:LPBOOL):WINBOOL; external 'advapi32' name 'PrivilegeCheck';
function PtInRect(var lprc:RECT; pt:POINT):WINBOOL; {external 'user32' name 'PtInRect';bug 1807 }
function PtInRect(lprc:LPRECT; pt:POINT):WINBOOL;
function PtVisible(_para1:HDC; _para2:longint; _para3:longint):WINBOOL; external 'gdi32' name 'PtVisible';
function QueryServiceObjectSecurity(hService:SC_HANDLE; dwSecurityInformation:SECURITY_INFORMATION; lpSecurityDescriptor:PSECURITY_DESCRIPTOR; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL;external 'advapi32' name 'QueryServiceObjectSecurity';
function QueryServiceStatus(hService:SC_HANDLE; lpServiceStatus:LPSERVICE_STATUS):WINBOOL; external 'advapi32' name 'QueryServiceStatus';
function ReadConsoleOutputAttribute(hConsoleOutput:HANDLE; lpAttribute:LPWORD; nLength:DWORD; dwReadCoord:COORD; lpNumberOfAttrsRead:LPDWORD):WINBOOL; external 'kernel32' name 'ReadConsoleOutputAttribute';
function ReadFileEx(hFile:HANDLE; lpBuffer:LPVOID; nNumberOfBytesToRead:DWORD; lpOverlapped:LPOVERLAPPED; lpCompletionRoutine:LPOVERLAPPED_COMPLETION_ROUTINE):WINBOOL; external 'kernel32' name 'ReadFileEx';
function RegSetKeySecurity(hKey:HKEY; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR):LONG; external 'advapi32' name 'RegSetKeySecurity';
function RegGetKeySecurity(hKey:HKEY; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; lpcbSecurityDescriptor:LPDWORD):LONG; external 'advapi32' name 'RegGetKeySecurity';
function RegNotifyChangeKeyValue(hKey:HKEY; bWatchSubtree:WINBOOL; dwNotifyFilter:DWORD; hEvent:HANDLE; fAsynchronus:WINBOOL):LONG; external 'advapi32' name 'RegNotifyChangeKeyValue';
function ReplyMessage(lResult:LRESULT):WINBOOL; external 'user32' name 'ReplyMessage';
function ResetEvent(hEvent:HANDLE):WINBOOL; external 'kernel32' name 'ResetEvent';
function ResizePalette(_para1:HPALETTE; _para2:UINT):WINBOOL; external 'gdi32' name 'ResizePalette';
function RevertToSelf:WINBOOL; external 'advapi32' name 'RevertToSelf';
function ScaleViewportExtEx(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:LPSIZE):WINBOOL; external 'gdi32' name 'ScaleViewportExtEx';
function ScaleWindowExtEx(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:LPSIZE):WINBOOL; external 'gdi32' name 'ScaleWindowExtEx';
function ScrollWindow(hWnd:HWND; XAmount:longint; YAmount:longint; var lpRect:RECT; var lpClipRect:RECT):WINBOOL; external 'user32' name 'ScrollWindow';
function SelectClipPath(_para1:HDC; _para2:longint):WINBOOL; external 'gdi32' name 'SelectClipPath';
function SetAclInformation(pAcl:PACL; pAclInformation:LPVOID; nAclInformationLength:DWORD; dwAclInformationClass:ACL_INFORMATION_CLASS):WINBOOL; external 'advapi32' name 'SetAclInformation';
function SetArcDirection(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetArcDirection';
function SetBitmapDimensionEx(_para1:HBITMAP; _para2:longint; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32' name 'SetBitmapDimensionEx';
function SetBoundsRect(_para1:HDC; var _para2:RECT; _para3:UINT):UINT; external 'gdi32' name 'SetBoundsRect';
function SetClassWord(hWnd:HWND; nIndex:longint; wNewWord:WORD):WORD; external 'user32' name 'SetClassWord';
function SetClipboardViewer(hWndNewViewer:HWND):HWND; external 'user32' name 'SetClipboardViewer';
function SetColorAdjustment(_para1:HDC; var _para2:COLORADJUSTMENT):WINBOOL; external 'gdi32' name 'SetColorAdjustment';
function SetColorSpace(_para1:HDC; _para2:HCOLORSPACE):WINBOOL; external 'gdi32' name 'SetColorSpace';
function SetCommConfig(hCommDev:HANDLE; lpCC:LPCOMMCONFIG; dwSize:DWORD):WINBOOL; external 'kernel32' name 'SetCommConfig';
function SetConsoleActiveScreenBuffer(hConsoleOutput:HANDLE):WINBOOL; external 'kernel32' name 'SetConsoleActiveScreenBuffer';
function SetConsoleCP(wCodePageID:UINT):WINBOOL; external 'kernel32' name 'SetConsoleCP';
function SetConsoleCtrlHandler(HandlerRoutine:PHANDLER_ROUTINE; Add:WINBOOL):WINBOOL; external 'kernel32' name 'SetConsoleCtrlHandler';
function SetConsoleCursorInfo(hConsoleOutput:HANDLE; lpConsoleCursorInfo:PCONSOLE_CURSOR_INFO):WINBOOL; external 'kernel32' name 'SetConsoleCursorInfo';
function SetConsoleCursorPosition(hConsoleOutput:HANDLE; dwCursorPosition:COORD):WINBOOL; external 'kernel32' name 'SetConsoleCursorPosition';
function SetConsoleOutputCP(wCodePageID:UINT):WINBOOL; external 'kernel32' name 'SetConsoleOutputCP';
function SetConsoleScreenBufferSize(hConsoleOutput:HANDLE; dwSize:COORD):WINBOOL; external 'kernel32' name 'SetConsoleScreenBufferSize';
function SetConsoleTextAttribute(hConsoleOutput:HANDLE; wAttributes:WORD):WINBOOL; external 'kernel32' name 'SetConsoleTextAttribute';
function SetConsoleMode(hConsoleHandle:HANDLE; dwMode:DWORD):WINBOOL; external 'kernel32' name 'SetConsoleMode';
function SetConsoleWindowInfo(hConsoleOutput:HANDLE; bAbsolute:WINBOOL; var lpConsoleWindow:SMALL_RECT):WINBOOL; external 'kernel32' name 'SetConsoleWindowInfo';
procedure SetDebugErrorLevel(dwLevel:DWORD); external 'user32' name 'SetDebugErrorLevel';
function SetDeviceGammaRamp(_para1:HDC; _para2:LPVOID):WINBOOL; external 'gdi32' name 'SetDeviceGammaRamp';
function SetDIBits(_para1:HDC; _para2:HBITMAP; _para3:UINT; _para4:UINT; _para5:pointer;_para6:PBITMAPINFO; _para7:UINT):longint; external 'gdi32' name 'SetDIBits';
function SetICMMode(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetICMMode';
function SetDoubleClickTime(_para1:UINT):WINBOOL; external 'user32' name 'SetDoubleClickTime';
function SetEnhMetaFileBits(_para1:UINT; var _para2:BYTE):HENHMETAFILE; external 'gdi32' name 'SetEnhMetaFileBits';
function SetErrorMode(uMode:UINT):UINT; external 'kernel32' name 'SetErrorMode';
function SetEvent(hEvent:HANDLE):WINBOOL; external 'kernel32' name 'SetEvent';
procedure SetFileApisToOEM; external 'kernel32' name 'SetFileApisToOEM';
procedure SetFileApisToANSI; external 'kernel32' name 'SetFileApisToANSI';
function SetGraphicsMode(hdc:HDC; iMode:longint):longint; external 'gdi32' name 'SetGraphicsMode';
function SetHandleInformation(hObject:HANDLE; dwMask:DWORD; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'SetHandleInformation';
function SetHandleCount(uNumber:UINT):UINT; external 'kernel32' name 'SetHandleCount';
function SetKernelObjectSecurity(Handle:HANDLE; SecurityInformation:SECURITY_INFORMATION; SecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL; external 'advapi32' name 'SetKernelObjectSecurity';
function SetKeyboardState(lpKeyState:LPBYTE):WINBOOL; external 'user32' name 'SetKeyboardState';
procedure SetLastErrorEx(dwErrCode:DWORD; dwType:DWORD); external 'user32' name 'SetLastErrorEx';
function SetLayeredWindowAttributes(HWND:hwnd;crKey :COLORREF;bAlpha : byte;dwFlags : DWORD):WINBOOL; external 'user32' name 'SetLayeredWindowAttributes';
function SetMailslotInfo(hMailslot:HANDLE; lReadTimeout:DWORD):WINBOOL; external 'kernel32' name 'SetMailslotInfo';
function SetMapMode(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetMapMode';
function SetMapperFlags(_para1:HDC; _para2:DWORD):DWORD; external 'gdi32' name 'SetMapperFlags';
function SetMenu(hWnd:HWND; hMenu:HMENU):WINBOOL; external 'user32' name 'SetMenu';
function SetMenuContextHelpId(_para1:HMENU; _para2:DWORD):WINBOOL; external 'user32' name 'SetMenuContextHelpId';
function SetMenuDefaultItem(hMenu:HMENU; uItem:UINT; fByPos:UINT):WINBOOL; external 'user32' name 'SetMenuDefaultItem';
function SetMenuItemBitmaps(hMenu:HMENU; uPosition:UINT; uFlags:UINT; hBitmapUnchecked:HBITMAP; hBitmapChecked:HBITMAP):WINBOOL; external 'user32' name 'SetMenuItemBitmaps';
function SetMessageExtraInfo(lParam:LPARAM):LPARAM; external 'user32' name 'SetMessageExtraInfo';
function SetMessageQueue(cMessagesMax:longint):WINBOOL; external 'user32' name 'SetMessageQueue';
function SetMetaRgn(_para1:HDC):longint; external 'gdi32' name 'SetMetaRgn';
function SetMetaFileBitsEx(_para1:UINT; var _para2:BYTE):HMETAFILE; external 'gdi32' name 'SetMetaFileBitsEx';
function SetMiterLimit(_para1:HDC; _para2:Single; _para3:PSingle):WINBOOL; external 'gdi32' name 'SetMiterLimit';
function SetNamedPipeHandleState(hNamedPipe:HANDLE; lpMode:LPDWORD; lpMaxCollectionCount:LPDWORD; lpCollectDataTimeout:LPDWORD):WINBOOL; external 'kernel32' name 'SetNamedPipeHandleState';
function SetPixelV(_para1:HDC; _para2:longint; _para3:longint; _para4:COLORREF):WINBOOL; external 'gdi32' name 'SetPixelV';
function SetPolyFillMode(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetPolyFillMode';
function SetPriorityClass(hProcess:HANDLE; dwPriorityClass:DWORD):WINBOOL; external 'kernel32' name 'SetPriorityClass';
function SetPrivateObjectSecurity(SecurityInformation:SECURITY_INFORMATION; ModificationDescriptor:PSECURITY_DESCRIPTOR; var ObjectsSecurityDescriptor:PSECURITY_DESCRIPTOR; GenericMapping:PGENERIC_MAPPING; Token:HANDLE):WINBOOL;
  external 'advapi32' name 'SetPrivateObjectSecurity';
function SetProcessShutdownParameters(dwLevel:DWORD; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'SetProcessShutdownParameters';
function SetProcessWindowStation(hWinSta:HWINSTA):WINBOOL; external 'user32' name 'SetProcessWindowStation';
function SetProcessWorkingSetSize(hProcess:HANDLE; dwMinimumWorkingSetSize:DWORD; dwMaximumWorkingSetSize:DWORD):WINBOOL; external 'kernel32' name 'SetProcessWorkingSetSize';
function SetSecurityDescriptorDacl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; bDaclPresent:WINBOOL; pDacl:PACL; bDaclDefaulted:WINBOOL):WINBOOL; external 'advapi32' name 'SetSecurityDescriptorDacl';
function SetSecurityDescriptorSacl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; bSaclPresent:WINBOOL; pSacl:PACL; bSaclDefaulted:WINBOOL):WINBOOL; external 'advapi32' name 'SetSecurityDescriptorSacl';
function SetSecurityDescriptorOwner(pSecurityDescriptor:PSECURITY_DESCRIPTOR; pOwner:PSID; bOwnerDefaulted:WINBOOL):WINBOOL; external 'advapi32' name 'SetSecurityDescriptorOwner';
function SetSecurityDescriptorGroup(pSecurityDescriptor:PSECURITY_DESCRIPTOR; pGroup:PSID; bGroupDefaulted:WINBOOL):WINBOOL; external 'advapi32' name 'SetSecurityDescriptorGroup';
function SetStdHandle(nStdHandle:DWORD; hHandle:HANDLE):WINBOOL; external 'kernel32' name 'SetStdHandle';
function SetSystemCursor(hcur:HCURSOR; anID:DWORD):WINBOOL; external 'user32' name 'SetSystemCursor';
function SetSystemTimeAdjustment(dwTimeAdjustment:DWORD; bTimeAdjustmentDisabled:WINBOOL):WINBOOL; external 'kernel32' name 'SetSystemTimeAdjustment';
function SetThreadDesktop(hDesktop:HDESK):WINBOOL; external 'user32' name 'SetThreadDesktop';
function SetTapeParameters(hDevice:HANDLE; dwOperation:DWORD; lpTapeInformation:LPVOID):DWORD; external 'kernel32' name 'SetTapeParameters';
function SetTapePosition(hDevice:HANDLE; dwPositionMethod:DWORD; dwPartition:DWORD; dwOffsetLow:DWORD; dwOffsetHigh:DWORD;bImmediate:WINBOOL):DWORD; external 'kernel32' name 'SetTapePosition';
function SetThreadAffinityMask(hThread:HANDLE; dwThreadAffinityMask:DWORD):DWORD; external 'kernel32' name 'SetThreadAffinityMask';
function SetThreadToken(Thread:PHANDLE; Token:HANDLE):WINBOOL; external 'advapi32' name 'SetThreadToken';
function SetTokenInformation(TokenHandle:HANDLE; TokenInformationClass:TOKEN_INFORMATION_CLASS; TokenInformation:LPVOID; TokenInformationLength:DWORD):WINBOOL; external 'advapi32' name 'SetTokenInformation';
function SetPixelFormat(_para1:HDC; _para2:longint;_para3:PPIXELFORMATDESCRIPTOR):WINBOOL; external 'gdi32' name 'SetPixelFormat';
function SetServiceObjectSecurity(hService:SC_HANDLE; dwSecurityInformation:SECURITY_INFORMATION; lpSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL;external 'advapi32' name 'SetServiceObjectSecurity';
function SetServiceStatus(hServiceStatus:SERVICE_STATUS_HANDLE; lpServiceStatus:LPSERVICE_STATUS):WINBOOL; external 'advapi32' name 'SetServiceStatus';
function SetStretchBltMode(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetStretchBltMode';
function SetSystemPaletteUse(_para1:HDC; _para2:UINT):UINT; external 'gdi32' name 'SetSystemPaletteUse';
function SetTextCharacterExtra(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetTextCharacterExtra';
function SetTextJustification(_para1:HDC; _para2:longint; _para3:longint):WINBOOL; external 'gdi32' name 'SetTextJustification';
function SetThreadLocale(Locale:LCID):WINBOOL; external 'kernel32' name 'SetThreadLocale';
function SetUserObjectSecurity(hObj:HANDLE; pSIRequested:PSECURITY_INFORMATION; pSID:PSECURITY_DESCRIPTOR):WINBOOL; external 'user32' name 'SetUserObjectSecurity';
function SetViewportExtEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32' name 'SetViewportExtEx';
function SetWindowContextHelpId(_para1:HWND; _para2:DWORD):WINBOOL; external 'user32' name 'SetWindowContextHelpId';
function SetWindowExtEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32' name 'SetWindowExtEx';
function SetWindowOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external 'gdi32' name 'SetWindowOrgEx';
function SetWindowPlacement(hWnd:HWND; lpwndpl:PWINDOWPLACEMENT):WINBOOL; external 'user32' name 'SetWindowPlacement';
function SetWindowPlacement(hWnd:HWND; var lpwndpl:WINDOWPLACEMENT):WINBOOL; external 'user32' name 'SetWindowPlacement';
function SetWindowWord(hWnd:HWND; nIndex:longint; wNewWord:WORD):WORD; external 'user32' name 'SetWindowWord';
function SetWinMetaFileBits(_para1:UINT; var _para2:BYTE; _para3:HDC; var _para4:METAFILEPICT):HENHMETAFILE; external 'gdi32' name 'SetWinMetaFileBits';
function SetWorldTransform(_para1:HDC; var _para2:XFORM):WINBOOL; external 'gdi32' name 'SetWorldTransform';
function SHBrowseForFolder(_para1:LPBROWSEINFO):LPITEMIDLIST; external 'shell32' name 'SHBrowseForFolder';
procedure SHChangeNotify(_para1:LONG; _para2:UINT; _para3:LPCVOID; _para4:LPCVOID); external 'shell32' name 'SHChangeNotify';
function SHFileOperation(_para1:LPSHFILEOPSTRUCT):longint; external 'shell32' name 'SHFileOperation';
procedure SHFreeNameMappings(_para1:HANDLE); external 'shell32' name 'SHFreeNameMappings';
function ShowHideMenuCtl(hWnd:HWND; uFlags:UINT; lpInfo:LPINT):WINBOOL; external 'comctl32' name 'ShowHideMenuCtl';
function ShowWindowAsync(hWnd:HWND; nCmdShow:longint):WINBOOL; external 'user32' name 'ShowWindowAsync';
function ShowOwnedPopups(hWnd:HWND; fShow:WINBOOL):WINBOOL; external 'user32' name 'ShowOwnedPopups';
function ShowScrollBar(hWnd:HWND; wBar:longint; bShow:WINBOOL):WINBOOL; external 'user32' name 'ShowScrollBar';
function SleepEx(dwMilliseconds:DWORD; bAlertable:WINBOOL):DWORD; external 'kernel32' name 'SleepEx';
function StrokeAndFillPath(_para1:HDC):WINBOOL; external 'gdi32' name 'StrokeAndFillPath';
function StrokePath(_para1:HDC):WINBOOL; external 'gdi32' name 'StrokePath';
function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation:LPTIME_ZONE_INFORMATION; lpUniversalTime:LPSYSTEMTIME; lpLocalTime:LPSYSTEMTIME):WINBOOL; external 'kernel32' name 'SystemTimeToTzSpecificLocalTime';
function SwapBuffers(_para1:HDC):WINBOOL; external 'gdi32' name 'SwapBuffers';
function SwapMouseButton(fSwap:WINBOOL):WINBOOL; external 'user32' name 'SwapMouseButton';
function SwitchDesktop(hDesktop:HDESK):WINBOOL; external 'user32' name 'SwitchDesktop';
function TlsAlloc:DWORD; external 'kernel32' name 'TlsAlloc';
function TlsFree(dwTlsIndex:DWORD):WINBOOL; external 'kernel32' name 'TlsFree';
function TileWindows(hwndParent:HWND; wHow:UINT; var lpRect:RECT; cKids:UINT; var lpKids:HWND):WORD; external 'user32' name 'TileWindows';
function ToAscii(uVirtKey:UINT; uScanCode:UINT; lpKeyState:PBYTE; lpChar:LPWORD; uFlags:UINT):longint; external 'user32' name 'ToAscii';
function ToAsciiEx(uVirtKey:UINT; uScanCode:UINT; lpKeyState:PBYTE; lpChar:LPWORD; uFlags:UINT;dwhkl:HKL):longint; external 'user32' name 'ToAsciiEx';
function ToUnicode(wVirtKey:UINT; wScanCode:UINT; lpKeyState:PBYTE; pwszBuff:LPWSTR; cchBuff:longint;wFlags:UINT):longint; external 'user32' name 'ToUnicode';
function TrackPopupMenu(hMenu:HMENU; uFlags:UINT; x:longint; y:longint; nReserved:longint;hWnd:HWND; var prcRect:RECT):WINBOOL;  external 'user32' name 'TrackPopupMenu';
function TransactNamedPipe(hNamedPipe:HANDLE; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID; nOutBufferSize:DWORD;lpBytesRead:LPDWORD; lpOverlapped:LPOVERLAPPED):WINBOOL; external 'kernel32' name 'TransactNamedPipe';
function TranslateMDISysAccel(hWndClient:HWND; lpMsg:LPMSG):WINBOOL; external 'user32' name 'TranslateMDISysAccel';
function PulseEvent(hEvent:HANDLE):WINBOOL; external 'kernel32' name 'PulseEvent';
function UnhandledExceptionFilter(ExceptionInfo:lpemptyrecord):LONG; external 'kernel32' name 'UnhandledExceptionFilter';
function UnhookWindowsHook(nCode:longint; pfnFilterProc:HOOKPROC):WINBOOL; external 'user32' name 'UnhookWindowsHook';
function UnloadKeyboardLayout(hkl:HKL):WINBOOL; external 'user32' name 'UnloadKeyboardLayout';
function UnlockFile(hFile:HANDLE; dwFileOffsetLow:DWORD; dwFileOffsetHigh:DWORD; nNumberOfBytesToUnlockLow:DWORD; nNumberOfBytesToUnlockHigh:DWORD):WINBOOL; external 'kernel32' name 'UnlockFile';
function UnlockFileEx(hFile:HANDLE; dwReserved:DWORD; nNumberOfBytesToUnlockLow:DWORD; nNumberOfBytesToUnlockHigh:DWORD; lpOverlapped:LPOVERLAPPED):WINBOOL; external 'kernel32' name 'UnlockFileEx';
function UnlockServiceDatabase(ScLock:SC_LOCK):WINBOOL; external 'advapi32' name 'UnlockServiceDatabase';
function UnrealizeObject(_para1:HGDIOBJ):WINBOOL; external 'gdi32' name 'UnrealizeObject';
function UpdateColors(_para1:HDC):WINBOOL; external 'gdi32' name 'UpdateColors';
function VirtualLock(lpAddress:LPVOID; dwSize:DWORD):WINBOOL; external 'kernel32' name 'VirtualLock';
function VirtualProtectEx(hProcess:HANDLE; lpAddress:LPVOID; dwSize:DWORD; flNewProtect:DWORD; lpflOldProtect:PDWORD):WINBOOL; external 'kernel32' name 'VirtualProtectEx';
function VirtualQueryEx(hProcess:HANDLE; lpAddress:LPCVOID; lpBuffer:PMEMORY_BASIC_INFORMATION; dwLength:DWORD):DWORD; external 'kernel32' name 'VirtualQueryEx';
function VirtualUnlock(lpAddress:LPVOID; dwSize:DWORD):WINBOOL; external 'kernel32' name 'VirtualUnlock';
function WaitForInputIdle(hProcess:HANDLE; dwMilliseconds:DWORD):DWORD; external 'user32' name 'WaitForInputIdle';
function WaitForMultipleObjectsEx(nCount:DWORD; lpHandles:LPHANDLE; bWaitAll:WINBOOL; dwMilliseconds:DWORD; bAlertable:WINBOOL):DWORD; external 'kernel32' name 'WaitForMultipleObjectsEx';
function WaitForSingleObjectEx(hHandle:HANDLE; dwMilliseconds:DWORD; bAlertable:WINBOOL):DWORD; external 'kernel32' name 'WaitForSingleObjectEx';
function WaitMessage:WINBOOL; external 'user32' name 'WaitMessage';
{ wgl Windows OpenGL helper functions }
function wglUseFontBitmaps(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL; external 'opengl32' name 'wglUseFontBitmapsA';
function wglCreateContext(_para1:HDC):HGLRC; external 'opengl32' name 'wglCreateContext';
function wglCreateLayerContext(_para1:HDC; _para2:longint):HGLRC; external 'opengl32' name 'wglCreateLayerContext';
function wglCopyContext(_para1:HGLRC; _para2:HGLRC; _para3:UINT):WINBOOL; external 'opengl32' name 'wglCopyContext';
function wglDeleteContext(_para1:HGLRC):WINBOOL; external 'opengl32' name 'wglDeleteContext';
function wglGetCurrentContext:HGLRC; external 'opengl32' name 'wglGetCurrentContext';
function wglGetCurrentDC:HDC; external 'opengl32' name 'wglGetCurrentDC';
function wglMakeCurrent(_para1:HDC; _para2:HGLRC):WINBOOL; external 'opengl32' name 'wglMakeCurrent';
function wglShareLists(_para1:HGLRC; _para2:HGLRC):WINBOOL; external 'opengl32' name 'wglShareLists';
function wglUseFontBitmapsW(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL; external 'opengl32' name 'wglUseFontBitmapsW';
{ Delphi doesn't declare these, but we do: }
function wglUseFontOutlines(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:Single;
  _para6:Single; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL; external 'opengl32' name 'wglUseFontOutlinesA';
function wglUseFontBitmapsA(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL; external 'opengl32' name 'wglUseFontBitmapsA';
function wglUseFontOutlinesA(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:Single;
  _para6:Single; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL; external 'opengl32' name 'wglUseFontOutlinesA';
function wglDescribeLayerPlane(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; _para5:LPLAYERPLANEDESCRIPTOR):WINBOOL; external 'opengl32' name 'wglDescribeLayerPlane';
function wglGetLayerPaletteEntries(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; var _para5:COLORREF):longint; external 'opengl32' name 'wglGetLayerPaletteEntries';
function wglGetProcAddress(_para1:LPCSTR):PROC; external 'opengl32' name 'wglGetProcAddress';
function wglRealizeLayerPalette(_para1:HDC; _para2:longint; _para3:WINBOOL):WINBOOL; external 'opengl32' name 'wglRealizeLayerPalette';
function wglSetLayerPaletteEntries(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; var _para5:COLORREF):longint; external 'opengl32' name 'wglSetLayerPaletteEntries';
function wglSwapLayerBuffers(_para1:HDC; _para2:UINT):WINBOOL; external 'opengl32' name 'wglSwapLayerBuffers';
function wglUseFontOutlinesW(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:Single;
  _para6:Single; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL; external 'opengl32' name 'wglUseFontOutlinesW';
function WidenPath(_para1:HDC):WINBOOL; external 'gdi32' name 'WidenPath';
function WindowFromDC(hDC:HDC):HWND; external 'user32' name 'WindowFromDC';
function WindowFromPoint(Point:POINT):HWND; { external 'user32' name 'WindowFromPoint';bug 1807 }
function WinExec(lpCmdLine:LPCSTR; uCmdShow:UINT):UINT; external 'kernel32' name 'WinExec';
function WNetConnectionDialog(hwnd:HWND; dwType:DWORD):DWORD; external 'mpr' name 'WNetConnectionDialog';
function WriteConsoleOutputAttribute(hConsoleOutput:HANDLE; var lpAttribute:WORD; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfAttrsWritten:LPDWORD):WINBOOL; external 'kernel32' name 'WriteConsoleOutputAttribute';
function WriteFileEx(hFile:HANDLE; lpBuffer:LPCVOID; nNumberOfBytesToWrite:DWORD; lpOverlapped:LPOVERLAPPED; lpCompletionRoutine:LPOVERLAPPED_COMPLETION_ROUTINE):WINBOOL; external 'kernel32' name 'WriteFileEx';
function WriteTapemark(hDevice:HANDLE; dwTapemarkType:DWORD; dwTapemarkCount:DWORD; bImmediate:WINBOOL):DWORD; external 'kernel32' name 'WriteTapemark';

function _hread(hFile:HFILE; lpBuffer:LPVOID; lBytes:longint):longint; external 'kernel32' name '_hread';
function _hwrite(hFile:HFILE; lpBuffer:LPCSTR; lBytes:longint):longint; external 'kernel32' name '_hwrite';
function _lclose(hFile:HFILE):HFILE; external 'kernel32' name '_lclose';
function _lcreat(lpPathName:LPCSTR; iAttribute:longint):HFILE; external 'kernel32' name '_lcreat';
function _llseek(hFile:HFILE; lOffset:LONG; iOrigin:longint):LONG; external 'kernel32' name '_llseek';
function _lopen(lpPathName:LPCSTR; iReadWrite:longint):HFILE; external 'kernel32' name '_lopen';
function _lread(hFile:HFILE; lpBuffer:LPVOID; uBytes:UINT):UINT; external 'kernel32' name '_lread';
function _lwrite(hFile:HFILE; lpBuffer:LPCSTR; uBytes:UINT):UINT; external 'kernel32' name '_lwrite';

{$ifdef Unknown_functions}
{ WARNING: functions not found !!}
{ WARNING: function is not in my gdi32.dll !! PM}function GetEnhMetaFilePixelFormat(_para1:HENHMETAFILE; _para2:DWORD; var _para3:PIXELFORMATDESCRIPTOR):UINT; external 'gdi32' name 'GetEnhMetaFilePixelFormat';procedure OpenSound; external External_library name 'OpenSound';
{    function GetPixelFormat(_para1:HDC):longint; external 'gdi32' name 'GetPixelFormat'; }
procedure CloseSound; external External_library name 'CloseSound';
procedure StartSound; external External_library name 'StartSound';
procedure StopSound; external External_library name 'StopSound';
function WaitSoundState(nState:DWORD):DWORD; external External_library name 'WaitSoundState';
function SyncAllVoices:DWORD; external External_library name 'SyncAllVoices';
function CountVoiceNotes(nVoice:DWORD):DWORD; external External_library name 'CountVoiceNotes';
function GetThresholdEvent:LPDWORD; external External_library name 'GetThresholdEvent';
function GetThresholdStatus:DWORD; external External_library name 'GetThresholdStatus';
function NetUserEnum(_para1:LPWSTR; _para2:DWORD; _para3:DWORD; var _para4:LPBYTE; _para5:DWORD;_para6:LPDWORD; _para7:LPDWORD; _para8:LPDWORD):DWORD; external 'netapi32' name 'NetUserEnum';
function NetApiBufferFree(_para1:LPVOID):DWORD; external 'netapi32' name 'NetApiBufferFree';
function NetUserGetInfo(_para1:LPWSTR; _para2:LPWSTR; _para3:DWORD; _para4:LPBYTE):DWORD; external 'netapi32' name 'NetUserGetInfo';
function NetGetDCName(_para1:LPWSTR; _para2:LPWSTR; var _para3:LPBYTE):DWORD; external 'netapi32' name 'NetGetDCName';
function NetGroupEnum(_para1:LPWSTR; _para2:DWORD; var _para3:LPBYTE; _para4:DWORD; _para5:LPDWORD;_para6:LPDWORD; _para7:LPDWORD):DWORD; external 'netapi32' name 'NetGroupEnum';
function NetLocalGroupEnum(_para1:LPWSTR; _para2:DWORD; var _para3:LPBYTE; _para4:DWORD; _para5:LPDWORD;_para6:LPDWORD; _para7:LPDWORD):DWORD; external 'netapi32' name 'NetLocalGroupEnum';
function SetSoundNoise(nSource:DWORD; nDuration:DWORD):DWORD; external External_library name 'SetSoundNoise';
function SetVoiceAccent(nVoice:DWORD; nTempo:DWORD; nVolume:DWORD; nMode:DWORD; nPitch:DWORD):DWORD; external External_library name 'SetVoiceAccent';
function SetVoiceEnvelope(nVoice:DWORD; nShape:DWORD; nRepeat:DWORD):DWORD; external External_library name 'SetVoiceEnvelope';
function SetVoiceNote(nVoice:DWORD; nValue:DWORD; nLength:DWORD; nCdots:DWORD):DWORD; external External_library name 'SetVoiceNote';
function SetVoiceQueueSize(nVoice:DWORD; nBytes:DWORD):DWORD; external External_library name 'SetVoiceQueueSize';
function SetVoiceSound(nVoice:DWORD; Frequency:DWORD; nDuration:DWORD):DWORD; external External_library name 'SetVoiceSound';
function SetVoiceThreshold(nVoice:DWORD; nNotes:DWORD):DWORD; external External_library name 'SetVoiceThreshold';
{ WARNING: function not found !!}
function WinMain(hInstance:HINST; hPrevInstance:HINST; lpCmdLine:LPSTR; nShowCmd:longint):longint; external External_library name 'WinMain';
{$endif Unknown_functions}

//end win32 only

//begin win32 or wince not checked

//end win32 or wince not checked

{$endif WIN32}


{$endif read_interface}


{$ifdef read_implementation}


{ Win32 API calling convention
  pushes POINT struct passed by value directly
  on stack instead of just pushing an address
  to overcome this we use a internal function
  that just pushes the two arguments.
  Bug report 1807. PM }

//begin common win32 & wince
function Animate_Create(hWndP:HWND; id:HMENU;dwStyle:DWORD;hInstance:HINST):HWND;
begin
  Animate_Create:=CreateWindow(LPCTSTR(ANIMATE_CLASS),nil,dwStyle,0,0,0,0,hwndP,id,hInstance,nil);
end;


function Animate_Open(hwnd : HWND;szName : LPTSTR) : LRESULT;
begin
  Animate_Open:=SendMessage(hwnd,ACM_OPEN,0,LPARAM(szName));
end;


function Animate_Play(hwnd : HWND;from,_to : longint;rep : UINT) : LRESULT;
begin
  Animate_Play:=SendMessage(hwnd,ACM_PLAY,WPARAM(rep),LPARAM(MAKELONG(from,_to)));
end;


function Animate_Stop(hwnd : HWND) : LRESULT;
begin
  Animate_Stop:=SendMessage(hwnd,ACM_STOP,0,0);
end;

function Animate_Close(hwnd : HWND) : LRESULT;
begin
  Animate_Close:=Animate_Open(hwnd,nil);
end;


function Animate_Seek(hwnd : HWND;frame : longint) : LRESULT;
begin
  Animate_Seek:=Animate_Play(hwnd,frame,frame,1);
end;

function CommDlg_OpenSave_GetSpecA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetSpecA:=SNDMSG(_hdlg,CDM_GETSPEC,WPARAM(_cbmax),LPARAM(_psz));
end;

function CommDlg_OpenSave_GetSpecW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetSpecW:=SNDMSG(_hdlg,CDM_GETSPEC,WPARAM(_cbmax),LPARAM(_psz));
end;


{$ifndef Unicode}
function CommDlg_OpenSave_GetSpec(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetSpec:=SNDMSG(_hdlg,CDM_GETSPEC,WPARAM(_cbmax),LPARAM(_psz));
end;
{$endif Unicode}


function CommDlg_OpenSave_GetFilePathA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFilePathA:=SNDMSG(_hdlg,CDM_GETFILEPATH,WPARAM(_cbmax),LPARAM(_psz));
end;


function CommDlg_OpenSave_GetFilePathW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFilePathW:=SNDMSG(_hdlg,CDM_GETFILEPATH,WPARAM(_cbmax),LPARAM(LPWSTR(_psz)));
end;

{$ifndef Unicode}
function CommDlg_OpenSave_GetFilePath(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFilePath:=SNDMSG(_hdlg,CDM_GETFILEPATH,WPARAM(_cbmax),LPARAM(_psz));
end;
{$endif Unicode}

function CommDlg_OpenSave_GetFolderPathA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFolderPathA:=SNDMSG(_hdlg,CDM_GETFOLDERPATH,WPARAM(_cbmax),LPARAM(LPSTR(_psz)));
end;


function CommDlg_OpenSave_GetFolderPathW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFolderPathW:=SNDMSG(_hdlg,CDM_GETFOLDERPATH,WPARAM(_cbmax),LPARAM(LPWSTR(_psz)));
end;

{$ifndef Unicode}
function CommDlg_OpenSave_GetFolderPath(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFolderPath:=SNDMSG(_hdlg,CDM_GETFOLDERPATH,WPARAM(_cbmax),LPARAM(LPSTR(_psz)));
end;
{$endif Unicode}

function CommDlg_OpenSave_GetFolderIDList(_hdlg:HWND;_pidl:LPVOID;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFolderIDList:=SNDMSG(_hdlg,CDM_GETFOLDERIDLIST,WPARAM(_cbmax),LPARAM(_pidl));
end;


function CommDlg_OpenSave_SetControlText(_hdlg:HWND;_id : longint;_text : LPSTR) : LRESULT;
begin
  CommDlg_OpenSave_SetControlText:=SNDMSG(_hdlg,CDM_SETCONTROLTEXT,WPARAM(_id),LPARAM(_text));
end;


function CommDlg_OpenSave_HideControl(_hdlg:HWND;_id : longint) : LRESULT;
begin
  CommDlg_OpenSave_HideControl:=SNDMSG(_hdlg,CDM_HIDECONTROL,WPARAM(_id),0);
end;


function CommDlg_OpenSave_SetDefExt(_hdlg:HWND;_pszext : LPSTR) : LRESULT;
begin
  CommDlg_OpenSave_SetDefExt:=SNDMSG(_hdlg,CDM_SETDEFEXT,0,LPARAM(_pszext));
end;

procedure CopyMemory(Destination:PVOID; Source:pointer; Length:DWORD);
begin
  Move(Source^, Destination^, Length);
end;

procedure FillMemory(Destination:PVOID; Length:DWORD; Fill:BYTE);
begin
  FillChar(Destination^,Length,Char(Fill));
end;
function GlobalAllocPtr(flags,cb:DWord):Pointer;
begin
  GlobalAllocPtr:=GlobalLock(GlobalAlloc(flags,cb));
end;

function GlobalDiscard(hglbMem:HGLOBAL):HGLOBAL;
begin
  GlobalDiscard:=GlobalReAlloc(hglbMem,0,GMEM_MOVEABLE);
end;

function GlobalFreePtr(lp:Pointer):Pointer;
begin
  GlobalFreePtr:=Pointer(GlobalFree(HWND(GlobalUnlockPtr(lp))));
end;

function GlobalUnlockPtr(lp:pointer):Pointer;
begin
  GlobalUnlock(GlobalHandle(lp));
  GlobalUnlockPtr:=lp;
end;

function GlobalLockPtr(lp:pointer):Pointer;
begin
  GlobalLockPtr:=GlobalLock(GlobalHandle(lp));
end;


function GlobalReAllocPtr(lp:Pointer;cbNew,flags:DWord):Pointer;
begin
  GlobalReAllocPtr:=GlobalLock(GlobalReAlloc(HWND(GlobalUnlockPtr(lp)),cbNew,flags));
end;


function GlobalPtrHandle(lp:pointer):Pointer;
begin
  GlobalPtrHandle:=Pointer(GlobalHandle(lp));
end;

function Header_DeleteItem(hwndHD:HWND;index : longint) : WINBOOL;
begin
  Header_DeleteItem:=WINBOOL(SendMessage(hwndHD,HDM_DELETEITEM,WPARAM(index),0));
end;


function Header_GetItem(hwndHD:HWND;index:longint;var hdi : HD_ITEM) : WINBOOL;
begin
  Header_GetItem:=WINBOOL(SendMessage(hwndHD,HDM_GETITEM,WPARAM(index),LPARAM(@hdi)));
end;


function Header_GetItemCount(hwndHD : HWND) : longint;
begin
  Header_GetItemCount:=longint(SendMessage(hwndHD,HDM_GETITEMCOUNT,0,0));
end;


function Header_InsertItem(hwndHD:HWND;index : longint;var hdi : HD_ITEM) : longint;
begin
  Header_InsertItem:=longint(SendMessage(hwndHD,HDM_INSERTITEM,WPARAM(index),LPARAM(@hdi)));
end;


function Header_Layout(hwndHD:HWND;var layout : HD_LAYOUT) : WINBOOL;
begin
  Header_Layout:=WINBOOL(SendMessage(hwndHD,HDM_LAYOUT,0,LPARAM(@layout)));
end;


function Header_SetItem(hwndHD:HWND;index : longint;var hdi : HD_ITEM) : WINBOOL;
begin
  Header_SetItem:=WINBOOL(SendMessage(hwndHD,HDM_SETITEM,WPARAM(index),LPARAM(@hdi)));
end;

function ImageList_AddIcon(himl:HIMAGELIST; hicon:HICON):longint;
begin
  ImageList_AddIcon:=ImageList_ReplaceIcon(himl,-(1),hicon);
end;

function ListView_Arrange(hwndLV:HWND;code : UINT) : LRESULT;
begin
  ListView_Arrange:=SendMessage(hwndLV,LVM_ARRANGE,WPARAM(UINT(code)),0);
end;


function ListView_CreateDragImage(hwnd:HWND;i : longint;lpptUpLeft : LPPOINT) : LRESULT;
begin
  ListView_CreateDragImage:=SendMessage(hwnd,LVM_CREATEDRAGIMAGE,WPARAM(i),LPARAM(lpptUpLeft));
end;


function ListView_DeleteAllItems(hwnd : HWND) : LRESULT;
begin
  ListView_DeleteAllItems:=SendMessage(hwnd,LVM_DELETEALLITEMS,0,0);
end;


function ListView_DeleteColumn(hwnd:HWND;iCol : longint) : LRESULT;
begin
  ListView_DeleteColumn:=SendMessage(hwnd,LVM_DELETECOLUMN,WPARAM(iCol),0);
end;


function ListView_DeleteItem(hwnd:HWND;iItem : longint) : LRESULT;
begin
  ListView_DeleteItem:=SendMessage(hwnd,LVM_DELETEITEM,WPARAM(iItem),0);
end;


function ListView_EditLabel(hwndLV:HWND;i : longint) : LRESULT;
begin
  ListView_EditLabel:=SendMessage(hwndLV,LVM_EDITLABEL,WPARAM(longint(i)),0);
end;


function ListView_EnsureVisible(hwndLV:HWND;i,fPartialOK : longint) : LRESULT;
begin
  ListView_EnsureVisible:=SendMessage(hwndLV,LVM_ENSUREVISIBLE,WPARAM(i),MAKELPARAM(fPartialOK,0));
end;


function ListView_FindItem(hwnd:HWND;iStart : longint;var lvfi : LV_FINDINFO) : longint;
begin
  ListView_FindItem:=SendMessage(hwnd,LVM_FINDITEM,WPARAM(iStart),LPARAM(@lvfi));
end;


function ListView_GetBkColor(hwnd : HWND) : LRESULT;
begin
  ListView_GetBkColor:=SendMessage(hwnd,LVM_GETBKCOLOR,0,0);
end;


function ListView_GetCallbackMask(hwnd : HWND) : LRESULT;
begin
  ListView_GetCallbackMask:=SendMessage(hwnd,LVM_GETCALLBACKMASK,0,0);
end;


function ListView_GetColumn(hwnd:HWND;iCol : longint;var col : LV_COLUMN) : LRESULT;
begin
  ListView_GetColumn:=SendMessage(hwnd,LVM_GETCOLUMN,WPARAM(iCol),LPARAM(@col));
end;


function ListView_GetColumnWidth(hwnd:HWND;iCol : longint) : LRESULT;
begin
  ListView_GetColumnWidth:=SendMessage(hwnd,LVM_GETCOLUMNWIDTH,WPARAM(iCol),0);
end;


function ListView_GetCountPerPage(hwndLV : HWND) : LRESULT;
begin
  ListView_GetCountPerPage:=SendMessage(hwndLV,LVM_GETCOUNTPERPAGE,0,0);
end;


function ListView_GetEditControl(hwndLV : HWND) : LRESULT;
begin
  ListView_GetEditControl:=SendMessage(hwndLV,LVM_GETEDITCONTROL,0,0);
end;


function ListView_GetImageList(hwnd:HWND;iImageList : wINT) : LRESULT;
begin
  ListView_GetImageList:=SendMessage(hwnd,LVM_GETIMAGELIST,WPARAM(iImageList),0);
end;


function ListView_GetISearchString(hwndLV:HWND;lpsz : LPTSTR) : LRESULT;
begin
  ListView_GetISearchString:=SendMessage(hwndLV,LVM_GETISEARCHSTRING,0,LPARAM(lpsz));
end;


function ListView_GetItem(hwnd:HWND;var item : LV_ITEM) : LRESULT;
begin
  ListView_GetItem:=SendMessage(hwnd,LVM_GETITEM,0,LPARAM(@item));
end;


function ListView_GetItemCount(hwnd : HWND) : LRESULT;
begin
  ListView_GetItemCount:=SendMessage(hwnd,LVM_GETITEMCOUNT,0,0);
end;


function ListView_GetItemPosition(hwndLV:HWND;i : longint;var pt : POINT) : longint;
begin
  ListView_GetItemPosition:=SendMessage(hwndLV,LVM_GETITEMPOSITION,WPARAM(longint(i)),LPARAM(@pt));
end;


function ListView_GetItemSpacing(hwndLV:HWND;fSmall : longint) : LRESULT;
begin
  ListView_GetItemSpacing:=SendMessage(hwndLV,LVM_GETITEMSPACING,fSmall,0);
end;


function ListView_GetItemState(hwndLV:HWND;i,mask : longint) : LRESULT;
begin
  ListView_GetItemState:=SendMessage(hwndLV,LVM_GETITEMSTATE,WPARAM(i),LPARAM(mask));
end;


function ListView_GetNextItem(hwnd:HWND; iStart, flags : longint) : LRESULT;
begin
  ListView_GetNextItem:=SendMessage(hwnd, LVM_GETNEXTITEM, WPARAM(iStart), LPARAM(flags));
end;


function ListView_GetOrigin(hwndLV:HWND;var pt : POINT) : LRESULT;
begin
  ListView_GetOrigin:=SendMessage(hwndLV,LVM_GETORIGIN,WPARAM(0),LPARAM(@pt));
end;


function ListView_GetSelectedCount(hwndLV : HWND) : LRESULT;
begin
  ListView_GetSelectedCount:=SendMessage(hwndLV,LVM_GETSELECTEDCOUNT,0,0);
end;


function ListView_GetStringWidth(hwndLV:HWND;psz : LPCTSTR) : LRESULT;
begin
  ListView_GetStringWidth:=SendMessage(hwndLV,LVM_GETSTRINGWIDTH,0,LPARAM(psz));
end;


function ListView_GetTextBkColor(hwnd : HWND) : LRESULT;
begin
  ListView_GetTextBkColor:=SendMessage(hwnd,LVM_GETTEXTBKCOLOR,0,0);
end;


function ListView_GetTextColor(hwnd : HWND) : LRESULT;
begin
  ListView_GetTextColor:=SendMessage(hwnd,LVM_GETTEXTCOLOR,0,0);
end;


function ListView_GetTopIndex(hwndLV : HWND) : LRESULT;
begin
  ListView_GetTopIndex:=SendMessage(hwndLV,LVM_GETTOPINDEX,0,0);
end;


function ListView_GetViewRect(hwnd:HWND;var rc : RECT) : LRESULT;
begin
  ListView_GetViewRect:=SendMessage(hwnd,LVM_GETVIEWRECT,0,LPARAM(@rc));
end;


function ListView_HitTest(hwndLV:HWND;var info : LV_HITTESTINFO) : LRESULT;
begin
  ListView_HitTest:=SendMessage(hwndLV,LVM_HITTEST,0,LPARAM(@info));
end;


function ListView_InsertColumn(hwnd:HWND;iCol : longint;var col : LV_COLUMN) : LRESULT;
begin
  ListView_InsertColumn:=SendMessage(hwnd,LVM_INSERTCOLUMN,WPARAM(iCol),LPARAM(@col));
end;


function ListView_InsertItem(hwnd:HWND;var item : LV_ITEM) : LRESULT;
begin
  ListView_InsertItem:=SendMessage(hwnd,LVM_INSERTITEM,0,LPARAM(@item));
end;


function ListView_RedrawItems(hwndLV:HWND;iFirst,iLast : longint) : LRESULT;
begin
  ListView_RedrawItems:=SendMessage(hwndLV,LVM_REDRAWITEMS,WPARAM(iFirst),LPARAM(iLast));
end;


function ListView_Scroll(hwndLV:HWND;dx,dy : longint) : LRESULT;
begin
  ListView_Scroll:=SendMessage(hwndLV,LVM_SCROLL,WPARAM(dx),LPARAM(dy));
end;


function ListView_SetBkColor(hwnd:HWND;clrBk : COLORREF) : LRESULT;
begin
  ListView_SetBkColor:=SendMessage(hwnd,LVM_SETBKCOLOR,0,LPARAM(clrBk));
end;


function ListView_SetCallbackMask(hwnd:HWND;mask : UINT) : LRESULT;
begin
  ListView_SetCallbackMask:=SendMessage(hwnd,LVM_SETCALLBACKMASK,WPARAM(mask),0);
end;


function ListView_SetColumn(hwnd:HWND;iCol : longint; var col : LV_COLUMN) : LRESULT;
begin
  ListView_SetColumn:=SendMessage(hwnd,LVM_SETCOLUMN,WPARAM(iCol),LPARAM(@col));
end;


function ListView_SetColumnWidth(hwnd:HWND;iCol,cx : longint) : LRESULT;
begin
  ListView_SetColumnWidth:=SendMessage(hwnd,LVM_SETCOLUMNWIDTH,WPARAM(iCol),MAKELPARAM(cx,0));
end;


function ListView_SetImageList(hwnd:HWND;himl : longint;iImageList : HIMAGELIST) : LRESULT;
begin
  ListView_SetImageList:=SendMessage(hwnd,LVM_SETIMAGELIST,WPARAM(iImageList),LPARAM(UINT(himl)));
end;


function ListView_SetItem(hwnd:HWND;var item : LV_ITEM) : LRESULT;
begin
  ListView_SetItem:=SendMessage(hwnd,LVM_SETITEM,0,LPARAM(@item));
end;


function ListView_SetItemCount(hwndLV:HWND;cItems : longint) : LRESULT;
begin
  ListView_SetItemCount:=SendMessage(hwndLV,LVM_SETITEMCOUNT,WPARAM(cItems),0);
end;


function ListView_SetItemPosition(hwndLV:HWND;i,x,y : longint) : LRESULT;
begin
  ListView_SetItemPosition:=SendMessage(hwndLV,LVM_SETITEMPOSITION,WPARAM(i),MAKELPARAM(x,y));
end;


function ListView_SetItemPosition32(hwndLV:HWND;i,x,y : longint) : LRESULT;
var
  ptNewPos : POINT;
begin
  ptNewPos.x:=x;
  ptNewPos.y:=y;
  ListView_SetItemPosition32:=SendMessage(hwndLV, LVM_SETITEMPOSITION32, WPARAM(i),LPARAM(@ptNewPos));
end;


function ListView_SetItemState(hwndLV:HWND; i, data, mask:longint) : LRESULT;
var
  _gnu_lvi : LV_ITEM;
begin
  _gnu_lvi.stateMask:=mask;
  _gnu_lvi.state:=data;
  ListView_SetItemState:=SendMessage(hwndLV, LVM_SETITEMSTATE, WPARAM(i),LPARAM(@_gnu_lvi));
end;


function ListView_SetItemText(hwndLV:HWND; i, iSubItem_:longint;pszText_ : LPTSTR) : LRESULT;
var
  _gnu_lvi : LV_ITEM;
begin
  _gnu_lvi.iSubItem:=iSubItem_;
  _gnu_lvi.pszText:=pszText_;
  ListView_SetItemText:=SendMessage(hwndLV, LVM_SETITEMTEXT, WPARAM(i),LPARAM(@_gnu_lvi));
end;


function ListView_SetTextBkColor(hwnd:HWND;clrTextBk : COLORREF) : LRESULT;
begin
  ListView_SetTextBkColor:=SendMessage(hwnd,LVM_SETTEXTBKCOLOR,0,LPARAM(clrTextBk));
end;


function ListView_SetTextColor(hwnd:HWND;clrText : COLORREF) : LRESULT;
begin
  ListView_SetTextColor:=SendMessage(hwnd,LVM_SETTEXTCOLOR,0,LPARAM(clrText));
end;


function ListView_SortItems(hwndLV:HWND;_pfnCompare:PFNLVCOMPARE;_lPrm : LPARAM) : LRESULT;
begin
  ListView_SortItems:=SendMessage(hwndLV,LVM_SORTITEMS,WPARAM(_lPrm),LPARAM(_pfnCompare));
end;


function ListView_Update(hwndLV:HWND;i : longint) : LRESULT;
begin
  ListView_Update:=SendMessage(hwndLV,LVM_UPDATE,WPARAM(i),0);
end;

function LocalDiscard(hlocMem:HLOCAL):HLOCAL;
begin
  LocalDiscard := LocalReAlloc(hlocMem,0,LMEM_MOVEABLE);
end;

procedure MoveMemory(Destination:PVOID; Source:pointer; Length:DWORD);
begin
  Move(Source^,Destination^,Length);
end;

function PropSheet_AddPage(hPropSheetDlg : HWND;hpage : HPROPSHEETPAGE) : LRESULT;
begin
  PropSheet_AddPage:=SendMessage(hPropSheetDlg,PSM_ADDPAGE,0,LPARAM(hpage));
end;


function PropSheet_Apply(hPropSheetDlg : HWND) : LRESULT;
begin
  PropSheet_Apply:=SendMessage(hPropSheetDlg,PSM_APPLY,0,0);
end;


function PropSheet_CancelToClose(hPropSheetDlg : HWND) : LRESULT;
begin
  PropSheet_CancelToClose:=SendMessage(hPropSheetDlg,PSM_CANCELTOCLOSE,0,0);
end;


function PropSheet_Changed(hPropSheetDlg,hwndPage : HWND) : LRESULT;
begin
  PropSheet_Changed:=SendMessage(hPropSheetDlg,PSM_CHANGED,WPARAM(hwndPage),0);
end;


function PropSheet_GetCurrentPageHwnd(hDlg : HWND) : LRESULT;
begin
  PropSheet_GetCurrentPageHwnd:=SendMessage(hDlg,PSM_GETCURRENTPAGEHWND,0,0);
end;


function PropSheet_GetTabControl(hPropSheetDlg : HWND) : LRESULT;
begin
  PropSheet_GetTabControl:=SendMessage(hPropSheetDlg,PSM_GETTABCONTROL,0,0);
end;


function PropSheet_IsDialogMessage(hDlg : HWND;pMsg : longint) : LRESULT;
begin
  PropSheet_IsDialogMessage:=SendMessage(hDlg,PSM_ISDIALOGMESSAGE,0,LPARAM(pMsg));
end;


function PropSheet_PressButton(hPropSheetDlg : HWND;iButton : longint) : LRESULT;
begin
  PropSheet_PressButton:=SendMessage(hPropSheetDlg,PSM_PRESSBUTTON,WPARAM(longint(iButton)),0);
end;


function PropSheet_QuerySiblings(hPropSheetDlg : HWND;param1,param2 : longint) : LRESULT;
begin
  PropSheet_QuerySiblings:=SendMessage(hPropSheetDlg,PSM_QUERYSIBLINGS,WPARAM(param1),LPARAM(param2));
end;


function PropSheet_RebootSystem(hPropSheetDlg : HWND) : LRESULT;
begin
  PropSheet_RebootSystem:=SendMessage(hPropSheetDlg,PSM_REBOOTSYSTEM,0,0);
end;


function PropSheet_RemovePage(hPropSheetDlg : HWND;hpage : HPROPSHEETPAGE; index : longint) : LRESULT;
begin
  PropSheet_RemovePage:=SendMessage(hPropSheetDlg,PSM_REMOVEPAGE,WPARAM(index),LPARAM(hpage));
end;


function PropSheet_RestartWindows(hPropSheetDlg : HWND) : LRESULT;
begin
  PropSheet_RestartWindows:=SendMessage(hPropSheetDlg,PSM_RESTARTWINDOWS,0,0);
end;


function PropSheet_SetCurSel(hPropSheetDlg : HWND;hpage : HPROPSHEETPAGE; index : longint) : LRESULT;
begin
  PropSheet_SetCurSel:=SendMessage(hPropSheetDlg,PSM_SETCURSEL,WPARAM(index),LPARAM(hpage));
end;


function PropSheet_SetCurSelByID(hPropSheetDlg : HWND; id : longint) : LRESULT;
begin
  PropSheet_SetCurSelByID:=SendMessage(hPropSheetDlg,PSM_SETCURSELID,0,LPARAM(id));
end;


function PropSheet_SetFinishText(hPropSheetDlg:HWND;lpszText : LPTSTR) : LRESULT;
begin
  PropSheet_SetFinishText:=SendMessage(hPropSheetDlg,PSM_SETFINISHTEXT,0,LPARAM(lpszText));
end;


function PropSheet_SetTitle(hPropSheetDlg:HWND;dwStyle:DWORD;lpszText : LPCTSTR) : LRESULT;
begin
  PropSheet_SetTitle:=SendMessage(hPropSheetDlg,PSM_SETTITLE,WPARAM(dwStyle),LPARAM(lpszText));
end;


function PropSheet_SetWizButtons(hPropSheetDlg:HWND;dwFlags : DWORD) : LRESULT;
begin
  PropSheet_SetWizButtons:=SendMessage(hPropSheetDlg,PSM_SETWIZBUTTONS,0,LPARAM(dwFlags));
end;


function PropSheet_UnChanged(hPropSheetDlg:HWND;hwndPage : HWND) : LRESULT;
begin
  PropSheet_UnChanged:=SendMessage(hPropSheetDlg,PSM_UNCHANGED,WPARAM(hwndPage),0);
end;

function SNDMSG(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;
begin
  SNDMSG:=SendMessage(hWnd,Msg,wParam,lParam);
end;

function TreeView_InsertItem(hwnd:HWND;lpis : LPTV_INSERTSTRUCT) : LRESULT;
begin
  TreeView_InsertItem:=SendMessage(hwnd,TVM_INSERTITEM,0,LPARAM(lpis));
end;

function TabCtrl_GetImageList(hwnd : HWND) : LRESULT;
begin
  TabCtrl_GetImageList:=SendMessage(hwnd,TCM_GETIMAGELIST,0,0);
end;


function TabCtrl_SetImageList(hwnd:HWND;himl : HIMAGELIST) : LRESULT;
begin
  TabCtrl_SetImageList:=SendMessage(hwnd,TCM_SETIMAGELIST,0,LPARAM(UINT(himl)));
end;


function TabCtrl_GetItemCount(hwnd : HWND) : LRESULT;
begin
  TabCtrl_GetItemCount:=SendMessage(hwnd,TCM_GETITEMCOUNT,0,0);
end;


function TabCtrl_GetItem(hwnd:HWND;iItem : longint;var item : TC_ITEM) : LRESULT;
begin
  TabCtrl_GetItem:=SendMessage(hwnd,TCM_GETITEM,WPARAM(iItem),LPARAM(@item));
end;


function TabCtrl_SetItem(hwnd:HWND;iItem : longint;var item : TC_ITEM) : LRESULT;
begin
  TabCtrl_SetItem:=SendMessage(hwnd,TCM_SETITEM,WPARAM(iItem),LPARAM(@item));
end;


function TabCtrl_InsertItem(hwnd:HWND;iItem : longint;var item : TC_ITEM) : LRESULT;
begin
  TabCtrl_InsertItem:=SendMessage(hwnd,TCM_INSERTITEM,WPARAM(iItem),LPARAM(@item));
end;


function TabCtrl_DeleteItem(hwnd:HWND;i : longint) : LRESULT;
begin
  TabCtrl_DeleteItem:=SendMessage(hwnd,TCM_DELETEITEM,WPARAM(i),0);
end;


function TabCtrl_DeleteAllItems(hwnd : HWND) : LRESULT;
begin
  TabCtrl_DeleteAllItems:=SendMessage(hwnd,TCM_DELETEALLITEMS,0,0);
end;


function TabCtrl_GetItemRect(hwnd:HWND;i : longint;var rc : RECT) : LRESULT;
begin
  TabCtrl_GetItemRect:=SendMessage(hwnd,TCM_GETITEMRECT,WPARAM(longint(i)),LPARAM(@rc));
end;


function TabCtrl_GetCurSel(hwnd : HWND) : LRESULT;
begin
  TabCtrl_GetCurSel:=SendMessage(hwnd,TCM_GETCURSEL,0,0);
end;


function TabCtrl_SetCurSel(hwnd:HWND;i : longint) : LRESULT;
begin
  TabCtrl_SetCurSel:=SendMessage(hwnd,TCM_SETCURSEL,WPARAM(i),0);
end;


function TabCtrl_HitTest(hwndTC:HWND;var info : TC_HITTESTINFO) : LRESULT;
begin
  TabCtrl_HitTest:=SendMessage(hwndTC,TCM_HITTEST,0,LPARAM(@info));
end;


function TabCtrl_SetItemExtra(hwndTC:HWND;cb : longint) : LRESULT;
begin
  TabCtrl_SetItemExtra:=SendMessage(hwndTC,TCM_SETITEMEXTRA,WPARAM(cb),0);
end;


function TabCtrl_AdjustRect(hwnd:HWND;bLarger:WINBOOL;var rc : RECT) : LRESULT;
begin
  TabCtrl_AdjustRect:=SendMessage(hwnd,TCM_ADJUSTRECT,WPARAM(bLarger),LPARAM(@rc));
end;


function TabCtrl_SetItemSize(hwnd:HWND;x,y : longint) : LRESULT;
begin
  TabCtrl_SetItemSize:=SendMessage(hwnd,TCM_SETITEMSIZE,0,MAKELPARAM(x,y));
end;


function TabCtrl_RemoveImage(hwnd:HWND;i : WPARAM) : LRESULT;
begin
  TabCtrl_RemoveImage:=SendMessage(hwnd,TCM_REMOVEIMAGE,i,0);
end;


function TabCtrl_SetPadding(hwnd:HWND;cx,cy : longint) : LRESULT;
begin
  TabCtrl_SetPadding:=SendMessage(hwnd,TCM_SETPADDING,0,MAKELPARAM(cx,cy));
end;


function TabCtrl_GetRowCount(hwnd : HWND) : LRESULT;
begin
  TabCtrl_GetRowCount:=SendMessage(hwnd,TCM_GETROWCOUNT,0,0);
end;


function TabCtrl_GetToolTips(hwnd : HWND) : LRESULT;
begin
  TabCtrl_GetToolTips:=SendMessage(hwnd,TCM_GETTOOLTIPS,0,0);
end;


function TabCtrl_SetToolTips(hwnd:HWND;hwndTT : longint) : LRESULT;
begin
  TabCtrl_SetToolTips:=SendMessage(hwnd,TCM_SETTOOLTIPS,WPARAM(hwndTT),0);
end;


function TabCtrl_GetCurFocus(hwnd : HWND) : LRESULT;
begin
  TabCtrl_GetCurFocus:=SendMessage(hwnd,TCM_GETCURFOCUS,0,0);
end;


function TabCtrl_SetCurFocus(hwnd:HWND;i : longint) : LRESULT;
begin
  TabCtrl_SetCurFocus:=SendMessage(hwnd,TCM_SETCURFOCUS,i,0);
end;

function TreeView_DeleteItem(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
begin
  TreeView_DeleteItem:=SendMessage(hwnd,TVM_DELETEITEM,0,LPARAM(hitem));
end;


function TreeView_DeleteAllItems(hwnd : HWND) : LRESULT;
begin
  TreeView_DeleteAllItems:=SendMessage(hwnd,TVM_DELETEITEM,0,LPARAM(TVI_ROOT));
end;


function TreeView_Expand(hwnd:HWND;hitem:HTREEITEM;code : longint) : LRESULT;
begin
  TreeView_Expand:=SendMessage(hwnd,TVM_EXPAND,WPARAM(code),LPARAM(hitem));
end;


function TreeView_GetCount(hwnd : HWND) : LRESULT;
begin
  TreeView_GetCount:=SendMessage(hwnd,TVM_GETCOUNT,0,0);
end;


function TreeView_GetIndent(hwnd : HWND) : LRESULT;
begin
  TreeView_GetIndent:=SendMessage(hwnd,TVM_GETINDENT,0,0);
end;


function TreeView_SetIndent(hwnd:HWND;indent : longint) : LRESULT;
begin
  TreeView_SetIndent:=SendMessage(hwnd,TVM_SETINDENT,WPARAM(indent),0);
end;


function TreeView_GetImageList(hwnd:HWND;iImage : WPARAM) : LRESULT;
begin
  TreeView_GetImageList:=SendMessage(hwnd,TVM_GETIMAGELIST,iImage,0);
end;


function TreeView_SetImageList(hwnd:HWND;himl:HIMAGELIST;iImage : WPARAM) : LRESULT;
begin
  TreeView_SetImageList:=SendMessage(hwnd,TVM_SETIMAGELIST,iImage,LPARAM(UINT(himl)));
end;


function TreeView_GetNextItem(hwnd:HWND;hitem:HTREEITEM;code : longint) : LRESULT;
begin
  TreeView_GetNextItem:=SendMessage(hwnd,TVM_GETNEXTITEM,WPARAM(code),LPARAM(hitem));
end;


function TreeView_GetChild(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
begin
  TreeView_GetChild:=TreeView_GetNextItem(hwnd,hitem,TVGN_CHILD);
end;


function TreeView_GetNextSibling(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
begin
  TreeView_GetNextSibling:=TreeView_GetNextItem(hwnd,hitem,TVGN_NEXT);
end;


function TreeView_GetPrevSibling(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
begin
  TreeView_GetPrevSibling:=TreeView_GetNextItem(hwnd,hitem,TVGN_PREVIOUS);
end;


function TreeView_GetParent(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
begin
  TreeView_GetParent:=TreeView_GetNextItem(hwnd,hitem,TVGN_PARENT);
end;


function TreeView_GetFirstVisible(hwnd : HWND) : LRESULT;
begin
  TreeView_GetFirstVisible:=TreeView_GetNextItem(hwnd,HTREEITEM(nil),TVGN_FIRSTVISIBLE);
end;


function TreeView_GetNextVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
begin
  TreeView_GetNextVisible:=TreeView_GetNextItem(hwnd,hitem,TVGN_NEXTVISIBLE);
end;


function TreeView_GetPrevVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
begin
  TreeView_GetPrevVisible:=TreeView_GetNextItem(hwnd,hitem,TVGN_PREVIOUSVISIBLE);
end;


function TreeView_GetSelection(hwnd : HWND) : LRESULT;
begin
  TreeView_GetSelection:=TreeView_GetNextItem(hwnd,HTREEITEM(nil),TVGN_CARET);
end;


function TreeView_GetDropHilight(hwnd : HWND) : LRESULT;
begin
  TreeView_GetDropHilight:=TreeView_GetNextItem(hwnd,HTREEITEM(nil),TVGN_DROPHILITE);
end;


function TreeView_GetRoot(hwnd : HWND) : LRESULT;
begin
  TreeView_GetRoot:=TreeView_GetNextItem(hwnd,HTREEITEM(nil),TVGN_ROOT);
end;


function TreeView_Select(hwnd:HWND;hitem:HTREEITEM;code : longint) : LRESULT;
begin
  TreeView_Select:=SendMessage(hwnd,TVM_SELECTITEM,WPARAM(code),LPARAM(hitem));
end;


function TreeView_SelectItem(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
begin
  TreeView_SelectItem:=TreeView_Select(hwnd,hitem,TVGN_CARET);
end;


function TreeView_SelectDropTarget(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
begin
  TreeView_SelectDropTarget:=TreeView_Select(hwnd,hitem,TVGN_DROPHILITE);
end;


function TreeView_SelectSetFirstVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
begin
  TreeView_SelectSetFirstVisible:=TreeView_Select(hwnd,hitem,TVGN_FIRSTVISIBLE);
end;


function TreeView_GetItem(hwnd:HWND;var item : TV_ITEM) : LRESULT;
begin
  TreeView_GetItem:=SendMessage(hwnd,TVM_GETITEM,0,LPARAM(@item));
end;


function TreeView_SetItem(hwnd:HWND;var item : TV_ITEM) : LRESULT;
begin
  TreeView_SetItem:=SendMessage(hwnd,TVM_SETITEM,0,LPARAM(@item));
end;


function TreeView_EditLabel(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
begin
  TreeView_EditLabel:=SendMessage(hwnd,TVM_EDITLABEL,0,LPARAM(hitem));
end;


function TreeView_GetEditControl(hwnd : HWND) : LRESULT;
begin
  TreeView_GetEditControl:=SendMessage(hwnd,TVM_GETEDITCONTROL,0,0);
end;


function TreeView_GetVisibleCount(hwnd : HWND) : LRESULT;
begin
  TreeView_GetVisibleCount:=SendMessage(hwnd,TVM_GETVISIBLECOUNT,0,0);
end;


function TreeView_HitTest(hwnd:HWND;lpht : LPTV_HITTESTINFO) : LRESULT;
begin
  TreeView_HitTest:=SendMessage(hwnd,TVM_HITTEST,0,LPARAM(lpht));
end;


function TreeView_CreateDragImage(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
begin
  TreeView_CreateDragImage:=SendMessage(hwnd,TVM_CREATEDRAGIMAGE,0,LPARAM(hitem));
end;


function TreeView_SortChildren(hwnd:HWND;hitem:HTREEITEM;recurse : longint) : LRESULT;
begin
  TreeView_SortChildren:=SendMessage(hwnd,TVM_SORTCHILDREN,WPARAM(recurse),LPARAM(hitem));
end;


function TreeView_EnsureVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
begin
  TreeView_EnsureVisible:=SendMessage(hwnd,TVM_ENSUREVISIBLE,0,LPARAM(hitem));
end;


function TreeView_SortChildrenCB(hwnd:HWND;psort:LPTV_SORTCB;recurse : longint) : LRESULT;
begin
  TreeView_SortChildrenCB:=SendMessage(hwnd,TVM_SORTCHILDRENCB,WPARAM(recurse),LPARAM(psort));
end;


function TreeView_EndEditLabelNow(hwnd:HWND;fCancel : longint) : LRESULT;
begin
  TreeView_EndEditLabelNow:=SendMessage(hwnd,TVM_ENDEDITLABELNOW,WPARAM(fCancel),0);
end;


function TreeView_GetISearchString(hwndTV:HWND;lpsz : LPTSTR) : LRESULT;
begin
  TreeView_GetISearchString:=SendMessage(hwndTV,TVM_GETISEARCHSTRING,0,LPARAM(lpsz));
end;

procedure ZeroMemory(Destination:PVOID; Length:DWORD);
begin
  FillChar(Destination^,Length,#0);
end;

//end common win32 & wince

{$ifdef WINCE}
//begin wince only
function DrawIcon(hDC:HDC; X:longint; Y:longint; hIcon:HICON):WINBOOL; 
begin
 DrawIcon:=DrawIconEx(hdc,x,y,hicon,0,0,0,NULL, DI_NORMAL);
end;

procedure ExitProcess(uExitCode:UINT);
begin
  TerminateProcess (GetCurrentProcess, uExitCode);
end;  

function GetCurrentThread:HANDLE; 
begin
  GetCurrentThread:=SH_CURTHREAD+SYS_HANDLE_BASE;
end;

function GetCurrentThreadId:DWORD; 
begin
 GetCurrentThreadId:=Phandle(PUserKData+SYSHANDLE_OFFSET+SH_CURTHREAD*SizeOf(THandle))^;
end;

function GetCurrentProcess:HANDLE; 
begin
  GetCurrentProcess:=SH_CURPROC+SYS_HANDLE_BASE;
end;

function GetCurrentProcessId:DWORD; 
begin	
 GetCurrentProcessId:=Phandle(PUserKData+SYSHANDLE_OFFSET+SH_CURPROC*SizeOf(THandle))^;
end;

function GlobalAlloc(uFlags:UINT; dwBytes:DWORD):HGLOBAL; 
begin
  GlobalAlloc:=LocalAlloc(uFlags,dwBytes);
end;

function GlobalFree(hMem:HGLOBAL):HGLOBAL; 
begin
  GlobalFree:=LocalFree(hMem);
end;

function GlobalReAlloc(hMem:HGLOBAL; dwBytes:DWORD; uFlags:UINT):HGLOBAL; 
begin
  GlobalReAlloc:=LocalReAlloc(hMem, dwBytes, LMEM_MOVEABLE);
end;

function GlobalSize(hMem:HGLOBAL):DWORD; 
begin
  GlobalSize:=LocalSize(hMem);
end;

function GlobalHandle(pMem:LPCVOID):HGLOBAL; 
begin
  //GlobalHandle:=LocalHandle(pMem);
  GlobalHandle:=HLOCAL(pMem); //see localhandle
end;

function GlobalLock(hMem:HGLOBAL):LPVOID; 
begin
  //GlobalLock:=LocalLock(hMem);
  GlobalLock:=LPVOID(hMem);  //see locallock
end;

function GlobalUnlock(hMem:HGLOBAL):WINBOOL; 
begin
  //GlobalUnlock:=LocalUnlock(hMem);
  GlobalUnlock:=True; //see localunlock
end;

function LocalHandle(pMem:LPCVOID):HLOCAL; 
begin
  LocalHandle:=HLOCAL(pMem);
end;

function LocalLock(hMem:HLOCAL):LPVOID; 
begin
  LocalLock:=LPVOID(hMem);
end;

function LocalUnlock(hMem:HLOCAL):WINBOOL; 
begin
  LocalUnlock:=True;
end;

function MsgWaitForMultipleObjects(nCount:DWORD; pHandles:LPHANDLE; fWaitAll:WINBOOL; dwMilliseconds:DWORD; dwWakeMask:DWORD):DWORD; 
begin
  MsgWaitForMultipleObjects:=MsgWaitForMultipleObjectsEx(nCount,pHandles,dwMilliseconds,dwWakeMask,0);
end;

function TrackPopupMenu(hMenu:HMENU; uFlags:UINT; x:longint; y:longint; nReserved:longint;hWnd:HWND; var prcRect:RECT):WINBOOL; 
begin
  TrackPopupMenu:=TrackPopupMenuEx(hMenu,uFlags,x,y,hWnd,nil);
end;

function PulseEvent(hEvent:HANDLE):WINBOOL;
begin
  PulseEvent:=EventModify(hEvent,EVENT_PULSE);
end;

function ResetEvent(hEvent:HANDLE):WINBOOL;
begin
  ResetEvent:=EventModify(hEvent,EVENT_RESET);
end;

function CheckDlgButton(hDlg:HWND; nIDButton:longint; uCheck:UINT):WINBOOL; 
begin
  CheckDlgButton:=WINBOOL(SendDlgItemMessage(hDlg, nIDButton, BM_SETCHECK, WPARAM(uCheck), LPARAM(0)));
end;


function SetEvent(hEvent:HANDLE):WINBOOL; 
begin
  SetEvent:=EventModify(hEvent,EVENT_SET);
end;

function TlsAlloc:DWORD; 
begin
 TlsAlloc:=TlsCall(TLS_FUNCALLOC, 0);
end;

function TlsFree(dwTlsIndex:DWORD):WINBOOL;
begin
 TlsFree:=WINBOOL(TlsCall(TLS_FUNCFREE, dwTlsIndex));
end;

//end wince only
{$endif WINCE}


{$ifdef WIN32}

//begin win32 only
function Internal_ChildWindowFromPoint(hWndParent:HWND; PointX,PointY : LONG):HWND; external 'user32' name 'ChildWindowFromPoint';
function Internal_ChildWindowFromPointEx(_para1:HWND; _para2X,_Para2Y : LONG; _para3:UINT):HWND; external 'user32' name 'ChildWindowFromPointEx';
function Internal_DragDetect(hwnd:HWND; ptX, ptY : LONG):WINBOOL; external 'user32' name 'DragDetect';
function Internal_GetLargestConsoleWindowSize(hConsoleOutput:HANDLE):DWord; external 'kernel32' name 'GetLargestConsoleWindowSize';
function Internal_LBItemFromPt(hLB:HWND; ptX, ptY : LONG; bAutoScroll:WINBOOL):longint; external 'comctl32' name 'LBItemFromPt';
function Internal_MenuItemFromPoint(hWnd:HWND; hMenu:HMENU; ptScreenX, ptScreenY : LONG):longint; external 'user32' name 'MenuItemFromPoint';
function Internal_PtInRect(var lprc:RECT; ptX,ptY : LONG):WINBOOL; external 'user32' name 'PtInRect';
function Internal_WindowFromPoint(PointX,PointY : LONG):HWND; external 'user32' name 'WindowFromPoint';

function ChildWindowFromPoint(hWndParent:HWND; Point:POINT):HWND;
begin
  ChildWindowFromPoint:=Internal_ChildWindowFromPoint(hWndParent, Point.X, Point.Y);
end;
function ChildWindowFromPointEx(_para1:HWND; _para2:POINT; _para3:UINT):HWND;
begin
  ChildWindowFromPointEx:=Internal_ChildWindowFromPointEx(_para1,_para2.X,_para2.Y,_para3);
end;

function DragDetect(hwnd:HWND; pt:POINT):WINBOOL; {external 'user32' name 'DragDetect'; bug report 1807. PM }
begin
  DragDetect:=Internal_DragDetect(hWnd, pt.X, pt.Y);
end;

function GetLargestConsoleWindowSize(hConsoleOutput:HANDLE):COORD;
var
  res : dword;
begin
  res:=InternalGetLargestConsoleWindowSize(hConsoleOutput);
  GetLargestConsoleWindowSize:=COORD(res);
end;

function LBItemFromPt(hLB:HWND; pt:POINT; bAutoScroll:WINBOOL):longint; { external 'comctl32' bug report 1807. PM }
begin
  LBItemFromPt:=Internal_LBItemFromPt(hLB, pt.X, pt.Y, bAutoScroll);
end;

function MenuItemFromPoint(hWnd:HWND; hMenu:HMENU; ptScreen:POINT):longint; {external 'user32' name 'MenuItemFromPoint'; bug report 1807. PM }
begin
  MenuItemFromPoint:=Internal_MenuItemFromPoint(hWnd, hMenu, ptScreen.X, ptScreen.Y);
end;
function PtInRect(var lprc:RECT; pt:POINT):WINBOOL;
begin
  PtInRect:=Internal_PtInRect(lprc,pt.X,pt.Y);
end;
function PtInRect(lprc:LPRECT; pt:POINT):WINBOOL;
begin
  PtInRect:=Internal_PtInRect(lprc^,pt.X,pt.Y);
end;

function WindowFromPoint(Point:POINT):HWND;
begin
  WindowFromPoint:=Internal_WindowFromPoint(Point.X, Point.Y);
end;
//end win32 only

//begin win32 or wince not checked

//end win32 or wince not checked

{$endif WIN32}


{$endif read_implementation}

