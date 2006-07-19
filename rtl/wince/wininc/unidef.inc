{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Contains the Unicode functions for windows unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  UnicodeFunctions.h

  Declarations for all the Windows32 API Unicode Functions

  Copyright (C) 1996 Free Software Foundation, Inc.

  Author:  Scott Christley <scottc@net-community.com>
  Date: 1996

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


  *What should contain this file*

  Imported functions including xxxW plus default name without the W.
  it's an union of rtl\win\wininc\unifun.inc and rtl\win\wininc\unidef.inc
  example: LoadLibraryW an Loadlibrary declaration.

  Changes :

  08/22/2005 update for wince4.2 port, orinaudo@gmail.com
  05/13/2006 added some xxxW declarations, remaining a lot !,orinaudo@gmail.com
}

{$ifdef read_interface}

//begin common win32 & wince

function AddFontResource(_para1:LPCWSTR):longint; external GdiDLL name 'AddFontResourceW';
function AppendMenu(hMenu:HMENU; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL; external UserDLLCore name 'AppendMenuW';
function CallWindowProc(lpPrevWndFunc:WNDPROC; hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external UserDLLCore name 'CallWindowProcW';
function ChangeDisplaySettingsEx( lpszDeviceName:LPCTSTR; lpDevMode:LPDEVMODE; hwnd:HWND; dwflags:DWORD; lParam:LPVOID):LONG; external UserDLLCore name 'ChangeDisplaySettingsEx'; //+windows
function CharLower(lpsz:LPWSTR):LPWSTR; external UserDLLCore name 'CharLowerW';
function CharLowerBuff(lpsz:LPWSTR; cchLength:DWORD):DWORD; external UserDLLCore name 'CharLowerBuffW';
function CharNext(lpsz:LPCWSTR):LPWSTR; external UserDLLCore name 'CharNextW';
function CharPrev(lpszStart:LPCWSTR; lpszCurrent:LPCWSTR):LPWSTR; external UserDLLCore name 'CharPrevW';
function CharUpper(lpsz:LPWSTR):LPWSTR; external UserDLLCore name 'CharUpperW';
function CharUpperBuff(lpsz:LPWSTR; cchLength:DWORD):DWORD; external UserDLLCore name 'CharUpperBuffW';
function ChooseFont(_para1:LPCHOOSEFONTW):WINBOOL; external ComdlgDLL name 'ChooseFontW';
function CommDlg_OpenSave_GetSpec(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
function CommDlg_OpenSave_GetFilePath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
function CommDlg_OpenSave_GetFolderPath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
function CompareString(Locale:LCID; dwCmpFlags:DWORD; lpString1:LPCWSTR; cchCount1:longint; lpString2:LPCWSTR;cchCount2:longint):longint; external KernelDLL name 'CompareStringW';
function CopyFile(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR; bFailIfExists:WINBOOL):WINBOOL; external KernelDLL name 'CopyFileW';
function CreateAcceleratorTable(_para1:LPACCEL; _para2:longint):HACCEL; external UserDLLCore name 'CreateAcceleratorTableW';
function CreateDC(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR; _para4:pDEVMODE):HDC; external GdiDLL name 'CreateDCW';
function CreateDialogIndirect(hInstance:HINST; lpTemplate:LPCDLGTEMPLATEW; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
function CreateDialogIndirectParam(hInstance:HINST; lpTemplate:LPCDLGTEMPLATEW; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND; external UserDLLCore name 'CreateDialogIndirectParamW';
function CreateDirectory(lpPathName:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL; external KernelDLL name 'CreateDirectoryW';
function CreateEvent(lpEventAttributes:LPSECURITY_ATTRIBUTES; bManualReset:WINBOOL; bInitialState:WINBOOL; lpName:LPCWSTR):HANDLE; external KernelDLL name 'CreateEventW';
function CreateEnhMetaFile(_para1:HDC; _para2:LPCWSTR; _para3:LPRECT; _para4:LPCWSTR):HDC; external GdiDLL name 'CreateEnhMetaFileW';
function CreateFile(lpFileName:LPCWSTR; dwDesiredAccess:DWORD; dwShareMode:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; dwCreationDisposition:DWORD;dwFlagsAndAttributes:DWORD; hTemplateFile:HANDLE):HANDLE; external KernelDLL name 'CreateFileW';
function CreateFileMapping(hFile:HANDLE; lpFileMappingAttributes:LPSECURITY_ATTRIBUTES; flProtect:DWORD; dwMaximumSizeHigh:DWORD; dwMaximumSizeLow:DWORD;lpName:LPCWSTR):HANDLE; external KernelDLL name 'CreateFileMappingW';
function CreateFontIndirect(_para1:PLOGFONT):HFONT; external GdiDLL name 'CreateFontIndirectW';
function CreateMutex(lpMutexAttributes:LPSECURITY_ATTRIBUTES; bInitialOwner:WINBOOL; lpName:LPCWSTR):HANDLE; external KernelDLL name 'CreateMutexW';
function CreateProcess(pszImageName:LPCWSTR; pszCmdLine:LPCWSTR; psaProcess:LPSECURITY_ATTRIBUTES; psaThread:LPSECURITY_ATTRIBUTES; bInheritHandles:WINBOOL;fdwCreate:DWORD; lpEnvironment:LPVOID;
  pszCurDir:LPCWSTR; psiStartInfo:LPSTARTUPINFO; pProcInfo:LPPROCESS_INFORMATION):WINBOOL; external KernelDLL name 'CreateProcessW';
function CreatePropertySheetPage(lppsp:LPCPROPSHEETPAGE):HPROPSHEETPAGE; external ComctlDLL name 'CreatePropertySheetPageW';
function CreateSemaphore(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES; lInitialCount:LONG; lMaximumCount:LONG; lpName:LPCWSTR):HANDLE; external KernelDLL name 'CreateSemaphoreW';
function CreateStatusWindow(style:LONG; lpszText:LPCWSTR; hwndParent:HWND; wID:UINT):HWND; external ComctlDll name 'CreateStatusWindowW';
function CreateWindow(lpClassName:LPCWSTR; lpWindowName:LPCWSTR; dwStyle:DWORD; X:Longint;Y:Longint; nWidth:Longint; nHeight:Longint; hWndParent:HWND; hMenu:HMENU;hInstance:HINST; lpParam:LPVOID):HWND;
function CreateWindowEx(dwExStyle:DWORD; lpClassName:LPCWSTR; lpWindowName:LPCWSTR; dwStyle:DWORD; X:longint;Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU;hInstance:HINST; lpParam:LPVOID):HWND;
  external UserDLLCore name 'CreateWindowExW';
function DeleteFile(lpFileName:LPCWSTR):WINBOOL; external KernelDLL name 'DeleteFileW';
function DefDlgProc(hDlg:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external UserDLLCore name 'DefDlgProcW';
function DefWindowProc(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external UserDLLCore name 'DefWindowProcW';
function DialogBox(hInstance:HINST; lpTemplate:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
function DialogBoxIndirect(hInstance:HINST; lpTemplate:LPCDLGTEMPLATEW; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
function DialogBoxIndirectParam(hInstance:HINST; hDialogTemplate:LPCDLGTEMPLATEW; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint; external UserDLLCore name 'DialogBoxIndirectParamW';
function DispatchMessage(lpMsg:LPMSG):LONG; external UserDLLCore name 'DispatchMessageW';
function DrawText(hDC:HDC; lpString:LPCWSTR; nCount:longint; lpRect:LPRECT; uFormat:UINT):longint; external UserDLLCore name 'DrawTextW';
procedure DrawStatusText(hDC:HDC; lprc:LPRECT; pszText:LPCWSTR; uFlags:UINT); external ComctlDLL name 'DrawStatusTextW';
function EnumCalendarInfo(lpCalInfoEnumProc:CALINFO_ENUMPROC; Locale:LCID; Calendar:CALID; CalType:CALTYPE):WINBOOL; external KernelDLL name 'EnumCalendarInfoW';
function EnumDateFormats(lpDateFmtEnumProc:DATEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL; external KernelDLL name 'EnumDateFormatsW';
function EnumFonts(_para1:HDC; _para2:LPCWSTR; _para3:ENUMFONTSPROC; _para4:LPARAM):longint; external GdiDLL name 'EnumFontsW';
function EnumFontFamilies(_para1:HDC; _para2:LPCWSTR; _para3:FONTENUMPROC; _para4:LPARAM):longint; external GdiDLL name 'EnumFontFamiliesW';
function EnumSystemCodePages(lpCodePageEnumProc:CODEPAGE_ENUMPROCW; dwFlags:DWORD):WINBOOL; external KernelDLL name 'EnumSystemCodePagesW';
function EnumSystemLocales(lpLocaleEnumProc:LOCALE_ENUMPROC; dwFlags:DWORD):WINBOOL; external KernelDLL name 'EnumSystemLocalesW';
function EnumTimeFormats(lpTimeFmtEnumProc:TIMEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL; external KernelDLL name 'EnumTimeFormatsW';
function ExtTextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; _para5:LPRECT;_para6:LPCWSTR; _para7:UINT; _para8:LPINT):WINBOOL; external GdiDLL name 'ExtTextOutW';
function ExtractIconEx(lpszFile:LPCTSTR; nIconIndex:longint; phiconLarge: LPHICON; phiconSmall:LPHICON; nIcons:UINT):UINT; external ShellDLLCore name 'ExtractIconExW';
function FoldString(dwMapFlags:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpDestStr:LPWSTR; cchDest:longint):longint; external KernelDLL name 'FoldStringW';
function FormatMessage(dwFlags:DWORD; lpSource:LPCVOID; dwMessageId:DWORD; dwLanguageId:DWORD; lpBuffer:LPWSTR;nSize:DWORD; Arguments:va_list):DWORD; external KernelDLL name 'FormatMessageW';
function FindFirstChangeNotification(lpPathName:LPCWSTR; bWatchSubtree:WINBOOL; dwNotifyFilter:DWORD):HANDLE; external KernelDLL name 'FindFirstChangeNotificationW';
function FindFirstFile(lpFileName:LPCWSTR; lpFindFileData:LPWIN32_FIND_DATAW):HANDLE; external KernelDLL name 'FindFirstFileW';
function FindFirstFileEx(lpFileName:LPCWSTR; lpInfoLevelId:FINDEX_INFO_LEVELS; lpFindFileData:LPVOID; fSearchOp:FINDEX_SEARCH_OPS; lpSearchFilter:LPVOID; dwAdditionalFlags:DWORD):HANDLE; external KernelDLL name 'FindFirstFileExW'; //+winbase
function FindNextFile(hFindFile:HANDLE; lpFindFileData:LPWIN32_FIND_DATAW):WINBOOL; external KernelDLL name 'FindNextFileW';
function FindResource(hModule:HMODULE; lpName:LPCWSTR; lpType:LPCWSTR):HRSRC; external KernelDLL name 'FindResourceW'; //~winbase hModule is HMODULE
function FindWindow(lpClassName:LPCWSTR; lpWindowName:LPCWSTR):HWND; external UserDLLCore name 'FindWindowW';
function GetClassName(hWnd:HWND; lpClassName:LPWSTR; nMaxCount:longint):longint; external UserDLLCore name 'GetClassNameW';
function GetClassInfo(hInstance:HINST; lpClassName:LPCWSTR; lpWndClass:LPWNDCLASS):WINBOOL; external UserDLLCore name 'GetClassInfoW';
function GetClassLong(hWnd:HWND; nIndex:longint):DWORD; external UserDLLCore name 'GetClassLongW';
function GetClipboardFormatName(format:UINT; lpszFormatName:LPWSTR; cchMaxCount:longint):longint; external UserDLLCore name 'GetClipboardFormatNameW';
function GetCommandLine : LPWSTR; external KernelDLL name 'GetCommandLineW';
function GetCurrencyFormat(Locale:LCID; dwFlags:DWORD; lpValue:LPCWSTR; lpFormat:PCURRENCYFMT; lpCurrencyStr:LPWSTR;cchCurrency:longint):longint; external KernelDLL name 'GetCurrencyFormatW';
function GetDateFormat(Locale:LCID; dwFlags:DWORD; lpDate:LPSYSTEMTIME; lpFormat:LPCWSTR; lpDateStr:LPWSTR;cchDate:longint):longint; external KernelDLL name 'GetDateFormatW';
function GetDiskFreeSpaceEx(lpDirectoryName:LPCWSTR; lpFreeBytesAvailableToCaller:PULARGE_INTEGER; lpTotalNumberOfBytes:PULARGE_INTEGER; lpTotalNumberOfFreeBytes:PULARGE_INTEGER):WINBOOL; external KernelDLL name 'GetDiskFreeSpaceExW'; //+winbase
function GetDlgItemText(hDlg:HWND; nIDDlgItem:longint; lpString:LPWSTR; nMaxCount:longint):UINT; external UserDLLCore name 'GetDlgItemTextW';
function GetFileAttributes(lpFileName:LPCWSTR):DWORD; external KernelDLL name 'GetFileAttributesW';
function GetFileAttributesEx(lpFileName:LPCWSTR; fInfoLevelId:GET_FILEEX_INFO_LEVELS; lpFileInformation:LPVOID):WINBOOL; external KernelDLL name 'GetFileAttributesExW'; //+winbase
function GetFileVersionInfoSize(lptstrFilename:LPWSTR; lpdwHandle:LPDWORD):DWORD; external VersionDLL name 'GetFileVersionInfoSizeW';
function GetFileVersionInfoSizeW(lptstrFilename:LPWSTR; lpdwHandle:LPDWORD):DWORD; external VersionDLL name 'GetFileVersionInfoSizeW';
function GetFileVersionInfo(lptstrFilename:LPWSTR; dwHandle:DWORD; dwLen:DWORD; lpData:LPVOID):WINBOOL; external VersionDLL name 'GetFileVersionInfoW';
function GetFileVersionInfoW(lptstrFilename:LPWSTR; dwHandle:DWORD; dwLen:DWORD; lpData:LPVOID):WINBOOL; external VersionDLL name 'GetFileVersionInfoW';
function GetKeyboardLayoutName(pwszKLID:LPWSTR):WINBOOL; external UserDLLCore name 'GetKeyboardLayoutNameW';
function GetLocaleInfo(Locale:LCID; LCType:LCTYPE; lpLCData:LPWSTR; cchData:longint):longint; external KernelDLL name 'GetLocaleInfoW';
function GetMenuItemInfo(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPMENUITEMINFO):WINBOOL; external UserDLLCore name 'GetMenuItemInfoW';
function GetMessage(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT):WINBOOL; external UserDLLCore name 'GetMessageW';
function GetModuleFileName(hModule:HMODULE; lpFilename:LPWSTR; nSize:DWORD):DWORD; external KernelDLL name 'GetModuleFileNameW';
function GetModuleHandle(lpModuleName:LPCWSTR):HMODULE; external KernelDLL name 'GetModuleHandleW';
function GetNumberFormat(Locale:LCID; dwFlags:DWORD; lpValue:LPCWSTR; lpFormat:PNUMBERFMT; lpNumberStr:LPWSTR;cchNumber:longint):longint; external KernelDLL name 'GetNumberFormatW';
function GetObject(_para1:HGDIOBJ; _para2:longint; _para3:LPVOID):longint; external GdiDLL name 'GetObjectW';
//warning not exported from comdgl32(win32)/commdlg(wince) but coredll
function GetOpenFileName(_para1:LPOPENFILENAMEW):WINBOOL; external ComdlgDLLCore name 'GetOpenFileNameW';
//warning not exported from comdgl32(win32)/commdlg(wince) but coredll
function GetSaveFileName(_para1:LPOPENFILENAMEW):WINBOOL; external ComdlgDLLCore name 'GetSaveFileNameW';
function GetStringTypeEx(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL; external KernelDLL name 'GetStringTypeExW';
function GetTempFileName(lpPathName:LPCWSTR; lpPrefixString:LPCWSTR; uUnique:UINT; lpTempFileName:LPWSTR):UINT; external KernelDLL name 'GetTempFileNameW';
function GetTempPath(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD; external KernelDLL name 'GetTempPathW';
function GetTextExtentExPoint(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:longint; _para5:LPINT;_para6:LPINT; _para7:LPSIZE):WINBOOL; external GdiDLL name 'GetTextExtentExPointW';
function GetTextExtentExPointW(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:longint; _para5:LPINT;_para6:LPINT; _para7:LPSIZE):WINBOOL; external GdiDLL name 'GetTextExtentExPointW';
function GetTextFace(_para1:HDC; _para2:longint; _para3:LPWSTR):longint; external GdiDLL name 'GetTextFaceW';
function GetTextMetrics(_para1:HDC; _para2:LPTEXTMETRICW):WINBOOL; external GdiDLL name 'GetTextMetricsW';
function GetTimeFormat(Locale:LCID; dwFlags:DWORD; lpTime:LPSYSTEMTIME; lpFormat:LPCWSTR; lpTimeStr:LPWSTR;cchTime:longint):longint; external KernelDLL name 'GetTimeFormatW';
function GetUserNameEx(NameFormat:EXTENDED_NAME_FORMAT; lpNameBuffer:LPWSTR; nSize:PULONG):WINBOOL; external SecurDLL name 'GetUserNameExW';
function GetVersionEx(VersionInformation:LPOSVERSIONINFOW):WINBOOL; external KernelDLL name 'GetVersionExW';
function GetWindowText(hWnd:HWND; lpString:LPWSTR; nMaxCount:longint):longint; external UserDLLCore name 'GetWindowTextW';
function GetWindowTextLength(hWnd:HWND):longint; external UserDLLCore name 'GetWindowTextLengthW';
function GetWindowLong(hWnd:HWND; nIndex:longint):LONG; external UserDLLCore name 'GetWindowLongW';
function GlobalAddAtom(lpString:LPCWSTR):ATOM; external KernelDLL name 'GlobalAddAtomW';
function GlobalFindAtom(lpString:LPCWSTR):ATOM; external KernelDLL name 'GlobalFindAtomW';
function IsDialogMessage(hDlg:HWND; lpMsg:LPMSG):WINBOOL; external UserDLLCore name 'IsDialogMessageW';
function InsertMenu(hMenu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL; external UserDLLCore name 'InsertMenuW';
function LCMapString(Locale:LCID; dwMapFlags:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpDestStr:LPWSTR;cchDest:longint):longint; external KernelDLL name 'LCMapStringW';
function LoadAccelerators(hInstance:HINST; lpTableName:LPCWSTR):HACCEL; external UserDLLCore name 'LoadAcceleratorsW';
function LoadBitmap(hInstance:HINST; lpBitmapName:LPCWSTR):HBITMAP; external UserDLLCore name 'LoadBitmapW';
function LoadCursor(hInstance:HINST; lpCursorName:LPCWSTR):HCURSOR; external UserDLLCore name 'LoadCursorW';
function LoadIcon(hInstance:HINST; lpIconName:LPCWSTR):HICON; external UserDLLCore name 'LoadIconW';
function LoadImage(_para1:HINST; _para2:LPCWSTR; _para3:UINT; _para4:longint; _para5:longint;_para6:UINT):HANDLE; external UserDLLCore name 'LoadImageW';
function LoadKeyboardLayout(pwszKLID:LPCWSTR; Flags:UINT):HKL; external UserDLLCore name 'LoadKeyboardLayoutW';
function LoadLibrary(lpLibFileName:LPCWSTR):HINST; external KernelDLL name 'LoadLibraryW';
function LoadLibraryW(lpLibFileName:LPCWSTR):HINST; external KernelDLL name 'LoadLibraryW';
function LoadLibraryEx(lpLibFileName:LPCWSTR; hFile:HANDLE; dwFlags:DWORD):HINST; external KernelDLL name 'LoadLibraryExW';
function LoadLibraryExW(lpLibFileName:LPCWSTR; hFile:HANDLE; dwFlags:DWORD):HINST; external KernelDLL name 'LoadLibraryExW';
function LoadMenu(hInstance:HINST; lpMenuName:LPCWSTR):HMENU; external UserDLLCore name 'LoadMenuW';
function LoadString(hInstance:HINST; uID:UINT; lpBuffer:LPWSTR; nBufferMax:longint):longint; external UserDLLCore name 'LoadStringW';
function lstrcmp(lpString1:LPCWSTR; lpString2:LPCWSTR):longint; external KernelDLL name 'lstrcmpW'; //~winbase result is int
function lstrcmpi(lpString1:LPCWSTR; lpString2:LPCWSTR):longint; external KernelDLL name 'lstrcmpiW'; //~winbase result is int
function MapVirtualKey(uCode:UINT; uMapType:UINT):UINT; external UserDLLCore name 'MapVirtualKeyW';
function MessageBox(hWnd:HWND; lpText:LPCWSTR; lpCaption:LPCWSTR; uType:UINT):longint; external UserDLLCore name 'MessageBoxW'; //~winuser, result declared as int
function MoveFile(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR):WINBOOL; external KernelDLL name 'MoveFileW';
function OpenEvent(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE; external KernelDLL name 'OpenEventW';
procedure OutputDebugString(lpOutputString:LPCWSTR); external KernelDLL name 'OutputDebugStringW';
function PageSetupDlg(_para1:LPPAGESETUPDLGW):WINBOOL; external ComdlgDLL name 'PageSetupDlgW';
function PeekMessage(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT; wRemoveMsg:UINT):WINBOOL; external UserDLLCore name 'PeekMessageW';
function PostThreadMessage(idThread:DWORD; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external UserDLLCore name 'PostThreadMessageW';
function PostMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external UserDLLCore name 'PostMessageW';
function PropertySheet(lppsph:LPCPROPSHEETHEADER):longint; external ComctlDll name 'PropertySheetW';
function RegCreateKeyEx(hKey:HKEY; lpSubKey:LPCWSTR; Reserved:DWORD; lpClass:LPWSTR; dwOptions:DWORD;samDesired:REGSAM; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; phkResult:PHKEY; lpdwDisposition:LPDWORD):LONG;
  external AdvapiDLLCore name 'RegCreateKeyExW';
function RegDeleteKey(hKey:HKEY; lpSubKey:LPCWSTR):LONG; external AdvapiDLLCore name 'RegDeleteKeyW';
function RegDeleteValue(hKey:HKEY; lpValueName:LPCWSTR):LONG; external AdvapiDLLCore name 'RegDeleteValueW';
function RegEnumKeyEx(hKey:HKEY; dwIndex:DWORD; lpName:LPWSTR; lpcbName:LPDWORD; lpReserved:LPDWORD;lpClass:LPWSTR; lpcbClass:LPDWORD; lpftLastWriteTime:PFILETIME):LONG; external AdvapiDLLCore name 'RegEnumKeyExW';
function RegEnumValue(hKey:HKEY; dwIndex:DWORD; lpValueName:LPWSTR; lpcbValueName:LPDWORD; lpReserved:LPDWORD;lpType:LPDWORD; lpData:LPBYTE; lpcbData:LPDWORD):LONG; external AdvapiDLLCore name 'RegEnumValueW';
function RegisterClass(lpWndClass:LPWNDCLASS):ATOM; external UserDLLCore name 'RegisterClassW';
function RegisterClipboardFormat(lpszFormat:LPCWSTR):UINT; external UserDLLCore name 'RegisterClipboardFormatW';
function RegisterWindowMessage(lpString:LPCWSTR):UINT; external KernelDLL name 'RegisterWindowMessageW';
function RegQueryInfoKey(hKey:HKEY; lpClass:LPWSTR; lpcbClass:LPDWORD; lpReserved:LPDWORD; lpcSubKeys:LPDWORD;lpcbMaxSubKeyLen:LPDWORD; lpcbMaxClassLen:LPDWORD; lpcValues:LPDWORD; lpcbMaxValueNameLen:LPDWORD;
  lpcbMaxValueLen:LPDWORD;lpcbSecurityDescriptor:LPDWORD; lpftLastWriteTime:PFILETIME):LONG; external AdvapiDLLCore name 'RegQueryInfoKeyW';
function RegOpenKeyEx(hKey:HKEY; lpSubKey:LPCWSTR; ulOptions:DWORD; samDesired:REGSAM; phkResult:PHKEY):LONG; external AdvapiDLLCore name 'RegOpenKeyExW';
function RegQueryValueEx(hKey:HKEY; lpValueName:LPCWSTR; lpReserved:LPDWORD; lpType:LPDWORD; lpData:LPBYTE;lpcbData:LPDWORD):LONG; external AdvapiDLLCore name 'RegQueryValueExW';function RegSetValueEx(hKey:HKEY; lpValueName:LPCWSTR; Reserved:DWORD; dwType:DWORD; lpData:LPBYTE;cbData:DWORD):LONG; external AdvapiDLLCore name 'RegSetValueExW';
function RemoveDirectory(lpPathName:LPCWSTR):WINBOOL; external KernelDLL name 'RemoveDirectoryW';
function RemoveFontResource(_para1:LPCWSTR):WINBOOL; external GdiDLL name 'RemoveFontResourceW';
function SendDlgItemMessage(hDlg:HWND; nIDDlgItem:longint; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LONG; external UserDLLCore name 'SendDlgItemMessageW';
function SendMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external UserDLLCore name 'SendMessageW';
function SendNotifyMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external UserDLLCore name 'SendNotifyMessageW';
function SetClassLong(hWnd:HWND; nIndex:longint; dwNewLong:LONG):DWORD; external UserDLLCore name 'SetClassLongW';
function SetDlgItemText(hDlg:HWND; nIDDlgItem:longint; lpString:LPCWSTR):WINBOOL; external UserDLLCore name 'SetDlgItemTextW';
function SetFileAttributes(lpFileName:LPCWSTR; dwFileAttributes:DWORD):WINBOOL; external KernelDLL name 'SetFileAttributesW';
function SetLocaleInfo(Locale:LCID; LCType:LCTYPE; lpLCData:LPCWSTR):WINBOOL; external KernelDLL name 'SetLocaleInfoW';
function SetMenuItemInfo(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL; external UserDLLCore name 'SetMenuItemInfoW';
function SetWindowsHookEx(idHook:longint; lpfn:HOOKPROC; hmod:HINST; dwThreadId:DWORD):HHOOK; external UserDLLCore name 'SetWindowsHookExW';
function SetWindowText(hWnd:HWND; lpString:LPCWSTR):WINBOOL; external UserDLLCore name 'SetWindowTextW';
function SetWindowLong(hWnd:HWND; nIndex:longint; dwNewLong:LONG):LONG; external UserDLLCore name 'SetWindowLongW'; //~winuser nIndex is int
function ShellExecuteEx(lpExecInfo:LPSHELLEXECUTEINFO):WINBOOL; external ShellDLLCore name 'ShellExecuteEx'; //+shellapi
function SystemParametersInfo(uiAction:UINT; uiParam:UINT; pvParam:PVOID; fWinIni:UINT):WINBOOL; external UserDLLCore name 'SystemParametersInfoW';
function StartDoc(_para1:HDC; _para2:PDOCINFOW):longint; external GdiDLL name 'StartDocW';
function TranslateAccelerator(hWnd:HWND; hAccTable:HACCEL; lpMsg:LPMSG):longint; external UserDLLCore name 'TranslateAcceleratorW';
function UnregisterClass(lpClassName:LPCWSTR; hInstance:HINST):WINBOOL; external UserDLLCore name 'UnregisterClassW';
function VerQueryValue(pBlock:LPVOID; lpSubBlock:LPWSTR; lplpBuffer:LPVOID; puLen:PUINT):WINBOOL; external versionDLL name 'VerQueryValueW';
function VerQueryValueW(pBlock:LPVOID; lpSubBlock:LPWSTR; lplpBuffer:LPVOID; puLen:PUINT):WINBOOL; external versionDLL name 'VerQueryValueW';
function WNetAddConnection3(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpPassword:LPCWSTR; lpUserName:LPCWSTR; dwFlags:DWORD):DWORD; external MprDLLCore name 'WNetAddConnection3W';
function WNetCancelConnection2(lpName:LPCWSTR; dwFlags:DWORD; fForce:WINBOOL):DWORD; external MprDLLCore name 'WNetCancelConnection2W';
function WNetConnectionDialog1(lpConnDlgStruct:LPCONNECTDLGSTRUCTW):DWORD; external MprDLLCore name 'WNetConnectionDialog1W';
function WNetDisconnectDialog1(lpConnDlgStruct:LPDISCDLGSTRUCTW):DWORD; external MprDLLCore name 'WNetDisconnectDialog1W';
function WNetEnumResource(hEnum:HANDLE; lpcCount:LPDWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD; external MprDLLCore name 'WNetEnumResourceW';
function WNetGetConnection(lpLocalName:LPCWSTR; lpRemoteName:LPWSTR; lpnLength:LPDWORD):DWORD; external MprDLLCore name 'WNetGetConnectionW';
function WNetGetUniversalName(lpLocalPath:LPCWSTR; dwInfoLevel:DWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD; external MprDLLCore name 'WNetGetUniversalNameW';
function WNetGetUser(lpName:LPCWSTR; lpUserName:LPWSTR; lpnLength:LPDWORD):DWORD; external MprDLLCore name 'WNetGetUserW';
function WNetOpenEnum(dwScope:DWORD; dwType:DWORD; dwUsage:DWORD; lpNetResource:LPNETRESOURCEW; lphEnum:LPHANDLE):DWORD; external MprDLLCore name 'WNetOpenEnumW';
function wsprintf(lpBuffer:LPWSTR; lpFormat:LPCWSTR; const args:array of const):longint; external UserDLLCore name 'wsprintfW'; //~winuser Result is int, cedcl directive removed, API doc say nothing about calling convention
function wsprintf(lpBuffer:LPWSTR; lpFormat:LPCWSTR):longint; external UserDLLCore name 'wsprintfW'; //~winuser Result is int, API doc say nothing about calling convention
function wvsprintf(_para1:LPWSTR; _para2:LPCWSTR; arglist:va_list):longint; external UserDLLCore name 'wvsprintfW'; //~winuser nIndex is int

//end common win32 & wince

{$ifdef WINCE}
//begin wince only

//'ChooseColorW' not exported but 'ChooseColor' yes
function ChooseColor(_para1:LPCHOOSECOLOR):WINBOOL; external ComdlgDLL name 'ChooseColor';
function CreateDialog(hInstance:HINST; lpName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
function CreateDialogParam(hInstance:HINST; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND;
function DialogBoxParam(hInstance:HINST; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint;
//'EnumDisplaySettingsW' not exported but 'EnumDisplaySettings' with widechar header is
function EnumDisplaySettings(lpszDeviceName:LPCWSTR; iModeNum:DWORD; lpDevMode:LPDEVMODEW):WINBOOL; external UserDLLCore name 'EnumDisplaySettings';
//'EnumPropsExW' not exported but 'EnumPropsEx' with widechar header is
function EnumPropsEx(hWnd:HWND; lpEnumFunc:PROPENUMPROCEX; lParam:LPARAM):longint; external UserDLLCore name 'EnumPropsEx';
//'...W' not exported but '...' is
function GetCharABCWidths(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABC):WINBOOL; external GdiDLL name 'GetCharABCWidths';
function GetCharWidth32(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL; external GdiDLL name 'GetCharWidth32';
function GetTextExtentPoint(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL;
function GetTextExtentPoint32(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL;
//wince has W
function GetProcAddress(hModule:HINST; lpProcName:LPCWSTR):FARPROC; external KernelDLL name 'GetProcAddressW';
//'GetPropW', not exported but 'GetProp' with widechar header are
function GetProp(hWnd:HWND; lpString:LPCWSTR):HANDLE; external UserDLLCore name 'GetProp';
//!header specific to CE, not in comctl32(win32)/commctl(wince) but in coredll
function ImageList_LoadImage(hi:HINST; lpbmp:LPCSTR; cx:longint; cGrow:longint; crMask:COLORREF;uType:UINT; uFlags:UINT):HIMAGELIST; external ComctlDLLCore name 'ImageList_LoadImage';
//'RemovePropW' not exported but 'RemoveProp' with widechar header is
function RemoveProp(hWnd:HWND; lpString:LPCWSTR):HANDLE; external UserDLLCore name 'RemoveProp';
//'SendMessageTimeoutW' not exported but 'SendMessageTimeout' yes
function SendMessageTimeout(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; fuFlags:UINT;uTimeout:UINT; lpdwResult:LPDWORD):LRESULT; external UserDLLCore name 'SendMessageTimeout';
//'SetPropW' not exported but 'SetProp' with widechar header is
function SetProp(hWnd:HWND; lpString:LPCWSTR; hData:HANDLE):WINBOOL; external UserDLLCore name 'SetProp';
//not exported as PrintDlgW but PrintDlg
function PrintDlg(_para1:LPPRINTDLG):WINBOOL; external ComdlgDLL name 'PrintDlg';
//not exported as RegSaveKeyW but RegSaveKey
function RegSaveKey(hKey:HKEY; lpFile:LPCTSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):LONG; external AdvapiDLLCore name 'RegSaveKey';
//not exported as RegReplaceKeyW but RegReplaceKey
function RegReplaceKey(hKey:HKEY; lpSubKey:LPCTSTR; lpNewFile:LPCTSTR; lpOldFile:LPCTSTR):LONG; external AdvapiDLLCore name 'RegReplaceKey';
//not exported as Shell_NotifyIconW but Shell_NotifyIcon
function Shell_NotifyIcon(dwMessage: DWORD; lpData: PNotifyIconDataA): WINBOOL; external ShellDLLCore name 'Shell_NotifyIcon';
//win32 already have without W or A
function SHFileOperation(lpFileOp:LPSHFILEOPSTRUCTW): longint; external ShellDLL name 'SHFileOperationW';
//end wince only

{$endif WINCE}

{$ifdef WIN32}
//begin win32 only ie not exist in wince4.2 second release lib imported functions from dlls
function AbortSystemShutdown(lpMachineName:LPWSTR):WINBOOL; external 'advapi32' name 'AbortSystemShutdownW';
function AccessCheckAndAuditAlarm(SubsystemName:LPCWSTR; HandleId:LPVOID; ObjectTypeName:LPWSTR; ObjectName:LPWSTR; SecurityDescriptor:PSECURITY_DESCRIPTOR;DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; ObjectCreation:WINBOOL;
function AddAtom(lpString:LPCWSTR):ATOM; external 'kernel32' name 'AddAtomW';
function CopyMetaFile(_para1:HMETAFILE; _para2:LPCWSTR):HMETAFILE; external 'gdi32' name 'CopyMetaFileW';
function CreateFont(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:longint;_para6:DWORD; _para7:DWORD; _para8:DWORD; _para9:DWORD; _para10:DWORD;_para11:DWORD; _para12:DWORD; _para13:DWORD; _para14:LPCWSTR):HFONT;
  external 'gdi32' name 'CreateFontW';
function BackupEventLog(hEventLog:HANDLE; lpBackupFileName:LPCWSTR):WINBOOL; external 'advapi32' name 'BackupEventLogW';
function BeginUpdateResource(pFileName:LPCWSTR; bDeleteExistingResources:WINBOOL):HANDLE; external 'kernel32' name 'BeginUpdateResourceW';
function BuildCommDCB(lpDef:LPCWSTR; lpDCB:LPDCB):WINBOOL; external 'kernel32' name 'BuildCommDCBW';
function BuildCommDCBAndTimeouts(lpDef:LPCWSTR; lpDCB:LPDCB; lpCommTimeouts:LPCOMMTIMEOUTS):WINBOOL; external 'kernel32' name 'BuildCommDCBAndTimeoutsW';
function CallMsgFilter(lpMsg:LPMSG; nCode:longint):WINBOOL; external 'user32' name 'CallMsgFilterW';
function CallNamedPipe(lpNamedPipeName:LPCWSTR; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID; nOutBufferSize:DWORD;lpBytesRead:LPDWORD; nTimeOut:DWORD):WINBOOL; external 'kernel32' name 'CallNamedPipeW';
function ChangeDisplaySettings(lpDevMode:LPDEVMODE; dwFlags:DWORD):LONG; external 'user32' name 'ChangeDisplaySettingsW';
function ChangeMenu(hMenu:HMENU; cmd:UINT; lpszNewItem:LPCWSTR; cmdInsert:UINT; flags:UINT):WINBOOL; external 'user32' name 'ChangeMenuW';
function CharToOem(lpszSrc:LPCWSTR; lpszDst:LPSTR):WINBOOL; external 'user32' name 'CharToOemW';
function CharToOemBuff(lpszSrc:LPCWSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL; external 'user32' name 'CharToOemBuffW';
function ChangeServiceConfig(hService:SC_HANDLE; dwServiceType:DWORD; dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCWSTR;lpLoadOrderGroup:LPCWSTR; lpdwTagId:LPDWORD; lpDependencies:LPCWSTR; lpServiceStartName:LPCWSTR;
  lpPassword:LPCWSTR;lpDisplayName:LPCWSTR):WINBOOL; external 'advapi32' name 'ChangeServiceConfigW';
function ChooseColor(_para1:LPCHOOSECOLOR):WINBOOL; external 'comdlg32' name 'ChooseColorW';
function ClearEventLog(hEventLog:HANDLE; lpBackupFileName:LPCWSTR):WINBOOL; external 'advapi32' name 'ClearEventLogW';
function CommConfigDialog(lpszName:LPCWSTR; hWnd:HWND; lpCC:LPCOMMCONFIG):WINBOOL; external 'kernel32' name 'CommConfigDialogW';
function CopyAcceleratorTable(hAccelSrc:HACCEL; lpAccelDst:LPACCEL; cAccelEntries:longint):longint; external 'user32' name 'CopyAcceleratorTableW';
function CopyEnhMetaFile(_para1:HENHMETAFILE; _para2:LPCWSTR):HENHMETAFILE; external 'gdi32' name 'CopyEnhMetaFileW';
function CreateColorSpace(_para1:LPLOGCOLORSPACE):HCOLORSPACE; external 'gdi32' name 'CreateColorSpaceW';
function CreateDesktop(lpszDesktop:LPWSTR; lpszDevice:LPWSTR; pDevmode:LPDEVMODE; dwFlags:DWORD; dwDesiredAccess:DWORD;lpsa:LPSECURITY_ATTRIBUTES):HDESK; external 'user32' name 'CreateDesktopW';
function CreateDialog(hInstance:HINST; lpName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
function CreateDialogParam(hInstance:HINST; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND; external 'user32' name 'CreateDialogParamW';
function CreateDirectoryEx(lpTemplateDirectory:LPCWSTR; lpNewDirectory:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL; external 'kernel32' name 'CreateDirectoryExW';
function CreateIC(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR; _para4:LPDEVMODE):HDC; external 'gdi32' name 'CreateICW';
function CreateMailslot(lpName:LPCWSTR; nMaxMessageSize:DWORD; lReadTimeout:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE; external 'kernel32' name 'CreateMailslotW';
function CreateMDIWindow(lpClassName:LPWSTR; lpWindowName:LPWSTR; dwStyle:DWORD; X:longint; Y:longint;nWidth:longint; nHeight:longint; hWndParent:HWND; hInstance:HINST; lParam:LPARAM):HWND; external 'user32' name 'CreateMDIWindowW';
function CreateMetaFile(_para1:LPCWSTR):HDC; external 'gdi32' name 'CreateMetaFileW';
function CreateNamedPipe(lpName:LPCWSTR; dwOpenMode:DWORD; dwPipeMode:DWORD; nMaxInstances:DWORD; nOutBufferSize:DWORD;nInBufferSize:DWORD; nDefaultTimeOut:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE;
  external 'kernel32' name 'CreateNamedPipeW';
function CreateProcessAsUser(_para1:HANDLE; _para2:LPCWSTR; _para3:LPWSTR; _para4:LPSECURITY_ATTRIBUTES; _para5:LPSECURITY_ATTRIBUTES;_para6:WINBOOL; _para7:DWORD; _para8:LPVOID; _para9:LPCWSTR;
  _para10:LPSTARTUPINFO;_para11:LPPROCESS_INFORMATION):WINBOOL; external 'advapi32' name 'CreateProcessAsUserW';
function CreateScalableFontResource(_para1:DWORD; _para2:LPCWSTR; _para3:LPCWSTR; _para4:LPCWSTR):WINBOOL; external 'gdi32' name 'CreateScalableFontResourceW';
function CreateService(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; lpDisplayName:LPCWSTR; dwDesiredAccess:DWORD; dwServiceType:DWORD;dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCWSTR; lpLoadOrderGroup:LPCWSTR;
  lpdwTagId:LPDWORD;lpDependencies:LPCWSTR; lpServiceStartName:LPCWSTR; lpPassword:LPCWSTR):SC_HANDLE; external AdvapiDLL name 'CreateServiceW';
function CreateWindowStation(lpwinsta:LPWSTR; dwReserved:DWORD; dwDesiredAccess:DWORD; lpsa:LPSECURITY_ATTRIBUTES):HWINSTA; external 'user32' name 'CreateWindowStationW';
function DdeCreateStringHandle(_para1:DWORD; _para2:LPCWSTR; _para3:longint):HSZ; external 'user32' name 'DdeCreateStringHandleW';
function DdeInitialize(_para1:LPDWORD; _para2:PFNCALLBACK; _para3:DWORD; _para4:DWORD):UINT; external 'user32' name 'DdeInitializeW';
function DdeQueryString(_para1:DWORD; _para2:HSZ; _para3:LPCWSTR; _para4:DWORD; _para5:longint):DWORD; external 'user32' name 'DdeQueryStringW';
function DefineDosDevice(dwFlags:DWORD; lpDeviceName:LPCWSTR; lpTargetPath:LPCWSTR):WINBOOL; external 'kernel32' name 'DefineDosDeviceW';
function DefFrameProc(hWnd:HWND; hWndMDIClient:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'DefFrameProcW';
function DefMDIChildProc(hWnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'DefMDIChildProcW';
function DialogBoxParam(hInstance:HINST; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint; external 'user32' name 'DialogBoxParamW';
function DlgDirList(hDlg:HWND; lpPathSpec:LPWSTR; nIDListBox:longint; nIDStaticPath:longint; uFileType:UINT):longint; external 'user32' name 'DlgDirListW';
function DlgDirSelectEx(hDlg:HWND; lpString:LPWSTR; nCount:longint; nIDListBox:longint):WINBOOL; external 'user32' name 'DlgDirSelectExW';
function DlgDirListComboBox(hDlg:HWND; lpPathSpec:LPWSTR; nIDComboBox:longint; nIDStaticPath:longint; uFiletype:UINT):longint; external 'user32' name 'DlgDirListComboBoxW';
function DlgDirSelectComboBoxEx(hDlg:HWND; lpString:LPWSTR; nCount:longint; nIDComboBox:longint):WINBOOL; external 'user32' name 'DlgDirSelectComboBoxExW';
function DragQueryFile(_para1:HDROP; _para2:cardinal; _para3:LPCWSTR; _para4:cardinal):cardinal; external 'shell32' name 'DragQueryFileW';
function DrawState(_para1:HDC; _para2:HBRUSH; _para3:DRAWSTATEPROC; _para4:LPARAM; _para5:WPARAM;_para6:longint; _para7:longint; _para8:longint; _para9:longint; _para10:UINT):WINBOOL; external 'user32' name 'DrawStateW';
function DrawTextEx(_para1:HDC; _para2:LPWSTR; _para3:longint; _para4:LPRECT; _para5:UINT;_para6:LPDRAWTEXTPARAMS):longint; external 'user32' name 'DrawTextExW';
function EndUpdateResource(hUpdate:HANDLE; fDiscard:WINBOOL):WINBOOL; external 'kernel32' name 'EndUpdateResourceW';
function EnumDependentServices(hService:SC_HANDLE; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD;lpServicesReturned:LPDWORD):WINBOOL; external 'advapi32' name 'EnumDependentServicesW';
function EnumDesktops(hwinsta:HWINSTA; lpEnumFunc:DESKTOPENUMPROC; lParam:LPARAM):WINBOOL; external 'user32' name 'EnumDesktopsW';
function EnumDisplaySettings(lpszDeviceName:LPCWSTR; iModeNum:DWORD; lpDevMode:LPDEVMODEW):WINBOOL; external UserDLLCore name 'EnumDisplaySettingsW';
function EnumFontFamiliesEx(_para1:HDC; _para2:LPLOGFONT; _para3:FONTENUMEXPROC; _para4:LPARAM; _para5:DWORD):longint; external 'gdi32' name 'EnumFontFamiliesExW';
function EnumICMProfiles(_para1:HDC; _para2:ICMENUMPROC; _para3:LPARAM):longint; external 'gdi32' name 'EnumICMProfilesW';
function EnumProps(hWnd:HWND; lpEnumFunc:PROPENUMPROC):longint; external 'user32' name 'EnumPropsW';
function EnumResourceTypes(hModule:HINST; lpEnumFunc:ENUMRESTYPEPROC; lParam:LONG):WINBOOL; external 'kernel32' name 'EnumResourceTypesW';
function EnumResourceNames(hModule:HINST; lpType:LPCWSTR; lpEnumFunc:ENUMRESNAMEPROC; lParam:LONG):WINBOOL; external 'kernel32' name 'EnumResourceNamesW';
function EnumResourceLanguages(hModule:HINST; lpType:LPCWSTR; lpName:LPCWSTR; lpEnumFunc:ENUMRESLANGPROC; lParam:LONG):WINBOOL; external 'kernel32' name 'EnumResourceLanguagesW';
function EnumServicesStatus(hSCManager:SC_HANDLE; dwServiceType:DWORD; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD;pcbBytesNeeded:LPDWORD; lpServicesReturned:LPDWORD; lpResumeHandle:LPDWORD):WINBOOL;
  external 'advapi32' name 'EnumServicesStatusW';
function EnumWindowStations(lpEnumFunc:ENUMWINDOWSTATIONPROC; lParam:LPARAM):WINBOOL; external 'user32' name 'EnumWindowStationsW';
function ExpandEnvironmentStrings(lpSrc:LPCWSTR; lpDst:LPWSTR; nSize:DWORD):DWORD; external 'kernel32' name 'ExpandEnvironmentStringsW';
function ExtractIcon(_para1:HINST; _para2:LPCWSTR; _para3:cardinal):HICON; external 'shell32' name 'ExtractIconW';
function ExtractAssociatedIcon(_para1:HINST; _para2:LPCWSTR; _para3:LPWORD):HICON; external 'shell32' name 'ExtractAssociatedIconW';
procedure FatalAppExit(uAction:UINT; lpMessageText:LPCWSTR); external 'kernel32' name 'FatalAppExitW';
function FillConsoleOutputCharacter(hConsoleOutput:HANDLE; cCharacter:WCHAR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL; external 'kernel32' name 'FillConsoleOutputCharacterW';
function FindAtom(lpString:LPCWSTR):ATOM; external 'kernel32' name 'FindAtomW';
function FindExecutable(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR):HINST; external 'shell32' name 'FindExecutableW';
function FindResourceEx(hModule:HINST; lpType:LPCWSTR; lpName:LPCWSTR; wLanguage:WORD):HRSRC; external 'kernel32' name 'FindResourceExW';
function FindText(_para1:LPFINDREPLACE):HWND; external 'comdlg32' name 'FindTextW';
function FindWindowEx(_para1:HWND; _para2:HWND; _para3:LPCWSTR; _para4:LPCWSTR):HWND; external 'user32' name 'FindWindowExW';
function FreeEnvironmentStrings(_para1:LPWSTR):WINBOOL; external 'kernel32' name 'FreeEnvironmentStringsW';
function GetAtomName(nAtom:ATOM; lpBuffer:LPWSTR; nSize:longint):UINT; external 'kernel32' name 'GetAtomNameW';
function GetBinaryType(lpApplicationName:LPCWSTR; lpBinaryType:LPDWORD):WINBOOL; external 'kernel32' name 'GetBinaryTypeW';
function GetCharABCWidthsFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABCFLOAT):WINBOOL; external 'gdi32' name 'GetCharABCWidthsFloatW';
function GetCharacterPlacement(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:longint; _para5:LPGCP_RESULTS;_para6:DWORD):DWORD; external 'gdi32' name 'GetCharacterPlacementW';
function GetCharWidth(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL; external 'gdi32' name 'GetCharWidthW';
function GetCharWidthFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:PSingle):WINBOOL; external 'gdi32' name 'GetCharWidthFloatW';
function GetCompressedFileSize(lpFileName:LPCWSTR; lpFileSizeHigh:LPDWORD):DWORD; external 'kernel32' name 'GetCompressedFileSizeW';
function GetComputerName(lpBuffer:LPWSTR; nSize:LPDWORD):WINBOOL; external 'kernel32' name 'GetComputerNameW';
function GetCharABCWidths(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABC):WINBOOL; external GdiDLL name 'GetCharABCWidthsW';
function GetCharWidth32(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL; external GdiDLL name 'GetCharWidth32W';
function GetClassInfoEx(_para1:HINST; _para2:LPCWSTR; _para3:LPWNDCLASSEX):WINBOOL; external 'user32' name 'GetClassInfoExW';
function GetConsoleTitle(lpConsoleTitle:LPWSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetConsoleTitleW';
function GetCurrentDirectory(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD; external 'kernel32' name 'GetCurrentDirectoryW';
function GetDefaultCommConfig(lpszName:LPCWSTR; lpCC:LPCOMMCONFIG; lpdwSize:LPDWORD):WINBOOL; external 'kernel32' name 'GetDefaultCommConfigW';
function GetDiskFreeSpace(lpRootPathName:LPCWSTR; lpSectorsPerCluster:LPDWORD; lpBytesPerSector:LPDWORD; lpNumberOfFreeClusters:LPDWORD; lpTotalNumberOfClusters:LPDWORD):WINBOOL; external 'kernel32' name 'GetDiskFreeSpaceW';
function GetDriveType(lpRootPathName:LPCWSTR):UINT; external 'kernel32' name 'GetDriveTypeW';
function GetEnhMetaFile(_para1:LPCWSTR):HENHMETAFILE; external 'gdi32' name 'GetEnhMetaFileW';
function GetEnhMetaFileDescription(_para1:HENHMETAFILE; _para2:UINT; _para3:LPWSTR):UINT; external 'gdi32' name 'GetEnhMetaFileDescriptionW';
function GetEnvironmentStrings : LPWSTR; external 'kernel32' name 'GetEnvironmentStringsW';
function GetEnvironmentVariable(lpName:LPCWSTR; lpBuffer:LPWSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetEnvironmentVariableW';
function GetFileSecurity(lpFileName:LPCWSTR; RequestedInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external 'advapi32' name 'GetFileSecurityW';
function GetFileTitle(_para1:LPCWSTR; _para2:LPWSTR; _para3:WORD):longint; external 'comdlg32' name 'GetFileTitleW';
function GetFullPathName(lpFileName:LPCWSTR; nBufferLength:DWORD; lpBuffer:LPWSTR; var lpFilePart:LPWSTR):DWORD; external 'kernel32' name 'GetFullPathNameW';
function GetGlyphOutline(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPGLYPHMETRICS; _para5:DWORD;_para6:LPVOID; _para7:PMAT2):DWORD; external 'gdi32' name 'GetGlyphOutlineW';
function GetICMProfile(_para1:HDC; _para2:DWORD; _para3:LPWSTR):WINBOOL; external 'gdi32' name 'GetICMProfileW';
function GetKeyNameText(lParam:LONG; lpString:LPWSTR; nSize:longint):longint; external 'user32' name 'GetKeyNameTextW';
function GetKerningPairs(_para1:HDC; _para2:DWORD; _para3:LPKERNINGPAIR):DWORD; external 'gdi32' name 'GetKerningPairsW';
function GetLogColorSpace(_para1:HCOLORSPACE; _para2:LPLOGCOLORSPACE; _para3:DWORD):WINBOOL; external 'gdi32' name 'GetLogColorSpaceW';
function GetLogicalDriveStrings(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD; external 'kernel32' name 'GetLogicalDriveStringsW';
function GetMenuString(hMenu:HMENU; uIDItem:UINT; lpString:LPWSTR; nMaxCount:longint; uFlag:UINT):longint; external 'user32' name 'GetMenuStringW';
function GetMetaFile(_para1:LPCWSTR):HMETAFILE; external 'gdi32' name 'GetMetaFileW';
function GetNamedPipeHandleState(hNamedPipe:HANDLE; lpState:LPDWORD; lpCurInstances:LPDWORD; lpMaxCollectionCount:LPDWORD; lpCollectDataTimeout:LPDWORD;lpUserName:LPWSTR; nMaxUserNameSize:DWORD):WINBOOL;
  external 'kernel32' name 'GetNamedPipeHandleStateW';
function GetTextExtentPoint(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32' name 'GetTextExtentPointW';
function GetTextExtentPoint32(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32' name 'GetTextExtentPoint32W';
function GetOutlineTextMetrics(_para1:HDC; _para2:UINT; _para3:LPOUTLINETEXTMETRIC):UINT; external 'gdi32' name 'GetOutlineTextMetricsW';
function GetProfileInt(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; nDefault:WINT):UINT; external 'kernel32' name 'GetProfileIntW';
function GetProfileString(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpDefault:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetProfileStringW';
function GetProfileSection(lpAppName:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetProfileSectionW';
//was in func.inc, wince has W
function GetProcAddress(hModule:HINST; lpProcName:LPCSTR):FARPROC; external 'kernel32' name 'GetProcAddress';
function GetProp(hWnd:HWND; lpString:LPCWSTR):HANDLE; external 'user32' name 'GetPropW';
function GetPrivateProfileInt(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; nDefault:WINT; lpFileName:LPCWSTR):UINT; external 'kernel32' name 'GetPrivateProfileIntW';
function GetPrivateProfileString(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpDefault:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD;lpFileName:LPCWSTR):DWORD; external 'kernel32' name 'GetPrivateProfileStringW';
function GetPrivateProfileSection(lpAppName:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD; lpFileName:LPCWSTR):DWORD; external 'kernel32' name 'GetPrivateProfileSectionW';
function GetServiceKeyName(hSCManager:SC_HANDLE; lpDisplayName:LPCWSTR; lpServiceName:LPWSTR; lpcchBuffer:LPDWORD):WINBOOL; external 'advapi32' name 'GetServiceKeyNameW';
function GetServiceDisplayName(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; lpDisplayName:LPWSTR; lpcchBuffer:LPDWORD):WINBOOL; external 'advapi32' name 'GetServiceDisplayNameW';
function GetShortPathName(lpszLongPath:LPCWSTR; lpszShortPath:LPWSTR; cchBuffer:DWORD):DWORD; external 'kernel32' name 'GetShortPathNameW';
procedure GetStartupInfo(lpStartupInfo:LPSTARTUPINFO); external 'kernel32' name 'GetStartupInfoW';
function GetStringType(dwInfoType:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL; external 'kernel32' name 'GetStringTypeW';
function GetSystemDirectory(lpBuffer:LPWSTR; uSize:UINT):UINT; external 'kernel32' name 'GetSystemDirectoryW';
function GetTabbedTextExtent(hDC:HDC; lpString:LPCWSTR; nCount:longint; nTabPositions:longint; lpnTabStopPositions:LPINT):DWORD; external 'user32' name 'GetTabbedTextExtentW';
function GetUserName(lpBuffer:LPWSTR; nSize:LPDWORD):WINBOOL; external 'advapi32' name 'GetUserNameW';
function GetUserObjectInformation(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external 'user32' name 'GetUserObjectInformationW';
function GetVolumeInformation(lpRootPathName:LPCWSTR; lpVolumeNameBuffer:LPWSTR; nVolumeNameSize:DWORD; lpVolumeSerialNumber:LPDWORD; lpMaximumComponentLength:LPDWORD;lpFileSystemFlags:LPDWORD; lpFileSystemNameBuffer:LPWSTR;
  nFileSystemNameSize:DWORD):WINBOOL; external 'kernel32' name 'GetVolumeInformationW';
function GetWindowsDirectory(lpBuffer:LPWSTR; uSize:UINT):UINT; external 'kernel32' name 'GetWindowsDirectoryW';
function GlobalGetAtomName(nAtom:ATOM; lpBuffer:LPWSTR; nSize:longint):UINT; external 'kernel32' name 'GlobalGetAtomNameW';
function GrayString(hDC:HDC; hBrush:HBRUSH; lpOutputFunc:GRAYSTRINGPROC; lpData:LPARAM; nCount:longint;X:longint; Y:longint; nWidth:longint; nHeight:longint):WINBOOL; external 'user32' name 'GrayStringW';
function ImageList_LoadImage(hi:HINST; lpbmp:LPCWSTR; cx:longint; cGrow:longint; crMask:COLORREF;uType:UINT; uFlags:UINT):HIMAGELIST; external 'comctl32' name 'ImageList_LoadImageW';
function IsBadStringPtr(lpsz:LPCWSTR; ucchMax:UINT):WINBOOL; external 'kernel32' name 'IsBadStringPtrW';
function IsCharAlpha(ch:WCHAR):WINBOOL; external 'user32' name 'IsCharAlphaW';
function IsCharAlphaNumeric(ch:WCHAR):WINBOOL; external 'user32' name 'IsCharAlphaNumericW';
function IsCharUpper(ch:WCHAR):WINBOOL; external 'user32' name 'IsCharUpperW';
function IsCharLower(ch:WCHAR):WINBOOL; external 'user32' name 'IsCharLowerW';
function InitiateSystemShutdown(lpMachineName:LPWSTR; lpMessage:LPWSTR; dwTimeout:DWORD; bForceAppsClosed:WINBOOL; bRebootAfterShutdown:WINBOOL):WINBOOL; external 'advapi32' name 'InitiateSystemShutdownW';
function InsertMenuItem(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL; external 'user32' name 'InsertMenuItemW';
function LoadCursorFromFile(lpFileName:LPCWSTR):HCURSOR; external 'user32' name 'LoadCursorFromFileW';
function LoadMenuIndirect(lpMenuTemplate:LPMENUTEMPLATE):HMENU; external 'user32' name 'LoadMenuIndirectW';
function LogonUser(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR; _para4:DWORD; _para5:DWORD;_para6:PHANDLE):WINBOOL; external 'advapi32' name 'LogonUserW';
function LookupAccountName(lpSystemName:LPCWSTR; lpAccountName:LPCWSTR; Sid:PSID; cbSid:LPDWORD; ReferencedDomainName:LPWSTR;cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL; external 'advapi32' name 'LookupAccountNameW';
function LookupAccountSid(lpSystemName:LPCWSTR; Sid:PSID; Name:LPWSTR; cbName:LPDWORD; ReferencedDomainName:LPWSTR;cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL; external 'advapi32' name 'LookupAccountSidW';
function LookupPrivilegeDisplayName(lpSystemName:LPCWSTR; lpName:LPCWSTR; lpDisplayName:LPWSTR; cbDisplayName:LPDWORD; lpLanguageId:LPDWORD):WINBOOL; external 'advapi32' name 'LookupPrivilegeDisplayNameW';
function LookupPrivilegeName(lpSystemName:LPCWSTR; lpLuid:PLUID; lpName:LPWSTR; cbName:LPDWORD):WINBOOL; external 'advapi32' name 'LookupPrivilegeNameW';
function LookupPrivilegeValue(lpSystemName:LPCWSTR; lpName:LPCWSTR; lpLuid:PLUID):WINBOOL; external 'advapi32' name 'LookupPrivilegeValueW';
function lstrcat(lpString1:LPWSTR; lpString2:LPCWSTR):LPWSTR; external 'kernel32' name 'lstrcatW';
function lstrcpyn(lpString1:LPWSTR; lpString2:LPCWSTR; iMaxLength:longint):LPWSTR; external 'kernel32' name 'lstrcpynW';
function lstrcpy(lpString1:LPWSTR; lpString2:LPCWSTR):LPWSTR; external 'kernel32' name 'lstrcpyW';
function lstrlen(lpString:LPCWSTR):longint; external 'kernel32' name 'lstrlenW';
function MapVirtualKeyEx(uCode:UINT; uMapType:UINT; dwhkl:HKL):UINT; external 'user32' name 'MapVirtualKeyExW';
function ModifyMenu(hMnu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL; external 'user32' name 'ModifyMenuW';
function MoveFileEx(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'MoveFileExW';
function MessageBoxEx(hWnd:HWND; lpText:LPCWSTR; lpCaption:LPCWSTR; uType:UINT; wLanguageId:WORD):longint; external UserDLLCore name 'MessageBoxExW';
function MessageBoxIndirect(_para1:LPMSGBOXPARAMS):longint; external UserDLLCore name 'MessageBoxIndirectW';
function MultinetGetConnectionPerformance(lpNetResource:LPNETRESOURCE; lpNetConnectInfoStruct:LPNETCONNECTINFOSTRUCT):DWORD; external 'mpr' name 'MultinetGetConnectionPerformanceW';
function ObjectCloseAuditAlarm(SubsystemName:LPCWSTR; HandleId:LPVOID; GenerateOnClose:WINBOOL):WINBOOL; external 'advapi32' name 'ObjectCloseAuditAlarmW';
function ObjectOpenAuditAlarm(SubsystemName:LPCWSTR; HandleId:LPVOID; ObjectTypeName:LPWSTR; ObjectName:LPWSTR; pSecurityDescriptor:PSECURITY_DESCRIPTOR;ClientToken:HANDLE; DesiredAccess:DWORD; GrantedAccess:DWORD;
  Privileges:PPRIVILEGE_SET; ObjectCreation:WINBOOL;AccessGranted:WINBOOL; GenerateOnClose:LPBOOL):WINBOOL; external 'advapi32' name 'ObjectOpenAuditAlarmW';
function ObjectPrivilegeAuditAlarm(SubsystemName:LPCWSTR; HandleId:LPVOID; ClientToken:HANDLE; DesiredAccess:DWORD; Privileges:PPRIVILEGE_SET;AccessGranted:WINBOOL):WINBOOL; external 'advapi32' name 'ObjectPrivilegeAuditAlarmW';
function OemToChar(lpszSrc:LPCSTR; lpszDst:LPWSTR):WINBOOL; external 'user32' name 'OemToCharW';
function OemToCharBuff(lpszSrc:LPCSTR; lpszDst:LPWSTR; cchDstLength:DWORD):WINBOOL; external 'user32' name 'OemToCharBuffW';
function OpenBackupEventLog(lpUNCServerName:LPCWSTR; lpFileName:LPCWSTR):HANDLE; external 'advapi32' name 'OpenBackupEventLogW';
function OpenDesktop(lpszDesktop:LPWSTR; dwFlags:DWORD; fInherit:WINBOOL; dwDesiredAccess:DWORD):HDESK; external 'user32' name 'OpenDesktopW';
function OpenEventLog(lpUNCServerName:LPCWSTR; lpSourceName:LPCWSTR):HANDLE; external 'advapi32' name 'OpenEventLogW';
function OpenFileMapping(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE; external 'kernel32' name 'OpenFileMappingW';
function OpenMutex(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE; external 'kernel32' name 'OpenMutexW';
function OpenSCManager(lpMachineName:LPCWSTR; lpDatabaseName:LPCWSTR; dwDesiredAccess:DWORD):SC_HANDLE; external 'advapi32' name 'OpenSCManagerW';
function OpenSemaphore(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE; external 'kernel32' name 'OpenSemaphoreW';
function OpenService(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; dwDesiredAccess:DWORD):SC_HANDLE; external 'advapi32' name 'OpenServiceW';
function OpenWindowStation(lpszWinSta:LPWSTR; fInherit:WINBOOL; dwDesiredAccess:DWORD):HWINSTA; external 'user32' name 'OpenWindowStationW';
function PeekConsoleInput(hConsoleInput:HANDLE; lpBuffer:PINPUTRECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL; external 'kernel32' name 'PeekConsoleInputW';
function PolyTextOut(_para1:HDC; _para2:PPOLYTEXT; _para3:longint):WINBOOL; external 'gdi32' name 'PolyTextOutW';
function PrintDlg(_para1:LPPRINTDLG):WINBOOL; external 'comdlg32' name 'PrintDlgW';
function PrivilegedServiceAuditAlarm(SubsystemName:LPCWSTR; ServiceName:LPCWSTR; ClientToken:HANDLE; Privileges:PPRIVILEGE_SET; AccessGranted:WINBOOL):WINBOOL; external 'advapi32' name 'PrivilegedServiceAuditAlarmW';
function QueryDosDevice(lpDeviceName:LPCWSTR; lpTargetPath:LPWSTR; ucchMax:DWORD):DWORD; external 'kernel32' name 'QueryDosDeviceW';
function QueryServiceConfig(hService:SC_HANDLE; lpServiceConfig:LPQUERY_SERVICE_CONFIG; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL; external 'advapi32' name 'QueryServiceConfigW';
function QueryServiceLockStatus(hSCManager:SC_HANDLE; lpLockStatus:LPQUERY_SERVICE_LOCK_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL; external 'advapi32' name 'QueryServiceLockStatusW';
function ReadConsole(hConsoleInput:HANDLE; lpBuffer:LPVOID; nNumberOfCharsToRead:DWORD; lpNumberOfCharsRead:LPDWORD; lpReserved:LPVOID):WINBOOL; external 'kernel32' name 'ReadConsoleW';
function ReadConsoleInput(hConsoleInput:HANDLE; lpBuffer:PINPUTRECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL; external 'kernel32' name 'ReadConsoleInputW';
function ReadConsoleOutput(hConsoleOutput:HANDLE; lpBuffer:PCHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpReadRegion:PSMALL_RECT):WINBOOL; external 'kernel32' name 'ReadConsoleOutputW';
function ReadConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPWSTR; nLength:DWORD; dwReadCoord:COORD; lpNumberOfCharsRead:LPDWORD):WINBOOL; external 'kernel32' name 'ReadConsoleOutputCharacterW';
function ReadEventLog(hEventLog:HANDLE; dwReadFlags:DWORD; dwRecordOffset:DWORD; lpBuffer:LPVOID; nNumberOfBytesToRead:DWORD;pnBytesRead:LPDWORD; pnMinNumberOfBytesNeeded:LPDWORD):WINBOOL; external 'advapi32' name 'ReadEventLogW';
function RegConnectRegistry(lpMachineName:LPWSTR; hKey:HKEY; phkResult:PHKEY):LONG; external 'advapi32' name 'RegConnectRegistryW';
function RegisterClassEx(_para1:LPWNDCLASSEXW):ATOM; external 'user32' name 'RegisterClassExW';
function RegisterEventSource(lpUNCServerName:LPCWSTR; lpSourceName:LPCWSTR):HANDLE; external 'advapi32' name 'RegisterEventSourceW';
function RegisterServiceCtrlHandler(lpServiceName:LPCWSTR; lpHandlerProc:LPHANDLER_FUNCTION):SERVICE_STATUS_HANDLE; external 'advapi32' name 'RegisterServiceCtrlHandlerW';
function RegEnumKey(hKey:HKEY; dwIndex:DWORD; lpName:LPWSTR; cbName:DWORD):LONG; external 'advapi32' name 'RegEnumKeyW';
function RegLoadKey(hKey:HKEY; lpSubKey:LPCWSTR; lpFile:LPCWSTR):LONG; external 'advapi32' name 'RegLoadKeyW';
function RegOpenKey(hKey:HKEY; lpSubKey:LPCWSTR; phkResult:PHKEY):LONG; external 'advapi32' name 'RegOpenKeyW';
function RegQueryMultipleValues(hKey:HKEY; val_list:PVALENT; num_vals:DWORD; lpValueBuf:LPWSTR; ldwTotsize:LPDWORD):LONG; external 'advapi32' name 'RegQueryMultipleValuesW';
function RegQueryValue(hKey:HKEY; lpSubKey:LPCWSTR; lpValue:LPWSTR; lpcbValue:PLONG):LONG; external 'advapi32' name 'RegQueryValueW';
function RegRestoreKey(hKey:HKEY; lpFile:LPCWSTR; dwFlags:DWORD):LONG; external 'advapi32' name 'RegRestoreKeyW';
function RegSetValue(hKey:HKEY; lpSubKey:LPCWSTR; dwType:DWORD; lpData:LPCWSTR; cbData:DWORD):LONG; external 'advapi32' name 'RegSetValueW';
function RegSaveKey(hKey:HKEY; lpFile:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):LONG; external 'advapi32' name 'RegSaveKeyW';
function RegUnLoadKey(hKey:HKEY; lpSubKey:LPCWSTR):LONG; external 'advapi32' name 'RegUnLoadKeyW';
function RemoveProp(hWnd:HWND; lpString:LPCWSTR):HANDLE; external 'user32' name 'RemovePropW';
function RegCreateKey(hKey:HKEY; lpSubKey:LPCWSTR; phkResult:PHKEY):LONG; external 'advapi32' name 'RegCreateKeyW';
function RegReplaceKey(hKey:HKEY; lpSubKey:LPCWSTR; lpNewFile:LPCWSTR; lpOldFile:LPCWSTR):LONG; external 'advapi32' name 'RegReplaceKeyW';
function ReplaceText(_para1:LPFINDREPLACE):HWND; external 'comdlg32' name 'ReplaceTextW';
function ReportEvent(hEventLog:HANDLE; wType:WORD; wCategory:WORD; dwEventID:DWORD; lpUserSid:PSID;wNumStrings:WORD; dwDataSize:DWORD; lpStrings:LPCWSTR; lpRawData:LPVOID):WINBOOL; external 'advapi32' name 'ReportEventW';
  GrantedAccess:LPDWORD; AccessStatus:LPBOOL;pfGenerateOnClose:LPBOOL):WINBOOL; external 'advapi32' name 'AccessCheckAndAuditAlarmW';
function ResetDC(_para1:HDC; _para2:LPDEVMODE):HDC; external 'gdi32' name 'ResetDCW';
function ScrollConsoleScreenBuffer(hConsoleOutput:HANDLE; lpScrollRectangle:PSMALL_RECT; lpClipRectangle:PSMALL_RECT; dwDestinationOrigin:COORD; lpFill:PCHAR_INFO):WINBOOL; external 'kernel32' name 'ScrollConsoleScreenBufferW';
function SearchPath(lpPath:LPCWSTR; lpFileName:LPCWSTR; lpExtension:LPCWSTR; nBufferLength:DWORD; lpBuffer:LPWSTR;lpFilePart:LPWSTR):DWORD; external 'kernel32' name 'SearchPathW';
function SendMessageCallback(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; lpResultCallBack:SENDASYNCPROC;dwData:DWORD):WINBOOL; external 'user32' name 'SendMessageCallbackW';
function SendMessageTimeout(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; fuFlags:UINT;uTimeout:UINT; lpdwResult:LPDWORD):LRESULT; external 'user32' name 'SendMessageTimeoutW';
function SetComputerName(lpComputerName:LPCWSTR):WINBOOL; external 'kernel32' name 'SetComputerNameW';
function SetConsoleTitle(lpConsoleTitle:LPCWSTR):WINBOOL; external 'kernel32' name 'SetConsoleTitleW';
function SetCurrentDirectory(lpPathName:LPCWSTR):WINBOOL; external 'kernel32' name 'SetCurrentDirectoryW';
function SetDefaultCommConfig(lpszName:LPCWSTR; lpCC:LPCOMMCONFIG; dwSize:DWORD):WINBOOL; external 'kernel32' name 'SetDefaultCommConfigW';
function SetEnvironmentVariable(lpName:LPCWSTR; lpValue:LPCWSTR):WINBOOL; external 'kernel32' name 'SetEnvironmentVariableW';
function SetFileSecurity(lpFileName:LPCWSTR; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL; external 'advapi32' name 'SetFileSecurityW';
function SetICMProfile(_para1:HDC; _para2:LPWSTR):WINBOOL; external 'gdi32' name 'SetICMProfileW';
function SetProp(hWnd:HWND; lpString:LPCWSTR; hData:HANDLE):WINBOOL; external 'user32' name 'SetPropW';
function SetUserObjectInformation(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD):WINBOOL; external 'user32' name 'SetUserObjectInformationW';
function SetVolumeLabel(lpRootPathName:LPCWSTR; lpVolumeName:LPCWSTR):WINBOOL; external 'kernel32' name 'SetVolumeLabelW';
function ShellAbout(_para1:HWND; _para2:LPCWSTR; _para3:LPCWSTR; _para4:HICON):longint; external 'shell32' name 'ShellAboutW';
function ShellExecute(_para1:HWND; _para2:LPCWSTR; _para3:LPCWSTR; _para4:LPCWSTR; _para5:LPCWSTR;_para6:longint):HINST; external 'shell32' name 'ShellExecuteW';
function Shell_NotifyIcon(dwMessage: DWORD; lpData: PNotifyIconDataA): WINBOOL; external 'shell32' name 'Shell_NotifyIconW';
function StartServiceCtrlDispatcher(lpServiceStartTable:LPSERVICE_TABLE_ENTRY):WINBOOL; external 'advapi32' name 'StartServiceCtrlDispatcherW';
function StartService(hService:SC_HANDLE; dwNumServiceArgs:DWORD; lpServiceArgVectors:LPCWSTR):WINBOOL; external 'advapi32' name 'StartServiceW';
function TabbedTextOut(hDC:HDC; X:longint; Y:longint; lpString:LPCWSTR; nCount:longint;nTabPositions:longint; lpnTabStopPositions:LPINT; nTabOrigin:longint):LONG; external 'user32' name 'TabbedTextOutW';
function TextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCWSTR; _para5:longint):WINBOOL; external 'gdi32' name 'TextOutW';
function UpdateICMRegKey(_para1:DWORD; _para2:DWORD; _para3:LPWSTR; _para4:UINT):WINBOOL; external 'gdi32' name 'UpdateICMRegKeyW';
function UpdateResource(hUpdate:HANDLE; lpType:LPCWSTR; lpName:LPCWSTR; wLanguage:WORD; lpData:LPVOID;cbData:DWORD):WINBOOL; external 'kernel32' name 'UpdateResourceW';
function VerFindFile(uFlags:DWORD; szFileName:LPWSTR; szWinDir:LPWSTR; szAppDir:LPWSTR; szCurDir:LPWSTR;lpuCurDirLen:PUINT; szDestDir:LPWSTR; lpuDestDirLen:PUINT):DWORD; external 'version' name 'VerFindFileW';
function VerInstallFile(uFlags:DWORD; szSrcFileName:LPWSTR; szDestFileName:LPWSTR; szSrcDir:LPWSTR; szDestDir:LPWSTR;szCurDir:LPWSTR; szTmpFile:LPWSTR; lpuTmpFileLen:PUINT):DWORD; external 'version' name 'VerInstallFileW';
function VerLanguageName(wLang:DWORD; szLang:LPWSTR; nSize:DWORD):DWORD; external 'kernel32' name 'VerLanguageNameW';
function VkKeyScan(ch:WCHAR):SHORT; external 'user32' name 'VkKeyScanW';
function VkKeyScanEx(ch:WCHAR; dwhkl:HKL):SHORT; external 'user32' name 'VkKeyScanExW';
function WaitNamedPipe(lpNamedPipeName:LPCWSTR; nTimeOut:DWORD):WINBOOL; external 'kernel32' name 'WaitNamedPipeW';
function WinHelp(hWndMain:HWND; lpszHelp:LPCWSTR; uCommand:UINT; dwData:DWORD):WINBOOL; external 'user32' name 'WinHelpW';
function WNetAddConnection(lpRemoteName:LPCWSTR; lpPassword:LPCWSTR; lpLocalName:LPCWSTR):DWORD; external 'mpr' name 'WNetAddConnectionW';
function WNetAddConnection2(lpNetResource:LPNETRESOURCE; lpPassword:LPCWSTR; lpUserName:LPCWSTR; dwFlags:DWORD):DWORD; external 'mpr' name 'WNetAddConnection2W';
function WNetCancelConnection(lpName:LPCWSTR; fForce:WINBOOL):DWORD; external 'mpr' name 'WNetCancelConnectionW';
function WNetGetProviderName(dwNetType:DWORD; lpProviderName:LPWSTR; lpBufferSize:LPDWORD):DWORD; external 'mpr' name 'WNetGetProviderNameW';
function WNetGetNetworkInformation(lpProvider:LPCWSTR; lpNetInfoStruct:LPNETINFOSTRUCT):DWORD; external 'mpr' name 'WNetGetNetworkInformationW';
function WNetGetLastError(lpError:LPDWORD; lpErrorBuf:LPWSTR; nErrorBufSize:DWORD; lpNameBuf:LPWSTR; nNameBufSize:DWORD):DWORD; external 'mpr' name 'WNetGetLastErrorW';
function WNetSetConnection(lpName:LPCWSTR; dwProperties:DWORD; pvValues:LPVOID):DWORD; external 'mpr' name 'WNetSetConnectionW';
function WNetUseConnection(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpUserID:LPCWSTR; lpPassword:LPCWSTR; dwFlags:DWORD;lpAccessName:LPWSTR; lpBufferSize:LPDWORD; lpResult:LPDWORD):DWORD; external 'mpr' name 'WNetUseConnectionW';
function WriteConsole(hConsoleOutput:HANDLE;lpBuffer:pointer; nNumberOfCharsToWrite:DWORD; lpNumberOfCharsWritten:LPDWORD; lpReserved:LPVOID):WINBOOL; external 'kernel32' name 'WriteConsoleW';
function WriteConsoleInput(hConsoleInput:HANDLE; lpBuffer:PINPUTRECORD; nLength:DWORD; lpNumberOfEventsWritten:LPDWORD):WINBOOL; external 'kernel32' name 'WriteConsoleInputW';
function WriteConsoleOutput(hConsoleOutput:HANDLE; lpBuffer:PCHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpWriteRegion:PSMALL_RECT):WINBOOL; external 'kernel32' name 'WriteConsoleOutputW';
function WriteConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPCWSTR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL; external 'kernel32' name 'WriteConsoleOutputCharacterW';
function WritePrivateProfileSection(lpAppName:LPCWSTR; lpString:LPCWSTR; lpFileName:LPCWSTR):WINBOOL; external 'kernel32' name 'WritePrivateProfileSectionW';
function WritePrivateProfileString(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpString:LPCWSTR; lpFileName:LPCWSTR):WINBOOL; external 'kernel32' name 'WritePrivateProfileStringW';
function WriteProfileSection(lpAppName:LPCWSTR; lpString:LPCWSTR):WINBOOL; external 'kernel32' name 'WriteProfileSectionW';
function WriteProfileString(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpString:LPCWSTR):WINBOOL; external 'kernel32' name 'WriteProfileStringW';

//end win32 only
{$endif WIN32}

{$endif read_interface}


{$ifdef read_implementation}

//begin common win32 & wince

function CommDlg_OpenSave_GetFolderPath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFolderPath:=SNDMSG(_hdlg,CDM_GETFOLDERPATH,WPARAM(_cbmax),LPARAM(LPWSTR(_psz)));
end;

function CommDlg_OpenSave_GetFilePath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetFilePath:=SNDMSG(_hdlg,CDM_GETFILEPATH,WPARAM(_cbmax),LPARAM(_psz));
end;

function CommDlg_OpenSave_GetSpec(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
begin
  CommDlg_OpenSave_GetSpec:=SNDMSG(_hdlg,CDM_GETSPEC,WPARAM(_cbmax),LPARAM(_psz));
end;

function CreateWindow(lpClassName:LPCWSTR; lpWindowName:LPCWSTR; dwStyle:DWORD; X:longint;Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU;hInstance:HINST; lpParam:LPVOID):HWND;
begin
  CreateWindow:=CreateWindowEx(0,lpClassName,lpWindowName,dwStyle,x,y,nWidth,nHeight,hWndParent,hMenu,hInstance,lpParam);
end;

function CreateDialogIndirect(hInstance:HINST; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
begin
  CreateDialogIndirect:=CreateDialogIndirectParam(hInstance,lpTemplate,hWndParent,lpDialogFunc,0);
end;

function DialogBox(hInstance:HINST; lpTemplate:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
begin
  DialogBox:=DialogBoxParam(hInstance,lpTemplate,hWndParent,lpDialogFunc,0);
end;

function DialogBoxIndirect(hInstance:HINST; lpTemplate:LPCDLGTEMPLATEW; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
begin
  DialogBoxIndirect:=DialogBoxIndirectParam(hInstance,lpTemplate,hWndParent,lpDialogFunc,0);
end;

//end common win32 & wince

{$ifdef WINCE}
//begin wince only
function CreateDialogParam(hInstance:HINST; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND;
begin
  CreateDialogParam:=CreateDialogIndirectParam(hInstance,
        LPCDLGTEMPLATEW(LoadResource(hInstance, FindResource(hInstance, lpTemplateName, RT_DIALOG))),
        hWndParent, lpDialogFunc, dwInitParam);
end;

function CreateDialog(hInstance:HINST; lpName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
begin
  CreateDialog:=CreateDialogParam(hInstance, lpName, hWndParent, lpDialogFunc, 0);
end;

function DialogBoxParam(hInstance:HINST; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint;
begin
 DialogBoxParam:=DialogBoxIndirectParam( hInstance,
                                         LPCDLGTEMPLATEW(LoadResource(hInstance, FindResource(hInstance, lpTemplateName, RT_DIALOG))),
                                         hWndParent,
                                         lpDialogFunc,
                                         dwInitParam);
end;

function GetTextExtentPoint(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL;
begin
  GetTextExtentPoint:=GetTextExtentExPoint(_para1, _para2, _para3, 0, nil, nil, _para4);
end;

function GetTextExtentPoint32(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL;
begin
  GetTextExtentPoint32:=GetTextExtentExPoint(_para1, _para2, _para3, 0, nil, nil, _para4);
end;

//end wince only
{$endif WINCE}

{$ifdef WIN32}
//begin win32 only

function CreateDialog(hInstance:HINST; lpName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
begin
  CreateDialog:=CreateDialogParam(hInstance,lpName,hWndParent,lpDialogFunc,0);
end;

//end win32 only



{$endif WIN32}

{$endif read_implementation}

