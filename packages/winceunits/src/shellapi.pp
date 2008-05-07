{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006-2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{  Declarations for ceshell WinCE API

}

{exported functions list = to do,
 * please remove functions done *
 
     Exports

       ordinal    name

       49 ?DLL_SHGetSpecialFolderPath@@YAHPAUHWND__@@PAGHH@Z
       4E ?PathIsRestrictedEx@@YAHPBGH@Z
       16 ?SHRemoveFontResource@@YAHPAG@Z
       17 ?_SHRemoveFontResource@@YAHPAG@Z
        C DLL_SHGetFileInfo
        5 DoDragDrop
       38 Host_CheckStack
       3A Host_Exec
       35 Host_FindBigDaddy
       36 Host_MaxWindow
       39 Host_MessageBox
       37 Host_OpenPositionDB
       3B Host_ShortcutGetArgs
       3C Host_ShortcutRemoveArgs
       34 Host_ShowFileError
       42 ILConcatenate
       43 ILCopy
       41 ILFree
       46 ILGetFileSystemPidlData
       3D ILIsFileSystemPidl
       3F ILIsGUIDPidl
       3E ILIsNameSpacePidl
       40 ILIsPidl
       44 ILIsRemovableDevice
       45 ILRealPathFromPidl
       21 PathCompactPath
       20 PathCompactSlashes
       28 PathFileExists
       1D PathFindExtension
       1E PathFindFileName
       2C PathGetArgs
       23 PathGetAssociation
       22 PathIsDirectory
       24 PathIsExe
       26 PathIsExtension
       31 PathIsGUID
       25 PathIsLink
       32 PathIsRemovableDevice
       33 PathIsRestricted
       19 PathIsValidFileName
       1A PathIsValidPath
       27 PathMakePretty
       30 PathMakeUniqueName
       2F PathMatchSpec
       2B PathRemoveArgs
       1B PathRemoveBlanks
       29 PathRemoveExtension
       2A PathRemoveFileSpec
       2E PathRemoveQuotes
       2D PathRemoveQuotesAndArgs
       1C PathRemoveTrailingSlashes
       1F PathStripPath
        6 RegisterDragDrop
        7 RevokeDragDrop
       12 SHAddToRecentDocs
       4D SHCanonicalizePath
       10 SHCreateShortcut
       15 SHFlushCache
       4A SHGetCEString
        8 SHGetDesktopFolder
       48 SHGetDocumentsFolder
        B SHGetMalloc
       11 SHGetShortcutTarget
       18 SHIsFileOperationRestricted
       4C SHIsFileOperationRestrictedEx
       47 SHIsRestrictedProcess
        F SHLoadDIBitmap
        E SHLoadDIBitmapBrush
       13 SHRegQuerySZ
       14 SHRegQuerySZEx
        4 SHSetSystemEUDCFont
       4B SHUnpackDirID
        1 __IMPORT_DESCRIPTOR_CEShell
        2 __NULL_IMPORT_DESCRIPTOR
       49 __imp_?DLL_SHGetSpecialFolderPath@@YAHPAUHWND__@@PAGHH@Z
       4E __imp_?PathIsRestrictedEx@@YAHPBGH@Z
       16 __imp_?SHRemoveFontResource@@YAHPAG@Z
       17 __imp_?_SHRemoveFontResource@@YAHPAG@Z
        C __imp_DLL_SHGetFileInfo
        5 __imp_DoDragDrop
       38 __imp_Host_CheckStack
       3A __imp_Host_Exec
       35 __imp_Host_FindBigDaddy
       36 __imp_Host_MaxWindow
       39 __imp_Host_MessageBox
       37 __imp_Host_OpenPositionDB
       3B __imp_Host_ShortcutGetArgs
       3C __imp_Host_ShortcutRemoveArgs
       34 __imp_Host_ShowFileError
       42 __imp_ILConcatenate
       43 __imp_ILCopy
       41 __imp_ILFree
       46 __imp_ILGetFileSystemPidlData
       3D __imp_ILIsFileSystemPidl
       3F __imp_ILIsGUIDPidl
       3E __imp_ILIsNameSpacePidl
       40 __imp_ILIsPidl
       44 __imp_ILIsRemovableDevice
       45 __imp_ILRealPathFromPidl
       21 __imp_PathCompactPath
       20 __imp_PathCompactSlashes
       28 __imp_PathFileExists
       1D __imp_PathFindExtension
       1E __imp_PathFindFileName
       2C __imp_PathGetArgs
       23 __imp_PathGetAssociation
       22 __imp_PathIsDirectory
       24 __imp_PathIsExe
       26 __imp_PathIsExtension
       31 __imp_PathIsGUID
       25 __imp_PathIsLink
       32 __imp_PathIsRemovableDevice
       33 __imp_PathIsRestricted
       19 __imp_PathIsValidFileName
       1A __imp_PathIsValidPath
       27 __imp_PathMakePretty
       30 __imp_PathMakeUniqueName
       2F __imp_PathMatchSpec
       2B __imp_PathRemoveArgs
       1B __imp_PathRemoveBlanks
       29 __imp_PathRemoveExtension
       2A __imp_PathRemoveFileSpec
       2E __imp_PathRemoveQuotes
       2D __imp_PathRemoveQuotesAndArgs
       1C __imp_PathRemoveTrailingSlashes
       1F __imp_PathStripPath
        6 __imp_RegisterDragDrop
        7 __imp_RevokeDragDrop
       12 __imp_SHAddToRecentDocs
       4D __imp_SHCanonicalizePath
       10 __imp_SHCreateShortcut
       15 __imp_SHFlushCache
       4A __imp_SHGetCEString
        8 __imp_SHGetDesktopFolder
       48 __imp_SHGetDocumentsFolder
        B __imp_SHGetMalloc
       11 __imp_SHGetShortcutTarget
       18 __imp_SHIsFileOperationRestricted
       4C __imp_SHIsFileOperationRestrictedEx
       47 __imp_SHIsRestrictedProcess
        F __imp_SHLoadDIBitmap
        E __imp_SHLoadDIBitmapBrush
       13 __imp_SHRegQuerySZ
       14 __imp_SHRegQuerySZEx
        4 __imp_SHSetSystemEUDCFont
       4B __imp_SHUnpackDirID
        3 CEShell_NULL_THUNK_DATA

}

{$mode objfpc}
unit shellapi;

interface

uses windows;

{$calling cdecl}

//*****************************************************************************
// consts
//*****************************************************************************
const
  ShellDLL      = 'CEShell';

  FO_MOVE           = $0001;
  FO_COPY           = $0002;
  FO_DELETE         = $0003;
  FO_RENAME         = $0004;

  FOF_MULTIDESTFILES         = $0001;
  FOF_CONFIRMMOUSE           = $0002;
  FOF_SILENT                 = $0004;  // don't create progress/report
  FOF_RENAMEONCOLLISION      = $0008;
  FOF_NOCONFIRMATION         = $0010;  // Don't prompt the user.
  FOF_WANTMAPPINGHANDLE      = $0020;  // Fill in SHFILEOPSTRUCT.hNameMappings
                                      // Must be freed using SHFreeNameMappings
  FOF_ALLOWUNDO              = $0040;
  FOF_FILESONLY              = $0080;  // on *.*, do only files
  FOF_SIMPLEPROGRESS         = $0100;  // means don't show names of files
  FOF_NOCONFIRMMKDIR         = $0200;  // don't confirm making any needed dirs
  
//*****************************************************************************
// types
//*****************************************************************************

type
{ IMalloc interface }

  IMalloc = interface(IUnknown)
    ['{00000002-0000-0000-C000-000000000046}']
    function Alloc(cb: Longint): Pointer;
    function Realloc(pv: Pointer; cb: Longint): Pointer;
    procedure Free(pv: Pointer);
    function GetSize(pv: Pointer): Longint;
    function DidAlloc(pv: Pointer): longint;
    procedure HeapMinimize;
  end;
  LPMALLOC = ^IMalloc;
  PMALLOC = ^IMalloc;

//*****************************************************************************
// functions
//*****************************************************************************
function SHFileOperation(lpFileOp:LPSHFILEOPSTRUCTW): longint; external ShellDLL name 'SHFileOperationW';
function SHFileOperation(const lpFileOp:SHFILEOPSTRUCTW): longint; external ShellDLL name 'SHFileOperationW';
function SHFileOperationW(lpFileOp:LPSHFILEOPSTRUCTW): longint; external ShellDLL name 'SHFileOperationW';
function SHFileOperationW(const lpFileOp:SHFILEOPSTRUCTW): longint; external ShellDLL name 'SHFileOperationW';
function SHGetPathFromIDList(_para1:LPCITEMIDLIST; _para2:LPTSTR):WINBOOL; external ShellDLL name 'SHGetPathFromIDList';
function SHGetPathFromIDListW(_para1:LPCITEMIDLIST; _para2:LPTSTR):WINBOOL; external ShellDLL name 'SHGetPathFromIDList';
function SHGetMalloc(var ppMalloc: LPMALLOC): HRESULT; external ShellDLL name 'SHGetMalloc';
function SHGetSpecialFolderLocation(_para1:HWND; _para2:longint; var _para3:LPITEMIDLIST):HRESULT; external ShellDLL name 'SHGetSpecialFolderLocation';

implementation

end.
