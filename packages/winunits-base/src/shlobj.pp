{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Florian Klaempfl
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Original copyright statement follows.

}
{$mode objfpc}
unit shlobj;

  interface

    uses
      windows,activex,shellapi;

    type
      IEnumIDList = interface(IUnknown)
        ['{000214F2-0000-0000-C000-000000000046}']
        function Next(celt: ULONG; out rgelt: PItemIDList; var pceltFetched: ULONG): HRESULT; stdcall;
        function Skip(celt: ULONG): HRESULT; stdcall; function Reset: HRESULT; stdcall;
        function Clone(out ppenum: IEnumIDList): HRESULT; stdcall;
      end;

      IShellFolder = interface(IUnknown)
        ['{000214E6-0000-0000-C000-000000000046}']
        function ParseDisplayName(hwndOwner: HWND; pbcReserved: Pointer; lpszDisplayName: POLESTR; out pchEaten: ULONG; out ppidl: PItemIDList; var dwAttributes: ULONG): HRESULT; stdcall;
        function EnumObjects(hwndOwner: HWND; grfFlags: DWORD; out EnumIDList: IEnumIDList): HRESULT; stdcall;
        function BindToObject(pidl: PItemIDList; pbcReserved: Pointer; const riid: TIID; out ppvOut): HRESULT; stdcall;
        function BindToStorage(pidl: PItemIDList; pbcReserved: Pointer; const riid: TIID; out ppvObj): HRESULT; stdcall;
        function CompareIDs(lParam: LPARAM; pidl1, pidl2: PItemIDList): HRESULT; stdcall;
        function CreateViewObject(hwndOwner: HWND; const riid: TIID; out ppvOut): HRESULT; stdcall;
        function GetAttributesOf(cidl: UINT; var apidl: PItemIDList; var rgfInOut: UINT): HRESULT; stdcall;
        function GetUIObjectOf(hwndOwner: HWND; cidl: UINT; var apidl: PItemIDList; const riid: TIID; prgfInOut: Pointer; out ppvOut): HRESULT; stdcall;
        function GetDisplayNameOf(pidl: PItemIDList; uFlags: DWORD; var lpName: TStrRet): HRESULT; stdcall;
        function SetNameOf(hwndOwner: HWND; pidl: PItemIDList; lpszName: POLEStr; uFlags: DWORD; var ppidlOut: PItemIDList): HRESULT; stdcall;
      end;

      IAutoComplete = interface(IUnknown)
        ['{00bb2762-6a77-11d0-a535-00c04fd7d062}']
        function Init(hwndEdit: HWND; punkACL: IUnknown; pwszRegKeyPath: LPCWSTR; pwszQuickComplete: LPCWSTR): HRESULT; stdcall;
        function Enable(fEnable: BOOL): HRESULT; stdcall;
      end;

    const
      CLSID_AutoComplete: TGUID = '{00BB2763-6A77-11D0-A535-00C04FD7D062}';

    const
      { IAutoComplete2 options }
      ACO_NONE           = 0;
      ACO_AUTOSUGGEST    = $1;
      ACO_AUTOAPPEND     = $2;
      ACO_SEARCH         = $4;
      ACO_FILTERPREFIXES = $8;
      ACO_USETAB         = $10;
      ACO_UPDOWNKEYDROPSLIST = $20;
      ACO_RTLREADING     = $40;

    type
      IAutoComplete2 = interface(IAutoComplete)
        ['{EAC04BC0-3791-11d2-BB95-0060977B464C}']
        function SetOptions(dwFlag: DWORD): HRESULT; stdcall;
        function GetOptions(var dwFlag: DWORD): HRESULT; stdcall;
      end;

     PCMINVOKECOMMANDINFO = ^TCMINVOKECOMMANDINFO;
     TCMINVOKECOMMANDINFO = packed record
          cbSize : DWORD;
          fMask  : DWORD;
          hwnd   : HWND;
          lpVerb : LPCSTR;
          lpParameters : LPCSTR;
          lpDirectory : LPCSTR;
          nShow  :  longint;
          dwHotKey: DWORD;
          hIcon  : THANDLE;
       end;
     LPCMINVOKECOMMANDINFO = PCMINVOKECOMMANDINFO;

    IContextMenu = interface(IUnknown)
         ['{000214E4-0000-0000-c000-000000000046}']
         function QueryContextMenu(hmenu:HMENU;indexMenu:UINT;idCmdFirst:UINT;idCmdLast:UINT;UFlags:uint):HRESULT;StdCall;
         function InvokeCommand(lpici : LPCMINVOKECOMMANDINFO):HResult; StdCall;         
         function GetCommandString(idcmd:UINT_Ptr;uType:UINT;pwreserved:puint;pszName:LPStr;cchMax:uint):HResult;StdCall;
       end;
    IContextMenu2 = interface(IContextMenu)
         ['{000214f4-0000-0000-c000-000000000046}']
         function HandleMenuMsg(uMsg:UINT;wParam:WPARAM;lParam:WPARAM):HResult;StdCall;
         end;
    IContextMenu3 = interface(IContextMenu2)
         ['{bcfce0a0-ec17-11d0-8d10-00a0c90f2719}']
         function HandleMenuMsg2(uMsg:UINT;wParam:WPARAM;lParam:WPARAM;presult:PLRESULT):HResult;StdCall;
         end;
    IEXtractIconA = interface(IUNknown)
         ['{000214eb-0000-0000-c000-000000000046}']
         function GetIconLocation(uFlags:UINT;szIconFIle:LPSTR;cchMax:UINT;piIndex : pint; pwflags:puint):HResult;StdCall;
         function Extract(pszFile:LPCStr;nIconIndex:UINT;phiconLarge:PHICON;phiconSmall:PHICON;nIconSize:UINT):HResult;StdCall;
         end;

    IEXtractIconW = interface(IUNknown)
         ['{000214fa-0000-0000-c000-000000000046}']
         function GetIconLocation(uFlags:UINT;szIconFIle:LPWSTR;cchMax:UINT;piIndex : pint; pwflags:puint):HResult;StdCall;
         function Extract(pszFile:LPCWStr;nIconIndex:UINT;phiconLarge:PHICON;phiconSmall:PHICON;nIconSize:UINT):HResult;StdCall;
         end;
    IEXtractIcon=IExtractIconA;

function SHGetMalloc(out ppmalloc: IMalloc):HResult;StdCall; external 'shell32' name 'SHGetMalloc';

implementation

end.
