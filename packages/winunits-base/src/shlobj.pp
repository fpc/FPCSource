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

Const 
   IID_IShellFolder    : TGUID ='{000214E6-0000-0000-C000-000000000046}';
   IID_IEnumList       : TGUID ='{000214F2-0000-0000-C000-000000000046}';
   IID_IAutoComplete   : TGUID ='{00bb2762-6a77-11d0-a535-00c04fd7d062}';
   IID_IAutoComplete2  : TGUID ='{EAC04BC0-3791-11d2-BB95-0060977B464C}';
   IID_IContextMenu    : TGUID ='{000214E4-0000-0000-c000-000000000046}';
   IID_IContextMenu2   : TGUID ='{000214f4-0000-0000-c000-000000000046}';
   IID_IContextMenu3   : TGUID ='{bcfce0a0-ec17-11d0-8d10-00a0c90f2719}';
   IID_IPersistFolder  : TGUID ='{000214EA-0000-0000-C000-000000000046}';
   IID_IPersistFolder2 : TGUID ='{1AC3D9F0-175C-11d1-95BE-00609797EA4F}';
   IID_IPersistIDListr : TGUID ='{1079acfc-29bd-11d3-8e0d-00c04f6837d5}';
   IID_IEnumExtraSearch: TGUID ='{0E700BE1-9DB6-11d1-A1CE-00C04FD75D13}';
   IID_IShellFolder2   : TGUID ='{93F2F68C-1D1B-11d3-A30E-00C04F79ABD1}';

Type 
      _SHELLDETAILS        =  record
                               fmt,
                               cxChar    : longint;
                               str       : TSTRRET;
                               end;
      TShellDetails        =  _SHELLDETAILS;
      SHELLDETAILS         =  _SHELLDETAILS;
      PShellDetails        =  ^TShellDetails;
      LPSHELLDETAILS       = PSHELLDETAILS;

      TShellDetailsEx      =  record
         Index:            UINT;
         Detail:           TShellDetails;
      end;
      tagEXTRASEARCH = packed record
                              guidSearch :     TGUID;
                              wszFriendlyName : array[0..80-1] of WideChar;
                              wszUrl : array[0..2084-1] of WideChar;
                             end;
      EXTRASEARCH  = TagEXTRASEARCH;
      TEXTRASEARCH  = TagEXTRASEARCH;
      LPEXTRASEARCH = ^EXTRASEARCH;
      PEXTRASEARCH  = ^EXTRASEARCH;

      SHCOLSTATEF = DWORD;
      PSHCOLSTATEF = ^SHCOLSTATEF;
      TSHCOLSTATEF = SHCOLSTATEF;

      SHColumnID = packed record
                    fmtid : TGUID;
                    pid   : DWORD;
                   end;
      LPSHColumnID = SHColumnID;
      TSHColumnid = SHColumnID;
      pSHColumnID = LPSHColumnID;

   IPersistFolder = Interface(IPersist)
        ['{000214EA-0000-0000-C000-000000000046}']
        function Initialize (pild : LPCITEMIDLIST): HResult; StdCall;
    end;

   IPersistFolder2 = Interface(IPersistFolder)
        ['{1AC3D9F0-175C-11d1-95BE-00609797EA4F}']
        function GetCurFolder(Out ppidl : LPITEMIDLIST):HResult; StdCall;
       end;

   IPersistIDList = Interface(IPersist)
        ['{1079acfc-29bd-11d3-8e0d-00c04f6837d5}']
         function SetIdList(pid:LPCITEMIDLIST):HResult;StdCall; 
        function GetIdList(out pid:LPCITEMIDLIST):HResult;StdCall;
        end;

   IEnumIDList = interface(IUnknown)
        ['{000214F2-0000-0000-C000-000000000046}']
        function Next(celt: ULONG; out rgelt: PItemIDList; var pceltFetched: ULONG): HRESULT; stdcall;
        function Skip(celt: ULONG): HRESULT; stdcall; function Reset: HRESULT; stdcall;
        function Clone(out ppenum: IEnumIDList): HRESULT; stdcall;
      end;

   IEnumExtraSearch = Interface(IUnknown)
       ['{0E700BE1-9DB6-11d1-A1CE-00C04FD75D13}']
       function Next(celt: ULONG; out rgelt: EXTRASEARCH; var pceltFetched: ULONG): HRESULT; stdcall;
       function Skip(celt: ULONG): HRESULT; stdcall; function Reset: HRESULT; stdcall;
       function Clone(out ppenum: IEnumExtraSearch): HRESULT; stdcall;
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

   IShellFolder2 = interface(IShellFolder)
     ['{93F2F68C-1D1B-11d3-A30E-00C04F79ABD1}']
      function GetDefaultSearchGUID(out guid:TGUID):HResult;StdCall;
      function EnumSearches(out ppenum:IEnumExtraSearch):HResult;StdCall;    
      function GetDefaultColumn(dwres:DWORD;psort :pulong; pdisplay:pulong):HResult;StdCall;   
      function GetDefaultColumnStart(icolumn:UINT;pscflag:PSHCOLSTATEF):HResult;StdCall;   
      function GetDetailsEx(pidl:LPCITEMIDLIST;pscid:PSHCOLUMNID; pv : pOLEvariant):HResult;StdCall;   
      function GetDetailsOf(pidl:LPCITEMIDLIST;iColumn:UINT;psd:PSHELLDETAILS):HResult;StdCall;   
      function MapColumnToSCID(iColumn:UINT;pscid:PSHCOLUMNID):HResult;StdCall;   
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
         function InvokeCommand(var lpici : TCMINVOKECOMMANDINFO):HResult; StdCall;         
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
