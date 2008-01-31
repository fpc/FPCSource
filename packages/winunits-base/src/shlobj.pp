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
      windows,activex,shellapi,commctrl;

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

Const
  SV2GV_CURRENTVIEW  = DWORD(-1);
  SV2GV_DEFAULTVIEW  = DWORD(-2);

Type 
     IShellView = Interface;
     IShellBrowser = Interface;

     LPTBBUTTONSB = LPTBBUTTON;
     SVSIF = UINT;
     TSVSIF = SVSIF;
     SHELLVIEWID = TGUID;
     TSHELLVIEWID = TGUID;
     PSHELLVIEWID = ^TGUID;
     LPVIEWSETTINGS = Pchar;
     FOLDERSettings = Packed Record 
                        ViewMode : UINT;       // View mode (FOLDERVIEWMODE values)
                        fFlags   : UINT;       // View options (FOLDERFLAGS bits)
                      end;
     TFOLDERSettings = FOLDERSettings;
     PFOLDERSettings = ^FOLDERSettings;
     LPFOLDERSettings= PFOLDERSettings;
     LPCFOLDERSettings= LPFOLDERSettings;
     PSV2CVW2_PARAMS = ^TSV2CVW2_PARAMS; 
     TSV2CVW2_PARAMS = packed record  // actually  <pshpack8.h>")
          cbSize    : DWORD;
          psvPrev   : IShellView;
          pfs       : LPCFOLDERSETTINGS;
          psbOwner  : IShellBrowser;
          prcView   : PRECT;
          pvid      : PSHELLVIEWID;
          hwndView  : HWND;
       end;
     LPSV2CVW2_PARAMS = PSV2CVW2_PARAMS;
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

  IShellView    = Interface(IOleWindow)
         ['{000214E3-0000-0000-C000-000000000046}']
         function TranslateAccelerator( pm :PMSG):HResult; StdCall;
         function EnableModeless(fEnable : BOOL):HResult; StdCall;
         function UIActivate(uState:UINT):HResult; StdCall;
         function Refresh:HResult; StdCall;
         function CreateViewWindow(psvPrevious:IShellView;pfs:LPCFOLDERSETTINGS;psb:IShellBrowser;prcview:prect;var ph:HWND):HResult;StdCall;
         function DestroyViewWindow:HResult; StdCall;        
         function GetCurrentInfo(pfs: LPFOLDERSETTINGS):HResult; StdCall;     
         function AddPropertySheetPages(dwreserved : DWORD;pfn:pointer{LPFNSVADDPROPSHEETPAGE};lp:lparam):HResult; StdCall;     
         function SaveViewState:HResult; StdCall;       
         function SelectItem( pidlItem: LPCITEMIDLIST;uflags:TSVSIF):HResult; StdCall;       
         function GetItemObject(uitem:UINT;const riid:TGUID;out ppv :PPOinter):HResult;StdCall;
       end;

  IShellView2    = Interface(IShellView)
         ['{88E39E80-3578-11CF-AE69-08002B2E1262}']
         function GetView(var pvid:TSHELLVIEWID ;uview:ULONG):HResult;StdCall;
         function CreateViewWindow2(lpParams:LPSV2CVW2_PARAMS):HResult;StdCall;
         function HandleRename(pidlNew: LPCITEMIDLIST ):HResult;StdCall;
         function SelectAndPositionItem(pidlItem:LPCITEMIDLIST ;uflags:UINT;ppt:PPOINT):HRESULT;STDCALL;
         end;

   IFolderView = Interface(IUnknown)
        ['{cde725b0-ccc9-4519-917e-325d72fab4ce}']
        function GetCurrentViewMode(pViewMode:PUINT):HResult; StdCall;       
        function SetCurrentViewMode(ViewMode:UINT):HResult; StdCall;       
        function GetFolder(const riid:TGUID;ppv:pointer):HResult; StdCall;       
        function Item(iItemIndex:longint;ppidl:LPITEMIDLIST):HResult; StdCall;       
        function ItemCount(uflags:uint;pcitems:plongint):HResult; StdCall;       
        function Items (uflags:uint;const id :TGUID;out ppv: pointer):HResult; StdCall;       
        function GetSelectionMarkedItem(piItem:pint):HResult; StdCall;       
        function GetFocussedItem(piItem:pint):HResult; StdCall;       
        function GetItemPosition(pidl:LPCITEMIDLIST;ppt:PPOINT):HResult; StdCall;       
        function GetSpacing(ppt:ppoint):HResult; StdCall;       
        function GetDefaultSpacing(ppt:ppoint):HResult; StdCall;       
        function GetAutoArrange:HResult; StdCall;       
        function SelectItem(iItem : longint;dwflags:Dword) :HResult; StdCall;       
        function SelectAndPositionItems(cild:uint;var apid: LPCITEMIDLIST   ;apt:PPOINT;dwflags:DWord):HResult; StdCall;       
       end;           
    IFolderFilterSite = Interface(IUnknown)
          ['{C0A651F5-B48B-11d2-B5ED-006097C686F6}']
          function SetFilter(punk:IUnknown):HResult; StdCall;
          end;
    IFolderFilter = Interface(IUnknown)
          ['{9CC22886-DC8E-11d2-B1D0-00C04F8EEB3E}']
          function ShouldShow(Psf:IShellFolder;pidlfolder:LPCITEMIDLIST;pidlItem:LPCITEMIDLIST):HResult; StdCall;
          function GetEnumFlags(Psf:IShellFolder;pidlfolder:LPCITEMIDLIST;var hwnd: hwnd;out pgrfflags:DWORD):HResult; StdCall;
          end;

//cpp_quote("#include <commctrl.h>")
//cpp_quote("typedef LPTBBUTTON LPTBBUTTONSB;")

    IShellBrowser = interface(IOleWindow)
          ['{000214E2-0000-0000-C000-000000000046}']
    function InsertMenusSB(hmenuShared: HMenu; var menuWidths: TOleMenuGroupWidths): HResult;StdCall;
    function SetMenuSB(hmenuShared: HMenu; holemenu: HOLEMenu; hwndActiveObject: HWnd): HResult;StdCall;
    function RemoveMenusSB(hmenuShared: HMenu): HResult;StdCall;
    function SetStatusTextSB(pszStatusText: POleStr): HResult;StdCall;
    function EnableModelessSB(fEnable: BOOL): HResult;StdCall;
    function TranslateAcceleratorSB(var msg: TMsg; wID: Word): HResult;StdCall;
    function BrowseObject(pidl:LPCITEMIDLIST;wFlags:UINT): HResult;StdCall;
    function GetViewStateStream(grfMode :DWORD; out ppstrm :IStream): HResult;StdCall;
    function GetControlWindow(id:UINT;var h:HWND): HResult;StdCall;
    function SendCOntrolMsg(id:uint;umsg:UINT;wparam:wparam;lparam:lparam;pret:PLRESULT): HResult;StdCall;
    function QueryActiveShellView(out ppsh :IShellView): HResult;StdCall;
    function OnViewWindowActive(psh :IShellView): HResult;StdCall;
    function SetToolBarItems(lpButtons:LPTBBUTTONSB;nButtons:UINT;uFlags:uint): HResult;StdCall;
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
