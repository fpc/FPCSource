{ Shell helper routines to create Start Menu and Desktop shortcuts. }
{ (C) Copyright 2001, Tramontana Co. Released into the public domain. }
{ 2003.01.27: renamed WinShell to remember that it is windows specific by Pierre Muller }
{ 2003.01.28: IShellLinkDataList support added by Pierre Muller }

unit WinShell;

interface

uses
  Windows;
const
  { GetCurrentPlatform constants }
  pfAll = %11111111;
  pfWin31 = %00000001;
  pfWin95 = %00000010;
  pfWin98 = %00000100;
  pfWinME = %00001000;
  pfWin9x = pfWin95 or pfWin98 or pfWinME;
  pfWinNT3 = %00010000;
  pfWinNT4 = %00100000;
  pfWin2000 = %01000000;
  pfWinNTx = pfWinNT3 or pfWinNT4 or pfWin2000;
  pfWin16 = pfWin31;
  pfWin32 = pfWin9x or pfWinNTx;
  { Execution context constants }
  CLSCTX_INPROC_SERVER = 1;
  CLSCTX_INPROC_HANDLER = 2;
  CLSCTX_LOCAL_SERVER = 4;
  CLSCTX_INPROC_SERVER16 = 8;
  CLSCTX_REMOTE_SERVER = 16;
  CLSCTX_SERVER = CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER or CLSCTX_REMOTE_SERVER;
  CLSCTX_ALL = CLSCTX_INPROC_HANDLER or CLSCTX_SERVER;
  { SHGetSpecialFolder... constants }
  CSIDL_PROGRAMS = $0002;
  CSIDL_DESKTOPDIRECTORY = $0010;
  CSIDL_COMMON_PROGRAMS = $0017;
  CSIDL_COMMON_DESKTOPDIRECTORY = $0019;
  { Various GUIDs/CLSIDs }
  CLSID_ShellDesktop : GUID = (Data1 : $00021400; Data2 : 0; Data3 : 0;
    Data4 : ($C0, 0, 0, 0, 0, 0, 0, $46));
  CLSID_ShellLink : GUID = (Data1 : $00021401; Data2 : 0; Data3 : 0;
    Data4 : ($C0, 0, 0, 0, 0, 0, 0, $46));
  IID_IShellLink : GUID = (Data1 : $000214EE; Data2 : 0; Data3 : 0;
    Data4 : ($C0, 0, 0, 0, 0, 0, 0, $46));
  IID_IShellLinkDataList : GUID = (Data1 : $45E2B4AE; Data2 : $B1C3; Data3 : $11D0;
    Data4 : ($B9, $2f, 0, $A0, $C9, $03, $12, $E1));
  IID_IPersistFile : GUID = (Data1 : $0000010B; Data2 : 0; Data3 : 0;
    Data4 : ($C0, 0, 0, 0, 0, 0, 0, $46));
type
  {$PACKRECORDS 1}
  { COM interfaces -- without explicit compiler support, also runs with FPC 1.0x }
  { Note that the original Ole2.pp coming with the compiler is not working, this is how IUnknown should look like: }
  { IUnknown }
  PIUnknown = ^IUnknown;
  IUnknown = packed record
    vtbl : ^IUnknownVtbl;
    end;
  IUnknownVtbl = packed record
    QueryInterface : function (const this : PIUnknown; const iid: TIID; var obj): HResult; stdcall;
    AddRef : function (const this : PIUnknown) : longint; stdcall;
    Release : function (const this : PIUnknown) : longint; stdcall;
    end;
  { IMalloc }
  PPIMalloc = ^PIMalloc;
  PIMalloc = ^IMalloc;
  IMalloc = packed record
    vtbl : ^IMallocVtbl;
    end;
  IMallocVtbl = packed record
    QueryInterface : function (const this : PIMalloc; const iid: TIID; var obj): HResult; stdcall;
    AddRef : function (const this : PIMalloc) : ULONG; stdcall;
    Release : function (const this : PIMalloc) : ULONG; stdcall;
    Alloc : function (const this : PIMalloc; cb : ULONG) : pointer; stdcall;
    Realloc : function (const this : PIMalloc; var pv; cb : ULONG) : pointer; stdcall;
    Free : procedure (const this : PIMalloc; pv : pointer); stdcall;
    GetSize : function (const this : PIMalloc; pv : pointer) : ULONG; stdcall;
    DidAlloc : function (const this : PIMalloc; pv : pointer) : INT; stdcall;
    HeapMinimize : procedure (const this : PIMalloc); stdcall;
    end;
  { IShellLink }
  PIShellLink = ^IShellLink;
  IShellLink = packed record
    vtbl : ^IShellLinkVtbl;
    end;
  IShellLinkVtbl = packed record
    QueryInterface : function (const this : PIShellLink; const iid: TIID; var obj): HResult; stdcall;
    AddRef : function (const this : PIShellLink) : ULONG; stdcall;
    Release : function (const this : PIShellLink) : ULONG; stdcall;
    GetPath : function (const this : PIShellLink; pszFile : LPSTR; cchMaxPAth : INT; var fd : WIN32_FIND_DATA; Flags : DWORD) : hResult; stdcall;
    GetIDList : function (const this : PIShellLink; var pidl : LPITEMIDLIST) : hResult; stdcall;
    SetIDList : function (const this : PIShellLink; pidl : LPITEMIDLIST) : hResult; stdcall;
    GetDescription : function (const this : PIShellLink; pszName : LPSTR; cchMaxName : INT) : hResult; stdcall;
    SetDescription : function (const this : PIShellLink; pszName : LPSTR) : hResult; stdcall;
    GetWorkingDirectory : function (const this : PIShellLink; pszDir : LPSTR; cchMaxName : INT) : hResult; stdcall;
    SetWorkingDirectory : function (const this : PIShellLink; pszDir : LPSTR) : hResult; stdcall;
    GetArguments : function (const this : PIShellLink; pszArgs : LPSTR; cchMaxName : INT) : hResult; stdcall;
    SetArguments : function (const this : PIShellLink; pszArgs : LPSTR) : hResult; stdcall;
    GetHotkey : function (const this : PIShellLink; var wHotKey : WORD) : hResult; stdcall;
    SetHotkey : function (const this : PIShellLink; wHotKey : WORD) : hResult; stdcall;
    GetShowCmd : function (const this : PIShellLink; var iShowCmd : INT) : hResult; stdcall;
    SetShowCmd : function (const this : PIShellLink; iShowCmd : INT) : hResult; stdcall;
    GetIconLocation : function (const this : PIShellLink; pszIconPath : LPSTR; cchIconPath : INT; var iIcon : INT) : hResult; stdcall;
    SetIconLocation : function (const this : PIShellLink; pszIconPath : LPSTR; iIcon : INT) : hResult; stdcall;
    SetRelativePath : function (const this : PIShellLink; pszPathRel : LPSTR; wReserved : DWORD) : hResult; stdcall;
    Resolve : function (const this : PIShellLink; hwnd : HWND; fFlags : DWORD) : hResult; stdcall;
    SetPath : function (const this : PIShellLink; pszFile : LPSTR) : hResult; stdcall;
    end;
  { IPersistFile }
  PIPersistFile = ^IPersistFile;
  IPersistFile = packed record
    vtbl : ^IPersistFileVtbl;
    end;
  IPersistFileVtbl = packed record
    QueryInterface : function (const this : PIPersistFile; const iid: TIID; var obj): HResult; stdcall;
    AddRef : function (const this : PIPersistFile) : ULONG; stdcall;
    Release : function (const this : PIPersistFile) : ULONG; stdcall;
    GetClassID : function (const this : PIPersistFile; ClassID : TCLSID) : hResult; stdcall;
    IsDirty : function (const this : PIPersistFile) : hResult; stdcall;
    Load : function (const this : PIPersistFile; plszFilename : LPWSTR; dwMode : DWORD) : hResult; stdcall;
    Save : function (const this : PIPersistFile; plszFilename : LPWSTR; fRemember : BOOL) : hResult; stdcall;
    SaveCompleted : function (const this : PIPersistFile; plszFilename : LPWSTR) : hResult; stdcall;
    GetCurFile : function (const this : PIPersistFile; var plszFilename : LPWSTR) : hResult; stdcall;
    end;


{    IShellLinkDataList }
{ inplemented in shell32.dll version 4.71 or later }

{Data block structureDescription}
const
  EXP_DARWIN_ID_SIG        = $A0000006;   // The link's Microsoft© Windows© Installer identifier (ID).
  EXP_LOGO3_ID_SIG         = $A0000007;
  EXP_SPECIAL_FOLDER_SIG   = $A0000005;   // Special folder information.
  EXP_SZ_LINK_SIG          = $A0000001;   // The target name.
  EXP_SZ_ICON_SIG          = $A0000007;   // The icon name.
  NT_CONSOLE_PROPS_SIG     = $A0000002;   // Console properties.
  NT_FE_CONSOLE_PROPS_SIG  = $A0000004;   // The console's code page.

  SLDF_HAS_ID_LIST         = $00000001;   // The link has ID list.
  SLDF_HAS_LINK_INFO       = $00000002;   // The link has LinkInfo.
  SLDF_HAS_NAME            = $00000004;   // The link has a name.
  SLDF_HAS_RELPATH         = $00000008;   // The link has a relative path.
  SLDF_HAS_WORKINGDIR      = $00000010;   // The link has a working directory.
  SLDF_HAS_ARGS            = $00000020;   // The link has arguments.
  SLDF_HAS_ICONLOCATION    = $00000040;   // The link has an icon location.
  SLDF_UNICODE             = $00000080;   // The strings are unicode.
  SLDF_FORCE_NO_LINKINFO   = $00000100;   // Do not create link information. Distributed tracking will be disabled.
  SLDF_HAS_EXP_SZ          = $00000200;   // The link contains expandable environment strings.
  SLDF_RUN_IN_SEPARATE     = $00000400;   // Run the 16-bit target exe in a separate VDM/WOW.
  SLDF_HAS_LOGO3ID         = $00000800;   // The link is a special Logo3/MSICD link.
  SLDF_HAS_DARWINID        = $00001000;   // The link is a special Darwin link.
  SLDF_RUNAS_USER          = $00002000;   // Run the link as a different user.
  SLDF_HAS_EXP_ICON_SZ     = $00004000;   // The link contains expandable env string for icon path.
  SLDF_NO_PIDL_ALIAS       = $00008000;   // Don't ever resolve to a logical location.
  SLDF_FORCE_UNCNAME       = $00010000;   // Make GetPath() prefer the UNC name to the local name.
  SLDF_RUN_WITH_SHIMLAYER  = $00020000;   // Launch the target of this link w/ shim layer active.
  SLDF_RESERVED            = $80000000;   // Reserved-- so we can use the low word as an index value in the future

Type

  DATABLOCK_HEADER = packed record
    cbSize,
    dwSignature : DWORD;
  end;

  EXP_DARWIN_LINK = packed record
    dbh : DATABLOCK_HEADER;
    szDarwinID : array [0..MAX_PATH-1] of char;
    szwDarwinID : array [0..MAX_PATH-1] of word;
  end;

  EXP_SPECIAL_FOLDER = packed record
    dbh : DATABLOCK_HEADER;
    idSpecialFolder,
    cbOffset : DWORD;
  end;

  EXP_SZ_LINK = packed record
    dbh : DATABLOCK_HEADER;
    szTarget : array [0..MAX_PATH-1] of char;
    szwTarget : array [0..MAX_PATH-1] of word;
  end;

  NT_CONSOLE_PROPS = packed record
    dbh : DATABLOCK_HEADER;
    wFillAttribute : WORD;
    wPopupFillAttribute : WORD;
    dwScreenBufferSize : COORD;
    dwWindowSize : COORD;
    dwWindowOrigin : COORD;
    nFont : DWORD;
    nInputBufferSize : DWORD;
    dwFontSize : COORD;
    uFontFamily : UINT;
    uFontWeight : UINT;
    FaceName : array [0..LF_FACESIZE-1] of word;
    uCursorSize : UINT;
    bFullScreen : BOOL;
    bQuickEdit : BOOL;
    bInsertMode : BOOL;
    bAutoPosition : BOOL;
    uHistoryBufferSize : UINT;
    uNumberOfHistoryBuffers : UINT;
    bHistoryNoDup : BOOL;
    ColorTable : array [0..16-1] of COLORREF;
  end;

  NT_FE_CONSOLE_PROPS = packed record
    dbh : DATABLOCK_HEADER;
    uCodePage : UINT;
  end;





{    IShellLinkDataList }
  PIShellLinkDataList = ^IShellLinkDataList;
  IShellLinkDataList = packed record
    vtbl : ^IShellLinkDataListVtbl;
    end;
  IShellLinkDataListVtbl = packed record
    QueryInterface : function (const this : PIShellLinkDataList; const iid: TIID; var obj): HResult; stdcall;
    AddRef : function (const this : PIShellLinkDataList) : ULONG; stdcall;
    Release : function (const this : PIShellLinkDataList) : ULONG; stdcall;
// AddDataBlock Adds a data block to a link.
    AddDataBlock : function (const this : PIShellLinkDataList;PDataBlock : pointer) : ULONG; stdcall;
// CopyDataBlock Retrieves a copy of a link's data block.
    CopyDataBlock : function (const this : PIShellLinkDataList;dwSig : ULONG;var pDataBlock : pointer) : ULONG; stdcall;
// pDataBlock must be freed with LocalFree
// RemoveDataBlock Removes a data block from a link.
    RemoveDataBlock : function (const this : PIShellLinkDataList;dwSig : DWORD) : HResult; stdcall;
// GetFlags Retrieves the current option settings.
    GetFlags : function (const this : PIShellLinkDataList;var dwFlags : DWORD) : HResult; stdcall;
// SetFlags Specifies the current option settings.
    SetFlags : function (const this : PIShellLinkDataList;dwFlags : DWORD) : HResult; stdcall;
  end;


{ GetCurrentPlatform -- determines the version of Windows
  RETURNS
    a pfXXXX constant }
function GetCurrentPlatform : cardinal;

{ CreateShortcut -- creates a shortcut (.lnk) file with the specified parameters
  INPUT
    pszLinkFile = path of the shortcut (.lnk) file
    pszPathName = the path of the file the shortcut references to
    pszArgs = optional arguments for the referenced file
    pszWorkingDir = path to working directory
    pszDesc = shortcut description (menu entry in Start Menu)
    pszIconPath = path to a file containing an icon resource (.EXE, .DLL, .RES, .ICO)
    nIconIndex = zero based index number of the icon in the pszIconPath file
  RETURNS
    S_OK = shortcut succesfully created
    E_FAIL or anything else = creation failed }
function CreateShortcut (pszLinkFile, pszPathName, pszArgs, pszWorkingDir, pszDesc, pszIconPath : LPSTR; nIconIndex : INT) : hResult;

{ GetDesktopFolder -- returns the folder of the Desktop
  INPUT
    ForThisUser = on multi-user systems (NT/2000): TRUE queries the desktop of the current user, FALSE that of all users;
                  on other systems (95/98/ME): its value is not important
  OUTPUT
    szPath = the string the folder name will be assigned to, must be at least MAX_PATH long }
procedure GetDesktopFolder (ForThisUser : Boolean; szPath : LPSTR);

{ GetStartMenuFolder -- returns the folder of the Start Menu
  INPUT
    ForThisUser = on multi-user systems (NT/2000): TRUE queries the Start Menu of the current user, FALSE that of all users;
                  on other systems (95/98/ME): its value is not important
  OUTPUT
    szPath = the string the folder name will be assigned to, must be at least MAX_PATH long }
procedure GetStartMenuFolder (ForThisUser : Boolean; szPath : LPSTR);

function SHGetMalloc (ppMalloc : PPIMalloc) : hResult; external 'SHELL32' name 'SHGetMalloc';
function CoCreateInstance (rclsid : TCLSID; pUnkOuter : PIUnknown; dwClsContext : longint; riid : TIID; var ppv) : hResult; external 'OLE32' name 'CoCreateInstance';
function CoInitialize (pvReserved : pointer) : hResult; external 'OLE32' name 'CoInitialize';
procedure CoUninitialize; external 'OLE32' name 'CoUninitialize';

implementation
var
  CurrentPlatform : cardinal;

function GetCurrentPlatform : cardinal;
var
  VersionInfo : OSVERSIONINFO;
begin
  VersionInfo.dwOSVersionInfoSize := sizeof (OSVERSIONINFO);
  GetVersionEx (VersionInfo);
  case VersionInfo.dwPlatformId of
    VER_PLATFORM_WIN32s:
      GetCurrentPlatform := pfWin31;
    VER_PLATFORM_WIN32_WINDOWS:
      case VersionInfo.dwMinorVersion of
        0: GetCurrentPlatform := pfWin95;
        1: GetCurrentPlatform := pfWin98;
        else GetCurrentPlatform := pfWinME;
        end;
    VER_PLATFORM_WIN32_NT:
      case VersionInfo.dwMajorVersion of
        3: GetCurrentPlatform := pfWinNT3;
        4: GetCurrentPlatform := pfWinNT4;
        5: GetCurrentPlatform := pfWin2000;
        end;
    else GetCurrentPlatform := 0;
    end;
end; { GetCurrentPlatform }

function CreateShortcut (pszLinkFile, pszPathName, pszArgs, pszWorkingDir, pszDesc, pszIconPath : LPSTR; nIconIndex : INT) : hResult;
var
  hres : hResult;
  link : PIShellLink;
  f : PIPersistFile;
  DL : PIShellLinkDataList;
  lszPath : array [0..MAX_PATH] of WCHAR;
  ConsoleProps : NT_CONSOLE_PROPS;
  p : ^NT_CONSOLE_PROPS;
  CodePage : NT_FE_CONSOLE_PROPS;
  pfe :^NT_FE_CONSOLE_PROPS;
  flags : DWORD;
begin
  hres := E_FAIL;
  CoInitialize (nil);
  if CoCreateInstance (CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IID_IShellLink, link) = S_OK then
    begin
      link^.vtbl^.SetPath (link, pszPathName);
      if pszArgs <> nil then link^.vtbl^.SetArguments (link, pszArgs);
      link^.vtbl^.SetDescription (link, pszDesc);
      link^.vtbl^.SetIconLocation (link, pszIconPath, nIconIndex);
      link^.vtbl^.SetWorkingDirectory (link, pszWorkingDir);
      if link^.vtbl^.QueryInterface (link, IID_IPersistFile, f) = S_OK then
        begin
          MultiByteToWideChar (CP_ACP, 0, pszLinkFile, -1, lszPath, MAX_PATH);
          hres := f^.vtbl^.Save (f, lszPath, true);
          f^.vtbl^.Release (f);
        end;
      if link^.vtbl^.QueryInterface (link, IID_IShellLinkDataList, DL) = S_OK then
        begin
          flags:=-1;
          if DL^.vtbl^.GetFlags(DL,flags)=S_OK then
            begin
              writeln('Link flag is ',hexstr(flags,8));
              // flags:=flags or SLDF_RUNAS_USER;
              if DL^.vtbl^.SetFlags(DL,flags)=S_OK then
                Writeln('Flags changed');
              flags:=0;
              DL^.vtbl^.GetFlags(DL,flags);
              writeln('Link flag after is ',hexstr(flags,8));
            end;

          if DL^.vtbl^.CopyDataBlock(DL,NT_CONSOLE_PROPS_SIG,p)=S_OK then
            begin
              ConsoleProps:=p^;
              Writeln('Has NT_CONSOLE_PROPS');
              ConsoleProps.bQuickEdit:=false;
              ConsoleProps.bInsertMode:=false;
              ConsoleProps.bFullScreen:=true;
              LocalFree(longint(p));
            end
          else
            begin
              FillChar(ConsoleProps,sizeof(ConsoleProps),#0);
              ConsoleProps.dbh.cbSize:=sizeof(ConsoleProps);
              ConsoleProps.dbh.dwSignature:=NT_CONSOLE_PROPS_SIG;
              ConsoleProps.wFillAttribute := $07;
              ConsoleProps.wPopupFillAttribute := $5f;
              ConsoleProps.dwScreenBufferSize.X :=80;
              ConsoleProps.dwScreenBufferSize.Y :=500;
              ConsoleProps.dwWindowSize.X:= 70;
              ConsoleProps.dwWindowSize.Y:= 40;
              //ConsoleProps.dwWindowOrigin : COORD;
              //ConsoleProps.nFont : DWORD;
              ConsoleProps.nInputBufferSize:=100;
              ConsoleProps.dwFontSize.X := 8;
              ConsoleProps.dwFontSize.Y := 12;
              //ConsoleProps.uFontFamily : UINT;
              //ConsoleProps.uFontWeight : UINT;
              //ConsoleProps.FaceName : array [0..LF_FACESIZE-1] of word;
              //ConsoleProps.uCursorSize : UINT;
              ConsoleProps.bFullScreen := false;
              ConsoleProps.bQuickEdit := false;
              ConsoleProps.bInsertMode := false;
              ConsoleProps.bAutoPosition := false;
              ConsoleProps.uHistoryBufferSize :=350;
              ConsoleProps.uNumberOfHistoryBuffers := 5;
              ConsoleProps.bHistoryNoDup := true;
              ConsoleProps.ColorTable[0]:=RGB(0,0,0);
              ConsoleProps.ColorTable[1]:=RGB(0,0,128);
              ConsoleProps.ColorTable[2]:=RGB(0,128,0);
              ConsoleProps.ColorTable[3]:=RGB(0,128,128);
              ConsoleProps.ColorTable[4]:=RGB(128,0,0);
              ConsoleProps.ColorTable[5]:=RGB(128,0,128);
              ConsoleProps.ColorTable[6]:=RGB(128,128,0);
              ConsoleProps.ColorTable[7]:=RGB(192,192,192);
              ConsoleProps.ColorTable[8]:=RGB(128,128,128);
              ConsoleProps.ColorTable[9]:=RGB(0,0,255);
              ConsoleProps.ColorTable[10]:=RGB(0,255,0);
              ConsoleProps.ColorTable[11]:=RGB(0,255,255);
              ConsoleProps.ColorTable[12]:=RGB(255,0,0);
              ConsoleProps.ColorTable[13]:=RGB(255,0,255);
              ConsoleProps.ColorTable[14]:=RGB(255,255,0);
              ConsoleProps.ColorTable[15]:=RGB(255,255,255);
              //ConsoleProps.ColorTable : array [0..16-1] of COLORREF;
            end;
          if DL^.vtbl^.AddDataBlock(DL,@ConsoleProps)=S_OK then
            begin
              Writeln('Insert mode successfully changed');
            end;
          if DL^.vtbl^.CopyDataBlock(DL,NT_CONSOLE_PROPS_SIG,p)=S_OK then
            begin
              Writeln('bQuickEdit=',p^.bQuickEdit);
              Writeln('bInsertMode=',p^.bInsertMode);
              Writeln('bFullScreen=',p^.bFullScreen);
              LocalFree(longint(p));
            end;
          if DL^.vtbl^.CopyDataBlock(DL,NT_FE_CONSOLE_PROPS_SIG,pfe)=S_OK then
            begin
              Writeln('Console code page=',pfe^.uCodePage);
              LocalFree(longint(pfe));
            end
          else
            begin
              CodePage.dbh.cbSize:=sizeof(CodePage);
              CodePage.dbh.dwSignature:=NT_FE_CONSOLE_PROPS_SIG;
              CodePage.uCodePage:=437;
              DL^.vtbl^.AddDataBlock(DL,@CodePage);
              if DL^.vtbl^.CopyDataBlock(DL,NT_FE_CONSOLE_PROPS_SIG,pfe)=S_OK then
                begin
                  Writeln('Console code page after=',pfe^.uCodePage);
                  LocalFree(longint(pfe));
                end;
            end;
          DL^.vtbl^.Release (DL);
        end;
      if link^.vtbl^.QueryInterface (link, IID_IPersistFile, f) = S_OK then
        begin
          MultiByteToWideChar (CP_ACP, 0, pszLinkFile, -1, lszPath, MAX_PATH);
          hres := f^.vtbl^.Save (f, lszPath, true);
          f^.vtbl^.Release (f);
        end;
      link^.vtbl^.Release (link);
    end;
  CoUninitialize;
  CreateShortcut := hres;
end; { CreateShortcut }

(* The reason for using SHGetSpecialFolderLocation instead of SHGetSpecialFolderPath is that the second is only
available from the version 4.71 (Internet Explorer 4) of the Shell32.dll while the first is present on all systems
starting with NT 4 and Win 95. *)

procedure GetDesktopFolder (ForThisUser : Boolean; szPath : LPSTR);
var
  Memory : PIMalloc;
  pidl : LPITEMIDLIST;
begin
  Memory := nil;
  pidl := nil;
  if SHGetMalloc (@Memory) = NOERROR then
    begin
      if not ForThisUser and ((CurrentPlatform and pfWinNTx) > 0) then
        SHGetSpecialFolderLocation (0, CSIDL_COMMON_DESKTOPDIRECTORY, pidl)
      else
        SHGetSpecialFolderLocation (0, CSIDL_DESKTOPDIRECTORY, pidl);
      SHGetPathFromIDList (pidl, szPath);
    end;
  if (pidl <> nil) and (Memory <> nil) then Memory^.vtbl^.Free (Memory, pidl);
  if (Memory <> nil) then Memory^.vtbl^.Release (Memory);
end; { GetDesktopFolder }

procedure GetStartMenuFolder (ForThisUser : Boolean; szPath : LPSTR);
var
  Memory : PIMalloc;
  pidl : LPITEMIDLIST;
begin
  Memory := nil;
  pidl := nil;
  if SHGetMalloc (@Memory) = NOERROR then
    begin
      if not ForThisUser and ((CurrentPlatform and pfWinNTx) > 0) then
        SHGetSpecialFolderLocation (0, CSIDL_COMMON_PROGRAMS, pidl)
      else
        SHGetSpecialFolderLocation (0, CSIDL_PROGRAMS, pidl);
      SHGetPathFromIDList (pidl, szPath);
    end;
  if (pidl <> nil) and (Memory <> nil) then Memory^.vtbl^.Free (Memory, pidl);
  if (Memory <> nil) then Memory^.vtbl^.Release (Memory);
end; { GetStartMenuFolder }

begin
  CurrentPlatform := GetCurrentPlatform;
end.
