{ ---------------------------------------------------------------------
  shfolder.dll is distributed standard with IE5.5, so it should ship 
  with 2000/XP or higher but is likely to be installed on NT/95/98 or 
  ME as well.  It works on all these systems.
  
  The info found here is also in the registry:
  HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders\ 
  HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders\ 
  
  Missing from shfolder.dll are:
    Desktop
    Favorites
    NetHood
    Printhood
    Recent
    Sendto
    Startup
    Start menu
    Templates
  These are present in the registry, though.
  ---------------------------------------------------------------------}

Unit shfolder;

Interface

Uses
  windows;

Const
  LibName = 'SHFolder.dll';

Const
  CSIDL_PERSONAL             = $0005; { %USERPROFILE%\My Documents }
  CSIDL_APPDATA              = $001A; { %USERPROFILE%\Application Data (roaming) }
  CSIDL_LOCAL_APPDATA        = $001C; { %USERPROFILE%\Local Settings\Application Data (non roaming) }
  CSIDL_INTERNET_CACHE       = $0020; { %USERPROFILE%\Local Settings\Temporary Internet Files}
  CSIDL_COOKIES              = $0021; { %USERPROFILE%\Cookies}
  CSIDL_HISTORY              = $0022; { %USERPROFILE%\Local settings\History}
  CSIDL_COMMON_APPDATA       = $0023; { %PROFILESPATH%\All Users\Application Data }
  CSIDL_WINDOWS              = $0024; { %SYSTEMROOT% }
  CSIDL_SYSTEM               = $0025; { %SYSTEMROOT%\SYSTEM32 (may be system on 95/98/ME) }
  CSIDL_PROGRAM_FILES        = $0026; { %SYSTEMDRIVE%\Program Files }
  CSIDL_MYPICTURES           = $0027; { %USERPROFILE%\My Documents\My Pictures }
  CSIDL_PROGRAM_FILES_COMMON = $002b; { %SYSTEMDRIVE%\Program Files\Common }
  CSIDL_COMMON_DOCUMENTS     = $002e; { %PROFILEPATH%\All Users\Documents }
  CSIDL_COMMON_ADMINTOOLS    = $002f; { %PROFILEPATH%\All Users\Start Menu\Programs\Administrative Tools }
  CSIDL_ADMINTOOLS           = $0030; { %USERPROFILEPATH%\<user name>\Start Menu\Programs\Administrative Tools }

  CSIDL_FLAG_CREATE          = $8000; { (force creation of requested folder if it doesn't exist yet) }

{ Original entry points }

Function SHGetFolderPathA(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall; external LibName name 'SHGetFolderPathA';
Function SHGetFolderPathW(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall; external LibName name 'SHGetFolderPathW';

Function SHGetFolderPath (Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall; external LibName name 'SHGetFolderPathA';


{ For Delphi compatibility }

type
  PFNSHGetFolderPathA = Function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall;
  PFNSHGetFolderPathW = Function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall;
  PFNSHGetFolderPath = PFNSHGetFolderPathA;

  TSHGetFolderPathA = PFNSHGetFolderPathA;
  TSHGetFolderPathW = PFNSHGetFolderPathW;
  TSHGetFolderPath = TSHGetFolderPathA;

implementation

const
  SHFolderDll = 'SHFolder.dll';

end.
