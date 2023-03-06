{$IFNDEF FPC_DOTTEDUNITS}
unit shlwapi;
{$ENDIF FPC_DOTTEDUNITS}

{
    This file is part of the Free Pascal run time library.
    partial shlwapi.h header translation
    Copyright (c) 1999-2019 by Marco van de Voort,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ************************************************************************}

interface
{$mode delphi}

{$IFDEF FPC_DOTTEDUNITS}
Uses WinApi.Windows,WinApi.Activex;
{$ELSE FPC_DOTTEDUNITS}
Uses Windows,ActiveX;
{$ENDIF FPC_DOTTEDUNITS}
  const
    SHLWAPIDLL='shlwapi.dll'; {Setup as you need}

Type
  SFBS_FLAGS = longint;
  STIF_FLAGS = longint;
  SRRF       = integer;
  LSTATUS    = LONG;
  HUSKEY     = THANDLE;
  PHUSKEY    = PHANDLE;
  SHCT_FLAGS = DWORD;   // SHCreateThread flags values
  ASSOCF     = DWORD;
  PHWND      = PHANDLE;
  PPIDLIST_RELATIVE  = PITEMIDLIST;
  PCUIDLIST_RELATIVE = PITEMIDLIST;
  PCIDLIST_ABSOLUTE  = PITEMIDLIST;

  PPERCEIVEDFLAG     = ^PERCEIVEDFLAG;
  PERCEIVEDFLAG      = DWORD;

  PSFBS_FLAGS  = ^SFBS_FLAGS;
  PSTIF_FLAGS  = ^STIF_FLAGS;
  PtagSFBS_FLAGS  = ^tagSFBS_FLAGS;
    tagSFBS_FLAGS = (SFBS_FLAGS_ROUND_TO_NEAREST_DISPLAYED_DIGIT = $0001,
      SFBS_FLAGS_TRUNCATE_UNDISPLAYED_DECIMAL_DIGITS = $0002
      );
  PCUITEMID_CHILD= PITEMIDLIST;
  PZPCSTR = PLPSTR;   // PLPCSTR
  PZPCWSTR = PLPWSTR; // PLPCWSTR

const
  STIF_DEFAULT     = $00000000;
  STIF_SUPPORT_HEX = $00000001;

Type
  PURL_SCHEME = ^URL_SCHEME;
  URL_SCHEME = (
      URL_SCHEME_INVALID = -(1),
      URL_SCHEME_UNKNOWN = 0,
      URL_SCHEME_FTP,
      URL_SCHEME_HTTP,
      URL_SCHEME_GOPHER,
      URL_SCHEME_MAILTO,
      URL_SCHEME_NEWS,
      URL_SCHEME_NNTP,
      URL_SCHEME_TELNET,
      URL_SCHEME_WAIS,
      URL_SCHEME_FILE,
      URL_SCHEME_MK,
      URL_SCHEME_HTTPS,
      URL_SCHEME_SHELL,
      URL_SCHEME_SNEWS,
      URL_SCHEME_LOCAL,
      URL_SCHEME_JAVASCRIPT,
      URL_SCHEME_VBSCRIPT,
      URL_SCHEME_ABOUT,
      URL_SCHEME_RES,
      URL_SCHEME_MSSHELLROOTED,
      URL_SCHEME_MSSHELLIDLIST,
      URL_SCHEME_MSHELP,
      URL_SCHEME_MSSHELLDEVICE,
      URL_SCHEME_WILDCARD,
      URL_SCHEME_SEARCH_MS,
      URL_SCHEME_SEARCH,
      URL_SCHEME_KNOWNFOLDER,
      URL_SCHEME_MAXVALUE
      );

  PURL_PART = ^URL_PART;
  URL_PART = (
      URL_PART_NONE = 0,
      URL_PART_SCHEME = 1,
      URL_PART_HOSTNAME,
      URL_PART_USERNAME,
      URL_PART_PASSWORD,
      URL_PART_PORT,
      URL_PART_QUERY);

  PURLIS = ^URLIS;
  URLIS = (
      URLIS_URL =0,
      URLIS_OPAQUE,
      URLIS_NOHISTORY,
      URLIS_FILEURL,
      URLIS_APPLIABLE,
      URLIS_DIRECTORY,
      URLIS_HASQUERY);


  SHREGDEL_FLAGS = (
    SHREGDEL_DEFAULT = $00000000,       // Delete's HKCU, or HKLM if HKCU is not found.
    SHREGDEL_HKCU    = $00000001,       // Delete HKCU only
    SHREGDEL_HKLM    = $00000010,       // Delete HKLM only.
    SHREGDEL_BOTH    = $00000011        // Delete both HKCU and HKLM.
   );


  SHREGENUM_FLAGS = (
    SHREGENUM_DEFAULT = $00000000,       // Enumerates HKCU or HKLM if not found.
    SHREGENUM_HKCU    = $00000001,       // Enumerates HKCU only
    SHREGENUM_HKLM    = $00000010,       // Enumerates HKLM only.
    SHREGENUM_BOTH    = $00000011        // Enumerates both HKCU and HKLM without duplicates.
   );                                       // This option is NYI.
  ASSOCSTR = (
      ASSOCSTR_COMMAND      = 1,  //  shell\verb\command string
      ASSOCSTR_EXECUTABLE,        //  the executable part of command string
      ASSOCSTR_FRIENDLYDOCNAME,   //  friendly name of the document type
      ASSOCSTR_FRIENDLYAPPNAME,   //  friendly name of executable
      ASSOCSTR_NOOPEN,            //  noopen value
      ASSOCSTR_SHELLNEWVALUE,     //  query values under the shellnew key
      ASSOCSTR_DDECOMMAND,        //  template for DDE commands
      ASSOCSTR_DDEIFEXEC,         //  DDECOMMAND to use if just create a process
      ASSOCSTR_DDEAPPLICATION,    //  Application name in DDE broadcast
      ASSOCSTR_DDETOPIC,          //  Topic Name in DDE broadcast
      ASSOCSTR_INFOTIP,           //  info tip for an item, or list of properties to create info tip from
  //#if (_WIN32_IE >= _WIN32_IE_IE60)
      ASSOCSTR_QUICKTIP,          //  same as ASSOCSTR_INFOTIP, except, this list contains only quickly retrievable properties
      ASSOCSTR_TILEINFO,          //  similar to ASSOCSTR_INFOTIP - lists important properties for tileview
      ASSOCSTR_CONTENTTYPE,       //  MIME Content type
      ASSOCSTR_DEFAULTICON,       //  Default icon source
      ASSOCSTR_SHELLEXTENSION,    //  Guid string pointing to the Shellex\Shellextensionhandler value.
  //#endif // _WIN32_IE_IE60

      ASSOCSTR_DROPTARGET,        //  The CLSID of DropTarget   IE8+
      ASSOCSTR_DELEGATEEXECUTE,   //  The CLSID of DelegateExecute IE8+

      // a string value of the uri protocol schemes, for example "http:https:ftp:file:" or "*" indicating all
      ASSOCSTR_SUPPORTED_URI_PROTOCOLS,
  //#if (NTDDI_VERSION >= NTDDI_WIN10)
      ASSOCSTR_PROGID,            // The ProgId provided by the app associated with the file type or uri scheme based on user default settings.
      ASSOCSTR_APPID,             // The AppUserModelID of the app associated with the file type or uri scheme based on user default settings.
      ASSOCSTR_APPPUBLISHER,      // THe publisher of the app associated with the file type or uri scheme based on user default settings.
      ASSOCSTR_APPICONREFERENCE,  // The icon reference of the app associated with the file type or uri scheme based on user default settings.
  //#endif // NTDDI_WIN10
      ASSOCSTR_MAX                //  last item in enum...
  );


  ASSOCKEY = (
      ASSOCKEY_SHELLEXECCLASS = 1,  //  the key that should be passed to ShellExec(hkeyClass)
      ASSOCKEY_APP,                 //  the "Application" key for the association
      ASSOCKEY_CLASS,               //  the progid or class key
      ASSOCKEY_BASECLASS,           //  the BaseClass key
      ASSOCKEY_MAX                  //  last item in enum...
   );

  ASSOCDATA = (
      ASSOCDATA_MSIDESCRIPTOR = 1,  //  Component Descriptor to pass to MSI APIs
      ASSOCDATA_NOACTIVATEHANDLER,  //  restrict attempts to activate window
      ASSOCDATA_UNUSED1,            //  removed QUERYCLASSSTORE, dead code
      ASSOCDATA_HASPERUSERASSOC,    //  defaults to user specified association
      ASSOCDATA_EDITFLAGS,          //  Edit flags. IE6+
      ASSOCDATA_VALUE,              //  use pszExtra as the Value name IE6+
      ASSOCDATA_MAX
  );

  ASSOCENUM = (
      ASSOCENUM_NONE = 1
  );


 SHGLOBALCOUNTER = (
    GLOBALCOUNTER_SEARCHMANAGER =0,
    GLOBALCOUNTER_SEARCHOPTIONS,
    GLOBALCOUNTER_FOLDERSETTINGSCHANGE,
    GLOBALCOUNTER_RATINGS,
    GLOBALCOUNTER_APPROVEDSITES,
    GLOBALCOUNTER_RESTRICTIONS,
    GLOBALCOUNTER_SHELLSETTINGSCHANGED,
    GLOBALCOUNTER_SYSTEMPIDLCHANGE,
    GLOBALCOUNTER_OVERLAYMANAGER,
    GLOBALCOUNTER_QUERYASSOCIATIONS,
    GLOBALCOUNTER_IESESSIONS,
    GLOBALCOUNTER_IEONLY_SESSIONS,
    GLOBALCOUNTER_APPLICATION_DESTINATIONS,
    __UNUSED_RECYCLE_WAS_GLOBALCOUNTER_CSCSYNCINPROGRESS,
    GLOBALCOUNTER_BITBUCKETNUMDELETERS,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_SHARES,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_A,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_B,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_C,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_D,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_E,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_F,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_G,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_H,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_I,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_J,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_K,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_L,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_M,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_N,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_O,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_P,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_Q,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_R,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_S,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_T,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_U,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_V,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_W,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_X,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_Y,
    GLOBALCOUNTER_RECYCLEDIRTYCOUNT_DRIVE_Z,
    __UNUSED_RECYCLE_WAS_GLOBALCOUNTER_RECYCLEDIRTYCOUNT_SERVERDRIVE,
    __UNUSED_RECYCLE_WAS_GLOBALCOUNTER_RECYCLEGLOBALDIRTYCOUNT,
    GLOBALCOUNTER_RECYCLEBINENUM,
    GLOBALCOUNTER_RECYCLEBINCORRUPTED,
    GLOBALCOUNTER_RATINGS_STATECOUNTER,
    GLOBALCOUNTER_PRIVATE_PROFILE_CACHE,
    GLOBALCOUNTER_INTERNETTOOLBAR_LAYOUT,
    GLOBALCOUNTER_FOLDERDEFINITION_CACHE,
    GLOBALCOUNTER_COMMONPLACES_LIST_CACHE,
    GLOBALCOUNTER_PRIVATE_PROFILE_CACHE_MACHINEWIDE,
    GLOBALCOUNTER_ASSOCCHANGED,  // throttles reading of the registry value "GlobalAssocChangedCounter" from HKLM\Software\Microsoft\Windows\CurrentVersion\Explorer
//#if (NTDDI_VERSION >= NTDDI_WIN8)
    GLOBALCOUNTER_APP_ITEMS_STATE_STORE_CACHE,
    GLOBALCOUNTER_SETTINGSYNC_ENABLED,
    GLOBALCOUNTER_APPSFOLDER_FILETYPEASSOCIATION_COUNTER,
    GLOBALCOUNTER_USERINFOCHANGED,
    GLOBALCOUNTER_SYNC_ENGINE_INFORMATION_CACHE_MACHINEWIDE, // WINBLUE+
//#endif // (NTDDI_VERSION >= NTDDI_WIN8)
    GLOBALCOUNTER_BANNERS_DATAMODEL_CACHE_MACHINEWIDE, // WIN10_RS1+

    GLOBALCOUNTER_MAXIMUMVALUE // should always be last value
  );


  // Stored under HKCR\<progId> EditFlags(REG_DWORD)
  //
  // Retrieve these values using IQueryAssociations::GetData as follows
  //
  // DWORD editFlags, size = sizeof(editFlags);
  // queryAssoc->GetData(nullptr, ASSOCDATA_EDITFLAGS, nullptr, &editFlags, &size);
  //
  // Some of these flags are no longer used since editing file type associations has been
  // removed from Explorer's folder options UI.


  FILETYPEATTRIBUTEFLAGS = (
      FTA_None                    = $00000000,
      FTA_Exclude                 = $00000001, // used to exclude (hide) types like drvfile
      FTA_Show                    = $00000002, // used to show types like folder that don't have associations
      FTA_HasExtension            = $00000004, // type has a file name extension
      FTA_NoEdit                  = $00000008, // no editing of file type
      FTA_NoRemove                = $00000010, // no removing of the file type
      FTA_NoNewVerb               = $00000020, // no adding of verbs
      FTA_NoEditVerb              = $00000040, // no editing of predefined verbs
      FTA_NoRemoveVerb            = $00000080, // no removing of predefined verbs
      FTA_NoEditDesc              = $00000100, // no editing of file type description
      FTA_NoEditIcon              = $00000200, // no editing of doc icon
      FTA_NoEditDflt              = $00000400, // no changing of default verb
      FTA_NoEditVerbCmd           = $00000800, // no modification of the commnds associated with the verbs
      FTA_NoEditVerbExe           = $00001000, // no editing of the verb's exe
      FTA_NoDDE                   = $00002000, // no editing of the DDE fields

      FTA_NoEditMIME              = $00008000, // no editing of the Content Type or Default Extension fields
      FTA_OpenIsSafe              = $00010000, // the open verb should be invoked automaticaly for downloaded files
      FTA_AlwaysUnsafe            = $00020000, // don't allow the "Never ask me" checkbox to be enabled; File Type dialog still allows user to turn this off

      FTA_NoRecentDocs            = $00100000, // don't add this file type to the Recent Documents folder
      FTA_SafeForElevation        = $00200000, // Win8: can be launched in medium IL by a process running in AppContainer
      FTA_AlwaysUseDirectInvoke   = $00400000 // Win8: when downloading use the direct invoke feature even if the server headers are not provided
   );
  PPERCEIVED  = ^PERCEIVED;
  PERCEIVED = (
       PERCEIVED_TYPE_CUSTOM    = -3,
       PERCEIVED_TYPE_UNSPECIFIED       = -2,
       PERCEIVED_TYPE_FOLDER    = -1,
       PERCEIVED_TYPE_UNKNOWN   = 0,
       PERCEIVED_TYPE_TEXT      = 1,
       PERCEIVED_TYPE_IMAGE     = 2,
       PERCEIVED_TYPE_AUDIO     = 3,
       PERCEIVED_TYPE_VIDEO     = 4,
       PERCEIVED_TYPE_COMPRESSED        = 5,
       PERCEIVED_TYPE_DOCUMENT  = 6,
       PERCEIVED_TYPE_SYSTEM    = 7,
       PERCEIVED_TYPE_APPLICATION       = 8,
       PERCEIVED_TYPE_GAMEMEDIA = 9,
       PERCEIVED_TYPE_CONTACTS  = 10
       );

const
      PERCEIVED_TYPE_LAST      = PERCEIVED_TYPE_CONTACTS;
      PERCEIVED_TYPE_FIRST     = PERCEIVED_TYPE_CUSTOM;

type



  PtagPARSEDURLA = ^tagPARSEDURLA;
    tagPARSEDURLA = record
        cbSize : DWORD;
        pszProtocol : LPCSTR;
        cchProtocol : UINT;
        pszSuffix : LPCSTR;
        cchSuffix : UINT;
        nScheme : UINT;
      end;
    PARSEDURLA = tagPARSEDURLA;
    PPARSEDURLA = ^PARSEDURLA;
    PPPARSEDURLA = ^PPARSEDURLA;
    PtagPARSEDURLW = ^tagPARSEDURLW;
    tagPARSEDURLW = record
        cbSize : DWORD;
        pszProtocol : LPCWSTR;
        cchProtocol : UINT;
        pszSuffix : LPCWSTR;
        cchSuffix : UINT;
        nScheme : UINT;
      end;
    PARSEDURLW = tagPARSEDURLW;
    PPARSEDURLW = ^PARSEDURLW;
    PPPARSEDURLW = ^PPARSEDURLW;
{$ifdef UNICODE}
    PARSEDURL = PARSEDURLW;
    PPPARSEDURL = ^PPARSEDURL;
    PPARSEDURL = PPARSEDURLW;
{$else}
    PARSEDURL = PARSEDURLA;
    PPPARSEDURL = ^PPARSEDURL;
    PPARSEDURL = PPARSEDURLA;
{$endif}

const

    URL_UNESCAPE               = $10000000;
    URL_ESCAPE_UNSAFE          = $20000000;
    URL_PLUGGABLE_PROTOCOL     = $40000000;
    URL_WININET_COMPATIBILITY  = $80000000;
    URL_DONT_ESCAPE_EXTRA_INFO = $02000000;
    URL_DONT_UNESCAPE_EXTRA_INFO = URL_DONT_ESCAPE_EXTRA_INFO;
    URL_BROWSER_MODE           = URL_DONT_ESCAPE_EXTRA_INFO;
    URL_ESCAPE_SPACES_ONLY     = $04000000;
    URL_DONT_SIMPLIFY          = $08000000;
    URL_NO_META = URL_DONT_SIMPLIFY;
    URL_UNESCAPE_INPLACE       = $00100000;
    URL_CONVERT_IF_DOSPATH     = $00200000;
    URL_UNESCAPE_HIGH_ANSI_ONLY= $00400000;
    URL_INTERNAL_PATH          = $00800000;    { Will escape #'s in paths }
    URL_FILE_USE_PATHURL       = $00010000;
    URL_DONT_UNESCAPE          = $00020000;    { Do not unescape the path/url at all IE6.0SP2++ }
    URL_ESCAPE_AS_UTF8         = $00040000;    { Percent-encode all non-ASCII characters as their UTF-8 equivalents. Win7+ }
  {#if (NTDDI_VERSION >= NTDDI_WIN8) }
    URL_UNESCAPE_AS_UTF8       = URL_ESCAPE_AS_UTF8;
    URL_ESCAPE_ASCII_URI_COMPONENT = $00080000;    { Percent-encode all ASCII characters outside of the unreserved set from URI RFC 3986 (a-zA-Z0-9-.~_) (i.e.) No need for URL_ESCAPE_PERCENT along with this. }
    URL_ESCAPE_URI_COMPONENT   = URL_ESCAPE_ASCII_URI_COMPONENT or URL_ESCAPE_AS_UTF8;
    URL_UNESCAPE_URI_COMPONENT = URL_UNESCAPE_AS_UTF8;
  {#endif // (NTDDI_VERSION >= NTDDI_WIN8) }
    URL_ESCAPE_PERCENT         = $00001000;
    URL_ESCAPE_SEGMENT_ONLY    = $00002000;    { Treat the entire URL param as one URL segment. }
    URL_PARTFLAG_KEEPSCHEME    = $00000001;
    URL_APPLY_DEFAULT          = $00000001;
    URL_APPLY_GUESSSCHEME      = $00000002;
    URL_APPLY_GUESSFILE        = $00000004;
    URL_APPLY_FORCEAPPLY       = $00000008;
    SRRF_RT_REG_NONE           = $00000001;  // restrict type to REG_NONE      (other data types will not return ERROR_SUCCESS)
    SRRF_RT_REG_SZ             = $00000002;  // restrict type to REG_SZ        (other data types will not return ERROR_SUCCESS) (automatically converts REG_EXPAND_SZ to REG_SZ unless SRRF_NOEXPAND is specified)
    SRRF_RT_REG_EXPAND_SZ      = $00000004;  // restrict type to REG_EXPAND_SZ (other data types will not return ERROR_SUCCESS) (must specify SRRF_NOEXPAND or SHRegGetValue will fail with ERROR_INVALID_PARAMETER)
    SRRF_RT_REG_BINARY         = $00000008;  // restrict type to REG_BINARY    (other data types will not return ERROR_SUCCESS)
    SRRF_RT_REG_DWORD          = $00000010;  // restrict type to REG_DWORD     (other data types will not return ERROR_SUCCESS)
    SRRF_RT_REG_MULTI_SZ       = $00000020;  // restrict type to REG_MULTI_SZ  (other data types will not return ERROR_SUCCESS)
    SRRF_RT_REG_QWORD          = $00000040;  // restrict type to REG_QWORD     (other data types will not return ERROR_SUCCESS)

    SRRF_RT_DWORD              = (SRRF_RT_REG_BINARY or SRRF_RT_REG_DWORD); // restrict type to *32-bit* SRRF_RT_REG_BINARY or SRRF_RT_REG_DWORD (other data types will not return ERROR_SUCCESS)
    SRRF_RT_QWORD              = (SRRF_RT_REG_BINARY or SRRF_RT_REG_QWORD); // restrict type to *64-bit* SRRF_RT_REG_BINARY or SRRF_RT_REG_DWORD (other data types will not return ERROR_SUCCESS)
    SRRF_RT_ANY                = $0000ffff;                               // no type restriction

    SRRF_RM_ANY                = $00000000;  // no mode restriction (default is to allow any mode)
    SRRF_RM_NORMAL             = $00010000;  // restrict system startup mode to "normal boot"               (other startup modes will not return ERROR_SUCCESS)
    SRRF_RM_SAFE               = $00020000;  // restrict system startup mode to "safe mode"                 (other startup modes will not return ERROR_SUCCESS)
    SRRF_RM_SAFENETWORK        = $00040000;  // restrict system startup mode to "safe mode with networking" (other startup modes will not return ERROR_SUCCESS)

    SRRF_NOEXPAND              = $10000000;  // do not automatically expand environment strings if value is of type REG_EXPAND_SZ
    SRRF_ZEROONFAILURE         = $20000000;  // if pvData is not NULL, set content to all zeros on failure
    SRRF_NOVIRT                = $40000000;  // if the requested key is virtualized, then fail with ERROR_FILE_NOT_FOUND


    SHREGSET_HKCU              = $00000001;       // Write to HKCU if empty.
    SHREGSET_FORCE_HKCU        = $00000002;       // Write to HKCU.
    SHREGSET_HKLM              = $00000004;       // Write to HKLM if empty.
    SHREGSET_FORCE_HKLM        = $00000008;       // Write to HKLM.
    SHREGSET_DEFAULT           = (SHREGSET_FORCE_HKCU or SHREGSET_HKLM);          // Default is SHREGSET_FORCE_HKCU | SHREGSET_HKLM.

    //
    //  Association APIs
    //
    //  these APIs are to assist in accessing the data in HKCR
    //  getting the Command strings and exe paths
    //  for different verbs and extensions are simplified this way
    //


    ASSOCF_NONE                = $00000000;
    ASSOCF_INIT_NOREMAPCLSID   = $00000001;  //  do not remap clsids to progids
    ASSOCF_INIT_BYEXENAME      = $00000002;  //  executable is being passed in
    ASSOCF_OPEN_BYEXENAME      = $00000002;  //  executable is being passed in
    ASSOCF_INIT_DEFAULTTOSTAR  = $00000004;  //  treat "*" as the BaseClass
    ASSOCF_INIT_DEFAULTTOFOLDER= $00000008;  //  treat "Folder" as the BaseClass
    ASSOCF_NOUSERSETTINGS      = $00000010;  //  dont use HKCU
    ASSOCF_NOTRUNCATE          = $00000020;  //  dont truncate the return string
    ASSOCF_VERIFY              = $00000040;  //  verify data is accurate (DISK HITS)
    ASSOCF_REMAPRUNDLL         = $00000080;  //  actually gets info about rundlls target if applicable
    ASSOCF_NOFIXUPS            = $00000100;  //  attempt to fix errors if found
    ASSOCF_IGNOREBASECLASS     = $00000200;  //  dont recurse into the baseclass
    ASSOCF_INIT_IGNOREUNKNOWN  = $00000400;  //  dont use the "Unknown" progid, instead fail
    //#if (NTDDI_VERSION >= NTDDI_WIN8)
    ASSOCF_INIT_FIXED_PROGID   = $00000800;  //  the Init() pszAssoc value is a ProgId that should not be mapped using the current user defaults
    ASSOCF_IS_PROTOCOL         = $00001000;  //  the Init() pszAssoc value is an uri scheme (not including the ":") that should be mapped using the current user defaults
    ASSOCF_INIT_FOR_FILE       = $00002000;  //  use this flag when specifying ASSOCF_INIT_FIXED_PROGID if the ProgId corresponds with a file extension based association
    //#endif
    //#if (NTDDI_VERSION >= NTDDI_WIN10_RS1)
    ASSOCF_IS_FULL_URI         = $00004000;  //  Used to specify that full http/https URI is being passed for target resolution
                                             //  Only one of ASSOCF_INIT_FIXED_PROGID, ASSOCF_IS_PROTOCOL or ASSOCF_IS_FULL_URI can be specified at a time.
    ASSOCF_PER_MACHINE_ONLY    = $00008000;  //  Enforces per-machine association look-up only and avoid HKCU.
    //#endif

    ASSOCF_APP_TO_APP          = $00010000;  // #if NTDDI_WIN10_RS4+



    CTF_INSIST                 = $00000001;   // call pfnThreadProc synchronously if CreateThread() fails
    CTF_THREAD_REF             = $00000002;   // hold a reference to the creating thread
    CTF_PROCESS_REF            = $00000004;   // hold a reference to the creating process
    CTF_COINIT_STA             = $00000008;   // init COM as STA for the created thread
    CTF_COINIT                 = $00000008;   // init COM as STA for the created thread
    CTF_FREELIBANDEXIT         = $00000010;   // hold a ref to the DLL and call FreeLibraryAndExitThread() when done IE6+
    CTF_REF_COUNTED            = $00000020;   // thread supports ref counting via SHGetThreadRef() or CTF_THREAD_REF so that child threads can keep this thread alive IE6+
    CTF_WAIT_ALLOWCOM          = $00000040;   // while waiting for pfnCallback, allow COM marshaling to the blocked calling thread IE6+

    CTF_UNUSED                 = $00000080;   // IE7+
    CTF_INHERITWOW64           = $00000100;   // new thread should inherit the wow64 disable state for the file system redirector IE7+


    CTF_WAIT_NO_REENTRANCY  = $00000200;   // don't allow re-entrancy when waiting for the sync proc, this won't work with marshalled objects or SendMessages() from the sync proc Vista+

    //#if (NTDDI_VERSION >= NTDDI_WIN7)
    CTF_KEYBOARD_LOCALE        = $00000400;   // carry the keyboard locale from creating to created thread
    CTF_OLEINITIALIZE          = $00000800;   // init OLE on the created thread (this will also init COM as STA)
    CTF_COINIT_MTA             = $00001000;   // init COM as MTA for the created thread
    CTF_NOADDREFLIB            = $00002000;   // this flag is the opposite of CTF_FREELIBANDEXIT that is now implicit as of Vista
                                                    // this avoids the LoadLibrary/FreeLibraryAndExitThread calls that result in contention for the loader lock
                                                    // only use this when the thread being created has some other means to ensure that the code
                                                    // of the thread proc will remain loaded. This should not be used in the context of COM objects as those
                                                    // need to ensure that the DLL stays loaded as COM will unload DLLs
    //#endif // (NTDDI_VERSION >= NTDDI_WIN7)


     OS_WINDOWS                  = 0;           // Windows 9x vs. NT
     OS_NT                       = 1;           // Windows 9x vs. NT
     OS_WIN95ORGREATER           = 2;           // Win95 or greater
     OS_NT4ORGREATER             = 3;           // NT4 or greater
     OS_WIN98ORGREATER           = 5;           // Win98 or greater
     OS_WIN98_GOLD               = 6;           // Win98 Gold (Version 4.10 build 1998)
     OS_WIN2000ORGREATER         = 7;           // Some derivative of Win2000

// NOTE: these flags check explicitly for (dwMajorVersion == 5)
     OS_WIN2000PRO               = 8;           // Windows 2000 Professional (Workstation)
     OS_WIN2000SERVER            = 9;           // Windows 2000 Server
     OS_WIN2000ADVSERVER         = 10;          // Windows 2000 Advanced Server
     OS_WIN2000DATACENTER        = 11;          // Windows 2000 Data Center Server
     OS_WIN2000TERMINAL          = 12;          // Windows 2000 Terminal Server in "Application Server" mode (now simply called "Terminal Server")

     OS_EMBEDDED                 = 13;          // Embedded Windows Edition
     OS_TERMINALCLIENT           = 14;          // Windows Terminal Client (eg user is comming in via tsclient)
     OS_TERMINALREMOTEADMIN      = 15;          // Terminal Server in "Remote Administration" mode
     OS_WIN95_GOLD               = 16;          // Windows 95 Gold (Version 4.0 Build 1995)
     OS_MEORGREATER              = 17;          // Windows Millennium (Version 5.0)
     OS_XPORGREATER              = 18;          // Windows XP or greater
     OS_HOME                     = 19;          // Home Edition (eg NOT Professional, Server, Advanced Server, or Datacenter)
     OS_PROFESSIONAL             = 20;          // Professional     (aka Workstation; eg NOT Server, Advanced Server, or Datacenter)
     OS_DATACENTER               = 21;          // Datacenter       (eg NOT Server, Advanced Server, Professional, or Personal)
     OS_ADVSERVER                = 22;          // Advanced Server  (eg NOT Datacenter, Server, Professional, or Personal)
     OS_SERVER                   = 23;          // Server           (eg NOT Datacenter, Advanced Server, Professional, or Personal)
     OS_TERMINALSERVER           = 24;          // Terminal Server - server running in what used to be called "Application Server" mode (now simply called "Terminal Server")
     OS_PERSONALTERMINALSERVER   = 25;          // Personal Terminal Server - per/pro machine running in single user TS mode
     OS_FASTUSERSWITCHING        = 26;          // Fast User Switching
     OS_WELCOMELOGONUI           = 27;          // New friendly logon UI
     OS_DOMAINMEMBER             = 28;          // Is this machine a member of a domain (eg NOT a workgroup)
     OS_ANYSERVER                = 29;          // is this machine any type of server? (eg datacenter or advanced server or server)?
     OS_WOW6432                  = 30;          // Is this process a 32-bit process running on an 64-bit platform?
     OS_WEBSERVER                = 31;          // Web Edition Server
     OS_SMALLBUSINESSSERVER      = 32;          // SBS Server
     OS_TABLETPC                 = 33;          // Are we running on a TabletPC?
     OS_SERVERADMINUI            = 34;          // Should defaults lean towards those preferred by server administrators?
     OS_MEDIACENTER              = 35;          // eHome Freestyle Project
     OS_APPLIANCE                = 36;          // Windows .NET Appliance Server
     PERCEIVEDFLAG_UNDEFINED     = $0000;
     PERCEIVEDFLAG_SOFTCODED     = $0001;
     PERCEIVEDFLAG_HARDCODED     = $0002;
     PERCEIVEDFLAG_NATIVESUPPORT = $0004;
     PERCEIVEDFLAG_GDIPLUS       = $0010;
     PERCEIVEDFLAG_WMSDK         = $0020;
     PERCEIVEDFLAG_ZIPFOLDER     = $0040;

  function  StrChrA(pszStart:PCSTR; wMatch:WORD):PCSTR;stdcall;external SHLWAPIDLL name 'StrChrA';
  function  StrChrW(pszStart:PCWSTR; wMatch:WCHAR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrChrW';
  function  StrChrIA(pszStart:PCSTR; wMatch:WORD):PCSTR;stdcall;external SHLWAPIDLL name 'StrChrIA';
  function  StrChrIW(pszStart:PCWSTR; wMatch:WCHAR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrChrIW';
  function  StrChrNW(pszStart:PCWSTR; wMatch:WCHAR; cchMax:UINT):PCWSTR;stdcall;external SHLWAPIDLL name 'StrChrNW';
  function  StrChrNIW(pszStart:PCWSTR; wMatch:WCHAR; cchMax:UINT):PWSTR;stdcall;external SHLWAPIDLL name 'StrChrNIW';
  function  StrCmpNA(psz1:PCSTR; psz2:PCSTR; nChar:longint):longint;stdcall;external SHLWAPIDLL name 'StrCmpNA';
  function  StrCmpNW(psz1:PCWSTR; psz2:PCWSTR; nChar:longint):longint;stdcall;external SHLWAPIDLL name 'StrCmpNW';
  function  StrCmpNIA(psz1:PCSTR; psz2:PCSTR; nChar:longint):longint;stdcall;external SHLWAPIDLL name 'StrCmpNIA';
  function  StrCmpNIW(psz1:PCWSTR; psz2:PCWSTR; nChar:longint):longint;stdcall;external SHLWAPIDLL name 'StrCmpNIW';
  function  StrCSpnA(pszStr:PCSTR; pszSet:PCSTR):longint;stdcall;external SHLWAPIDLL name 'StrCSpnA';
  function  StrCSpnW(pszStr:PCWSTR; pszSet:PCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrCSpnW';
  function  StrCSpnIA(pszStr:PCSTR; pszSet:PCSTR):longint;stdcall;external SHLWAPIDLL name 'StrCSpnIA';
  function  StrCSpnIW(pszStr:PCWSTR; pszSet:PCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrCSpnIW';
  function  StrDupA(pszSrch:PCSTR):PSTR;stdcall;external SHLWAPIDLL name 'StrDupA';
  function  StrDupW(pszSrch:PCWSTR):PWSTR;stdcall;external SHLWAPIDLL name 'StrDupW';
  function  StrFormatByteSizeEx(ull:ULONGLONG; flags:SFBS_FLAGS; pszBuf:PWSTR; cchBuf:UINT):HRESULT;stdcall;external SHLWAPIDLL name 'StrFormatByteSizeEx';
  function  StrFormatByteSizeA(dw:DWORD; pszBuf:PSTR; cchBuf:UINT):PSTR;stdcall;external SHLWAPIDLL name 'StrFormatByteSizeA';
  function  StrFormatByteSize64A(qdw:LONGLONG; pszBuf:PSTR; cchBuf:UINT):PSTR;stdcall;external SHLWAPIDLL name 'StrFormatByteSize64A';
  function  StrFormatByteSizeW(qdw:LONGLONG; pszBuf:PWSTR; cchBuf:UINT):PWSTR;stdcall;external SHLWAPIDLL name 'StrFormatByteSizeW';
  function  StrFormatKBSizeW(qdw:LONGLONG; pszBuf:PWSTR; cchBuf:UINT):PWSTR;stdcall;external SHLWAPIDLL name 'StrFormatKBSizeW';
  function  StrFormatKBSizeA(qdw:LONGLONG; pszBuf:PSTR; cchBuf:UINT):PSTR;stdcall;external SHLWAPIDLL name 'StrFormatKBSizeA';
  function  StrFromTimeIntervalA(pszOut:PSTR; cchMax:UINT; dwTimeMS:DWORD; digits:longint):longint;stdcall;external SHLWAPIDLL name 'StrFromTimeIntervalA';
  function  StrFromTimeIntervalW(pszOut:PWSTR; cchMax:UINT; dwTimeMS:DWORD; digits:longint):longint;stdcall;external SHLWAPIDLL name 'StrFromTimeIntervalW';
  function  StrIsIntlEqualA(fCaseSens:BOOL; pszString1:PCSTR; pszString2:PCSTR; nChar:longint):BOOL;stdcall;external SHLWAPIDLL name 'StrIsIntlEqualA';
  function  StrIsIntlEqualW(fCaseSens:BOOL; pszString1:PCWSTR; pszString2:PCWSTR; nChar:longint):BOOL;stdcall;external SHLWAPIDLL name 'StrIsIntlEqualW';
  function  StrNCatA(psz1:PSTR; psz2:PCSTR; cchMax:longint):PSTR;stdcall;external SHLWAPIDLL name 'StrNCatA';
  function  StrNCatW(psz1:PWSTR; psz2:PCWSTR; cchMax:longint):PWSTR;stdcall;external SHLWAPIDLL name 'StrNCatW';
  function  StrPBrkA(psz:PCSTR; pszSet:PCSTR):PCSTR;stdcall;external SHLWAPIDLL name 'StrPBrkA';
  function  StrPBrkW(psz:PCWSTR; pszSet:PCWSTR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrPBrkW';
  function  StrRChrA(pszStart:PCSTR; pszEnd:PCSTR; wMatch:WORD):PCSTR;stdcall;external SHLWAPIDLL name 'StrRChrA';
  function  StrRChrW(pszStart:PCWSTR; pszEnd:PCWSTR; wMatch:WCHAR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrRChrW';
  function  StrRChrIA(pszStart:PCSTR; pszEnd:PCSTR; wMatch:WORD):PCSTR;stdcall;external SHLWAPIDLL name 'StrRChrIA';
  function  StrRChrIW(pszStart:PCWSTR; pszEnd:PCWSTR; wMatch:WCHAR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrRChrIW';
  function  StrRStrIA(pszSource:PCSTR; pszLast:PCSTR; pszSrch:PCSTR):PCSTR;stdcall;external SHLWAPIDLL name 'StrRStrIA';
  function  StrRStrIW(pszSource:PCWSTR; pszLast:PCWSTR; pszSrch:PCWSTR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrRStrIW';
  function  StrSpnA(psz:PCSTR; pszSet:PCSTR):longint;stdcall;external SHLWAPIDLL name 'StrSpnA';
  function  StrSpnW(psz:PCWSTR; pszSet:PCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrSpnW';
  function  StrStrA(pszFirst:PCSTR; pszSrch:PCSTR):PCSTR;stdcall;external SHLWAPIDLL name 'StrStrA';
  function  StrStrW(pszFirst:PCWSTR; pszSrch:PCWSTR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrStrW';
  function  StrStrIA(pszFirst:PCSTR; pszSrch:PCSTR):PCSTR;stdcall;external SHLWAPIDLL name 'StrStrIA';
  function  StrStrIW(pszFirst:PCWSTR; pszSrch:PCWSTR):PCWSTR;stdcall;external SHLWAPIDLL name 'StrStrIW';
  function  StrStrNW(pszFirst:PCWSTR; pszSrch:PCWSTR; cchMax:UINT):PCWSTR;stdcall;external SHLWAPIDLL name 'StrStrNW';
  function  StrStrNIW(pszFirst:PCWSTR; pszSrch:PCWSTR; cchMax:UINT):PCWSTR;stdcall;external SHLWAPIDLL name 'StrStrNIW';
  function  StrToIntA(pszSrc:PCSTR):longint;stdcall;external SHLWAPIDLL name 'StrToIntA';
  function  StrToIntW(pszSrc:PCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrToIntW';
  function  StrToIntExA(pszString:PCSTR; dwFlags:STIF_FLAGS; piRet:plongint):BOOL;stdcall;external SHLWAPIDLL name 'StrToIntExA';
  function  StrToIntExW(pszString:PCWSTR; dwFlags:STIF_FLAGS; piRet:plongint):BOOL;stdcall;external SHLWAPIDLL name 'StrToIntExW';
  function  StrToInt64ExA(pszString:PCSTR; dwFlags:STIF_FLAGS; pllRet:pLONGLONG):BOOL;stdcall;external SHLWAPIDLL name 'StrToInt64ExA';
  function  StrToInt64ExW(pszString:PCWSTR; dwFlags:STIF_FLAGS; pllRet:pLONGLONG):BOOL;stdcall;external SHLWAPIDLL name 'StrToInt64ExW';
  function  StrTrimA(psz:PSTR; pszTrimChars:PCSTR):BOOL;stdcall;external SHLWAPIDLL name 'StrTrimA';
  function  StrTrimW(psz:PWSTR; pszTrimChars:PCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'StrTrimW';
  function  StrCatW(psz1:PWSTR; psz2:PCWSTR):PWSTR;stdcall;external SHLWAPIDLL name 'StrCatW';
  function  StrCmpW(psz1:PCWSTR; psz2:PCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrCmpW';
  function  StrCmpIW(psz1:PCWSTR; psz2:PCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrCmpIW';
  function  StrCpyW(psz1:PWSTR; psz2:PCWSTR):PWSTR;stdcall;external SHLWAPIDLL name 'StrCpyW';
  function  StrCpyNW(pszDst:PWSTR; pszSrc:PCWSTR; cchMax:longint):PWSTR;stdcall;external SHLWAPIDLL name 'StrCpyNW';
  function  StrCatBuffW(pszDest:PWSTR; pszSrc:PCWSTR; cchDestBuffSize:longint):PWSTR;stdcall;external SHLWAPIDLL name 'StrCatBuffW';
  function  StrCatBuffA(pszDest:PSTR; pszSrc:PCSTR; cchDestBuffSize:longint):PSTR;stdcall;external SHLWAPIDLL name 'StrCatBuffA';
  function  ChrCmpIA(w1:WORD; w2:WORD):BOOL;stdcall;external SHLWAPIDLL name 'ChrCmpIA';
  function  ChrCmpIW(w1:WCHAR; w2:WCHAR):BOOL;stdcall;external SHLWAPIDLL name 'ChrCmpIW';

  function  StrRetToStrA(pstr:PSTRRET; pidl:PCUITEMID_CHILD; ppsz:PLPSTR):HRESULT;stdcall;external SHLWAPIDLL name 'StrRetToStrA';
  function  StrRetToStrW(pstr:PSTRRET; pidl:PCUITEMID_CHILD; ppsz:PLPWSTR):HRESULT;stdcall;external SHLWAPIDLL name 'StrRetToStrW';
  function  StrRetToBufA(pstr:PSTRRET; pidl:PCUITEMID_CHILD; pszBuf:LPSTR; cchBuf:UINT):HRESULT;stdcall;external SHLWAPIDLL name 'StrRetToBufA';
  function  StrRetToBufW(pstr:PSTRRET; pidl:PCUITEMID_CHILD; pszBuf:LPWSTR; cchBuf:UINT):HRESULT;stdcall;external SHLWAPIDLL name 'StrRetToBufW';
  function  SHStrDupA(psz:LPCSTR; ppwsz:PLPWSTR):HRESULT;stdcall;external SHLWAPIDLL name 'SHStrDupA';
  function  SHStrDupW(psz:LPCWSTR; ppwsz:PLPWSTR):HRESULT;stdcall;external SHLWAPIDLL name 'SHStrDupW';
  function  StrCmpLogicalW(psz1:PCWSTR; psz2:PCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrCmpLogicalW';
  function  StrCatChainW(pszDst:PWSTR; cchDst:DWORD; ichAt:DWORD; pszSrc:PCWSTR):DWORD;stdcall;external SHLWAPIDLL name 'StrCatChainW';
  function  StrRetToBSTR(pstr:PSTRRET; pidl:PCUITEMID_CHILD; out pbstr:BSTR):HRESULT;stdcall;external SHLWAPIDLL name 'StrRetToBSTR';
  function  SHLoadIndirectString(pszSource:PCWSTR; pszOutBuf:PWSTR; cchOutBuf:UINT; ppvReserved:Ppointer):HRESULT;stdcall;external SHLWAPIDLL name 'SHLoadIndirectString';
  function  IsCharSpaceA(wch:AnsiChar):BOOL;stdcall;external SHLWAPIDLL name 'IsCharSpaceA';
  function  IsCharSpaceW(wch:WCHAR):BOOL;stdcall;external SHLWAPIDLL name 'IsCharSpaceW';
  function  StrCmpCA(pszStr1:LPCSTR; pszStr2:LPCSTR):longint;stdcall;external SHLWAPIDLL name 'StrCmpCA';
  function  StrCmpCW(pszStr1:LPCWSTR; pszStr2:LPCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrCmpCW';
  function  StrCmpICA(pszStr1:LPCSTR; pszStr2:LPCSTR):longint;stdcall;external SHLWAPIDLL name 'StrCmpICA';
  function  StrCmpICW(pszStr1:LPCWSTR; pszStr2:LPCWSTR):longint;stdcall;external SHLWAPIDLL name 'StrCmpICW';
  function  StrCmpNCA(pszStr1:LPCSTR; pszStr2:LPCSTR; nChar:longint):longint;stdcall;external SHLWAPIDLL name 'StrCmpNCA';
  function  StrCmpNCW(pszStr1:LPCWSTR; pszStr2:LPCWSTR; nChar:longint):longint;stdcall;external SHLWAPIDLL name 'StrCmpNCW';
  function  StrCmpNICA(pszStr1:LPCSTR; pszStr2:LPCSTR; nChar:longint):longint;stdcall;external SHLWAPIDLL name 'StrCmpNICA';
  function  StrCmpNICW(pszStr1:LPCWSTR; pszStr2:LPCWSTR; nChar:longint):longint;stdcall;external SHLWAPIDLL name 'StrCmpNICW';
  function  IntlStrEqWorkerA(fCaseSens:BOOL; lpString1:LPCSTR; lpString2:LPCSTR; nChar:longint):BOOL;stdcall;external SHLWAPIDLL name 'IntlStrEqWorkerA';
  function  IntlStrEqWorkerW(fCaseSens:BOOL; lpString1:LPCWSTR; lpString2:LPCWSTR; nChar:longint):BOOL;stdcall;external SHLWAPIDLL name 'IntlStrEqWorkerW';
  function  PathAddBackslashA(pszPath:LPSTR):LPSTR;stdcall;external SHLWAPIDLL name 'PathAddBackslashA';
  function  PathAddBackslashW(pszPath:LPWSTR):LPWSTR;stdcall;external SHLWAPIDLL name 'PathAddBackslashW';
  function  PathAddExtensionA(pszPath:LPSTR; pszExt:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathAddExtensionA';
  function  PathAddExtensionW(pszPath:LPWSTR; pszExt:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathAddExtensionW';
  function  PathAppendA(pszPath:LPSTR; pszMore:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathAppendA';
  function  PathAppendW(pszPath:LPWSTR; pszMore:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathAppendW';
  function  PathBuildRootA(pszRoot:LPSTR; iDrive:longint):LPSTR;stdcall;external SHLWAPIDLL name 'PathBuildRootA';
  function  PathBuildRootW(pszRoot:LPWSTR; iDrive:longint):LPWSTR;stdcall;external SHLWAPIDLL name 'PathBuildRootW';
  function  PathCanonicalizeA(pszBuf:LPSTR; pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathCanonicalizeA';
  function  PathCanonicalizeW(pszBuf:LPWSTR; pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathCanonicalizeW';
  function  PathCombineA(pszDest:LPSTR; pszDir:LPCSTR; pszFile:LPCSTR):LPSTR;stdcall;external SHLWAPIDLL name 'PathCombineA';
  function  PathCombineW(pszDest:LPWSTR; pszDir:LPCWSTR; pszFile:LPCWSTR):LPWSTR;stdcall;external SHLWAPIDLL name 'PathCombineW';
  function  PathCompactPathA(hDC:HDC; pszPath:LPSTR; dx:UINT):BOOL;stdcall;external SHLWAPIDLL name 'PathCompactPathA';
  function  PathCompactPathW(hDC:HDC; pszPath:LPWSTR; dx:UINT):BOOL;stdcall;external SHLWAPIDLL name 'PathCompactPathW';
  function  PathCompactPathExA(pszOut:LPSTR; pszSrc:LPCSTR; cchMax:UINT; dwFlags:DWORD):BOOL;stdcall;external SHLWAPIDLL name 'PathCompactPathExA';
  function  PathCompactPathExW(pszOut:LPWSTR; pszSrc:LPCWSTR; cchMax:UINT; dwFlags:DWORD):BOOL;stdcall;external SHLWAPIDLL name 'PathCompactPathExW';
  function  PathCommonPrefixA(pszFile1:LPCSTR; pszFile2:LPCSTR; achPath:LPSTR):longint;stdcall;external SHLWAPIDLL name 'PathCommonPrefixA';
  function  PathCommonPrefixW(pszFile1:LPCWSTR; pszFile2:LPCWSTR; achPath:LPWSTR):longint;stdcall;external SHLWAPIDLL name 'PathCommonPrefixW';
  function  PathFileExistsA(pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathFileExistsA';
  function  PathFileExistsW(pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathFileExistsW';
  function  PathFindExtensionA(pszPath:LPCSTR):LPCSTR;stdcall;external SHLWAPIDLL name 'PathFindExtensionA';
  function  PathFindExtensionW(pszPath:LPCWSTR):LPCWSTR;stdcall;external SHLWAPIDLL name 'PathFindExtensionW';
  function  PathFindFileNameA(pszPath:LPCSTR):LPCSTR;stdcall;external SHLWAPIDLL name 'PathFindFileNameA';
  function  PathFindFileNameW(pszPath:LPCWSTR):LPCWSTR;stdcall;external SHLWAPIDLL name 'PathFindFileNameW';
  function  PathFindNextComponentA(pszPath:LPCSTR):LPCSTR;stdcall;external SHLWAPIDLL name 'PathFindNextComponentA';
  function  PathFindNextComponentW(pszPath:LPCWSTR):LPCWSTR;stdcall;external SHLWAPIDLL name 'PathFindNextComponentW';
  function  PathFindOnPathA(pszPath:LPSTR; ppszOtherDirs:PZPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathFindOnPathA';
  function  PathFindOnPathW(pszPath:LPWSTR; ppszOtherDirs:PZPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathFindOnPathW';
  function  PathFindSuffixArrayA(pszPath:LPCSTR; apszSuffix:PLPSTR; iArraySize:longint):LPCSTR;stdcall;external SHLWAPIDLL name 'PathFindSuffixArrayA';
  function  PathFindSuffixArrayW(pszPath:LPCWSTR; apszSuffix:PLPWSTR; iArraySize:longint):LPCWSTR;stdcall;external SHLWAPIDLL name 'PathFindSuffixArrayW';
  function  PathGetArgsA(pszPath:LPCSTR):LPCSTR;stdcall;external SHLWAPIDLL name 'PathGetArgsA';
  function  PathGetArgsW(pszPath:LPCWSTR):LPCWSTR;stdcall;external SHLWAPIDLL name 'PathGetArgsW';
  function  PathIsLFNFileSpecA(pszName:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsLFNFileSpecA';
  function  PathIsLFNFileSpecW(pszName:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsLFNFileSpecW';
  function  PathGetCharTypeA(ch:UCHAR):Uint;stdcall;external SHLWAPIDLL name 'PathGetCharTypeA';
  function  PathGetCharTypeW(ch:WCHAR):Uint;stdcall;external SHLWAPIDLL name 'PathGetCharTypeW';
  function  PathGetDriveNumberA(pszPath:LPCSTR):longint;stdcall;external SHLWAPIDLL name 'PathGetDriveNumberA';
  function  PathGetDriveNumberW(pszPath:LPCWSTR):longint;stdcall;external SHLWAPIDLL name 'PathGetDriveNumberW';
  function  PathIsDirectoryA(pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsDirectoryA';
  function  PathIsDirectoryW(pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsDirectoryW';
  function  PathIsDirectoryEmptyA(pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsDirectoryEmptyA';
  function  PathIsDirectoryEmptyW(pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsDirectoryEmptyW';
  function  PathIsFileSpecA(pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsFileSpecA';
  function  PathIsFileSpecW(pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsFileSpecW';
  function  PathIsPrefixA(pszPrefix:LPCSTR; pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsPrefixA';
  function  PathIsPrefixW(pszPrefix:LPCWSTR; pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsPrefixW';
  function  PathIsRelativeA(pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsRelativeA';
  function  PathIsRelativeW(pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsRelativeW';
  function  PathIsRootA(pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsRootA';
  function  PathIsRootW(pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsRootW';
  function  PathIsSameRootA(pszPath1:LPCSTR; pszPath2:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsSameRootA';
  function  PathIsSameRootW(pszPath1:LPCWSTR; pszPath2:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsSameRootW';
  function  PathIsUNCA(pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsUNCA';
  function  PathIsUNCW(pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsUNCW';
  function  PathIsNetworkPathA(pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsNetworkPathA';
  function  PathIsNetworkPathW(pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsNetworkPathW';
  function  PathIsUNCServerA(pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsUNCServerA';
  function  PathIsUNCServerW(pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsUNCServerW';
  function  PathIsUNCServerShareA(pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsUNCServerShareA';
  function  PathIsUNCServerShareW(pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsUNCServerShareW';
  function  PathIsContentTypeA(pszPath:LPCSTR; pszContentType:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsContentTypeA';
  function  PathIsContentTypeW(pszPath:LPCWSTR; pszContentType:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsContentTypeW';
  function  PathIsURLA(pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsURLA';
  function  PathIsURLW(pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathIsURLW';
  function  PathMakePrettyA(pszPath:LPSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathMakePrettyA';
  function  PathMakePrettyW(pszPath:LPWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathMakePrettyW';
  function  PathMatchSpecA(pszFile:LPCSTR; pszSpec:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathMatchSpecA';
  function  PathMatchSpecW(pszFile:LPCWSTR; pszSpec:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathMatchSpecW';
  function  PathMatchSpecExA(pszFile:LPCSTR; pszSpec:LPCSTR; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'PathMatchSpecExA';
  function  PathMatchSpecExW(pszFile:LPCWSTR; pszSpec:LPCWSTR; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'PathMatchSpecExW';
  function  PathParseIconLocationA(pszIconFile:LPSTR):longint;stdcall;external SHLWAPIDLL name 'PathParseIconLocationA';
  function  PathParseIconLocationW(pszIconFile:LPWSTR):longint;stdcall;external SHLWAPIDLL name 'PathParseIconLocationW';
  function  PathQuoteSpacesA(lpsz:LPSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathQuoteSpacesA';
  function  PathQuoteSpacesW(lpsz:LPWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathQuoteSpacesW';
  function  PathRelativePathToA(pszPath:LPSTR; pszFrom:LPCSTR; dwAttrFrom:DWORD; pszTo:LPCSTR; dwAttrTo:DWORD):BOOL;stdcall;external SHLWAPIDLL name 'PathRelativePathToA';
  function  PathRelativePathToW(pszPath:LPWSTR; pszFrom:LPCWSTR; dwAttrFrom:DWORD; pszTo:LPCWSTR; dwAttrTo:DWORD):BOOL;stdcall;external SHLWAPIDLL name 'PathRelativePathToW';
  procedure PathRemoveArgsA(pszPath:LPSTR);stdcall;external SHLWAPIDLL name 'PathRemoveArgsA';
  procedure PathRemoveArgsW(pszPath:LPWSTR);stdcall;external SHLWAPIDLL name 'PathRemoveArgsW';
  function  PathRemoveBackslashA(pszPath:LPSTR):LPSTR;stdcall;external SHLWAPIDLL name 'PathRemoveBackslashA';
  function  PathRemoveBackslashW(pszPath:LPWSTR):LPWSTR;stdcall;external SHLWAPIDLL name 'PathRemoveBackslashW';
  procedure PathRemoveBlanksA(pszPath:LPSTR);stdcall;external SHLWAPIDLL name 'PathRemoveBlanksA';
  procedure PathRemoveBlanksW(pszPath:LPWSTR);stdcall;external SHLWAPIDLL name 'PathRemoveBlanksW';
  procedure PathRemoveExtensionA(pszPath:LPSTR);stdcall;external SHLWAPIDLL name 'PathRemoveExtensionA';
  procedure PathRemoveExtensionW(pszPath:LPWSTR);stdcall;external SHLWAPIDLL name 'PathRemoveExtensionW';
  function  PathRemoveFileSpecA(pszPath:LPSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathRemoveFileSpecA';
  function  PathRemoveFileSpecW(pszPath:LPWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathRemoveFileSpecW';
  function  PathRenameExtensionA(pszPath:LPSTR; pszExt:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathRenameExtensionA';
  function  PathRenameExtensionW(pszPath:LPWSTR; pszExt:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathRenameExtensionW';
  function  PathSearchAndQualifyA(pszPath:LPCSTR; pszBuf:LPSTR; cchBuf:UINT):BOOL;stdcall;external SHLWAPIDLL name 'PathSearchAndQualifyA';
  function  PathSearchAndQualifyW(pszPath:LPCWSTR; pszBuf:LPWSTR; cchBuf:UINT):BOOL;stdcall;external SHLWAPIDLL name 'PathSearchAndQualifyW';
  procedure PathSetDlgItemPathA(hDlg:HWND; id:longint; pszPath:LPCSTR);stdcall;external SHLWAPIDLL name 'PathSetDlgItemPathA';
  procedure PathSetDlgItemPathW(hDlg:HWND; id:longint; pszPath:LPCWSTR);stdcall;external SHLWAPIDLL name 'PathSetDlgItemPathW';
  function  PathSkipRootA(pszPath:LPCSTR):LPCSTR;stdcall;external SHLWAPIDLL name 'PathSkipRootA';
  function  PathSkipRootW(pszPath:LPCWSTR):LPCWSTR;stdcall;external SHLWAPIDLL name 'PathSkipRootW';
  procedure PathStripPathA(pszPath:LPSTR);stdcall;external SHLWAPIDLL name 'PathStripPathA';
  procedure PathStripPathW(pszPath:LPWSTR);stdcall;external SHLWAPIDLL name 'PathStripPathW';
  function  PathStripToRootA(pszPath:LPSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathStripToRootA';
  function  PathStripToRootW(pszPath:LPWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathStripToRootW';
  function  PathUnquoteSpacesA(lpsz:LPSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathUnquoteSpacesA';
  function  PathUnquoteSpacesW(lpsz:LPWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathUnquoteSpacesW';
  function  PathMakeSystemFolderA(pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathMakeSystemFolderA';
  function  PathMakeSystemFolderW(pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathMakeSystemFolderW';
  function  PathUnmakeSystemFolderA(pszPath:LPCSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathUnmakeSystemFolderA';
  function  PathUnmakeSystemFolderW(pszPath:LPCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'PathUnmakeSystemFolderW';
  function  PathIsSystemFolderA(pszPath:LPCSTR; dwAttrb:DWORD):BOOL;stdcall;external SHLWAPIDLL name 'PathIsSystemFolderA';
  function  PathIsSystemFolderW(pszPath:LPCWSTR; dwAttrb:DWORD):BOOL;stdcall;external SHLWAPIDLL name 'PathIsSystemFolderW';
  procedure PathUndecorateA(pszPath:LPSTR);stdcall;external SHLWAPIDLL name 'PathUndecorateA';
  procedure PathUndecorateW(pszPath:LPWSTR);stdcall;external SHLWAPIDLL name 'PathUndecorateW';
  function  PathUnExpandEnvStringsA(pszPath:LPCSTR; pszBuf:LPSTR; cchBuf:UINT):BOOL;stdcall;external SHLWAPIDLL name 'PathUnExpandEnvStringsA';
  function  PathUnExpandEnvStringsW(pszPath:LPCWSTR; pszBuf:LPWSTR; cchBuf:UINT):BOOL;stdcall;external SHLWAPIDLL name 'PathUnExpandEnvStringsW';
  function  UrlCompareA(psz1:PCSTR; psz2:PCSTR; fIgnoreSlash:BOOL):longint;stdcall;external SHLWAPIDLL name 'UrlCompareA';
  function  UrlCompareW(psz1:PCWSTR; psz2:PCWSTR; fIgnoreSlash:BOOL):longint;stdcall;external SHLWAPIDLL name 'UrlCompareW';
  function  UrlCombineA(pszBase:PCSTR; pszRelative:PCSTR; pszCombined:PSTR; pcchCombined:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlCombineA';
  function  UrlCombineW(pszBase:PCWSTR; pszRelative:PCWSTR; pszCombined:PWSTR; pcchCombined:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlCombineW';
  function  UrlCanonicalizeA(pszUrl:PCSTR; pszCanonicalized:PSTR; pcchCanonicalized:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlCanonicalizeA';
  function  UrlCanonicalizeW(pszUrl:PCWSTR; pszCanonicalized:PWSTR; pcchCanonicalized:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlCanonicalizeW';
  function  UrlIsOpaqueA(pszURL:PCSTR):BOOL;stdcall;external SHLWAPIDLL name 'UrlIsOpaqueA';
  function  UrlIsOpaqueW(pszURL:PCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'UrlIsOpaqueW';
  function  UrlIsNoHistoryA(pszURL:PCSTR):BOOL;stdcall;external SHLWAPIDLL name 'UrlIsNoHistoryA';
  function  UrlIsNoHistoryW(pszURL:PCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'UrlIsNoHistoryW';
  function  UrlIsA(pszUrl:PCSTR; UrlIs:URLIS):BOOL;stdcall;external SHLWAPIDLL name 'UrlIsA';
  function  UrlIsW(pszUrl:PCWSTR; UrlIs:URLIS):BOOL;stdcall;external SHLWAPIDLL name 'UrlIsW';
  function  UrlGetLocationA(pszURL:PCSTR):LPCSTR;stdcall;external SHLWAPIDLL name 'UrlGetLocationA';
  function  UrlGetLocationW(pszURL:PCWSTR):LPCWSTR;stdcall;external SHLWAPIDLL name 'UrlGetLocationW';
  function  UrlUnescapeA(pszUrl:PSTR; pszUnescaped:PSTR; pcchUnescaped:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlUnescapeA';
  function  UrlUnescapeW(pszUrl:PWSTR; pszUnescaped:PWSTR; pcchUnescaped:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlUnescapeW';
  function  UrlEscapeA(pszUrl:PCSTR; pszEscaped:PSTR; pcchEscaped:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlEscapeA';
  function  UrlEscapeW(pszUrl:PCWSTR; pszEscaped:PWSTR; pcchEscaped:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlEscapeW';
  function  UrlCreateFromPathA(pszPath:PCSTR; pszUrl:PSTR; pcchUrl:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlCreateFromPathA';
  function  UrlCreateFromPathW(pszPath:PCWSTR; pszUrl:PWSTR; pcchUrl:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlCreateFromPathW';
  function  PathCreateFromUrlA(pszUrl:PCSTR; pszPath:PSTR; pcchPath:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'PathCreateFromUrlA';
  function  PathCreateFromUrlW(pszUrl:PCWSTR; pszPath:PWSTR; pcchPath:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'PathCreateFromUrlW';
  function  PathCreateFromUrlAlloc(pszIn:PCWSTR; out ppszOut:PWSTR; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'PathCreateFromUrlAlloc';
  function  UrlHashA(pszUrl:PCSTR; pbHash:PBYTE; cbHash:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlHashA';
  function  UrlHashW(pszUrl:PCWSTR; pbHash:PBYTE; cbHash:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlHashW';
  function  UrlGetPartW(pszIn:PCWSTR; pszOut:PWSTR; pcchOut:PDWORD; dwPart:DWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlGetPartW';
  function  UrlGetPartA(pszIn:PCSTR; pszOut:PSTR; pcchOut:PDWORD; dwPart:DWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlGetPartA';
  function  UrlApplySchemeA(pszIn:PCSTR; pszOut:PSTR; pcchOut:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlApplySchemeA';
  function  UrlApplySchemeW(pszIn:PCWSTR; pszOut:PWSTR; pcchOut:PDWORD; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlApplySchemeW';
  function  HashData(pbData:PBYTE; cbData:DWORD; pbHash:PBYTE; cbHash:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'HashData';
  function  UrlFixupW(pcszUrl:PCWSTR; pszTranslatedUrl:PWSTR; cchMax:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'UrlFixupW';
  function SHDeleteEmptyKeyA(hKey:HKEY; pszSubKey:LPCSTR):LSTATUS;stdcall;external SHLWAPIDLL name 'SHDeleteEmptyKeyA';
  function SHDeleteEmptyKeyW(hKey:HKEY; pszSubKey:LPCWSTR):LSTATUS;stdcall;external SHLWAPIDLL name 'SHDeleteEmptyKeyW';
  function SHDeleteKeyA(hKey:HKEY; pszSubKey:LPCSTR):LSTATUS;stdcall;external SHLWAPIDLL name 'SHDeleteKeyA';
  function SHDeleteKeyW(hKey:HKEY; pszSubKey:LPCWSTR):LSTATUS;stdcall;external SHLWAPIDLL name 'SHDeleteKeyW';
  function SHRegDuplicateHKey(hKey:HKEY):HKEY;stdcall;external SHLWAPIDLL name 'SHRegDuplicateHKey';
  function SHDeleteValueA(hKey:HKEY; pszSubKey:LPCSTR; pszValue:LPCSTR):LSTATUS;stdcall;external SHLWAPIDLL name 'SHDeleteValueA';
  function SHDeleteValueW(hKey:HKEY; pszSubKey:LPCWSTR; pszValue:LPCWSTR):LSTATUS;stdcall;external SHLWAPIDLL name 'SHDeleteValueW';
  {LSTATUS    SHGetValueA( }
  {LSTATUS    SHGetValueW( }
  function SHSetValueA(hKey:HKEY; pszSubKey:LPCSTR; pszValue:LPCSTR; dwType:DWORD; pvData:LPCVOID;
             cbData:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHSetValueA';
  function SHSetValueW(hKey:HKEY; pszSubKey:LPCWSTR; pszValue:LPCWSTR; dwType:DWORD; pvData:LPCVOID;
             cbData:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHSetValueW';
  function SHRegGetValueA(hKey:HKEY; pszSubKey:LPCSTR; pszValue:LPCSTR; srrfFlags:SRRF; pdwType:PDWORD;
             pvData:pointer; pcbData:PDWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegGetValueA';
  function SHRegGetValueW(hKey:HKEY; pszSubKey:LPCWSTR; pszValue:LPCWSTR; srrfFlags:SRRF; pdwType:PDWORD;
             pvData:pointer; pcbData:PDWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegGetValueW';
  function SHRegSetValue(hkey:HKEY; pszSubKey:LPCWSTR; pszValue:LPCWSTR; srrfFlags:SRRF; dwType:DWORD;
             pvData:LPCVOID; cbData:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegSetValue';

  function SHRegGetValueFromHKCUHKLM(pwszKey:PCWSTR; pwszValue:PCWSTR; srrfFlags:SRRF; pdwType:PDWORD; pvData:pointer;
             pcbData:PDWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegGetValueFromHKCUHKLM';

  function SHQueryValueExA(hkey:HKEY; pszValue:LPCSTR; pdwReserved:PDWORD; pdwType:PDWORD; pvData:pointer;
             pcbData:PDWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHQueryValueExA';

  function SHQueryValueExW(hkey:HKEY; pszValue:LPCWSTR; pdwReserved:PDWORD; pdwType:PDWORD; pvData:pointer;
             pcbData:PDWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHQueryValueExW';

  function SHEnumKeyExA(hKey:HKEY; dwIndex:DWORD; pszName:LPSTR; pcchName:LPDWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHEnumKeyExA';

  function SHEnumKeyExW(hKey:HKEY; dwIndex:DWORD; pszName:LPWSTR; pcchName:LPDWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHEnumKeyExW';

  function SHEnumValueA(hKey:HKEY; dwIndex:DWORD; pszValueName:PSTR; pcchValueName:LPDWORD; pdwType:LPDWORD;
             pvData:pointer; pcbData:LPDWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHEnumValueA';

  function SHEnumValueW(hKey:HKEY; dwIndex:DWORD; pszValueName:PWSTR; pcchValueName:LPDWORD; pdwType:LPDWORD;
             pvData:pointer; pcbData:LPDWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHEnumValueW';

  function SHQueryInfoKeyA(hKey:HKEY; pcSubKeys:LPDWORD; pcchMaxSubKeyLen:LPDWORD; pcValues:LPDWORD; pcchMaxValueNameLen:LPDWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHQueryInfoKeyA';

  function SHQueryInfoKeyW(hKey:HKEY; pcSubKeys:LPDWORD; pcchMaxSubKeyLen:LPDWORD; pcValues:LPDWORD; pcchMaxValueNameLen:LPDWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHQueryInfoKeyW';

  function SHCopyKeyA(_hKeySrc:HKEY; pszSrcSubKey:LPCSTR; _hKeyDest:HKEY; fReserved:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHCopyKeyA';

  function SHCopyKeyW(_hKeySrc:HKEY; pszSrcSubKey:LPCWSTR; _hKeyDest:HKEY; fReserved:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHCopyKeyW';

  function SHRegGetPathA(hKey:HKEY; pcszSubKey:LPCSTR; pcszValue:LPCSTR; pszPath:LPSTR; dwFlags:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegGetPathA';

  function SHRegGetPathW(hKey:HKEY; pcszSubKey:LPCWSTR; pcszValue:LPCWSTR; pszPath:LPWSTR; dwFlags:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegGetPathW';

  function SHRegSetPathA(hKey:HKEY; pcszSubKey:LPCSTR; pcszValue:LPCSTR; pcszPath:LPCSTR; dwFlags:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegSetPathA';

  function SHRegSetPathW(hKey:HKEY; pcszSubKey:LPCWSTR; pcszValue:LPCWSTR; pcszPath:LPCWSTR; dwFlags:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegSetPathW';

  function SHRegCreateUSKeyA(pszPath:LPCSTR; samDesired:REGSAM; hRelativeUSKey:HUSKEY; phNewUSKey:PHUSKEY; dwFlags:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegCreateUSKeyA';

  function SHRegCreateUSKeyW(pwzPath:LPCWSTR; samDesired:REGSAM; hRelativeUSKey:HUSKEY; phNewUSKey:PHUSKEY; dwFlags:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegCreateUSKeyW';

  function SHRegOpenUSKeyA(pszPath:LPCSTR; samDesired:REGSAM; hRelativeUSKey:HUSKEY; phNewUSKey:PHUSKEY; fIgnoreHKCU:BOOL):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegOpenUSKeyA';

  function SHRegOpenUSKeyW(pwzPath:LPCWSTR; samDesired:REGSAM; hRelativeUSKey:HUSKEY; phNewUSKey:PHUSKEY; fIgnoreHKCU:BOOL):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegOpenUSKeyW';

  function SHRegQueryUSValueA(hUSKey:HUSKEY; pszValue:LPCSTR; pdwType:PDWORD; pvData:pointer; pcbData:PDWORD;
             fIgnoreHKCU:BOOL; pvDefaultData:pointer; dwDefaultDataSize:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegQueryUSValueA';

  function SHRegQueryUSValueW(hUSKey:HUSKEY; pszValue:LPCWSTR; pdwType:PDWORD; pvData:pointer; pcbData:PDWORD;
             fIgnoreHKCU:BOOL; pvDefaultData:pointer; dwDefaultDataSize:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegQueryUSValueW';

(* Const before type ignored *)
  function SHRegWriteUSValueA(hUSKey:HUSKEY; pszValue:LPCSTR; dwType:DWORD; pvData:pointer; cbData:DWORD;
             dwFlags:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegWriteUSValueA';

(* Const before type ignored *)
  function SHRegWriteUSValueW(hUSKey:HUSKEY; pwzValue:LPCWSTR; dwType:DWORD; pvData:pointer; cbData:DWORD;
             dwFlags:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegWriteUSValueW';

  function SHRegDeleteUSValueA(hUSKey:HUSKEY; pszValue:LPCSTR; delRegFlags:SHREGDEL_FLAGS):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegDeleteUSValueA';

  function SHRegDeleteUSValueW(hUSKey:HUSKEY; pwzValue:LPCWSTR; delRegFlags:SHREGDEL_FLAGS):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegDeleteUSValueW';

  function SHRegDeleteEmptyUSKeyW(hUSKey:HUSKEY; pwzSubKey:LPCWSTR; delRegFlags:SHREGDEL_FLAGS):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegDeleteEmptyUSKeyW';

  function SHRegDeleteEmptyUSKeyA(hUSKey:HUSKEY; pszSubKey:LPCSTR; delRegFlags:SHREGDEL_FLAGS):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegDeleteEmptyUSKeyA';

  function SHRegEnumUSKeyA(hUSKey:HUSKEY; dwIndex:DWORD; pszName:LPSTR; pcchName:LPDWORD; enumRegFlags:SHREGENUM_FLAGS):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegEnumUSKeyA';

  function SHRegEnumUSKeyW(hUSKey:HUSKEY; dwIndex:DWORD; pwzName:LPWSTR; pcchName:LPDWORD; enumRegFlags:SHREGENUM_FLAGS):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegEnumUSKeyW';

  function SHRegEnumUSValueA(hUSkey:HUSKEY; dwIndex:DWORD; pszValueName:LPSTR; pcchValueName:LPDWORD; pdwType:LPDWORD;
             pvData:pointer; pcbData:LPDWORD; enumRegFlags:SHREGENUM_FLAGS):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegEnumUSValueA';

  function SHRegEnumUSValueW(hUSkey:HUSKEY; dwIndex:DWORD; pszValueName:LPWSTR; pcchValueName:LPDWORD; pdwType:LPDWORD;
             pvData:pointer; pcbData:LPDWORD; enumRegFlags:SHREGENUM_FLAGS):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegEnumUSValueW';

  function SHRegQueryInfoUSKeyA(hUSKey:HUSKEY; pcSubKeys:LPDWORD; pcchMaxSubKeyLen:LPDWORD; pcValues:LPDWORD; pcchMaxValueNameLen:LPDWORD;
             enumRegFlags:SHREGENUM_FLAGS):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegQueryInfoUSKeyA';

  function SHRegQueryInfoUSKeyW(hUSKey:HUSKEY; pcSubKeys:LPDWORD; pcchMaxSubKeyLen:LPDWORD; pcValues:LPDWORD; pcchMaxValueNameLen:LPDWORD;
             enumRegFlags:SHREGENUM_FLAGS):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegQueryInfoUSKeyW';

  function SHRegCloseUSKey(hUSKey:HUSKEY):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegCloseUSKey';

(* Const before type ignored *)
  function SHRegSetUSValueA(pszSubKey:LPCSTR; pszValue:LPCSTR; dwType:DWORD; pvData:pointer; cbData:DWORD;
             dwFlags:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegSetUSValueA';

(* Const before type ignored *)
  function SHRegSetUSValueW(pwzSubKey:LPCWSTR; pwzValue:LPCWSTR; dwType:DWORD; pvData:pointer; cbData:DWORD;
             dwFlags:DWORD):LSTATUS;stdcall;external SHLWAPIDLL name 'SHRegSetUSValueW';

  function SHRegGetIntW(hk:HKEY; pwzKey:PCWSTR; iDefault:longint):longint;stdcall;external SHLWAPIDLL name 'SHRegGetIntW';
  function SHRegGetBoolUSValueA(pszSubKey:LPCSTR; pszValue:LPCSTR; fIgnoreHKCU:BOOL; fDefault:BOOL):BOOL;stdcall;external SHLWAPIDLL name 'SHRegGetBoolUSValueA';
  function SHRegGetBoolUSValueW(pszSubKey:LPCWSTR; pszValue:LPCWSTR; fIgnoreHKCU:BOOL; fDefault:BOOL):BOOL;stdcall;external SHLWAPIDLL name 'SHRegGetBoolUSValueW';
  function AssocCreate(clsid:CLSID; riid:REFIID; ppv:Ppointer):HRESULT;stdcall;external SHLWAPIDLL name 'AssocCreate';
  function AssocQueryStringA(flags:ASSOCF; str:ASSOCSTR; pszAssoc:LPCSTR; pszExtra:LPCSTR; pszOut:LPSTR;
             pcchOut:PDWORD):HRESULT;stdcall;external SHLWAPIDLL name 'AssocQueryStringA';
  function AssocQueryStringW(flags:ASSOCF; str:ASSOCSTR; pszAssoc:LPCWSTR; pszExtra:LPCWSTR; pszOut:LPWSTR;
             pcchOut:PDWORD):HRESULT;stdcall;external SHLWAPIDLL name 'AssocQueryStringW';
  function AssocQueryStringByKeyA(flags:ASSOCF; str:ASSOCSTR; hkAssoc:HKEY; pszExtra:LPCSTR; pszOut:LPSTR;
             pcchOut:PDWORD):HRESULT;stdcall;external SHLWAPIDLL name 'AssocQueryStringByKeyA';
  function AssocQueryStringByKeyW(flags:ASSOCF; str:ASSOCSTR; hkAssoc:HKEY; pszExtra:LPCWSTR; pszOut:LPWSTR;
             pcchOut:PDWORD):HRESULT;stdcall;external SHLWAPIDLL name 'AssocQueryStringByKeyW';
  function AssocQueryKeyA(flags:ASSOCF; key:ASSOCKEY; pszAssoc:LPCSTR; pszExtra:LPCSTR; phkeyOut:PHKEY):HRESULT;stdcall;external SHLWAPIDLL name 'AssocQueryKeyA';
  function AssocQueryKeyW(flags:ASSOCF; key:ASSOCKEY; pszAssoc:LPCWSTR; pszExtra:LPCWSTR; phkeyOut:PHKEY):HRESULT;stdcall;external SHLWAPIDLL name 'AssocQueryKeyW';
  function AssocIsDangerous(pszAssoc:PCWSTR):BOOL;stdcall;external SHLWAPIDLL name 'AssocIsDangerous';
  { these are cdecl varargs }
 function wvnsprintfA(pszDest : PSTR;cchDest : WINT; pszFmt:PCSTR ):integer;  cdecl;varargs; external SHLWAPIDLL name 'wvnsprintfA';
 function wvnsprintfW(pszDest :PWSTR;cchDest : WINT; pszFmt:PCWSTR ):integer; cdecl;varargs; external SHLWAPIDLL name 'wvnsprintfW';
 function wnsprintfA (pszDest : PSTR;cchDest : WINT; pszFmt:PCSTR ):integer;  cdecl;varargs; external SHLWAPIDLL name 'wnsprintfA';
 function wnsprintfW (pszDest :PWSTR;cchDest : WINT; pszFmt:PCWSTR ):integer; cdecl;varargs; external SHLWAPIDLL name 'wnsprintfW';

 function  ParseURLA(pcszURL:LPCSTR; ppu:PPARSEDURLA):HRESULT;stdcall;external SHLWAPIDLL name 'ParseURLA';
 function  ParseURLW(pcszURL:LPCWSTR; ppu:PPARSEDURLW):HRESULT;stdcall;external SHLWAPIDLL name 'ParseURLW';

 function AssocGetPerceivedType(pszExt:PCWSTR; ptype:PPERCEIVED; pflag:PPERCEIVEDFLAG; ppszType:PLPWSTR):HRESULT;stdcall;external SHLWAPIDLL name 'AssocGetPerceivedType';
 function SHOpenRegStreamA(hKey:HKEY; pszSubkey:LPCSTR; pszValue:LPCSTR; grfMode:DWORD):IStream;stdcall;external SHLWAPIDLL name 'SHOpenRegStreamA';
 function SHOpenRegStreamW(hKey:HKEY; pszSubkey:LPCWSTR; pszValue:LPCWSTR; grfMode:DWORD):IStream;stdcall;external SHLWAPIDLL name 'SHOpenRegStreamW';
 function SHOpenRegStream2A(hKey:HKEY; pszSubkey:LPCSTR; pszValue:LPCSTR; grfMode:DWORD):IStream;stdcall;external SHLWAPIDLL name 'SHOpenRegStream2A';
 function SHOpenRegStream2W(hKey:HKEY; pszSubkey:LPCWSTR; pszValue:LPCWSTR; grfMode:DWORD):IStream;stdcall;external SHLWAPIDLL name 'SHOpenRegStream2W';
 function SHCreateStreamOnFileA(pszFile:LPCSTR; grfMode:DWORD; out ppstm:IStream):HRESULT;stdcall;external SHLWAPIDLL name 'SHCreateStreamOnFileA';
 function SHCreateStreamOnFileW(pszFile:LPCWSTR; grfMode:DWORD; out ppstm:IStream):HRESULT;stdcall;external SHLWAPIDLL name 'SHCreateStreamOnFileW';
 function SHCreateStreamOnFileEx(pszFile:LPCWSTR; grfMode:DWORD; dwAttributes:DWORD; fCreate:BOOL; pstmTemplate:IStream;
            out ppstm:IStream):HRESULT;stdcall;external SHLWAPIDLL name 'SHCreateStreamOnFileEx';
(* Const before type ignored *)
 function SHCreateMemStream(pInit:PBYTE; cbInit:UINT):IStream;stdcall;external SHLWAPIDLL name 'SHCreateMemStream';
 function GetAcceptLanguagesA(pszLanguages:LPSTR; pcchLanguages:PDWORD):HRESULT;stdcall;external SHLWAPIDLL name 'GetAcceptLanguagesA';
 function GetAcceptLanguagesW(pszLanguages:LPWSTR;  pcchLanguages:PDWORD):HRESULT;stdcall;external SHLWAPIDLL name 'GetAcceptLanguagesW';
 procedure IUnknown_Set(out ppunk:IUnknown; punk:IUnknown);stdcall;external SHLWAPIDLL name 'IUnknown_Set';
 procedure IUnknown_AtomicRelease(ppunk:Ppointer);stdcall;external SHLWAPIDLL name 'IUnknown_AtomicRelease';

 function IUnknown_GetWindow(punk:IUnknown; phwnd:PHWND):HRESULT;stdcall;external SHLWAPIDLL name 'IUnknown_GetWindow';
 function IUnknown_SetSite( punk:IUnknown; punkSite:IUnknown):HRESULT;stdcall;external SHLWAPIDLL name 'IUnknown_SetSite';
 function IUnknown_GetSite( punk:IUnknown; riid:REFIID; ppv:Ppointer):HRESULT;stdcall;external SHLWAPIDLL name 'IUnknown_GetSite';
 function IUnknown_QueryService(punk:IUnknown; constref guidService:TGUID; riid:REFIID; ppvOut:Ppointer):HRESULT;stdcall;external SHLWAPIDLL name 'IUnknown_QueryService';
 { _COM_Outptr_  }
 function IStream_Read(out pstm:IStream; pv:pointer; cb:ULONG):HRESULT;stdcall;external SHLWAPIDLL name 'IStream_Read';

(* Const before type ignored *)
 function IStream_Write(out pstm:IStream; pv:pointer; cb:ULONG):HRESULT;stdcall;external SHLWAPIDLL name 'IStream_Write';

 function IStream_Reset(out pstm:IStream):HRESULT;stdcall;external SHLWAPIDLL name 'IStream_Reset';

 function IStream_Size(pstm:IStream; pui:PULARGE_INTEGER):HRESULT;stdcall;external SHLWAPIDLL name 'IStream_Size';

 function ConnectToConnectionPoint( punk:IUnknown; riidEvent:REFIID; fConnect:BOOL;  punkTarget:IUnknown; pdwCookie:PDWORD;
            out ppcpOut:IConnectionPoint):HRESULT;stdcall;external SHLWAPIDLL name 'ConnectToConnectionPoint';

 function IStream_ReadPidl(pstm:IStream; ppidlOut:PPIDLIST_RELATIVE):HRESULT;stdcall;external SHLWAPIDLL name 'IStream_ReadPidl';
 function IStream_WritePidl(pstm:IStream; pidlWrite:PCUIDLIST_RELATIVE):HRESULT;stdcall;external SHLWAPIDLL name 'IStream_WritePidl';
 function IStream_ReadStr(pstm:IStream; ppsz:PLPWSTR):HRESULT;stdcall;external SHLWAPIDLL name 'IStream_ReadStr';
 function IStream_WriteStr(pstm:IStream; psz:PCWSTR):HRESULT;stdcall;external SHLWAPIDLL name 'IStream_WriteStr';
 function IStream_Copy(pstmFrom:IStream; pstmTo:IStream; cb:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'IStream_Copy';
 function SHGetViewStatePropertyBag(pidl:PCIDLIST_ABSOLUTE; pszBagName:PCWSTR; dwFlags:DWORD; riid:REFIID; ppv:Ppointer):HRESULT;stdcall;external SHLWAPIDLL name 'SHGetViewStatePropertyBag';

(* Const before type ignored *)
 function SHFormatDateTimeA(pft:PFILETIME; pdwFlags:PDWORD; pszBuf:LPSTR; cchBuf:UINT):longint;stdcall;external SHLWAPIDLL name 'SHFormatDateTimeA';

(* Const before type ignored *)
 function SHFormatDateTimeW(pft:PFILETIME;  pdwFlags:PDWORD; pszBuf:LPWSTR; cchBuf:UINT):longint;stdcall;external SHLWAPIDLL name 'SHFormatDateTimeW';

 { filetime unaligned }
 function SHAnsiToUnicode(pszSrc:PCSTR; pwszDst:PWSTR; cwchBuf:longint):longint;stdcall;external SHLWAPIDLL name 'SHAnsiToUnicode';

 function SHAnsiToAnsi(pszSrc:PCSTR; pszDst:PSTR; cchBuf:longint):longint;stdcall;external SHLWAPIDLL name 'SHAnsiToAnsi';

 function SHUnicodeToAnsi(pwszSrc:PCWSTR; pszDst:PSTR; cchBuf:longint):longint;stdcall;external SHLWAPIDLL name 'SHUnicodeToAnsi';

 function SHUnicodeToUnicode(pwzSrc:PCWSTR; pwzDst:PWSTR; cwchBuf:longint):longint;stdcall;external SHLWAPIDLL name 'SHUnicodeToUnicode';

 function SHMessageBoxCheckA(hwnd:HWND; pszText:LPCSTR; pszCaption:LPCSTR; uType:UINT; iDefault:longint;
            pszRegVal:LPCSTR):longint;stdcall;external SHLWAPIDLL name 'SHMessageBoxCheckA';

 function SHMessageBoxCheckW(hwnd:HWND; pszText:LPCWSTR; pszCaption:LPCWSTR; uType:UINT; iDefault:longint;
            pszRegVal:LPCWSTR):longint;stdcall;external SHLWAPIDLL name 'SHMessageBoxCheckW';

 function SHSendMessageBroadcastA(uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;stdcall;external SHLWAPIDLL name 'SHSendMessageBroadcastA';
 function SHSendMessageBroadcastW(uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;stdcall;external SHLWAPIDLL name 'SHSendMessageBroadcastW';
 function SHStripMneumonicA(pszMenu:LPSTR):AnsiChar;stdcall;external SHLWAPIDLL name 'SHStripMneumonicA';
 function SHStripMneumonicW(pszMenu:LPWSTR):WCHAR;stdcall;external SHLWAPIDLL name 'SHStripMneumonicW';
 function IsOS(dwOS:DWORD):BOOL;stdcall;external SHLWAPIDLL name 'IsOS';
(* Const before type ignored *)
 function SHGlobalCounterGetValue(id:SHGLOBALCOUNTER):longint;stdcall;external SHLWAPIDLL name 'SHGlobalCounterGetValue';

(* Const before type ignored *)
 function SHGlobalCounterIncrement(id:SHGLOBALCOUNTER):longint;stdcall;external SHLWAPIDLL name 'SHGlobalCounterIncrement';

(* Const before type ignored *)
 function SHGlobalCounterDecrement(id:SHGLOBALCOUNTER):longint;stdcall;external SHLWAPIDLL name 'SHGlobalCounterDecrement';

(* Const before type ignored *)
 function SHAllocShared(pvData:pointer; dwSize:DWORD; dwProcessId:DWORD):HANDLE;stdcall;external SHLWAPIDLL name 'SHAllocShared';
 function SHFreeShared(hData:HANDLE; dwProcessId:DWORD):BOOL;stdcall;external SHLWAPIDLL name 'SHFreeShared';
 function SHLockShared(hData:HANDLE; dwProcessId:DWORD):pointer;stdcall;external SHLWAPIDLL name 'SHLockShared';
 function SHUnlockShared(pvData:pointer):BOOL;stdcall;external SHLWAPIDLL name 'SHUnlockShared';
 function WhichPlatform:UINT;stdcall;external SHLWAPIDLL name 'WhichPlatform';
 function SHIsLowMemoryMachine(dwType:DWORD):BOOL;stdcall;external SHLWAPIDLL name 'SHIsLowMemoryMachine';
 function GetMenuPosFromID(hmenu:HMENU; id:UINT):longint;stdcall;external SHLWAPIDLL name 'GetMenuPosFromID';
 function SHGetInverseCMAP(pbMap:PBYTE; cbMap:ULONG):HRESULT;stdcall;external SHLWAPIDLL name 'SHGetInverseCMAP';
 function SHAutoComplete(hwndEdit:HWND; dwFlags:DWORD):HRESULT;stdcall;external SHLWAPIDLL name 'SHAutoComplete';
 function SHCreateThreadRef(pcRef:PLONG; out ppunk:IUnknown):HRESULT;stdcall;external SHLWAPIDLL name 'SHCreateThreadRef';
 function SHSetThreadRef(punk:IUnknown):HRESULT;stdcall;external SHLWAPIDLL name 'SHSetThreadRef';
 function SHGetThreadRef(out ppunk:IUnknown):HRESULT;stdcall;external SHLWAPIDLL name 'SHGetThreadRef';

 { _COM_Outptr_ }
(* Const before type ignored *)
 function SHSkipJunction(pbc:IBindCtx; pclsid:PCLSID):BOOL;stdcall;external SHLWAPIDLL name 'SHSkipJunction';
 function SHCreateThread(pfnThreadProc:LPTHREAD_START_ROUTINE; pData:pointer; flags:SHCT_FLAGS; pfnCallback:LPTHREAD_START_ROUTINE):BOOL;stdcall;external SHLWAPIDLL name 'SHCreateThread';
 function SHCreateThreadWithHandle(pfnThreadProc:LPTHREAD_START_ROUTINE; pData:pointer; flags:SHCT_FLAGS; pfnCallback:LPTHREAD_START_ROUTINE; pHandle:PHANDLE):BOOL;stdcall;external SHLWAPIDLL name 'SHCreateThreadWithHandle';
 procedure SetProcessReference( punk:IUnknown);stdcall;external SHLWAPIDLL name 'SetProcessReference';
 function GetProcessReference(punk:IUnknown):HRESULT;stdcall;external SHLWAPIDLL name 'GetProcessReference';
 {_COM_Outptr_ }
 function SHReleaseThreadRef:HRESULT;stdcall;external SHLWAPIDLL name 'SHReleaseThreadRef';
 { release a CTF_THREAD_REF reference earlier than the return of pfnThreadProc }
 function SHCreateShellPalette(hdc:HDC):HPALETTE;stdcall;external SHLWAPIDLL name 'SHCreateShellPalette';
 procedure ColorRGBToHLS(clrRGB:COLORREF; pwHue:PWORD; pwLuminance:PWORD; pwSaturation:PWORD);stdcall;external SHLWAPIDLL name 'ColorRGBToHLS';
 function ColorHLSToRGB(wHue:WORD; wLuminance:WORD; wSaturation:WORD):COLORREF;stdcall;external SHLWAPIDLL name 'ColorHLSToRGB';
 function ColorAdjustLuma(clrRGB:COLORREF; n:longint; fScale:BOOL):COLORREF;stdcall;external SHLWAPIDLL name 'ColorAdjustLuma';
 function IsInternetESCEnabled:BOOL;stdcall;external SHLWAPIDLL name 'IsInternetESCEnabled';

implementation

end.
