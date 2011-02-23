{$IFDEF OGC_INTERFACE}
{$ifdef HW_RVL}

const
  IOS_EINVAL = - $3004;
  IOS_EBADVERSION = - $3100;
  IOS_ETOOMANYVIEWS = - $3101;
  IOS_EMISMATCH = - $3102;
function __IOS_InitializeSubsystems: cint32; cdecl; external;

function __IOS_ShutdownSubsystems: cint32; cdecl; external;

function __IOS_LoadStartupIOS: cint32; cdecl; external;

function __IOS_LaunchNewIOS(version: cint): cint32; cdecl; external;

function IOS_GetPreferredVersion: cint32; cdecl; external;

function IOS_ReloadIOS(version: cint): cint32; cdecl; external;

function IOS_GetVersion: cint32; cdecl; external;

function IOS_GetRevision: cint32; cdecl; external;

function IOS_GetRevisionMajor: cint32; cdecl; external;

function IOS_GetRevisionMinor: cint32; cdecl; external;


{$endif HW_RVL}
{$ENDIF}
