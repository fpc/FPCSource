{$ifdef NDS_INTERFACE}

const
  FIX_ALL = $01;
  FIX_GLUE = $02;
  FIX_GOT = $04;
  FIX_BSS = $08;

  DLDI_MAGIC_STRING_LEN = 8;
  DLDI_FRIENDLY_NAME_LEN = 48;

var
  DLDI_MAGIC_NUMBER: cuint32; cvar; external;

type
  DLDI_INTERFACE = packed record
    magicNumber: cuint32;
    magicString: array [0..DLDI_MAGIC_STRING_LEN-1] of cchar;
    versionNumber: cuint8;
    driverSize: cuint8;
    fixSectionsFlags: cuint8;
    allocatedSize: cuint8;
    
    friendlyName: array [0..DLDI_FRIENDLY_NAME_LEN-1]of cchar;
    
    // Pointers to sections that need address fixing
    dldiStart: pointer;
    dldiEnd: pointer;
    interworkStart: pointer;
    interworkEnd: pointer;
    gotStart: pointer;
    gotEnd: pointer;
    bssStart: pointer;
    bssEnd: pointer;
    // Original I/O interface data
    ioInterface: DISC_INTERFACE;
  end;
  PDLDI_INTERFACE = ^DLDI_INTERFACE;
  TDLDI_INTERFACE = DLDI_INTERFACE;

var
  io_dldi_data: PDLDI_INTERFACE; cvar; external;

function dldiGetInternal(): PDISC_INTERFACE; cdecl; external;

function dldiIsValid(const io: PDLDI_INTERFACE): cbool; cdecl; external;

procedure dldiFixDriverAddresses(io: PDLDI_INTERFACE); cdecl; external;

function dldiLoadFromFile(const path: pcchar): PDLDI_INTERFACE; cdecl; external;

procedure dldiFree(dldi: PDLDI_INTERFACE); cdecl; external;

{$endif NDS_INTERFACE}
