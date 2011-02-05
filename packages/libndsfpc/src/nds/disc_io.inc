{$ifdef NDS_INTERFACE}

const
  FEATURE_MEDIUM_CANREAD		= $00000001;
  FEATURE_MEDIUM_CANWRITE		= $00000002;
  FEATURE_SLOT_GBA			= $00000010;
  FEATURE_SLOT_NDS			= $00000020;

function DEVICE_TYPE_DSI_SD(): cuint32; inline;

type
  FN_MEDIUM_STARTUP = function: cbool;
  FN_MEDIUM_ISINSERTED = function: cbool;
  FN_MEDIUM_READSECTORS = function(sector, numSectors: cuint32; buffer: pointer): cbool;
  FN_MEDIUM_WRITESECTORS = function(sector, numSectors: cuint32; const buffer: pointer): cbool;
  FN_MEDIUM_CLEARSTATUS = function: cbool;
  FN_MEDIUM_SHUTDOWN = function: cbool;

  DISC_INTERFACE_STRUCT = record
    ioType: cuint32;
    features: cuint32;
    startup: FN_MEDIUM_STARTUP;
    isInserted: FN_MEDIUM_ISINSERTED;
    readSectors: FN_MEDIUM_READSECTORS;
    writeSectors: FN_MEDIUM_WRITESECTORS;
    clearStatus: FN_MEDIUM_CLEARSTATUS;
    shutdown: FN_MEDIUM_SHUTDOWN;
  end;

  DISC_INTERFACE = DISC_INTERFACE_STRUCT;
  TDISC_INTERFACE = DISC_INTERFACE_STRUCT;
  PDISC_INTERFACE = ^DISC_INTERFACE_STRUCT;
(*
  PARTITION_INTERFACE = (
    PI_DEFAULT = 0,
    PI_SLOT_1, 
    PI_SLOT_2, 
    PI_CUSTOM
  );
*)

var
  __io_dsisd: DISC_INTERFACE; cvar; external;

{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}

function DEVICE_TYPE_DSI_SD(): cuint32; inline; 
begin
  DEVICE_TYPE_DSI_SD := ord('i') or (ord('_') shl 8) or (ord('S') shl 16) or (ord('D') shl 24);
end;


{$endif NDS_IMPLEMENTATION}
