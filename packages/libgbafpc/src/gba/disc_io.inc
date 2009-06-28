{$ifdef GBA_INTERFACE}

const
  FEATURE_MEDIUM_CANREAD		= $00000001;
  FEATURE_MEDIUM_CANWRITE		= $00000002;
  FEATURE_SLOT_GBA			= $00000010;
  FEATURE_SLOT_NDS			= $00000020;


type
  FN_MEDIUM_STARTUP = function: boolean;
  FN_MEDIUM_ISINSERTED = function: boolean;
  FN_MEDIUM_READSECTORS = function(sector, numSectors: cint32; buffer: pointer): boolean;
  FN_MEDIUM_WRITESECTORS = function(sector, numSectors: cint32; const buffer: pointer): boolean;
  FN_MEDIUM_CLEARSTATUS = function: boolean;
  FN_MEDIUM_SHUTDOWN = function: boolean;

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

{$endif GBA_INTERFACE}
