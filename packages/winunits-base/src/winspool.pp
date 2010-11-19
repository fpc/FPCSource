unit winspool;

interface

{$PACKRECORDS C}

{$ifndef NO_SMART_LINK}
{$smartlink on}
{$endif}

  uses
    ctypes,windows;
{
  Automatically converted by H2Pas 1.0.0 from winspool.h
  The following command line parameters were used:
    winspool.h
    -D
    -w
}

  const
    External_library='winspool.drv';

  type
    PPVOID = ppointer;

  const
     DI_CHANNEL = 1;
     DI_CHANNEL_WRITE = 2;
     DI_READ_SPOOL_JOB = 3;
     DI_MEMORYMAP_WRITE = $1;
     FORM_USER = $0;
     FORM_BUILTIN = $1;
     FORM_PRINTER = $2;
     DRIVER_KERNELMODE = $00000001;
     DRIVER_USERMODE = $00000002;
     DSPRINT_PUBLISH = $00000001;
     DSPRINT_UPDATE = $00000002;
     DSPRINT_UNPUBLISH = $00000004;
     DSPRINT_REPUBLISH = $00000008;
     DSPRINT_PENDING = $80000000;
     JOB_CONTROL_PAUSE = 1;
     JOB_CONTROL_RESUME = 2;
     JOB_CONTROL_CANCEL = 3;
     JOB_CONTROL_RESTART = 4;
     JOB_CONTROL_DELETE = 5;
     JOB_CONTROL_SENT_TO_PRINTER = 6;
     JOB_CONTROL_LAST_PAGE_EJECTED = 7;
     JOB_STATUS_PAUSED = $1;
     JOB_STATUS_ERROR = $2;
     JOB_STATUS_DELETING = $4;
     JOB_STATUS_SPOOLING = $8;
     JOB_STATUS_PRINTING = $10;
     JOB_STATUS_OFFLINE = $20;
     JOB_STATUS_PAPEROUT = $40;
     JOB_STATUS_PRINTED = $80;
     JOB_STATUS_DELETED = $100;
     JOB_STATUS_BLOCKED_DEVQ = $200;
     JOB_STATUS_USER_INTERVENTION = $400;
     JOB_STATUS_RESTART = $800;
     JOB_STATUS_COMPLETE = $1000;
     JOB_POSITION_UNSPECIFIED = 0;
     JOB_NOTIFY_TYPE = 1;
     JOB_NOTIFY_FIELD_PRINTER_NAME = 0;
     JOB_NOTIFY_FIELD_MACHINE_NAME = 1;
     JOB_NOTIFY_FIELD_PORT_NAME = 2;
     JOB_NOTIFY_FIELD_USER_NAME = 3;
     JOB_NOTIFY_FIELD_NOTIFY_NAME = 4;
     JOB_NOTIFY_FIELD_DATATYPE = 5;
     JOB_NOTIFY_FIELD_PRINT_PROCESSOR = 6;
     JOB_NOTIFY_FIELD_PARAMETERS = 7;
     JOB_NOTIFY_FIELD_DRIVER_NAME = 8;
     JOB_NOTIFY_FIELD_DEVMODE = 9;
     JOB_NOTIFY_FIELD_STATUS = 10;
     JOB_NOTIFY_FIELD_STATUS_STRING = 11;
     JOB_NOTIFY_FIELD_SECURITY_DESCRIPTOR = 12;
     JOB_NOTIFY_FIELD_DOCUMENT = 13;
     JOB_NOTIFY_FIELD_PRIORITY = 14;
     JOB_NOTIFY_FIELD_POSITION = 15;
     JOB_NOTIFY_FIELD_SUBMITTED = 16;
     JOB_NOTIFY_FIELD_START_TIME = 17;
     JOB_NOTIFY_FIELD_UNTIL_TIME = 18;
     JOB_NOTIFY_FIELD_TIME = 19;
     JOB_NOTIFY_FIELD_TOTAL_PAGES = 20;
     JOB_NOTIFY_FIELD_PAGES_PRINTED = 21;
     JOB_NOTIFY_FIELD_TOTAL_BYTES = 22;
     JOB_NOTIFY_FIELD_BYTES_PRINTED = 23;
     JOB_ACCESS_ADMINISTER = 16;
     JOB_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or JOB_ACCESS_ADMINISTER;
     JOB_READ = STANDARD_RIGHTS_READ or JOB_ACCESS_ADMINISTER;
     JOB_WRITE = STANDARD_RIGHTS_WRITE or JOB_ACCESS_ADMINISTER;
     JOB_EXECUTE = STANDARD_RIGHTS_EXECUTE or JOB_ACCESS_ADMINISTER;
     PRINTER_NOTIFY_OPTIONS_REFRESH = 1;
     PRINTER_ACCESS_ADMINISTER = 4;
     PRINTER_ACCESS_USE = 8;
     PRINTER_ERROR_INFORMATION = $80000000;
     PRINTER_ERROR_WARNING = $40000000;
     PRINTER_ERROR_SEVERE = $20000000;
     PRINTER_ERROR_OUTOFPAPER = 1;
     PRINTER_ERROR_JAM = 2;
     PRINTER_ERROR_OUTOFTONER = 4;
     PRINTER_CONTROL_PAUSE = 1;
     PRINTER_CONTROL_RESUME = 2;
     PRINTER_CONTROL_PURGE = 3;
     PRINTER_CONTROL_SET_STATUS = 4;
     PRINTER_STATUS_PAUSED = 1;
     PRINTER_STATUS_ERROR = 2;
     PRINTER_STATUS_PENDING_DELETION = 4;
     PRINTER_STATUS_PAPER_JAM = 8;
     PRINTER_STATUS_PAPER_OUT = $10;
     PRINTER_STATUS_MANUAL_FEED = $20;
     PRINTER_STATUS_PAPER_PROBLEM = $40;
     PRINTER_STATUS_OFFLINE = $80;
     PRINTER_STATUS_IO_ACTIVE = $100;
     PRINTER_STATUS_BUSY = $200;
     PRINTER_STATUS_PRINTING = $400;
     PRINTER_STATUS_OUTPUT_BIN_FULL = $800;
     PRINTER_STATUS_NOT_AVAILABLE = $1000;
     PRINTER_STATUS_WAITING = $2000;
     PRINTER_STATUS_PROCESSING = $4000;
     PRINTER_STATUS_INITIALIZING = $8000;
     PRINTER_STATUS_WARMING_UP = $10000;
     PRINTER_STATUS_TONER_LOW = $20000;
     PRINTER_STATUS_NO_TONER = $40000;
     PRINTER_STATUS_PAGE_PUNT = $80000;
     PRINTER_STATUS_USER_INTERVENTION = $100000;
     PRINTER_STATUS_OUT_OF_MEMORY = $200000;
     PRINTER_STATUS_DOOR_OPEN = $400000;
     PRINTER_STATUS_SERVER_UNKNOWN = $800000;
     PRINTER_STATUS_POWER_SAVE = $1000000;
     PRINTER_ATTRIBUTE_QUEUED = 1;
     PRINTER_ATTRIBUTE_DIRECT = 2;
     PRINTER_ATTRIBUTE_DEFAULT = 4;
     PRINTER_ATTRIBUTE_SHARED = 8;
     PRINTER_ATTRIBUTE_NETWORK = $10;
     PRINTER_ATTRIBUTE_HIDDEN = $20;
     PRINTER_ATTRIBUTE_LOCAL = $40;
     PRINTER_ATTRIBUTE_ENABLE_DEVQ = $80;
     PRINTER_ATTRIBUTE_KEEPPRINTEDJOBS = $100;
     PRINTER_ATTRIBUTE_DO_COMPLETE_FIRST = $200;
     PRINTER_ATTRIBUTE_WORK_OFFLINE = $400;
     PRINTER_ATTRIBUTE_ENABLE_BIDI = $800;
     PRINTER_ATTRIBUTE_RAW_ONLY = $1000;
     PRINTER_ATTRIBUTE_PUBLISHED = $2000;
     PRINTER_ENUM_DEFAULT = 1;
     PRINTER_ENUM_LOCAL = 2;
     PRINTER_ENUM_CONNECTIONS = 4;
     PRINTER_ENUM_FAVORITE = 4;
     PRINTER_ENUM_NAME = 8;
     PRINTER_ENUM_REMOTE = 16;
     PRINTER_ENUM_SHARED = 32;
     PRINTER_ENUM_NETWORK = $40;
     PRINTER_ENUM_EXPAND = $4000;
     PRINTER_ENUM_CONTAINER = $8000;
     PRINTER_ENUM_ICONMASK = $ff0000;
     PRINTER_ENUM_ICON1 = $10000;
     PRINTER_ENUM_ICON2 = $20000;
     PRINTER_ENUM_ICON3 = $40000;
     PRINTER_ENUM_ICON4 = $80000;
     PRINTER_ENUM_ICON5 = $100000;
     PRINTER_ENUM_ICON6 = $200000;
     PRINTER_ENUM_ICON7 = $400000;
     PRINTER_ENUM_ICON8 = $800000;
     PRINTER_NOTIFY_TYPE = 0;
     PRINTER_NOTIFY_FIELD_SERVER_NAME = 0;
     PRINTER_NOTIFY_FIELD_PRINTER_NAME = 1;
     PRINTER_NOTIFY_FIELD_SHARE_NAME = 2;
     PRINTER_NOTIFY_FIELD_PORT_NAME = 3;
     PRINTER_NOTIFY_FIELD_DRIVER_NAME = 4;
     PRINTER_NOTIFY_FIELD_COMMENT = 5;
     PRINTER_NOTIFY_FIELD_LOCATION = 6;
     PRINTER_NOTIFY_FIELD_DEVMODE = 7;
     PRINTER_NOTIFY_FIELD_SEPFILE = 8;
     PRINTER_NOTIFY_FIELD_PRINT_PROCESSOR = 9;
     PRINTER_NOTIFY_FIELD_PARAMETERS = 10;
     PRINTER_NOTIFY_FIELD_DATATYPE = 11;
     PRINTER_NOTIFY_FIELD_SECURITY_DESCRIPTOR = 12;
     PRINTER_NOTIFY_FIELD_ATTRIBUTES = 13;
     PRINTER_NOTIFY_FIELD_PRIORITY = 14;
     PRINTER_NOTIFY_FIELD_DEFAULT_PRIORITY = 15;
     PRINTER_NOTIFY_FIELD_START_TIME = 16;
     PRINTER_NOTIFY_FIELD_UNTIL_TIME = 17;
     PRINTER_NOTIFY_FIELD_STATUS = 18;
     PRINTER_NOTIFY_FIELD_STATUS_STRING = 19;
     PRINTER_NOTIFY_FIELD_CJOBS = 20;
     PRINTER_NOTIFY_FIELD_AVERAGE_PPM = 21;
     PRINTER_NOTIFY_FIELD_TOTAL_PAGES = 22;
     PRINTER_NOTIFY_FIELD_PAGES_PRINTED = 23;
     PRINTER_NOTIFY_FIELD_TOTAL_BYTES = 24;
     PRINTER_NOTIFY_FIELD_BYTES_PRINTED = 25;
     PRINTER_CHANGE_ADD_PRINTER = 1;
     PRINTER_CHANGE_SET_PRINTER = 2;
     PRINTER_CHANGE_DELETE_PRINTER = 4;
     PRINTER_CHANGE_FAILED_CONNECTION_PRINTER = 8;
     PRINTER_CHANGE_PRINTER = $FF;
     PRINTER_CHANGE_ADD_JOB = $100;
     PRINTER_CHANGE_SET_JOB = $200;
     PRINTER_CHANGE_DELETE_JOB = $400;
     PRINTER_CHANGE_WRITE_JOB = $800;
     PRINTER_CHANGE_JOB = $FF00;
     PRINTER_CHANGE_ADD_FORM = $10000;
     PRINTER_CHANGE_SET_FORM = $20000;
     PRINTER_CHANGE_DELETE_FORM = $40000;
     PRINTER_CHANGE_FORM = $70000;
     PRINTER_CHANGE_ADD_PORT = $100000;
     PRINTER_CHANGE_CONFIGURE_PORT = $200000;
     PRINTER_CHANGE_DELETE_PORT = $400000;
     PRINTER_CHANGE_PORT = $700000;
     PRINTER_CHANGE_ADD_PRINT_PROCESSOR = $1000000;
     PRINTER_CHANGE_DELETE_PRINT_PROCESSOR = $4000000;
     PRINTER_CHANGE_PRINT_PROCESSOR = $7000000;
     PRINTER_CHANGE_ADD_PRINTER_DRIVER = $10000000;
     PRINTER_CHANGE_SET_PRINTER_DRIVER = $20000000;
     PRINTER_CHANGE_DELETE_PRINTER_DRIVER = $40000000;
     PRINTER_CHANGE_PRINTER_DRIVER = $70000000;
     PRINTER_CHANGE_TIMEOUT = $80000000;
     PRINTER_CHANGE_ALL = $7777FFFF;
     PRINTER_NOTIFY_INFO_DISCARDED = 1;
     PRINTER_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED or PRINTER_ACCESS_ADMINISTER) or PRINTER_ACCESS_USE;
     PRINTER_READ = STANDARD_RIGHTS_READ or PRINTER_ACCESS_USE;
     PRINTER_WRITE = STANDARD_RIGHTS_WRITE or PRINTER_ACCESS_USE;
     PRINTER_EXECUTE = STANDARD_RIGHTS_EXECUTE or PRINTER_ACCESS_USE;
     NO_PRIORITY = 0;
     MAX_PRIORITY = 99;
     MIN_PRIORITY = 1;
     DEF_PRIORITY = 1;
     PORT_TYPE_WRITE = 1;
     PORT_TYPE_READ = 2;
     PORT_TYPE_REDIRECTED = 4;
     PORT_TYPE_NET_ATTACHED = 8;
     SERVER_ACCESS_ADMINISTER = 1;
     SERVER_ACCESS_ENUMERATE = 2;
     SERVER_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED or SERVER_ACCESS_ADMINISTER) or SERVER_ACCESS_ENUMERATE;
     SERVER_READ = STANDARD_RIGHTS_READ or SERVER_ACCESS_ENUMERATE;
     SERVER_WRITE = (STANDARD_RIGHTS_WRITE or SERVER_ACCESS_ADMINISTER) or SERVER_ACCESS_ENUMERATE;
     SERVER_EXECUTE = STANDARD_RIGHTS_EXECUTE or SERVER_ACCESS_ENUMERATE;
     PORT_STATUS_TYPE_ERROR = 1;
     PORT_STATUS_TYPE_WARNING = 2;
     PORT_STATUS_TYPE_INFO = 3;
     PORT_STATUS_OFFLINE = 1;
     PORT_STATUS_PAPER_JAM = 2;
     PORT_STATUS_PAPER_OUT = 3;
     PORT_STATUS_OUTPUT_BIN_FULL = 4;
     PORT_STATUS_PAPER_PROBLEM = 5;
     PORT_STATUS_NO_TONER = 6;
     PORT_STATUS_DOOR_OPEN = 7;
     PORT_STATUS_USER_INTERVENTION = 8;
     PORT_STATUS_OUT_OF_MEMORY = 9;
     PORT_STATUS_TONER_LOW = 10;
     PORT_STATUS_WARMING_UP = 11;
     PORT_STATUS_POWER_SAVE = 12;

  type

     _ADDJOB_INFO_1A = record
          Path : LPSTR;
          JobId : DWORD;
       end;
     ADDJOB_INFO_1A = _ADDJOB_INFO_1A;
     PADDJOB_INFO_1A = ^_ADDJOB_INFO_1A;
     LPADDJOB_INFO_1A = ^_ADDJOB_INFO_1A;

     _ADDJOB_INFO_1W = record
          Path : LPWSTR;
          JobId : DWORD;
       end;
     ADDJOB_INFO_1W = _ADDJOB_INFO_1W;
     PADDJOB_INFO_1W = ^_ADDJOB_INFO_1W;
     LPADDJOB_INFO_1W = ^_ADDJOB_INFO_1W;

     _DATATYPES_INFO_1A = record
          pName : LPSTR;
       end;
     DATATYPES_INFO_1A = _DATATYPES_INFO_1A;
     PDATATYPES_INFO_1A = ^_DATATYPES_INFO_1A;
     LPDATATYPES_INFO_1A = ^_DATATYPES_INFO_1A;

     _DATATYPES_INFO_1W = record
          pName : LPWSTR;
       end;
     DATATYPES_INFO_1W = _DATATYPES_INFO_1W;
     PDATATYPES_INFO_1W = ^_DATATYPES_INFO_1W;
     LPDATATYPES_INFO_1W = ^_DATATYPES_INFO_1W;

     _JOB_INFO_1A = record
          JobId : DWORD;
          pPrinterName : LPSTR;
          pMachineName : LPSTR;
          pUserName : LPSTR;
          pDocument : LPSTR;
          pDatatype : LPSTR;
          pStatus : LPSTR;
          Status : DWORD;
          Priority : DWORD;
          Position : DWORD;
          TotalPages : DWORD;
          PagesPrinted : DWORD;
          Submitted : SYSTEMTIME;
       end;
     JOB_INFO_1A = _JOB_INFO_1A;
     PJOB_INFO_1A = ^_JOB_INFO_1A;
     LPJOB_INFO_1A = ^_JOB_INFO_1A;

     _JOB_INFO_1W = record
          JobId : DWORD;
          pPrinterName : LPWSTR;
          pMachineName : LPWSTR;
          pUserName : LPWSTR;
          pDocument : LPWSTR;
          pDatatype : LPWSTR;
          pStatus : LPWSTR;
          Status : DWORD;
          Priority : DWORD;
          Position : DWORD;
          TotalPages : DWORD;
          PagesPrinted : DWORD;
          Submitted : SYSTEMTIME;
       end;
     JOB_INFO_1W = _JOB_INFO_1W;
     PJOB_INFO_1W = ^_JOB_INFO_1W;
     LPJOB_INFO_1W = ^_JOB_INFO_1W;

     _JOB_INFO_2A = record
          JobId : DWORD;
          pPrinterName : LPSTR;
          pMachineName : LPSTR;
          pUserName : LPSTR;
          pDocument : LPSTR;
          pNotifyName : LPSTR;
          pDatatype : LPSTR;
          pPrintProcessor : LPSTR;
          pParameters : LPSTR;
          pDriverName : LPSTR;
          pDevMode : LPDEVMODE;
          pStatus : LPSTR;
          pSecurityDescriptor : PSECURITY_DESCRIPTOR;
          Status : DWORD;
          Priority : DWORD;
          Position : DWORD;
          StartTime : DWORD;
          UntilTime : DWORD;
          TotalPages : DWORD;
          Size : DWORD;
          Submitted : SYSTEMTIME;
          Time : DWORD;
          PagesPrinted : DWORD;
       end;
     JOB_INFO_2A = _JOB_INFO_2A;
     PJOB_INFO_2A = ^_JOB_INFO_2A;
     LPJOB_INFO_2A = ^_JOB_INFO_2A;

     _JOB_INFO_2W = record
          JobId : DWORD;
          pPrinterName : LPWSTR;
          pMachineName : LPWSTR;
          pUserName : LPWSTR;
          pDocument : LPWSTR;
          pNotifyName : LPWSTR;
          pDatatype : LPWSTR;
          pPrintProcessor : LPWSTR;
          pParameters : LPWSTR;
          pDriverName : LPWSTR;
          pDevMode : LPDEVMODEW;
          pStatus : LPWSTR;
          pSecurityDescriptor : PSECURITY_DESCRIPTOR;
          Status : DWORD;
          Priority : DWORD;
          Position : DWORD;
          StartTime : DWORD;
          UntilTime : DWORD;
          TotalPages : DWORD;
          Size : DWORD;
          Submitted : SYSTEMTIME;
          Time : DWORD;
          PagesPrinted : DWORD;
       end;
     JOB_INFO_2W = _JOB_INFO_2W;
     PJOB_INFO_2W = ^_JOB_INFO_2W;
     LPJOB_INFO_2W = ^_JOB_INFO_2W;

     _JOB_INFO_3 = record
          JobId : DWORD;
          NextJobId : DWORD;
          Reserved : DWORD;
       end;
     JOB_INFO_3 = _JOB_INFO_3;
     PJOB_INFO_3 = ^_JOB_INFO_3;
     LPJOB_INFO_3 = ^_JOB_INFO_3;

     _DOC_INFO_1A = record
          pDocName : LPSTR;
          pOutputFile : LPSTR;
          pDatatype : LPSTR;
       end;
     DOC_INFO_1A = _DOC_INFO_1A;
     PDOC_INFO_1A = ^_DOC_INFO_1A;
     LPDOC_INFO_1A = ^_DOC_INFO_1A;

     _DOC_INFO_1W = record
          pDocName : LPWSTR;
          pOutputFile : LPWSTR;
          pDatatype : LPWSTR;
       end;
     DOC_INFO_1W = _DOC_INFO_1W;
     PDOC_INFO_1W = ^_DOC_INFO_1W;
     LPDOC_INFO_1W = ^_DOC_INFO_1W;

     _DOC_INFO_2A = record
          pDocName : LPSTR;
          pOutputFile : LPSTR;
          pDatatype : LPSTR;
          dwMode : DWORD;
          JobId : DWORD;
       end;
     DOC_INFO_2A = _DOC_INFO_2A;
     PDOC_INFO_2A = ^_DOC_INFO_2A;
     LPDOC_INFO_2A = ^_DOC_INFO_2A;

     _DOC_INFO_2W = record
          pDocName : LPWSTR;
          pOutputFile : LPWSTR;
          pDatatype : LPWSTR;
          dwMode : DWORD;
          JobId : DWORD;
       end;
     DOC_INFO_2W = _DOC_INFO_2W;
     PDOC_INFO_2W = ^_DOC_INFO_2W;
     LPDOC_INFO_2W = ^_DOC_INFO_2W;

     _DRIVER_INFO_1A = record
          pName : LPSTR;
       end;
     DRIVER_INFO_1A = _DRIVER_INFO_1A;
     PDRIVER_INFO_1A = ^_DRIVER_INFO_1A;
     LPDRIVER_INFO_1A = ^_DRIVER_INFO_1A;

     _DRIVER_INFO_1W = record
          pName : LPWSTR;
       end;
     DRIVER_INFO_1W = _DRIVER_INFO_1W;
     PDRIVER_INFO_1W = ^_DRIVER_INFO_1W;
     LPDRIVER_INFO_1W = ^_DRIVER_INFO_1W;

     _DRIVER_INFO_2A = record
          cVersion : DWORD;
          pName : LPSTR;
          pEnvironment : LPSTR;
          pDriverPath : LPSTR;
          pDataFile : LPSTR;
          pConfigFile : LPSTR;
       end;
     DRIVER_INFO_2A = _DRIVER_INFO_2A;
     PDRIVER_INFO_2A = ^_DRIVER_INFO_2A;
     LPDRIVER_INFO_2A = ^_DRIVER_INFO_2A;

     _DRIVER_INFO_2W = record
          cVersion : DWORD;
          pName : LPWSTR;
          pEnvironment : LPWSTR;
          pDriverPath : LPWSTR;
          pDataFile : LPWSTR;
          pConfigFile : LPWSTR;
       end;
     DRIVER_INFO_2W = _DRIVER_INFO_2W;
     PDRIVER_INFO_2W = ^_DRIVER_INFO_2W;
     LPDRIVER_INFO_2W = ^_DRIVER_INFO_2W;

     _DRIVER_INFO_3A = record
          cVersion : DWORD;
          pName : LPSTR;
          pEnvironment : LPSTR;
          pDriverPath : LPSTR;
          pDataFile : LPSTR;
          pConfigFile : LPSTR;
          pHelpFile : LPSTR;
          pDependentFiles : LPSTR;
          pMonitorName : LPSTR;
          pDefaultDataType : LPSTR;
       end;
     DRIVER_INFO_3A = _DRIVER_INFO_3A;
     PDRIVER_INFO_3A = ^_DRIVER_INFO_3A;
     LPDRIVER_INFO_3A = ^_DRIVER_INFO_3A;

     _DRIVER_INFO_3W = record
          cVersion : DWORD;
          pName : LPWSTR;
          pEnvironment : LPWSTR;
          pDriverPath : LPWSTR;
          pDataFile : LPWSTR;
          pConfigFile : LPWSTR;
          pHelpFile : LPWSTR;
          pDependentFiles : LPWSTR;
          pMonitorName : LPWSTR;
          pDefaultDataType : LPWSTR;
       end;
     DRIVER_INFO_3W = _DRIVER_INFO_3W;
     PDRIVER_INFO_3W = ^_DRIVER_INFO_3W;
     LPDRIVER_INFO_3W = ^_DRIVER_INFO_3W;

     _DRIVER_INFO_4A = record
          cVersion : DWORD;
          pName : LPSTR;
          pEnvironment : LPSTR;
          pDriverPath : LPSTR;
          pDataFile : LPSTR;
          pConfigFile : LPSTR;
          pHelpFile : LPSTR;
          pDependentFiles : LPSTR;
          pMonitorName : LPSTR;
          pDefaultDataType : LPSTR;
          pszzPreviousNames : LPSTR;
       end;
     DRIVER_INFO_4A = _DRIVER_INFO_4A;
     PDRIVER_INFO_4A = ^_DRIVER_INFO_4A;
     LPDRIVER_INFO_4A = ^_DRIVER_INFO_4A;

     _DRIVER_INFO_4W = record
          cVersion : DWORD;
          pName : LPWSTR;
          pEnvironment : LPWSTR;
          pDriverPath : LPWSTR;
          pDataFile : LPWSTR;
          pConfigFile : LPWSTR;
          pHelpFile : LPWSTR;
          pDependentFiles : LPWSTR;
          pMonitorName : LPWSTR;
          pDefaultDataType : LPWSTR;
          pszzPreviousNames : LPWSTR;
       end;
     DRIVER_INFO_4W = _DRIVER_INFO_4W;
     PDRIVER_INFO_4W = ^_DRIVER_INFO_4W;
     LPDRIVER_INFO_4W = ^_DRIVER_INFO_4W;

     _DRIVER_INFO_5A = record
          cVersion : DWORD;
          pName : LPSTR;
          pEnvironment : LPSTR;
          pDriverPath : LPSTR;
          pDataFile : LPSTR;
          pConfigFile : LPSTR;
          dwDriverAttributes : DWORD;
          dwConfigVersion : DWORD;
          dwDriverVersion : DWORD;
       end;
     DRIVER_INFO_5A = _DRIVER_INFO_5A;
     PDRIVER_INFO_5A = ^_DRIVER_INFO_5A;
     LPDRIVER_INFO_5A = ^_DRIVER_INFO_5A;

     _DRIVER_INFO_5W = record
          cVersion : DWORD;
          pName : LPWSTR;
          pEnvironment : LPWSTR;
          pDriverPath : LPWSTR;
          pDataFile : LPWSTR;
          pConfigFile : LPWSTR;
          dwDriverAttributes : DWORD;
          dwConfigVersion : DWORD;
          dwDriverVersion : DWORD;
       end;
     DRIVER_INFO_5W = _DRIVER_INFO_5W;
     PDRIVER_INFO_5W = ^_DRIVER_INFO_5W;
     LPDRIVER_INFO_5W = ^_DRIVER_INFO_5W;

     _DRIVER_INFO_6A = record
          cVersion : DWORD;
          pName : LPSTR;
          pEnvironment : LPSTR;
          pDriverPath : LPSTR;
          pDataFile : LPSTR;
          pConfigFile : LPSTR;
          pHelpFile : LPSTR;
          pDependentFiles : LPSTR;
          pMonitorName : LPSTR;
          pDefaultDataType : LPSTR;
          pszzPreviousNames : LPSTR;
          ftDriverDate : FILETIME;
          dwlDriverVersion : DWORDLONG;
          pszMfgName : LPSTR;
          pszOEMUrl : LPSTR;
          pszHardwareID : LPSTR;
          pszProvider : LPSTR;
       end;
     DRIVER_INFO_6A = _DRIVER_INFO_6A;
     PDRIVER_INFO_6A = ^_DRIVER_INFO_6A;
     LPDRIVER_INFO_6A = ^_DRIVER_INFO_6A;

     _DRIVER_INFO_6W = record
          cVersion : DWORD;
          pName : LPWSTR;
          pEnvironment : LPWSTR;
          pDriverPath : LPWSTR;
          pDataFile : LPWSTR;
          pConfigFile : LPWSTR;
          pHelpFile : LPWSTR;
          pDependentFiles : LPWSTR;
          pMonitorName : LPWSTR;
          pDefaultDataType : LPWSTR;
          pszzPreviousNames : LPWSTR;
          ftDriverDate : FILETIME;
          dwlDriverVersion : DWORDLONG;
          pszMfgName : LPWSTR;
          pszOEMUrl : LPWSTR;
          pszHardwareID : LPWSTR;
          pszProvider : LPWSTR;
       end;
     DRIVER_INFO_6W = _DRIVER_INFO_6W;
     PDRIVER_INFO_6W = ^_DRIVER_INFO_6W;
     LPDRIVER_INFO_6W = ^_DRIVER_INFO_6W;

     _MONITOR_INFO_1A = record
          pName : LPSTR;
       end;
     MONITOR_INFO_1A = _MONITOR_INFO_1A;
     PMONITOR_INFO_1A = ^_MONITOR_INFO_1A;
     LPMONITOR_INFO_1A = ^_MONITOR_INFO_1A;

     _MONITOR_INFO_1W = record
          pName : LPWSTR;
       end;
     MONITOR_INFO_1W = _MONITOR_INFO_1W;
     PMONITOR_INFO_1W = ^_MONITOR_INFO_1W;
     LPMONITOR_INFO_1W = ^_MONITOR_INFO_1W;

     _PORT_INFO_1A = record
          pName : LPSTR;
       end;
     PORT_INFO_1A = _PORT_INFO_1A;
     PPORT_INFO_1A = ^_PORT_INFO_1A;
     LPPORT_INFO_1A = ^_PORT_INFO_1A;

     _PORT_INFO_1W = record
          pName : LPWSTR;
       end;
     PORT_INFO_1W = _PORT_INFO_1W;
     PPORT_INFO_1W = ^_PORT_INFO_1W;
     LPPORT_INFO_1W = ^_PORT_INFO_1W;

     _MONITOR_INFO_2A = record
          pName : LPSTR;
          pEnvironment : LPSTR;
          pDLLName : LPSTR;
       end;
     MONITOR_INFO_2A = _MONITOR_INFO_2A;
     PMONITOR_INFO_2A = ^_MONITOR_INFO_2A;
     LPMONITOR_INFO_2A = ^_MONITOR_INFO_2A;

     _MONITOR_INFO_2W = record
          pName : LPWSTR;
          pEnvironment : LPWSTR;
          pDLLName : LPWSTR;
       end;
     MONITOR_INFO_2W = _MONITOR_INFO_2W;
     PMONITOR_INFO_2W = ^_MONITOR_INFO_2W;
     LPMONITOR_INFO_2W = ^_MONITOR_INFO_2W;

     _PORT_INFO_2A = record
          pPortName : LPSTR;
          pMonitorName : LPSTR;
          pDescription : LPSTR;
          fPortType : DWORD;
          Reserved : DWORD;
       end;
     PORT_INFO_2A = _PORT_INFO_2A;
     PPORT_INFO_2A = ^_PORT_INFO_2A;
     LPPORT_INFO_2A = ^_PORT_INFO_2A;

     _PORT_INFO_2W = record
          pPortName : LPWSTR;
          pMonitorName : LPWSTR;
          pDescription : LPWSTR;
          fPortType : DWORD;
          Reserved : DWORD;
       end;
     PORT_INFO_2W = _PORT_INFO_2W;
     PPORT_INFO_2W = ^_PORT_INFO_2W;
     LPPORT_INFO_2W = ^_PORT_INFO_2W;

     _PORT_INFO_3A = record
          dwStatus : DWORD;
          pszStatus : LPSTR;
          dwSeverity : DWORD;
       end;
     PORT_INFO_3A = _PORT_INFO_3A;
     PPORT_INFO_3A = ^_PORT_INFO_3A;
     LPPORT_INFO_3A = ^_PORT_INFO_3A;

     _PORT_INFO_3W = record
          dwStatus : DWORD;
          pszStatus : LPWSTR;
          dwSeverity : DWORD;
       end;
     PORT_INFO_3W = _PORT_INFO_3W;
     PPORT_INFO_3W = ^_PORT_INFO_3W;
     LPPORT_INFO_3W = ^_PORT_INFO_3W;

     _PRINTER_ENUM_VALUESA = record
          pValueName : LPSTR;
          cbValueName : DWORD;
          dwType : DWORD;
          pData : LPBYTE;
          cbData : DWORD;
       end;
     PRINTER_ENUM_VALUESA = _PRINTER_ENUM_VALUESA;
     PPRINTER_ENUM_VALUESA = ^_PRINTER_ENUM_VALUESA;
     LPRINTER_ENUM_VALUESA = ^_PRINTER_ENUM_VALUESA;

     _PRINTER_ENUM_VALUESW = record
          pValueName : LPWSTR;
          cbValueName : DWORD;
          dwType : DWORD;
          pData : LPBYTE;
          cbData : DWORD;
       end;
     PRINTER_ENUM_VALUESW = _PRINTER_ENUM_VALUESW;
     PPRINTER_ENUM_VALUESW = ^_PRINTER_ENUM_VALUESW;
     LPRINTER_ENUM_VALUESW = ^_PRINTER_ENUM_VALUESW;

     _PRINTER_INFO_1A = record
          Flags : DWORD;
          pDescription : LPSTR;
          pName : LPSTR;
          pComment : LPSTR;
       end;
     PRINTER_INFO_1A = _PRINTER_INFO_1A;
     PPRINTER_INFO_1A = ^_PRINTER_INFO_1A;
     LPPRINTER_INFO_1A = ^_PRINTER_INFO_1A;

     _PRINTER_INFO_1W = record
          Flags : DWORD;
          pDescription : LPWSTR;
          pName : LPWSTR;
          pComment : LPWSTR;
       end;
     PRINTER_INFO_1W = _PRINTER_INFO_1W;
     PPRINTER_INFO_1W = ^_PRINTER_INFO_1W;
     LPPRINTER_INFO_1W = ^_PRINTER_INFO_1W;

     _PRINTER_INFO_2A = record
          pServerName : LPSTR;
          pPrinterName : LPSTR;
          pShareName : LPSTR;
          pPortName : LPSTR;
          pDriverName : LPSTR;
          pComment : LPSTR;
          pLocation : LPSTR;
          pDevMode : LPDEVMODE;
          pSepFile : LPSTR;
          pPrintProcessor : LPSTR;
          pDatatype : LPSTR;
          pParameters : LPSTR;
          pSecurityDescriptor : PSECURITY_DESCRIPTOR;
          Attributes : DWORD;
          Priority : DWORD;
          DefaultPriority : DWORD;
          StartTime : DWORD;
          UntilTime : DWORD;
          Status : DWORD;
          cJobs : DWORD;
          AveragePPM : DWORD;
       end;
     PRINTER_INFO_2A = _PRINTER_INFO_2A;
     PPRINTER_INFO_2A = ^_PRINTER_INFO_2A;
     LPPRINTER_INFO_2A = ^_PRINTER_INFO_2A;

     _PRINTER_INFO_2W = record
          pServerName : LPWSTR;
          pPrinterName : LPWSTR;
          pShareName : LPWSTR;
          pPortName : LPWSTR;
          pDriverName : LPWSTR;
          pComment : LPWSTR;
          pLocation : LPWSTR;
          pDevMode : LPDEVMODEW;
          pSepFile : LPWSTR;
          pPrintProcessor : LPWSTR;
          pDatatype : LPWSTR;
          pParameters : LPWSTR;
          pSecurityDescriptor : PSECURITY_DESCRIPTOR;
          Attributes : DWORD;
          Priority : DWORD;
          DefaultPriority : DWORD;
          StartTime : DWORD;
          UntilTime : DWORD;
          Status : DWORD;
          cJobs : DWORD;
          AveragePPM : DWORD;
       end;
     PRINTER_INFO_2W = _PRINTER_INFO_2W;
     PPRINTER_INFO_2W = ^_PRINTER_INFO_2W;
     LPPRINTER_INFO_2W = ^_PRINTER_INFO_2W;

     _PRINTER_INFO_3 = record
          pSecurityDescriptor : PSECURITY_DESCRIPTOR;
       end;
     PRINTER_INFO_3 = _PRINTER_INFO_3;
     PPRINTER_INFO_3 = ^_PRINTER_INFO_3;
     LPPRINTER_INFO_3 = ^_PRINTER_INFO_3;

     _PRINTER_INFO_4A = record
          pPrinterName : LPSTR;
          pServerName : LPSTR;
          Attributes : DWORD;
       end;
     PRINTER_INFO_4A = _PRINTER_INFO_4A;
     PPRINTER_INFO_4A = ^_PRINTER_INFO_4A;
     LPPRINTER_INFO_4A = ^_PRINTER_INFO_4A;

     _PRINTER_INFO_4W = record
          pPrinterName : LPWSTR;
          pServerName : LPWSTR;
          Attributes : DWORD;
       end;
     PRINTER_INFO_4W = _PRINTER_INFO_4W;
     PPRINTER_INFO_4W = ^_PRINTER_INFO_4W;
     LPPRINTER_INFO_4W = ^_PRINTER_INFO_4W;

     _PRINTER_INFO_5A = record
          pPrinterName : LPSTR;
          pPortName : LPSTR;
          Attributes : DWORD;
          DeviceNotSelectedTimeout : DWORD;
          TransmissionRetryTimeout : DWORD;
       end;
     PRINTER_INFO_5A = _PRINTER_INFO_5A;
     PPRINTER_INFO_5A = ^_PRINTER_INFO_5A;
     LPPRINTER_INFO_5A = ^_PRINTER_INFO_5A;

     _PRINTER_INFO_5W = record
          pPrinterName : LPWSTR;
          pPortName : LPWSTR;
          Attributes : DWORD;
          DeviceNotSelectedTimeout : DWORD;
          TransmissionRetryTimeout : DWORD;
       end;
     PRINTER_INFO_5W = _PRINTER_INFO_5W;
     PPRINTER_INFO_5W = ^_PRINTER_INFO_5W;
     LPPRINTER_INFO_5W = ^_PRINTER_INFO_5W;

     _PRINTER_INFO_6 = record
          dwStatus : DWORD;
       end;
     PRINTER_INFO_6 = _PRINTER_INFO_6;
     PPRINTER_INFO_6 = ^_PRINTER_INFO_6;
     LPPRINTER_INFO_6 = ^_PRINTER_INFO_6;

     _PRINTER_INFO_7A = record
          pszObjectGUID : LPWSTR;
          dwAction : DWORD;
       end;
     PRINTER_INFO_7A = _PRINTER_INFO_7A;
     PPRINTER_INFO_7A = ^_PRINTER_INFO_7A;
     LPPRINTER_INFO_7A = ^_PRINTER_INFO_7A;

     _PRINTER_INFO_7W = record
          pszObjectGUID : LPWSTR;
          dwAction : DWORD;
       end;
     PRINTER_INFO_7W = _PRINTER_INFO_7W;
     PPRINTER_INFO_7W = ^_PRINTER_INFO_7W;
     LPPRINTER_INFO_7W = ^_PRINTER_INFO_7W;

     _PRINTER_INFO_8 = record
          pDevMode : LPDEVMODE;
       end;
     PRINTER_INFO_8 = _PRINTER_INFO_8;
     PPRINTER_INFO_8 = ^_PRINTER_INFO_8;
     LPPRINTER_INFO_8 = ^_PRINTER_INFO_8;

     _PRINTER_INFO_9 = record
          pDevMode : LPDEVMODE;
       end;
     PRINTER_INFO_9 = _PRINTER_INFO_9;
     PPRINTER_INFO_9 = ^_PRINTER_INFO_9;
     LPPRINTER_INFO_9 = ^_PRINTER_INFO_9;

     _PRINTPROCESSOR_INFO_1A = record
          pName : LPSTR;
       end;
     PRINTPROCESSOR_INFO_1A = _PRINTPROCESSOR_INFO_1A;
     PPRINTPROCESSOR_INFO_1A = ^_PRINTPROCESSOR_INFO_1A;
     LPPRINTPROCESSOR_INFO_1A = ^_PRINTPROCESSOR_INFO_1A;

     _PRINTPROCESSOR_INFO_1W = record
          pName : LPWSTR;
       end;
     PRINTPROCESSOR_INFO_1W = _PRINTPROCESSOR_INFO_1W;
     PPRINTPROCESSOR_INFO_1W = ^_PRINTPROCESSOR_INFO_1W;
     LPPRINTPROCESSOR_INFO_1W = ^_PRINTPROCESSOR_INFO_1W;

     _PRINTER_NOTIFY_INFO_DATA = record
          _Type : WORD;
          Field : WORD;
          Reserved : DWORD;
          Id : DWORD;
          NotifyData : record
              case longint of
                 0 : ( adwData : array[0..1] of DWORD );
                 1 : ( Data : record
                      cbBuf : DWORD;
                      pBuf : PVOID;
                   end );
              end;
       end;
     PRINTER_NOTIFY_INFO_DATA = _PRINTER_NOTIFY_INFO_DATA;
     PPRINTER_NOTIFY_INFO_DATA = ^_PRINTER_NOTIFY_INFO_DATA;
     LPPRINTER_NOTIFY_INFO_DATA = ^_PRINTER_NOTIFY_INFO_DATA;

     _PRINTER_NOTIFY_INFO = record
          Version : DWORD;
          Flags : DWORD;
          Count : DWORD;
          aData : array[0..0] of PRINTER_NOTIFY_INFO_DATA;
       end;
     PRINTER_NOTIFY_INFO = _PRINTER_NOTIFY_INFO;
     PPRINTER_NOTIFY_INFO = ^_PRINTER_NOTIFY_INFO;
     LPPRINTER_NOTIFY_INFO = ^_PRINTER_NOTIFY_INFO;

     _FORM_INFO_1A = record
          Flags : DWORD;
          pName : LPSTR;
          Size : SIZEL;
          ImageableArea : RECTL;
       end;
     FORM_INFO_1A = _FORM_INFO_1A;
     PFORM_INFO_1A = ^_FORM_INFO_1A;
     LPFORM_INFO_1A = ^_FORM_INFO_1A;

     _FORM_INFO_1W = record
          Flags : DWORD;
          pName : LPWSTR;
          Size : SIZEL;
          ImageableArea : RECTL;
       end;
     FORM_INFO_1W = _FORM_INFO_1W;
     PFORM_INFO_1W = ^_FORM_INFO_1W;
     LPFORM_INFO_1W = ^_FORM_INFO_1W;

     _PRINTER_DEFAULTSA = record
          pDatatype : LPSTR;
          pDevMode : LPDEVMODE;
          DesiredAccess : ACCESS_MASK;
       end;
     PRINTER_DEFAULTSA = _PRINTER_DEFAULTSA;
     PPRINTER_DEFAULTSA = ^_PRINTER_DEFAULTSA;
     LPPRINTER_DEFAULTSA = ^_PRINTER_DEFAULTSA;

     _PRINTER_DEFAULTSW = record
          pDatatype : LPWSTR;
          pDevMode : LPDEVMODE;
          DesiredAccess : ACCESS_MASK;
       end;
     PRINTER_DEFAULTSW = _PRINTER_DEFAULTSW;
     PPRINTER_DEFAULTSW = ^_PRINTER_DEFAULTSW;
     LPPRINTER_DEFAULTSW = ^_PRINTER_DEFAULTSW;

     _PRINTPROCESSOR_CAPS_1 = record
          dwLevel : DWORD;
          dwNupOptions : DWORD;
          dwPageOrderFlags : DWORD;
          dwNumberOfCopies : DWORD;
       end;
     PRINTPROCESSOR_CAPS_1 = _PRINTPROCESSOR_CAPS_1;
     PPRINTPROCESSOR_CAPS_1 = ^_PRINTPROCESSOR_CAPS_1;
     LPPRINTPROCESSOR_CAPS_1 = ^_PRINTPROCESSOR_CAPS_1;

     _PROVIDOR_INFO_1A = record
          pName : LPSTR;
          pEnvironment : LPSTR;
          pDLLName : LPSTR;
       end;
     PROVIDOR_INFO_1A = _PROVIDOR_INFO_1A;
     PPROVIDOR_INFO_1A = ^_PROVIDOR_INFO_1A;
     LPPROVIDOR_INFO_1A = ^_PROVIDOR_INFO_1A;

     _PROVIDOR_INFO_1W = record
          pName : LPWSTR;
          pEnvironment : LPWSTR;
          pDLLName : LPWSTR;
       end;
     PROVIDOR_INFO_1W = _PROVIDOR_INFO_1W;
     PPROVIDOR_INFO_1W = ^_PROVIDOR_INFO_1W;
     LPPROVIDOR_INFO_1W = ^_PROVIDOR_INFO_1W;

     _PROVIDOR_INFO_2A = record
          pOrder : LPSTR;
       end;
     PROVIDOR_INFO_2A = _PROVIDOR_INFO_2A;
     PPROVIDOR_INFO_2A = ^_PROVIDOR_INFO_2A;
     LPROVIDOR_INFO_2A = ^_PROVIDOR_INFO_2A;

     _PROVIDOR_INFO_2W = record
          pOrder : LPWSTR;
       end;
     PROVIDOR_INFO_2W = _PROVIDOR_INFO_2W;
     PPROVIDOR_INFO_2W = ^_PROVIDOR_INFO_2W;
     LPROVIDOR_INFO_2W = ^_PROVIDOR_INFO_2W;

  function AbortPrinter(_para1:HANDLE):BOOL;stdcall; external External_library name 'AbortPrinter';

  function AddForm(_para1:HANDLE; _para2:DWORD; _para3:PBYTE):BOOL;stdcall; external External_library name 'AddFormA';

  function AddFormA(_para1:HANDLE; _para2:DWORD; _para3:PBYTE):BOOL;stdcall; external External_library name 'AddFormA';

  function AddFormW(_para1:HANDLE; _para2:DWORD; _para3:PBYTE):BOOL;stdcall; external External_library name 'AddFormW';

  function AddJob(_para1:HANDLE; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD):BOOL;stdcall; external External_library name 'AddJobA';

  function AddJobA(_para1:HANDLE; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD):BOOL;stdcall; external External_library name 'AddJobA';

  function AddJobW(_para1:HANDLE; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD):BOOL;stdcall; external External_library name 'AddJobW';

  function AddMonitor(_para1:LPSTR; _para2:DWORD; _para3:PBYTE):BOOL;stdcall; external External_library name 'AddMonitorA';

  function AddMonitorA(_para1:LPSTR; _para2:DWORD; _para3:PBYTE):BOOL;stdcall; external External_library name 'AddMonitorA';

  function AddMonitorW(_para1:LPWSTR; _para2:DWORD; _para3:PBYTE):BOOL;stdcall; external External_library name 'AddMonitorW';

  function AddPort(_para1:LPSTR; _para2:HWND; _para3:LPSTR):BOOL;stdcall; external External_library name 'AddPortA';

  function AddPortA(_para1:LPSTR; _para2:HWND; _para3:LPSTR):BOOL;stdcall; external External_library name 'AddPortA';

  function AddPortW(_para1:LPWSTR; _para2:HWND; _para3:LPWSTR):BOOL;stdcall; external External_library name 'AddPortW';

  function AddPrinter(_para1:LPSTR; _para2:DWORD; _para3:PBYTE):HANDLE;stdcall; external External_library name 'AddPrinterA';

  function AddPrinterA(_para1:LPSTR; _para2:DWORD; _para3:PBYTE):HANDLE;stdcall; external External_library name 'AddPrinterA';

  function AddPrinterW(_para1:LPWSTR; _para2:DWORD; _para3:PBYTE):HANDLE;stdcall; external External_library name 'AddPrinterW';

  function AddPrinterConnection(_para1:LPSTR):BOOL;stdcall; external External_library name 'AddPrinterConnectionA';

  function AddPrinterConnectionA(_para1:LPSTR):BOOL;stdcall; external External_library name 'AddPrinterConnectionA';

  function AddPrinterConnectionW(_para1:LPWSTR):BOOL;stdcall; external External_library name 'AddPrinterConnectionW';

  function AddPrinterDriver(_para1:LPSTR; _para2:DWORD; _para3:PBYTE):BOOL;stdcall; external External_library name 'AddPrinterDriverA';

  function AddPrinterDriverA(_para1:LPSTR; _para2:DWORD; _para3:PBYTE):BOOL;stdcall; external External_library name 'AddPrinterDriverA';

  function AddPrinterDriverW(_para1:LPWSTR; _para2:DWORD; _para3:PBYTE):BOOL;stdcall; external External_library name 'AddPrinterDriverW';

  function AddPrintProcessor(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR; _para4:LPSTR):BOOL;stdcall; external External_library name 'AddPrintProcessorA';

  function AddPrintProcessorA(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR; _para4:LPSTR):BOOL;stdcall; external External_library name 'AddPrintProcessorA';

  function AddPrintProcessorW(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR; _para4:LPWSTR):BOOL;stdcall; external External_library name 'AddPrintProcessorW';

  function AddPrintProvidor(_para1:LPSTR; _para2:DWORD; _para3:PBYTE):BOOL;stdcall; external External_library name 'AddPrintProvidorA';

  function AddPrintProvidorA(_para1:LPSTR; _para2:DWORD; _para3:PBYTE):BOOL;stdcall; external External_library name 'AddPrintProvidorA';

  function AddPrintProvidorW(_para1:LPWSTR; _para2:DWORD; _para3:PBYTE):BOOL;stdcall; external External_library name 'AddPrintProvidorW';

  function AdvancedDocumentProperties(_para1:HWND; _para2:HANDLE; _para3:LPSTR; _para4:PDEVMODE; _para5:PDEVMODE):LONG;stdcall; external External_library name 'AdvancedDocumentPropertiesA';

  function AdvancedDocumentPropertiesA(_para1:HWND; _para2:HANDLE; _para3:LPSTR; _para4:PDEVMODE; _para5:PDEVMODE):LONG;stdcall; external External_library name 'AdvancedDocumentPropertiesA';

  function AdvancedDocumentProperties(_para1:HWND; _para2:HANDLE; _para3:LPWSTR; _para4:PDEVMODE; _para5:PDEVMODEW):LONG;stdcall; external External_library name 'AdvancedDocumentPropertiesW';

  function AdvancedDocumentPropertiesW(_para1:HWND; _para2:HANDLE; _para3:LPWSTR; _para4:PDEVMODE; _para5:PDEVMODEW):LONG;stdcall; external External_library name 'AdvancedDocumentPropertiesW';

  function ClosePrinter(_para1:HANDLE):BOOL;stdcall; external External_library name 'ClosePrinter';

  function ConfigurePort(_para1:LPSTR; _para2:HWND; _para3:LPSTR):BOOL;stdcall; external External_library name 'ConfigurePortA';

  function ConfigurePort(_para1:LPWSTR; _para2:HWND; _para3:LPWSTR):BOOL;stdcall; external External_library name 'ConfigurePortW';

  function ConfigurePortA(_para1:LPSTR; _para2:HWND; _para3:LPSTR):BOOL;stdcall; external External_library name 'ConfigurePortA';

  function ConfigurePortW(_para1:LPWSTR; _para2:HWND; _para3:LPWSTR):BOOL;stdcall; external External_library name 'ConfigurePortW';

  function ConnectToPrinterDlg(_para1:HWND; _para2:DWORD):HANDLE;stdcall; external External_library name 'ConnectToPrinterDlg';

  function DeleteForm(_para1:HANDLE; _para2:LPSTR):BOOL;stdcall; external External_library name 'DeleteFormA';

  function DeleteForm(_para1:HANDLE; _para2:LPWSTR):BOOL;stdcall; external External_library name 'DeleteFormW';

  function DeleteFormA(_para1:HANDLE; _para2:LPSTR):BOOL;stdcall; external External_library name 'DeleteFormA';

  function DeleteFormW(_para1:HANDLE; _para2:LPWSTR):BOOL;stdcall; external External_library name 'DeleteFormW';

  function DeleteMonitor(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR):BOOL;stdcall; external External_library name 'DeleteMonitorA';

  function DeleteMonitor(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR):BOOL;stdcall; external External_library name 'DeleteMonitorW';

  function DeleteMonitorA(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR):BOOL;stdcall; external External_library name 'DeleteMonitorA';

  function DeleteMonitorW(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR):BOOL;stdcall; external External_library name 'DeleteMonitorW';

  function DeletePort(_para1:LPSTR; _para2:HWND; _para3:LPSTR):BOOL;stdcall; external External_library name 'DeletePortA';

  function DeletePort(_para1:LPWSTR; _para2:HWND; _para3:LPWSTR):BOOL;stdcall; external External_library name 'DeletePortW';

  function DeletePortA(_para1:LPSTR; _para2:HWND; _para3:LPSTR):BOOL;stdcall; external External_library name 'DeletePortA';

  function DeletePortW(_para1:LPWSTR; _para2:HWND; _para3:LPWSTR):BOOL;stdcall; external External_library name 'DeletePortW';

  function DeletePrinter(_para1:HANDLE):BOOL;stdcall; external External_library name 'DeletePrinter';

  function DeletePrinterConnection(_para1:LPSTR):BOOL;stdcall; external External_library name 'DeletePrinterConnectionA';

  function DeletePrinterConnection(_para1:LPWSTR):BOOL;stdcall; external External_library name 'DeletePrinterConnectionW';

  function DeletePrinterConnectionA(_para1:LPSTR):BOOL;stdcall; external External_library name 'DeletePrinterConnectionA';

  function DeletePrinterConnectionW(_para1:LPWSTR):BOOL;stdcall; external External_library name 'DeletePrinterConnectionW';

  function DeletePrinterData(_para1:HANDLE; _para2:LPSTR):DWORD;stdcall; external External_library name 'DeletePrinterDataA';

  function DeletePrinterData(_para1:HANDLE; _para2:LPWSTR):DWORD;stdcall; external External_library name 'DeletePrinterDataW';

  function DeletePrinterDataA(_para1:HANDLE; _para2:LPSTR):DWORD;stdcall; external External_library name 'DeletePrinterDataA';

  function DeletePrinterDataW(_para1:HANDLE; _para2:LPWSTR):DWORD;stdcall; external External_library name 'DeletePrinterDataW';

  function DeletePrinterDriver(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR):BOOL;stdcall; external External_library name 'DeletePrinterDriverA';

  function DeletePrinterDriver(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR):BOOL;stdcall; external External_library name 'DeletePrinterDriverW';

  function DeletePrinterDriverA(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR):BOOL;stdcall; external External_library name 'DeletePrinterDriverA';

  function DeletePrinterDriverW(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR):BOOL;stdcall; external External_library name 'DeletePrinterDriverW';

  function DeletePrintProcessor(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR):BOOL;stdcall; external External_library name 'DeletePrintProcessorA';

  function DeletePrintProcessor(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR):BOOL;stdcall; external External_library name 'DeletePrintProcessorW';

  function DeletePrintProcessorA(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR):BOOL;stdcall; external External_library name 'DeletePrintProcessorA';

  function DeletePrintProcessorW(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR):BOOL;stdcall; external External_library name 'DeletePrintProcessorW';

  function DeletePrintProvidor(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR):BOOL;stdcall; external External_library name 'DeletePrintProvidorA';

  function DeletePrintProvidor(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR):BOOL;stdcall; external External_library name 'DeletePrintProvidorW';

  function DeletePrintProvidorA(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR):BOOL;stdcall; external External_library name 'DeletePrintProvidorA';

  function DeletePrintProvidorW(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR):BOOL;stdcall; external External_library name 'DeletePrintProvidorW';

  function DocumentProperties(_para1:HWND; _para2:HANDLE; _para3:LPSTR; _para4:PDEVMODE; _para5:PDEVMODE;
             _para6:DWORD):LONG;stdcall; external External_library name 'DocumentPropertiesA';

  function DocumentProperties(_para1:HWND; _para2:HANDLE; _para3:LPWSTR; _para4:PDEVMODEW; _para5:PDEVMODEW;
             _para6:DWORD):LONG;stdcall; external External_library name 'DocumentPropertiesW';

  function DocumentPropertiesA(_para1:HWND; _para2:HANDLE; _para3:LPSTR; _para4:PDEVMODE; _para5:PDEVMODE;
             _para6:DWORD):LONG;stdcall; external External_library name 'DocumentPropertiesA';

  function DocumentPropertiesW(_para1:HWND; _para2:HANDLE; _para3:LPWSTR; _para4:PDEVMODEW; _para5:PDEVMODEW;
             _para6:DWORD):LONG;stdcall; external External_library name 'DocumentPropertiesW';

  function EndDocPrinter(_para1:HANDLE):BOOL;stdcall; external External_library name 'EndDocPrinter';

  function EndPagePrinter(_para1:HANDLE):BOOL;stdcall; external External_library name 'EndPagePrinter';

  function EnumForms(_para1:HANDLE; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'EnumFormsA';

  function EnumFormsA(_para1:HANDLE; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'EnumFormsA';

  function EnumFormsW(_para1:HANDLE; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'EnumFormsW';

  function EnumJobs(_para1:HANDLE; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:PBYTE;
             _para6:DWORD; _para7:PDWORD; _para8:PDWORD):BOOL;stdcall; external External_library name 'EnumJobsA';

  function EnumJobsA(_para1:HANDLE; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:PBYTE;
             _para6:DWORD; _para7:PDWORD; _para8:PDWORD):BOOL;stdcall; external External_library name 'EnumJobsA';

  function EnumJobsW(_para1:HANDLE; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:PBYTE;
             _para6:DWORD; _para7:PDWORD; _para8:PDWORD):BOOL;stdcall; external External_library name 'EnumJobsW';

  function EnumMonitors(_para1:LPSTR; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'EnumMonitorsA';

  function EnumMonitors(_para1:LPWSTR; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'EnumMonitorsW';

  function EnumMonitorsA(_para1:LPSTR; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'EnumMonitorsA';

  function EnumMonitorsW(_para1:LPWSTR; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'EnumMonitorsW';

  function EnumPorts(_para1:LPSTR; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'EnumPortsA';

  function EnumPortsA(_para1:LPSTR; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'EnumPortsA';

  function EnumPortsW(_para1:LPWSTR; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'EnumPortsW';

  function EnumPrinterData(_para1:HANDLE; _para2:DWORD; _para3:LPSTR; _para4:DWORD; _para5:PDWORD;
             _para6:PDWORD; _para7:PBYTE; _para8:DWORD; _para9:PDWORD):DWORD;stdcall; external External_library name 'EnumPrinterDataA';

  function EnumPrinterDataA(_para1:HANDLE; _para2:DWORD; _para3:LPSTR; _para4:DWORD; _para5:PDWORD;
             _para6:PDWORD; _para7:PBYTE; _para8:DWORD; _para9:PDWORD):DWORD;stdcall; external External_library name 'EnumPrinterDataA';

  function EnumPrinterDataW(_para1:HANDLE; _para2:DWORD; _para3:LPWSTR; _para4:DWORD; _para5:PDWORD;
             _para6:PDWORD; _para7:PBYTE; _para8:DWORD; _para9:PDWORD):DWORD;stdcall; external External_library name 'EnumPrinterDataW';

  function EnumPrinterDrivers(_para1:LPSTR; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrinterDriversA';

  function EnumPrinterDrivers(_para1:LPWSTR; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrinterDriversW';

  function EnumPrinterDriversA(_para1:LPSTR; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrinterDriversA';

  function EnumPrinterDriversW(_para1:LPWSTR; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrinterDriversW';

  function EnumPrinters(_para1:DWORD; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrintersA';

  function EnumPrinters(_para1:DWORD; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrintersW';

  function EnumPrintersA(_para1:DWORD; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrintersA';

  function EnumPrintersW(_para1:DWORD; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrintersW';

  function EnumPrintProcessorDatatypes(_para1:LPSTR; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrintProcessorDatatypesA';

  function EnumPrintProcessorDatatypes(_para1:LPWSTR; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrintProcessorDatatypesW';

  function EnumPrintProcessorDatatypesA(_para1:LPSTR; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrintProcessorDatatypesA';

  function EnumPrintProcessorDatatypesW(_para1:LPWSTR; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrintProcessorDatatypesW';

  function EnumPrintProcessorsA(_para1:LPSTR; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrintProcessorsA';

  function EnumPrintProcessorsW(_para1:LPWSTR; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrintProcessorsW';

  function EnumPrintProcessors(_para1:LPSTR; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrintProcessorsA';

  function EnumPrintProcessors(_para1:LPWSTR; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD; _para7:PDWORD):BOOL;stdcall; external External_library name 'EnumPrintProcessorsW';

  function FindClosePrinterChangeNotification(_para1:HANDLE):BOOL;stdcall; external External_library name 'FindClosePrinterChangeNotification';

  function FindFirstPrinterChangeNotification(_para1:HANDLE; _para2:DWORD; _para3:DWORD; _para4:PVOID):HANDLE;stdcall; external External_library name 'FindFirstPrinterChangeNotification';

  function FindNextPrinterChangeNotification(_para1:HANDLE; _para2:PDWORD; _para3:PVOID; _para4:PPVOID):HANDLE;stdcall; external External_library name 'FindNextPrinterChangeNotification';

  function FreePrinterNotifyInfo(_para1:PPRINTER_NOTIFY_INFO):BOOL;stdcall; external External_library name 'FreePrinterNotifyInfo';

  function GetDefaultPrinter(_para1:LPSTR; _para2:LPDWORD):BOOL;stdcall; external External_library name 'GetDefaultPrinterA';

  function GetDefaultPrinter(_para1:LPWSTR; _para2:LPDWORD):BOOL;stdcall; external External_library name 'GetDefaultPrinterW';

  function GetDefaultPrinterA(_para1:LPSTR; _para2:LPDWORD):BOOL;stdcall; external External_library name 'GetDefaultPrinterA';

  function GetDefaultPrinterW(_para1:LPWSTR; _para2:LPDWORD):BOOL;stdcall; external External_library name 'GetDefaultPrinterW';

  function GetForm(_para1:HANDLE; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'GetFormA';

  function GetForm(_para1:HANDLE; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'GetFormW';

  function GetFormA(_para1:HANDLE; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'GetFormA';

  function GetFormW(_para1:HANDLE; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'GetFormW';

  function GetJob(_para1:HANDLE; _para2:DWORD; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'GetJobA';

  function GetJobA(_para1:HANDLE; _para2:DWORD; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'GetJobA';

  function GetJobW(_para1:HANDLE; _para2:DWORD; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):BOOL;stdcall; external External_library name 'GetJobW';

  function GetPrinter(_para1:HANDLE; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD):BOOL;stdcall; external External_library name 'GetPrinterA';

  function GetPrinterA(_para1:HANDLE; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD):BOOL;stdcall; external External_library name 'GetPrinterA';

  function GetPrinterW(_para1:HANDLE; _para2:DWORD; _para3:PBYTE; _para4:DWORD; _para5:PDWORD):BOOL;stdcall; external External_library name 'GetPrinterW';

  function GetPrinterData(_para1:HANDLE; _para2:LPSTR; _para3:PDWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrinterDataA';

  function GetPrinterData(_para1:HANDLE; _para2:LPWSTR; _para3:PDWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrinterDataW';

  function GetPrinterDataA(_para1:HANDLE; _para2:LPSTR; _para3:PDWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrinterDataA';

  function GetPrinterDataW(_para1:HANDLE; _para2:LPWSTR; _para3:PDWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrinterDataW';

  function GetPrinterDriver(_para1:HANDLE; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrinterDriverA';

  function GetPrinterDriver(_para1:HANDLE; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrinterDriverW';

  function GetPrinterDriverA(_para1:HANDLE; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrinterDriverA';

  function GetPrinterDriverW(_para1:HANDLE; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrinterDriverW';

  function GetPrinterDriverDirectory(_para1:LPSTR; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrinterDriverDirectoryA';

  function GetPrinterDriverDirectory(_para1:LPWSTR; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrinterDriverDirectoryW';

  function GetPrinterDriverDirectoryA(_para1:LPSTR; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrinterDriverDirectoryA';

  function GetPrinterDriverDirectoryW(_para1:LPWSTR; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrinterDriverDirectoryW';

  function GetPrintProcessorDirectory(_para1:LPSTR; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrintProcessorDirectoryA';

  function GetPrintProcessorDirectory(_para1:LPWSTR; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrintProcessorDirectoryW';

  function GetPrintProcessorDirectoryA(_para1:LPSTR; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrintProcessorDirectoryA';

  function GetPrintProcessorDirectoryW(_para1:LPWSTR; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD;
             _para6:PDWORD):DWORD;stdcall; external External_library name 'GetPrintProcessorDirectoryW';

  function OpenPrinter(_para1:LPSTR; _para2:PHANDLE; _para3:LPPRINTER_DEFAULTSA):BOOL;stdcall; external External_library name 'OpenPrinterA';

  function OpenPrinter(_para1:LPWSTR; _para2:PHANDLE; _para3:LPPRINTER_DEFAULTSW):BOOL;stdcall; external External_library name 'OpenPrinterW';

  function OpenPrinterA(_para1:LPSTR; _para2:PHANDLE; _para3:LPPRINTER_DEFAULTSA):BOOL;stdcall; external External_library name 'OpenPrinterA';

  function OpenPrinterW(_para1:LPWSTR; _para2:PHANDLE; _para3:LPPRINTER_DEFAULTSW):BOOL;stdcall; external External_library name 'OpenPrinterW';

  function PrinterMessageBox(_para1:HANDLE; _para2:DWORD; _para3:HWND; _para4:LPSTR; _para5:LPSTR;
             _para6:DWORD):DWORD;stdcall; external External_library name 'PrinterMessageBoxA';

  function PrinterMessageBox(_para1:HANDLE; _para2:DWORD; _para3:HWND; _para4:LPWSTR; _para5:LPWSTR;
             _para6:DWORD):DWORD;stdcall; external External_library name 'PrinterMessageBoxW';

  function PrinterMessageBoxA(_para1:HANDLE; _para2:DWORD; _para3:HWND; _para4:LPSTR; _para5:LPSTR;
             _para6:DWORD):DWORD;stdcall; external External_library name 'PrinterMessageBoxA';

  function PrinterMessageBoxW(_para1:HANDLE; _para2:DWORD; _para3:HWND; _para4:LPWSTR; _para5:LPWSTR;
             _para6:DWORD):DWORD;stdcall; external External_library name 'PrinterMessageBoxW';

  function PrinterProperties(_para1:HWND; _para2:HANDLE):BOOL;stdcall; external External_library name 'PrinterProperties';

  function ReadPrinter(_para1:HANDLE; _para2:PVOID; _para3:DWORD; _para4:PDWORD):BOOL;stdcall; external External_library name 'ReadPrinter';

  function ResetPrinter(_para1:HANDLE; _para2:LPPRINTER_DEFAULTSA):BOOL;stdcall; external External_library name 'ResetPrinterA';

  function ResetPrinter(_para1:HANDLE; _para2:LPPRINTER_DEFAULTSW):BOOL;stdcall; external External_library name 'ResetPrinterW';

  function ResetPrinterA(_para1:HANDLE; _para2:LPPRINTER_DEFAULTSA):BOOL;stdcall; external External_library name 'ResetPrinterA';

  function ResetPrinterW(_para1:HANDLE; _para2:LPPRINTER_DEFAULTSW):BOOL;stdcall; external External_library name 'ResetPrinterW';

  function ScheduleJob(_para1:HANDLE; _para2:DWORD):BOOL;stdcall; external External_library name 'ScheduleJob';

  function SetForm(_para1:HANDLE; _para2:LPSTR; _para3:DWORD; _para4:PBYTE):BOOL;stdcall; external External_library name 'SetFormA';

  function SetForm(_para1:HANDLE; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE):BOOL;stdcall; external External_library name 'SetFormW';

  function SetFormA(_para1:HANDLE; _para2:LPSTR; _para3:DWORD; _para4:PBYTE):BOOL;stdcall; external External_library name 'SetFormA';

  function SetFormW(_para1:HANDLE; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE):BOOL;stdcall; external External_library name 'SetFormW';

  function SetJob(_para1:HANDLE; _para2:DWORD; _para3:DWORD; _para4:PBYTE; _para5:DWORD):BOOL;stdcall; external External_library name 'SetJobA';

  function SetJobA(_para1:HANDLE; _para2:DWORD; _para3:DWORD; _para4:PBYTE; _para5:DWORD):BOOL;stdcall; external External_library name 'SetJobA';

  function SetJobW(_para1:HANDLE; _para2:DWORD; _para3:DWORD; _para4:PBYTE; _para5:DWORD):BOOL;stdcall; external External_library name 'SetJobW';

  function SetPrinter(_para1:HANDLE; _para2:DWORD; _para3:PBYTE; _para4:DWORD):BOOL;stdcall; external External_library name 'SetPrinterA';

  function SetPrinterA(_para1:HANDLE; _para2:DWORD; _para3:PBYTE; _para4:DWORD):BOOL;stdcall; external External_library name 'SetPrinterA';

  function SetPrinterW(_para1:HANDLE; _para2:DWORD; _para3:PBYTE; _para4:DWORD):BOOL;stdcall; external External_library name 'SetPrinterW';

  function SetPrinterData(_para1:HANDLE; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD):BOOL;stdcall; external External_library name 'SetPrinterDataA';

  function SetPrinterDataA(_para1:HANDLE; _para2:LPSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD):BOOL;stdcall; external External_library name 'SetPrinterDataA';

  function SetPrinterDataW(_para1:HANDLE; _para2:LPWSTR; _para3:DWORD; _para4:PBYTE; _para5:DWORD):BOOL;stdcall; external External_library name 'SetPrinterDataW';

  function StartDocPrinter(_para1:HANDLE; _para2:DWORD; _para3:PBYTE):DWORD;stdcall; external External_library name 'StartDocPrinterA';

  function StartDocPrinterA(_para1:HANDLE; _para2:DWORD; _para3:PBYTE):DWORD;stdcall; external External_library name 'StartDocPrinterA';

  function StartDocPrinterW(_para1:HANDLE; _para2:DWORD; _para3:PBYTE):DWORD;stdcall; external External_library name 'StartDocPrinterW';

  function StartPagePrinter(_para1:HANDLE):BOOL;stdcall; external External_library name 'StartPagePrinter';

  function WaitForPrinterChange(_para1:HANDLE; _para2:DWORD):DWORD;stdcall; external External_library name 'WaitForPrinterChange';

  function WritePrinter(_para1:HANDLE; _para2:PVOID; _para3:DWORD; _para4:PDWORD):BOOL;stdcall; external External_library name 'WritePrinter';


  type
     JOB_INFO_1 = JOB_INFO_1A;
     PJOB_INFO_1 = ^JOB_INFO_1;
     LPJOB_INFO_1 = ^JOB_INFO_1;

     JOB_INFO_2 = JOB_INFO_2A;
     PJOB_INFO_2 = ^JOB_INFO_2;
     LPJOB_INFO_2 = ^JOB_INFO_2;

     ADDJOB_INFO_1 = ADDJOB_INFO_1A;
     PADDJOB_INFO_1 = ^ADDJOB_INFO_1;
     LPADDJOB_INFO_1 = ^ADDJOB_INFO_1;

     DATATYPES_INFO_1 = DATATYPES_INFO_1A;
     PDATATYPES_INFO_1 = ^DATATYPES_INFO_1;
     LPDATATYPES_INFO_1 = ^DATATYPES_INFO_1;

     MONITOR_INFO_1 = MONITOR_INFO_1A;
     PMONITOR_INFO_1 = ^MONITOR_INFO_1;
     LPMONITOR_INFO_1 = ^MONITOR_INFO_1;

     MONITOR_INFO_2 = MONITOR_INFO_2A;
     PMONITOR_INFO_2 = ^MONITOR_INFO_2;
     LPMONITOR_INFO_2 = ^MONITOR_INFO_2;

     DOC_INFO_1 = DOC_INFO_1A;
     PDOC_INFO_1 = ^DOC_INFO_1;
     LPDOC_INFO_1 = ^DOC_INFO_1;

     DOC_INFO_2 = DOC_INFO_2A;
     PDOC_INFO_2 = ^DOC_INFO_2;
     LPDOC_INFO_2 = ^DOC_INFO_2;

     PORT_INFO_1 = PORT_INFO_1A;
     PPORT_INFO_1 = ^PORT_INFO_1;
     LPPORT_INFO_1 = ^PORT_INFO_1;

     PORT_INFO_2 = PORT_INFO_2A;
     PPORT_INFO_2 = ^PORT_INFO_2;
     LPPORT_INFO_2 = ^PORT_INFO_2;

     PORT_INFO_3 = PORT_INFO_3A;
     PPORT_INFO_3 = ^PORT_INFO_3;
     LPPORT_INFO_3 = ^PORT_INFO_3;

     DRIVER_INFO_1 = DRIVER_INFO_1A;
     PDRIVER_INFO_1 = ^DRIVER_INFO_1;
     LPDRIVER_INFO_1 = ^DRIVER_INFO_1;

     DRIVER_INFO_2 = DRIVER_INFO_2A;
     PDRIVER_INFO_2 = ^DRIVER_INFO_2;
     LPDRIVER_INFO_2 = ^DRIVER_INFO_2;

     DRIVER_INFO_3 = DRIVER_INFO_3A;
     PDRIVER_INFO_3 = ^DRIVER_INFO_3;
     LPDRIVER_INFO_3 = ^DRIVER_INFO_3;

     DRIVER_INFO_4 = DRIVER_INFO_4A;
     PDRIVER_INFO_4 = ^DRIVER_INFO_4;
     LPDRIVER_INFO_4 = ^DRIVER_INFO_4;

     DRIVER_INFO_5 = DRIVER_INFO_5A;
     PDRIVER_INFO_5 = ^DRIVER_INFO_5;
     LPDRIVER_INFO_5 = ^DRIVER_INFO_5;

     DRIVER_INFO_6 = DRIVER_INFO_6A;
     PDRIVER_INFO_6 = ^DRIVER_INFO_6;
     LPDRIVER_INFO_6 = ^DRIVER_INFO_6;

     PRINTER_ENUM_VALUES = PRINTER_ENUM_VALUESA;
     PPRINTER_ENUM_VALUES = ^PRINTER_ENUM_VALUES;
     LPRINTER_ENUM_VALUES = ^PRINTER_ENUM_VALUES;

     PRINTER_INFO_1 = PRINTER_INFO_1A;
     PPRINTER_INFO_1 = ^PRINTER_INFO_1;
     LPPRINTER_INFO_1 = ^PRINTER_INFO_1;

     PRINTER_INFO_2 = PRINTER_INFO_2A;
     PPRINTER_INFO_2 = ^PRINTER_INFO_2;
     LPPRINTER_INFO_2 = ^PRINTER_INFO_2;

     PRINTER_INFO_4 = PRINTER_INFO_4A;
     PPRINTER_INFO_4 = ^PRINTER_INFO_4;
     LPPRINTER_INFO_4 = ^PRINTER_INFO_4;

     PRINTER_INFO_5 = PRINTER_INFO_5A;
     PPRINTER_INFO_5 = ^PRINTER_INFO_5;
     LPPRINTER_INFO_5 = ^PRINTER_INFO_5;

     PRINTER_INFO_7 = PRINTER_INFO_7A;
     PPRINTER_INFO_7 = ^PRINTER_INFO_7;
     LPPRINTER_INFO_7 = ^PRINTER_INFO_7;

     PRINTPROCESSOR_INFO_1 = PRINTPROCESSOR_INFO_1A;
     PPRINTPROCESSOR_INFO_1 = ^PRINTPROCESSOR_INFO_1;
     LPPRINTPROCESSOR_INFO_1 = ^PRINTPROCESSOR_INFO_1;

     FORM_INFO_1 = FORM_INFO_1A;
     PFORM_INFO_1 = ^FORM_INFO_1;
     LPFORM_INFO_1 = ^FORM_INFO_1;

     PRINTER_DEFAULTS = PRINTER_DEFAULTSA;
     PPRINTER_DEFAULTS = ^PRINTER_DEFAULTS;
     LPPRINTER_DEFAULTS = ^PRINTER_DEFAULTS;

     PROVIDOR_INFO_1 = PROVIDOR_INFO_1A;
     PPROVIDOR_INFO_1 = ^PROVIDOR_INFO_1;
     LPROVIDOR_INFO_1 = ^PROVIDOR_INFO_1;

     PROVIDOR_INFO_2 = PROVIDOR_INFO_2A;
     PPROVIDOR_INFO_2 = ^PROVIDOR_INFO_2;
     LPROVIDOR_INFO_2 = ^PROVIDOR_INFO_2;

implementation


end.
