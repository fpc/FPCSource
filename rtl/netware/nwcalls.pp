{
  Netware Server Imports for FreePascal Netware Clib RTL, contains
  definitions from the following NDK header files:

  ntypes.h,nwacct.h,nwafp.h,nwalias.h,nwapidef.h,nwbindry.h,
  nwcaldef.h,nwcalls.h,nwconnec.h,nwdel.h,nwdentry.h,nwdirect.h,
  nwdpath.h,nwea.h,nwerror.h,nwfattr.h,nwfile.h,nwfse.h,nwmigrat.h,
  nwmisc.h,nwmsg.h,nwnamspc.h,nwprint.h,nwqms.h,nwserver.h,nwsm.h,
  nwsync.h,nwtts.h,nwvol.h,stddef.h,unicode.h

  Initial Version 2005/01/14 Armin (armin@freepascal.org)

  The C-NDK and Documentation can be found here:
    http://developer.novell.com

  This program is distributed in the hope that it will be useful,but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.

  Do not blame Novell if there are errors in this file, instead
  create a bug report on http://www.freepascal.org and i will see what i
  can do.
}


unit nwcalls;
{$MODE objfpc}
{$MACRO on}

interface

{Macros for netware imports:}
{$DEFINE NWLIB_CALNLM32 := cdecl; external 'calnlm32'}
{$DEFINE NWLIB_LOCNLM32 := cdecl; external 'locnlm32'}
{$DEFINE NWLIB_UNICODE := cdecl; external 'unicode'}
{$DEFINE NWLIB_UNKNOWN := cdecl; external}            {for these ones i have not found the exporting module}
{$DEFINE NWLIB_CLIB := cdecl; external 'clib'}
{$DEFINE NWLIB_DSAPI := cdecl; external 'DSAPI'}

type
  Tnuint16 = word;
  Pnuint16 = pword;
  Tnuint8 = byte;
  Pnuint8 = ^byte;
  Tnptr = pointer;
  pnstr = pchar;
  ppnstr = ppchar;
  Tnstr8 = char;
  Pnstr8 = pchar;
  nptr = pointer;
  Pnptr = ^pointer;
  Tnflag32 = longint;

  PBYTE_REGISTERS = ^TBYTE_REGISTERS;
  TBYTE_REGISTERS = record
    si: Tnuint16;
    ds: Tnuint16;
    di: Tnuint16;
    es: Tnuint16;
    al: Tnuint8;
    ah: Tnuint8;
    bl: Tnuint8;
    bh: Tnuint8;
    cl: Tnuint8;
    ch: Tnuint8;
    dl: Tnuint8;
    dh: Tnuint8;
  end;

  PWORD_REGISTERS = ^TWORD_REGISTERS;
  TWORD_REGISTERS = record
    si: Tnuint16;
    ds: Tnuint16;
    di: Tnuint16;
    es: Tnuint16;
    ax: Tnuint16;
    bx: Tnuint16;
    cx: Tnuint16;
    dx: Tnuint16;
    bp: Tnuint16;
    flags: Tnuint16;
  end;

  PPTR_REGISTERS = ^TPTR_REGISTERS;
  TPTR_REGISTERS = record
    requestBuffer: Tnptr;
    replyBuffer: Tnptr;
  end;

  PSEG_OFF_REGISTERS = ^TSEG_OFF_REGISTERS;
  TSEG_OFF_REGISTERS = record
    ds_si: Tnptr;
    es_di: Tnptr;
  end;

  PREGISTERS = ^TREGISTERS;
  TREGISTERS = record
    case longint of
      0: (w: TWORD_REGISTERS);
      1: (b: TBYTE_REGISTERS);
      2: (p: TPTR_REGISTERS);
      3: (s: TSEG_OFF_REGISTERS);
  end;


{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{$DEFINE N_PLAT_NLM}


//*****************************************************************************
//nwapidef.h
//*****************************************************************************


{ Miscellaneous string lengths (constant) }
{ NOTE: These max values include a nullbyte }

const
  NW_MAX_USER_NAME_LEN = 49;
  NW_MAX_VOLUME_NAME_LEN = 17;
  NW_MAX_SERVER_NAME_LEN = 49;
  NW_MAX_TREE_NAME_LEN = 33;
  NW_MAX_SERVICE_TYPE_LEN = 49;
{ Miscellaneous unicode string sizes in bytes (constant)  }

  NW_MAX_USER_NAME_BYTES = 2 * NW_MAX_USER_NAME_LEN;
  NW_MAX_VOLUME_NAME_BYTES = 2 * NW_MAX_VOLUME_NAME_LEN;
  NW_MAX_SERVER_NAME_BYTES = 2 * NW_MAX_SERVER_NAME_LEN;
  NW_MAX_TREE_NAME_BYTES = 2 * NW_MAX_TREE_NAME_LEN;
  NW_MAX_SERVICE_TYPE_BYTES = 2 * NW_MAX_SERVICE_TYPE_LEN;
{ PrintFlags (word value)  }
  NW_PRINT_FLAG_RELEASE = $0001;
  NW_PRINT_FLAG_SUPPRESS_FF = $0002;
  NW_PRINT_FLAG_TEXT_FILE = $0004;
  NW_PRINT_FLAG_PRINT_BANNER = $0008;
  NW_PRINT_FLAG_NOTIFY = $0010;
{ Print string lengths (constant)  }
  NW_MAX_JOBDESCR_LEN = 50;
  NW_MAX_FORM_NAME_LEN = 13;
  NW_MAX_BANNER_NAME_LEN = 13;
  NW_MAX_QUEUE_NAME_LEN = 65;
{ Client Types : these are returned by NWGetClientType  }
  NW_NETX_SHELL = 1;  {dos netx}
  NW_VLM_REQ = 2;     {dos vlm}
  NW_CLIENT32 = 3;    {dos or 9x nlm client}
  NW_NT_REQ = 4;
  NW_OS2_REQ = 5;
  NW_NLM_REQ = 6;     {a netware server}


type
  Tnuint = cardinal;
  pnuint = ^Tnuint;
  Tnint = longint;
  pnint = plongint;
  Tnint32 = longint;
  pnint32 = plongint;
  Tnuint32 = cardinal;
  pnuint32 = pcardinal;
  Tnint16 = smallint;
  Pnint16 = ^smallint;
  TNWCONN_HANDLE = Tnuint;
  pNWCONN_HANDLE = pnuint;
  NWCONN_NUM = Tnuint16;
       //NWCCODE = Tnuint;
  TNWCCODE = Tnuint;
  NWDIR_HANDLE = Tnuint8;
  TNWDIR_HANDLE = NWDIR_HANDLE;
  PNWDIR_HANDLE = ^NWDIR_HANDLE;

  NWFILE_HANDLE = Tnint;
  TNWFILE_HANDLE = Tnint;
  PNWFILE_HANDLE = ^NWFILE_HANDLE;
  LONG = Tnuint32;

const
  FA_NORMAL = $00;
  FA_READ_ONLY = $01;
  FA_HIDDEN = $02;
  FA_SYSTEM = $04;
  FA_EXECUTE_ONLY = $08;
  FA_DIRECTORY = $10;
  FA_NEEDS_ARCHIVED = $20;
  FA_SHAREABLE = $80;
    { Extended file attributes  }
  FA_TRANSACTIONAL = $10;
  FA_INDEXED = $20;
  FA_READ_AUDIT = $40;
  FA_WRITE_AUDIT = $80;

       { the following is a the correct attribute mask list  }
    { The difference between these and the FA_ constants above is that these
       are in the correct positions. The last four attributes above are 8 bits
       off. (They need to be shifted 8 bits to the left.)  }

const
  A_NORMAL = $00000000;
  A_READ_ONLY = $00000001;
  A_HIDDEN = $00000002;
  A_SYSTEM = $00000004;
  A_EXECUTE_ONLY = $00000008;
  A_DIRECTORY = $00000010;
  A_NEEDS_ARCHIVED = $00000020;
  A_SHAREABLE = $00000080;
  A_DONT_SUBALLOCATE = $00000800;
  A_TRANSACTIONAL = $00001000;
    { not in the NCP book  }
  A_INDEXED = $00002000;
  A_READ_AUDIT = $00004000;
  A_WRITE_AUDIT = $00008000;
  A_IMMEDIATE_PURGE = $00010000;
  A_RENAME_INHIBIT = $00020000;
  A_DELETE_INHIBIT = $00040000;
  A_COPY_INHIBIT = $00080000;
  A_FILE_MIGRATED = $00400000;
  A_DONT_MIGRATE = $00800000;
  A_IMMEDIATE_COMPRESS = $02000000;
  A_FILE_COMPRESSED = $04000000;
  A_DONT_COMPRESS = $08000000;
  A_CANT_COMPRESS = $20000000;
    { access rights attributes  }

const
  AR_READ = $0001;
  AR_WRITE = $0002;
  AR_READ_ONLY = $0001;
  AR_WRITE_ONLY = $0002;
  AR_DENY_READ = $0004;
  AR_DENY_WRITE = $0008;
  AR_COMPATIBILITY = $0010;
  AR_WRITE_THROUGH = $0040;
  AR_OPEN_COMPRESSED = $0100;

  { search attributes  }
  SA_NORMAL = $0000;
  SA_HIDDEN = $0002;
  SA_SYSTEM = $0004;
  SA_SUBDIR_ONLY = $0010;
  SA_SUBDIR_FILES = $8000;
  SA_ALL = $8006;
  USE_NW_WILD_MATCH = 0;
  USE_DOS_WILD_MATCH = 1;

{ Scope specifiers  }
  GLOBAL = 0;
  PRIVATE = 1;
  MY_SESSION = 2;
  ALL_SESSIONS = 3;

//****************************************************************************
// nwalias.h
//****************************************************************************

type
  FILE_ATTRIBUTES_MASK = Tnuint32;
  NWACCESS_MODE = Tnuint8;
  NWACCESS_RIGHTS = Tnuint8;
  NWACCT_BALANCE = Tnint32;
  NWACCT_HOLDS = Tnuint16;
  NWACCT_LIMIT = Tnint32;
  NWADDR_LEN = Tnuint8;
  NWADDR_TYPE = Tnuint8;
  NWAES_COUNT = Tnuint16;

       //!! NWASN1_ID = Asn1ID_T;
  NWATTR = Tnuint32;
  NWATTRIBUTES = Tnuint32;
       //!! NWATTR_INFO = Attr_Info_T;
  NWAUDIT_BUF_SIZE = Tnuint16;
  NWAUDIT_CONN_ID = Tnuint32;
  NWAUDIT_CONTAINER_BIT_MAP = Tnuint32;
  NWAUDIT_DATA_LEN = Tnuint32;
  NWAUDIT_DATE_TIME = Tnuint32;
  NWAUDIT_DS_FLAG = Tnint16;
  NWAUDIT_EVENT = Tnuint16;
  NWAUDIT_FILE_CODE = Tnint16;
  NWAUDIT_FILE_HANDLE = Tnuint32;
  NWAUDIT_FLAGS = Tnuint32;
  NWAUDIT_KEY_BUF = pnuint8;
  NWAUDIT_LEVEL = Tnuint8;
  NWAUDIT_NAME_SPACE = Tnuint32;
  NWAUDIT_OBJ_SECURITY = Tnuint32;
  NWAUDIT_PASSWORD = pnuint8;
  NWAUDIT_PROCESS_ID = Tnuint32;
  NWAUDIT_QUEUE_TYPE = Tnuint32;
  NWAUDIT_RECORD_ID = Tnuint32;
  NWAUDIT_REC_NUM = Tnuint32;
  NWAUDIT_REPLICA_NUM = Tnuint16;
       //!! NWAUDIT_SIZE = NWSIZE;
  NWAUDIT_STATUS_CODE = Tnuint32;
  NWAUDIT_TRUSTEE_RIGHTS = Tnuint32;
  NWAUDIT_VOL_NUM = Tnuint32;
    { AN ADDITIONAL FLAG SIZE  }
  NWAUGMENT = Tnuint16;
  NWBITS = Tnuint32;
  NWBROADCAST_MODE = Tnuint16;
  NWBUF_SIZE = Tnuint16;
  NWCHANGE_BITS = Tnuint32;
  NWCHANGE_TYPE = Tnuint32;
  NWCHARGE_AMOUNT = Tnint32;
       //!! NWCLASS_INFO = Class_Info_T;
  NWCONFIG_DEFAULT_VALUE = Tnint32;
  NWCONFIG_ELEMENT_NUM = Tnint16;
  NWCONFIG_PARAM_TYPE = Tnint16;
  NWCONN_FLAGS = Tnuint16;
  NWCONN_NUM_WORD = Tnuint16;
  NWCONN_TYPE = Tnuint8;
  NWCOUNT = Tnuint32;
  NWCTLR_NUM = Tnuint8;
  NWCTLR_TYPE = Tnuint8;
  NWCURRENT_REC = Tnuint16;
  NWDATA_STREAM = Tnuint32;
  NWDATE = Tnuint16;
  NWDATE_TIME = Tnuint32;
  NWDELETE_TIME = Tnuint32;
  NWDENY_COUNT = Tnuint16;
  NWDEVICE_ID = Tnuint16;
  NWDIR_ATTRIBUTES = Tnuint8;
  NWDIR_BASE = Tnuint32;
  NWDIR_ENTRY = Tnuint32;
  NWDIR_ID = Tnuint8;
  NWDIR_NUM = Tnuint16;
  NWDIR_SPACE = Tnuint32;
  NWDIR_STAMP = Tnuint16;
  NWDIR_TRUSTEE_RIGHTS = Tnuint16;
  NWDIR_VOL = Tnuint8;
  NWDISK_CHANNEL = byte;
  NWDISK_DRV_TYPE = byte;
  NWDISK_FLAGS = word;
  NWDISK_NUM = byte;
  NWDISK_SPACE = cardinal;
  NWDISK_TYPE = byte;
  NWDISTANCE = word;
  NWDMA = byte;
  NWDM_FLAGS = cardinal;
  NWDRIVE_NUM = word;
  NWDRIVE_NUMBER = byte;
  NWDRV_COMMAND = cardinal;
  NWDRV_CONFIG = cardinal;
  NWDRV_FLAGS = word;
  NWDRV_ID = word;
  NWDRV_LINK = cardinal;
  NWDRV_MEM = cardinal;
  NWDRV_NAME = cardinal;
  NWDRV_TAG = cardinal;
  NWDRV_TYPE = cardinal;
  NWDRV_VERSION = byte;
  NWDSLEN = cardinal;
       //!! NWDS_BUFFER = Buf_T;
  NWDS_EVENT = cardinal;
       //!! NWDS_FILTER_CURSOR = Filter_Cursor_T;
  NWDS_FILTER_LEVEL = word;
       //!! NWDS_FILTER_NODE = Filter_Node_T;
  NWDS_FLAGS = cardinal;
  NWDS_ID = Tnint16;
  NWDS_INTERVAL = cardinal;
  NWDS_ITERATION = Tnint32;
  NWDS_LOGIN_FILE = Tnint16;
  NWDS_NUM_OBJ = Tnint32;
  NWDS_OPERATION = cardinal;
  NWDS_PRIVILEGES = cardinal;
  NWDS_SEARCH_SCOPE = word;
       //!! NWDS_SESSION_KEY = NWDS_Session_Key_T;
  NWDS_SIZE = cardinal;
  NWDS_SYNTAX_FLAGS = Tnint16;
  NWDS_TOKEN = word;
  NWDS_TYPE = cardinal;
  NWDS_TYPE_LEVEL = cardinal;
  NWDS_VALIDITY = cardinal;
  NWDS_VALUE = cardinal;
       //!! NWEA = NW_EA_HANDLE;
  NWEA_HANDLE = cardinal;
  NWEA_KEY = word;
  NWEA_KEY_LEN = word;
  NWEA_KEY_OFFSET = word;
       //!! NWEA_SCAN = NW_EA_FF_STRUCT;
  NWECB_CANCEL_COUNT = word;
  NWELEMENT_VALUE = Tnint16;
  NWEMAIL_TYPE = cardinal;
  NWFACTOR = cardinal;
  NWFAT = cardinal;
  NWFILE_ATTR = byte;
  NWFILE_LEN = cardinal;
  NWFILE_MODE = byte;
  NWFILE_SYS_ID = cardinal;
  NWFINDER_INFO = byte;
  NWFLAGS = byte;
  NWFORM_NUM = byte;
  NWFORM_TYPE = word;
  NWFRAG_SIZE = word;
  NWFSE_CONN_TYPE = cardinal;
  NWFSE_FLAGS = cardinal;
  NWGLT_FAIL_COUNT = word;
  NWHANDLE = byte;
  NWHF_START = cardinal;
       //!! NWHOLDS_INFO = HOLDS_INFO;
       //!! NWHOLDS_STATUS = HOLDS_STATUS;
  NWHOLD_AMOUNT = cardinal;
  NWHOLD_CANCEL_AMOUNT = cardinal;
  NWINFO_LEVEL = cardinal;
  NWINTERRUPT = byte;
  NWIO_MEM = word;
  NWJOB_FLAGS = word;
  NWJOB_HANDLE = cardinal;
  NWJOB_POSITION = byte;
  NWJOB_POSITION2 = word;
  NWJOB_TYPE = word;
  NWLAN_NUM = byte;
  NWLAST_RECORD = Tnint16;
  NWLEN = cardinal;
  NWLENGTH = word;
    { FOR DOS, OS/2, AND WINDOWS  }
  NWLOCAL_FILE_HANDLE = word;
  NWLOCAL_MODE = word;
  NWLOCAL_SCOPE = word;
  NWLOCK_COUNT = word;
  NWLOCK_DATA_STREAM = byte;
  NWLOCK_STATE = byte;
  NWLOCK_TYPE = byte;
       //NWLOCK_TYPE = byte;
  NWLOGIN_TIME = array[0..6] of byte;


type
  NWLPT = byte;
  NWMAX_PACKET_SIZE = word;
  NWMEDIA_MASK = cardinal;
  NWMEDIA_TYPE = cardinal;
  NWMEM_OFFSET = word;
  NWMINUTES = byte;
  NWMODULE_ID = cardinal;
  NWNAME = pnuint8;
  NWNAME_LEN = byte;
  NWNAME_SPACE = byte;
  NWNAME_SPACE_TYPE = cardinal;
  NWNET_ADDR = byte;
  NWNET_ADDR_LEN = cardinal;
  NWNET_ADDR_TYPE = cardinal;
  NWNEXT_REQUEST = word;
  NWNLM_ID = cardinal;
  NWNLM_TYPE = cardinal;
  NWNOTE_TYPE = word;
  NWNS_ACCESS_MODE = word;
  NWNS_ACCESS_RIGHTS = word;
  NWNS_ATTR = word;
  NWNS_BITS = word;
  NWNS_DATA_STREAM = byte;
  NWNS_DATA_STREAM2 = word;
  NWNS_FLAGS = word;
  NWNS_HANDLE = cardinal;
  NWNS_LIST_SIZE = byte;
  NWNS_MASK = cardinal;
  NWNS_NUM = byte;
  NWNS_TYPE = word;
  NWNUM = cardinal;
  NWNUMBER = word;
  NWNUMBER_ENTRIES = byte;
  NWNUM_BLOCKS = cardinal;
  NWNUM_BUFFERS = word;
  NWNUM_BYTES = cardinal;
  NWNUM_CONNS = byte;
  NWNUM_COPIES = byte;
  NWNUM_DIR_ENTRIES = cardinal;
  NWNUM_DRIVES = byte;
  NWNUM_ELEMENTS = Tnint16;
  NWNUM_ENTRIES = word;
  NWNUM_FORKS = byte;
  NWNUM_HEADS = byte;
  NWNUM_HOPS = word;
  NWNUM_PACKETS = cardinal;
  NWNUM_REQUESTS = cardinal;
  NWNUM_SECTORS = byte;
  NWNUM_TRANSACTIONS = byte;
       //!! NWOBJECT_INFO = Object_Info_T;
  NWOBJ_ID = cardinal;
  NWOBJ_TYPE = word;
  NWOFFSET = cardinal;
  NWOPEN_COUNT = word;
  NWOPTION_NUM = byte;
  NWOS_REVISION = word;
  NWOS_VERSION = word;
  NWPATH_SIZE = word;
  NWPATH_VOL = byte;
  NWPOSITION = cardinal;
  NWPRINTER = word;
  NWPRINT_FLAGS = word;
  NWPRINT_TASK = cardinal;
  NWPROTOCOL_MASK = cardinal;
  NWPROTOCOL_VERSION = byte;
  NWPSTR = pnstr;
  NWQMS_HANDLE = cardinal;
  NWQMS_TASK = cardinal;
  NWREC_OFFSET = word;
  NWREPLICA_NUM = Tnint32;
  NWREPLICA_TYPE = cardinal;
  NWREQUESTER_VERSION = byte;
  NWREQUEST_MASK = word;
  NWRESERVED16 = cardinal;
  NWRESERVED32 = cardinal;
  NWREVISION = cardinal;
  NWRIGHTS = cardinal;
  NWRIGHTS_MASK = word;
  NWSEARCH_ATTR = byte;
  NWSEARCH_ATTRIBUTES = word;
  NWSEARCH_CONTEXT = word;
  NWSEARCH_MASK = word;
  NWSECONDS = cardinal;
  NWSEGMENT_DATA = pnuint8;
  NWSEGMENT_NUM = byte;
  NWSEM_HANDLE = cardinal;
  NWSEM_INT = Tnint16;
  NWSEM_VALUE = word;
  NWSEQUENCE = cardinal;
  NWSEQUENCE_NUM = word;
  NWSEQ_NUM = byte;
  NWSERVER_NAME_LEN = word;
  NWSERVER_TYPE = word;
  NWSERVICE_VERSION = byte;
  NWSESSION_ID = word;
  NWSIZE = cardinal;
  NWSOCKET_COUNT = word;
  NWSPX_COUNT = word;
  NWSTATION_NUM = byte;
  NWSTATION_NUM2 = cardinal;
  NWSTATS_VERSION = byte;
  NWSTATUS = cardinal;
  NWSTRUCT_SIZE = word;
  NWSUPPORT_LEVEL = byte;
  NWSYNTAX_ID = cardinal;
       //!! NWSYNTAX_INFO = Syntax_Info_T;
  NWSYS_TIME = cardinal;
  NWTAB = byte;
  NWTASK = word;
  NWTASK_COUNT = byte;
  NWTASK_NUM = word;
  NWTASK_STATE = byte;
  NWTDS = word;
  NWTDS_OFFSET = word;
  NWTICKS = word;
  NWTIME = word;
  NWTRAN_TYPE = byte;
  NWTRUSTEE_SEQUENCE_NUM = word;
  NWUSE_COUNT = word;
  NWUTILIZATION = cardinal;
  NWVCONSOLE_REVISION = byte;
  NWVCONSOLE_VERSION = byte;
  NWVERSION = cardinal;
  NWVOL = cardinal;
  NWVOL_FLAGS = word;
  NWVOL_NUM = word;
  NWVOL_NUMBER = byte;
  NWVOL_TYPE = cardinal;
  TRUSTEE_RIGHTS = cardinal;
//*****************************************************************************
//nwafp.h
//*****************************************************************************


    {* This is the structure that the application expects to see. Note that the
        long name and short name will be null terminated, and one extra byte has
        been added to long name and short name to assure word alignment * }
type

  PAFPFILEINFO = ^TAFPFILEINFO;
  TAFPFILEINFO = record
    entryID: Tnuint32;
    parentID: Tnuint32;
    attributes: Tnuint16;
    dataForkLength: Tnuint32;
    resourceForkLength: Tnuint32;
    numOffspring: Tnuint16;
    creationDate: Tnuint16;
    accessDate: Tnuint16;
    modifyDate: Tnuint16;
    modifyTime: Tnuint16;
    backupDate: Tnuint16;
    backupTime: Tnuint16;
    finderInfo: array[0..31] of Tnuint8;
    longName: array[0..33] of Tnstr8;
    ownerID: Tnuint32;
    shortName: array[0..13] of Tnstr8;
    accessPrivileges: Tnuint16;
    proDOSInfo: array[0..5] of Tnuint8;
  end;
  TNW_AFP_FILE_INFO = TAFPFILEINFO;
  PNW_AFP_FILE_INFO = ^TNW_AFP_FILE_INFO;
  {This is the structure that actually returned from the NCP call}

  PRECPKT_AFPFILEINFO = ^TRECPKT_AFPFILEINFO;
  TRECPKT_AFPFILEINFO = record
    entryID: Tnuint32;
    parentID: Tnuint32;
    attributes: Tnuint16;
    dataForkLength: Tnuint32;
    resourceForkLength: Tnuint32;
    numOffspring: Tnuint16;
    creationDate: Tnuint16;
    accessDate: Tnuint16;
    modifyDate: Tnuint16;
    modifyTime: Tnuint16;
    backupDate: Tnuint16;
    backupTime: Tnuint16;
    finderInfo: array[0..31] of Tnuint8;
    longName: array[0..31] of Tnstr8;
    ownerID: Tnuint32;
    shortName: array[0..11] of Tnstr8;
    accessPrivileges: Tnuint16;
    proDOSInfo: array[0..5] of Tnuint8;
  end;

  PAFPSETINFO = ^TAFPSETINFO;
  TAFPSETINFO = record
    attributes: Tnuint16;
    creationDate: Tnuint16;
    accessDate: Tnuint16;
    modifyDate: Tnuint16;
    modifyTime: Tnuint16;
    backupDate: Tnuint16;
    backupTime: Tnuint16;
    finderInfo: array[0..31] of Tnuint8;
    proDOSInfo: array[0..5] of Tnuint8;
  end;
  TNW_AFP_SET_INFO = TAFPSETINFO;
  PNW_AFP_SET_INFO = ^TNW_AFP_SET_INFO;

  NWAFP_ACCESS_PRIVILEGES = word;
  NWAFP_ENTRY_ID = cardinal;
  NWAFP_FILE_ATTRIBUTES = word;
       //!! NWAFP_FILE_INFO = AFPFILEINFO;
  NWAFP_FORK_LEN = cardinal;
  NWAFP_NUM_OFFSPRING = word;
       //!! NWAFP_SET_INFO = AFPSETINFO;
  NWAPP_NUM = word;



{ the following are the constants that can be used for requestMasks
  in NWAFPScanFileInformation and NWAFPGetFileInformation. }

const
  AFP_GET_ATTRIBUTES = $0001;
  AFP_GET_PARENT_ID = $0002;
  AFP_GET_CREATE_DATE = $0004;
  AFP_GET_ACCESS_DATE = $0008;
  AFP_GET_MODIFY_DATETIME = $0010;
  AFP_GET_BACKUP_DATETIME = $0020;
  AFP_GET_FINDER_INFO = $0040;
  AFP_GET_LONG_NAME = $0080;
  AFP_GET_ENTRY_ID = $0100;
  AFP_GET_DATA_LEN = $0200;
  AFP_GET_RESOURCE_LEN = $0400;
  AFP_GET_NUM_OFFSPRING = $0800;
  AFP_GET_OWNER_ID = $1000;
  AFP_GET_SHORT_NAME = $2000;
  AFP_GET_ACCESS_RIGHTS = $4000;
  AFP_GET_PRO_DOS_INFO = $8000;
  AFP_GET_ALL = $FFFF;

{ used for NWAFPSetFileInformation }
  AFP_SET_ATTRIBUTES = $0001;
  AFP_SET_CREATE_DATE = $0004;
  AFP_SET_ACCESS_DATE = $0008;
  AFP_SET_MODIFY_DATETIME = $0010;
  AFP_SET_BACKUP_DATETIME = $0020;
  AFP_SET_FINDER_INFO = $0040;
  AFP_SET_PRO_DOS_INFO = $8000;
  AFP_SA_NORMAL = $0000;
  AFP_SA_HIDDEN = $0100;
  AFP_SA_SYSTEM = $0200;
  AFP_SA_SUBDIR = $0400;
  AFP_SA_FILES = $0800;
  AFP_SA_ALL = $0F00;


function NWAFPAllocTemporaryDirHandle(conn: TNWCONN_HANDLE; volNum: Tnuint16; AFPEntryID: Tnuint32; AFPPathString: Pnstr8; dirHandle: PNWDIR_HANDLE;
  accessRights: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWAFPCreateDirectory(conn: TNWCONN_HANDLE; volNum: Tnuint16; AFPEntryID: Tnuint32; finderInfo: pnuint8; AFPPathString: pnstr8;
  newAFPEntryID: pnuint32): TNWCCODE; NWLIB_CLIB;

function NWAFPCreateFile(conn: TNWCONN_HANDLE; volNum: Tnuint16; AFPEntryID: Tnuint32; delExistingFile: Tnuint8; finderInfo: pnuint8;
  AFPPathString: Pnstr8; newAFPEntryID: pnuint32): TNWCCODE; NWLIB_CALNLM32;

function NWAFPDelete(conn: TNWCONN_HANDLE; volNum: Tnuint16; AFPEntryID: Tnuint32; AFPPathString: Pnstr8): TNWCCODE; NWLIB_CALNLM32;

function NWAFPGetEntryIDFromName(conn: TNWCONN_HANDLE; volNum: Tnuint16; AFPEntryID: Tnuint32; AFPPathString: Pnstr8; newAFPEntryID: pnuint32): TNWCCODE; NWLIB_CALNLM32;

function NWAFPGetEntryIDFromHandle(conn: TNWCONN_HANDLE; NWHandle: Pnuint8; volNum: pnuint16; AFPEntryID: pnuint32; forkIndicator: pnuint8): TNWCCODE; NWLIB_CALNLM32;

function NWAFPGetEntryIDFromPathName(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; AFPEntryID: pnuint32): TNWCCODE; NWLIB_CALNLM32;

function NWAFPGetFileInformation(conn: TNWCONN_HANDLE; volNum: Tnuint16; AFPEntryID: Tnuint32; reqMask: Tnuint16; AFPPathString: Pnstr8;
  structSize: Tnuint16; AFPFileInfo: PNW_AFP_FILE_INFO): TNWCCODE; NWLIB_CALNLM32;

function NWAFPDirectoryEntry(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8): TNWCCODE; NWLIB_CALNLM32;

function NWAFPOpenFileFork(conn: TNWCONN_HANDLE; volNum: Tnuint16; AFPEntryID: Tnuint32; forkIndicator: Tnuint8; accessMode: Tnuint8;
  AFPPathString: Pnstr8; fileID: pnuint32; forkLength: pnuint32; NWHandle: pnuint8; DOSFileHandle: PNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;


function NWAFPRename(conn: TNWCONN_HANDLE; volNum: Tnuint16; AFPSourceEntryID: Tnuint32; AFPDestEntryID: Tnuint32; AFPSrcPath: Pnstr8;
  AFPDstPath: Pnstr8): TNWCCODE; NWLIB_CALNLM32;

function NWAFPScanFileInformation(conn: TNWCONN_HANDLE; volNum: Tnuint16; AFPEntryID: Tnuint32; AFPLastSeenID: pnuint32; searchMask: Tnuint16;
  reqMask: Tnuint16; AFPPathString: Pnstr8; structSize: Tnuint16; AFPFileInfo: PNW_AFP_FILE_INFO): TNWCCODE; NWLIB_CALNLM32;

function NWAFPSetFileInformation(conn: TNWCONN_HANDLE; volNum: Tnuint16; AFPBaseID: Tnuint32; reqMask: Tnuint16; AFPPathString: Pnstr8;
  structSize: Tnuint16; AFPSetInfo: PNW_AFP_SET_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWAFPSupported(conn: TNWCONN_HANDLE; volNum: Tnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWAFPASCIIZToLenStr(pbstrDstStr: Pnstr8; pbstrSrcStr: Pnstr8): TNWCCODE; NWLIB_CALNLM32;

//*******************
// unicode.h
//*******************

type
  Psize_t = ^Tsize_t;
  Tsize_t = dword;
  Punicode = ^Tunicode;
  Tunicode = word;
    { Unicode data must be 16 bits    }

    {typedef unicode  * punicode; }
    {typedef unicode  *  * ppunicode; }
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    //function nwunisize(x : longint) : longint;

    { Converter handle }
type

  PCONVERT = ^TCONVERT;
  TCONVERT = pointer;

  PpCONVERT = ^TpCONVERT;
  TpCONVERT = TCONVERT;

    {************************************************************************** }
    {Type definitions for converter based APIs }
    {Function called when non-mappable bytes are found }
    { Handle to Byte <-> Uni converter   }
    { Pointer to current output position }
    { Space left in output buffer        }
    { Space used in output buffer        }
    { Pointer to unmappable bytes        }
    { Size of unmappable input           }
type

  TNMBYTE = function(byteUniHandle: TpCONVERT; output: punicode; outputLeft: Tnuint; outputUsed: pnuint; badInput: Pnuint8;
    badInputSize: Tnuint): Tnint; cdecl;
    {
        Function called when non-mappable unicode characters are found
     }
    { Handle to Byte <-> Uni converter   }
    { Pointer to current output position }
    { Space left in output buffer        }
    { Space used in output buffer        }
    { Ptr to unmappable unicode chars    }
    { Size of unmappable input           }

  TNMUNI = function(byteUniHandle: TpCONVERT; output: pnuint8; outputLeft: Tnuint; outputUsed: pnuint; badInput: Punicode;
    badInputSize: Tnuint): Tnint; cdecl;
    {
        Function called to scan for special byte input
     }
    { Handle to Byte <-> Uni converter   }
    { Input to scan for special bytes    }
    { Maximum # of bytes to scan or -1   }

  TSCBYTE = function(byteUniHandle: TpCONVERT; input: Pnuint8; scanmax: Tnint): pnuint8; cdecl;
    {
        Function called to scan for special Unicode input
     }
    { Handle to Byte <-> Uni converter  }
    { Input to scan for special chars   }
    { Maximum # of bytes to scan or -1  }

  TSCUNI = function(byteUniHandle: TpCONVERT; input: Punicode; scanmax: Tnint): punicode; cdecl;
    {
        Function called to parse special byte input
     }
    { Handle to Byte <-> Uni converter   }
    { Buffer for Unicode output          }
    { Space left in output buffer        }
    { Space used in output buffer        }
    { Buffer containing byte input       }
    { Number of bytes of input used      }

  TPRBYTE = function(byteUniHandle: TpCONVERT; output: punicode; outputleft: Tnuint; outputUsed: pnuint; input: Pnuint8;
    inputUsed: pnuint): Tnint; cdecl;
    {
        Function called to parse special Unicode input
     }
    { Handle to Byte <-> Uni converter  }
    { Buffer for bytes output           }
    { Space left in output buffer       }
    { Space used in output buffer       }
    { Buffer containing byte input      }
    { Number of Unicodes of input used  }

  TPRUNI = function(byteUniHandle: TpCONVERT; output: pnuint8; outputLeft: Tnuint; outputUsed: pnuint; input: Punicode;
    inputUsed: pnuint): Tnint; cdecl;
    {************************************************************************** }
    {
       Macros used by and returned from converter based API calls
     (i.e. NWUS*, NWUX*)
     }
    {
       Novell-defined Unicode characters.
       Consult with the Internationalization group before adding to this list.
     }

const
  UNI_CHANGE_NAMESPACE = $F8F4;
  UNI_PREVIOUS_DIR = $F8F5;
  UNI_CURRENT_DIR = $F8F6;
  UNI_PATH_SEPARATOR = $F8F7;
  UNI_VOLUMENAME_ROOT = $F8F8;
  UNI_VOLUME_ROOT = $F8F9;
  UNI_NDS_ROOT = $F8FA;
  UNI_WILD_QMARK = $F8FB;
  UNI_WILD_ASTERISK = $F8FC;
  UNI_WILD_AUG_QMARK = $F8FD;
  UNI_WILD_AUG_ASTERISK = $F8FE;
  UNI_WILD_AUG_PERIOD = $F8FF;
    {
        Actions to take when an unmappable byte or uni character is encountered.
        Used in SetNoMapAction call.
     }
    { Leave action unchanged                    }
  NWU_UNCHANGED_ACTION = -(1);
    { Return error code NWU_UNMAPPABLE_CHAR     }
  NWU_RETURN_ERROR = 0;
    { Use the current substitution character    }
  NWU_SUBSTITUTE = 1;
    { Call the no map handler function          }
  NWU_CALL_HANDLER = 2;
    {
        Codes to enable the Scan and Parse handler functions.
        Used in SetScanAction call.
     }
  NWU_DISABLED = 0; // Disable Scan/Parse functions
  NWU_ENABLED = 2; // Enable  Scan/Parse functions
    { Flags to pass to NWUXGetCaseConverter to specify whether to load
      a converter which converts to upper, lower or title case. }
  NWU_LOWER_CASE = 0; // Lower case
  NWU_UPPER_CASE = 1;
  NWU_TITLE_CASE = 2;
    { Flags to pass to NWUXGetNormalizeConverter to specify whether to
      load a converter which converts to pre-composed or de-composed
      unicode characters. }
  NWU_PRECOMPOSED = 0;
  NWU_DECOMPOSED = 1;
    { For use in SetByte/UniFunction calls }
    //function NWU_UNCHANGED_FUNCTION : pointer;


const
  NWU_RESET_TO_DEFAULT = nil;
    { Error codes.  FFFFFDE0 to FFFFFDFF reserved for new unicode APIs. }
  NWU_NO_CONVERTER = -(544); // Default converter not loaded
  NWU_CONVERTER_NOT_FOUND = -(543); // Converter file was not found
  NWU_TOO_MANY_FILES = -(542); // Too many open files
  NWU_NO_PERMISSION = -(541); // Access to file was denied
  NWU_OPEN_FAILED = -(540); // File open failed
  NWU_READ_FAILED = -(539); // File read failed
  NWU_OUT_OF_MEMORY = -(538); // Insufficient memory
  NWU_CANT_LOAD_CONVERTER = -(537); // Unable to load converter
  NWU_CONVERTER_CORRUPT = -(536); // The converter is invalid
  NWU_NULL_HANDLE = -(535); // Converter handle was NULL
  NWU_BAD_HANDLE = -(534); // Converter handle is invalid
  NWU_HANDLE_MISMATCH = -(533); // Handle doesn't match operation
  NWU_UNMAPPABLE_CHAR = -(532); // Unmappable character found
  NWU_RANGE_ERROR = -(531); // Invalid constant passed to fn
  NWU_BUFFER_FULL = -(530); // Buffer too small for output
  NWU_INPUT_MAX = -(529); // Processed max # of input chars
  UNI_PARSER_ERROR = -(528); // Error from user-written parser
  NWU_OLD_CONVERTER_VERSION = -(527); // Outdated converter DLL
  NWU_UNSUPPORTED_AUX_FUNCTION = -(526); // Unsupported AUX function
  NWU_EMBEDDED_NULL = -(525); // Embedded null in len spec string
  NWU_GET_CODE_PAGE_FAILED = -(524); // Failed to get system cp or cc
  NWU_ILLEGAL_UTF8_CHARACTER = -(506); // Cannot convert UTF8 char to Uni
  NWU_INSUFFICIENT_BUFFER = -(500);
    { Error codes for translator based APIs (i.e. NW prefix) }
  UNI_ALREADY_LOADED = -(489); // Already loaded another country or code page
  UNI_FUTURE_OPCODE = -(490); // Rule table has unimplimented rules
  UNI_NO_SUCH_FILE = -(491); // No such file or directory
  UNI_TOO_MANY_FILES = -(492); // Too many files already open
  UNI_NO_PERMISSION = -(493); // Permission denied on file open
  UNI_NO_MEMORY = -(494); // Not enough memory
  UNI_LOAD_FAILED = -(495); // NWLoadRuleTable failed, don't know why
  UNI_HANDLE_BAD = -(496); // Rule table handle was bad
  UNI_HANDLE_MISMATCH = -(497); // Rule table handle doesn't match operation
  UNI_RULES_CORRUPT = -(498); // Rule table is corrupt
  UNI_NO_DEFAULT = -(499); // No default rule and no 'No map' character
  UNI_INSUFFICIENT_BUFFER = -(500);
  UNI_OPEN_FAILED = -(501); // Open failed in NWLoadRuleTable
  UNI_NO_LOAD_DIR = -(502); // Load directory could not be determined
  UNI_BAD_FILE_HANDLE = -(503); // File handle was bad
  UNI_READ_FAILED = -(504); // File read of rule table failed
  UNI_TRANS_CORRUPT = -(505); // Translator is corrupt
  UNI_ILLEGAL_UTF8_CHARACTER = -(506); // Illegal UTF-8 character encountered

    {************************************************************************** }
    { Unicode converter prototypes - These APIs are preferred over the older
      non-converter counterparts (i.e. NWUnicodeToLocal, NWLocalToUnicode, etc.)}
    { These are the Standard API's }

    { Initialize standard converters }

function NWUSStandardUnicodeInit: Tnint; NWLIB_LOCNLM32;
    { Replace standard converter.               }
function NWUSStandardUnicodeOverride(codepage: Tnuint): Tnint; NWLIB_LOCNLM32;
    { Release the standard converters           }
procedure NWUSStandardUnicodeRelease; NWLIB_LOCNLM32;
    { Get the native code page and country      }
function NWUSGetCodePage(pCodePage: pnuint; pCountry: pnuint): Tnint; NWLIB_LOCNLM32;
    { NOTE:  The actualLength parameter returned by the conversion routines
              does *not* include the null terminator.
     }
    { Convert bytes to Unicode                  }
    { Buffer for resulting Unicode     }
    { Length of output buffer. Or 0    }

    { Buffer for input bytes           }
    { Length of results in uni chars   }
function NWUSByteToUnicode(unicodeOutput: punicode; outputBufferLen: Tnuint; byteInput: Pnuint8; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert bytes to Unicode for file path    }
    { Buffer for resulting Unicode     }
    { Length of output buffer. Or 0    }

    { Buffer for input bytes           }
    { Length of results in uni chars   }
function NWUSByteToUnicodePath(unicodeOutput: punicode; outputBufferLen: Tnuint; byteInput: Pnuint8; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert bytes to Unicode                  }
    { Buffer for resulting Unicode     }
    { Length of output buffer. Or 0    }

    { Buffer for input bytes           }
    { Input str length in bytes or -1  }
    { Length of results in uni chars   }
function NWUSLenByteToUnicode(unicodeOutput: punicode; outputBufferLen: Tnuint; byteInput: Pnuint8; inLength: Tnint; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert bytes to Unicode for file path    }
    { Buffer for resulting Unicode     }
    { Length of  output buffer. Or 0   }

    { Buffer for input bytes           }
    { Input str length in bytes or -1  }
    { Length of results in uni chars   }
function NWUSLenByteToUnicodePath(unicodeOutput: punicode; outputBufferLen: Tnuint; byteInput: Pnuint8; inLength: Tnint; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to bytes                  }
    { Buffer for output bytes         }
    { Length of output buffer. Or 0   }

    { Buffer for Unicode input        }
    { Length of results in bytes      }
function NWUSUnicodeToByte(byteOutput: pnuint8; outputBufferLen: Tnuint; unicodeInput: Punicode; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to bytes for file path    }
    { Buffer for output bytes         }
    { Length of output buffer. Or 0   }

    { Buffer for Unicode input        }
    { Length of results in bytes      }
function NWUSUnicodeToBytePath(byteOutput: pnuint8; outputBufferLen: Tnuint; unicodeInput: Punicode; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to bytes                  }
    { Buffer for output bytes         }
    { Length of output buffer         }

    { Buffer for Unicode input        }
    { Length of results in bytes      }
function NWUSUnicodeToUntermByte(byteOutput: pnuint8; outputBufferLen: Tnuint; unicodeInput: Punicode; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to bytes for file path    }
    { Buffer for output bytes         }
    { Length of output buffer         }

    { Buffer for Unicode input        }
    { Length of results in bytes      }
function NWUSUnicodeToUntermBytePath(byteOutput: pnuint8; outputBufferLen: Tnuint; unicodeInput: Punicode; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to lower case             }
    { Buffer for lower cased output   }
    { Length of output buffer. Or 0   }

    { Buffer for Unicode input        }
    { Length of results in uni chars  }
function NWUSUnicodeToLowerCase(lowerCaseOutput: punicode; outputBufferLen: Tnuint; unicodeInput: Punicode; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to upper case             }
    { Buffer for upper cased output   }
    { Length of output buffer. Or 0   }

    { Buffer for Unicode input        }
    { Length of results in uni chars  }
function NWUSUnicodeToUpperCase(upperCaseOutput: punicode; outputBufferLen: Tnuint; unicodeInput: Punicode; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    {
        These are the Extended API's
     }
    { Load a Byte <-> Unicode converter         }
    { Codepage number                        }
    { Converter handle returned here         }
function NWUXLoadByteUnicodeConverter(codepage: Tnuint; byteUniHandle: PpCONVERT): Tnint; NWLIB_LOCNLM32;
    { Load a Unicode -> Case converter          }
    { Want upper, lower or title casing?       }
    { Converter handle returned here           }
function NWUXLoadCaseConverter(caseFlag: Tnuint; caseHandle: PpCONVERT): Tnint; NWLIB_LOCNLM32;
    { Load a Unicode -> Collation converter     }
    { Country code for this locale         }
    { Converter handle returned here       }
function NWUXLoadCollationConverter(countryCode: Tnuint; collationHandle: PpCONVERT): Tnint; NWLIB_LOCNLM32;
    { Load a Unicode -> Normalized converter    }
    { Want precomposed or decomposed flag? }
    { Converter handle returned here       }
function NWUXLoadNormalizeConverter(preDeFlag: Tnuint; normalizeHandle: PpCONVERT): Tnint; NWLIB_LOCNLM32;
    { Release a converter from memory           }
    { Handle to converter to be released        }
function NWUXUnloadConverter(converterHandle: TpCONVERT): Tnint; NWLIB_LOCNLM32;
    { Convert bytes to Unicode                  }
    { Handle to Byte <-> Uni converter }
    { Buffer for resulting Unicode     }
    { Length of output buffer. Or 0    }

    { Buffer for input bytes           }
    { Length of results in uni chars   }
function NWUXByteToUnicode(byteUniHandle: TpCONVERT; unicodeOutput: punicode; outputBufferLen: Tnuint; byteInput: Pnuint8; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert bytes to Unicode for file path    }
    { Handle to Byte <-> Uni converter }
    { Buffer for resulting Unicode     }
    { Length of output buffer. Or 0    }

    { Buffer for input bytes           }
    { Length of results in uni chars   }
function NWUXByteToUnicodePath(byteUniHandle: TpCONVERT; unicodeOutput: punicode; outputBufferLen: Tnuint; byteInput: Pnuint8; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert bytes to Unicode                  }
    { Handle to Byte <-> Uni converter }
    { Buffer for resulting Unicode     }
    { Length of output buffer          }

    { Buffer for input bytes           }
    { Input str length in bytes or -1  }
    { Length of results in uni chars   }
function NWUXLenByteToUnicode(byteUniHandle: TpCONVERT; unicodeOutput: punicode; outputBufferLen: Tnuint; byteInput: Pnuint8; inLength: Tnint;
  actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert bytes to Unicode for file path    }
    { Handle to Byte <-> Uni converter }
    { Buffer for resulting Unicode     }
    { Length of output buffer          }

    { Buffer for input bytes           }
    { Input str length in bytes or -1  }
    { Length of results in uni chars   }
function NWUXLenByteToUnicodePath(byteUniHandle: TpCONVERT; unicodeOutput: punicode; outputBufferLen: Tnuint; byteInput: Pnuint8; inLength: Tnint;
  actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to bytes                  }
    { Handle to Byte <-> Uni converter }
    { Buffer for output bytes         }
    { Length of output buffer         }

    { Buffer for Unicode input        }
    { Length of results in bytes      }
function NWUXUnicodeToByte(byteUniHandle: TpCONVERT; byteOutput: pnuint8; outputBufferLen: Tnuint; unicodeInput: Punicode; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to bytes for file path    }
    { Handle to Byte <-> Uni converter }
    { Buffer for output bytes         }
    { Length of output buffer. Or 0   }

    { Buffer for Unicode input        }
    { Length of results in bytes      }
function NWUXUnicodeToBytePath(byteUniHandle: TpCONVERT; byteOutput: pnuint8; outputBufferLen: Tnuint; unicodeInput: Punicode; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to bytes                  }
    { Handle to Byte <-> Uni converter }
    { Buffer for output bytes         }
    { Length of output buffer         }

    { Buffer for Unicode input        }
    { Length of results in bytes      }
function NWUXUnicodeToUntermByte(byteUniHandle: TpCONVERT; byteOutput: pnuint8; outputBufferLen: Tnuint; unicodeInput: Punicode; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to bytes for file path    }
    { Handle to Byte <-> Uni converter }
    { Buffer for output bytes         }
    { Length of output buffer         }

    { Buffer for Unicode input        }
    { Length of results in bytes      }
function NWUXUnicodeToUntermBytePath(byteUniHandle: TpCONVERT; byteOutput: pnuint8; outputBufferLen: Tnuint; unicodeInput: Punicode; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert to upper, lower or title case     }
    { Handle to converter             }
    { Buffer for output               }
    { Length of output buffer. Or 0   }

    { Buffer for Unicode input        }
    { Length of results in uni chars  }
function NWUXUnicodeToCase(caseHandle: TpCONVERT; monocasedOutput: punicode; outputBufferLen: Tnuint; unicodeInput: Punicode; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to Collation weights      }
    { Handle to converter            }
    { Buffer for collation weights   }
    { Length of output buffer. Or 0  }

    { Buffer for Unicode input       }
    { Length of results in uni chars }
function NWUXUnicodeToCollation(collationHandle: TpCONVERT; collationWeights: punicode; outputBufferLen: Tnuint; unicodeInput: Punicode; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to normalized             }
    { Handle to converter            }
    { Buffer for normalized output   }
    { Length of output buffer. Or 0  }

    { Buffer for Unicode input       }
    { Length of results in uni chars }
function NWUXUnicodeToNormalized(normalizeHandle: TpCONVERT; normalizedOutput: punicode; outputBufferLen: Tnuint; unicodeInput: Punicode; actualLength: pnuint): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to bytes for file path    }
    { Handle to Byte <-> Uni converter   }

    { Ptr to single or double-byte char  }
    { # bytes in character (1 or 2)      }
function NWUXGetCharSize(byteUniHandle: TpCONVERT; byteInput: Pnuint8; pCharSize: pnuint): Tnint; NWLIB_LOCNLM32;
    { Set action to be taken for no map chars   }
    { Handle to a Byte <-> Unicode converter    }
    { Action to take for unmappable bytes       }
    { Action to take for unmappable unicode     }
function NWUXSetNoMapAction(byteUniHandle: TpCONVERT; noMapByteAction: Tnint; noMapUniAction: Tnint): Tnint; NWLIB_LOCNLM32;
    { Get action to be taken for no map chars   }
    { Handle to a Byte <-> Unicode converter    }
    { Action to take for unmappable bytes       }
    { Action to take for unmappable unicode     }
function NWUXGetNoMapAction(byteUniHandle: TpCONVERT; noMapByteAction: pnint; noMapUniAction: pnint): Tnint; NWLIB_LOCNLM32;
    { Enable or disable scan/parse functions    }
    { Handle to a Byte <-> Unicode converter    }
    { Set action for scan/parse byte functions  }
    { Set action for scan/parse uni functions   }
function NWUXSetScanAction(byteUniHandle: TpCONVERT; scanByteAction: Tnint; scanUniAction: Tnint): Tnint; NWLIB_LOCNLM32;
    { Get status of scan/parse functions        }
    { Handle to a Byte <-> Unicode converter    }
    { Status of scan/parse byte functions       }
    { Status of scan/parse uni functions        }
function NWUXGetScanAction(byteUniHandle: TpCONVERT; scanByteAction: pnint; scanUniAction: pnint): Tnint; NWLIB_LOCNLM32;
    { Set substitution byte for converter       }
    { Handle to a Byte <-> Unicode converter    }
    { Byte to be substituted                    }
function NWUXSetSubByte(byteUniHandle: TpCONVERT; substituteByte: Tnuint8): Tnint; NWLIB_LOCNLM32;
    { Get substitution byte for converter       }
    { Handle to a Byte <-> Unicode converter    }
    { Substitution byte returned here           }
function NWUXGetSubByte(byteUniHandle: TpCONVERT; substituteByte: pnuint8): Tnint; NWLIB_LOCNLM32;
    { Set substitute uni char for converter     }
    { Handle to a Byte <-> Unicode converter    }
    { Unicode character to be substituted       }
function NWUXSetSubUni(byteUniHandle: TpCONVERT; substituteUni: Tunicode): Tnint; NWLIB_LOCNLM32;
    { Get substitute uni char for converter     }
    { Handle to a Byte <-> Unicode converter    }
    { Substitution unicode char returned here   }
function NWUXGetSubUni(byteUniHandle: TpCONVERT; substituteUni: punicode): Tnint; NWLIB_LOCNLM32;
    { Set up unmappable byte handling           }
    { Handle to a Byte <-> Unicode converter    }
    { Function called for unmappable bytes      }
    { Byte scanning function                    }
    { Byte parsing function                     }
function NWUXSetByteFunctions(byteUniHandle: TpCONVERT; noMapByteFunc: TNMBYTE; scanByteFunc: TSCBYTE; parseByteFunc: TPRBYTE): Tnint; NWLIB_LOCNLM32;
    { Get unmappable byte handling functions    }
    { Handle to a Byte <-> Unicode converter    }
    { Handler function returned here            }
    { Byte scanning function                    }
    { Byte parsing function                     }
function NWUXGetByteFunctions(byteUniHandle: TpCONVERT; var noMapByteFunc: TNMBYTE; var scanByteFunc: TSCBYTE; var parseByteFunc: TPRBYTE): Tnint; NWLIB_LOCNLM32;
    { Set up unmappable character handling      }
    { Handle to a Byte <-> Unicode converter    }
    { Function called for unmappable uni chars  }
    { Unicode scanning function                 }
    { Unicode parsing function                  }
function NWUXSetUniFunctions(byteUniHandle: TpCONVERT; noMapUniFunc: TNMUNI; scanUniFunc: TSCUNI; parseUniFunc: TPRUNI): Tnint; NWLIB_LOCNLM32;
    { Set up unmappable unicode char handling   }
    { Handle to a Byte <-> Unicode converter    }
    { Function called for unmappable uni chars  }
    { Unicode scan function                     }
    { Unicode parse function                    }
function NWUXGetUniFunctions(byteUniHandle: TpCONVERT; var noMapUniFunc: TNMUNI; var scanUniFunc: TSCUNI; var parseUniFunc: TPRUNI): Tnint; NWLIB_LOCNLM32;
    { Set up converter to use the NW OEM Euro   }
function NWUXEnableOemEuro(convert: TpCONVERT): Tnint; NWLIB_LOCNLM32;
    { Reset a converter to default state        }
function NWUXResetConverter(convert: TpCONVERT): Tnint; NWLIB_LOCNLM32;
    {************************************************************************** }
    {
        Table based Unicode/Local text conversion APIs. The converter based
      APIs are preferred over these.
     }
function NWInitUnicodeTables(countryCode: Tnint; codePage: Tnint): Tnint; NWLIB_LOCNLM32;
function NWLSetPrimaryUnicodeSearchPath(strSearchPath: Pnstr): longint; NWLIB_UNICODE;
function NWFreeUnicodeTables: Tnint; NWLIB_LOCNLM32;
function NWLoadRuleTable(ruleTableName: pnstr; ruleHandle: pnptr): Tnint; NWLIB_UNICODE;
    { Rule table handle                    }
function NWUnloadRuleTable(ruleHandle: Tnptr): Tnint; NWLIB_UNICODE;

    { NWUSByteToUnicode or NWUXByteToUnicode are preferred  }
    { Convert local to Unicode             }
    { Rule table handle                    }
    { Buffer for resulting Unicode         }
    { Size of results buffer               }

    { Buffer with source local code        }
    { No map character                     }
    { Number of unicode chars in output    }
    { Flag indicating default map is allowable  }

function NWLocalToUnicode(ruleHandle: Tnptr; dest: punicode; maxLen: Tnuint32; src: pointer; noMap: Tunicode;
  len: pnuint; allowNoMapFlag: Tnuint32): Tnint; NWLIB_LOCNLM32;
    { NWUSUnicodeToByte or NWUXUnicodeToByte are preferred  }
    { Convert Unicode to local code        }
    { Rule table handle                    }
    { Buffer for resulting local code      }
    { Size of results buffer               }

    { Buffer with source Unicode           }
    { No Map character                     }
    { Number of bytes in output            }
    { Flag indicating default map is allowable  }
function NWUnicodeToLocal(ruleHandle: Tnptr; dest: Tnptr; maxLen: Tnuint32; src: Punicode; noMap: Tnuint8;
  len: pnuint; allowNoMapFlag: Tnuint32): Tnint; NWLIB_LOCNLM32;

    { was #define dname(params) para_def_expr }
    //function NWLocalToUnicode(P1,P2,P3,P4,P5,P6 : longint) : longint;

    { was #define dname(params) para_def_expr }
    //function NWUnicodeToLocal(P1,P2,P3,P4,P5,P6 : longint) : longint;

    { If I could make size_t be cardinal for N_PLAT_NLM all of the functions  }
    { below here could be single sourced.                                    }
{$IF 0}
    { Convert Unicode to collation         }
    { Rule table handle                    }
    { Buffer for resulting Unicode weights }
    { Size of results buffer               }

    { Buffer with source Unicode           }
    { No map character                     }
    { Number of unicode chars in output    }

function NWUnicodeToCollation(ruleHandle: Tnptr; dest: punicode; maxLen: Tnuint32; src: Punicode; noMap: Tunicode;
  len: pnuint32): Tnint; NWLIB_LOCNLM32;
    { Compare two unicode characters       }
    { Rule table handle                    }
    { 1st character                        }
    { 2nd character                        }
function NWUnicodeCompare(ruleHandle: Tnptr; chr1: Tunicode; chr2: Tunicode): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to collation         }
    { Rule table handle                    }
    { Buffer for resulting Unicode weights }
    { Size of results buffer               }

    { Buffer with source Unicode           }
    { Number of unicode chars in output    }
function NWUnicodeToMonocase(ruleHandle: Tnptr; dest: punicode; maxLen: Tnuint32; src: Punicode; len: pnuint32): Tnint; NWLIB_LOCNLM32;
{$ENDIF}

    {  not N_PLAT_NLM   }
    { NWUSByteToUnicode or NWUXByteToUnicode are preferred  }
    { Convert local to Unicode             }
    { Rule table handle                    }
    { Buffer for resulting Unicode         }
    { Size of results buffer               }

    { Buffer with source local code        }
    { No map character                     }
    { Number of unicode chars in output    }

function NWLocalToUnicode(ruleHandle: Tnptr; dest: punicode; maxLen: Tsize_t; src: Pnuint8; noMap: Tunicode;
  len: Psize_t): Tnint; NWLIB_LOCNLM32;
    { NWUSUnicodeToByte or NWUXUnicodeToByte are preferred  }
    { Convert Unicode to local code        }
    { Rule table handle                    }
    { Buffer for resulting local code      }
    { Size of results buffer               }

    { Buffer with source Unicode           }
    { No Map character                     }
    { Number of bytes in output            }
function NWUnicodeToLocal(ruleHandle: Tnptr; dest: pnuint8; maxLen: Tsize_t; src: Punicode; noMap: byte;
  len: Psize_t): Tnint; NWLIB_LOCNLM32;

    { not N_PLAT_NLM  }
    { Convert Unicode to collation         }
    { Rule table handle                    }
    { Buffer for resulting Unicode weights }
    { Size of results buffer               }

    { Buffer with source Unicode           }
    { No map character                     }
    { Number of unicode chars in output    }

function NWUnicodeToCollation(ruleHandle: Tnptr; dest: punicode; maxLen: Tsize_t; src: Punicode; noMap: Tunicode;
  len: Psize_t): Tnint; NWLIB_LOCNLM32;
    { Compare two unicode characters       }
    { Rule table handle                    }
    { 1st character                        }
    { 2nd character                        }
function NWUnicodeCompare(ruleHandle: Tnptr; chr1: Tunicode; chr2: Tunicode): Tnint; NWLIB_LOCNLM32;
    { Convert Unicode to collation         }
    { Rule table handle                    }
    { Buffer for resulting Unicode weights }
    { Size of results buffer               }

    { Buffer with source Unicode           }
    { Number of unicode chars in output    }
function NWUnicodeToMonocase(ruleHandle: Tnptr; dest: punicode; maxLen: Tsize_t; src: Punicode; len: Psize_t): Tnint; NWLIB_LOCNLM32;
    {
     *    Functions that work with XLate Tables
      }
{$IFDEF 0} // defined N_PLAT_DOS && defined N_UNI_NEW_TABLES}

const
  N_UNI_LOAD_MONOCASE = $0001;
  N_UNI_LOAD_COLLATION = $0002;

function NWLInitXlateTables(codePage: Tnint; flags: Tnflag8): Tnint; NWLIB_UNKNOWN;
function NWLFreeXlateTables: Tnint; NWLIB_UNKNOWN;
    { Name of the rule table               }
    { Where to put the rule table handle   }
function NWLLoadXlateTable(ruleTableName: pnstr; ruleHandle: pnptr): Tnint; NWLIB_UNKNOWN;

    { Rule table handle                    }
function NWLUnloadXlateTable(ruleHandle: pointer): Tnint; NWLIB_UNKNOWN;
{function NWInitUnicodeTables(CountryCode,CodePage : longint) : longint;}


const
  NWFreeUnicodeTables = NWLFreeXlateTables;
  NWLoadRuleTable = NWLLoadXlateTable;
  NWUnloadRuleTable = NWLUnloadXlateTable;
{$ENDIF}

function NWGetUnicodeToLocalHandle(handle: pnptr): Tnint; NWLIB_LOCNLM32;
function NWGetLocalToUnicodeHandle(handle: pnptr): Tnint; NWLIB_LOCNLM32;
function NWGetMonocaseHandle(handle: pnptr): Tnint; NWLIB_LOCNLM32;
function NWGetCollationHandle(handle: pnptr): Tnint; NWLIB_LOCNLM32;
    {************************************************************************** }
    {
        Redefine these functions to use the new unicode API monocase routines.
     }
    { was #define dname(params) para_def_expr }
    //function uniicmp(s1,s2 : longint) : longint;

    { was #define dname(params) para_def_expr }
    //function uninicmp(s1,s2,l : longint) : longint;

    { Unicode string functions that work like those in string.h }
    { Corresponds to strcat     }
    { Original string                      }

    { String to be appended                }

function unicat(s1: punicode; s2: Punicode): punicode; NWLIB_LOCNLM32;
    { Corresponds to strchr     }

    { String to be scanned                 }
    { Character to be found                }
function unichr(s: Punicode; c: Tunicode): punicode; NWLIB_LOCNLM32;
    { Corresponds to strcpy     }
    { Destination string                   }

    { Source string                        }
function unicpy(s1: punicode; s2: Punicode): punicode; NWLIB_LOCNLM32;
    { Corresponds to strcspn    }

    { String to be scanned                 }

    { Character set                        }
function unicspn(s1: Punicode; s2: Punicode): Tsize_t; NWLIB_LOCNLM32;
    { Corresponds to strlen     }

    { String to determine length of        }
function unilen(s: Punicode): Tsize_t; NWLIB_LOCNLM32;
    { Corresponds to strncat    }
    { Original string                      }

    { String to be appended                }
    { Maximum characters to be appended    }
function unincat(s1: punicode; s2: Punicode; n: Tsize_t): punicode; NWLIB_LOCNLM32;
    { Corresponds to strncpy    }
    { Destination string                   }

    { Source string                        }
    { Maximum length                       }
function unincpy(s1: punicode; s2: Punicode; n: Tsize_t): punicode; NWLIB_LOCNLM32;
    { Corresponds to strnset    }
    { String to be modified                }
    { Fill character                       }
    { Maximum length                       }
function uninset(s: punicode; c: Tunicode; n: Tsize_t): punicode; NWLIB_LOCNLM32;
    { Corresponds to strpbrk    }

    { String to be scanned                 }

    { Character set                        }
function unipbrk(s1: Punicode; s2: Punicode): punicode; NWLIB_LOCNLM32;
    { Corresponds to strpcpy  }
    { Destination string                   }

    { Source string                        }
function unipcpy(s1: punicode; s2: Punicode): punicode; NWLIB_LOCNLM32;
    { Corresponds to strrchr    }

    { String to be scanned                 }
    { Character to be found                }
function unirchr(s: Punicode; c: Tunicode): punicode; NWLIB_LOCNLM32;
    { Corresponds to strrev     }
    { String to be reversed                }
function unirev(s: punicode): punicode; NWLIB_LOCNLM32;
    { Corresponds to strset     }
    { String to modified                   }
    { Fill character                       }
function uniset(s: punicode; c: Tunicode): punicode; NWLIB_LOCNLM32;
    { Corresponds to strspn     }

    { String to be tested                  }

    { Character set                        }
function unispn(s1: Punicode; s2: Punicode): Tsize_t; NWLIB_LOCNLM32;
    { Corresponds to strstr     }

    { String to be scanned                 }

    { String to be located                 }
function unistr(s1: Punicode; s2: Punicode): punicode; NWLIB_LOCNLM32;
    { Corresponds to strtok     }
    { String to be parsed                  }

    { Delimiter values                     }
function unitok(s1: punicode; s2: Punicode): punicode; NWLIB_LOCNLM32;
    { Corresponds to stricmp    }

    { 1st string to be compared            }

    { 2nd string to be compared            }
function uniicmp(s1: Punicode; s2: Punicode): Tnint; NWLIB_LOCNLM32;
    { Corresponds to strnicmp   }

    { 1st string to be compared            }

    { 2nd string to be compared            }
    { Maximum length                       }
function uninicmp(s1: Punicode; s2: Punicode; len: Tsize_t): Tnint; NWLIB_LOCNLM32;
    { Unicode compare           }


function unicmp(s1: Punicode; s2: Punicode): Tnint; NWLIB_LOCNLM32;
    { Unicode length compare   }


function unincmp(s1: Punicode; s2: Punicode; len: Tsize_t): Tnint; NWLIB_LOCNLM32;
    { Corresponds to sizeof     }

function unisize(s: Punicode): Tsize_t; NWLIB_LOCNLM32;
    {
     * UTF-8  <--> Unicode Conversion APIS
      }

function NWLUnicodeToUTF8(uniStr: Punicode; maxSize: Tnuint; utf8Str: pnuint8; utf8Size: pnuint): Tnint; NWLIB_LOCNLM32;

function NWLUTF8ToUnicode(utf8Str: Pnuint8; maxSize: Tnuint; uniStr: punicode; uniSize: pnuint; badSequence: ppnstr): Tnint; NWLIB_LOCNLM32;

function NWLUTF8ToUnicodeSize(utf8Str: Pnuint8; size: pnuint): Tnint; NWLIB_LOCNLM32;

function NWLUnicodeToUTF8Size(uniStr: Punicode): Tnuint; NWLIB_LOCNLM32;


//**************************************************************************
// nwbindry.h
//**************************************************************************

    { Bindery object types (in HIGH-LOW order)  }

const
  OT_WILD = $FFFF;
  OT_UNKNOWN = $0000;
  OT_USER = $0100;
  OT_USER_GROUP = $0200;
  OT_PRINT_QUEUE = $0300;
  OT_FILE_SERVER = $0400;
  OT_JOB_SERVER = $0500;
  OT_GATEWAY = $0600;
  OT_PRINT_SERVER = $0700;
  OT_ARCHIVE_QUEUE = $0800;
  OT_ARCHIVE_SERVER = $0900;
  OT_JOB_QUEUE = $0A00;
  OT_ADMINISTRATION = $0B00;
  OT_NAS_SNA_GATEWAY = $2100;
  OT_REMOTE_BRIDGE_SERVER = $2600;
  OT_TCPIP_GATEWAY = $2700;
  OT_TREE_NAME = $7802;
    { Extended bindery object types  }
  OT_TIME_SYNCHRONIZATION_SERVER = $2D00;
  OT_ARCHIVE_SERVER_DYNAMIC_SAP = $2E00;
  OT_ADVERTISING_PRINT_SERVER = $4700;
  OT_BTRIEVE_VAP = $5000;
  OT_PRINT_QUEUE_USER = $5300;
    { Bindery object and property flags  }
  BF_STATIC = $00;
  BF_DYNAMIC = $01;
  BF_ITEM = $00;
  BF_SET = $02;
    {********  Bindery object and property security access levels  ********* }
  BS_ANY_READ = $00; // Readable by anyone
  BS_LOGGED_READ = $01; // Must be logged in to read
  BS_OBJECT_READ = $02; // Readable by same object or super
  BS_SUPER_READ = $03; // Readable by supervisor only
  BS_BINDERY_READ = $04; // Readable only by the bindery
  BS_ANY_WRITE = $00; // Writeable by anyone
  BS_LOGGED_WRITE = $10; // Must be logged in to write
  BS_OBJECT_WRITE = $20; // Writeable by same object or super
  BS_SUPER_WRITE = $30; // Writeable only by the supervisor
  BS_BINDERY_WRITE = $40; // Writeable by the bindery only



function NWVerifyObjectPassword
  (conn: TNWCONN_HANDLE;
  objName: Pnstr8;
  objType: Tnuint16;
  password: Pnstr8): TNWCCODE; NWLIB_CALNLM32;


function NWDisallowObjectPassword(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; disallowedPassword: Pnstr8): TNWCCODE; NWLIB_CALNLM32;



function NWChangeObjectPassword(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; oldPassword: Pnstr8; newPassword: Pnstr8): TNWCCODE; NWLIB_CALNLM32;


function NWReadPropertyValue(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; propertyName: Pnstr8; segmentNum: Tnuint8;
  segmentData: pnuint8; moreSegments: pnuint8; flags: pnuint8): TNWCCODE; NWLIB_CALNLM32;



function NWWritePropertyValue(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; propertyName: Pnstr8; segmentNum: Tnuint8;
  segmentData: Pnuint8; moreSegments: Tnuint8): TNWCCODE; NWLIB_CALNLM32;



function NWAddObjectToSet(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; propertyName: Pnstr8; memberName: Pnstr8;
  memberType: Tnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWDeleteObjectFromSet(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; propertyName: Pnstr8; memberName: Pnstr8;
  memberType: Tnuint16): TNWCCODE; NWLIB_CALNLM32;



function NWIsObjectInSet(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; propertyName: Pnstr8; memberName: Pnstr8;
  memberType: Tnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWScanProperty(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; searchPropertyName: Pnstr8; iterHandle: pnuint32;
  propertyName: Pnstr8; propertyFlags: pnuint8; propertySecurity: pnuint8; valueAvailable: pnuint8; moreFlag: pnuint8): TNWCCODE; NWLIB_CALNLM32;

function NWGetObjectID(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; objID: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetObjectDiskSpaceLeft(conn: TNWCONN_HANDLE; objID: Tnuint32; systemElapsedTime: pnuint32; unusedDiskBlocks: pnuint32; restrictionEnforced: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWGetObjectName(conn: TNWCONN_HANDLE; objID: Tnuint32; objName: Pnstr8; objType: pnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWScanObject(conn: TNWCONN_HANDLE; searchName: Pnstr8; searchType: Tnuint16; objID: pnuint32; objName: Pnstr8;
  objType: pnuint16; hasPropertiesFlag: pnuint8; objFlags: pnuint8; objSecurity: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWGetBinderyAccessLevel(conn: TNWCONN_HANDLE; accessLevel: pnuint8; objID: pnuint32): TNWCCODE; NWLIB_CALNLM32;


function NWCreateProperty(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; propertyName: Pnstr8; propertyFlags: Tnuint8;
  propertySecurity: Tnuint8): TNWCCODE; NWLIB_CALNLM32;


function NWDeleteProperty(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; propertyName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;


function NWChangePropertySecurity(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; propertyName: Pnstr8; newPropertySecurity: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWCreateObject(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; objFlags: Tnuint8; objSecurity: Tnuint8): TNWCCODE; NWLIB_CALNLM32;

function NWDeleteObject(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16): TNWCCODE; NWLIB_CALNLM32;


function NWRenameObject(conn: TNWCONN_HANDLE; oldObjName: Pnstr8; newObjName: Pnstr8; objType: Tnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWChangeObjectSecurity(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; newObjSecurity: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWOpenBindery(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWCloseBindery(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWScanObjectTrusteePaths(conn: TNWCONN_HANDLE; objID: Tnuint32; volNum: Tnuint16; iterHandle: pnuint16; accessRights: pnuint8;
  dirPath: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWScanObjectTrusteePathsExt(conn: TNWCONN_HANDLE; objID: Tnuint32; volNum: Tnuint16; iterHandle: pnuint16; accessRights: pnuint8;
  dirPath1506: Pnstr8): TNWCCODE; NWLIB_UNKNOWN;

function NWGetObjectEffectiveRights(conn: TNWCONN_HANDLE; objID: Tnuint32; dirHandle: TNWDIR_HANDLE; path: Pnstr8; rightsMask: pnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWGetObjectEffectiveRightsExt(conn: TNWCONN_HANDLE; objID: Tnuint32; dirHandle: TNWDIR_HANDLE; path: Pnstr8; buNameSpace: Tnuint8;
  rightsMask: pnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWGetObjectEffectiveRights2(conn: TNWCONN_HANDLE; objID: Tnuint32; dirHandle: TNWDIR_HANDLE; path: Pnstr8; rightsMask: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWGetObjectNamesBeginA(luObjectType: Tnuint32; pluHandle: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetObjectNamesNextA(luHandle: Tnuint32; pluLenBuffer: pnuint32; strBuffer: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWGetObjectNamesEndA(luHandle: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetObjectNamesBeginW(luObjectType: Tnuint32; pluHandle: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetObjectNamesNextW(luHandle: Tnuint32; pluLenBuffer: pnuint32; strBuffer: punicode): TNWCCODE; NWLIB_CALNLM32;
function NWGetObjectNamesEndW(luHandle: Tnuint32): TNWCCODE; NWLIB_CALNLM32;


//*****************************************************************************
//nwconnec.h
//*****************************************************************************

    { 3.11 and above only: 0=not in use, 2=NCP over IPX, 4=AFP  }
type
  PtNWINET_ADDR = ^TtNWINET_ADDR;
  TtNWINET_ADDR = record
    networkAddr: array[0..3] of Tnuint8;
    netNodeAddr: array[0..5] of Tnuint8;
    socket: Tnuint16;
    connType: Tnuint16;
  end;
  TNWINET_ADDR = TtNWINET_ADDR;
  PNWINET_ADDR = ^TNWINET_ADDR;

const
  CONNECTION_AVAILABLE = $0001;
    { obsolete  }
  CONNECTION_PRIVATE = $0002;
  CONNECTION_LOGGED_IN = $0004;
  CONNECTION_LICENSED = $0004;
  CONNECTION_BROADCAST_AVAILABLE = $0008;
  CONNECTION_ABORTED = $0010;
  CONNECTION_REFUSE_GEN_BROADCAST = $0020;
  CONNECTION_BROADCASTS_DISABLED = $0040;
  CONNECTION_PRIMARY = $0080;
  CONNECTION_NDS = $0100;
    { obsolete  }
  CONNECTION_PNW = $4000;
  CONNECTION_AUTHENTICATED = $8000;
    { End of new connection model calls.  }

function NWLockConnection(connHandle: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWGetConnectionInformation(connHandle: TNWCONN_HANDLE; connNumber: Tnuint16; pObjName: Pnstr8; pObjType: pnuint16; pObjID: pnuint32;
  pLoginTime: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWGetInternetAddress(connHandle: TNWCONN_HANDLE; connNumber: Tnuint16; pInetAddr: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWGetInetAddr(connHandle: TNWCONN_HANDLE; connNum: Tnuint16; pInetAddr: PNWINET_ADDR): TNWCCODE; NWLIB_CALNLM32;
function NWClearConnectionNumber(connHandle: TNWCONN_HANDLE; connNumber: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWGetDefaultConnRef(pConnReference: pnuint32): TNWCCODE; NWLIB_UNKNOWN;

function NWGetObjectConnectionNumbers(connHandle: TNWCONN_HANDLE; pObjName: Pnstr8; objType: Tnuint16; pNumConns: pnuint16; pConnHandleList: pnuint16;
  maxConns: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWGetConnListFromObject(connHandle: TNWCONN_HANDLE; objID: Tnuint32; searchConnNum: Tnuint32; pConnListLen: pnuint16; pConnList: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetPreferredServer(pConnHandle: PNWCONN_HANDLE): TNWCCODE; NWLIB_UNKNOWN;
function NWSetPreferredServer(connHandle: TNWCONN_HANDLE): TNWCCODE; NWLIB_UNKNOWN;

    { The NLM LibC x-plat libraries do not support obsolete apis }

//*****************************************************************************
// nwdel.h
//*****************************************************************************

type

  PNWDELETED_INFO = ^TNWDELETED_INFO;
  TNWDELETED_INFO = record
    sequence: Tnuint32;
    parent: Tnuint32;
    attributes: Tnuint32;
    uniqueID: Tnuint8;
    flags: Tnuint8;
    nameSpace: Tnuint8;
    nameLength: Tnuint8;
    name: array[0..255] of Tnuint8;
    creationDateAndTime: Tnuint32;
    ownerID: Tnuint32;
    lastArchiveDateAndTime: Tnuint32;
    lastArchiverID: Tnuint32;
    updateDateAndTime: Tnuint32;
    updatorID: Tnuint32;
    fileSize: Tnuint32;
    reserved: array[0..43] of Tnuint8;
    inheritedRightsMask: Tnuint16;
    lastAccessDate: Tnuint16;
    deletedTime: Tnuint32;
    deletedDateAndTime: Tnuint32;
    deletorID: Tnuint32;
    reserved3: array[0..15] of Tnuint8;
  end;

  PNWDELETED_INFO_EXT = ^TNWDELETED_INFO_EXT;
  TNWDELETED_INFO_EXT = record
    sequence: Tnuint32;
    parent: Tnuint32;
    attributes: Tnuint32;
    uniqueID: Tnuint8;
    flags: Tnuint8;
    nameSpace: Tnuint8;
    nameLength: Tnuint16;
    name: array[0..765] of Tnuint8;
    creationDateAndTime: Tnuint32;
    ownerID: Tnuint32;
    lastArchiveDateAndTime: Tnuint32;
    lastArchiverID: Tnuint32;
    updateDateAndTime: Tnuint32;
    updatorID: Tnuint32;
    fileSize: Tnuint32;
    reserved: array[0..43] of Tnuint8;
    inheritedRightsMask: Tnuint16;
    lastAccessDate: Tnuint16;
    deletedTime: Tnuint32;
    deletedDateAndTime: Tnuint32;
    deletorID: Tnuint32;
    reserved3: array[0..15] of Tnuint8;
  end;


function NWPurgeDeletedFile(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; iterHandle: Tnuint32; volNum: Tnuint32; dirBase: Tnuint32;
  fileName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWRecoverDeletedFile(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; iterHandle: Tnuint32; volNum: Tnuint32; dirBase: Tnuint32;
  delFileName: Pnstr8; rcvrFileName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWRecoverDeletedFileExt(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; iterHandle: Tnuint32; volNum: Tnuint32; dirBase: Tnuint32;
  delFileName: Pnstr8; rcvrFileName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWScanForDeletedFiles(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; iterHandle: pnuint32; volNum: pnuint32; dirBase: pnuint32;
  entryInfo: PNWDELETED_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWScanForDeletedFilesExt(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; iterHandle: pnuint32; volNum: pnuint32; dirBase: pnuint32;
  entryInfo: PNWDELETED_INFO_EXT): TNWCCODE; NWLIB_CALNLM32;


//*****************************************************************************
//nwdentry.h
//*****************************************************************************

type
  PTRUSTEE_INFO = ^TTRUSTEE_INFO;
  TTRUSTEE_INFO = record
    objectID: Tnuint32;
    objectRights: Tnuint16;
  end;

  PNW_LIMIT_LIST = ^TNW_LIMIT_LIST;
  TNW_LIMIT_LIST = record
    numEntries: Tnuint8;
    list: array[0..101] of record
      level: Tnuint8;
      max: Tnuint32;
      current: Tnuint32;
    end;
  end;


  PNWET_INFO = ^TNWET_INFO;
  TNWET_INFO = record
    entryName: array[0..15] of Tnstr8;
    creationDateAndTime: Tnuint32;
    ownerID: Tnuint32;
    sequenceNumber: Tnuint32;
    trusteeList: array[0..19] of TTRUSTEE_INFO;
  end;

  PNWET_INFO_EXT = ^TNWET_INFO_EXT;
  TNWET_INFO_EXT = record
    entryName: array[0..15] of Tnstr8;
    creationDateAndTime: Tnuint32;
    ownerID: Tnuint32;
    sequenceNumber: Tnuint32;
    trusteeList: array[0..99] of TTRUSTEE_INFO;
  end;

  PNWFILE_INFO = ^TNWFILE_INFO;
  TNWFILE_INFO = record
    updateDateAndTime: Tnuint32;
    updatorID: Tnuint32;
    fileSize: Tnuint32;
    reserved: array[0..43] of Tnuint8;
    inheritedRightsMask: Tnuint16;
    lastAccessDate: Tnuint16;
    reserved2: array[0..27] of Tnuint8;
  end;

  PNWDIR_INFO = ^TNWDIR_INFO;
  TNWDIR_INFO = record
    lastModifyDateAndTime: Tnuint32;
    nextTrusteeEntry: Tnuint32;
    reserved: array[0..47] of Tnuint8;
    maximumSpace: Tnuint32;
    inheritedRightsMask: Tnuint16;
    reserved2: array[0..13] of Tnuint8;
    volObjectID: Tnuint32;
    reserved3: array[0..7] of Tnuint8;
  end;

  PNWENTRY_INFO = ^TNWENTRY_INFO;
  TNWENTRY_INFO = record
    sequence: Tnuint32;
    parent: Tnuint32;
    attributes: Tnuint32;
    uniqueID: Tnuint8;
    flags: Tnuint8;
    nameSpace: Tnuint8;
    nameLength: Tnuint8;
    name: array[0..11] of Tnuint8;
    creationDateAndTime: Tnuint32;
    ownerID: Tnuint32;
    lastArchiveDateAndTime: Tnuint32;
    lastArchiverID: Tnuint32;
    info: record
      case longint of
        0: (_file: TNWFILE_INFO);
        1: (dir: TNWDIR_INFO);
    end;
  end;
    { file size  }

  PNW_EXT_FILE_INFO = ^TNW_EXT_FILE_INFO;
  TNW_EXT_FILE_INFO = record
    sequence: Tnuint32;
    parent: Tnuint32;
    attributes: Tnuint32;
    uniqueID: Tnuint8;
    flags: Tnuint8;
    nameSpace: Tnuint8;
    nameLength: Tnuint8;
    name: array[0..11] of Tnuint8;
    creationDateAndTime: Tnuint32;
    ownerID: Tnuint32;
    lastArchiveDateAndTime: Tnuint32;
    lastArchiverID: Tnuint32;
    updateDateAndTime: Tnuint32;
    lastUpdatorID: Tnuint32;
    dataForkSize: Tnuint32;
    dataForkFirstFAT: Tnuint32;
    nextTrusteeEntry: Tnuint32;
    reserved: array[0..35] of Tnuint8;
    inheritedRightsMask: Tnuint16;
    lastAccessDate: Tnuint16;
    deletedFileTime: Tnuint32;
    deletedDateAndTime: Tnuint32;
    deletorID: Tnuint32;
    reserved2: array[0..15] of Tnuint8;
    otherForkSize: array[0..1] of Tnuint32;
  end;

const
  TR_NONE = $0000;
  TR_READ = $0001;
  TR_WRITE = $0002;
  TR_OPEN = $0004;
  TR_DIRECTORY = $0004;
  TR_CREATE = $0008;
  TR_DELETE = $0010;
  TR_ERASE = $0010;
  TR_OWNERSHIP = $0020;
  TR_ACCESS_CTRL = $0020;
  TR_FILE_SCAN = $0040;
  TR_SEARCH = $0040;
  TR_FILE_ACCESS = $0040;
  TR_MODIFY = $0080;
  TR_ALL = $01FB;
  TR_SUPERVISOR = $0100;
  TR_NORMAL = $00FB;

  MModifyNameBit = $0001;
  MFileAttributesBit = $0002;
  MCreateDateBit = $0004;
  MCreateTimeBit = $0008;
  MOwnerIDBit = $0010;
  MLastArchivedDateBit = $0020;
  MLastArchivedTimeBit = $0040;
  MLastArchivedIDBit = $0080;
  MLastUpdatedDateBit = $0100;
  MLastUpdatedTimeBit = $0200;
  MLastUpdatedIDBit = $0400;
  MLastAccessedDateBit = $0800;
  MInheritedRightsMaskBit = $1000;
  MMaximumSpaceBit = $2000;


function NWDeleteTrustee(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; dirPath: Pnstr8; objID: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWDeleteTrusteeExt(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; dirPath: Pnstr8; objID: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWAddTrustee(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; objID: Tnuint32; rightsMask: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWAddTrusteeExt(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; objID: Tnuint32; rightsMask: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWIntScanDirEntryInfo(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; attrs: Tnuint16; iterHandle: pnuint32; searchPattern: Pnuint8;
  entryInfo: PNWENTRY_INFO; augmentFlag: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
    { was #define dname(params) para_def_expr }
    // function NWScanForTrustees(a,b,c,d,e,f : longint) : longint;


function NWIntScanForTrustees(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; iterHandle: pnuint32; numOfEntries: pnuint16;
  entryTrusteeInfo: PNWET_INFO; augmentFlag: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
    { was #define dname(params) para_def_expr }
    //function NWScanForTrusteesExt(a,b,c,d,e,f : longint) : longint;


function NWIntScanForTrusteesExt(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; iterHandle: pnuint32; numOfEntries: pnuint16;
  entryTrusteeInfo: PNWET_INFO_EXT; augmentFlag: Tnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWIntMoveDirEntry(conn: TNWCONN_HANDLE; searchAttrs: Tnuint8; srcDirHandle: TNWDIR_HANDLE; srcPath: Pnstr8; dstDirHandle: TNWDIR_HANDLE;
  dstPath: Pnstr8; augmentFlag: Tnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWSetDirEntryInfo(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; searchAttrs: Tnuint8; iterHandle: Tnuint32; changeBits: Tnuint32;
  newEntryInfo: PNWENTRY_INFO): TNWCCODE; NWLIB_CALNLM32;

function NWIntScanExtendedInfo(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; attrs: Tnuint8; iterHandle: pnuint32; searchPattern: Pnstr8;
  entryInfo: PNW_EXT_FILE_INFO; augmentFlag: Tnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWGetEffectiveRights(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; effectiveRights: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWGetEffectiveRightsExt(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; effectiveRights: pnuint16): TNWCCODE; NWLIB_CALNLM32;


//*****************************************************************************
//nwdirect.h
//*****************************************************************************

    { set to zero if a dirHandle is present  }
    {....when the NWGetDIrSpaceInfo() is called  }
type

  PDIR_SPACE_INFO = ^TDIR_SPACE_INFO;
  TDIR_SPACE_INFO = record
    totalBlocks: Tnuint32;
    availableBlocks: Tnuint32;
    purgeableBlocks: Tnuint32;
    notYetPurgeableBlocks: Tnuint32;
    totalDirEntries: Tnuint32;
    availableDirEntries: Tnuint32;
    reserved: Tnuint32;
    sectorsPerBlock: Tnuint8;
    volLen: Tnuint8;
    volName: array[0..(NW_MAX_VOLUME_NAME_LEN) - 1] of Tnuint8;
  end;
    { Trustee Access Rights in a network directory  }
    { NOTE: TA_OPEN is obsolete in 3.x  }

const
  TA_NONE = $00;
  TA_READ = $01;
  TA_WRITE = $02;
  TA_OPEN = $04;
  TA_CREATE = $08;
  TA_DELETE = $10;
  TA_OWNERSHIP = $20;
  TA_SEARCH = $40;
  TA_MODIFY = $80;
  TA_ALL = $FB;


function NWAddTrusteeToDirectory(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; trusteeID: Tnuint32; rightsMask: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWDeleteTrusteeFromDirectory(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; objID: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetEffectiveDirectoryRights(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; rightsMask: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWModifyMaximumRightsMask(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; revokeRightsMask: Tnuint8; grantRightsMask: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWScanDirectoryForTrustees(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; searchPath: Pnstr8; iterHandle: pnuint16; dirName: Pnstr8;
  dirDateTime: pnuint32; ownerID: pnuint32; trusteeIDs: pnuint32; trusteeRights: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWScanDirectoryForTrustees2(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; searchPath: Pnstr8; iterHandle: pnuint32; dirName: Pnstr8;
  dirDateTime: pnuint32; ownerID: pnuint32; trusteeList: PTRUSTEE_INFO): TNWCCODE; NWLIB_CALNLM32;
    { was #define dname(params) para_def_expr }
    // function NWScanDirectoryInformation(a,b,c,d,e,f,g,h : longint) : longint;


function NWIntScanDirectoryInformation(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; searchPath: Pnstr8; iterHandle: pnuint16; dirName: Pnstr8;
  dirDateTime: pnuint32; ownerID: pnuint32; rightsMask: pnuint8; augmentFlag: Tnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWIntScanDirectoryInformation2(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; searchPath: Pnstr8; sequence: pnuint8; dirName: Pnstr8;
  dirDateTime: pnuint32; ownerID: pnuint32; rightsMask: pnuint8; augmentFlag: Tnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWSetDirectoryInformation(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; dirDateTime: Tnuint32; ownerID: Tnuint32;
  rightsMask: Tnuint8): TNWCCODE; NWLIB_CALNLM32;

function NWAllocPermanentDirectoryHandle(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; dirPath: Pnstr8; newDirHandle: PNWDIR_HANDLE; effectiveRights: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWAllocTemporaryDirectoryHandle(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; dirPath: Pnstr8; newDirHandle: PNWDIR_HANDLE; rightsMask: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWDeallocateDirectoryHandle(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWSetDirectoryHandlePath(conn: TNWCONN_HANDLE; sourceDirHandle: TNWDIR_HANDLE; dirPath: Pnstr8; destDirHandle: TNWDIR_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWGetDirectoryHandlePath(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; dirPath: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWCreateDirectory(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; dirPath: Pnstr8; accessMask: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWDeleteDirectory(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; dirPath: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWRenameDirectory(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; oldName: Pnstr8; newName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWSetDirSpaceLimit(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; spaceLimit: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetDirSpaceLimitList(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; returnBuf: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWGetDirSpaceLimitList2(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; limitList: PNW_LIMIT_LIST): TNWCCODE; NWLIB_CALNLM32;
function NWGetDirSpaceInfo(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; volNum: Tnuint16; spaceInfo: PDIR_SPACE_INFO): TNWCCODE; NWLIB_CALNLM32;

const
  TF_NORMAL = $0000;
  TF_READ_ONLY = $0001;
  TF_HIDDEN = $0002;
  TF_SYSTEM = $0004;
  TF_EXECUTE_ONLY = $0008;
  TF_DIRECTORY = $0010;
  TF_NEEDS_ARCHIVED = $0020;
  TF_EXECUTE_CONFIRM = $0040;
  TF_SHAREABLE = $0080;
  TF_LOW_SEARCH_BIT = $0100;
  TF_MID_SEARCH_BIT = $0200;
  TF_HI_SEARCH_BIT = $0400;
  TF_PRIVATE = $0800;
  TF_TRANSACTIONAL = $1000;
  TF_INDEXED = $2000;
  TF_READ_AUDIT = $4000;
  TF_WRITE_AUDIT = $8000;
  TF_PURGE = $10000;
  TF_RENAME_INHIBIT = $20000;
  TF_DELETE_INHIBIT = $40000;
  TF_COPY_INHIBIT = $80000;
  TF_AUDITING_BIT = $00100000;
    { DIRECTORY ATTRIBUTES  }

  TD_HIDDEN = TF_HIDDEN;
  TD_SYSTEM = TF_SYSTEM;
  TD_PURGE = TF_PURGE;
  TD_PRIVATE = TF_PRIVATE;
  TD_VISIBLE = TF_PRIVATE;
  TD_RENAME_INHIBIT = TF_RENAME_INHIBIT;
  TD_DELETE_INHIBIT = TF_DELETE_INHIBIT;


//*****************************************************************************
//nwdpath.h
//*****************************************************************************


const
  NW_UNMAPPED_DRIVE = $0000;
  NW_FREE_DRIVE = $0000;
  NW_CDROM_DRIVE = $0400;
  NW_LOCAL_FREE_DRIVE = $0800;
  NW_LOCAL_DRIVE = $1000;
  NW_NETWORK_DRIVE = $2000;
  NW_LITE_DRIVE = $4000;
  NW_PNW_DRIVE = $4000;
  NW_NETWARE_DRIVE = $8000;
    { return error for NWGetDriveStatus  }
  NW_INVALID_DRIVE = 15;
    { defined for pathFormat parameter in NWGetDriveStatus  }
  NW_FORMAT_NETWARE = 0;
  NW_FORMAT_SERVER_VOLUME = 1;
  NW_FORMAT_DRIVE = 2;
  NW_FORMAT_UNC = 3;


function NWSetDriveBase(driveNum: Tnuint16; conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; dirPath: Pnstr8; driveScope: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWSetInitDrive(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWSetSearchDriveVector(vectorBuffer: Pnstr8): TNWCCODE; NWLIB_UNKNOWN;
function NWGetSearchDriveVector(vectorBuffer: Pnstr8): TNWCCODE; NWLIB_UNKNOWN;
function NWDeleteDriveBase(driveNum: Tnuint16; driveScope: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
    { 3.x & 4.x file servers  }
function NWGetPathFromDirectoryBase(conn: TNWCONN_HANDLE; volNum: Tnuint8; dirBase: Tnuint32; namSpc: Tnuint8; len: pnuint8;
  pathName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
    { 2.x file servers only  }
function NWGetPathFromDirectoryEntry(conn: TNWCONN_HANDLE; volNum: Tnuint8; dirEntry: Tnuint16; len: pnuint8; pathName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWGetDrivePathConnRef(driveNum: Tnuint16; mode: Tnuint16; connRef: pnuint32; basePath: Pnstr8; driveScope: pnuint16): TNWCCODE; NWLIB_UNKNOWN;
function NWGetDrivePath(driveNum: Tnuint16; mode: Tnuint16; conn: PNWCONN_HANDLE; basePath: Pnstr8; driveScope: pnuint16): TNWCCODE; NWLIB_UNKNOWN;
function NWGetDriveInformation(driveNum: Tnuint16; mode: Tnuint16; conn: PNWCONN_HANDLE; dirHandle: PNWDIR_HANDLE; driveScope: pnuint16;
  dirPath: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWGetDriveInfoConnRef(driveNum: Tnuint16; mode: Tnuint16; connRef: pnuint32; dirHandle: PNWDIR_HANDLE; driveScope: pnuint16;
  dirPath: Pnstr8): TNWCCODE; NWLIB_UNKNOWN;
function NWGetDriveStatus(driveNum: Tnuint16; pathFormat: Tnuint16; status: pnuint16; conn: PNWCONN_HANDLE; rootPath: Pnstr8;
  relPath: Pnstr8; fullPath: Pnstr8): TNWCCODE; NWLIB_UNKNOWN;
function NWGetDriveStatusConnRef(driveNum: Tnuint16; pathFormat: Tnuint16; status: pnuint16; connRef: pnuint32; rootPath: Pnstr8;
  relPath: Pnstr8; fullPath: Pnstr8): TNWCCODE; NWLIB_UNKNOWN;
function NWGetFirstDrive(firstDrive: pnuint16): TNWCCODE; NWLIB_UNKNOWN;
function NWParseNetWarePath(path: Pnstr8; conn: PNWCONN_HANDLE; dirHandle: PNWDIR_HANDLE; newPath: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWParseNetWarePathConnRef(path: Pnstr8; connRef: pnuint32; dirHandle: PNWDIR_HANDLE; newPath: Pnstr8): TNWCCODE; NWLIB_UNKNOWN;
function NWParsePathConnRef(path: Pnstr8; serverName: Pnstr8; connRef: pnuint32; volName: Pnstr8; dirPath: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWParsePath(path: Pnstr8; serverName: Pnstr8; conn: PNWCONN_HANDLE; volName: Pnstr8; dirPath: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWStripServerOffPath(path: Pnstr8; server: Pnstr8): Pnstr8; NWLIB_CALNLM32;
function NWCreateUNCPath(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; UNCPath: Pnstr8): TNWCCODE; NWLIB_CALNLM32;


//*****************************************************************************
//nwea.h
//*****************************************************************************


const
  EA_EOF = 1;
  EA_DONE = 1;
  EA_READWRITE = 0;
  EA_CREATE = 1;
    { 0xC8  }
  MISSING_EA_KEY = 200;
    { 0xC9  }
  EA_NOT_FOUND = 201;
    { 0xCA  }
  INVALID_EA_HANDLE_TYPE = 202;
    { 0xCB  }
  EA_NO_KEY_NO_DATA = 203;
    { 0xCC  }
  EA_NUMBER_MISMATCH = 204;
    { 0xCD  }
  EXTENT_NUMBER_OUT_OF_RANGE = 205;
    { 0xCE  }
  EA_BAD_DIR_NUM = 206;
    { 0xCF  }
  INVALID_EA_HANDLE = 207;
    { 0xD0  }
  EA_POSITION_OUT_OF_RANGE = 208;
    { 0xD1  }
  EA_ACCESS_DENIED = 209;
    { 0xD2  }
  DATA_PAGE_ODD_SIZE = 210;
    { 0xD3  }
  EA_VOLUME_NOT_MOUNTED = 211;
    { 0xD4  }
  BAD_PAGE_BOUNDARY = 212;
    { 0xD5  }
  INSPECT_FAILURE = 213;
    { 0xD6  }
  EA_ALREADY_CLAIMED = 214;
    { 0xD7  }
  ODD_BUFFER_SIZE = 215;
    { 0xD8  }
  NO_SCORECARDS = 216;
    { 0xD9  }
  BAD_EDS_SIGNATURE = 217;
    { 0xDA  }
  EA_SPACE_LIMIT = 218;
    { 0xDB  }
  EA_KEY_CORRUPT = 219;
    { 0xDC  }
  EA_KEY_LIMIT = 220;
    { 0xDD  }
  TALLY_CORRUPT = 221;


type
  PNW_EA_HANDLE = ^TNW_EA_HANDLE;
  TNW_EA_HANDLE = record
    connID: TNWCONN_HANDLE;
    rwPosition: Tnuint32;
    EAHandle: Tnuint32;
    volNumber: Tnuint32;
    dirBase: Tnuint32;
    keyUsed: Tnuint8;
    keyLength: Tnuint16;
    key: array[0..255] of Tnuint8;
  end;

  PNW_EA_HANDLE_EXT = ^TNW_EA_HANDLE_EXT;
  TNW_EA_HANDLE_EXT = record
    connID: TNWCONN_HANDLE;
    rwPosition: Tnuint32;
    EAHandle: Tnuint32;
    volNumber: Tnuint32;
    dirBase: Tnuint32;
    keyUsed: Tnuint8;
    keyLength: Tnuint16;
    key: array[0..765] of Tnuint8;
  end;

  PNW_EA_FF_STRUCT = ^TNW_EA_FF_STRUCT;
  TNW_EA_FF_STRUCT = record
    connID: TNWCONN_HANDLE;
    nextKeyOffset: Tnuint16;
    nextKey: Tnuint16;
    numKeysRead: Tnuint32;
    totalKeys: Tnuint32;
    EAHandle: Tnuint32;
    sequence: Tnuint16;
    numKeysInBuffer: Tnuint16;
    enumBuffer: array[0..511] of Tnuint8;
  end;

  PNW_EA_FF_STRUCT_EXT = ^TNW_EA_FF_STRUCT_EXT;
  TNW_EA_FF_STRUCT_EXT = record
    connID: TNWCONN_HANDLE;
    nextKeyOffset: Tnuint16;
    nextKey: Tnuint16;
    numKeysRead: Tnuint32;
    totalKeys: Tnuint32;
    EAHandle: Tnuint32;
    sequence: Tnuint16;
    numKeysInBuffer: Tnuint16;
    enumBuffer: array[0..1529] of Tnuint8;
  end;

  PNW_IDX = ^TNW_IDX;
  TNW_IDX = record
    volNumber: Tnuint8;
    srcNameSpace: Tnuint8;
    srcDirBase: Tnuint32;
    dstNameSpace: Tnuint8;
    dstDirBase: Tnuint32;
  end;




function NWCloseEA(EAHandle: PNW_EA_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWCloseEAExt(EAHandle: PNW_EA_HANDLE_EXT): TNWCCODE; NWLIB_CALNLM32;
function NWFindFirstEA(conn: TNWCONN_HANDLE; idxStruct: PNW_IDX; ffStruct: PNW_EA_FF_STRUCT; EAHandle: PNW_EA_HANDLE; EAName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWFindFirstEAExt(conn: TNWCONN_HANDLE; idxStruct: PNW_IDX; ffStruct: PNW_EA_FF_STRUCT_EXT; EAHandle: PNW_EA_HANDLE_EXT; EAName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWFindNextEA(ffStruct: PNW_EA_FF_STRUCT; EAHandle: PNW_EA_HANDLE; EAName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWFindNextEAExt(ffStruct: PNW_EA_FF_STRUCT_EXT; EAHandle: PNW_EA_HANDLE_EXT; EAName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWReadEA(EAHandle: PNW_EA_HANDLE; bufferSize: Tnuint32; buffer: pnuint8; totalEASize: pnuint32; amountRead: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWReadEAExt(EAHandle: PNW_EA_HANDLE_EXT; bufferSize: Tnuint32; buffer: pnuint8; totalEASize: pnuint32; amountRead: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWWriteEA(EAHandle: PNW_EA_HANDLE; totalWriteSize: Tnuint32; bufferSize: Tnuint32; buffer: Pnuint8; amountWritten: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWWriteEAExt(EAHandle: PNW_EA_HANDLE_EXT; totalWriteSize: Tnuint32; bufferSize: Tnuint32; buffer: Pnuint8; amountWritten: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetEAHandleStruct(conn: TNWCONN_HANDLE; EAName: Pnstr8; idxStruct: PNW_IDX; EAHandle: PNW_EA_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWGetEAHandleStructExt(conn: TNWCONN_HANDLE; EAName: Pnstr8; idxStruct: PNW_IDX; EAHandle: PNW_EA_HANDLE_EXT): TNWCCODE; NWLIB_CALNLM32;
function NWOpenEA(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; EAName: Pnstr8; nameSpace: Tnuint8;
  EAHandle: PNW_EA_HANDLE): TNWCCODE; NWLIB_CALNLM32;

function NWOpenEAExt(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; EAName: Pnstr8; nameSpace: Tnuint8;
  EAHandle: PNW_EA_HANDLE_EXT): TNWCCODE; NWLIB_CALNLM32;



const
  SUCCESS = 0;
    { Network errors }
    { Decimal values at end of line are 32768 lower than actual }
  SHELL_ERROR = $8800;
  VLM_ERROR = $8800;
  ALREADY_ATTACHED = $8800; { 0 - Attach attempted to server with valid, existing connection }
  INVALID_CONNECTION = $8801; { 1 - Request attempted with invalid or non-attached connection handle }
  DRIVE_IN_USE = $8802; { 2 - OS/2 only (NOT USED) }
  CANT_ADD_CDS = $8803; { 3 - Map drive attempted but unable to add new current directory structure }
  DRIVE_CANNOT_MAP = $8803;
  BAD_DRIVE_BASE = $8804; { 4 - Map drive attempted with invalid path specification }
  NET_READ_ERROR = $8805; { 5 - Attempt to receive from the selected transport failed }
  NET_RECV_ERROR = $8805;
  UNKNOWN_NET_ERROR = $8806; { 6 - Network send attempted with an un-specific network error }
  SERVER_INVALID_SLOT = $8807; { 7 - Server request attempted with invalid server connection slot }
  BAD_SERVER_SLOT = $8807;
  NO_SERVER_SLOTS = $8808; { 8 - Attach attempted to server with no connection slots available }
  NET_WRITE_ERROR = $8809; { 9 - Attempt to send on the selected transport failed }
  CONNECTION_IN_ERROR_STATE = $8809; { Client-32 }
  NET_SEND_ERROR = $8809;
  SERVER_NO_ROUTE = $880A; { 10 - Attempted to find route to server where no route exists }
  BAD_LOCAL_TARGET = $880B; { 11 - OS/2 only }
  TOO_MANY_REQ_FRAGS = $880C; { 12 - Attempted request with too many request fragments specified }
  CONNECT_LIST_OVERFLOW = $880D;
  BUFFER_OVERFLOW = $880E; { 14 - Attempt to receive more data than the reply buffer had room for }
  MORE_DATA_ERROR = $880E; { Client-32 }
  NO_CONN_TO_SERVER = $880F;
  NO_CONNECTION_TO_SERVER = $880F; { 15 - Attempt to get connection for a server not connected }
  NO_ROUTER_FOUND = $8810; { 16 - OS/2 only }
  BAD_FUNC_ERROR = $8811;
  INVALID_SHELL_CALL = $8811; { 17 - Attempted function call to non- existent or illegal function }
  SCAN_COMPLETE = $8812;
  LIP_RESIZE_ERROR = $8812; { Client-32 }
  UNSUPPORTED_NAME_FORMAT_TYPE = $8813;
  INVALID_DIR_HANDLE = $8813; { Client-32 }
  HANDLE_ALREADY_LICENSED = $8814;
  OUT_OF_CLIENT_MEMORY = $8814; { Client-32 }
  HANDLE_ALREADY_UNLICENSED = $8815;
  PATH_NOT_OURS = $8815; { Client-32 }
  INVALID_NCP_PACKET_LENGTH = $8816;
  PATH_IS_PRINT_DEVICE = $8816; { Client-32 }
  SETTING_UP_TIMEOUT = $8817;
  PATH_IS_EXCLUDED_DEVICE = $8817; { Client-32 }
  SETTING_SIGNALS = $8818;
  PATH_IS_INVALID = $8818; { Client-32 }
  SERVER_CONNECTION_LOST = $8819;
  NOT_SAME_DEVICE = $8819; { Client-32 }
  OUT_OF_HEAP_SPACE = $881A;
  INVALID_SERVICE_REQUEST = $881B;
  INVALID_SEARCH_HANDLE = $881B; { Client-32 }
  INVALID_TASK_NUMBER = $881C;
  INVALID_DEVICE_HANDLE = $881C; { Client-32 }
  INVALID_MESSAGE_LENGTH = $881D;
  INVALID_SEM_HANDLE = $881D; { Client-32 }
  EA_SCAN_DONE = $881E;
  INVALID_CFG_HANDLE = $881E; { Client-32 }
  BAD_CONNECTION_NUMBER = $881F;
  INVALID_MOD_HANDLE = $881F; { Client-32 }
  ASYN_FIRST_PASS = $8820;
  INVALID_DEVICE_INDEX = $8821;
  INVALID_CONN_HANDLE = $8822;
  INVALID_QUEUE_ID = $8823;
  INVALID_PDEVICE_HANDLE = $8824;
  INVALID_JOB_HANDLE = $8825;
  INVALID_ELEMENT_ID = $8826;
  ALIAS_NOT_FOUND = $8827;
  RESOURCE_SUSPENDED = $8828;
  INVALID_QUEUE_SPECIFIED = $8829;
  DEVICE_ALREADY_OPEN = $882A;
  JOB_ALREADY_OPEN = $882B;
  QUEUE_NAME_ID_MISMATCH = $882C;
  JOB_ALREADY_STARTED = $882D;
  SPECT_DAA_TYPE_NOT_SUPPORTED = $882E;
  INVALID_ENVIR_HANDLE = $882F;
  NOT_SAME_CONNECTION = $8830; { 48 - Internal server request attempted accross different server connections }
  PRIMARY_CONNECTION_NOT_SET = $8831; { 49 - Attempt to retrieve default connection with no primary connection set }
  NO_PRIMARY_SET = $8831;
  KEYWORD_NOT_FOUND = $8832; { Client-32 }
  PRINT_CAPTURE_NOT_IN_PROGRESS = $8832; { Client-32 }
  NO_CAPTURE_SET = $8832;
  NO_CAPTURE_IN_PROGRESS = $8832; { 50 - Capture information requested on port with no capture in progress }
  BAD_BUFFER_LENGTH = $8833;
  INVALID_BUFFER_LENGTH = $8833; { 51 - Used to indicate length which caller requested on a GetDNC or SetDNC was too large }
  NO_USER_NAME = $8834;
  NO_NETWARE_PRINT_SPOOLER = $8835; { 53 - Capture requested without having the local print spooler installed }
  INVALID_PARAMETER = $8836; { 54 - Attempted function with an invalid function parameter specified }
  CONFIG_FILE_OPEN_FAILED = $8837; { 55 - OS/2 only }
  NO_CONFIG_FILE = $8838; { 56 - OS/2 only }
  CONFIG_FILE_READ_FAILED = $8839; { 57 - OS/2 only }
  CONFIG_LINE_TOO_LONG = $883A; { 58 - OS/2 only }
  CONFIG_LINES_IGNORED = $883B; { 59 - OS/2 only }
  NOT_MY_RESOURCE = $883C; { 60 - Attempted request made with a parameter using foriegn resource }
  DAEMON_INSTALLED = $883D; { 61 - OS/2 only }
  SPOOLER_INSTALLED = $883E; { 62 - Attempted load of print spooler with print spooler already installed }
  CONN_TABLE_FULL = $883F;
  CONNECTION_TABLE_FULL = $883F; { 63 - Attempted to allocate a connection handle with no more local connection table entries }
  CONFIG_SECTION_NOT_FOUND = $8840; { 64 - OS/2 only }
  BAD_TRAN_TYPE = $8841;
  INVALID_TRANSPORT_TYPE = $8841; { 65 - Attempted function on a connection with an invalid transport selected }
  TDS_TAG_IN_USE = $8842; { 66 - OS/2 only }
  TDS_OUT_OF_MEMORY = $8843; { 67 - OS/2 only }
  TDS_INVALID_TAG = $8844; { 68 - Attempted TDS function with invalid tag }
  TDS_WRITE_TRUNCATED = $8845; { 69 - Attempted TDS write with buffer that exceeded buffer }
  NO_CONNECTION_TO_DS = $8846; { Client-32 }
  NO_DIRECTORY_SERVICE_CONNECTION = $8846;
  SERVICE_BUSY = $8846; { 70 - Attempted request made to partially asynchronous function in busy state }
  NO_SERVER_ERROR = $8847; { 71 - Attempted connect failed to find any servers responding }
  BAD_VLM_ERROR = $8848; { 72 - Attempted function call to non-existant or not-loaded overlay }
  NETWORK_DRIVE_IN_USE = $8849; { 73 - Attempted map to network drive that was already mapped }
  LOCAL_DRIVE_IN_USE = $884A; { 74 - Attempted map to local drive that was in use }
  NO_DRIVES_AVAILABLE = $884B; { 75 - Attempted map to next available drive when none were available }
  DEVICE_NOT_REDIRECTED = $884C; { 76 - The device is not redirected }
  NO_MORE_SFT_ENTRIES = $884D; { 77 - Maximum number of files was reached }
  UNLOAD_ERROR = $884E; { 78 - Attempted unload failed }
  IN_USE_ERROR = $884F; { 79 - Attempted re-use of already in use connection entry }
  TOO_MANY_REP_FRAGS = $8850; { 80 - Attempted request with too many reply fragments specified }
  TABLE_FULL = $8851; { 81 - Attempted to add a name into the name table after it was full }
  SOCKET_NOT_OPEN = $8852; { 82 - Listen was posted on unopened socket }
  MEM_MGR_ERROR = $8853; { 83 - Attempted enhanced memory operation failed }
  SFT3_ERROR = $8854; { 84 - An SFT3 switch occured mid-transfer }
  PREFERRED_NOT_FOUND = $8855; { 85 - the preferred directory server was not established but another directory server was returned }
  DEVICE_NOT_RECOGNIZED = $8856; { 86 - used to determine if the device is not used by VISE so pass it on to the next redirector, if any. }
  BAD_NET_TYPE = $8857; { 87 - the network type (Bind/NDS) does not match the server version }
  ERROR_OPENING_FILE = $8858; { 88 - generic open failure error, invalid path, access denied, etc.. }
  NO_PREFERRED_SPECIFIED = $8859; { 89 - no preferred name specified }
  ERROR_OPENING_SOCKET = $885A; { 90 - error opening a socket }
  REQUESTER_FAILURE = $885A; { Client-32 }
  RESOURCE_ACCESS_DENIED = $885B; { Client-32 }
  SIGNATURE_LEVEL_CONFLICT = $8861;
  NO_LOCK_FOUND = $8862; { OS/2 - process lock on conn handle failed, process ID not recognized }
  LOCK_TABLE_FULL = $8863; { OS/2 - process lock on conn handle failed, process lock table full }
  INVALID_MATCH_DATA = $8864;
  MATCH_FAILED = $8865;
  NO_MORE_ENTRIES = $8866;
  INSUFFICIENT_RESOURCES = $8867;
  STRING_TRANSLATION = $8868;
  STRING_TRANSLATION_NEEDED = $8868; { Client-32 }
  ACCESS_VIOLATION = $8869;
  NOT_AUTHENTICATED = $886A;
  INVALID_LEVEL = $886B;
  RESOURCE_LOCK_ERROR = $886C;
  INVALID_NAME_FORMAT = $886D;
  OBJECT_EXISTS = $886E;
  OBJECT_NOT_FOUND = $886F;
  UNSUPPORTED_TRAN_TYPE = $8870;
  INVALID_STRING_TYPE = $8871;
  INVALID_OWNER = $8872;
  UNSUPPORTED_AUTHENTICATOR = $8873;
  IO_PENDING = $8874;
  INVALID_DRIVE_NUM = $8875;
  SHELL_FAILURE = $88FF;
  VLM_FAILURE = $88FF;
  SVC_ALREADY_REGISTERED = $8880; { Client-32 }
  SVC_REGISTRY_FULL = $8881; { Client-32 }
  SVC_NOT_REGISTERED = $8882; { Client-32 }
  OUT_OF_RESOURCES = $8883; { Client-32 }
  RESOLVE_SVC_FAILED = $8884; { Client-32 }
  CONNECT_FAILED = $8885; { Client-32 }
  PROTOCOL_NOT_BOUND = $8886; { Client-32 }
  AUTHENTICATION_FAILED = $8887; { Client-32 }
  INVALID_AUTHEN_HANDLE = $8888; { Client-32 }
  AUTHEN_HANDLE_ALREADY_EXISTS = $8889; { Client-32 }
  DIFF_OBJECT_ALREADY_AUTHEN = $8890; { Client-32 }
  REQUEST_NOT_SERVICEABLE = $8891; { Client-32 }
  AUTO_RECONNECT_SO_REBUILD = $8892; { Client-32 }
  AUTO_RECONNECT_RETRY_REQUEST = $8893; { Client-32 }
  ASYNC_REQUEST_IN_USE = $8894; { Client-32 }
  ASYNC_REQUEST_CANCELED = $8895; { Client-32 }
  SESS_SVC_ALREADY_REGISTERED = $8896; { Client-32 }
  SESS_SVC_NOT_REGISTERED = $8897; { Client-32 }
  PREVIOUSLY_AUTHENTICATED = $8899; { Client-32 }
  RESOLVE_SVC_PARTIAL = $889A; { Client-32 }
  NO_DEFAULT_SPECIFIED = $889B; { Client-32 }
  HOOK_REQUEST_NOT_HANDLED = $889C; { Client-32 }
  HOOK_REQUEST_BUSY = $889D; { Client-32 }
  HOOK_REQUEST_QUEUED = $889D; { Client-32 }
  AUTO_RECONNECT_SO_IGNORE = $889E; { Client-32 }
  ASYNC_REQUEST_NOT_IN_USE = $889F; { Client-32 }
  AUTO_RECONNECT_FAILURE = $88A0; { Client-32 }
  NET_ERROR_ABORT_APPLICATION = $88A1; { Client-32 }
  NET_ERROR_SUSPEND_APPLICATION = $88A2; { Client-32 }
  NET_ERROR_ABORTED_PROCESS_GROUP = $88A3; { Client-32 }
  NET_ERROR_PASSWORD_HAS_EXPIRED = $88A5; { Client-32 }
  NET_ERROR_NETWORK_INACTIVE = $88A6; { Client-32 }
  REPLY_TRUNCATED = $88E6; { 230 NLM }
  UTF8_CONVERSION_FAILED = $88F0; { NWCALLS }
    { Server Errors }
  ERR_INSUFFICIENT_SPACE = $8901; { 001 }
  NLM_INVALID_CONNECTION = $890A; { 010 }
  ERR_TIMEOUT = $8910; { 016 - nlm connection timeout }
  ERR_NO_MORE_ENTRY = $8914; { 020 }
  ERR_BUFFER_TOO_SMALL = $8977; { 119 }
  ERR_VOLUME_FLAG_NOT_SET = $8978; { 120 the service requested, not avail. on the selected vol. }
  ERR_NO_ITEMS_FOUND = $8979; { 121 }
  ERR_CONN_ALREADY_TEMP = $897A; { 122 }
  ERR_CONN_ALREADY_LOGGED_IN = $897B; { 123 }
  ERR_CONN_NOT_AUTHENTICATED = $897C; { 124 }
  ERR_CONN_NOT_LOGGED_IN = $897D; { 125 }
  NCP_BOUNDARY_CHECK_FAILED = $897E; { 126 }
  ERR_LOCK_WAITING = $897F; { 127 }
  ERR_LOCK_FAIL = $8980; { 128 }
  FILE_IN_USE_ERROR = $8980; { 128 }
  NO_MORE_FILE_HANDLES = $8981; { 129 }
  NO_OPEN_PRIVILEGES = $8982; { 130 }
  IO_ERROR_NETWORK_DISK = $8983; { 131 }
  ERR_AUDITING_HARD_IO_ERROR = $8983; { 131 }
  NO_CREATE_PRIVILEGES = $8984; { 132 }
  ERR_AUDITING_NOT_SUPV = $8984; { 132 }
  NO_CREATE_DELETE_PRIVILEGES = $8985; { 133 }
  CREATE_FILE_EXISTS_READ_ONLY = $8986; { 134 }
  WILD_CARDS_IN_CREATE_FILE_NAME = $8987;
  CREATE_FILENAME_ERROR = $8987; { 135 }
  INVALID_FILE_HANDLE = $8988; { 136 }
  NO_SEARCH_PRIVILEGES = $8989; { 137 }
  NO_DELETE_PRIVILEGES = $898A; { 138 }
  NO_RENAME_PRIVILEGES = $898B; { 139 }
  NO_MODIFY_PRIVILEGES = $898C; { 140 }
  SOME_FILES_AFFECTED_IN_USE = $898D; { 141 }
  NO_FILES_AFFECTED_IN_USE = $898E; { 142 }
  SOME_FILES_AFFECTED_READ_ONLY = $898F; { 143 }
  NO_FILES_AFFECTED_READ_ONLY = $8990; { 144 }
  SOME_FILES_RENAMED_NAME_EXISTS = $8991; { 145 }
  NO_FILES_RENAMED_NAME_EXISTS = $8992; { 146 }
  NO_READ_PRIVILEGES = $8993; { 147 }
  NO_WRITE_PRIVILEGES_OR_READONLY = $8994; { 148 }
  FILE_DETACHED = $8995; { 149 }
  SERVER_OUT_OF_MEMORY = $8996; { 150 }
  ERR_TARGET_NOT_A_SUBDIRECTORY = $8996; { 150 can be changed later (note written by server people). }
  NO_DISK_SPACE_FOR_SPOOL_FILE = $8997; { 151 }
  ERR_AUDITING_NOT_ENABLED = $8997; { 151 }
  VOLUME_DOES_NOT_EXIST = $8998; { 152 }
  DIRECTORY_FULL = $8999; { 153 }
  RENAMING_ACROSS_VOLUMES = $899A; { 154 }
  BAD_DIRECTORY_HANDLE = $899B; { 155 }
  INVALID_PATH = $899C; { 156 }
  NO_MORE_TRUSTEES = $899C; { 156 }
  NO_MORE_DIRECTORY_HANDLES = $899D; { 157 }
  INVALID_FILENAME = $899E; { 158 }
  DIRECTORY_ACTIVE = $899F; { 159 }
  DIRECTORY_NOT_EMPTY = $89A0; { 160 }
  DIRECTORY_IO_ERROR = $89A1; { 161 }
  READ_FILE_WITH_RECORD_LOCKED = $89A2; { 162 }
  ERR_TRANSACTION_RESTARTED = $89A3; { 163 }
  ERR_RENAME_DIR_INVALID = $89A4; { 164 }
  ERR_INVALID_OPENCREATE_MODE = $89A5; { 165 }
  ERR_ALREADY_IN_USE = $89A6; { 166 }
  ERR_AUDITING_ACTIVE = $89A6; { 166 }
  ERR_INVALID_RESOURCE_TAG = $89A7; { 167 }
  ERR_ACCESS_DENIED = $89A8; { 168 }
  ERR_AUDITING_NO_RIGHTS = $89A8; { 168 }
  ERR_LINK_IN_PATH = $89A9; { 169 }
  INVALID_DATA_TYPE = $89AA; { 170 }
  INVALID_DATA_STREAM = $89BE; { 190 }
  INVALID_NAME_SPACE = $89BF; { 191 }
  NO_ACCOUNTING_PRIVILEGES = $89C0; { 192 }
  LOGIN_DENIED_NO_ACCOUNT_BALANCE = $89C1; { 193 }
  LOGIN_DENIED_NO_CREDIT = $89C2; { 194 }
  ERR_AUDITING_RECORD_SIZE = $89C2; { 194 }
  ERR_TOO_MANY_HOLDS = $89C3; { 195 }
  ACCOUNTING_DISABLED = $89C4; { 196 }
  INTRUDER_DETECTION_LOCK = $89C5; { 197 }
  NO_CONSOLE_OPERATOR = $89C6; { 198 }
  NO_CONSOLE_PRIVILEGES = $89C6; { 198 }
  ERR_Q_IO_FAILURE = $89D0; { 208 }
  ERR_NO_QUEUE = $89D1; { 209 }
  ERR_NO_Q_SERVER = $89D2; { 210 }
  ERR_NO_Q_RIGHTS = $89D3; { 211 }
  ERR_Q_FULL = $89D4; { 212 }
  ERR_NO_Q_JOB = $89D5; { 213 }
  ERR_NO_Q_JOB_RIGHTS = $89D6; { 214 }
  ERR_Q_IN_SERVICE = $89D7; { 215 }
  PASSWORD_NOT_UNIQUE = $89D7; { 215 }
  ERR_Q_NOT_ACTIVE = $89D8; { 216 }
  PASSWORD_TOO_SHORT = $89D8; { 216 }
  ERR_Q_STN_NOT_SERVER = $89D9; { 217 }
  LOGIN_DENIED_NO_CONNECTION = $89D9; { 217 }
  ERR_MAXIMUM_LOGINS_EXCEEDED = $89D9; { 217 }
  ERR_Q_HALTED = $89DA; { 218 }
  UNAUTHORIZED_LOGIN_TIME = $89DA; { 218 }
  UNAUTHORIZED_LOGIN_STATION = $89DB; { 219 }
  ERR_Q_MAX_SERVERS = $89DB; { 219 }
  ACCOUNT_DISABLED = $89DC; { 220 }
  PASSWORD_HAS_EXPIRED_NO_GRACE = $89DE; { 222 }
  PASSWORD_HAS_EXPIRED = $89DF; { 223 }
  E_NO_MORE_USERS = $89E7; { 231 }
  NOT_ITEM_PROPERTY = $89E8; { 232 }
  WRITE_PROPERTY_TO_GROUP = $89E8; { 232 }
  MEMBER_ALREADY_EXISTS = $89E9; { 233 }
  NO_SUCH_MEMBER = $89EA; { 234 }
  NOT_GROUP_PROPERTY = $89EB; { 235 }
  NO_SUCH_SEGMENT = $89EC; { 236 }
  PROPERTY_ALREADY_EXISTS = $89ED; { 237 }
  OBJECT_ALREADY_EXISTS = $89EE; { 238 }
  INVALID_NAME = $89EF; { 239 }
  WILD_CARD_NOT_ALLOWED = $89F0; { 240 }
  INVALID_BINDERY_SECURITY = $89F1; { 241 }
  NO_OBJECT_READ_PRIVILEGE = $89F2; { 242 }
  NO_OBJECT_RENAME_PRIVILEGE = $89F3; { 243 }
  NO_OBJECT_DELETE_PRIVILEGE = $89F4; { 244 }
  NO_OBJECT_CREATE_PRIVILEGE = $89F5; { 245 }
  NO_PROPERTY_DELETE_PRIVILEGE = $89F6; { 246 }
  NO_PROPERTY_CREATE_PRIVILEGE = $89F7; { 247 }
  NO_PROPERTY_WRITE_PRIVILEGE = $89F8; { 248 }
  NO_FREE_CONNECTION_SLOTS = $89F9; { 249 }
  NO_PROPERTY_READ_PRIVILEGE = $89F9; { 249 }
  NO_MORE_SERVER_SLOTS = $89FA; { 250 }
  TEMP_REMAP_ERROR = $89FA; { 250 }
  INVALID_PARAMETERS = $89FB; { 251 }
  NO_SUCH_PROPERTY = $89FB; { 251 }
  ERR_NCP_NOT_SUPPORTED = $89FB; { 251 }
  INTERNET_PACKET_REQT_CANCELED = $89FC; { 252 }
  UNKNOWN_FILE_SERVER = $89FC; { 252 }
  MESSAGE_QUEUE_FULL = $89FC; { 252 }
  NO_SUCH_OBJECT = $89FC; { 252 }
  LOCK_COLLISION = $89FD; { 253 }
  BAD_STATION_NUMBER = $89FD; { 253 }
  INVALID_PACKET_LENGTH = $89FD; { 253 }
  UNKNOWN_REQUEST = $89FD; { 253 }
  BINDERY_LOCKED = $89FE; { 254 }
  TRUSTEE_NOT_FOUND = $89FE; { 254 }
  DIRECTORY_LOCKED = $89FE; { 254 }
  INVALID_SEMAPHORE_NAME_LENGTH = $89FE; { 254 }
  PACKET_NOT_DELIVERABLE = $89FE; { 254 }
  SERVER_BINDERY_LOCKED = $89FE; { 254 }
  SOCKET_TABLE_FULL = $89FE; { 254 }
  SPOOL_DIRECTORY_ERROR = $89FE; { 254 }
  SUPERVISOR_HAS_DISABLED_LOGIN = $89FE; { 254 }
  TIMEOUT_FAILURE = $89FE; { 254 }
  BAD_PRINTER_ERROR = $89FF; { 255 }
  BAD_RECORD_OFFSET = $89FF; { 255 }
  CLOSE_FCB_ERROR = $89FF; { 255 }
  FILE_EXTENSION_ERROR = $89FF; { 255 }
  FILE_NAME_ERROR = $89FF; { 255 }
  HARDWARE_FAILURE = $89FF; { 255 }
  INVALID_DRIVE_NUMBER = $89FF; { 255 }
  DOS_INVALID_DRIVE = $000F; { 255 }
  INVALID_INITIAL_SEMAPHORE_VALUE = $89FF; { 255 }
  INVALID_SEMAPHORE_HANDLE = $89FF; { 255 }
  IO_BOUND_ERROR = $89FF; { 255 }
  NO_FILES_FOUND_ERROR = $89FF; { 255 }
  NO_RESPONSE_FROM_SERVER = $89FF; { 255 }
  NO_SUCH_OBJECT_OR_BAD_PASSWORD = $89FF; { 255 }
  PATH_NOT_LOCATABLE = $89FF; { 255 }
  QUEUE_FULL_ERROR = $89FF; { 255 }
  REQUEST_NOT_OUTSTANDING = $89FF; { 255 }
  SOCKET_ALREADY_OPEN = $89FF; { 255 }
  LOCK_ERROR = $89FF; { 255 }
  FAILURE = $89FF; { 255 Generic Failure }
     { NOT_SAME_LOCAL_DRIVE = $89F6; }
     { TARGET_DRIVE_NOT_LOCAL = $89F7; }
     { ALREADY_ATTACHED_TO_SERVER = $89F8; // 248 }
     { NOT_ATTACHED_TO_SERVER = $89F8; }
     {/// Network errors ///// }
     { Decimal values at end of line are 32768 lower than actual }


  NWE_ALREADY_ATTACHED = $8800; { 0 - Attach attempted to server with valid, existing connection }
  NWE_CONN_INVALID = $8801; { 1 - Request attempted with invalid or non-attached connection handle }
  NWE_DRIVE_IN_USE = $8802; { 2 - OS/2 only (NOT USED) }
  NWE_DRIVE_CANNOT_MAP = $8803; { 3 - Map drive attempted but unable to add new current directory structure }
  NWE_DRIVE_BAD_PATH = $8804; { 4 - Map drive attempted with invalid path specification }
  NWE_NET_RECEIVE = $8805; { 5 - Attempt to receive from the selected transport failed }
  NWE_NET_UNKNOWN = $8806; { 6 - Network send attempted with an un-specific network error }
  NWE_SERVER_BAD_SLOT = $8807; { 7 - Server request attempted with invalid server connection slot }
  NWE_SERVER_NO_SLOTS = $8808; { 8 - Attach attempted to server with no connection slots available }
  NWE_NET_SEND = $8809; { 9 - Attempt to send on the selected transport failed }
  NWE_SERVER_NO_ROUTE = $880A; { 10 - Attempted to find route to server where no route exists }
  NWE_BAD_LOCAL_TARGET = $880B; { 11 - OS/2 only }
  NWE_REQ_TOO_MANY_REQ_FRAGS = $880C; { 12 - Attempted request with too many request fragments specified }
  NWE_CONN_LIST_OVERFLOW = $880D;
  NWE_BUFFER_OVERFLOW = $880E; { 14 - Attempt to receive more data than the reply buffer had room for }
  NWE_SERVER_NO_CONN = $880F; { 15 - Attempt to get connection for a server not connected }
  NWE_NO_ROUTER_FOUND = $8810; { 16 - OS/2 only }
  NWE_FUNCTION_INVALID = $8811; { 17 - Attempted function call to non- existent or illegal function }
  NWE_SCAN_COMPLETE = $8812;
  NWE_UNSUPPORTED_NAME_FORMAT_TYP = $8813;
  NWE_HANDLE_ALREADY_LICENSED = $8814;
  NWE_HANDLE_ALREADY_UNLICENSED = $8815;
  NWE_INVALID_NCP_PACKET_LENGTH = $8816;
  NWE_SETTING_UP_TIMEOUT = $8817;
  NWE_SETTING_SIGNALS = $8818;
  NWE_SERVER_CONNECTION_LOST = $8819;
  NWE_OUT_OF_HEAP_SPACE = $881A;
  NWE_INVALID_SERVICE_REQUEST = $881B;
  NWE_INVALID_TASK_NUMBER = $881C;
  NWE_INVALID_MESSAGE_LENGTH = $881D;
  NWE_EA_SCAN_DONE = $881E;
  NWE_BAD_CONNECTION_NUMBER = $881F;
  NWE_MULT_TREES_NOT_SUPPORTED = $8820; { 32 - Attempt to open a connection to a DS tree other than the default tree }
  NWE_CONN_NOT_SAME = $8830; { 48 - Internal server request attempted across different server connections }
  NWE_CONN_PRIMARY_NOT_SET = $8831; { 49 - Attempt to retrieve default connection with no primary connection set }
  NWE_PRN_CAPTURE_NOT_IN_PROGRESS = $8832; { 50 - Capture information requested on port with no capture in progress }
  NWE_BUFFER_INVALID_LEN = $8833; { 51 - Used to indicate length which caller requested on a GetDNC or SetDNC was too large }
  NWE_USER_NO_NAME = $8834; { 52 }
  NWE_PRN_NO_LOCAL_SPOOLER = $8835; { 53 - Capture requested without having the local print spooler installed }
  NWE_PARAM_INVALID = $8836; { 54 - Attempted function with an invalid function parameter specified }
  NWE_CFG_OPEN_FAILED = $8837; { 55 - OS/2 only }
  NWE_CFG_NO_FILE = $8838; { 56 - OS/2 only }
  NWE_CFG_READ_FAILED = $8839; { 57 - OS/2 only }
  NWE_CFG_LINE_TOO_LONG = $883A; { 58 - OS/2 only }
  NWE_CFG_LINES_IGNORED = $883B; { 59 - OS/2 only }
  NWE_RESOURCE_NOT_OWNED = $883C; { 60 - Attempted request made with a parameter using foriegn resource }
  NWE_DAEMON_INSTALLED = $883D; { 61 - OS/2 only }
  NWE_PRN_SPOOLER_INSTALLED = $883E; { 62 - Attempted load of print spooler with print spooler already installed }
  NWE_CONN_TABLE_FULL = $883F; { 63 - Attempted to allocate a connection handle with no more local connection table entries }
  NWE_CFG_SECTION_NOT_FOUND = $8840; { 64 - OS/2 only }
  NWE_TRAN_INVALID_TYPE = $8841; { 65 - Attempted function on a connection with an invalid transport selected }
  NWE_TDS_TAG_IN_USE = $8842; { 66 - OS/2 only }
  NWE_TDS_OUT_OF_MEMORY = $8843; { 67 - OS/2 only }
  NWE_TDS_INVALID_TAG = $8844; { 68 - Attempted TDS function with invalid tag }
  NWE_TDS_WRITE_TRUNCATED = $8845; { 69 - Attempted TDS write with buffer that exceeded buffer }
  NWE_DS_NO_CONN = $8846; { 70 }
  NWE_SERVICE_BUSY = $8846; { 70 - Attempted request made to partially asynchronous function in busy state }
  NWE_SERVER_NOT_FOUND = $8847; { 71 - Attempted connect failed to find any servers responding }
  NWE_VLM_INVALID = $8848; { 72 - Attempted function call to non-existant or not-loaded overlay }
  NWE_DRIVE_ALREADY_MAPPED = $8849; { 73 - Attempted map to network drive that was already mapped }
  NWE_DRIVE_LOCAL_IN_USE = $884A; { 74 - Attempted map to local drive that was in use }
  NWE_DRIVE_NONE_AVAILABLE = $884B; { 75 - Attempted map to next available drive when none were available }
  NWE_DEVICE_NOT_REDIRECTED = $884C; { 76 - The device is not redirected }
  NWE_FILE_MAX_REACHED = $884D; { 77 - Maximum number of files was reached }
  NWE_UNLOAD_FAILED = $884E; { 78 - Attempted unload failed }
  NWE_CONN_IN_USE = $884F; { 79 - Attempted re-use of already in use connection entry }
  NWE_REQ_TOO_MANY_REP_FRAGS = $8850; { 80 - Attempted request with too many reply fragments specified }
  NWE_NAME_TABLE_FULL = $8851; { 81 - Attempted to add a name into the name table after it was full }
  NWE_SOCKET_NOT_OPEN = $8852; { 82 - Listen was posted on unopened socket }
  NWE_MEMORY_MGR_ERROR = $8853; { 83 - Attempted enhanced memory operation failed }
  NWE_SFT3_ERROR = $8854; { 84 - An SFT3 switch occured mid-transfer }
  NWE_DS_PREFERRED_NOT_FOUND = $8855; { 85 - the preferred directory server was not established but another directory server was returned }
  NWE_DEVICE_NOT_RECOGNIZED = $8856; { 86 - used to determine if the device is not used by VISE so pass it on to the next redirector, if any. }
  NWE_NET_INVALID_TYPE = $8857; { 87 - the network type (Bind/NDS) does not match the server version }
  NWE_FILE_OPEN_FAILED = $8858; { 88 - generic open failure error, invalid path, access denied, etc.. }
  NWE_DS_PREFERRED_NOT_SPECIFIED = $8859; { 89 - no preferred name specified }
  NWE_SOCKET_OPEN_FAILED = $885A; { 90 - error opening a socket }
  NWE_SIGNATURE_LEVEL_CONFLICT = $8861;
  NWE_NO_LOCK_FOUND = $8862; { OS/2 - process lock on conn handle failed, process ID not recognized }
  NWE_LOCK_TABLE_FULL = $8863; { OS/2 - process lock on conn handle failed, process lock table full }
  NWE_INVALID_MATCH_DATA = $8864;
  NWE_MATCH_FAILED = $8865;
  NWE_NO_MORE_ENTRIES = $8866;
  NWE_INSUFFICIENT_RESOURCES = $8867;
  NWE_STRING_TRANSLATION = $8868;
  NWE_ACCESS_VIOLATION = $8869;
  NWE_NOT_AUTHENTICATED = $886A;
  NWE_INVALID_LEVEL = $886B;
  NWE_RESOURCE_LOCK = $886C;
  NWE_INVALID_NAME_FORMAT = $886D;
  NWE_OBJECT_EXISTS = $886E;
  NWE_OBJECT_NOT_FOUND = $886F;
  NWE_UNSUPPORTED_TRAN_TYPE = $8870;
  NWE_INVALID_STRING_TYPE = $8871;
  NWE_INVALID_OWNER = $8872;
  NWE_UNSUPPORTED_AUTHENTICATOR = $8873;
  NWE_IO_PENDING = $8874;
  NWE_INVALID_DRIVE_NUMBER = $8875;
  NWE_REPLY_TRUNCATED = $88E6; { 230 NLM }
  NWE_REQUESTER_FAILURE = $88FF;
    { Server Errors }
  NWE_INSUFFICIENT_SPACE = $8901; { 001 }
  NWE_INVALID_CONNECTION = $890A; { 010 - nlm invalid connection }
  NWE_TIMEOUT = $8910; { 016 - nlm connection timeout }
  NWE_NO_MORE_ENTRY = $8914; { 020 }
  NWE_BUFFER_TOO_SMALL = $8977; { 119 }
  NWE_VOL_FLAG_NOT_SET = $8978; { 120 the service requested, not avail. on the selected vol. }
  NWE_NO_ITEMS_FOUND = $8979; { 121 }
  NWE_CONN_ALREADY_TEMP = $897A; { 122 }
  NWE_CONN_ALREADY_LOGGED_IN = $897B; { 123 }
  NWE_CONN_NOT_AUTHENTICATED = $897C; { 124 }
  NWE_CONN_NOT_LOGGED_IN = $897D; { 125 }
  NWE_NCP_BOUNDARY_CHECK_FAILED = $897E; { 126 }
  NWE_LOCK_WAITING = $897F; { 127 }
  NWE_LOCK_FAIL = $8980; { 128 }
  NWE_FILE_IN_USE = $8980; { 128 }
  NWE_FILE_NO_HANDLES = $8981; { 129 }
  NWE_FILE_NO_OPEN_PRIV = $8982; { 130 }
  NWE_DISK_IO_ERROR = $8983; { 131 }
  NWE_AUDITING_HARD_IO_ERROR = $8983; { 131 }
  NWE_FILE_NO_CREATE_PRIV = $8984; { 132 }
  NWE_AUDITING_NOT_SUPV = $8984; { 132 }
  NWE_FILE_NO_CREATE_DEL_PRIV = $8985; { 133 }
  NWE_FILE_EXISTS_READ_ONLY = $8986; { 134 }
  NWE_FILE_WILD_CARDS_IN_NAME = $8987; { 135 }
  NWE_FILE_INVALID_HANDLE = $8988; { 136 }
  NWE_FILE_NO_SRCH_PRIV = $8989; { 137 }
  NWE_FILE_NO_DEL_PRIV = $898A; { 138 }
  NWE_FILE_NO_RENAME_PRIV = $898B; { 139 }
  NWE_FILE_NO_MOD_PRIV = $898C; { 140 }
  NWE_FILE_SOME_IN_USE = $898D; { 141 }
  NWE_FILE_NONE_IN_USE = $898E; { 142 }
  NWE_FILE_SOME_READ_ONLY = $898F; { 143 }
  NWE_FILE_NONE_READ_ONLY = $8990; { 144 }
  NWE_FILE_SOME_RENAMED_EXIST = $8991; { 145 }
  NWE_FILE_NONE_RENAMED_EXIST = $8992; { 146 }
  NWE_FILE_NO_READ_PRIV = $8993; { 147 }
  NWE_FILE_NO_WRITE_PRIV = $8994; { 148 }
  NWE_FILE_READ_ONLY = $8994; { 148 }
  NWE_FILE_DETACHED = $8995; { 149 }
  NWE_SERVER_OUT_OF_MEMORY = $8996; { 150 }
  NWE_DIR_TARGET_INVALID = $8996; { 150 }
  NWE_DISK_NO_SPOOL_SPACE = $8997; { 151 }
  NWE_AUDITING_NOT_ENABLED = $8997; { 151 }
  NWE_VOL_INVALID = $8998; { 152 }
  NWE_DIR_FULL = $8999; { 153 }
  NWE_VOL_RENAMING_ACROSS = $899A; { 154 }
  NWE_DIRHANDLE_INVALID = $899B; { 155 }
  NWE_PATH_INVALID = $899C; { 156 }
  NWE_TRUSTEES_NO_MORE = $899C; { 156 }
  NWE_DIRHANDLE_NO_MORE = $899D; { 157 }
  NWE_FILE_NAME_INVALID = $899E; { 158 }
  NWE_DIR_ACTIVE = $899F; { 159 }
  NWE_DIR_NOT_EMPTY = $89A0; { 160 }
  NWE_DIR_IO_ERROR = $89A1; { 161 }
  NWE_FILE_IO_LOCKED = $89A2; { 162 }
  NWE_TTS_RANSACTION_RESTARTED = $89A3; { 163 }
  NWE_TTS_TRANSACTION_RESTARTED = $89A3; { 163 }
  NWE_DIR_RENAME_INVALID = $89A4; { 164 }
  NWE_FILE_OPENCREAT_MODE_INVALID = $89A5; { 165 }
  NWE_ALREADY_IN_USE = $89A6; { 166 }
  NWE_AUDITING_ACTIVE = $89A6; { 166 }
  NWE_RESOURCE_TAG_INVALID = $89A7; { 167 }
  NWE_ACCESS_DENIED = $89A8; { 168 }
  NWE_AUDITING_NO_RIGHTS = $89A8; { 168 }
  NWE_DATA_STREAM_INVALID = $89BE; { 190 }
  NWE_NAME_SPACE_INVALID = $89BF; { 191 }
  NWE_ACCTING_NO_PRIV = $89C0; { 192 }
  NWE_ACCTING_NO_BALANCE = $89C1; { 193 }
  NWE_ACCTING_NO_CREDIT = $89C2; { 194 }
  NWE_AUDITING_RECORD_SIZE = $89C2; { 194 }
  NWE_ACCTING_TOO_MANY_HOLDS = $89C3; { 195 }
  NWE_ACCTING_DISABLED = $89C4; { 196 }
  NWE_LOGIN_LOCKOUT = $89C5; { 197 }
  NWE_CONSOLE_NO_PRIV = $89C6; { 198 }
  NWE_Q_IO_FAILURE = $89D0; { 208 }
  NWE_Q_NONE = $89D1; { 209 }
  NWE_Q_NO_SERVER = $89D2; { 210 }
  NWE_Q_NO_RIGHTS = $89D3; { 211 }
  NWE_Q_FULL = $89D4; { 212 }
  NWE_Q_NO_JOB = $89D5; { 213 }
  NWE_Q_NO_JOB_RIGHTS = $89D6; { 214 }
  NWE_PASSWORD_UNENCRYPTED = $89D6; { 214 }
  NWE_Q_IN_SERVICE = $89D7; { 215 }
  NWE_PASSWORD_NOT_UNIQUE = $89D7; { 215 }
  NWE_Q_NOT_ACTIVE = $89D8; { 216 }
  NWE_PASSWORD_TOO_SHORT = $89D8; { 216 }
  NWE_Q_STN_NOT_SERVER = $89D9; { 217 }
  NWE_LOGIN_NO_CONN = $89D9; { 217 }
  NWE_LOGIN_MAX_EXCEEDED = $89D9; { 217 }
  NWE_Q_HALTED = $89DA; { 218 }
  NWE_LOGIN_UNAUTHORIZED_TIME = $89DA; { 218 }
  NWE_LOGIN_UNAUTHORIZED_STATION = $89DB; { 219 }
  NWE_Q_MAX_SERVERS = $89DB; { 219 }
  NWE_ACCT_DISABLED = $89DC; { 220 }
  NWE_PASSWORD_INVALID = $89DE; { 222 }
  NWE_PASSWORD_EXPIRED = $89DF; { 223 }
  NWE_LOGIN_NO_CONN_AVAIL = $89E0; { 224 }
  NWE_E_NO_MORE_USERS = $89E7; { 231 }
  NWE_BIND_NOT_ITEM_PROP = $89E8; { 232 }
  NWE_BIND_WRITE_TO_GROUP_PROP = $89E8; { 232 }
  NWE_BIND_MEMBER_ALREADY_EXISTS = $89E9; { 233 }
  NWE_BIND_NO_SUCH_MEMBER = $89EA; { 234 }
  NWE_BIND_NOT_GROUP_PROP = $89EB; { 235 }
  NWE_BIND_NO_SUCH_SEGMENT = $89EC; { 236 }
  NWE_BIND_PROP_ALREADY_EXISTS = $89ED; { 237 }
  NWE_BIND_OBJ_ALREADY_EXISTS = $89EE; { 238 }
  NWE_BIND_NAME_INVALID = $89EF; { 239 }
  NWE_BIND_WILDCARD_INVALID = $89F0; { 240 }
  NWE_BIND_SECURITY_INVALID = $89F1; { 241 }
  NWE_BIND_OBJ_NO_READ_PRIV = $89F2; { 242 }
  NWE_BIND_OBJ_NO_RENAME_PRIV = $89F3; { 243 }
  NWE_BIND_OBJ_NO_DELETE_PRIV = $89F4; { 244 }
  NWE_BIND_OBJ_NO_CREATE_PRIV = $89F5; { 245 }
  NWE_BIND_PROP_NO_DELETE_PRIV = $89F6; { 246 }
  NWE_BIND_PROP_NO_CREATE_PRIV = $89F7; { 247 }
  NWE_BIND_PROP_NO_WRITE_PRIV = $89F8; { 248 }
  NWE_BIND_PROP_NO_READ_PRIV = $89F9; { 249 }
  NWE_NO_FREE_CONN_SLOTS = $89F9; { 249 }
  NWE_NO_MORE_SERVER_SLOTS = $89FA; { 250 }
  NWE_TEMP_REMAP_ERROR = $89FA; { 250 }
  NWE_PARAMETERS_INVALID = $89FB; { 251 }
  NWE_BIND_NO_SUCH_PROP = $89FB; { 251 }
  NWE_NCP_NOT_SUPPORTED = $89FB; { 251 }
  NWE_INET_PACKET_REQ_CANCELED = $89FC; { 252 }
  NWE_SERVER_UNKNOWN = $89FC; { 252 }
  NWE_MSG_Q_FULL = $89FC; { 252 }
  NWE_BIND_NO_SUCH_OBJ = $89FC; { 252 }
  NWE_LOCK_COLLISION = $89FD; { 253 }
  NWE_CONN_NUM_INVALID = $89FD; { 253 }
  NWE_PACKET_LEN_INVALID = $89FD; { 253 }
  NWE_UNKNOWN_REQ = $89FD; { 253 }
  NWE_BIND_LOCKED = $89FE; { 254 }
  NWE_TRUSTEE_NOT_FOUND = $89FE; { 254 }
  NWE_DIR_LOCKED = $89FE; { 254 }
  NWE_SEM_INVALID_NAME_LEN = $89FE; { 254 }
  NWE_PACKET_NOT_DELIVERABLE = $89FE; { 254 }
  NWE_SOCKET_TABLE_FULL = $89FE; { 254 }
  NWE_SPOOL_DIR_ERROR = $89FE; { 254 }
  NWE_LOGIN_DISABLED_BY_SUPER = $89FE; { 254 }
  NWE_TIMEOUT_FAILURE = $89FE; { 254 }
  NWE_FILE_EXT = $89FF; { 255 }
  NWE_FILE_NAME = $89FF; { 255 }
  NWE_HARD_FAILURE = $89FF; { 255 }
  NWE_FCB_CLOSE = $89FF; { 255 }
  NWE_IO_BOUND = $89FF; { 255 }
  NWE_BAD_SPOOL_PRINTER = $89FF; { 255 }
  NWE_BAD_RECORD_OFFSET = $89FF; { 255 }
  NWE_DRIVE_INVALID_NUM = $89FF; { 255 }
  NWE_SEM_INVALID_INIT_VAL = $89FF; { 255 }
  NWE_SEM_INVALID_HANDLE = $89FF; { 255 }
  NWE_NO_FILES_FOUND_ERROR = $89FF; { 255 }
  NWE_NO_RESPONSE_FROM_SERVER = $89FF; { 255 }
  NWE_NO_OBJ_OR_BAD_PASSWORD = $89FF; { 255 }
  NWE_PATH_NOT_LOCATABLE = $89FF; { 255 }
  NWE_Q_FULL_ERROR = $89FF; { 255 }
  NWE_REQ_NOT_OUTSTANDING = $89FF; { 255 }
  NWE_SOCKET_ALREADY_OPEN = $89FF; { 255 }
  NWE_LOCK_ERROR = $89FF; { 255 }
  NWE_FAILURE = $89FF; { 255 Generic Failure }

//*****************************************************************************
//nwfile.h
//*****************************************************************************

type

  PNW_FILE_INFO = ^TNW_FILE_INFO;
  TNW_FILE_INFO = record
    fileName: array[0..13] of Tnstr8;
    fileAttributes: Tnuint8;
    extendedFileAttributes: Tnuint8;
    fileSize: Tnuint32;
    creationDate: Tnuint16;
    lastAccessDate: Tnuint16;
    lastUpdateDateAndTime: Tnuint32;
    fileOwnerID: Tnuint32;
    lastArchiveDateAndTime: Tnuint32;
  end;

  PNW_FILE_INFO2 = ^TNW_FILE_INFO2;
  TNW_FILE_INFO2 = record
    fileAttributes: Tnuint8;
    extendedFileAttributes: Tnuint8;
    fileSize: Tnuint32;
    creationDate: Tnuint16;
    lastAccessDate: Tnuint16;
    lastUpdateDateAndTime: Tnuint32;
    fileOwnerID: Tnuint32;
    lastArchiveDateAndTime: Tnuint32;
    fileName: array[0..259] of Tnstr8;
  end;
    { 255*3 + 1  }

  PNW_FILE_INFO2_EXT = ^TNW_FILE_INFO2_EXT;
  TNW_FILE_INFO2_EXT = record
    fileAttributes: Tnuint8;
    extendedFileAttributes: Tnuint8;
    fileSize: Tnuint32;
    creationDate: Tnuint16;
    lastAccessDate: Tnuint16;
    lastUpdateDateAndTime: Tnuint32;
    fileOwnerID: Tnuint32;
    lastArchiveDateAndTime: Tnuint32;
    fileName: array[0..765] of Tnstr8;
  end;

  PSEARCH_FILE_INFO = ^TSEARCH_FILE_INFO;
  TSEARCH_FILE_INFO = record
    sequenceNumber: Tnuint16;
    reserved: Tnuint16;
    fileName: array[0..14] of Tnstr8;
    fileAttributes: Tnuint8;
    fileMode: Tnuint8;
    fileLength: Tnuint32;
    createDate: Tnuint16;
    accessDate: Tnuint16;
    updateDate: Tnuint16;
    updateTime: Tnuint16;
  end;

  PSEARCH_DIR_INFO = ^TSEARCH_DIR_INFO;
  TSEARCH_DIR_INFO = record
    sequenceNumber: Tnuint16;
    reserved1: Tnuint16;
    directoryName: array[0..14] of Tnstr8;
    directoryAttributes: Tnuint8;
    directoryAccessRights: Tnuint8;
    createDate: Tnuint16;
    createTime: Tnuint16;
    owningObjectID: Tnuint32;
    reserved2: Tnuint16;
    directoryStamp: Tnuint16;
  end;

  PCONN_OPEN_FILE = ^TCONN_OPEN_FILE;
  TCONN_OPEN_FILE = record
    taskNumber: Tnuint8;
    lockType: Tnuint8;
    accessControl: Tnuint8;
    lockFlag: Tnuint8;
    volNumber: Tnuint8;
    dirEntry: Tnuint16;
    fileName: array[0..13] of Tnstr8;
  end;

  PCONN_OPEN_FILES = ^TCONN_OPEN_FILES;
  TCONN_OPEN_FILES = record
    nextRequest: Tnuint16;
    connCount: Tnuint8;
    connInfo: array[0..21] of TCONN_OPEN_FILE;
  end;

  POPEN_FILE_CONN = ^TOPEN_FILE_CONN;
  TOPEN_FILE_CONN = record
    taskNumber: Tnuint16;
    lockType: Tnuint8;
    accessControl: Tnuint8;
    lockFlag: Tnuint8;
    volNumber: Tnuint8;
    parent: Tnuint32;
    dirEntry: Tnuint32;
    forkCount: Tnuint8;
    nameSpace: Tnuint8;
    nameLen: Tnuint8;
    fileName: array[0..254] of Tnstr8;
  end;

  POPEN_FILE_CONN_CTRL = ^TOPEN_FILE_CONN_CTRL;
  TOPEN_FILE_CONN_CTRL = record
    nextRequest: Tnuint16;
    openCount: Tnuint16;
    buffer: array[0..511] of Tnuint8;
    curRecord: Tnuint16;
  end;

  PCONN_USING_FILE = ^TCONN_USING_FILE;
  TCONN_USING_FILE = record
    connNumber: Tnuint16;
    taskNumber: Tnuint16;
    lockType: Tnuint8;
    accessControl: Tnuint8;
    lockFlag: Tnuint8;
  end;

  PCONNS_USING_FILE = ^TCONNS_USING_FILE;
  TCONNS_USING_FILE = record
    nextRequest: Tnuint16;
    useCount: Tnuint16;
    openCount: Tnuint16;
    openForReadCount: Tnuint16;
    openForWriteCount: Tnuint16;
    denyReadCount: Tnuint16;
    denyWriteCount: Tnuint16;
    locked: Tnuint8;
    forkCount: Tnuint8;
    connCount: Tnuint16;
    connInfo: array[0..69] of TCONN_USING_FILE;
  end;

const
  SEEK_FROM_BEGINNING = 1;
  SEEK_FROM_CURRENT_OFFSET = 2;
  SEEK_FROM_END = 3;
    { The following flags are to be used in the createFlag parameter of
       the NWCreateFile call.  }
  NWCREATE_NEW_FILE = 1;
  NWOVERWRITE_FILE = 2;

function NWSetCompressedFileSize(conn: TNWCONN_HANDLE; fileHandle: TNWFILE_HANDLE; reqFileSize: Tnuint32; resFileSize: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWFileServerFileCopy(srcFileHandle: TNWFILE_HANDLE; dstFileHandle: TNWFILE_HANDLE; srcOffset: Tnuint32; dstOffset: Tnuint32; bytesToCopy: Tnuint32;
  bytesCopied: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetFileConnectionID(fileHandle: TNWFILE_HANDLE; conn: PNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWGetFileConnRef(fileHandle: TNWFILE_HANDLE; connRef: pnuint32): TNWCCODE; NWLIB_UNKNOWN;

function NWFileSearchInitialize(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; volNum: pnuint8; dirID: pnuint16;
  iterhandle: pnuint16; accessRights: pnuint8): TNWCCODE; NWLIB_CALNLM32;
    { was #define dname(params) para_def_expr }
    //function NWIntFileSearchInitialize(a,b,c,d,e,f,g,h : longint) : longint;


function NWIntFileSearchContinue(conn: TNWCONN_HANDLE; volNum: Tnuint8; dirID: Tnuint16; searchContext: Tnuint16; searchAttr: Tnuint8;
  searchPath: Pnstr8; retBuf: pnuint8; augmentFlag: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
    { was #define dname(params) para_def_expr }
    // function NWScanFileInformation(a,b,c,d,e,f : longint) : longint;


function NWIntScanFileInformation(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; filePattern: Pnstr8; searchAttr: Tnuint8; iterhandle: pnint16;
  info: PNW_FILE_INFO; augmentFlag: Tnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWSetFileInformation(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; fileName: Pnstr8; searchAttrs: Tnuint8; info: PNW_FILE_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWSetFileInformation2(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; fileName: Pnstr8; searchAttrs: Tnuint8; info: PNW_FILE_INFO2): TNWCCODE; NWLIB_CALNLM32;

function NWIntScanFileInformation2(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; filePattern: Pnstr8; searchAttrs: Tnuint8; iterHandle: pnuint8;
  info: PNW_FILE_INFO2; augmentFlag: Tnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWIntScanFileInformation2Ext(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; filePattern: Pnstr8; searchAttrs: Tnuint8; iterHandle: pnuint8;
  info: PNW_FILE_INFO2_EXT; augmentFlag: Tnuint16): TNWCCODE; NWLIB_CALNLM32;

function NWSetFileAttributes(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; fileName: Pnstr8; searchAttrs: Tnuint8; newAttrs: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWGetExtendedFileAttributes2(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; extAttrs: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWScanConnectionsUsingFile(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; filePath: Pnstr8; iterhandle: pnint16; fileUse: PCONN_USING_FILE;
  fileUsed: PCONNS_USING_FILE): TNWCCODE; NWLIB_CALNLM32;
function NWScanOpenFilesByConn2(conn: TNWCONN_HANDLE; connNum: Tnuint16; iterHandle: pnint16; openCtrl: POPEN_FILE_CONN_CTRL; openFile: POPEN_FILE_CONN): TNWCCODE; NWLIB_CALNLM32;
function NWScanOpenFilesByConn(conn: TNWCONN_HANDLE; connNum: Tnuint16; iterHandle: pnint16; openFile: PCONN_OPEN_FILE; openFiles: PCONN_OPEN_FILES): TNWCCODE; NWLIB_CALNLM32;
function NWSetExtendedFileAttributes2(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; extAttrs: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWRenameFile(conn: TNWCONN_HANDLE; oldDirHandle: TNWDIR_HANDLE; oldFileName: Pnstr8; searchAttrs: Tnuint8; newDirHandle: TNWDIR_HANDLE;
  newFileName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;

function NWIntEraseFiles(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; searchAttrs: Tnuint8; augmentFlag: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWGetSparseFileBitMap(conn: TNWCONN_HANDLE; fileHandle: Tnuint32; flag: Tnint16; offset: Tnuint32; blockSize: pnuint32;
  bitMap: pnuint8): TNWCCODE; NWLIB_CALNLM32;

function NWLogPhysicalRecord(fileHandle: TNWFILE_HANDLE; recStartOffset: Tnuint32; recLength: Tnuint32; lockFlags: Tnuint8; timeOut: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWLockPhysicalRecordSet(lockFlags: Tnuint8; timeOut: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWReleasePhysicalRecordSet: TNWCCODE; NWLIB_CALNLM32;
function NWClearPhysicalRecordSet: TNWCCODE; NWLIB_CALNLM32;
function NWReleasePhysicalRecord(fileHandle: TNWFILE_HANDLE; recStartOffset: Tnuint32; recSize: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWClearPhysicalRecord(fileHandle: TNWFILE_HANDLE; recStartOffset: Tnuint32; recSize: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWLockFileLockSet(timeOut: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWReleaseFileLockSet: TNWCCODE; NWLIB_CALNLM32;
function NWClearFileLockSet: TNWCCODE; NWLIB_CALNLM32;
function NWClearFileLock2(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWReleaseFileLock2(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWLogFileLock2(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; lockFlags: Tnuint8; timeOut: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWLogLogicalRecord(conn: TNWCONN_HANDLE; logRecName: Pnstr8; lockFlags: Tnuint8; timeOut: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWLockLogicalRecordSet(lockFlags: Tnuint8; timeOut: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWReleaseLogicalRecordSet: TNWCCODE; NWLIB_CALNLM32;
function NWClearLogicalRecordSet: TNWCCODE; NWLIB_CALNLM32;
function NWReleaseLogicalRecord(conn: TNWCONN_HANDLE; logRecName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWClearLogicalRecord(conn: TNWCONN_HANDLE; logRecName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWCloseFile(fileHandle: TNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWCreateFile(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; fileName: Pnstr8; fileAttrs: Tnuint8; fileHandle: PNWFILE_HANDLE;
  createFlag: Tnflag32): TNWCCODE; NWLIB_UNKNOWN;
function NWOpenFile(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; fileName: Pnstr8; searchAttr: Tnuint16; accessRights: Tnuint8;
  fileHandle: PNWFILE_HANDLE): TNWCCODE; NWLIB_UNKNOWN;
function NWReadFile(fileHandle: TNWFILE_HANDLE; bytesToRead: Tnuint32; bytesActuallyRead: pnuint32; data: pnuint8): TNWCCODE; NWLIB_UNKNOWN;
function NWWriteFile(fileHandle: TNWFILE_HANDLE; bytesToWrite: Tnuint32; data: pnuint8): TNWCCODE; NWLIB_UNKNOWN;
function NWCommitFile(fileHandle: TNWFILE_HANDLE): TNWCCODE; NWLIB_UNKNOWN;
function NWGetEOF(fileHandle: TNWFILE_HANDLE; getEOF: pnuint32): TNWCCODE; NWLIB_UNKNOWN;
function NWSetEOF(fileHandle: TNWFILE_HANDLE; setEOF: Tnuint32): TNWCCODE; NWLIB_UNKNOWN;
function NWGetFilePos(fileHandle: TNWFILE_HANDLE; filePos: pnuint32): TNWCCODE; NWLIB_UNKNOWN;
function NWSetFilePos(fileHandle: TNWFILE_HANDLE; mode: Tnuint; filePos: Tnuint32): TNWCCODE; NWLIB_UNKNOWN;
function NWGetFileDirEntryNumber(fileHandle: TNWFILE_HANDLE; volumeNum: pnuint32; directoryEntry: pnuint32; DOSDirectoryEntry: pnuint32; nameSpace: pnuint32;
  dataStream: pnuint32; parentDirEntry: pnuint32; parentDOSDirEntry: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetDirectoryEntryNumber(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; volumeNum: pnuint32; directoryEntry: pnuint32; DOSDirectoryEntry: pnuint32;
  nameSpace: pnuint32; parentDirEntry: pnuint32; parentDOSDirEntry: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetNSFileDirEntryNumber(fileHandle: TNWFILE_HANDLE; nameSpace: Tnuint8; volumeNum: pnuint32; directoryEntry: pnuint32; dataStream: pnuint32): TNWCCODE; NWLIB_CALNLM32;


//*****************************************************************************
//nwmisc.h
//*****************************************************************************

const
  NW_SHORT_NAME_SERVER = 0;
  NW_LONG_NAME_SERVER = 1;
  NW_ENCP_SERVER = 1;
  NW_EXTENDED_NCP_SERVER = 1;
  _NETX_COM = $0001;
  _NETX_VLM = $0002;
  _REDIR_LOADED = $4000;
  _VLM_LOADED = $8000;

type
  PNW_DATE = ^TNW_DATE;
  TNW_DATE = record
    day: Tnuint8;
    month: Tnuint8;
    year: Tnuint16;
  end;
    { hours is a word  so that this structure will be the same length as a dword  }

  PNW_TIME = ^TNW_TIME;
  TNW_TIME = record
    seconds: Tnuint8;
    minutes: Tnuint8;
    hours: Tnuint16;
  end;

  PNW_REQUESTER_TYPE = ^TNW_REQUESTER_TYPE;
  TNW_REQUESTER_TYPE = Longint;
const
  NW_LONG_NAME_REQUESTER = 0;
  NW_SHORT_NAME_REQUESTER = 1;
  NW_ERROR_ON_REQUESTER_TYPE = 2;
type

  PNW_FRAGMENT = ^TNW_FRAGMENT;
  TNW_FRAGMENT = record
    fragAddress: Tnptr;
    fragSize: Tnuint32;
    fragSize16: Tnuint16;
  end;

  PCONN_TASK = ^TCONN_TASK;
  TCONN_TASK = record
    taskNumber: Tnuint16;
    taskState: Tnuint8;
  end;
    { use NW_ constants from nwserver.h  }
    { this field is only valid in 3.11  }
    { this field is only valid in 3.11  }
    { this field is only valid in 2.x   }

  PCONN_TASK_INFO = ^TCONN_TASK_INFO;
  TCONN_TASK_INFO = record
    serverVersion: Tnuint16;
    lockState: Tnuint8;
    waitingTaskNumber: Tnuint16;
    recordStart: Tnuint32;
    recordEnd: Tnuint32;
    volNumber: Tnuint8;
    dirEntry: Tnuint32;
    nameSpace: Tnuint8;
    dirID: Tnuint16;
    lockedName: array[0..255] of Tnstr8;
    taskCount: Tnuint8;
    tasks: array[0..255] of TCONN_TASK;
  end;

  PDIR_ENTRY = ^TDIR_ENTRY;
  TDIR_ENTRY = record
    volNumber: Tnuint8;
    dirEntry: Tnuint32;
  end;

procedure NWUnpackDateTime(dateTime: Tnuint32; sDate: PNW_DATE; sTime: PNW_TIME); NWLIB_CALNLM32;
procedure NWUnpackDate(date: Tnuint16; sDate: PNW_DATE); NWLIB_CALNLM32;
procedure NWUnpackTime(time: Tnuint16; sTime: PNW_TIME); NWLIB_CALNLM32;
function NWPackDateTime(sDate: PNW_DATE; sTime: PNW_TIME): Tnuint32; NWLIB_CALNLM32;
function NWPackDate(sDate: PNW_DATE): Tnuint16; NWLIB_CALNLM32;
function NWPackTime(sTime: PNW_TIME): Tnuint16; NWLIB_CALNLM32;
    { Avoid using the following three NWConvertDate/Time functions,
       they just call the NWUnpackDate/Time functions. They are here for
       compatibility reasons only.  }
procedure NWConvertDateTime(dateTime: Tnuint32; sDate: PNW_DATE; sTime: PNW_TIME); NWLIB_CALNLM32;
procedure NWConvertDate(date: Tnuint16; sDate: PNW_DATE); NWLIB_CALNLM32;
procedure NWConvertTime(time: Tnuint16; sTime: PNW_TIME); NWLIB_CALNLM32;
function NWRequest(conn: TNWCONN_HANDLE; _function: Tnuint16; numReqFrags: Tnuint16; reqFrags: PNW_FRAGMENT; numReplyFrags: Tnuint16;
  replyFrags: PNW_FRAGMENT): TNWCCODE; NWLIB_CALNLM32;
function _NWGetRequesterType(_type: PNW_REQUESTER_TYPE): TNWCCODE; NWLIB_CALNLM32;
function NWWordSwap(swapWord: Tnuint16): Tnuint16; NWLIB_CALNLM32;
function NWLongSwap(swapLong: Tnuint32): Tnuint32; NWLIB_CALNLM32;
function NWInitDBCS: Tnint16; NWLIB_UNKNOWN;
function NWConvertPathToDirEntry(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; dirEntry: PDIR_ENTRY): TNWCCODE; NWLIB_CALNLM32;
function NWGetTaskInformationByConn(conn: TNWCONN_HANDLE; connNum: Tnuint16; taskInfo: PCONN_TASK_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetRequesterVersion(majorVer: pnuint8; minorVer: pnuint8; revision: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWIsLNSSupportedOnVolume(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWConvertFileHandle(fileHandle: TNWFILE_HANDLE; handleType: Tnuint16; NWHandle: pnuint8; conn: PNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWConvertFileHandleConnRef(fileHandle: TNWFILE_HANDLE; handleType: Tnuint16; NWHandle: pnuint8; connRef: pnuint32): TNWCCODE; NWLIB_UNKNOWN;
procedure _NWConvert4ByteTo6ByteHandle(NW4ByteHandle: pnuint8; NW6ByteHandle: pnuint8); NWLIB_CALNLM32;
function NWEndOfJob: TNWCCODE; NWLIB_UNKNOWN;
function NWCallsInit(reserved1: Tnptr; reserved2: Tnptr): TNWCCODE; NWLIB_CALNLM32;
function NWCallsTerm(reserved: Tnptr): TNWCCODE; NWLIB_CALNLM32;
function NWGetClientType: Tnuint16; NWLIB_CALNLM32;
function __NWGetNWCallsState: Tnuint16; NWLIB_UNKNOWN;
function NWSetNetWareErrorMode(errorMode: Tnuint8; prevMode: pnuint8): TNWCCODE; NWLIB_UNKNOWN;
function NWSetEndOfJobStatus(endOfJobStatus: Tnuint8; prevStatus: pnuint8): TNWCCODE; NWLIB_UNKNOWN;
procedure NWGetNWCallsVersion(majorVer: pnuint8; minorVer: pnuint8; revLevel: pnuint8; betaLevel: pnuint8); NWLIB_CALNLM32;
function NWConvertHandle(conn: TNWCONN_HANDLE; accessMode: Tnuint8; NWHandle: pointer; handleSize: Tnuint16; fileSize: Tnuint32;
  fileHandle: PNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;


//*****************************************************************************
//nwmsg.h
//*****************************************************************************

function NWDisableBroadcasts(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWEnableBroadcasts(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWSendBroadcastMessage(conn: TNWCONN_HANDLE; message: Pnstr8; connCount: Tnuint16; connList: Pnuint16; resultList: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWGetBroadcastMessage(conn: TNWCONN_HANDLE; message: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWSetBroadcastMode(conn: TNWCONN_HANDLE; mode: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWBroadcastToConsole(conn: TNWCONN_HANDLE; message: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWSendConsoleBroadcast(conn: TNWCONN_HANDLE; message: Pnstr8; connCount: Tnuint16; connList: pnuint16): TNWCCODE; NWLIB_CALNLM32;



//*****************************************************************************
//nwnamspc.h
//*****************************************************************************

const
  SUCCESSFUL = 0;
  MORE_NS_TO_READ = 0;
  NO_EXTENDED_NS_INFO = 9;
  NS_EOF = $8910;
  NW_NS_DOS = 0;
  NW_NS_MAC = 1;
  NW_NS_NFS = 2;
  NW_NS_FTAM = 3;
  NW_NS_OS2 = 4;
  NW_NS_LONG = 4;
  NW_DS_DOS = 0;
  NW_DS_MAC = 1;
  NW_DS_FTAM = 2;

type
  PNWNSINFO = ^TNWNSINFO;
  TNWNSINFO = record
    NSInfoBitMask: Tnuint32;
    fixedBitMask: Tnuint32;
    reservedBitMask: Tnuint32;
    extendedBitMask: Tnuint32;
    fixedBitsDefined: Tnuint16;
    reservedBitsDefined: Tnuint16;
    extendedBitsDefined: Tnuint16;
    fieldsLenTable: array[0..31] of Tnuint32;
    hugeStateInfo: array[0..15] of Tnuint8;
    hugeDataLength: Tnuint32;
  end;
  TNW_NS_INFO = TNWNSINFO;
  PNW_NS_INFO = ^TNW_NS_INFO;

  PNW_ENTRY_INFO = ^TNW_ENTRY_INFO;
  TNW_ENTRY_INFO = record
    spaceAlloc: Tnuint32;
    attributes: Tnuint32;
    flags: Tnuint16;
    dataStreamSize: Tnuint32;
    totalStreamSize: Tnuint32;
    numberOfStreams: Tnuint16;
    creationTime: Tnuint16;
    creationDate: Tnuint16;
    creatorID: Tnuint32;
    modifyTime: Tnuint16;
    modifyDate: Tnuint16;
    modifierID: Tnuint32;
    lastAccessDate: Tnuint16;
    archiveTime: Tnuint16;
    archiveDate: Tnuint16;
    archiverID: Tnuint32;
    inheritedRightsMask: Tnuint16;
    dirEntNum: Tnuint32;
    DosDirNum: Tnuint32;
    volNumber: Tnuint32;
    EADataSize: Tnuint32;
    EAKeyCount: Tnuint32;
    EAKeySize: Tnuint32;
    NSCreator: Tnuint32;
    nameLength: Tnuint8;
    entryName: array[0..255] of Tnstr8;
  end;
    { 255*3 + 1  }

  PNW_ENTRY_INFO_EXT = ^TNW_ENTRY_INFO_EXT;
  TNW_ENTRY_INFO_EXT = record
    spaceAlloc: Tnuint32;
    attributes: Tnuint32;
    flags: Tnuint16;
    dataStreamSize: Tnuint32;
    totalStreamSize: Tnuint32;
    numberOfStreams: Tnuint16;
    creationTime: Tnuint16;
    creationDate: Tnuint16;
    creatorID: Tnuint32;
    modifyTime: Tnuint16;
    modifyDate: Tnuint16;
    modifierID: Tnuint32;
    lastAccessDate: Tnuint16;
    archiveTime: Tnuint16;
    archiveDate: Tnuint16;
    archiverID: Tnuint32;
    inheritedRightsMask: Tnuint16;
    dirEntNum: Tnuint32;
    DosDirNum: Tnuint32;
    volNumber: Tnuint32;
    EADataSize: Tnuint32;
    EAKeyCount: Tnuint32;
    EAKeySize: Tnuint32;
    NSCreator: Tnuint32;
    nameLength: Tnuint16;
    entryName: array[0..765] of Tnstr8;
  end;

  PNW_DATA_STREAM_FAT_INFO = ^TNW_DATA_STREAM_FAT_INFO;
  TNW_DATA_STREAM_FAT_INFO = record
    dataStreamNumber: Tnuint32;
    dataStreamFATBlocksSize: Tnuint32;
  end;

  PNW_DATA_STREAM_SIZE_INFO = ^TNW_DATA_STREAM_SIZE_INFO;
  TNW_DATA_STREAM_SIZE_INFO = record
    dataStreamNumber: Tnuint32;
    dataStreamSize: Tnuint32;
  end;

  PNW_MAC_TIME = ^TNW_MAC_TIME;
  TNW_MAC_TIME = record
    MACCreateTime: Tnuint32;
    MACBackupTime: Tnuint32;
  end;

  PNW_ENTRY_INFO2 = ^TNW_ENTRY_INFO2;
  TNW_ENTRY_INFO2 = record
    spaceAlloc: Tnuint32;
    attributes: Tnuint32;
    flags: Tnuint16;
    dataStreamSize: Tnuint32;
    totalStreamSize: Tnuint32;
    numberOfStreams: Tnuint16;
    EADataSize: Tnuint32;
    EAKeyCount: Tnuint32;
    EAKeySize: Tnuint32;
    archiveTime: Tnuint16;
    archiveDate: Tnuint16;
    archiverID: Tnuint32;
    modifyTime: Tnuint16;
    modifyDate: Tnuint16;
    modifierID: Tnuint32;
    lastAccessDate: Tnuint16;
    creationTime: Tnuint16;
    creationDate: Tnuint16;
    creatorID: Tnuint32;
    NSCreator: Tnuint32;
    dirEntNum: Tnuint32;
    DosDirNum: Tnuint32;
    volNumber: Tnuint32;
    inheritedRightsMask: Tnuint16;
    currentReferenceID: Tnuint16;
    NSFileAttributes: Tnuint32;
    numberOfDataStreamFATInfo: Tnuint32;
    dataStreamFATInfo: array[0..2] of TNW_DATA_STREAM_FAT_INFO;
    numberOfDataStreamSizeInfo: Tnuint32;
    dataStreamSizeInfo: array[0..2] of TNW_DATA_STREAM_SIZE_INFO;
    secondsRelativeToTheYear2000: Tnint32;
    DOSNameLen: Tnuint8;
    DOSName: array[0..12] of Tnstr8;
    flushTime: Tnuint32;
    parentBaseID: Tnuint32;
    MacFinderInfo: array[0..31] of Tnuint8;
    siblingCount: Tnuint32;
    effectiveRights: Tnuint32;
    MacTime: TNW_MAC_TIME;
    lastAccessedTime: Tnuint16;
    nameLength: Tnuint8;
    entryName: array[0..255] of Tnstr8;
  end;

  PMODIFY_DOS_INFO = ^TMODIFY_DOS_INFO;
  TMODIFY_DOS_INFO = record
    attributes: Tnuint32;
    createDate: Tnuint16;
    createTime: Tnuint16;
    creatorID: Tnuint32;
    modifyDate: Tnuint16;
    modifyTime: Tnuint16;
    modifierID: Tnuint32;
    archiveDate: Tnuint16;
    archiveTime: Tnuint16;
    archiverID: Tnuint32;
    lastAccessDate: Tnuint16;
    inheritanceGrantMask: Tnuint16;
    inheritanceRevokeMask: Tnuint16;
    maximumSpace: Tnuint32;
  end;

  PSEARCH_SEQUENCE = ^TSEARCH_SEQUENCE;
  TSEARCH_SEQUENCE = record
    volNumber: Tnuint8;
    dirNumber: Tnuint32;
    searchDirNumber: Tnuint32;
  end;

  PNW_NS_PATH = ^TNW_NS_PATH;
  TNW_NS_PATH = record
    srcPath,
      dstPath: Pnstr8;
    dstPathSize: Tnuint16;
  end;

  PNW_NS_OPENCREATE = ^TNW_NS_OPENCREATE;
  TNW_NS_OPENCREATE = record
    openCreateMode: Tnuint8;
    searchAttributes: Tnuint16;
    reserved: Tnuint32;
    createAttributes: Tnuint32;
    accessRights: Tnuint16;
    NetWareHandle: Tnuint32;
    openCreateAction: Tnuint8;
  end;
  TNW_NS_OPEN = TNW_NS_OPENCREATE;
  PNW_NS_OPEN = ^TNW_NS_OPEN;
    { open/create modes  }

const
  OC_MODE_OPEN = $01;
  OC_MODE_TRUNCATE = $02;
  OC_MODE_REPLACE = $02;
  OC_MODE_CREATE = $08;
    { open/create results  }
  OC_ACTION_NONE = $00;
  OC_ACTION_OPEN = $01;
  OC_ACTION_CREATE = $02;
  OC_ACTION_TRUNCATE = $04;
  OC_ACTION_REPLACE = $04;
    { return info mask  }
  IM_NAME = $0001;
  IM_ENTRY_NAME = $0001;
  IM_SPACE_ALLOCATED = $0002;
  IM_ATTRIBUTES = $0004;
  IM_SIZE = $0008;
  IM_TOTAL_SIZE = $0010;
  IM_EA = $0020;
  IM_ARCHIVE = $0040;
  IM_MODIFY = $0080;
  IM_CREATION = $0100;
  IM_OWNING_NAMESPACE = $0200;
  IM_DIRECTORY = $0400;
  IM_RIGHTS = $0800;
  IM_ALMOST_ALL = $0FED;
  IM_ALL = $0FFF;
  IM_REFERENCE_ID = $1000;
  IM_NS_ATTRIBUTES = $2000;
  IM_DATASTREAM_SIZES = $4000;
  IM_DATASTREAM_ACTUAL = $4000;
  IM_DATASTREAM_LOGICAL = $8000;
  IM_LASTUPDATEDINSECONDS = $00010000;
  IM_DOSNAME = $00020000;
  IM_FLUSHTIME = $00040000;
  IM_PARENTBASEID = $00080000;
  IM_MACFINDER = $00100000;
  IM_SIBLINGCOUNT = $00200000;
  IM_EFECTIVERIGHTS = $00400000;
  IM_MACTIME = $00800000;
  IM_LASTACCESSEDTIME = $01000000;
  IM_EXTENDED_ALL = $01FFF000;
  IM_NSS_LARGE_SIZES = $40000000;
  IM_COMPRESSED_INFO = $80000000;
  IM_NS_SPECIFIC_INFO = $80000000;
    { access rights attributes  }

const
  NW_TYPE_FILE = $8000;
  NW_TYPE_SUBDIR = $0010;
  NW_NAME_CONVERT = $03;
  NW_NO_NAME_CONVERT = $04;
    { modify mask - use with MODIFY_DOS_INFO structure  }
  DM_FILENAME = $0001;
  DM_ATTRIBUTES = $0002;
  DM_CREATE_DATE = $0004;
  DM_CREATE_TIME = $0008;
  DM_CREATOR_ID = $0010;
  DM_ARCHIVE_DATE = $0020;
  DM_ARCHIVE_TIME = $0040;
  DM_ARCHIVER_ID = $0080;
  DM_MODIFY_DATE = $0100;
  DM_MODIFY_TIME = $0200;
  DM_MODIFIER_ID = $0400;
  DM_LAST_ACCESS_DATE = $0800;
  DM_INHERITED_RIGHTS_MASK = $1000;
  DM_MAXIMUM_SPACE = $2000;

{$IF defined( N_PLAT_NLM )}
    {const
       NWGetNSLoadedList = NWGetNSLoadedList2;
       NWGetNSInfo = NWGetNSInfo2;}
{$ENDIF}


function NWGetDirectoryBase(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; path: Pnstr8; dstNamSpc: Tnuint8; idxStruct: PNW_IDX): TNWCCODE; NWLIB_CALNLM32;
function NWGetDirectoryBaseExt(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; path: Pnstr8; dstNamSpc: Tnuint8; idxStruct: PNW_IDX): TNWCCODE; NWLIB_CALNLM32;
function NWScanNSEntryInfo(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; namSpc: Tnuint8; attrs: Tnuint16; sequence: PSEARCH_SEQUENCE;
  searchPattern: Pnstr8; retInfoMask: Tnuint32; entryInfo: PNW_ENTRY_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWScanNSEntryInfoExt(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; namSpc: Tnuint8; attrs: Tnuint16; sequence: PSEARCH_SEQUENCE;
  searchPattern: Pnstr8; retInfoMask: Tnuint32; entryInfo: PNW_ENTRY_INFO_EXT): TNWCCODE; NWLIB_CALNLM32;
function NWScanNSEntryInfo2(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; namSpc: Tnuint8; attrs: Tnuint16; sequence: PSEARCH_SEQUENCE;
  searchPattern: Pnstr8; retInfoMask: Tnuint32; entryInfo2: PNW_ENTRY_INFO2): TNWCCODE; NWLIB_CALNLM32;
function NWGetNSLoadedList(conn: TNWCONN_HANDLE; volNum: Tnuint8; maxListLen: Tnuint8; NSLoadedList: pnuint8; actualListLen: pnuint8): TNWCCODE; NWLIB_CLIB;
function NWGetOwningNameSpace(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; path: Pnstr8; namSpc: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWOpenCreateNSEntry(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; namSpc: Tnuint8; path: Pnstr8; NSOpenCreate: PNW_NS_OPENCREATE;
  fileHandle: PNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWOpenCreateNSEntryExt(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; namSpc: Tnuint8; path: Pnstr8; NSOpenCreate: PNW_NS_OPENCREATE;
  fileHandle: PNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWOpenNSEntry(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; namSpc: Tnuint8; dataStream: Tnuint8; path: Pnstr8;
  NSOpen: PNW_NS_OPEN; fileHandle: PNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWOpenNSEntryExt(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; namSpc: Tnuint8; dataStream: Tnuint8; path: Pnstr8;
  NSOpen: PNW_NS_OPEN; fileHandle: PNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWSetLongName(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; namSpc: Tnuint8; dstPath: Pnstr8; dstType: Tnuint16;
  longName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWGetLongName(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; path: Pnstr8; srcNamSpc: Tnuint8; dstNamSpc: Tnuint8;
  longName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWGetLongNameExt(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; path: Pnstr8; srcNamSpc: Tnuint8; dstNamSpc: Tnuint8;
  longName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWGetNSInfo(conn: TNWCONN_HANDLE; idxStruct: PNW_IDX; NSInfo: PNW_NS_INFO): TNWCCODE; NWLIB_CLIB;
function NWWriteNSInfo(conn: TNWCONN_HANDLE; idxStruct: PNW_IDX; NSInfo: PNW_NS_INFO; data: Pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWWriteNSInfoExt(conn: TNWCONN_HANDLE; idxStruct: PNW_IDX; NSInfo: PNW_NS_INFO; data: Pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWWriteExtendedNSInfo(conn: TNWCONN_HANDLE; idxStruct: PNW_IDX; NSInfo: PNW_NS_INFO; data: Pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWReadNSInfo(conn: TNWCONN_HANDLE; idxStruct: PNW_IDX; NSInfo: PNW_NS_INFO; data: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWReadNSInfoExt(conn: TNWCONN_HANDLE; idxStruct: PNW_IDX; NSInfo: PNW_NS_INFO; data: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWReadExtendedNSInfo(conn: TNWCONN_HANDLE; idxStruct: PNW_IDX; NSInfo: PNW_NS_INFO; data: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWGetNSPath(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; fileFlag: Tnuint16; srcNamSpc: Tnuint8; dstNamSpc: Tnuint8;
  NSPath: PNW_NS_PATH): TNWCCODE; NWLIB_CALNLM32;
function NWGetNSPathExt(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; fileFlag: Tnuint16; srcNamSpc: Tnuint8; dstNamSpc: Tnuint8;
  NSPath: PNW_NS_PATH): TNWCCODE; NWLIB_CALNLM32;
function NWAllocTempNSDirHandle2(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; path: Pnstr8; namSpc: Tnuint8; newDirHandle: pnuint8;
  newNamSpc: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWAllocTempNSDirHandle2Ext(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; path: Pnstr8; namSpc: Tnuint8; newDirHandle: pnuint8;
  newNamSpc: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWGetNSEntryInfo(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; path: Pnstr8; srcNamSpc: Tnuint8; dstNamSpc: Tnuint8;
  searchAttrs: Tnuint16; retInfoMask: Tnuint32; entryInfo: PNW_ENTRY_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetNSEntryInfoExt(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; path: Pnstr8; srcNamSpc: Tnuint8; dstNamSpc: Tnuint8;
  searchAttrs: Tnuint16; retInfoMask: Tnuint32; entryInfo: PNW_ENTRY_INFO_EXT): TNWCCODE; NWLIB_CALNLM32;
function NWNSGetMiscInfo(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; path: Pnstr8; dstNameSpace: Tnuint8; idxStruct: PNW_IDX): TNWCCODE; NWLIB_CALNLM32;
function NWOpenDataStream(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; fileName: Pnstr8; dataStream: Tnuint16; attrs: Tnuint16;
  accessMode: Tnuint16; NWHandle: pnuint32; fileHandle: PNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWNSRename(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; namSpc: Tnuint8; oldName: Pnstr8; oldType: Tnuint16;
  newName: Pnstr8; renameFlag: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWNSRenameExt(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; namSpc: Tnuint8; oldName: Pnstr8; oldType: Tnuint16;
  newName: Pnstr8; renameFlag: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWSetNSEntryDOSInfo(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; path: Pnstr8; namSpc: Tnuint8; searchAttrs: Tnuint16;
  modifyDOSMask: Tnuint32; dosInfo: PMODIFY_DOS_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWSetNSEntryDOSInfoExt(conn: TNWCONN_HANDLE; dirHandle: Tnuint8; path: Pnstr8; namSpc: Tnuint8; searchAttrs: Tnuint16;
  modifyDOSMask: Tnuint32; dosInfo: PMODIFY_DOS_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetFullPath(conn: TNWCONN_HANDLE; volNum: Tnuint8; dirBase: Tnuint32; handleFlag: Tnuint16; srcNamSpc: Tnint;
  dstNamSpc: Tnint; maxPathLen: Tnuint16; path: Pnstr8; pathType: pnuint16): TNWCCODE; NWLIB_UNKNOWN;
function NWDeleteNSEntry(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; fileName: Pnstr8; nameSpace: Tnuint8; searchAttr: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWDeleteNSEntryExt(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; fileName: Pnstr8; nameSpace: Tnuint8; searchAttr: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWNSGetDefaultNS(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; pbuDefaultNameSpace: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWScanNSEntryInfoSet(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; buNameSpace: Tnuint8; suAttr: Tnuint16; pIterHnd: PSEARCH_SEQUENCE;
  pbstrSrchPattern: Pnstr8; luRetMask: Tnuint32; pbuMoreEntriesFlag: pnuint8; psuNumReturned: pnuint16; suNumItems: Tnuint16;
  pEntryInfo: PNW_ENTRY_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWAddTrusteeToNSDirectory(conn: TNWCONN_HANDLE; namSpc: Tnuint8; dirHandle: TNWDIR_HANDLE; path: Pnstr8; trusteeID: Tnuint32;
  rightsMask: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWDeleteTrusteeFromNSDirectory(conn: TNWCONN_HANDLE; namSpc: Tnuint8; dirHandle: TNWDIR_HANDLE; dirPath: Pnstr8; objID: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWScanNSDirectoryForTrustees(conn: TNWCONN_HANDLE; namSpc: Tnuint8; dirHandle: TNWDIR_HANDLE; pbstrSrchPath: Pnstr8; pluIterHnd: pnuint32;
  pbstrDirName: Pnstr8; pluDirDateTime: pnuint32; pluOwnerID: pnuint32; trusteeList: PTRUSTEE_INFO): TNWCCODE; NWLIB_CALNLM32;

function SetCurrentNameSpace(newNameSpace: Tnuint8): Tnuint8; NWLIB_CLIB;
function SetTargetNameSpace(newNameSpace: Tnuint8): Tnuint8; NWLIB_CLIB;

//*****************************************************************************
//nwprint.h
//*****************************************************************************

const
  LPT1 = 1;
  LPT2 = 2;
  LPT3 = 3;
  LPT4 = 4;
  LPT5 = 5;
  LPT6 = 6;
  LPT7 = 7;
  LPT8 = 8;
  LPT9 = 9;
  START_CAPTURE = 1;
  END_CAPTURE = 2;
  CANCEL_CAPTURE = 3;
  GET_PRINT_JOB_FLAGS = 4;
  SET_PRINT_JOB_FLAGS = 5;
  GET_BANNER_USER_NAME = 6;
  SET_BANNER_USER_NAME = 7;
  GET_PRINTER_SETUP_STRING = 8;
  SET_PRINTER_SETUP_STRING = 9;
  GET_PRINTER_RESET_STRING = 10;
  SET_PRINTER_RESET_STRING = 11;
    { must be set to zeros  }
type

  PPrintJobStruct = ^TPrintJobStruct;
  TPrintJobStruct = record
    clientStation: Tnuint8;
    clientTask: Tnuint8;
    clientID: Tnuint32;
    targetServerID: Tnuint32;
    targetExecutionTime: array[0..5] of Tnuint8;
    jobEntryTime: array[0..5] of Tnuint8;
    jobNumber: Tnuint16;
    formType: Tnuint16;
    jobPosition: Tnuint8;
    jobControlFlags: Tnuint8;
    jobFileName: array[0..13] of Tnuint8;
    jobFileHandle: array[0..5] of Tnuint8;
    servicingServerStation: Tnuint8;
    servicingServerTask: Tnuint8;
    servicingServerID: Tnuint32;
    jobDescription: array[0..49] of Tnuint8;
    clientJobInfoVer: Tnuint8;
    tabSize: Tnuint8;
    numberCopies: Tnuint16;
    printFlags: Tnuint16;
    maxLines: Tnuint16;
    maxChars: Tnuint16;
    formName: array[0..15] of Tnuint8;
    reserved: array[0..5] of Tnuint8;
    bannerUserName: array[0..12] of Tnuint8;
    bannerFileName: array[0..12] of Tnuint8;
    bannerHeaderFileName: array[0..13] of Tnuint8;
    filePathName: array[0..79] of Tnuint8;
  end;
    { must be set to zeros  }

  PNWPrintJobStruct = ^TNWPrintJobStruct;
  TNWPrintJobStruct = record
    clientStation: Tnuint32;
    clientTask: Tnuint32;
    clientID: Tnuint32;
    targetServerID: Tnuint32;
    targetExecutionTime: array[0..5] of Tnuint8;
    jobEntryTime: array[0..5] of Tnuint8;
    jobNumber: Tnuint32;
    formType: Tnuint16;
    jobPosition: Tnuint16;
    jobControlFlags: Tnuint16;
    jobFileName: array[0..13] of Tnuint8;
    jobFileHandle: Tnuint32;
    servicingServerStation: Tnuint32;
    servicingServerTask: Tnuint32;
    servicingServerID: Tnuint32;
    jobDescription: array[0..49] of Tnuint8;
    clientJobInfoVer: Tnuint8;
    tabSize: Tnuint8;
    numberCopies: Tnuint16;
    printFlags: Tnuint16;
    maxLines: Tnuint16;
    maxChars: Tnuint16;
    formName: array[0..15] of Tnuint8;
    reserved: array[0..5] of Tnuint8;
    bannerUserName: array[0..12] of Tnuint8;
    bannerFileName: array[0..12] of Tnuint8;
    bannerHeaderFileName: array[0..13] of Tnuint8;
    filePathName: array[0..79] of Tnuint8;
  end;

  PPRINTER_STATUS = ^TPRINTER_STATUS;
  TPRINTER_STATUS = record
    printerHalted: Tnuint8;
    printerOffline: Tnuint8;
    currentFormType: Tnuint8;
    redirectedPrinter: Tnuint8;
  end;
    { OS/2, VLM only                          }
    { VLM returns or sets only 12 characters  }
    { plus the NULL -- a total of 13 byte's    }
    { OS/2, VLM only  }
    { DOS/WIN only  }
    { DOS/WIN only  }

  PNWCAPTURE_FLAGSRW = ^TNWCAPTURE_FLAGSRW;
  TNWCAPTURE_FLAGSRW = record
    jobDescription: array[0..49] of Tnuint8;
    jobControlFlags: Tnuint8;
    tabSize: Tnuint8;
    numCopies: Tnuint16;
    printFlags: Tnuint16;
    maxLines: Tnuint16;
    maxChars: Tnuint16;
    formName: array[0..12] of Tnuint8;
    reserved: array[0..8] of Tnuint8;
    formType: Tnuint16;
    bannerText: array[0..12] of Tnuint8;
    reserved2: Tnuint8;
    flushCaptureTimeout: Tnuint16;
    flushCaptureOnClose: Tnuint8;
  end;


  TNWCAPTURE_FLAGS1 = TNWCAPTURE_FLAGSRW;
  PNWCAPTURE_FLAGS1 = ^TNWCAPTURE_FLAGS1;

    { DOS/WIN only  }
    { DOS/WIN only  }
    { DOS/WIN only  }
    { DOS/WIN only  }
    { DOS/WIN only  }
    { DOS/WIN only  }
    { VLM only      }


  PNWCAPTURE_FLAGSRO = ^TNWCAPTURE_FLAGSRO;
  TNWCAPTURE_FLAGSRO = record
    connID: TNWCONN_HANDLE;
    queueID: Tnuint32;
    setupStringMaxLen: Tnuint16;
    resetStringMaxLen: Tnuint16;
    LPTCaptureFlag: Tnuint8;
    fileCaptureFlag: Tnuint8;
    timingOutFlag: Tnuint8;
    inProgress: Tnuint8;
    printQueueFlag: Tnuint8;
    printJobValid: Tnuint8;
    queueName: array[0..64] of Tnstr8;
  end;


  TNWCAPTURE_FLAGS2 = TNWCAPTURE_FLAGSRO;
  PNWCAPTURE_FLAGS2 = ^TNWCAPTURE_FLAGS2;
    { DOS/WIN only  }
    { DOS/WIN only  }
    { DOS/WIN only  }
    { DOS/WIN only  }
    { DOS/WIN only  }
    { DOS/WIN only  }
    { VLM only      }
type

  PNWCAPTURE_FLAGSRO3 = ^TNWCAPTURE_FLAGSRO3;
  TNWCAPTURE_FLAGSRO3 = record
    connRef: Tnuint32;
    queueID: Tnuint32;
    setupStringMaxLen: Tnuint16;
    resetStringMaxLen: Tnuint16;
    LPTCaptureFlag: Tnuint8;
    fileCaptureFlag: Tnuint8;
    timingOutFlag: Tnuint8;
    inProgress: Tnuint8;
    printQueueFlag: Tnuint8;
    printJobValid: Tnuint8;
    queueName: array[0..64] of Tnstr8;
  end;


  TNWCAPTURE_FLAGS3 = TNWCAPTURE_FLAGSRO3;
  PNWCAPTURE_FLAGS3 = ^TNWCAPTURE_FLAGS3;


  PCaptureFlagsStruct = ^TCaptureFlagsStruct;
  TCaptureFlagsStruct = record
    status: Tnuint8;
    flags: Tnuint8;
    tabSize: Tnuint8;
    serverPrinter: Tnuint8;
    numberCopies: Tnuint8;
    formType: Tnuint8;
    reserved: Tnuint8;
    bannerText: array[0..12] of Tnuint8;
    reserved2: Tnuint8;
    localLPTDevice: Tnuint8;
    captureTimeOutCount: Tnuint16;
    captureOnDeviceClose: Tnuint8;
  end;

function NWGetPrinterDefaults(status: pnuint8; flags: pnuint8; tabSize: pnuint8; serverPrinter: pnuint8; numberCopies: pnuint8;
  formType: pnuint8; bannerText: Pnstr8; localLPTDevice: pnuint8; captureTimeOutCount: pnuint16; captureOnDeviceClose: pnuint8): TNWCCODE; NWLIB_UNKNOWN;
function NWSetPrinterDefaults(flags: Tnuint8; tabSize: Tnuint8; serverPrinter: Tnuint8; numberCopies: Tnuint8; formType: Tnuint8;
  bannerText: Pnstr8; localLPTDevice: Tnuint8; captureTimeOutCount: Tnuint16; captureOnDeviceClose: Tnuint8): TNWCCODE; NWLIB_UNKNOWN;
function NWStartLPTCapture(deviceID: Tnuint16): TNWCCODE; NWLIB_UNKNOWN;
function NWGetLPTCaptureStatus(conn: PNWCONN_HANDLE): TNWCCODE; NWLIB_UNKNOWN;

function NWSpoolStartCapture(deviceID: Tnuint16; queueID: Tnuint32; conn: TNWCONN_HANDLE; scope: Tnuint16): TNWCCODE; NWLIB_UNKNOWN;
function NWSpoolEndCapture(deviceID: Tnuint16; scope: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWSpoolCancelCapture(deviceID: Tnuint16; scope: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWSpoolGetBannerUserName(username: Pnstr8; mode: Tnuint16; scope: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWSpoolSetBannerUserName(username: Pnstr8; scope: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWGetPrinterStatus(conn: TNWCONN_HANDLE; printerNumber: Tnuint16; status: PPRINTER_STATUS): TNWCCODE; NWLIB_CALNLM32;
function NWStartQueueCapture(conn: TNWCONN_HANDLE; LPTDevice: Tnuint8; queueID: Tnuint32; queueName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWGetCaptureStatus(LPTDevice: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWFlushCapture(LPTDevice: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWEndCapture(LPTDevice: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWCancelCapture(LPTDevice: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWGetBannerUserName(userName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWSetBannerUserName(userName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWGetCaptureFlags(LPTDevice: Tnuint8; captureFlags1: PNWCAPTURE_FLAGS1; captureFlags2: PNWCAPTURE_FLAGS2): TNWCCODE; NWLIB_CALNLM32;
function NWGetCaptureFlagsConnRef(LPTDevice: Tnuint8; captureFlags1: PNWCAPTURE_FLAGS1; captureFlags3: PNWCAPTURE_FLAGS3): TNWCCODE; NWLIB_UNKNOWN;
function NWSetCaptureFlags(conn: TNWCONN_HANDLE; LPTDevice: Tnuint8; captureFlags1: PNWCAPTURE_FLAGS1): TNWCCODE; NWLIB_CALNLM32;
function NWGetPrinterStrings(LPTDevice: Tnuint8; setupStringLen: pnuint16; setupString: Pnstr8; resetStringLen: pnuint16; resetString: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWSetPrinterStrings(LPTDevice: Tnuint8; setupStringLen: Tnuint16; setupString: Pnstr8; resetStringLen: Tnuint16; resetString: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWGetMaxPrinters(numPrinters: pnuint16): TNWCCODE; NWLIB_CALNLM32;


//*****************************************************************************
//nwqms.h
//*****************************************************************************

const
  QF_AUTO_START = $08;
  QF_ENTRY_RESTART = $10;
  QF_ENTRY_OPEN = $20;
  QF_USER_HOLD = $40;
  QF_OPERATOR_HOLD = $80;
  QS_CANT_ADD_JOBS = $01;
  QS_SERVERS_CANT_ATTACH = $02;
  QS_CANT_SERVICE_JOBS = $04;
    {
    This struct is taken from NLM platform in the nwqueue.h file.  This
    structure is the format for a print queue only.  Other queue types
    might have different structures.  Used with the clientRecordArea field
    in some of the structures listed below.
     }
type

  PQueuePrintJobStruct = ^TQueuePrintJobStruct;
  TQueuePrintJobStruct = record
    versionNumber: Tnuint8;
    tabSize: Tnuint8;
    numberOfCopies: Tnuint16;
    printControlFlags: Tnuint16;
    maxLinesPerPage: Tnuint16;
    maxCharsPerLine: Tnuint16;
    formName: array[0..12] of Tnuint8;
    reserve: array[0..8] of Tnuint8;
    bannerNameField: array[0..12] of Tnuint8;
    bannerFileField: array[0..12] of Tnuint8;
    bannerFileName: array[0..13] of Tnuint8;
    directoryPath: array[0..79] of Tnuint8;
  end;

  PQueueJobStruct = ^TQueueJobStruct;
  TQueueJobStruct = record
    clientStation: Tnuint8;
    clientTask: Tnuint8;
    clientID: Tnuint32;
    targetServerID: Tnuint32;
    targetExecutionTime: array[0..5] of Tnuint8;
    jobEntryTime: array[0..5] of Tnuint8;
    jobNumber: Tnuint16;
    jobType: Tnuint16;
    jobPosition: Tnuint8;
    jobControlFlags: Tnuint8;
    jobFileName: array[0..13] of Tnuint8;
    jobFileHandle: array[0..5] of Tnuint8;
    servicingServerStation: Tnuint8;
    servicingServerTask: Tnuint8;
    servicingServerID: Tnuint32;
    jobDescription: array[0..49] of Tnuint8;
    clientRecordArea: array[0..151] of Tnuint8;
  end;

  PReplyJobStruct = ^TReplyJobStruct;
  TReplyJobStruct = record
    clientStation: Tnuint8;
    clientTask: Tnuint8;
    clientID: Tnuint32;
    targetServerID: Tnuint32;
    targetExecutionTime: array[0..5] of Tnuint8;
    jobEntryTime: array[0..5] of Tnuint8;
    jobNumber: Tnuint16;
    jobType: Tnuint16;
    jobPosition: Tnuint8;
    jobControlFlags: Tnuint8;
    jobFileName: array[0..13] of Tnuint8;
    jobFileHandle: array[0..5] of Tnuint8;
    servicingServerStation: Tnuint8;
    servicingServerTask: Tnuint8;
    servicingServerID: Tnuint32;
  end;

  PNWQueueJobStruct = ^TNWQueueJobStruct;
  TNWQueueJobStruct = record
    clientStation: Tnuint32;
    clientTask: Tnuint32;
    clientID: Tnuint32;
    targetServerID: Tnuint32;
    targetExecutionTime: array[0..5] of Tnuint8;
    jobEntryTime: array[0..5] of Tnuint8;
    jobNumber: Tnuint32;
    jobType: Tnuint16;
    jobPosition: Tnuint16;
    jobControlFlags: Tnuint16;
    jobFileName: array[0..13] of Tnuint8;
    jobFileHandle: Tnuint32;
    servicingServerStation: Tnuint32;
    servicingServerTask: Tnuint32;
    servicingServerID: Tnuint32;
    jobDescription: array[0..49] of Tnuint8;
    clientRecordArea: array[0..151] of Tnuint8;
  end;

  PNWReplyJobStruct = ^TNWReplyJobStruct;
  TNWReplyJobStruct = record
    clientStation: Tnuint32;
    clientTask: Tnuint32;
    clientID: Tnuint32;
    targetServerID: Tnuint32;
    targetExecutionTime: array[0..5] of Tnuint8;
    jobEntryTime: array[0..5] of Tnuint8;
    jobNumber: Tnuint32;
    jobType: Tnuint16;
    jobPosition: Tnuint16;
    jobControlFlags: Tnuint16;
    jobFileName: array[0..13] of Tnuint8;
    jobFileHandle: Tnuint32;
    servicingServerStation: Tnuint32;
    servicingServerTask: Tnuint32;
    servicingServerID: Tnuint32;
  end;
    { 250 to hold job #'s for old NCP }

  PQueueJobListReply = ^TQueueJobListReply;
  TQueueJobListReply = record
    totalQueueJobs: Tnuint32;
    replyQueueJobNumbers: Tnuint32;
    jobNumberList: array[0..249] of Tnuint32;
  end;

function NWCreateQueueFile(conn: TNWCONN_HANDLE; queueID: Tnuint32; job: PQueueJobStruct; fileHandle: PNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWCreateQueueFile2(conn: TNWCONN_HANDLE; queueID: Tnuint32; job: PNWQueueJobStruct; fileHandle: PNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWCloseFileAndStartQueueJob(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint16; fileHandle: TNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWCloseFileAndStartQueueJob2(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint32; fileHandle: TNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWCloseFileAndAbortQueueJob(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint16; fileHandle: TNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWCloseFileAndAbortQueueJob2(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint32; fileHandle: TNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWRemoveJobFromQueue(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWRemoveJobFromQueue2(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetQueueJobList(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobCount: pnuint16; jobList: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWGetQueueJobList2(conn: TNWCONN_HANDLE; queueID: Tnuint32; queueStartPos: Tnuint32; job: PQueueJobListReply): TNWCCODE; NWLIB_CALNLM32;
function NWReadQueueJobEntry(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint16; job: PQueueJobStruct): TNWCCODE; NWLIB_CALNLM32;
function NWReadQueueJobEntry2(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint32; job: PNWQueueJobStruct): TNWCCODE; NWLIB_CALNLM32;
function NWGetQueueJobFileSize(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint16; fileSize: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetQueueJobFileSize2(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint32; fileSize: pnuint32): TNWCCODE; NWLIB_CALNLM32;

function NWChangeQueueJobEntry(conn: TNWCONN_HANDLE; queueID: Tnuint32; job: PQueueJobStruct): TNWCCODE; NWLIB_CALNLM32;

function NWChangeQueueJobEntry2(conn: TNWCONN_HANDLE; queueID: Tnuint32; job: PNWQueueJobStruct): TNWCCODE; NWLIB_CALNLM32;
function NWChangeQueueJobPosition(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint16; newJobPos: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWChangeQueueJobPosition2(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint32; newJobPos: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWServiceQueueJob(conn: TNWCONN_HANDLE; queueID: Tnuint32; targetJobType: Tnuint16; job: PQueueJobStruct; fileHandle: PNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWServiceQueueJob2(conn: TNWCONN_HANDLE; queueID: Tnuint32; targetJobType: Tnuint16; job: PNWQueueJobStruct; fileHandle: PNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWAbortServicingQueueJob(conn: TNWCONN_HANDLE; QueueID: Tnuint32; JobNumber: Tnuint16; fileHandle: TNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWAbortServicingQueueJob2(conn: TNWCONN_HANDLE; QueueID: Tnuint32; JobNumber: Tnuint32; fileHandle: TNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWChangeToClientRights(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWChangeToClientRights2(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWFinishServicingQueueJob(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint16; fileHandle: TNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWFinishServicingQueueJob2(conn: TNWCONN_HANDLE; queueID: Tnuint32; jobNumber: Tnuint32; fileHandle: TNWFILE_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWGetPrinterQueueID(conn: TNWCONN_HANDLE; printerNum: Tnuint16; queueID: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWCreateQueue(conn: TNWCONN_HANDLE; queueName: Pnstr8; queueType: Tnuint16; dirPath: Tnuint8; path: Pnstr8;
  queueID: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWDestroyQueue(conn: TNWCONN_HANDLE; queueID: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWReadQueueCurrentStatus(conn: TNWCONN_HANDLE; queueID: Tnuint32; queueStatus: pnuint8; numberOfJobs: pnuint16; numberOfServers: pnuint16;
  serverIDlist: pnuint32; serverConnList: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWReadQueueCurrentStatus2(conn: TNWCONN_HANDLE; queueID: Tnuint32; queueStatus: pnuint32; numberOfJobs: pnuint32; numberOfServers: pnuint32;
  serverIDlist: pnuint32; serverConnList: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWSetQueueCurrentStatus(conn: TNWCONN_HANDLE; queueID: Tnuint32; queueStatus: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWSetQueueCurrentStatus2(conn: TNWCONN_HANDLE; queueID: Tnuint32; queueStatus: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWReadQueueServerCurrentStatus(conn: TNWCONN_HANDLE; queueID: Tnuint32; serverID: Tnuint32; serverConn: Tnuint16; statusRec: Tnptr): TNWCCODE; NWLIB_CALNLM32;
function NWReadQueueServerCurrentStatus2(conn: TNWCONN_HANDLE; queueID: Tnuint32; serverID: Tnuint32; serverConn: Tnuint32; statusRec: Tnptr): TNWCCODE; NWLIB_CALNLM32;
function NWAttachQueueServerToQueue(conn: TNWCONN_HANDLE; queueID: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWDetachQueueServerFromQueue(conn: TNWCONN_HANDLE; queueID: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWRestoreQueueServerRights(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;

function NWSetQueueServerCurrentStatus(conn: TNWCONN_HANDLE; queueID: Tnuint32; statusRec: pointer): TNWCCODE; NWLIB_CALNLM32;

//*****************************************************************************
//nwserver.h
//*****************************************************************************

const
  LNS_CHECK = 0;
  VERSION_CHECK = 1;
  NW_2X = 0;
  NW_30 = 1;
  NW_311 = 2;
  NW_32 = 3;
  NW_40 = 4;
type

  PVERSION_INFO = ^TVERSION_INFO;
  TVERSION_INFO = record
    serverName: array[0..47] of Tnuint8;
    fileServiceVersion: Tnuint8;
    fileServiceSubVersion: Tnuint8;
    maximumServiceConnections: Tnuint16;
    connectionsInUse: Tnuint16;
    maxNumberVolumes: Tnuint16;
    revision: Tnuint8;
    SFTLevel: Tnuint8;
    TTSLevel: Tnuint8;
    maxConnectionsEverUsed: Tnuint16;
    accountVersion: Tnuint8;
    VAPVersion: Tnuint8;
    queueVersion: Tnuint8;
    printVersion: Tnuint8;
    virtualConsoleVersion: Tnuint8;
    restrictionLevel: Tnuint8;
    internetBridge: Tnuint8;
    reserved: array[0..59] of Tnuint8;
  end;

  PNETWARE_PRODUCT_VERSION = ^TNETWARE_PRODUCT_VERSION;
  TNETWARE_PRODUCT_VERSION = record
    majorVersion: Tnuint16;
    minorVersion: Tnuint16;
    revision: Tnuint16;
  end;
    { Defines that are used for the NWCheckNetWareVersion call for values
       that can be returned in the compatibilityFlag byte.   }

const
  COMPATIBLE = $00;
  VERSION_NUMBER_TOO_LOW = $01;
  SFT_LEVEL_TOO_LOW = $02;
  TTS_LEVEL_TOO_LOW = $04;

function NWCheckConsolePrivileges(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWDownFileServer(conn: TNWCONN_HANDLE; forceFlag: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWGetFileServerDateAndTime(conn: TNWCONN_HANDLE; dateTimeBuffer: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWSetFileServerDateAndTime(conn: TNWCONN_HANDLE; year: Tnuint8; month: Tnuint8; day: Tnuint8; hour: Tnuint8;
  minute: Tnuint8; second: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWCheckNetWareVersion(conn: TNWCONN_HANDLE; minVer: Tnuint16; minSubVer: Tnuint16; minRev: Tnuint16; minSFT: Tnuint16;
  minTTS: Tnuint16; compatibilityFlag: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWGetFileServerVersionInfo(conn: TNWCONN_HANDLE; versBuffer: PVERSION_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetNetWareProductVersion(conn: TNWCONN_HANDLE; version: PNETWARE_PRODUCT_VERSION): TNWCCODE; NWLIB_CALNLM32;
function NWGetFileServerInformation(conn: TNWCONN_HANDLE; serverName: Pnstr8; majorVer: pnuint8; minVer: pnuint8; rev: pnuint8;
  maxConns: pnuint16; maxConnsUsed: pnuint16; connsInUse: pnuint16; numVolumes: pnuint16; SFTLevel: pnuint8;
  TTSLevel: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWGetFileServerExtendedInfo(conn: TNWCONN_HANDLE; accountingVer: pnuint8; VAPVer: pnuint8; queueingVer: pnuint8; printServerVer: pnuint8;
  virtualConsoleVer: pnuint8; securityVer: pnuint8; internetBridgeVer: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function _NWGetFileServerType(conn: TNWCONN_HANDLE; typeFlag: Tnuint16; serverType: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWAttachToFileServer(serverName: Pnstr8; scopeFlag: Tnuint16; newConnID: PNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWGetFileServerLoginStatus(conn: TNWCONN_HANDLE; loginEnabledFlag: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWLogoutFromFileServer(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWLoginToFileServer(conn: TNWCONN_HANDLE; objName: Pnstr8; objType: Tnuint16; password: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWEnableFileServerLogin(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWDisableFileServerLogin(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWGetFileServerDescription(conn: TNWCONN_HANDLE; companyName: Pnstr8; revision: Pnstr8; revisionDate: Pnstr8; copyrightNotice: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWAttachToFileServerByConn(conn: TNWCONN_HANDLE; serverName: Pnstr8; scopeFlag: Tnuint16; newConnID: PNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWGetNetworkSerialNumber(conn: TNWCONN_HANDLE; serialNum: pnuint32; appNum: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWIsManager(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;


//*****************************************************************************
//nwsync.h
//*****************************************************************************

type

  PLOGICAL_LOCK = ^TLOGICAL_LOCK;
  TLOGICAL_LOCK = record
    connNumber: Tnuint16;
    taskNumber: Tnuint16;
    lockStatus: Tnuint8;
  end;

  PLOGICAL_LOCKS = ^TLOGICAL_LOCKS;
  TLOGICAL_LOCKS = record
    useCount: Tnuint16;
    shareableLockCount: Tnuint16;
    locked: Tnuint8;
    nextRequest: Tnuint16;
    numRecords: Tnuint16;
    logicalLock: array[0..127] of TLOGICAL_LOCK;
    curRecord: Tnuint16;
  end;

  PCONN_LOGICAL_LOCK = ^TCONN_LOGICAL_LOCK;
  TCONN_LOGICAL_LOCK = record
    taskNumber: Tnuint16;
    lockStatus: Tnuint8;
    logicalName: array[0..127] of Tnstr8;
  end;

  PCONN_LOGICAL_LOCKS = ^TCONN_LOGICAL_LOCKS;
  TCONN_LOGICAL_LOCKS = record
    nextRequest: Tnuint16;
    numRecords: Tnuint16;
    records: array[0..507] of Tnuint8;
    curOffset: Tnuint16;
    curRecord: Tnuint16;
  end;

  PPHYSICAL_LOCK = ^TPHYSICAL_LOCK;
  TPHYSICAL_LOCK = record
    loggedCount: Tnuint16;
    shareableLockCount: Tnuint16;
    recordStart: Tnuint32;
    recordEnd: Tnuint32;
    connNumber: Tnuint16;
    taskNumber: Tnuint16;
    lockType: Tnuint8;
  end;

  PPHYSICAL_LOCKS = ^TPHYSICAL_LOCKS;
  TPHYSICAL_LOCKS = record
    nextRequest: Tnuint16;
    numRecords: Tnuint16;
    locks: array[0..31] of TPHYSICAL_LOCK;
    curRecord: Tnuint16;
    reserved: array[0..7] of Tnuint8;
  end;

  PCONN_PHYSICAL_LOCK = ^TCONN_PHYSICAL_LOCK;
  TCONN_PHYSICAL_LOCK = record
    taskNumber: Tnuint16;
    lockType: Tnuint8;
    recordStart: Tnuint32;
    recordEnd: Tnuint32;
  end;

  PCONN_PHYSICAL_LOCKS = ^TCONN_PHYSICAL_LOCKS;
  TCONN_PHYSICAL_LOCKS = record
    nextRequest: Tnuint16;
    numRecords: Tnuint16;
    locks: array[0..50] of TCONN_PHYSICAL_LOCK;
    curRecord: Tnuint16;
    reserved: array[0..21] of Tnuint8;
  end;

  PSEMAPHORE = ^TSEMAPHORE;
  TSEMAPHORE = record
    connNumber: Tnuint16;
    taskNumber: Tnuint16;
  end;

  PSEMAPHORES = ^TSEMAPHORES;
  TSEMAPHORES = record
    nextRequest: Tnuint16;
    openCount: Tnuint16;
    semaphoreValue: Tnuint16;
    semaphoreCount: Tnuint16;
    semaphores: array[0..169] of TSEMAPHORE;
    curRecord: Tnuint16;
  end;

  PCONN_SEMAPHORE = ^TCONN_SEMAPHORE;
  TCONN_SEMAPHORE = record
    openCount: Tnuint16;
    semaphoreValue: Tnuint16;
    taskNumber: Tnuint16;
    semaphoreName: array[0..127] of Tnstr8;
  end;

  PCONN_SEMAPHORES = ^TCONN_SEMAPHORES;
  TCONN_SEMAPHORES = record
    nextRequest: Tnuint16;
    numRecords: Tnuint16;
    records: array[0..507] of Tnuint8;
    curOffset: Tnuint16;
    curRecord: Tnuint16;
  end;

function NWScanPhysicalLocksByFile(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; dataStream: Tnuint8; iterHandle: pnint16;
  lock: PPHYSICAL_LOCK; locks: PPHYSICAL_LOCKS): TNWCCODE; NWLIB_CALNLM32;
function NWScanLogicalLocksByConn(conn: TNWCONN_HANDLE; connNum: Tnuint16; iterHandle: pnint16; logicalLock: PCONN_LOGICAL_LOCK; logicalLocks: PCONN_LOGICAL_LOCKS): TNWCCODE; NWLIB_CALNLM32;
function NWScanPhysicalLocksByConnFile(conn: TNWCONN_HANDLE; connNum: Tnuint16; dirHandle: TNWDIR_HANDLE; path: Pnstr8; dataStream: Tnuint8;
  iterHandle: pnint16; lock: PCONN_PHYSICAL_LOCK; locks: PCONN_PHYSICAL_LOCKS): TNWCCODE; NWLIB_CALNLM32;
function NWScanLogicalLocksByName(conn: TNWCONN_HANDLE; logicalName: Pnstr8; iterHandle: pnint16; logicalLock: PLOGICAL_LOCK; logicalLocks: PLOGICAL_LOCKS): TNWCCODE; NWLIB_CALNLM32;
function NWScanSemaphoresByConn(conn: TNWCONN_HANDLE; connNum: Tnuint16; iterHandle: pnint16; semaphore: PCONN_SEMAPHORE; semaphores: PCONN_SEMAPHORES): TNWCCODE; NWLIB_CALNLM32;
function NWScanSemaphoresByName(conn: TNWCONN_HANDLE; semName: Pnstr8; iterHandle: pnint16; semaphore: PSEMAPHORE; semaphores: PSEMAPHORES): TNWCCODE; NWLIB_CALNLM32;
function NWSignalSemaphore(conn: TNWCONN_HANDLE; semHandle: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWCloseSemaphore(conn: TNWCONN_HANDLE; semHandle: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWOpenSemaphore(conn: TNWCONN_HANDLE; semName: Pnstr8; initSemHandle: Tnint16; semHandle: pnuint32; semOpenCount: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWExamineSemaphore(conn: TNWCONN_HANDLE; semHandle: Tnuint32; semValue: pnint16; semOpenCount: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWWaitOnSemaphore(conn: TNWCONN_HANDLE; semHandle: Tnuint32; timeOutValue: Tnuint16): TNWCCODE; NWLIB_CALNLM32;



//*****************************************************************************
//nwtts.h
//*****************************************************************************

type

  PTTS_STATS = ^TTTS_STATS;
  TTTS_STATS = record
    systemElapsedTime: Tnuint32;
    TTS_Supported: Tnuint8;
    TTS_Enabled: Tnuint8;
    TTS_VolumeNumber: Tnuint16;
    TTS_MaxOpenTransactions: Tnuint16;
    TTS_MaxTransactionsOpened: Tnuint16;
    TTS_CurrTransactionsOpen: Tnuint16;
    TTS_TotalTransactions: Tnuint32;
    TTS_TotalWrites: Tnuint32;
    TTS_TotalBackouts: Tnuint32;
    TTS_UnfilledBackouts: Tnuint16;
    TTS_DiskBlocksInUse: Tnuint16;
    TTS_FATAllocations: Tnuint32;
    TTS_FileSizeChanges: Tnuint32;
    TTS_FilesTruncated: Tnuint32;
    numberOfTransactions: Tnuint8;
    connTask: array[0..234] of record
      connNumber: Tnuint8;
      taskNumber: Tnuint8;
    end;
  end;

function NWTTSAbortTransaction(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWTTSBeginTransaction(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWTTSIsAvailable(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWTTSGetControlFlags(conn: TNWCONN_HANDLE; controlFlags: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWTTSSetControlFlags(conn: TNWCONN_HANDLE; controlFlags: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWTTSEndTransaction(conn: TNWCONN_HANDLE; transactionNum: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWTTSTransactionStatus(conn: TNWCONN_HANDLE; transactionNum: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWTTSGetProcessThresholds(conn: TNWCONN_HANDLE; logicalLockLevel: pnuint8; physicalLockLevel: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWTTSSetProcessThresholds(conn: TNWCONN_HANDLE; logicalLockLevel: Tnuint8; physicalLockLevel: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWTTSGetConnectionThresholds(conn: TNWCONN_HANDLE; logicalLockLevel: pnuint8; physicalLockLevel: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWTTSSetConnectionThresholds(conn: TNWCONN_HANDLE; logicalLockLevel: Tnuint8; physicalLockLevel: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWEnableTTS(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWDisableTTS(conn: TNWCONN_HANDLE): TNWCCODE; NWLIB_CALNLM32;
function NWGetTTSStats(conn: TNWCONN_HANDLE; ttsStats: PTTS_STATS): TNWCCODE; NWLIB_CALNLM32;

//*****************************************************************************
//nwvol.h
//*****************************************************************************

    { define volume types   }

const
  VINetWare386 = 0;
  VINetWare286 = 1;
  VINetWare386v30 = 2;
  VINetWare386v31 = 3;
    {    define the extended volume information status flag bits   }
  NWSubAllocEnabledBit = $01;
  NWCompressionEnabledBit = $02;
  NWMigrationEnabledBit = $04;
  NWAuditingEnabledBit = $08;
  NWReadOnlyEnabledBit = $10;
  NWPSSEnabledBit = $80000000;
    { define the constant for volume request flag for NWScanMountedVolumeList  }
  NW_VOLUME_NUMBER_ONLY = 0;
  NW_VOLUME_NUMBER_AND_NAME = 1;
type

  PNWOBJ_REST = ^TNWOBJ_REST;
  TNWOBJ_REST = record
    objectID: Tnuint32;
    restriction: Tnuint32;
  end;

  PNWVolumeRestrictions = ^TNWVolumeRestrictions;
  TNWVolumeRestrictions = record
    numberOfEntries: Tnuint8;
    resInfo: array[0..11] of record
      objectID: Tnuint32;
      restriction: Tnuint32;
    end;
  end;

  PNWVOL_RESTRICTIONS = ^TNWVOL_RESTRICTIONS;
  TNWVOL_RESTRICTIONS = record
    numberOfEntries: Tnuint8;
    resInfo: array[0..15] of record
      objectID: Tnuint32;
      restriction: Tnuint32;
    end;
  end;

  PVOL_STATS = ^TVOL_STATS;
  TVOL_STATS = record
    systemElapsedTime: Tnint32;
    volumeNumber: Tnuint8;
    logicalDriveNumber: Tnuint8;
    sectorsPerBlock: Tnuint16;
    startingBlock: Tnuint16;
    totalBlocks: Tnuint16;
    availableBlocks: Tnuint16;
    totalDirectorySlots: Tnuint16;
    availableDirectorySlots: Tnuint16;
    maxDirectorySlotsUsed: Tnuint16;
    isHashing: Tnuint8;
    isCaching: Tnuint8;
    isRemovable: Tnuint8;
    isMounted: Tnuint8;
    volumeName: array[0..15] of Tnstr8;
  end;
    { non freeable  }

  PExtendedVolInfo_tag = ^TExtendedVolInfo_tag;
  TExtendedVolInfo_tag = record
    volType: Tnuint32;
    statusFlag: Tnuint32;
    sectorSize: Tnuint32;
    sectorsPerCluster: Tnuint32;
    volSizeInClusters: Tnuint32;
    freeClusters: Tnuint32;
    subAllocFreeableClusters: Tnuint32;
    freeableLimboSectors: Tnuint32;
    nonfreeableLimboSectors: Tnuint32;
    availSubAllocSectors: Tnuint32;
    nonuseableSubAllocSectors: Tnuint32;
    subAllocClusters: Tnuint32;
    numDataStreams: Tnuint32;
    numLimboDataStreams: Tnuint32;
    oldestDelFileAgeInTicks: Tnuint32;
    numCompressedDataStreams: Tnuint32;
    numCompressedLimboDataStreams: Tnuint32;
    numNoncompressibleDataStreams: Tnuint32;
    precompressedSectors: Tnuint32;
    compressedSectors: Tnuint32;
    numMigratedDataStreams: Tnuint32;
    migratedSectors: Tnuint32;
    clustersUsedByFAT: Tnuint32;
    clustersUsedByDirs: Tnuint32;
    clustersUsedByExtDirs: Tnuint32;
    totalDirEntries: Tnuint32;
    unusedDirEntries: Tnuint32;
    totalExtDirExtants: Tnuint32;
    unusedExtDirExtants: Tnuint32;
    extAttrsDefined: Tnuint32;
    extAttrExtantsUsed: Tnuint32;
    DirectoryServicesObjectID: Tnuint32;
    volLastModifiedDateAndTime: Tnuint32;
  end;
  TNWVolExtendedInfo = TExtendedVolInfo_tag;
  PNWVolExtendedInfo = ^TNWVolExtendedInfo;

  PNWVolMountNumWithName_tag = ^TNWVolMountNumWithName_tag;
  TNWVolMountNumWithName_tag = record
    volumeNumber: Tnuint32;
    volumeName: array[0..(NW_MAX_VOLUME_NAME_LEN) - 1] of Tnstr8;
  end;
  TNWVolMountNumWithName = TNWVolMountNumWithName_tag;
  PNWVolMountNumWithName = ^TNWVolMountNumWithName;

function NWGetDiskUtilization(conn: TNWCONN_HANDLE; objID: Tnuint32; volNum: Tnuint8; usedDirectories: pnuint16; usedFiles: pnuint16;
  usedBlocks: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWGetObjDiskRestrictions(conn: TNWCONN_HANDLE; volNumber: Tnuint8; objectID: Tnuint32; restriction: pnuint32; inUse: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWScanVolDiskRestrictions(conn: TNWCONN_HANDLE; volNum: Tnuint8; iterhandle: pnuint32; volInfo: PNWVolumeRestrictions): TNWCCODE; NWLIB_CALNLM32;
function NWScanVolDiskRestrictions2(conn: TNWCONN_HANDLE; volNum: Tnuint8; iterhandle: pnuint32; volInfo: PNWVOL_RESTRICTIONS): TNWCCODE; NWLIB_CALNLM32;
function NWRemoveObjectDiskRestrictions(conn: TNWCONN_HANDLE; volNum: Tnuint8; objID: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWSetObjectVolSpaceLimit(conn: TNWCONN_HANDLE; volNum: Tnuint16; objID: Tnuint32; restriction: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWGetVolumeInfoWithHandle(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; volName: Pnstr8; totalBlocks: pnuint16; sectorsPerBlock: pnuint16;
  availableBlocks: pnuint16; totalDirEntries: pnuint16; availableDirEntries: pnuint16; volIsRemovableFlag: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWGetVolumeInfoWithNumber(conn: TNWCONN_HANDLE; volNum: Tnuint16; volName: Pnstr8; totalBlocks: pnuint16; sectorsPerBlock: pnuint16;
  availableBlocks: pnuint16; totalDirEntries: pnuint16; availableDirEntries: pnuint16; volIsRemovableFlag: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWGetVolumeName(conn: TNWCONN_HANDLE; volNum: Tnuint16; volName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWGetVolumeNumber(conn: TNWCONN_HANDLE; volName: Pnstr8; volNum: pnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWGetVolumeStats(conn: TNWCONN_HANDLE; volNum: Tnuint8; volInfo: PVOL_STATS): TNWCCODE; NWLIB_CALNLM32;


function NWGetExtendedVolumeInfo(conn: TNWCONN_HANDLE; volNum: Tnuint16; volInfo: PNWVolExtendedInfo): TNWCCODE; NWLIB_CLIB;
function NWGetExtendedVolumeInfo2(conn: TNWCONN_HANDLE; volNum: Tnuint16; volInfo: PNWVolExtendedInfo): TNWCCODE; NWLIB_CLIB;
function NWScanMountedVolumeList(conn: TNWCONN_HANDLE; volRequestFlags: Tnuint32; nameSpace: Tnuint32; iterHandle: pnuint32; numberItems: Tnuint32;
  numberReturned: pnuint32; volMountArr: PNWVolMountNumWithName): TNWCCODE; NWLIB_CALNLM32;


//*****************************************************************************
//nwacct.h
//*****************************************************************************

type
  PHOLDS_INFO = ^THOLDS_INFO;
  THOLDS_INFO = record
    objectID: Tnuint32;
    amount: Tnint32;
  end;

  PHOLDS_STATUS = ^THOLDS_STATUS;
  THOLDS_STATUS = record
    holdsCount: Tnuint16;
    holds: array[0..15] of THOLDS_INFO;
  end;


function NWGetAccountStatus(conn: TNWCONN_HANDLE; objType: Tnuint16; objName: Pnstr8; balance: pnint32; limit: pnint32;
  holds: PHOLDS_STATUS): TNWCCODE; NWLIB_CALNLM32;
function NWQueryAccountingInstalled(conn: TNWCONN_HANDLE; installed: pnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWSubmitAccountCharge(conn: TNWCONN_HANDLE; objType: Tnuint16; objName: Pnstr8; serviceType: Tnuint16; chargeAmt: Tnint32;
  holdCancelAmt: Tnint32; noteType: Tnuint16; note: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWSubmitAccountHold(conn: TNWCONN_HANDLE; objType: Tnuint16; objName: Pnstr8; holdAmt: Tnint32): TNWCCODE; NWLIB_CALNLM32;
function NWSubmitAccountNote(conn: TNWCONN_HANDLE; objType: Tnuint16; objName: Pnstr8; serviceType: Tnuint16; noteType: Tnuint16;
  note: Pnstr8): TNWCCODE; NWLIB_CALNLM32;



//*****************************************************************************
//nwfse.h
//*****************************************************************************

type
  PSERVER_AND_VCONSOLE_INFO = ^TSERVER_AND_VCONSOLE_INFO;
  TSERVER_AND_VCONSOLE_INFO = record
    currentServerTime: Tnuint32;
    vconsoleVersion: Tnuint8;
    vconsoleRevision: Tnuint8;
  end;
    { Get Cache Information  }

  PCACHE_COUNTERS = ^TCACHE_COUNTERS;
  TCACHE_COUNTERS = record
    readExistingBlockCount: Tnuint32;
    readExistingWriteWaitCount: Tnuint32;
    readExistingPartialReadCount: Tnuint32;
    readExistingReadErrorCount: Tnuint32;
    writeBlockCount: Tnuint32;
    writeEntireBlockCount: Tnuint32;
    getDiskCount: Tnuint32;
    getDiskNeedToAllocCount: Tnuint32;
    getDiskSomeoneBeatMeCount: Tnuint32;
    getDiskPartialReadCount: Tnuint32;
    getDiskReadErrorCount: Tnuint32;
    getAsyncDiskCount: Tnuint32;
    getAsyncDiskNeedToAlloc: Tnuint32;
    getAsyncDiskSomeoneBeatMe: Tnuint32;
    errorDoingAsyncReadCount: Tnuint32;
    getDiskNoReadCount: Tnuint32;
    getDiskNoReadAllocCount: Tnuint32;
    getDiskNoReadSomeoneBeatMeCount: Tnuint32;
    diskWriteCount: Tnuint32;
    diskWriteAllocCount: Tnuint32;
    diskWriteSomeoneBeatMeCount: Tnuint32;
    writeErrorCount: Tnuint32;
    waitOnSemaphoreCount: Tnuint32;
    allocBlockWaitForSomeoneCount: Tnuint32;
    allocBlockCount: Tnuint32;
    allocBlockWaitCount: Tnuint32;
  end;

  PCACHE_MEM_COUNTERS = ^TCACHE_MEM_COUNTERS;
  TCACHE_MEM_COUNTERS = record
    originalNumOfCacheBuffers: Tnuint32;
    currentNumOfCacheBuffers: Tnuint32;
    cacheDirtyBlockThreshold: Tnuint32;
    waitNodeCount: Tnuint32;
    waitNodeAllocFailureCount: Tnuint32;
    moveCacheNodeCount: Tnuint32;
    moveCacheNodeFromAvailCount: Tnuint32;
    accelerateCacheNodeWriteCount: Tnuint32;
    removeCacheNodeCount: Tnuint32;
    removeCacheNodeFromAvailCount: Tnuint32;
  end;

  PCACHE_TREND_COUNTERS = ^TCACHE_TREND_COUNTERS;
  TCACHE_TREND_COUNTERS = record
    numCacheChecks: Tnuint32;
    numCacheHits: Tnuint32;
    numDirtyCacheChecks: Tnuint32;
    numDirtyCacheHits: Tnuint32;
    cacheUsedWhileChecking: Tnuint32;
    waitForDirtyBlocksDecreaseCount: Tnuint32;
    allocBlockFromAvailCount: Tnuint32;
    allocBlockFromLRUCount: Tnuint32;
    allocBlockAlreadyWaiting: Tnuint32;
    LRUSittingTime: Tnuint32;
  end;

  PCACHE_INFO = ^TCACHE_INFO;
  TCACHE_INFO = record
    maxByteCount: Tnuint32;
    minNumOfCacheBuffers: Tnuint32;
    minCacheReportThreshold: Tnuint32;
    allocWaitingCount: Tnuint32;
    numDirtyBlocks: Tnuint32;
    cacheDirtyWaitTime: Tnuint32;
    cacheMaxConcurrentWrites: Tnuint32;
    maxDirtyTime: Tnuint32;
    numOfDirCacheBuffers: Tnuint32;
    cacheByteToBlockShiftFactor: Tnuint32;
  end;

  PNWFSE_CACHE_INFO = ^TNWFSE_CACHE_INFO;
  TNWFSE_CACHE_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    cacheCounters: TCACHE_COUNTERS;
    cacheMemCounters: TCACHE_MEM_COUNTERS;
    cacheTrendCounters: TCACHE_TREND_COUNTERS;
    cacheInformation: TCACHE_INFO;
  end;
    { Get File Server Information  }
    { writeHeldOffWithDuplicateRequest  }

  PFSE_SERVER_INFO = ^TFSE_SERVER_INFO;
  TFSE_SERVER_INFO = record
    replyCanceledCount: Tnuint32;
    writeHeldOffCount: Tnuint32;
    writeHeldOffWithDupRequest: Tnuint32;
    invalidRequestTypeCount: Tnuint32;
    beingAbortedCount: Tnuint32;
    alreadyDoingReallocCount: Tnuint32;
    deAllocInvalidSlotCount: Tnuint32;
    deAllocBeingProcessedCount: Tnuint32;
    deAllocForgedPacketCount: Tnuint32;
    deAllocStillTransmittingCount: Tnuint32;
    startStationErrorCount: Tnuint32;
    invalidSlotCount: Tnuint32;
    beingProcessedCount: Tnuint32;
    forgedPacketCount: Tnuint32;
    stillTransmittingCount: Tnuint32;
    reExecuteRequestCount: Tnuint32;
    invalidSequenceNumCount: Tnuint32;
    duplicateIsBeingSentAlreadyCnt: Tnuint32;
    sentPositiveAcknowledgeCount: Tnuint32;
    sentDuplicateReplyCount: Tnuint32;
    noMemForStationCtrlCount: Tnuint32;
    noAvailableConnsCount: Tnuint32;
    reallocSlotCount: Tnuint32;
    reallocSlotCameTooSoonCount: Tnuint32;
  end;

  PFILE_SERVER_COUNTERS = ^TFILE_SERVER_COUNTERS;
  TFILE_SERVER_COUNTERS = record
    tooManyHops: Tnuint16;
    unknownNetwork: Tnuint16;
    noSpaceForService: Tnuint16;
    noReceiveBuffers: Tnuint16;
    notMyNetwork: Tnuint16;
    netBIOSProgatedCount: Tnuint32;
    totalPacketsServiced: Tnuint32;
    totalPacketsRouted: Tnuint32;
  end;

  PNWFSE_FILE_SERVER_INFO = ^TNWFSE_FILE_SERVER_INFO;
  TNWFSE_FILE_SERVER_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    NCPStationsInUseCount: Tnuint32;
    NCPPeakStationsInUseCount: Tnuint32;
    numOfNCPRequests: Tnuint32;
    serverUtilization: Tnuint32;
    ServerInfo: TFSE_SERVER_INFO;
    fileServerCounters: TFILE_SERVER_COUNTERS;
  end;
    { Netware File Systems Information  }

  PFSE_FILE_SYSTEM_INFO = ^TFSE_FILE_SYSTEM_INFO;
  TFSE_FILE_SYSTEM_INFO = record
    FATMovedCount: Tnuint32;
    FATWriteErrorCount: Tnuint32;
    someoneElseDidItCount0: Tnuint32;
    someoneElseDidItCount1: Tnuint32;
    someoneElseDidItCount2: Tnuint32;
    iRanOutSomeoneElseDidItCount0: Tnuint32;
    iRanOutSomeoneElseDidItCount1: Tnuint32;
    iRanOutSomeoneElseDidItCount2: Tnuint32;
    turboFATBuildScrewedUpCount: Tnuint32;
    extraUseCountNodeCount: Tnuint32;
    extraExtraUseCountNodeCount: Tnuint32;
    errorReadingLastFATCount: Tnuint32;
    someoneElseUsingThisFileCount: Tnuint32;
  end;

  PNWFSE_FILE_SYSTEM_INFO = ^TNWFSE_FILE_SYSTEM_INFO;
  TNWFSE_FILE_SYSTEM_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    fileSystemInfo: TFSE_FILE_SYSTEM_INFO;
  end;
    { User Information  }
    { status  }

const
  FSE_LOGGED_IN = $00000001;
  FSE_BEING_ABORTED = $00000002;
  FSE_AUDITED = $00000004;
  FSE_NEEDS_SECURITY_CHANGE = $00000008;
  FSE_MAC_STATION = $00000010;
  FSE_AUTHENTICATED_TEMPORARY = $00000020;
  FSE_AUDIT_CONNECTION_RECORDED = $00000040;
  FSE_DSAUDIT_CONNECTION_RECORDED = $00000080;
    { fileWriteFlags  }
  FSE_WRITE = 1;
  FSE_WRITE_ABORTED = 2;
    { fileWriteState  }
  FSE_NOT_WRITING = 0;
  FSE_WRITE_IN_PROGRESS = 1;
  FSE_WRITE_BEING_STOPPED = 2;
    { Includes active and stop bits  }
type

  PUSER_INFO = ^TUSER_INFO;
  TUSER_INFO = record
    connNum: Tnuint32;
    useCount: Tnuint32;
    connServiceType: Tnuint8;
    loginTime: array[0..6] of Tnuint8;
    status: Tnuint32;
    expirationTime: Tnuint32;
    objType: Tnuint32;
    transactionFlag: Tnuint8;
    logicalLockThreshold: Tnuint8;
    recordLockThreshold: Tnuint8;
    fileWriteFlags: Tnuint8;
    fileWriteState: Tnuint8;
    filler: Tnuint8;
    fileLockCount: Tnuint16;
    recordLockCount: Tnuint16;
    totalBytesRead: array[0..5] of Tnuint8;
    totalBytesWritten: array[0..5] of Tnuint8;
    totalRequests: Tnuint32;
    heldRequests: Tnuint32;
    heldBytesRead: array[0..5] of Tnuint8;
    heldBytesWritten: array[0..5] of Tnuint8;
  end;

  PNWFSE_USER_INFO = ^TNWFSE_USER_INFO;
  TNWFSE_USER_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    userInfo: TUSER_INFO;
  end;
    { Packet Burst Information  }
    { writeTooManyBuffersCheckedOutCount  }
    { writeDidntNeedButRequestedACKCount  }

  PPACKET_BURST_INFO = ^TPACKET_BURST_INFO;
  TPACKET_BURST_INFO = record
    bigInvalidSlotCount: Tnuint32;
    bigForgedPacketCount: Tnuint32;
    bigInvalidPacketCount: Tnuint32;
    bigStillTransmittingCount: Tnuint32;
    stillDoingTheLastRequestCount: Tnuint32;
    invalidCtrlRequestCount: Tnuint32;
    ctrlInvalidMessageNumCount: Tnuint32;
    ctrlBeingTornDownCount: Tnuint32;
    bigRepeatTheFileReadCount: Tnuint32;
    bigSendExtraCCCount: Tnuint32;
    bigReturnAbortMessageCount: Tnuint32;
    bigReadInvalidMessageNumCount: Tnuint32;
    bigReadDoItOverCount: Tnuint32;
    bigReadBeingTornDownCount: Tnuint32;
    previousCtrlPacketCount: Tnuint32;
    sendHoldOffMessageCount: Tnuint32;
    bigReadNoDataAvailableCount: Tnuint32;
    bigReadTryingToReadTooMuchCount: Tnuint32;
    asyncReadErrorCount: Tnuint32;
    bigReadPhysicalReadErrorCount: Tnuint32;
    ctrlBadACKFragmentListCount: Tnuint32;
    ctrlNoDataReadCount: Tnuint32;
    writeDuplicateRequestCount: Tnuint32;
    shouldntBeACKingHereCount: Tnuint32;
    writeInconsistentPktLengthsCnt: Tnuint32;
    firstPacketIsntAWriteCount: Tnuint32;
    writeTrashedDuplicateRequestCnt: Tnuint32;
    bigWriteInvalidMessageNumCount: Tnuint32;
    bigWriteBeingTornDownCount: Tnuint32;
    bigWriteBeingAbortedCount: Tnuint32;
    zeroACKFragmentCountCount: Tnuint32;
    writeCurrentlyTransmittingCount: Tnuint32;
    tryingToWriteTooMuchCount: Tnuint32;
    writeOutOfMemForCtrlNodesCount: Tnuint32;
    writeDidntNeedThisFragmentCount: Tnuint32;
    writeTooManyBuffsCheckedOutCnt: Tnuint32;
    writeTimeOutCount: Tnuint32;
    writeGotAnACKCount: Tnuint32;
    writeGotAnACKCount1: Tnuint32;
    pollerAbortedTheConnCount: Tnuint32;
    maybeHadOutOfOrderWritesCount: Tnuint32;
    hadAnOutOfOrderWriteCount: Tnuint32;
    movedTheACKBitDownCount: Tnuint32;
    bumpedOutOfOrderWriteCount: Tnuint32;
    pollerRemovedOldOutOfOrderCount: Tnuint32;
    writeDidntNeedButRequestACKCnt: Tnuint32;
    writeTrashedPacketCount: Tnuint32;
    tooManyACKFragmentsCount: Tnuint32;
    savedAnOutOfOrderPacketCount: Tnuint32;
    connBeingAbortedCount: Tnuint32;
  end;

  PNWFSE_PACKET_BURST_INFO = ^TNWFSE_PACKET_BURST_INFO;
  TNWFSE_PACKET_BURST_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    packetBurstInfo: TPACKET_BURST_INFO;
  end;
    { IPX SPX Information  }

  PIPX_INFO = ^TIPX_INFO;
  TIPX_INFO = record
    IPXSendPacketCount: Tnuint32;
    IPXMalformPacketCount: Tnuint16;
    IPXGetECBRequestCount: Tnuint32;
    IPXGetECBFailCount: Tnuint32;
    IPXAESEventCount: Tnuint32;
    IPXPostponedAESCount: Tnuint16;
    IPXMaxConfiguredSocketCount: Tnuint16;
    IPXMaxOpenSocketCount: Tnuint16;
    IPXOpenSocketFailCount: Tnuint16;
    IPXListenECBCount: Tnuint32;
    IPXECBCancelFailCount: Tnuint16;
    IPXGetLocalTargetFailCount: Tnuint16;
  end;

  PSPX_INFO = ^TSPX_INFO;
  TSPX_INFO = record
    SPXMaxConnsCount: Tnuint16;
    SPXMaxUsedConns: Tnuint16;
    SPXEstConnReq: Tnuint16;
    SPXEstConnFail: Tnuint16;
    SPXListenConnectReq: Tnuint16;
    SPXListenConnectFail: Tnuint16;
    SPXSendCount: Tnuint32;
    SPXWindowChokeCount: Tnuint32;
    SPXBadSendCount: Tnuint16;
    SPXSendFailCount: Tnuint16;
    SPXAbortedConn: Tnuint16;
    SPXListenPacketCount: Tnuint32;
    SPXBadListenCount: Tnuint16;
    SPXIncomingPacketCount: Tnuint32;
    SPXBadInPacketCount: Tnuint16;
    SPXSuppressedPackCount: Tnuint16;
    SPXNoSesListenECBCount: Tnuint16;
    SPXWatchDogDestSesCount: Tnuint16;
  end;

  PNWFSE_IPXSPX_INFO = ^TNWFSE_IPXSPX_INFO;
  TNWFSE_IPXSPX_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    IPXInfo: TIPX_INFO;
    SPXInfo: TSPX_INFO;
  end;
    { Garbage Collection Information  }

  PNWFSE_GARBAGE_COLLECTION_INFO = ^TNWFSE_GARBAGE_COLLECTION_INFO;
  TNWFSE_GARBAGE_COLLECTION_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    failedAllocRequestCount: Tnuint32;
    numOfAllocs: Tnuint32;
    noMoreMemAvailableCount: Tnuint32;
    numOfGarbageCollections: Tnuint32;
    garbageFoundSomeMem: Tnuint32;
    garbageNumOfChecks: Tnuint32;
  end;
    { CPU Information  }

const
  FSE_CPU_STR_MAX = 16;
  FSE_COPROCESSOR_STR_MAX = 48;
  FSE_BUS_STR_MAX = 32;
type

  PCPU_INFO = ^TCPU_INFO;
  TCPU_INFO = record
    pageTableOwnerFlag: Tnuint32;
    CPUTypeFlag: Tnuint32;
    coProcessorFlag: Tnuint32;
    busTypeFlag: Tnuint32;
    IOEngineFlag: Tnuint32;
    FSEngineFlag: Tnuint32;
    nonDedicatedFlag: Tnuint32;
  end;

  PNWFSE_CPU_INFO = ^TNWFSE_CPU_INFO;
  TNWFSE_CPU_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    numOfCPUs: Tnuint32;
    CPUInfo: TCPU_INFO;
  end;
    { Volume Switch Information  }
    { cardinal  mapPathToDirectoryNumberOrPhantom;  }
    { cardinal  stationHasAccessRightsGrantedBelow;  }
    { cardinal  getDataStreamLengthsFromPathStringBase;  }

  PVOLUME_SWITCH_INFO = ^TVOLUME_SWITCH_INFO;
  TVOLUME_SWITCH_INFO = record
    readFile: Tnuint32;
    writeFile: Tnuint32;
    deleteFile: Tnuint32;
    renMove: Tnuint32;
    openFile: Tnuint32;
    createFile: Tnuint32;
    createAndOpenFile: Tnuint32;
    closeFile: Tnuint32;
    scanDeleteFile: Tnuint32;
    salvageFile: Tnuint32;
    purgeFile: Tnuint32;
    migrateFile: Tnuint32;
    deMigrateFile: Tnuint32;
    createDir: Tnuint32;
    deleteDir: Tnuint32;
    directoryScans: Tnuint32;
    mapPathToDirNum: Tnuint32;
    modifyDirEntry: Tnuint32;
    getAccessRights: Tnuint32;
    getAccessRightsFromIDs: Tnuint32;
    mapDirNumToPath: Tnuint32;
    getEntryFromPathStrBase: Tnuint32;
    getOtherNSEntry: Tnuint32;
    getExtDirInfo: Tnuint32;
    getParentDirNum: Tnuint32;
    addTrusteeR: Tnuint32;
    scanTrusteeR: Tnuint32;
    delTrusteeR: Tnuint32;
    purgeTrust: Tnuint32;
    findNextTrustRef: Tnuint32;
    scanUserRestNodes: Tnuint32;
    addUserRest: Tnuint32;
    deleteUserRest: Tnuint32;
    rtnDirSpaceRest: Tnuint32;
    getActualAvailDskSp: Tnuint32;
    cntOwnedFilesAndDirs: Tnuint32;
    migFileInfo: Tnuint32;
    volMigInfo: Tnuint32;
    readMigFileData: Tnuint32;
    getVolUsageStats: Tnuint32;
    getActualVolUsageStats: Tnuint32;
    getDirUsageStats: Tnuint32;
    NMFileReadsCount: Tnuint32;
    NMFileWritesCount: Tnuint32;
    mapPathToDirNumOrPhantom: Tnuint32;
    stationHasAccessRgtsGntedBelow: Tnuint32;
    gtDataStreamLensFromPathStrBase: Tnuint32;
    checkAndGetDirectoryEntry: Tnuint32;
    getDeletedEntry: Tnuint32;
    getOriginalNameSpace: Tnuint32;
    getActualFileSize: Tnuint32;
    verifyNameSpaceNumber: Tnuint32;
    verifyDataStreamNumber: Tnuint32;
    checkVolumeNumber: Tnuint32;
    commitFile: Tnuint32;
    VMGetDirectoryEntry: Tnuint32;
    createDMFileEntry: Tnuint32;
    renameNameSpaceEntry: Tnuint32;
    logFile: Tnuint32;
    releaseFile: Tnuint32;
    clearFile: Tnuint32;
    setVolumeFlag: Tnuint32;
    clearVolumeFlag: Tnuint32;
    getOriginalInfo: Tnuint32;
    createMigratedDir: Tnuint32;
    F3OpenCreate: Tnuint32;
    F3InitFileSearch: Tnuint32;
    F3ContinueFileSearch: Tnuint32;
    F3RenameFile: Tnuint32;
    F3ScanForTrustees: Tnuint32;
    F3ObtainFileInfo: Tnuint32;
    F3ModifyInfo: Tnuint32;
    F3EraseFile: Tnuint32;
    F3SetDirHandle: Tnuint32;
    F3AddTrustees: Tnuint32;
    F3DeleteTrustees: Tnuint32;
    F3AllocDirHandle: Tnuint32;
    F3ScanSalvagedFiles: Tnuint32;
    F3RecoverSalvagedFiles: Tnuint32;
    F3PurgeSalvageableFile: Tnuint32;
    F3GetNSSpecificInfo: Tnuint32;
    F3ModifyNSSpecificInfo: Tnuint32;
    F3SearchSet: Tnuint32;
    F3GetDirBase: Tnuint32;
    F3QueryNameSpaceInfo: Tnuint32;
    F3GetNameSpaceList: Tnuint32;
    F3GetHugeInfo: Tnuint32;
    F3SetHugeInfo: Tnuint32;
    F3GetFullPathString: Tnuint32;
    F3GetEffectiveDirectoryRights: Tnuint32;
  end;
    { 512 / sizeof(cardinal)  }
    { VOLUME_SWITCH_INFO volumeSwitchInfo;  }{ Cant return all counters  }

  PNWFSE_VOLUME_SWITCH_INFO = ^TNWFSE_VOLUME_SWITCH_INFO;
  TNWFSE_VOLUME_SWITCH_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    totalLFSCounters: Tnuint32;
    CurrentLFSCounters: Tnuint32;
    LFSCounters: array[0..127] of Tnuint32;
  end;
    { Get NLM Loaded List  }

const
  FSE_NLM_NUMS_RETURNED_MAX = 128;
  FSE_NLM_NUMS_MAX = 130;
type

  PNWFSE_NLM_LOADED_LIST = ^TNWFSE_NLM_LOADED_LIST;
  TNWFSE_NLM_LOADED_LIST = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    numberNLMsLoaded: Tnuint32;
    NLMsInList: Tnuint32;
    NLMNums: array[0..(FSE_NLM_NUMS_RETURNED_MAX) - 1] of Tnuint32;
  end;

  PNWFSE_NLM_LOADED_LIST_LG = ^TNWFSE_NLM_LOADED_LIST_LG;
  TNWFSE_NLM_LOADED_LIST_LG = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    numberNLMsLoaded: Tnuint32;
    NLMsInList: Tnuint32;
    NLMNums: array[0..(FSE_NLM_NUMS_MAX) - 1] of Tnuint32;
  end;
    { NLM Information  }
    { 1 is added for the NULL  }

const
  FSE_NLM_FILENAME_LEN_MAX = 37;
  FSE_NLM_NAMELEN_MAX = 129;
  FSE_NLM_COPYRIGHTLEN_MAX = 256;
type

  PNLM_INFO = ^TNLM_INFO;
  TNLM_INFO = record
    identificationNum: Tnuint32;
    flags: Tnuint32;
    _type: Tnuint32;
    parentID: Tnuint32;
    majorVersion: Tnuint32;
    minorVersion: Tnuint32;
    revision: Tnuint32;
    year: Tnuint32;
    month: Tnuint32;
    day: Tnuint32;
    allocAvailableBytes: Tnuint32;
    allocFreeCount: Tnuint32;
    lastGarbageCollection: Tnuint32;
    messageLanguage: Tnuint32;
    numOfReferencedPublics: Tnuint32;
  end;

  PNWFSE_NLM_INFO = ^TNWFSE_NLM_INFO;
  TNWFSE_NLM_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    NLMInfo: TNLM_INFO;
  end;
    { Get Directory Cache Information  }

  PDIR_CACHE_INFO = ^TDIR_CACHE_INFO;
  TDIR_CACHE_INFO = record
    minTimeSinceFileDelete: Tnuint32;
    absMinTimeSinceFileDelete: Tnuint32;
    minNumOfDirCacheBuffers: Tnuint32;
    maxNumOfDirCacheBuffers: Tnuint32;
    numOfDirCacheBuffers: Tnuint32;
    dCMinNonReferencedTime: Tnuint32;
    dCWaitTimeBeforeNewBuffer: Tnuint32;
    dCMaxConcurrentWrites: Tnuint32;
    dCDirtyWaitTime: Tnuint32;
    dCDoubleReadFlag: Tnuint32;
    mapHashNodeCount: Tnuint32;
    spaceRestrictionNodeCount: Tnuint32;
    trusteeListNodeCount: Tnuint32;
    percentOfVolumeUsedByDirs: Tnuint32;
  end;

  PNWFSE_DIR_CACHE_INFO = ^TNWFSE_DIR_CACHE_INFO;
  TNWFSE_DIR_CACHE_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    dirCacheInfo: TDIR_CACHE_INFO;
  end;
    { Get Operating System Version Information  }

  PNWFSE_OS_VERSION_INFO = ^TNWFSE_OS_VERSION_INFO;
  TNWFSE_OS_VERSION_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    OSMajorVersion: Tnuint8;
    OSMinorVersion: Tnuint8;
    OSRevisionNum: Tnuint8;
    accountingVersion: Tnuint8;
    VAPVersion: Tnuint8;
    queueingVersion: Tnuint8;
    securityRestrictionsLevel: Tnuint8;
    bridgingSupport: Tnuint8;
    maxNumOfVolumes: Tnuint32;
    numOfConnSlots: Tnuint32;
    maxLoggedInConns: Tnuint32;
    maxNumOfNameSpaces: Tnuint32;
    maxNumOfLans: Tnuint32;
    maxNumOfMediaTypes: Tnuint32;
    maxNumOfProtocols: Tnuint32;
    maxMaxSubdirTreeDepth: Tnuint32;
    maxNumOfDataStreams: Tnuint32;
    maxNumOfSpoolPrinters: Tnuint32;
    serialNum: Tnuint32;
    applicationNum: Tnuint16;
  end;
    { Get Active Connection List by Type  }
    { Connection service type  }
    { NOTE: type 1 is reserved by CLIB for backward compatability  }

const
  FSE_NCP_CONNECTION_TYPE = 2;
  FSE_NLM_CONNECTION_TYPE = 3;
  FSE_AFP_CONNECTION_TYPE = 4;
  FSE_FTAM_CONNECTION_TYPE = 5;
  FSE_ANCP_CONNECTION_TYPE = 6;
  FSE_ACP_CONNECTION_TYPE = 7;
  FSE_SMB_CONNECTION_TYPE = 8;
  FSE_WINSOCK_CONNECTION_TYPE = 9;
  FSE_HTTP_CONNECTION_TYPE = 10;
  FSE_UDP_CONNECTION_TYPE = 11;
type

  PNWFSE_ACTIVE_CONN_LIST = ^TNWFSE_ACTIVE_CONN_LIST;
  TNWFSE_ACTIVE_CONN_LIST = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    activeConnBitList: array[0..511] of Tnuint8;
  end;
    { Get NLM's Resource Tag List  }
    { This packed structure consisting of:
       **
       ** cardinal number,
       ** cardinal signature,
       ** cardinal count,
       ** byte name[]  }

  PNWFSE_NLMS_RESOURCE_TAG_LIST = ^TNWFSE_NLMS_RESOURCE_TAG_LIST;
  TNWFSE_NLMS_RESOURCE_TAG_LIST = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    totalNumOfResourceTags: Tnuint32;
    packetResourceTags: Tnuint32;
    resourceTagBuf: array[0..511] of Tnuint8;
  end;
    { Active LAN Board List --- 20  }

const
  FSE_MAX_NUM_OF_LANS = 64;
type

  PNWFSE_ACTIVE_LAN_BOARD_LIST = ^TNWFSE_ACTIVE_LAN_BOARD_LIST;
  TNWFSE_ACTIVE_LAN_BOARD_LIST = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    MaxNumOfLANs: Tnuint32;
    LANLoadedCount: Tnuint32;
    boardNums: array[0..(FSE_MAX_NUM_OF_LANS) - 1] of Tnuint32;
  end;
    { LAN Configuration Information  }

  PLAN_CONFIG_INFO = ^TLAN_CONFIG_INFO;
  TLAN_CONFIG_INFO = record
    DriverCFG_MajorVersion: Tnuint8;
    DriverCFG_MinorVersion: Tnuint8;
    DriverNodeAddress: array[0..5] of Tnuint8;
    DriverModeFlags: Tnuint16;
    DriverBoardNum: Tnuint16;
    DriverBoardInstance: Tnuint16;
    DriverMaxSize: Tnuint32;
    DriverMaxRecvSize: Tnuint32;
    DriverRecvSize: Tnuint32;
    Reserved1: array[0..2] of Tnuint32;
    DriverCardID: Tnuint16;
    DriverMediaID: Tnuint16;
    DriverTransportTime: Tnuint16;
    DriverReserved: array[0..15] of Tnuint8;
    DriverMajorVersion: Tnuint8;
    DriverMinorVersion: Tnuint8;
    DriverFlags: Tnuint16;
    DriverSendRetries: Tnuint16;
    DriverLink: Tnuint32;
    DriverSharingFlags: Tnuint16;
    DriverSlot: Tnuint16;
    DriverIOPortsAndLengths: array[0..3] of Tnuint16;
    DriverMemDecode0: Tnuint32;
    DriverLength0: Tnuint16;
    DriverMemDecode1: Tnuint32;
    DriverLength1: Tnuint16;
    DriverInterrupt: array[0..1] of Tnuint8;
    DriverDMAUsage: array[0..1] of Tnuint8;
    Reserved2: array[0..2] of Tnuint32;
    DriverLogicalName: array[0..17] of Tnuint8;
    DriverLinearMem: array[0..1] of Tnuint32;
    DriverChannelNum: Tnuint16;
    DriverIOReserved: array[0..5] of Tnuint8;
  end;

  PNWFSE_LAN_CONFIG_INFO = ^TNWFSE_LAN_CONFIG_INFO;
  TNWFSE_LAN_CONFIG_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    LANConfigInfo: TLAN_CONFIG_INFO;
  end;
    { LAN Common Counters Information  }

  PLAN_COMMON_INFO = ^TLAN_COMMON_INFO;
  TLAN_COMMON_INFO = record
    notSupportedMask: Tnuint32;
    totalTxPacketCount: Tnuint32;
    totalRxPacketCount: Tnuint32;
    noECBAvailableCount: Tnuint32;
    packetTxTooBigCount: Tnuint32;
    packetTxTooSmallCount: Tnuint32;
    packetRxOverflowCount: Tnuint32;
    packetRxTooBigCount: Tnuint32;
    packetRxTooSmallCount: Tnuint32;
    packetTxMiscErrorCount: Tnuint32;
    packetRxMiscErrorCount: Tnuint32;
    retryTxCount: Tnuint32;
    checksumErrorCount: Tnuint32;
    hardwareRxMismatchCount: Tnuint32;
    reserved: array[0..49] of Tnuint32;
  end;

  PNWFSE_LAN_COMMON_COUNTERS_INFO = ^TNWFSE_LAN_COMMON_COUNTERS_INFO;
  TNWFSE_LAN_COMMON_COUNTERS_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    statisticsMajorVersion: Tnuint8;
    statisticsMinorVersion: Tnuint8;
    numberOfGenericCounters: Tnuint32;
    numberOfCounterBlocks: Tnuint32;
    customVariableCount: Tnuint32;
    NextCounterBlock: Tnuint32;
    LANCommonInfo: TLAN_COMMON_INFO;
  end;
    { LAN Custom Counters Information  }
    { (Tnint32, byte[])[] - byte[] is a length preceded
                                  ** non-null terminated string.  }

  PNWFSE_LAN_CUSTOM_INFO = ^TNWFSE_LAN_CUSTOM_INFO;
  TNWFSE_LAN_CUSTOM_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    numCustomVar: Tnuint32;
    customInfo: array[0..511] of Tnuint8;
  end;
    { LSL Information  }

  PLSL_INFO = ^TLSL_INFO;
  TLSL_INFO = record
    rxBufs: Tnuint32;
    rxBufs75PerCent: Tnuint32;
    rxBufsCheckedOut: Tnuint32;
    rxBufMaxSize: Tnuint32;
    maxPhysicalSize: Tnuint32;
    lastTimeRxBufAllocated: Tnuint32;
    maxNumsOfProtocols: Tnuint32;
    maxNumsOfMediaTypes: Tnuint32;
    totalTXPackets: Tnuint32;
    getECBBfrs: Tnuint32;
    getECBFails: Tnuint32;
    AESEventCounts: Tnuint32;
    postponedEvents: Tnuint32;
    ECBCxlFails: Tnuint32;
    validBfrsReused: Tnuint32;
    enqueuedSendCount: Tnuint32;
    totalRXPackets: Tnuint32;
    unclaimedPackets: Tnuint32;
    StatisticsTableMajorVersion: Tnuint8;
    StatisticsTableMinorVersion: Tnuint8;
  end;

  PNWFSE_LSL_INFO = ^TNWFSE_LSL_INFO;
  TNWFSE_LSL_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    LSLInfo: TLSL_INFO;
  end;
    { LSL Logical Board Statistics  }

  PNWFSE_LSL_LOGICAL_BOARD_STATS = ^TNWFSE_LSL_LOGICAL_BOARD_STATS;
  TNWFSE_LSL_LOGICAL_BOARD_STATS = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved0: Tnuint16;
    LogTtlTxPackets: Tnuint32;
    LogTtlRxPackets: Tnuint32;
    LogUnclaimedPackets: Tnuint32;
    reserved1: Tnuint32;
  end;
    { objtype  }

const
  FSE_ADAPTER_OBJECT = 0;
  FSE_CHANGER_OBJECT = 1;
  FSE_DEVICE_OBJECT = 2;
  FSE_MEDIA_OBJECT = 4;
  FSE_PARTITION_OBJECT = 5;
  FSE_SLOT_OBJECT = 6;
  FSE_HOTFIX_OBJECT = 7;
  FSE_MIRROR_OBJECT = 8;
  FSE_PARITY_OBJECT = 9;
  FSE_VOLUME_SEG_OBJECT = 10;
  FSE_VOLUME_OBJECT = 11;
  FSE_CLONE_OBJECT = 12;
  FSE_MAGAZINE_OBJECT = 14;
  FSE_VIRTUAL_DEVICE_OBJECT = 15;
  FSE_MAX_OBJECTS = 128;
  FSE_UNKNOWN_OBJECT = $FFFF;
  FSE_UNKNOWN_OBJECT_TYPE = $FFFF;
    { mediatype  }
  FSE_HARD_DISK = 0;
  FSE_CDROM_DISK = 1;
  FSE_WORM_DISK = 2;
  FSE_TAPE_DEVICE = 3;
  FSE_MAGNETO_OPTICAL = 4;
    { cartridgetype  }
  FSE_FIXED_MEDIA = $00000000;
  FSE_FLOPPY_5_25 = $00000001;
  FSE_FLOPPY_3_5 = $00000002;
  FSE_OPTICAL_5_25 = $00000003;
  FSE_OPTICAL_3_5 = $00000004;
  FSE_TAPE_0_5 = $00000005;
  FSE_TAPE_0_25 = $00000006;
  FSE_TAPE_8_MM = $00000007;
  FSE_TAPE_4_MM = $00000008;
  FSE_BERNOULLI_DISK = $00000009;
    { type  }
    { same as defined below for object types  }
    { status bits  }
  FSE_OBJECT_ACTIVATED = $00000001;
  FSE_OBJECT_CREATED = $00000002;
  FSE_OBJECT_SCRAMBLED = $00000004;
  FSE_OBJECT_RESERVED = $00000010;
  FSE_OBJECT_BEING_IDENTIFIED = $00000020;
  FSE_OBJECT_MAGAZINE_LOADED = $00000040;
  FSE_OBJECT_FAILURE = $00000080;
  FSE_OBJECT_REMOVABLE = $00000100;
  FSE_OBJECT_READ_ONLY = $00000200;
  FSE_OBJECT_IN_DEVICE = $00010000;
  FSE_OBJECT_ACCEPTS_MAGAZINES = $00020000;
  FSE_OBJECT_IS_IN_A_CHANGER = $00040000;
  FSE_OBJECT_LOADABLE = $00080000;
  FSE_OBJECT_BEING_LOADED = $00080000;
  FSE_OBJECT_DEVICE_LOCK = $01000000;
  FSE_OBJECT_CHANGER_LOCK = $02000000;
  FSE_OBJECT_REMIRRORING = $04000000;
  FSE_OBJECT_SELECTED = $08000000;
    { functionmask  }
  FSE_RANDOM_READ = $0001;
  FSE_RANDOM_WRITE = $0002;
  FSE_RANDOM_WRITE_ONCE = $0004;
  FSE_SEQUENTIAL_READ = $0008;
  FSE_SEQUENTIAL_WRITE = $0010;
  FSE_RESET_END_OF_TAPE = $0020;
  FSE_SINGLE_FILE_MARK = $0040;
  FSE_MULTIPLE_FILE_MARK = $0080;
  FSE_SINGLE_SET_MARK = $0100;
  FSE_MULTIPLE_SET_MARK = $0200;
  FSE_SPACE_DATA_BLOCKS = $0400;
  FSE_LOCATE_DATA_BLOCKS = $0800;
  FSE_POSITION_PARTITION = $1000;
  FSE_POSITION_MEDIA = $2000;
    { controlmask  }
  FSE_ACTIVATE_DEACTIVE = $0001;
  FSE_MOUNT_DISMOUNT = $0002;
  FSE_SELECT_UNSELECT = $0004;
  FSE_LOCK_UNLOCK = $0008;
  FSE_EJECT = $0010;
  FSE_MOVE = $0020;
type

  PMEDIA_INFO_DEF = ^TMEDIA_INFO_DEF;
  TMEDIA_INFO_DEF = record
    _label: array[0..63] of Tnuint8;
    identificationType: Tnuint32;
    identificationTimeStamp: Tnuint32;
  end;

  PFSE_MM_OBJ_INFO = ^TFSE_MM_OBJ_INFO;
  TFSE_MM_OBJ_INFO = record
    MediaInfo: TMEDIA_INFO_DEF;
    mediaType: Tnuint32;
    cartridgeType: Tnuint32;
    unitSize: Tnuint32;
    blockSize: Tnuint32;
    capacity: Tnuint32;
    preferredUnitSize: Tnuint32;
    name: array[0..63] of Tnuint8;
    _type: Tnuint32;
    status: Tnuint32;
    functionMask: Tnuint32;
    controlMask: Tnuint32;
    parentCount: Tnuint32;
    siblingCount: Tnuint32;
    childCount: Tnuint32;
    specificInfoSize: Tnuint32;
    objectUniqueID: Tnuint32;
    mediaSlot: Tnuint32;
  end;

  PNWFSE_MEDIA_MGR_OBJ_INFO = ^TNWFSE_MEDIA_MGR_OBJ_INFO;
  TNWFSE_MEDIA_MGR_OBJ_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    fseMMObjInfo: TFSE_MM_OBJ_INFO;
  end;
    { Get Media Manager Objects List
       Get Media Manager Object Children's List   }

  PNWFSE_MEDIA_MGR_OBJ_LIST = ^TNWFSE_MEDIA_MGR_OBJ_LIST;
  TNWFSE_MEDIA_MGR_OBJ_LIST = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    nextStartObjNum: Tnuint32;
    objCount: Tnuint32;
    objs: array[0..(FSE_MAX_OBJECTS) - 1] of Tnuint32;
  end;
    { Get Volume Segment List  }

const
  FSE_MAX_NUM_SEGS_RETURNED = 43;
type

  PVOLUME_SEGMENT = ^TVOLUME_SEGMENT;
  TVOLUME_SEGMENT = record
    volumeSegmentDeviceNum: Tnuint32;
    volumeSegmentOffset: Tnuint32;
    volumeSegmentSize: Tnuint32;
  end;
    { segment info follows  }
    { VOLUME_SEGMENT structures are packed  }

  PNWFSE_VOLUME_SEGMENT_LIST = ^TNWFSE_VOLUME_SEGMENT_LIST;
  TNWFSE_VOLUME_SEGMENT_LIST = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    numOfVolumeSegments: Tnuint32;
    volumeSegment: array[0..41] of TVOLUME_SEGMENT;
  end;
    { Volume Information by Level  }

  PVOLUME_INFO_BY_LEVEL_DEF = ^TVOLUME_INFO_BY_LEVEL_DEF;
  TVOLUME_INFO_BY_LEVEL_DEF = record
    volumeType: Tnuint32;
    statusFlagBits: Tnuint32;
    sectorSize: Tnuint32;
    sectorsPerCluster: Tnuint32;
    volumeSizeInClusters: Tnuint32;
    freedClusters: Tnuint32;
    subAllocFreeableClusters: Tnuint32;
    freeableLimboSectors: Tnuint32;
    nonFreeableLimboSectors: Tnuint32;
    nonFreeableAvailSubAllocSectors: Tnuint32;
    notUsableSubAllocSectors: Tnuint32;
    subAllocClusters: Tnuint32;
    dataStreamsCount: Tnuint32;
    limboDataStreamsCount: Tnuint32;
    oldestDeletedFileAgeInTicks: Tnuint32;
    compressedDataStreamsCount: Tnuint32;
    compressedLimboDataStreamsCount: Tnuint32;
    unCompressableDataStreamsCount: Tnuint32;
    preCompressedSectors: Tnuint32;
    compressedSectors: Tnuint32;
    migratedFiles: Tnuint32;
    migratedSectors: Tnuint32;
    clustersUsedByFAT: Tnuint32;
    clustersUsedByDirectories: Tnuint32;
    clustersUsedByExtendedDirs: Tnuint32;
    totalDirectoryEntries: Tnuint32;
    unUsedDirectoryEntries: Tnuint32;
    totalExtendedDirectoryExtants: Tnuint32;
    unUsedExtendedDirectoryExtants: Tnuint32;
    extendedAttributesDefined: Tnuint32;
    extendedAttributeExtantsUsed: Tnuint32;
    directoryServicesObjectID: Tnuint32;
    volumeLastModifiedDateAndTime: Tnuint32;
  end;

  PVOLUME_INFO_BY_LEVEL_DEF2 = ^TVOLUME_INFO_BY_LEVEL_DEF2;
  TVOLUME_INFO_BY_LEVEL_DEF2 = record
    volumeActiveCount: Tnuint32;
    volumeUseCount: Tnuint32;
    mACRootIDs: Tnuint32;
    volumeLastModifiedDateAndTime: Tnuint32;
    volumeReferenceCount: Tnuint32;
    compressionLowerLimit: Tnuint32;
    outstandingIOs: Tnuint32;
    outstandingCompressionIOs: Tnuint32;
    compressionIOsLimit: Tnuint32;
  end;

  PVOLUME_INFO_BY_LEVEL = ^TVOLUME_INFO_BY_LEVEL;
  TVOLUME_INFO_BY_LEVEL = record
    case longint of
      0: (volInfoDef: TVOLUME_INFO_BY_LEVEL_DEF);
      1: (volInfoDef2: TVOLUME_INFO_BY_LEVEL_DEF2);
  end;

  PNWFSE_VOLUME_INFO_BY_LEVEL = ^TNWFSE_VOLUME_INFO_BY_LEVEL;
  TNWFSE_VOLUME_INFO_BY_LEVEL = record
    serverAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    infoLevel: Tnuint32;
    volumeInfo: TVOLUME_INFO_BY_LEVEL;
  end;
    { Active Protocol Stacks  }

const
  FSE_MAX_NUM_OF_STACKINFO = 25;
type

  PSTACK_INFO = ^TSTACK_INFO;
  TSTACK_INFO = record
    StackNum: Tnuint32;
    StackShortName: array[0..15] of Tnuint8;
  end;

  PNWFSE_ACTIVE_STACKS = ^TNWFSE_ACTIVE_STACKS;
  TNWFSE_ACTIVE_STACKS = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    maxNumOfStacks: Tnuint32;
    stackCount: Tnuint32;
    nextStartNum: Tnuint32;
    stackInfo: array[0..(FSE_MAX_NUM_OF_STACKINFO) - 1] of TSTACK_INFO;
  end;
    { Get Protocol Stack Configuration Information  }

const
  FSE_STK_FULL_NAME_STR_LEN_MAX = 256;
type

  PNWFSE_PROTOCOL_STK_CONFIG_INFO = ^TNWFSE_PROTOCOL_STK_CONFIG_INFO;
  TNWFSE_PROTOCOL_STK_CONFIG_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    configMajorVersionNum: Tnuint8;
    configMinorVersionNum: Tnuint8;
    stackMajorVersionNum: Tnuint8;
    stackMinorVersionNum: Tnuint8;
    stackShortName: array[0..15] of Tnuint8;
  end;
    { Get Protocol Stack Statistics Information   }
    { always set to 3?  }

  PNWFSE_PROTOCOL_STK_STATS_INFO = ^TNWFSE_PROTOCOL_STK_STATS_INFO;
  TNWFSE_PROTOCOL_STK_STATS_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    statMajorVersionNum: Tnuint8;
    statMinorVersionNum: Tnuint8;
    commonCounters: Tnuint16;
    validCountersMask: Tnuint32;
    totalTxPackets: Tnuint32;
    totalRxPackets: Tnuint32;
    ignoredRxPackets: Tnuint32;
    numCustomCounters: Tnuint16;
  end;
    { Get Protocol Stack Custom Information  }
    { (Tnint32, byte[])[] - byte[] is a length preceded
                                   ** non-null terminated string.  }

  PNWFSE_PROTOCOL_CUSTOM_INFO = ^TNWFSE_PROTOCOL_CUSTOM_INFO;
  TNWFSE_PROTOCOL_CUSTOM_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved0: Tnuint16;
    customCount: Tnuint32;
    customStruct: array[0..511] of Tnuint8;
  end;

const
  FSE_STACK_IDS_MAX = 128;
  FSE_NO_FRAME_ID_MAC = 0;
  FSE_APPLE_LOCALTALK = 1;
  FSE_ETHERNETII_DEC = 2;
  FSE_ETHERNET_802_3_USING_802_2 = 3;
  FSE_TRING_802_5_USING_802_2 = 4;
  FSE_IPX_802_3 = 5;
  FSE_TOKEN_PASSING_BUS = 6;
  FSE_IBM_PC_NETWORK_II = 7;
  FSE_GATEWAY_GNET = 8;
  FSE_PROTEON_PRONET = 9;
  FSE_ENET_802_3_USING_802_2_SNAP = 10;
  FSE_TRING_802_5_USE_802_2_SNAP = 11;
  FSE_RACORE_FRAME = 12;
  FSE_ISDN_FRAME = 13;
  FSE_NOVELL_ARCNET = 14;
  FSE_IBM_PCN2_USING_802_2 = 15;
  FSE_IBM_PCN2_USING_802_2_SNAP = 16;
  FSE_CORVUS_FRAME = 17;
  FSE_HARRIS_ADACOM_FRAME = 18;
  FSE_IP_TUNNEL_FRAME = 19;
  FSE_FDDI_USING_802_2 = 20;
  FSE_COMMTEX_FRAME = 21;
  FSE_DATACO_FRAME = 22;
  FSE_FDDI_USING_802_2_SMAP = 23;
  FSE_SDLC_TUNNEL = 24;
  FSE_PC_OFFICE_FRAME = 25;
  FSE_HYPERCOMMUNICATIONS = 26;
  FSE_NOVELL_FRAME = 27;
type

  PNWFSE_PROTOCOL_ID_NUMS = ^TNWFSE_PROTOCOL_ID_NUMS;
  TNWFSE_PROTOCOL_ID_NUMS = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    stackIDCount: Tnuint32;
    stackIDs: array[0..(FSE_STACK_IDS_MAX) - 1] of Tnuint32;
  end;
    { Get Media Name by Media Number  }

const
  FSE_MEDIA_NAME_LEN_MAX = 81;
type

  PNWFSE_MEDIA_NAME_LIST = ^TNWFSE_MEDIA_NAME_LIST;
  TNWFSE_MEDIA_NAME_LIST = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
  end;
    { Get Loaded Media Number List  }

const
  FSE_MEDIA_LIST_MAX = 32;
type

  PNWFSE_LOADED_MEDIA_NUM_LIST = ^TNWFSE_LOADED_MEDIA_NUM_LIST;
  TNWFSE_LOADED_MEDIA_NUM_LIST = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    maxMediaTypes: Tnuint32;
    mediaListCount: Tnuint32;
    mediaList: array[0..(FSE_MEDIA_LIST_MAX) - 1] of Tnuint32;
  end;
    { Get General Router And SAP Information  }

  PNWFSE_GENERAL_ROUTER_SAP_INFO = ^TNWFSE_GENERAL_ROUTER_SAP_INFO;
  TNWFSE_GENERAL_ROUTER_SAP_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    internalRIPSocket: Tnuint32;
    internalRouterDownFlag: Tnuint32;
    trackOnFlag: Tnuint32;
    externalRouterActiveFlag: Tnuint32;
    internalSAPSocketNumber: Tnuint32;
    replyToNearestServerFlag: Tnuint32;
  end;
    { Get Network Router Information  }

  PNWFSE_NETWORK_ROUTER_INFO = ^TNWFSE_NETWORK_ROUTER_INFO;
  TNWFSE_NETWORK_ROUTER_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    NetIDNumber: Tnuint32;
    HopsToNet: Tnuint16;
    NetStatus: Tnuint16;
    TimeToNet: Tnuint16;
  end;
    { Get Network Routers Information  }

  PROUTERS_INFO = ^TROUTERS_INFO;
  TROUTERS_INFO = record
    nodeAddress: array[0..5] of Tnuint8;
    connectedLAN: Tnuint32;
    routeHops: Tnuint16;
    routeTime: Tnuint16;
  end;
    { 512 / sizeof( ROUTERS_INFO )  }

  PNWFSE_NETWORK_ROUTERS_INFO = ^TNWFSE_NETWORK_ROUTERS_INFO;
  TNWFSE_NETWORK_ROUTERS_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    NumberOfEntries: Tnuint32;
    routersInfo: array[0..35] of TROUTERS_INFO;
  end;
    { Get Known Networks Information  }

const
  FSE_LOCALBIT = $01;
  FSE_NETSTARBIT = $02;
  FSE_NETRELIABLEBIT = $04;
  FSE_NETWANBIT = $10;
type

  PKNOWN_NET_INFO = ^TKNOWN_NET_INFO;
  TKNOWN_NET_INFO = record
    netIDNumber: Tnuint32;
    hopsToNet: Tnuint16;
    netStatus: Tnuint16;
    timeToNet: Tnuint16;
  end;
    { 512 / sizeof( KNOWN_NET_INFO )  }

  PNWFSE_KNOWN_NETWORKS_INFO = ^TNWFSE_KNOWN_NETWORKS_INFO;
  TNWFSE_KNOWN_NETWORKS_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    numberOfEntries: Tnuint32;
    knownNetInfo: array[0..50] of TKNOWN_NET_INFO;
  end;
    { Get Server Information  }

  PNWFSE_SERVER_INFO = ^TNWFSE_SERVER_INFO;
  TNWFSE_SERVER_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    serverAddress: array[0..11] of Tnuint8;
    hopsToServer: Tnuint16;
  end;
    { Get Server Sources Information  }

  PSERVERS_SRC_INFO = ^TSERVERS_SRC_INFO;
  TSERVERS_SRC_INFO = record
    serverNode: array[0..5] of Tnuint8;
    connectedLAN: Tnuint32;
    sourceHops: Tnuint16;
  end;
    { 512 / sizeof( SERVERS_SRC_INFO )  }

  PNWFSE_SERVER_SRC_INFO = ^TNWFSE_SERVER_SRC_INFO;
  TNWFSE_SERVER_SRC_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    numberOfEntries: Tnuint32;
    serversSrcInfo: array[0..41] of TSERVERS_SRC_INFO;
  end;

  PNWFSE_KNOWN_SERVER_INFO = ^TNWFSE_KNOWN_SERVER_INFO;
  TNWFSE_KNOWN_SERVER_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    numberOfEntries: Tnuint32;
    data: array[0..511] of Tnuint8;
  end;

const
  FSE_TYPE_NUMBER = 0;
  FSE_TYPE_BOOLEAN = 1;
  FSE_TYPE_TICKS = 2;
    { 512 * number  }
  FSE_TYPE_BLOCK_SHIFT = 3;
    { [+|-]hh:mm:ss converted to seconds  }
  FSE_TYPE_TIME_OFFSET = 4;
  FSE_TYPE_STRING = 5;
    { The following show the types of triggers  }
  FSE_TYPE_TRIGGER = 6;
  FSE_TYPE_TRIGGER_OFF = $00;
  FSE_TYPE_TRIGGER_ON = $01;
  FSE_TYPE_TRIGGER_PENDING = $10;
  FSE_TYPE_TRIGGER_SUCCESS = $20;
  FSE_TYPE_TRIGGER_FAILED = $30;
    { setCmdFlags  }
  FSE_STARTUP_ONLY = $01;
  FSE_HIDE = $02;
  FSE_ADVANCED = $04;
  FSE_STARTUP_OR_LATER = $08;
    { Can't be performed on secured console }
  FSE_NOT_SECURED_CONSOLE = $10;
    { setCmdCategory     }
  FSE_COMMUNICATIONS = 0;
  FSE_MEMORY = 1;
  FSE_FILE_CACHE = 2;
  FSE_DIR_CACHE = 3;
  FSE_FILE_SYSTEM = 4;
  FSE_LOCKS = 5;
  FSE_TRANSACTION_TRACKING = 6;
  FSE_DISK = 7;
  FSE_TIME = 8;
  FSE_NCP = 9;
  FSE_MISCELLANEOUS = 10;
  FSE_ERRORS = 11;
  FSE_DIRECTORY_SERVICES = 12;
  FSE_MULTIPROCESSOR = 13;
  FSE_SERVICE_LOCATION_PROTOCOL = 14;
    {  The setNameAndValueInfo contains ASCIIZ strings in the following layout:
       **    byte setCmdName[ ];
       **    byte setCmdValue[ ];  }
type

  PNWFSE_SERVER_SET_CMDS_INFO = ^TNWFSE_SERVER_SET_CMDS_INFO;
  TNWFSE_SERVER_SET_CMDS_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    numberOfSetCommands: Tnuint32;
    nextSequenceNumber: Tnuint32;
    setCmdType: Tnuint32;
    setCmdCategory: Tnuint32;
    setCmdFlags: Tnuint32;
    setNameAndValueInfo: array[0..499] of Tnuint8;
  end;
    { Len preceded string which is not NULL terminated  }

  PNWFSE_SERVER_SET_CATEGORIES = ^TNWFSE_SERVER_SET_CATEGORIES;
  TNWFSE_SERVER_SET_CATEGORIES = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint16;
    numberOfSetCategories: Tnuint32;
    nextSequenceNumber: Tnuint32;
    categoryName: array[0..511] of Tnuint8;
  end;
    { MLID Board Info  }

const
  FSE_MAX_NUM_BOARD_INFO = 18;
type

  PMLID_BOARD_INFO = ^TMLID_BOARD_INFO;
  TMLID_BOARD_INFO = record
    protocolBoardNum: Tnuint32;
    protocolNumber: Tnuint16;
    protocolID: array[0..5] of Tnuint8;
    protocolName: array[0..15] of Tnuint8;
  end;

  PNWFSE_MLID_BOARD_INFO = ^TNWFSE_MLID_BOARD_INFO;
  TNWFSE_MLID_BOARD_INFO = record
    serverTimeAndVConsoleInfo: TSERVER_AND_VCONSOLE_INFO;
    reserved: Tnuint8;
    numberProtocols: Tnuint8;
    MLIDBoardInfo: array[0..(FSE_MAX_NUM_BOARD_INFO) - 1] of TMLID_BOARD_INFO;
  end;
    { Enumerate Network Addresses  }

  PNW_GUID = ^TNW_GUID;
  TNW_GUID = record
    GUID: array[0..15] of Tnuint8;
  end;

  PNWFSE_NETWORK_ADDRESS = ^TNWFSE_NETWORK_ADDRESS;
  TNWFSE_NETWORK_ADDRESS = record
    addressType: Tnuint32;
    addressSize: Tnuint32;
    address: pnuint8;
  end;
    { retInfoMask for NWEnumServerConnInfo  }

const
  CONN_INFO_TRANS_MASK = $00000001;
  CONN_INFO_LOGIN_TIME_MASK = $00000002;
  CONN_INFO_LOGIN_NAME_MASK = $00000004;
  CONN_INFO_LOCK_MASK = $00000008;
  CONN_INFO_PRINT_MASK = $00000010;
  CONN_INFO_STATS_MASK = $00000020;
  CONN_INFO_ACCT_MASK = $00000040;
  CONN_INFO_AUTH_MASK = $00000080;
  CONN_INFO_ALL_MASK = $FFFFFFFF;
    { some structs for NWEnumServerConnInfo  }
type

  PNWFSE_LOGIN_TIME = ^TNWFSE_LOGIN_TIME;
  TNWFSE_LOGIN_TIME = record
    loginTime: array[0..6] of Tnuint8;
    loginExpirationTime: Tnuint32;
  end;

  PNWFSE_LOGIN_NAME = ^TNWFSE_LOGIN_NAME;
  TNWFSE_LOGIN_NAME = record
    loginObjectType: Tnuint32;
    loginNameLen: Tnuint8;
    loginName: pnuint8;
  end;

  PNWFSE_LOCK_INFO = ^TNWFSE_LOCK_INFO;
  TNWFSE_LOCK_INFO = record
    logicalLockThreshold: Tnuint8;
    recordLockThreshold: Tnuint8;
    fileLockCount: Tnuint16;
    recordLockCount: Tnuint16;
  end;

  PNWFSE_PRINT_INFO = ^TNWFSE_PRINT_INFO;
  TNWFSE_PRINT_INFO = record
    printFlags: Tnuint8;
    tabSize: Tnuint8;
    numberCopies: Tnuint8;
    printToFileFlag: Tnuint8;
    bannerFileName: array[0..13] of Tnuint8;
    targetServerID: Tnuint8;
    formType: Tnuint8;
  end;

  PNWFSE_STATS_INFO = ^TNWFSE_STATS_INFO;
  TNWFSE_STATS_INFO = record
    totalBytesRead: array[0..5] of Tnuint8;
    totalBytesWritten: array[0..5] of Tnuint8;
    totalRequests: Tnuint32;
  end;

  PNWFSE_ACCT_INFO = ^TNWFSE_ACCT_INFO;
  TNWFSE_ACCT_INFO = record
    holdTime: Tnuint32;
    holdAmt: Tnuint32;
    chargeAmt: Tnuint32;
    heldConnectTimeInMinutes: Tnuint32;
    heldRequests: Tnuint32;
    heldBytesRead: array[0..5] of Tnuint8;
    heldBytesWritten: array[0..5] of Tnuint8;
  end;

  PNWFSE_AUTH_INFO = ^TNWFSE_AUTH_INFO;
  TNWFSE_AUTH_INFO = record
    loginStatus: Tnuint32;
    loginPrivileges: Tnuint32;
  end;

function NWGetCacheInfo(conn: TNWCONN_HANDLE; fseCacheInfo: PNWFSE_CACHE_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetFileServerInfo(conn: TNWCONN_HANDLE; fseFileServerInfo: PNWFSE_FILE_SERVER_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetNetWareFileSystemsInfo(conn: TNWCONN_HANDLE; fseFileSystemInfo: PNWFSE_FILE_SYSTEM_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetUserInfo(conn: TNWCONN_HANDLE; connNum: Tnuint32; userName: Pnstr8; fseUserInfo: PNWFSE_USER_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetPacketBurstInfo(conn: TNWCONN_HANDLE; fsePacketBurstInfo: PNWFSE_PACKET_BURST_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetIPXSPXInfo(conn: TNWCONN_HANDLE; fseIPXSPXInfo: PNWFSE_IPXSPX_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetGarbageCollectionInfo(conn: TNWCONN_HANDLE; fseGarbageCollectionInfo: PNWFSE_GARBAGE_COLLECTION_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetCPUInfo(conn: TNWCONN_HANDLE; CPUNum: Tnuint32; CPUName: Pnstr8; numCoprocessor: Pnstr8; bus: Pnstr8;
  fseCPUInfo: PNWFSE_CPU_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetVolumeSwitchInfo(conn: TNWCONN_HANDLE; startNum: Tnuint32; fseVolumeSwitchInfo: PNWFSE_VOLUME_SWITCH_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetNLMLoadedList(conn: TNWCONN_HANDLE; startNum: Tnuint32; fseNLMLoadedList: PNWFSE_NLM_LOADED_LIST): TNWCCODE; NWLIB_CALNLM32;
function NWGetNLMInfo(conn: TNWCONN_HANDLE; NLMNum: Tnuint32; fileName: Pnstr8; NLMname: Pnstr8; copyright: Pnstr8;
  fseNLMInfo: PNWFSE_NLM_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetDirCacheInfo(conn: TNWCONN_HANDLE; fseDirCacheInfo: PNWFSE_DIR_CACHE_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetOSVersionInfo(conn: TNWCONN_HANDLE; fseOSVersionInfo: PNWFSE_OS_VERSION_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetActiveConnListByType(conn: TNWCONN_HANDLE; startConnNum: Tnuint32; connType: Tnuint32; fseActiveConnListByType: PNWFSE_ACTIVE_CONN_LIST): TNWCCODE; NWLIB_CALNLM32;
function NWGetNLMsResourceTagList(conn: TNWCONN_HANDLE; NLMNum: Tnuint32; startNum: Tnuint32; fseNLMsResourceTagList: PNWFSE_NLMS_RESOURCE_TAG_LIST): TNWCCODE; NWLIB_CALNLM32;
function NWGetActiveLANBoardList(conn: TNWCONN_HANDLE; startNum: Tnuint32; fseActiveLANBoardList: PNWFSE_ACTIVE_LAN_BOARD_LIST): TNWCCODE; NWLIB_CALNLM32;
function NWGetLANConfigInfo(conn: TNWCONN_HANDLE; boardNum: Tnuint32; fseLANConfigInfo: PNWFSE_LAN_CONFIG_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetLANCommonCountersInfo(conn: TNWCONN_HANDLE; boardNum: Tnuint32; blockNum: Tnuint32; fseLANCommonCountersInfo: PNWFSE_LAN_COMMON_COUNTERS_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetLANCustomCountersInfo(conn: TNWCONN_HANDLE; boardNum: Tnuint32; startingNum: Tnuint32; fseLANCustomInfo: PNWFSE_LAN_CUSTOM_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetLSLInfo(conn: TNWCONN_HANDLE; fseLSLInfo: PNWFSE_LSL_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetLSLLogicalBoardStats(conn: TNWCONN_HANDLE; LANBoardNum: Tnuint32; fseLSLLogicalBoardStats: PNWFSE_LSL_LOGICAL_BOARD_STATS): TNWCCODE; NWLIB_CALNLM32;
function NWGetMediaMgrObjInfo(conn: TNWCONN_HANDLE; objNum: Tnuint32; fseMediaMgrObjInfo: PNWFSE_MEDIA_MGR_OBJ_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetMediaMgrObjList(conn: TNWCONN_HANDLE; startNum: Tnuint32; objType: Tnuint32; fseMediaMgrObjList: PNWFSE_MEDIA_MGR_OBJ_LIST): TNWCCODE; NWLIB_CALNLM32;
function NWGetMediaMgrObjChildrenList(conn: TNWCONN_HANDLE; startNum: Tnuint32; objType: Tnuint32; parentObjNum: Tnuint32; fseMediaMgrObjList: PNWFSE_MEDIA_MGR_OBJ_LIST): TNWCCODE; NWLIB_CALNLM32;
function NWGetVolumeSegmentList(conn: TNWCONN_HANDLE; volNum: Tnuint32; fseVolumeSegmentList: PNWFSE_VOLUME_SEGMENT_LIST): TNWCCODE; NWLIB_CALNLM32;
function NWGetVolumeInfoByLevel(conn: TNWCONN_HANDLE; volNum: Tnuint32; infoLevel: Tnuint32; fseVolumeInfo: PNWFSE_VOLUME_INFO_BY_LEVEL): TNWCCODE; NWLIB_CALNLM32;
function NWGetActiveProtocolStacks(conn: TNWCONN_HANDLE; startNum: Tnuint32; fseActiveStacks: PNWFSE_ACTIVE_STACKS): TNWCCODE; NWLIB_CALNLM32;
function NWGetProtocolStackConfigInfo(conn: TNWCONN_HANDLE; stackNum: Tnuint32; stackFullName: Pnstr8; fseProtocolStkConfigInfo: PNWFSE_PROTOCOL_STK_CONFIG_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetProtocolStackStatsInfo(conn: TNWCONN_HANDLE; stackNum: Tnuint32; fseProtocolStkStatsInfo: PNWFSE_PROTOCOL_STK_STATS_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetProtocolStackCustomInfo(conn: TNWCONN_HANDLE; stackNum: Tnuint32; customStartNum: Tnuint32; fseProtocolStackCustomInfo: PNWFSE_PROTOCOL_CUSTOM_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetProtocolStkNumsByMediaNum(conn: TNWCONN_HANDLE; mediaNum: Tnuint32; fseProtocolStkIDNums: PNWFSE_PROTOCOL_ID_NUMS): TNWCCODE; NWLIB_CALNLM32;
function NWGetProtocolStkNumsByLANBrdNum(conn: TNWCONN_HANDLE; LANBoardNum: Tnuint32; fseProtocolStkIDNums: PNWFSE_PROTOCOL_ID_NUMS): TNWCCODE; NWLIB_CALNLM32;
function NWGetMediaNameByMediaNum(conn: TNWCONN_HANDLE; mediaNum: Tnuint32; mediaName: Pnstr8; fseMediaNameList: PNWFSE_MEDIA_NAME_LIST): TNWCCODE; NWLIB_CALNLM32;
function NWGetLoadedMediaNumList(conn: TNWCONN_HANDLE; fseLoadedMediaNumList: PNWFSE_LOADED_MEDIA_NUM_LIST): TNWCCODE; NWLIB_CALNLM32;
function NWGetGeneralRouterAndSAPInfo(conn: TNWCONN_HANDLE; fseGeneralRouterSAPInfo: PNWFSE_GENERAL_ROUTER_SAP_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetNetworkRouterInfo(conn: TNWCONN_HANDLE; networkNum: Tnuint32; fseNetworkRouterInfo: PNWFSE_NETWORK_ROUTER_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetNetworkRoutersInfo(conn: TNWCONN_HANDLE; networkNum: Tnuint32; startNum: Tnuint32; fseNetworkRoutersInfo: PNWFSE_NETWORK_ROUTERS_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetKnownNetworksInfo(conn: TNWCONN_HANDLE; startNum: Tnuint32; fseKnownNetworksInfo: PNWFSE_KNOWN_NETWORKS_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetServerInfo(conn: TNWCONN_HANDLE; serverType: Tnuint32; serverName: Pnstr8; fseServerInfo: PNWFSE_SERVER_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetServerSourcesInfo(conn: TNWCONN_HANDLE; startNum: Tnuint32; serverType: Tnuint32; serverName: Pnstr8; fseServerSrcInfo: PNWFSE_SERVER_SRC_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetKnownServersInfo(conn: TNWCONN_HANDLE; startNum: Tnuint32; serverType: Tnuint32; fseKnownServerInfo: PNWFSE_KNOWN_SERVER_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetServerSetCommandsInfo(conn: TNWCONN_HANDLE; startNum: Tnuint32; fseServerSetCmdsInfo: PNWFSE_SERVER_SET_CMDS_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWGetServerSetCategories(conn: TNWCONN_HANDLE; startNum: Tnuint32; fseServerSetCategories: PNWFSE_SERVER_SET_CATEGORIES): TNWCCODE; NWLIB_CALNLM32;
function NWGetMLIDBoardInfo(conn: TNWCONN_HANDLE; MLIDBoardNum: Tnuint32; fseMLIDBoardInfo: PNWFSE_MLID_BOARD_INFO): TNWCCODE; NWLIB_CALNLM32;
function NWEnumNetAddresses(conn: TNWCONN_HANDLE; searchNumber: pnuint32; serverTimeAndVConsoleInfo: PSERVER_AND_VCONSOLE_INFO; reserved: pnuint16; fseServerGUID: PNW_GUID;
  itemsInArray: Tnuint32; itemsReturned: pnuint32; fseNetworkAddresses: PNWFSE_NETWORK_ADDRESS): TNWCCODE; NWLIB_CALNLM32;
function NWGenerateGUIDs(connHandle: TNWCONN_HANDLE; GUIDSize: Tnuint32; GUIDList: PNW_GUID): TNWCCODE; NWLIB_CALNLM32;
function NWGetServerConnInfo(conn: TNWCONN_HANDLE; retInfoMask: Tnuint32; connectionNumber: Tnuint32; serverTimeAndVConsoleInfo: PSERVER_AND_VCONSOLE_INFO; reserved: pnuint16;
  networkAddress: PNWFSE_NETWORK_ADDRESS; loginTime: PNWFSE_LOGIN_TIME; loginName: PNWFSE_LOGIN_NAME; lockInfo: PNWFSE_LOCK_INFO; printInfo: PNWFSE_PRINT_INFO;
  statsInfo: PNWFSE_STATS_INFO; acctInfo: PNWFSE_ACCT_INFO; authInfo: PNWFSE_AUTH_INFO): TNWCCODE; NWLIB_CALNLM32;



//*****************************************************************************
//nwmigrat.h
//*****************************************************************************

const
  MAX_NUM_OF_DATA_STREAMS = 3;
  MAX_SIZE_OF_SM_STRING = 128;
  MAX_SIZE_OF_SM_INFO = 128;
  MAX_NUM_OF_SM = 32;
  ERR_INVALID_SM_ID = 240;
  ERR_SM_ALREADY_REGISTERED = 241;
  ERR_SM_CREATE_FAILED = 242;
  ERR_SM_CLOSE_FAILED = 243;
  ERR_SM_WRITE_NO_SPACE = 244;
  ERR_SM_WRITE_IO_ERROR = 245;
  ERR_SM_READ_IO_ERROR = 246;
  ERR_SM_OPEN_FAILED = 247;

  ERR_SM_DELETE_FAILED = 248;
    { A length preceded string is followed by SMInfo data  }
type
  PSUPPORT_MODULE_INFO = ^TSUPPORT_MODULE_INFO;
  TSUPPORT_MODULE_INFO = record
    IOStatus: Tnuint32;
    InfoBlockSize: Tnuint32;
    AvailSpace: Tnuint32;
    UsedSpace: Tnuint32;
    SMInfo: array[0..(MAX_SIZE_OF_SM_STRING + MAX_SIZE_OF_SM_INFO) - 1] of Tnuint8;
  end;

  PSUPPORT_MODULE_IDS = ^TSUPPORT_MODULE_IDS;
  TSUPPORT_MODULE_IDS = record
    numberOfSMs: Tnuint32;
    SMIDs: array[0..(MAX_NUM_OF_SM) - 1] of Tnuint32;
  end;


{    const
       NWMoveFileToDM = NWMoveFileToDM2;
       NWMoveFileFromDM = NWMoveFileFromDM2;
       NWGetDMFileInfo = NWGetDMFileInfo2;
       NWGetDMVolumeInfo = NWGetDMVolumeInfo2;
       NWGetDefaultSupportModule = NWGetDefaultSupportModule2;
       NWSetDefaultSupportModule = NWSetDefaultSupportModule2;
       NWGetDataMigratorInfo = NWGetDataMigratorInfo2;
       NWGetSupportModuleInfo = NWGetSupportModuleInfo2;}



function NWMoveFileToDM(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; nameSpace: Tnuint8; supportModuleID: Tnuint32;
  saveKeyFlag: Tnuint32): TNWCCODE; NWLIB_CLIB;
function NWMoveFileFromDM(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; nameSpace: Tnuint8): TNWCCODE; NWLIB_CLIB;
function NWGetDMFileInfo(conn: TNWCONN_HANDLE; dirHandle: TNWDIR_HANDLE; path: Pnstr8; nameSpace: Tnuint8; supportModuleID: pnuint32;
  restoreTime: pnuint32; dataStreams: pnuint32): TNWCCODE; NWLIB_CLIB;
function NWGetDMVolumeInfo(conn: TNWCONN_HANDLE; volume: Tnuint16; supportModuleID: Tnuint32; numberOfFilesMigrated: pnuint32; totalMigratedSize: pnuint32;
  spaceUsedOnDM: pnuint32; limboSpaceUsedOnDM: pnuint32; spaceMigrated: pnuint32; filesInLimbo: pnuint32): TNWCCODE; NWLIB_CLIB;
function NWGetSupportModuleInfo(conn: TNWCONN_HANDLE; infomationLevel: Tnuint32; supportModuleID: Tnuint32; returnInfo: pnuint8; returnInfoLen: pnuint32): TNWCCODE; NWLIB_CLIB;
function NWGetDataMigratorInfo(conn: TNWCONN_HANDLE; DMPresentFlag: pnuint32; majorVersion: pnuint32; minorVersion: pnuint32; DMSMRegistered: pnuint32): TNWCCODE; NWLIB_CLIB;
function NWGetDefaultSupportModule(conn: TNWCONN_HANDLE; supportModuleID: pnuint32): TNWCCODE; NWLIB_CLIB;
function NWSetDefaultSupportModule(conn: TNWCONN_HANDLE; supportModuleID: pnuint32): TNWCCODE; NWLIB_CLIB;
function NWGetSupportModuleCapacity(conn: TNWCONN_HANDLE; luSupportModuleID: Tnuint32; luVolume: Tnuint32; luDirectoryBase: Tnuint32; pluSMBlockSizeInSectors: pnuint32;
  pluSMTotalBlocks: pnuint32; pluSMUsedBlocks: pnuint32): TNWCCODE; NWLIB_CALNLM32;



//*****************************************************************************
//nwsm.h
//*****************************************************************************

const
  LOAD_COULD_NOT_FIND_FILE = 1;
  LOAD_ERROR_READING_FILE = 2;
  LOAD_NOT_NLM_FILE_FORMAT = 3;
  LOAD_WRONG_NLM_FILE_VERSION = 4;
  LOAD_REENTRANT_INITIALIZE_FAILURE = 5;
  LOAD_CAN_NOT_LOAD_MULTIPLE_COPIES = 6;
  LOAD_ALREADY_IN_PROGRESS = 7;
  LOAD_NOT_ENOUGH_MEMORY = 8;
  LOAD_INITIALIZE_FAILURE = 9;
  LOAD_INCONSISTENT_FILE_FORMAT = 10;
  LOAD_CAN_NOT_LOAD_AT_STARTUP = 11;
  LOAD_AUTO_LOAD_MODULES_NOT_LOADED = 12;
  LOAD_UNRESOLVED_EXTERNAL = 13;
  LOAD_PUBLIC_ALREADY_DEFINED = 14;
  LOAD_XDC_DATA_ERROR = 15;
  LOAD_NOT_OS_DOMAIN = 16;

function NWSMLoadNLM(connHandle: TNWCONN_HANDLE; loadCommand: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWSMLoadNLM2(connHandle: TNWCONN_HANDLE; loadCommand: Pnstr8; loadNLMReturnCode: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWSMUnloadNLM(connHandle: TNWCONN_HANDLE; NLMName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWSMMountVolume(connHandle: TNWCONN_HANDLE; volumeName: Pnstr8; volumeNumber: pnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWSMDismountVolumeByNumber(connHandle: TNWCONN_HANDLE; volumeNumber: Tnuint16): TNWCCODE; NWLIB_CALNLM32;
function NWSMDismountVolumeByName(connHandle: TNWCONN_HANDLE; volumeName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWSMAddNSToVolume(connHandle: TNWCONN_HANDLE; volNumber: Tnuint16; namspc: Tnuint8): TNWCCODE; NWLIB_CALNLM32;
function NWSMSetDynamicCmdStrValue(connHandle: TNWCONN_HANDLE; setCommandName: Pnstr8; cmdValue: Pnstr8): TNWCCODE; NWLIB_CALNLM32;
function NWSMSetDynamicCmdIntValue(connHandle: TNWCONN_HANDLE; setCommandName: Pnstr8; cmdValue: Tnuint32): TNWCCODE; NWLIB_CALNLM32;
function NWSMExecuteNCFFile(connHandle: TNWCONN_HANDLE; NCFFileName: Pnstr8): TNWCCODE; NWLIB_CALNLM32;

// Obsolete API's
//****************************************
//o_ndscon
//****************************************
{ replacement - NWCCGetConnInfo }
function NWDSGetConnectionInfo
  (connHandle: TNWCONN_HANDLE;
  connStatus: pnuint8;
  connType: pnuint8;
  serverFlags: pnuint8;
  serverName: pchar;
  transType: pnuint8;
  transLen: pnuint32;
  transBuf: pointer;
  distance: pnuint16;
  maxPacketSize: pnuint16): TNWCCODE; NWLIB_DSAPI;
{ replacement - NWDSOpenMonitoredConn }
function NWDSGetMonitoredConnection
  (connHandle: PNWCONN_HANDLE): TNWCCODE; NWLIB_DSAPI;
{ replacement - NWGetPreferredConnName & NWCCOpenConnByName }
function NWGetPreferredDSServer(connHandle: PNWCONN_HANDLE): TNWCCODE; NWLIB_DSAPI;
{ replacement - NWCCLicenseConn }
function NWDSLockConnection(connHandle: TNWCONN_HANDLE): TNWCCODE; NWLIB_DSAPI;
{ replacement - NWCCScanConnRefs }
function NWGetNextConnectionID(connHandle: PNWCONN_HANDLE): TNWCCODE; NWLIB_DSAPI;
{ replacement - NWCCOpenConnByAddr followed by NWCCLicenseConn }
function NWDSGetConnectionSlot
  (connType: Tnuint8;
  transType: Tnuint8;
  transLen: Tnuint32;
  transBuf: pointer;
  connHandle: PNWCONN_HANDLE): TNWCCODE; NWLIB_DSAPI;
{ replacement - NWCCScanConnInfo }
function NWGetNearestDirectoryService(connHandle: PNWCONN_HANDLE): TNWCCODE; NWLIB_DSAPI;
{ replacement - NWCCScanConnInfo, NWCCOpenConnByRef, NWCCLicenseConn }
function NWGetConnectionIDFromAddress
  (transType: Tnuint8;
  transLen: Tnuint32;
  transBuf: pointer;
  connHandle: PNWCONN_HANDLE): TNWCCODE; NWLIB_DSAPI;
{ replacement - NWCCScanConnInfo, NWCCOpenConnByRef, NWCCLicenseConn }
function NWGetConnectionIDFromName
  (nameLen: Tnuint32;
  name: pchar;
  connHandle: PNWCONN_HANDLE): TNWCCODE; NWLIB_DSAPI;
{ replacement - NWCCScanConnInfo, NWCCOpenConnByRef }
function NWGetNearestDSConnRef(connRef: pnuint32): TNWCCODE; NWLIB_DSAPI;
{ replacement - NWDSSetDefNameContext }
function NWSetDefaultNameContext
  (contextLength: Tnuint16;
  context: pnuint8): TNWCCODE; NWLIB_DSAPI;

{ replacement - NWDSGetDefNameContext }
function NWGetDefaultNameContext
  (bufferSize: Tnuint16;
  context: pnuint8): TNWCCODE; NWLIB_DSAPI;

{ replacement - NWCCGetNumConns }
function NWGetNumConnections(numConnections: pnuint16): TNWCCODE; NWLIB_DSAPI;

{ replacement - NWDSCanDSAuthenticate }
function NWIsDSAuthenticated: TNWCCODE; NWLIB_DSAPI;

{ replacement - NWCCUnlicenseConn }
function NWDSUnlockConnection(connHandle: TNWCONN_HANDLE): TNWCCODE; NWLIB_DSAPI;

{ replacement - NWCCGetPrefServerName }
function NWGetPreferredConnName(preferredName: pnuint8; preferredType: pnuint8): TNWCCODE; NWLIB_DSAPI;

{ replacment - NWCSysCloseConnRef }
function NWFreeConnectionSlot(connHandle: TNWCONN_HANDLE; disconnectType: Tnuint8): TNWCCODE; NWLIB_DSAPI;

{ replacement - NONE (monitored connections are managed automatically
    * by the client software) }
function NWDSSetMonitoredConnection(connHandle: TNWCONN_HANDLE): TNWCCODE; NWLIB_DSAPI;



type
  PNMBYTE = ^TNMBYTE;
  PNMUNI = ^TNMUNI;
  PSCBYTE = ^TSCBYTE;
  PSCUNI = ^TSCUNI;


implementation
(*
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    function nwunisize(x : longint) : longint;
    begin
       //nwunisize:=(sizeof(x)) / (sizeof(unicode));
    end;

    { was #define dname def_expr }
    function NWU_UNCHANGED_FUNCTION : pointer;
      begin
         //NWU_UNCHANGED_FUNCTION:=pointer(-(1));
      end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    function NWLocalToUnicode(P1,P2,P3,P4,P5,P6 : longint) : longint;
    begin
       //NWLocalToUnicode:=NWLocalToUnicode(P1,P2,P3,P4,P5,P6,1);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    function NWUnicodeToLocal(P1,P2,P3,P4,P5,P6 : longint) : longint;
    begin
       //NWUnicodeToLocal:=NWUnicodeToLocal(P1,P2,P3,P4,P5,P6,1);
    end;

    {function NWInitUnicodeTables(CountryCode,CodePage : longint) : longint;
    begin
      NWInitUnicodeTables:=NWLInitXlateTables(CodePage,N_UNI_LOAD_MONOCASE or N_UNI_LOAD_COLLATION);
    end;}

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    function uniicmp(s1,s2 : longint) : longint;
    begin
       //uniicmp:=nwusuniicmp(s1,s2);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    function uninicmp(s1,s2,l : longint) : longint;
    begin
       //uninicmp:=nwusuninicmp(s1,s2,l);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    function NWScanForTrustees(a,b,c,d,e,f : longint) : longint;
    begin
       //NWScanForTrustees:=NWIntScanForTrustees(a,b,c,d,e,f,0);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    function NWScanForTrusteesExt(a,b,c,d,e,f : longint) : longint;
    begin
       //NWScanForTrusteesExt:=NWIntScanForTrusteesExt(a,b,c,d,e,f,0);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    function NWScanDirectoryInformation(a,b,c,d,e,f,g,h : longint) : longint;
    begin
       //NWScanDirectoryInformation:=NWIntScanDirectoryInformation(a,b,c,d,e,f,g,h,0);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    function NWIntFileSearchInitialize(a,b,c,d,e,f,g,h : longint) : longint;
    begin
       //NWIntFileSearchInitialize:=NWFileSearchInitialize(a,b,c,d,e,f,g);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    function NWScanFileInformation(a,b,c,d,e,f : longint) : longint;
    begin
       //NWScanFileInformation:=NWIntScanFileInformation(a,b,c,d,e,f,0);
    end;
*)
end.
