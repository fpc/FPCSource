{
    $Id$
    This file is part of the Free Pascal run time library.

    dos definitions (V50) for MorphOS/PowerPC
    Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved.
    
    Free Pascal conversion 
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}



{ * dos global definitions (V50)
  *********************************************************************
  * }


const
  DOSNAME = 'dos.library';

const
  DOSTRUE  = -1;
  DOSFALSE =  0;

const
  MODE_OLDFILE   = 1005;
  MODE_NEWFILE   = 1006;
  MODE_READWRITE = 1004;

const
  OFFSET_BEGINNING = -1;
  OFFSET_CURRENT   =  0;
  OFFSET_END       =  1;
  OFFSET_BEGINING  =  OFFSET_BEGINNING; { * Typo fix * }

const
  BITSPERBYTE  = 8;
  BYTESPERLONG = 4;
  BITSPERLONG  = 32;
  _MAXINT      = $7FFFFFFF;
  _MININT      = $80000000;

const
  SHARED_LOCK    = -2;
  ACCESS_READ    = -2;
  EXCLUSIVE_LOCK = -1;
  ACCESS_WRITE   = -1;


type
  PDateStamp = ^TDateStamp;
  TDateStamp = packed record
    ds_Days  : LongInt;
    ds_Minute: LongInt;
    ds_Tick  : LongInt;
  end;

const
  TICKS_PER_SECOND = 50;


type
  PFileInfoBlock = ^TFileInfoBlock;
  TFileInfoBlock = packed record
    fib_DiskKey     : LongInt;
    fib_DirEntryType: LongInt;
    fib_FileName    : Array[0..107] Of Char;
    fib_Protection  : LongInt;
    fib_EntryType   : LongInt;
    fib_Size        : LongInt;
    fib_NumBlocks   : LongInt;
    fib_Date        : TDateStamp;
    fib_Comment     : Array[0..79] Of Char;
    
    fib_OwnerUID    : Word;
    fib_OwnerGID    : Word;

    fib_Reserved    : Array[0..31] Of Char;
  end;

const
  FIBB_OTR_READ    = 15;
  FIBB_OTR_WRITE   = 14;
  FIBB_OTR_EXECUTE = 13;
  FIBB_OTR_DELETE  = 12;
  FIBB_GRP_READ    = 11;
  FIBB_GRP_WRITE   = 10;
  FIBB_GRP_EXECUTE = 9;
  FIBB_GRP_DELETE  = 8;

  FIBB_SCRIPT      = 6;
  FIBB_PURE        = 5;
  FIBB_ARCHIVE     = 4;
  FIBB_READ        = 3;
  FIBB_WRITE       = 2;
  FIBB_EXECUTE     = 1;
  FIBB_DELETE      = 0;

const
  FIBF_OTR_READ    = (1 Shl FIBB_OTR_READ);
  FIBF_OTR_WRITE   = (1 Shl FIBB_OTR_WRITE);
  FIBF_OTR_EXECUTE = (1 Shl FIBB_OTR_EXECUTE);
  FIBF_OTR_DELETE  = (1 Shl FIBB_OTR_DELETE);
  FIBF_GRP_READ    = (1 Shl FIBB_GRP_READ);
  FIBF_GRP_WRITE   = (1 Shl FIBB_GRP_WRITE);
  FIBF_GRP_EXECUTE = (1 Shl FIBB_GRP_EXECUTE);
  FIBF_GRP_DELETE  = (1 Shl FIBB_GRP_DELETE);

  FIBF_SCRIPT      = (1 Shl FIBB_SCRIPT);
  FIBF_PURE        = (1 Shl FIBB_PURE);
  FIBF_ARCHIVE     = (1 Shl FIBB_ARCHIVE);
  FIBF_READ        = (1 Shl FIBB_READ);
  FIBF_WRITE       = (1 Shl FIBB_WRITE);
  FIBF_EXECUTE     = (1 Shl FIBB_EXECUTE);
  FIBF_DELETE      = (1 Shl FIBB_DELETE);

const
  FAULT_MAX = 82;

{$WARNING FIX ME!!!}
{
typedef long BPTR;
typedef long BSTR;


#ifdef OBSOLETE_LIBRARIES_DOS_H
# define BADDR(bptr)  (((ULONG)bptr) << 2)
#else
# define BADDR(x)     ((APTR)((ULONG)(x) << 2))
#endif

  MKBADDR(x)    (((LONG)(x)) >> 2)
}

type
  PInfoData = ^TInfoData;
  TInfoData = packed record
    id_NumSoftErrors: LongInt;
    id_UnitNumber   : LongInt;
    id_DiskState    : LongInt;
    id_NumBlocks    : LongInt;
    id_NumBlocksUsed: LongInt;
    id_BytesPerBlock: LongInt;
    id_DiskType     : LongInt;
    id_VolumeNode   : LongInt; {BPTR}
    id_InUse        : LongInt;
  end;

const
  ID_WRITE_PROTECTED = 80;
  ID_VALIDATING      = 81;
  ID_VALIDATED       = 82;

  ID_NO_DISK_PRESENT  = -1;
  ID_UNREADABLE_DISK  = $42414400;
  ID_DOS_DISK         = $444F5300;
  ID_FFS_DISK         = $444F5301;
  ID_INTER_DOS_DISK   = $444F5302;
  ID_INTER_FFS_DISK   = $444F5303;
  ID_FASTDIR_DOS_DISK = $444F5304;
  ID_FASTDIR_FFS_DISK = $444F5305;
  ID_LNFS_DOS_DISK    = $444F5306;
  ID_LNFS_FFS_DISK    = $444F5307;
  ID_NOT_REALLY_DOS   = $4E444F53;
  ID_KICKSTART_DISK   = $4B49434B;
  ID_MSDOS_DISK       = $4d534400;

const
  ERROR_NO_FREE_STORE            = 103;
  ERROR_TASK_TABLE_FULL          = 105;
  ERROR_BAD_TEMPLATE             = 114;
  ERROR_BAD_NUMBER               = 115;
  ERROR_REQUIRED_ARG_MISSING     = 116;
  ERROR_KEY_NEEDS_ARG            = 117;
  ERROR_TOO_MANY_ARGS            = 118;
  ERROR_UNMATCHED_QUOTES         = 119;
  ERROR_LINE_TOO_LONG            = 120;
  ERROR_FILE_NOT_OBJECT          = 121;
  ERROR_INVALID_RESIDENT_LIBRARY = 122;
  ERROR_NO_DEFAULT_DIR           = 201;
  ERROR_OBJECT_IN_USE            = 202;
  ERROR_OBJECT_EXISTS            = 203;
  ERROR_DIR_NOT_FOUND            = 204;
  ERROR_OBJECT_NOT_FOUND         = 205;
  ERROR_BAD_STREAM_NAME          = 206;
  ERROR_OBJECT_TOO_LARGE         = 207;
  ERROR_ACTION_NOT_KNOWN         = 209;
  ERROR_INVALID_COMPONENT_NAME   = 210;
  ERROR_INVALID_LOCK             = 211;
  ERROR_OBJECT_WRONG_TYPE        = 212;
  ERROR_DISK_NOT_VALIDATED       = 213;
  ERROR_DISK_WRITE_PROTECTED     = 214;
  ERROR_RENAME_ACROSS_DEVICES    = 215;
  ERROR_DIRECTORY_NOT_EMPTY      = 216;
  ERROR_TOO_MANY_LEVELS          = 217;
  ERROR_DEVICE_NOT_MOUNTED       = 218;
  ERROR_SEEK_ERROR               = 219;
  ERROR_COMMENT_TOO_BIG          = 220;
  ERROR_DISK_FULL                = 221;
  ERROR_DELETE_PROTECTED         = 222;
  ERROR_WRITE_PROTECTED          = 223;
  ERROR_READ_PROTECTED           = 224;
  ERROR_NOT_A_DOS_DISK           = 225;
  ERROR_NO_DISK                  = 226;
  ERROR_NO_MORE_ENTRIES          = 232;

  ERROR_IS_SOFT_LINK             = 233;
  ERROR_OBJECT_LINKED            = 234;
  ERROR_BAD_HUNK                 = 235;
  ERROR_NOT_IMPLEMENTED          = 236;
  ERROR_RECORD_NOT_LOCKED        = 240;
  ERROR_LOCK_COLLISION           = 241;
  ERROR_LOCK_TIMEOUT             = 242;
  ERROR_UNLOCK_ERROR             = 243;

const
  RETURN_OK    = 0;
  RETURN_WARN  = 5;
  RETURN_ERROR = 10;
  RETURN_FAIL  = 20;

const
  SIGBREAKB_CTRL_C = 12;
  SIGBREAKB_CTRL_D = 13;
  SIGBREAKB_CTRL_E = 14;
  SIGBREAKB_CTRL_F = 15;

  SIGBREAKF_CTRL_C = (1 Shl SIGBREAKB_CTRL_C);
  SIGBREAKF_CTRL_D = (1 Shl SIGBREAKB_CTRL_D);
  SIGBREAKF_CTRL_E = (1 Shl SIGBREAKB_CTRL_E);
  SIGBREAKF_CTRL_F = (1 Shl SIGBREAKB_CTRL_F);

const
  LOCK_DIFFERENT    = -1;
  LOCK_SAME         =  0;
  LOCK_SAME_VOLUME  =  1;
  LOCK_SAME_HANDLER =  LOCK_SAME_VOLUME;

const
  CHANGE_LOCK = 0;
  CHANGE_FH   = 1;

const
  LINK_HARD  = 0;
  LINK_SOFT  = 1;

const
  ITEM_EQUAL    = -2;
  ITEM_ERROR    = -1;
  ITEM_NOTHING  =  0;
  ITEM_UNQUOTED =  1;
  ITEM_QUOTED   =  2;

const
  DOS_FILEHANDLE   = 0;
  DOS_EXALLCONTROL = 1;
  DOS_FIB          = 2;
  DOS_STDPKT       = 3;
  DOS_CLI          = 4;
  DOS_RDARGS       = 5;



{ * dos date/time definitions
  *********************************************************************
  * }


type
  { * Required to avoid conflict with default types * }
  _PDateTime = ^_TDateTime;
  _TDateTime = packed record
    dat_Stamp  : TDateStamp;
    dat_Format : Byte;
    dat_Flags  : Byte;
    dat_StrDay : Pointer;
    dat_StrDate: Pointer;
    dat_StrTime: Pointer;
  end;

const
  LEN_DATSTRING  = 16;

const
  DTB_SUBST  = 0;
  DTF_SUBST  = (1 Shl DTB_SUBST);
  DTB_FUTURE = 1;
  DTF_FUTURE = (1 Shl DTB_FUTURE);

const
  FORMAT_DOS = 0;
  FORMAT_INT = 1;
  FORMAT_USA = 2;
  FORMAT_CDN = 3;
  FORMAT_MAX = FORMAT_CDN;
  FORMAT_DEF = 4;



{ * dos extended structures definitions
  *********************************************************************
  * }


type
  PProcess = ^TProcess;
  TProcess = packed record
    pr_Task          : TTask;
    pr_MsgPort       : TMsgPort;
    pr_Pad           : Word;
    pr_SegList       : DWord;    { BPTR }
    pr_StackSize     : LongInt;  { 68k stacksize! }
    pr_GlobVec       : Pointer; 
    pr_TaskNum       : LongInt;
    pr_StackBase     : DWord;    { BPTR }
    pr_Result2       : LongInt;
    pr_CurrentDir    : DWord;    { BPTR }
    pr_CIS           : DWord;    { BPTR }
    pr_COS           : DWord;    { BPTR }
    pr_ConsoleTask   : Pointer;
    pr_FileSystemTask: Pointer;
    pr_CLI           : DWord;    { BPTR }
    pr_ReturnAddr    : Pointer;
    pr_PktWait       : Pointer;
    pr_WindowPtr     : Pointer;
    pr_HomeDir       : DWord;    { BPTR }
    pr_Flags         : LongInt;
    pr_ExitCode      : Pointer;  { Procedure }
    pr_ExitData      : LongInt;
    pr_Arguments     : PChar;
    pr_LocalVars     : TMinList;
    pr_ShellPrivate  : DWord;
    pr_CES           : DWord;    { BPTR }
  end;

const
  PRB_FREESEGLIST = 0;
  PRF_FREESEGLIST = (1 Shl PRB_FREESEGLIST);

  PRB_FREECURRDIR = 1;
  PRF_FREECURRDIR = (1 Shl PRB_FREECURRDIR);

  PRB_FREECLI     = 2;
  PRF_FREECLI     = (1 Shl PRB_FREECLI);

  PRB_CLOSEINPUT  = 3;
  PRF_CLOSEINPUT  = (1 Shl PRB_CLOSEINPUT);

  PRB_CLOSEOUTPUT = 4;
  PRF_CLOSEOUTPUT = (1 Shl PRB_CLOSEOUTPUT);

  PRB_FREEARGS    = 5;
  PRF_FREEARGS    = (1 Shl PRB_FREEARGS);


type
  {$WARNING FIX ME!!!}
  PFileHandle = ^TFileHandle;
  TFileHandle = packed record
    fh_Flags      : DWord;
    fh_Interactive: LongInt;
    fh_Type       : PMsgPort;
    fh_Buf        : LongInt;
    fh_Pos        : LongInt;
    fh_End        : LongInt;
    fh_Func1      : LongInt;
    fh_Func2      : LongInt;
    fh_Func3      : LongInt;
    fh_Arg1       : LongInt;
    fh_Arg2       : LongInt;
    { *** V50 MorphOS *** }
    fh_BufSize    : LongInt;
    fh_OrigBuf    : LongInt;
  end;

type
  PDOSPacket = ^TDOSPacket;
  TDOSPacket = packed record
    dp_Link: PMessage;
    dp_Port: PMsgPort;
    case Byte of
    0 : ( dp_Action : Longint;
          dp_Status : Longint;
          dp_Status2: Longint;
          dp_BufAddr: Longint;
        );
    1 : ( dp_Type: Longint;      
          dp_Res1: Longint;      
          dp_Res2: Longint;      
          dp_Arg1: Longint;
          dp_Arg2: Longint;
          dp_Arg3: Longint;
          dp_Arg4: Longint;
          dp_Arg5: Longint;
          dp_Arg6: Longint;
          dp_Arg7: Longint;
        );
  end;

type
  PStandardPacket = ^TStandardPacket;
  TStandardPacket = packed record
    sp_Msg: TMessage;
    sp_Pkt: TDOSPacket;
  end;


const
  ACTION_NIL            = 0;
  ACTION_STARTUP        = 0;
  ACTION_GET_BLOCK      = 2; { *** OBSOLETE *** }
  ACTION_SET_MAP        = 4;
  ACTION_DIE            = 5;
  ACTION_EVENT          = 6;
  ACTION_CURRENT_VOLUME = 7;
  ACTION_LOCATE_OBJECT  = 8;
  ACTION_RENAME_DISK    = 9;
  ACTION_WRITE          = 'W';
  ACTION_READ           = 'R';
  ACTION_FREE_LOCK      = 15;
  ACTION_DELETE_OBJECT  = 16;
  ACTION_RENAME_OBJECT  = 17;
  ACTION_MORE_CACHE     = 18;
  ACTION_COPY_DIR       = 19;
  ACTION_WAIT_CHAR      = 20;
  ACTION_SET_PROTECT    = 21;
  ACTION_CREATE_DIR     = 22;
  ACTION_EXAMINE_OBJECT = 23;
  ACTION_EXAMINE_NEXT   = 24;
  ACTION_DISK_INFO      = 25;
  ACTION_INFO           = 26;
  ACTION_FLUSH          = 27;
  ACTION_SET_COMMENT    = 28;
  ACTION_PARENT         = 29;
  ACTION_TIMER          = 30;
  ACTION_INHIBIT        = 31;
  ACTION_DISK_TYPE      = 32;
  ACTION_DISK_CHANGE    = 33;
  ACTION_SET_DATE       = 34;

  ACTION_SAME_LOCK      = 40;

  ACTION_SCREEN_MODE    = 994;

  ACTION_CHANGE_SIGNAL  = 995;

  ACTION_READ_RETURN     = 1001;
  ACTION_WRITE_RETURN    = 1002;
  ACTION_SEEK            = 1008;
  ACTION_FINDUPDATE      = 1004;
  ACTION_FINDINPUT       = 1005;
  ACTION_FINDOUTPUT      = 1006;
  ACTION_END             = 1007;

  ACTION_FORMAT          = 1020;
  ACTION_MAKE_LINK       = 1021;

  ACTION_SET_FILE_SIZE   = 1022;
  ACTION_WRITE_PROTECT   = 1023;

  ACTION_READ_LINK       = 1024;
  ACTION_FH_FROM_LOCK    = 1026;
  ACTION_IS_FILESYSTEM   = 1027;
  ACTION_CHANGE_MODE     = 1028;

  ACTION_COPY_DIR_FH     = 1030;
  ACTION_PARENT_FH       = 1031;
  ACTION_EXAMINE_ALL     = 1033;
  ACTION_EXAMINE_FH      = 1034;

  ACTION_EXAMINE_ALL_END = 1035;
  ACTION_SET_OWNER       = 1036;

  ACTION_LOCK_RECORD     = 2008;
  ACTION_FREE_RECORD     = 2009;

  ACTION_ADD_NOTIFY      = 4097;
  ACTION_REMOVE_NOTIFY   = 4098;

  ACTION_SERIALIZE_DISK  = 4200;

  ACTION_GET_DISK_FSSM   = 4201;
  ACTION_FREE_DISK_FSSM  = 4202;


type
  PErrorString = ^TErrorString;
  TErrorString = packed record
    estr_Nums: Pointer; { ^LongInt }
    estr_Byte: Pointer; { ^Byte    }
  end;

type
  PRootNode = ^TRootNode;
  TRootNode = packed record
    rn_TaskArray         : DWord;      { BPTR }
    rn_ConsoleSegment    : DWord;      { BPTR }
    rn_Time              : TDateStamp;
    rn_RestartSeg        : LongInt;
    rn_Info              : DWord;      { BPTR }
    rn_FileHandlerSegment: DWord;      { BPTR }
    rn_CliList           : TMinList;
    rn_BootProc          : PMsgPort;
    rn_ShellSegment      : DWord;      { BPTR }
    rn_Flags             : LongInt;
  end;

type
  PDOSLibrary = ^TDOSLibrary;
  TDOSLibrary = packed record
    dl_Lib          : TLibrary;
    dl_Root         : PRootNode;
    dl_GU           : Pointer;
    dl_A2           : LongInt;
    dl_A5           : LongInt;
    dl_A6           : LongInt;
    dl_Errors       : PErrorString;
    dl_TimeReq      : PTimeRequest;
    dl_UtilityBase  : PLibrary;
    dl_IntuitionBase: PLibrary;
  end;


const
  RNB_WILDSTAR = 24;
  RNF_WILDSTAR = (1 Shl RNB_WILDSTAR);

  RNB_PRIVATE1 = 1;
  RNF_PRIVATE1 = (1 Shl RNB_PRIVATE1);


type
  PCliProcList = ^TCliProcList;
  TCliProcList = packed record
    cpl_Node : TMinNode;
    cpl_First: LongInt;
    cpl_Array: Array[0..0] Of PMsgPort;
  end;

type
  PDOSInfo = ^TDOSInfo;
  TDOSInfo = packed record
    case smallint of
      0 : ( di_ResList: DWord; { BPTR }
          );
      1 : ( di_McName    : DWord; { BPTR }
            di_DevInfo   : DWord; { BPTR }          
            di_Devices   : DWord; { BPTR }          
            di_Handlers  : DWord; { BPTR }          
            di_NetHand   : Pointer;       
            di_DevLock   : TSignalSemaphore;                      
            di_EntryLock : TSignalSemaphore;
            di_DeleteLock: TSignalSemaphore;
          );
  end;

type
  PSegment = ^TSegment;
  TSegment = packed record
    seg_Next : DWord;   { BPTR }
    seg_UC   : LongInt;
    seg_Seg  : DWord;   { BPTR }
    seg_Name : Array[0..3] Of Byte;
    { * seg_Name continues * }
  end;


const
  CMD_SYSTEM    = -1;
  CMD_INTERNAL  = -2;
  CMD_NOTLOADED = -998;
  CMD_DISABLED  = -999;


type
  PCommandLineInterface = ^TCommandLineInterface;
  TCommandLineInterface = packed record
    cli_Result2       : LongInt;       
    cli_SetName       : DWord;   { BSTR }
    cli_CommandDir    : DWord;   { BPTR }
    cli_ReturnCode    : LongInt;       
    cli_CommandName   : DWord;   { BSTR }
    cli_FailLevel     : LongInt;       
    cli_Prompt        : DWord;   { BSTR }
    cli_StandardInput : DWord;   { BPTR }
    cli_CurrentInput  : DWord;   { BPTR }
    cli_CommandFile   : DWord;   { BSTR }
    cli_Interactive   : LongInt;       
    cli_Background    : LongInt;       
    cli_CurrentOutput : DWord;   { BPTR }
    cli_DefaultStack  : LongInt;       
    cli_StandardOutput: DWord;   { BPTR }
    cli_Module        : DWord;   { BPTR }
  end;

type
  PDeviceList = ^TDeviceList;
  TDeviceList = packed record
    dl_Next      : DWord;      { BPTR }
    dl_Type      : LongInt;
    dl_Task      : PMsgPort;
    dl_Lock      : DWord;      { BPTR }
    dl_VolumeDate: TDateStamp;
    dl_LockList  : DWord;      { BPTR }  
    dl_DiskType  : LongInt;
    dl_unused    : LongInt;
    dl_Name      : DWord;      { BSTR }
  end;

type
  PDevInfo = ^TDevInfo;
  TDevInfo = packed record
    dvi_Next     : DWord; { BPTR }
    dvi_Type     : LongInt;
    dvi_Task     : Pointer;
    dvi_Lock     : DWord; { BPTR }
    dvi_Handler  : DWord; { BSTR }
    dvi_StackSize: LongInt;
    dvi_Priority : LongInt;
    dvi_Startup  : LongInt;
    dvi_SegList  : DWord; { BPTR }
    dvi_GlobVec  : DWord; { BPTR }
    dvi_Name     : DWord; { BSTR }  
  end;

type 
  PAssignList = ^TAssignList;
  TAssignList = packed record
    al_Next: PAssignList;
    al_Lock: DWord;       { BPTR }
  end;

type
  PDOSList = ^TDOSList;
  TDOSList = packed record
    dol_Next: DWord;    { BPTR }      
    dol_Type: LongInt;
    dol_Task: PMsgPort;
    dol_Lock: DWord;    { BPTR }      
    case Byte of
      0: ( dol_handler : record
             dol_Handler  : DWord;    { BSTR }
             dol_StackSize: LongInt;
             dol_Priority : LongInt;
             dol_Startup  : DWord;
             dol_SegList  : DWord;    { BPTR }
             dol_GlobVec  : DWord;    { BPTR }
           end;
         );
      1: ( dol_volume : record
             dol_VolumeDate: TDateStamp;
             dol_LockList  : DWord;   { BPTR } 
             dol_DiskType  : LongInt;
           end;
         );
      2: ( dol_assign : record
             dol_AssignName: PChar;
             dol_List      : PAssignList;    
           end;
           dol_Name: DWord;    { BPTR }
         );
  end;


const
  DLT_DEVICE     = 0;
  DLT_DIRECTORY  = 1;
  DLT_VOLUME     = 2;
  DLT_LATE       = 3;
  DLT_NONBINDING = 4;
  DLT_PRIVATE    = -1;


type
  PDevProc = ^TDevProc;
  TDevProc = packed record
    dvp_Port   : PMsgPort;
    dvp_Lock   : DWord;    { BPTR }
    dvp_Flags  : DWord;       
    dvp_DevNode: PDOSList;
  end;


const
  DVPB_UNLOCK = 0;
  DVPF_UNLOCK = (1 Shl DVPB_UNLOCK);

  DVPB_ASSIGN = 1;
  DVPF_ASSIGN = (1 Shl DVPB_ASSIGN);

const
  LDB_READ    = 0;
  LDF_READ    = (1 Shl LDB_READ);

  LDB_WRITE   = 1;
  LDF_WRITE   = (1 Shl LDB_WRITE);

  LDB_DEVICES = 2;
  LDF_DEVICES = (1 Shl LDB_DEVICES);

  LDB_VOLUMES = 3;
  LDF_VOLUMES = (1 Shl LDB_VOLUMES);

  LDB_ASSIGNS = 4;
  LDF_ASSIGNS = (1 Shl LDB_ASSIGNS);

  LDB_ENTRY   = 5;
  LDF_ENTRY   = (1 Shl LDB_ENTRY);

  LDB_DELETE  = 6;
  LDF_DELETE  = (1 Shl LDB_DELETE);

  LDF_ALL     = (LDF_DEVICES Or LDF_VOLUMES Or LDF_ASSIGNS);


type
  PFileLock = ^TFileLock;
  TFileLock = packed record
    fl_Link  : DWord;   { BPTR }
    fl_Key   : LongInt;         
    fl_Access: LongInt;         
    fl_Task  : PMsgPort;  
    fl_Volume: DWord;   { BPTR } 
  end;


const
  REPORT_STREAM = 0;
  REPORT_TASK   = 1;
  REPORT_LOCK   = 2;
  REPORT_VOLUME = 3;
  REPORT_INSERT = 4;

const
  ABORT_DISK_ERROR = 296;
  ABORT_BUSY       = 288;

const
  RUN_EXECUTE       = -1;
  RUN_SYSTEM        = -2;
  RUN_SYSTEM_ASYNCH = -3;

const
  ST_ROOT      = 1;
  ST_USERDIR   = 2;
  ST_SOFTLINK  = 3;
  ST_LINKDIR   = 4;
  ST_FILE      = -3;
  ST_LINKFILE  = -4;
  ST_PIPEFILE  = -5;



{ * dos asl definitions
  *********************************************************************
  * }


type
  PAChain = ^TAChain;
  TAChain = packed record
    an_Child : PAChain;
    an_Parent: PAChain;
    an_Lock  : DWord;   { BPTR }
    an_Info  : TFileInfoBlock;
    an_Flags : ShortInt;
    an_String: Array[0..0] Of Char;
    { * an_String continues * }
  end;

type
  PAnchorPath = ^TAnchorPath;
  TAnchorPath = packed record
    case Byte of
      0 : ( ap_First: PAChain;
            ap_Last : PAChain;
          );
      1 : ( ap_Base      : PAChain;
            ap_Current   : PAChain;   
            ap_BreakBits : LongInt;
            ap_FoundBreak: LongInt;  
            ap_Flags     : ShortInt;
            ap_Reserved  : ShortInt;
            ap_Strlen    : Integer;
            ap_Info      : TFileInfoBlock;
            ap_Buf       : Array[0..0] of Char;
            { * an_Buf continues * }
          );
  end;


const
  APB_DOWILD       = 0;
  APF_DOWILD       = (1 Shl APB_DOWILD);

  APB_ITSWILD      = 1;
  APF_ITSWILD      = (1 Shl APB_ITSWILD);

  APB_DODIR        = 2;
  APF_DODIR        = (1 Shl APB_DODIR);

  APB_DIDDIR       = 3;
  APF_DIDDIR       = (1 Shl APB_DIDDIR);

  APB_NOMEMERR     = 4;
  APF_NOMEMERR     = (1 Shl APB_NOMEMERR);

  APB_DODOT        = 5;
  APF_DODOT        = (1 Shl APB_DODOT);

  APB_DirChanged   = 6;
  APF_DirChanged   = (1 Shl APB_DirChanged);

  APB_FollowHLinks = 7;
  APF_FollowHLinks = (1 Shl APB_FollowHLinks);

const
  APSB_EXTENDED        = 15;
  APSF_EXTENDED        = (1 Shl APSB_EXTENDED);

  APEB_DoMultiAssigns  = 0;
  APEF_DoMultiAssigns  = (1 Shl APEB_DoMultiAssigns);

  APEB_FutureExtension = 7;
  APEF_FutureExtension = (1 Shl APEB_FutureExtension);

const
  DDB_PatternBit  = 0;
  DDF_PatternBit  = (1 Shl DDB_PatternBit);

  DDB_ExaminedBit = 1;
  DDF_ExaminedBit = (1 Shl DDB_ExaminedBit);

  DDB_Completed   = 2;
  DDF_Completed   = (1 Shl DDB_Completed);

  DDB_AllBit      = 3;
  DDF_AllBit      = (1 Shl DDB_AllBit);

  DDB_Single      = 4;
  DDF_Single      = (1 Shl DDB_Single);

const
  P_ANY      = $80;
  P_SINGLE   = $81;
  P_ORSTART  = $82;
  P_ORNEXT   = $83;
  P_OREND    = $84;
  P_NOT      = $85;
  P_NOTEND   = $86;
  P_NOTCLASS = $87;
  P_CLASS    = $88;
  P_REPBEG   = $89;
  P_REPEND   = $8A;
  P_STOP     = $8B;

const
  COMPLEX_BIT = 1;
  EXAMINE_BIT = 2;

const
  ERROR_BUFFER_OVERFLOW = 303;
  ERROR_BREAK           = 304;
  ERROR_NOT_EXECUTABLE  = 305;



{ * dos hunk definitions
  *********************************************************************
  * }


const
  HUNK_UNIT         = 999;
  HUNK_NAME         = 1000;
  HUNK_CODE         = 1001;
  HUNK_DATA         = 1002;
  HUNK_BSS          = 1003;

  HUNK_RELOC32      = 1004;
  HUNK_ABSRELOC32   = HUNK_RELOC32;

  HUNK_RELOC16      = 1005;
  HUNK_RELRELOC16   = HUNK_RELOC16;

  HUNK_RELOC8       = 1006;
  HUNK_RELRELOC8    = HUNK_RELOC8;

  HUNK_EXT          = 1007;
  HUNK_SYMBOL       = 1008;
  HUNK_DEBUG        = 1009;
  HUNK_END          = 1010;
  HUNK_HEADER       = 1011;

  HUNK_OVERLAY      = 1013;
  HUNK_BREAK        = 1014;

  HUNK_DREL32       = 1015;
  HUNK_DREL16       = 1016;
  HUNK_DREL8        = 1017;

  HUNK_LIB          = 1018;
  HUNK_INDEX        = 1019;

  HUNK_RELOC32SHORT = 1020;

  HUNK_RELRELOC32   = 1021;
  HUNK_ABSRELOC16   = 1022;

const
  HUNKB_ADVISORY = 29;
  HUNKB_CHIP     = 30;
  HUNKB_FAST     = 31;

  HUNKF_ADVISORY = (1 Shl HUNKB_ADVISORY);
  HUNKF_CHIP     = (1 Shl HUNKB_CHIP);
  HUNKF_FAST     = (1 Shl HUNKB_FAST);

const
  EXT_SYMB      = 0;
  EXT_DEF       = 1;
  EXT_ABS       = 2;
  EXT_RES       = 3;

  EXT_REF32     = 129;
  EXT_ABSREF32  = EXT_REF32;

  EXT_COMMON    = 130;
  EXT_ABSCOMMON = EXT_COMMON;

  EXT_REF16     = 131;
  EXT_RELREF16  = EXT_REF16;

  EXT_REF8      = 132;
  EXT_RELREF8   = EXT_REF8;

  EXT_DEXT32    = 133;
  EXT_DEXT16    = 134;
  EXT_DEXT8     = 135;

  EXT_RELREF32  = 136;
  EXT_RELCOMMON = 137;

  EXT_ABSREF16  = 138;

  EXT_ABSREF8   = 139;



{
  $Log$
  Revision 1.1  2004-06-26 20:46:17  karoly
    * initial revision

}
