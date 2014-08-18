{
    This file is part of the Free Pascal run time library.

    dos definitions (V50) for MorphOS/PowerPC
    Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved.

    Free Pascal conversion
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>
    
    AROS conversion
    Copyright (c) 2011 Marcus Sackrow

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}



{ * dos global definitions
  *********************************************************************
  * }


const  // Checked OK 04.08.2011 ALB
  DOSNAME = 'dos.library';

const  // Checked OK 04.08.2011 ALB
  DOSTRUE  = -1;
  DOSFALSE =  0;

const  // Checked OK 04.08.2011 ALB
  MODE_OLDFILE   = 1005;
  MODE_NEWFILE   = 1006;
  MODE_READWRITE = 1004;

const  // Checked OK 04.08.2011 ALB
  OFFSET_BEGINNING = -1;
  OFFSET_CURRENT   =  0;
  OFFSET_END       =  1;

const  // Checked OK 04.08.2011 ALB
  BITSPERBYTE  = 8;
  BYTESPERLONG = 4;
  BITSPERLONG  = 32;
  _MAXINT      = $7FFFFFFF;
  _MININT      = $80000000;

const  // Checked OK 04.08.2011 ALB
  SHARED_LOCK    = -2;
  ACCESS_READ    = SHARED_LOCK;
  EXCLUSIVE_LOCK = -1;
  ACCESS_WRITE   = EXCLUSIVE_LOCK;

type  // Checked OK 04.08.2011 ALB
  PDateStamp = ^TDateStamp; 
  TDateStamp = record
    ds_Days  : LongInt;
    ds_Minute: LongInt;
    ds_Tick  : LongInt;
  end;

const  // Checked OK 04.08.2011 ALB
  TICKS_PER_SECOND = 50;

const  // Checked OK 04.08.2011 ALB
  MAXFILENAMELENGTH = 108; 
  MAXCOMMENTLENGTH = 80; 

type  // Checked OK 04.08.2011 ALB
  PFileInfoBlock = ^TFileInfoBlock;
  TFileInfoBlock = record          
    fib_DiskKey     : PtrUInt;
    fib_DirEntryType: LongInt;
    fib_FileName    : array[0..MAXFILENAMELENGTH - 1] of char;
    fib_Protection  : LongInt;
    fib_EntryType   : LongInt;
    fib_Size        : LongInt;
    fib_NumBlocks   : LongInt;
    fib_Date        : TDateStamp;
    fib_Comment     : Array[0..MAXCOMMENTLENGTH - 1] Of char;

    fib_OwnerUID    : Word;
    fib_OwnerGID    : Word;

    fib_Reserved    : array[0..31] of char;
  end;

const  // Checked OK 04.08.2011 ALB
  FIBB_OTR_READ    = 15;
  FIBB_OTR_WRITE   = 14;
  FIBB_OTR_EXECUTE = 13;
  FIBB_OTR_DELETE  = 12;

  FIBB_GRP_READ    = 11;
  FIBB_GRP_WRITE   = 10;
  FIBB_GRP_EXECUTE = 9;
  FIBB_GRP_DELETE  = 8;

  FIBB_SCRIPT      = 6; // File is a script (DOS or ARexx).
  FIBB_PURE        = 5; // Make program resident on execution.
  FIBB_ARCHIVE     = 4; // File was archived (not used by OS).
  FIBB_READ        = 3; // File is readable.
  FIBB_WRITE       = 2; // File is writable.
  FIBB_EXECUTE     = 1; // File is executable (no scripts!).
  FIBB_DELETE      = 0; // File is deleteable.

const  // Checked OK 04.08.2011 ALB
  FIBF_OTR_READ    = (1 shl FIBB_OTR_READ);
  FIBF_OTR_WRITE   = (1 shl FIBB_OTR_WRITE);
  FIBF_OTR_EXECUTE = (1 shl FIBB_OTR_EXECUTE);
  FIBF_OTR_DELETE  = (1 shl FIBB_OTR_DELETE);

  FIBF_GRP_READ    = (1 shl FIBB_GRP_READ);
  FIBF_GRP_WRITE   = (1 shl FIBB_GRP_WRITE);
  FIBF_GRP_EXECUTE = (1 shl FIBB_GRP_EXECUTE);
  FIBF_GRP_DELETE  = (1 shl FIBB_GRP_DELETE);

  FIBF_SCRIPT      = (1 shl FIBB_SCRIPT);
  FIBF_PURE        = (1 shl FIBB_PURE);
  FIBF_ARCHIVE     = (1 shl FIBB_ARCHIVE);
  FIBF_READ        = (1 shl FIBB_READ);
  FIBF_WRITE       = (1 shl FIBB_WRITE);
  FIBF_EXECUTE     = (1 shl FIBB_EXECUTE);
  FIBF_DELETE      = (1 shl FIBB_DELETE);

const // Checked OK 04.08.2011 ALB
  FAULT_MAX = 82;


type  // Checked OK 04.08.2011 ALB
  BPTR = Pointer;
  BSTR = Pointer;

type  // Checked OK 04.08.2011 ALB
  PInfoData = ^TInfoData;
  TInfoData = record    
    id_NumSoftErrors: LongInt; // Number of soft errors on device.
    id_UnitNumber   : LongInt; // Unit number of device.
    id_DiskState    : LongInt; // State the current volume is in (see below).
    id_NumBlocks    : LongInt; // Number of blocks on device.
    id_NumBlocksUsed: LongInt; // Number of blocks in use.
    id_BytesPerBlock: LongInt; // Bytes per block.
    id_DiskType     : LongInt; // Type of disk (see below).
    id_VolumeNode   : BPTR;
    id_InUse        : PtrUInt; // Set, if device is in use.
  end;


const  // Checked OK 04.08.2011 ALB
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
  ID_SFS_BE_DISK      = $53465330;
  ID_SFS_LE_DISK      = $73667330;

const  // Checked OK 04.08.2011 ALB
  ERROR_UNKNOWN                  = 100;
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

const  // Checked OK 04.08.2011 ALB
  RETURN_OK    = 0;
  RETURN_WARN  = 5;
  RETURN_ERROR = 10;
  RETURN_FAIL  = 20;

const  // Checked OK 04.08.2011 ALB
  SIGBREAKB_CTRL_C = 12;
  SIGBREAKB_CTRL_D = 13;
  SIGBREAKB_CTRL_E = 14;
  SIGBREAKB_CTRL_F = 15;

  SIGBREAKF_CTRL_C = (1 shl SIGBREAKB_CTRL_C);
  SIGBREAKF_CTRL_D = (1 shl SIGBREAKB_CTRL_D);
  SIGBREAKF_CTRL_E = (1 shl SIGBREAKB_CTRL_E);
  SIGBREAKF_CTRL_F = (1 shl SIGBREAKB_CTRL_F);

const  // Checked OK 04.08.2011 ALB
  LOCK_DIFFERENT    = -1;
  LOCK_SAME         =  0;
  LOCK_SAME_VOLUME  =  1;

const  // Checked OK 04.08.2011 ALB
  CHANGE_LOCK = 0;
  CHANGE_FH   = 1;

const // Checked OK 04.08.2011 ALB
  LINK_HARD  = 0;
  LINK_SOFT  = 1;

const  // Checked OK 04.08.2011 ALB
  ITEM_EQUAL    = -2;
  ITEM_ERROR    = -1;
  ITEM_NOTHING  =  0;
  ITEM_UNQUOTED =  1;
  ITEM_QUOTED   =  2;

const  // Checked OK 04.08.2011 ALB
  DOS_FILEHANDLE   = 0;
  DOS_EXALLCONTROL = 1;
  DOS_FIB          = 2;
  DOS_STDPKT       = 3;
  DOS_CLI          = 4;
  DOS_RDARGS       = 5;



{ * dos date/time definitions
  *********************************************************************
  * }


type  // Checked OK 04.08.2011 ALB
  { * Required to avoid conflict with default types * }
  _PDateTime = ^_TDateTime;
  _TDateTime = record    
    dat_Stamp  : TDateStamp;
    dat_Format : Byte;
    dat_Flags  : Byte;
    dat_StrDay : PByte;
    dat_StrDate: PByte;
    dat_StrTime: PByte;
  end;

const  // Checked OK 04.08.2011 ALB
  LEN_DATSTRING  = 16;

const  // Checked OK 04.08.2011 ALB
  DTB_SUBST  = 0;
  DTF_SUBST  = (1 Shl DTB_SUBST);
  DTB_FUTURE = 1;
  DTF_FUTURE = (1 Shl DTB_FUTURE);

const  // Checked OK 04.08.2011 ALB
  FORMAT_DOS = 0;
  FORMAT_INT = 1;
  FORMAT_USA = 2;
  FORMAT_CDN = 3;
  FORMAT_MAX = FORMAT_CDN;
  FORMAT_DEF = 4;



{ * dos extended structures definitions
  *********************************************************************
  * }


type  // Checked OK 04.08.2011 ALB
  PProcess = ^TProcess;
  TProcess = record
    pr_Task          : TTask;    // Embedded task structure 
    pr_MsgPort       : TMsgPort; // Processes standard message-port. Used for various puposes.
    pr_Pad           : Word;     // Private
    pr_SegList       : BPTR;     // SegList array, used by this process.
    pr_StackSize     : LongInt;  // StackSize of the current process.
    pr_GlobVec       : Pointer;  
    pr_TaskNum       : LongInt;  // CLI process number. This may be 0, in which case the process is not connected to a CLI.
    pr_StackBase     : BPTR;     // Pointer to upper end of stack.
    pr_Result2       : PtrInt;   // Secondary return-value
    pr_CurrentDir    : BPTR;     // Lock of the current directory
    pr_CIS           : BPTR;    { BPTR }
    pr_COS           : BPTR;    { BPTR }
    pr_ConsoleTask   : Pointer;
    pr_FileSystemTask: Pointer;
    pr_CLI           : BPTR;    { BPTR }
    pr_ReturnAddr    : Pointer;
    pr_PktWait       : Pointer;
    pr_WindowPtr     : Pointer;
    pr_HomeDir       : BPTR;    { BPTR }
    pr_Flags         : LongInt;
    pr_ExitCode      : Pointer;  { Procedure }
    pr_ExitData      : PtrUInt;
    pr_Arguments     : PChar;
    pr_LocalVars     : TMinList;
    pr_ShellPrivate  : DWord;
    pr_CES           : BPTR;    { BPTR }
  end;

const  // Checked OK 04.08.2011 ALB
  PRB_FREESEGLIST = 0;
  PRF_FREESEGLIST = (1 shl PRB_FREESEGLIST);

  PRB_FREECURRDIR = 1;
  PRF_FREECURRDIR = (1 shl PRB_FREECURRDIR);

  PRB_FREECLI = 2;
  PRF_FREECLI = (1 shl PRB_FREECLI);

  PRB_CLOSEINPUT = 3;
  PRF_CLOSEINPUT = (1 shl PRB_CLOSEINPUT);

  PRB_CLOSEOUTPUT = 4;
  PRF_CLOSEOUTPUT = (1 shl PRB_CLOSEOUTPUT);

  PRB_FREEARGS = 5;
  PRF_FREEARGS = (1 shl PRB_FREEARGS);

  PRB_CLOSEERROR = 6;
  PRF_CLOSEERROR = (1 shl PRB_CLOSEERROR);

  PRB_SYNCHRONOUS = 7;
  PRF_SYNCHRONOUS = (1 shl PRB_SYNCHRONOUS);

  PRB_WAITINGFORCHILD = 8; // This one is subject to change!
  PRF_WAITINGFORCHILD = (1 shl PRB_WAITINGFORCHILD);

  PRB_NOTIFYONDEATH = 9;
  PRF_NOTIFYONDEATH = (1 shl PRB_NOTIFYONDEATH);

type  // Checked OK 04.08.2011 ALB
  PFileHandle = ^TFileHandle;
  TFileHandle = record
    // The next three are used with packet-based filesystems 
    fh_Flags      : DWord;
    fh_Interactive: PMsgPort;  // packet reply port // alias -> fh_Port
    fh_Type       : PMsgPort;  // port to send packets to

    fh_Buf        : BPTR;
    fh_Pos        : LongInt;
    fh_End        : LongInt;

    fh_Func1      : LongInt;
    fh_Func2      : LongInt;
    fh_Func3      : Pointer;
    fh_Arg1       : PtrInt;
    fh_Arg2       : Pointer;

    { *** V39+ *** }
    fh_Size       : DWord;
    fh_Buf2       : BPTR;
  end;

(*   OBSOLETE *************************
type
  PDOSPacket = ^TDOSPacket;
  TDOSPacket = record
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
  TStandardPacket = record
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

*)

type   // Checked OK 04.08.2011 ALB
  PErrorString = ^TErrorString;
  TErrorString = record
    estr_Nums: PLongInt; 
    estr_Strings: PChar;
  end;

type   // Checked OK 04.08.2011 ALB 
  PRootNode = ^TRootNode;
  TRootNode = record
    rn_TaskArray         : BPTR;      { BPTR }
    rn_ConsoleSegment    : BPTR;      { BPTR }
    rn_Time              : TDateStamp;
    rn_RestartSeg        : Pointer;
    rn_Info              : BPTR;      { BPTR }
    rn_FileHandlerSegment: BPTR;      { BPTR }
    rn_CliList           : TMinList;
    rn_BootProc          : PMsgPort;
    rn_ShellSegment      : BPTR;      { BPTR }
    rn_Flags             : LongInt;
    // Aros Special
    rn_RootLock          : TSignalSemaphore;
  end;

type  // Checked OK 04.08.2011 ALB 
  // AROS Specific 
  PCLIInfo = ^TCLIInfo;
  TCLIInfo = record
    ci_Node: TNode;
    ci_Process: PProcess;
  end; 


type  // Checked OK 05.08.2011 ALB 
  PDOSLibrary = ^TDOSLibrary;
  TDOSLibrary = record
    dl_Lib          : TLibrary;
    dl_Root         : PRootNode;
    {$ifdef AROS_BINCOMPAT}
    dl_GV           : Pointer;
    dl_A2           : LongInt;
    dl_A5           : LongInt;
    dl_A6           : LongInt;
    {$endif}
    dl_Errors       : PErrorString;
    dl_TimeReq      : PTimeRequest;
    dl_UtilityBase  : PLibrary;
    dl_IntuitionBase: PLibrary;
    // AROS Specific private Fields
    dl_TimerBase    : PDevice;
    dl_TimerIO      : TTimeRequest;
    dl_DevInfo      : BPTR; //PDosList;
    dl_SysBase      : PExecBase;
    dl_SegList      : BPTR;
    dl_NulHandler   : PDevice;
    dl_NulLock      : PUnit;
    // LDDemon private Data
    dl_LDObjectsListSigSem : TSignalSemaphore;
    dl_LDObjectsList       : TList;
    dl_LDHandler           : TInterrupt;
    dl_LDDemonPort         : PMsgPort;
    dl_LDDemonTask         : PProcess;
    dl_LDReturn            : Cardinal;

    // AROS-specific and private. Can go away in future
    dl_SYSLock: BPTR;
    // The flags are ORed with RootNode->rn_Flags. See below for definitions.
    dl_Flags: Cardinal;
  end;


const  // Checked OK 04.08.2011 ALB 
  RNB_WILDSTAR = 24;
  RNF_WILDSTAR = (1 Shl RNB_WILDSTAR);

type  // Checked OK 04.08.2011 ALB 
  PDOSInfo = ^TDOSInfo;
  TDOSInfo = record
    di_McName    : BPTR; { BPTR }
    di_DevInfo   : BPTR; { BPTR }
    di_Devices   : BPTR; { BPTR }
    di_Handlers  : BPTR; { BPTR }
    di_NetHand   : BPTR;             // Alias di_ResList
    di_DevLock   : TSignalSemaphore;
    di_EntryLock : TSignalSemaphore;
    di_DeleteLock: TSignalSemaphore;
  end;

type  // Checked OK 04.08.2011 ALB
  PSegment = ^TSegment;
  TSegment = record
    seg_Next : PSegment;  // Pointer to next segment.
    seg_UC   : LongInt;
    seg_Seg  : BPTR;   { BPTR }
    seg_Name : Array[0..3] of char;
    { * seg_Name continues * }
  end;


const  // Checked OK 04.08.2011 ALB
  CMD_SYSTEM    = -1;
  CMD_INTERNAL  = -2;
  CMD_DISABLED  = -999;

type  // Checked TODO 04.08.2011 ALB
  PDeviceList = ^TDeviceList;
  TDeviceList = record
    dl_Next      : PDeviceList;      { BPTR }
    dl_Type      : LongInt;
    dl_Task      : PMsgPort;
    dl_Lock      : BPTR;      { BPTR }
    dl_VolumeDate: TDateStamp;
    dl_LockList  : BPTR;      { BPTR }
    dl_DiskType  : LongInt;
    dl_unused    : LongInt;
    // Padding Bytes?
    dl_Name      : BSTR;      { BSTR }
   // Missing DosListAROSExt
  end;

type  // Checked OK 04.08.2011 ALB
  PFileLock = ^TFileLock;
  TFileLock = record
    fl_Link  : PFileLock;     // (PFileLock) Pointer to next lock.
    fl_Key   : PtrUInt;
    fl_Access: LongInt;
    fl_Task  : PMsgPort;
    fl_Volume: PDeviceList;  // (BPTR)
  end;


type  // Checked OK 04.08.2011 ALB
  PCommandLineInterface = ^TCommandLineInterface;
  TCommandLineInterface = record
    cli_Result2       : LongInt;   // Secondary error code, set by last command.
    cli_SetName       : BSTR;      // Name of the current directory.
    cli_CommandDir    : PFileLock; // Lock of the first directory in path.
    cli_ReturnCode    : LongInt;   // Error code, the last command returned.
    cli_CommandName   : BSTR;      // Name of the command that is currently executed.
    cli_FailLevel     : LongInt;   // Fail-Level as set by the command "FailAt".
    cli_Prompt        : BSTR;      // Current prompt in the CLI window.
    cli_StandardInput : PFileLock; // Standard/Default input file.
    cli_CurrentInput  : PFileLock; // Current input file.
    cli_CommandFile   : BSTR;      // Name of the file that is currently executed.
    cli_Interactive   : LongInt;   // TRUE if the currently CLI is connected to a controlling terminal, otherwise FALSE.
    cli_Background    : LongInt;   // FALSE if there is no controlling terminal, otherwise TRUE.
    cli_CurrentOutput : PFileLock; // Current output file. }
    cli_DefaultStack  : LongInt;   // Default stack size as set by the command "Stack"
    cli_StandardOutput: PFileLock; // Standard/Default output file. 
    cli_Module        : BPTR;      // SegList of currently loaded command. 
    // Aros Specific Part
    cli_Standard_Error: PFileLock; // Standard/Default Error file.
  end;

const // Checked OK 04.08.2011 ALB
  CLI_DEFAULTSTACK_UNIT = SizeOf(PtrUInt);

type  // Checked TODO 04.08.2011 ALB
  PDevInfo = ^TDevInfo;
  TDevInfo = record
    dvi_Next     : PDevInfo; { BPTR }
    dvi_Type     : LongInt;
    dvi_Task     : PMsgPort;
    dvi_Lock     : BPTR; { BPTR }
    dvi_Handler  : BSTR; { BSTR }
    dvi_StackSize: LongInt;
    dvi_Priority : LongInt;
    dvi_Startup  : BPTR;
    dvi_NoAROS1  : BPTR; { BPTR }
    dvi_NoAROS2  : BPTR; { BPTR }
    dvi_Name     : BSTR; { BSTR }
    // missing Aros Extension... DosListAROSExt;
  end;

type  // Checked TODO 04.08.2011 ALB
  PAssignList = ^TAssignList;
  TAssignList = record
    al_Next: PAssignList;  // Pointer to next assign node.
    al_Lock: PFileLock;    // Lock of on of the directories.
  end;

type  // Checked TODO 04.08.2011 ALB
  PDOSList = ^TDOSList;
  TDOSList = record
    dol_Next: BPTR;    // PDOSList
    dol_Type: LongInt;
    dol_Task: PMsgPort;
    dol_Lock: PFileLock; { BPTR }
    case Byte of
    0: ( dol_handler : record
           dol_Handler  : BSTR;    { BSTR }
           dol_StackSize: LongInt;
           dol_Priority : LongInt;
           dol_Startup  : BPTR;
           dol_NoAROS1  : BPTR;    { BPTR }
           dol_NoAROS2  : BPTR;    { BPTR }
           dol_NoAROS3  : BPTR;    { BPTR }
         end;
       );
    1: ( dol_volume : record
           dol_VolumeDate: TDateStamp;
           dol_LockList  : BPTR;   { BPTR }
           dol_DiskType  : LongInt;
           dol_unused    : BPTR;
         end;
       );
    2: ( dol_assign : record
           dol_AssignName: PChar;
           dol_List      : PAssignList;
         end;
       );
    3: ( dol_Misc: array[0..23] of Byte;
         dol_Name: BSTR;
       );
    // missing Aros Extension... DosListAROSExt;
  end;
  
const  // Checked OK 04.08.2011 ALB
  DLT_DEVICE     = 0;
  DLT_DIRECTORY  = 1;
  DLT_VOLUME     = 2;
  DLT_LATE       = 3;
  DLT_NONBINDING = 4;
  DLT_PRIVATE    = -1;


type  // Checked OK 04.08.2011 ALB
  PDevProc = ^TDevProc;
  TDevProc = record
    dvp_Port   : PMsgPort;
    dvp_Lock   : PFileLock; { BPTR }
    dvp_Flags  : DWord;     // see below
    dvp_DevNode: PDOSList;  // Private
  end;


const  // Checked OK 04.08.2011 ALB
  DVPB_UNLOCK = 0;
  DVPF_UNLOCK = (1 shl DVPB_UNLOCK);

  DVPB_ASSIGN = 1;
  DVPF_ASSIGN = (1 shl DVPB_ASSIGN);

const  // Checked OK 04.08.2011 ALB
  LDB_READ    = 0;
  LDF_READ    = (1 shl LDB_READ);

  LDB_WRITE   = 1;
  LDF_WRITE   = (1 shl LDB_WRITE);

  LDB_DEVICES = 2;
  LDF_DEVICES = (1 shl LDB_DEVICES);

  LDB_VOLUMES = 3;
  LDF_VOLUMES = (1 shl LDB_VOLUMES);

  LDB_ASSIGNS = 4;
  LDF_ASSIGNS = (1 shl LDB_ASSIGNS);

  LDB_ENTRY   = 5;
  LDF_ENTRY   = (1 shl LDB_ENTRY);

  LDB_DELETE  = 6;
  LDF_DELETE  = (1 shl LDB_DELETE);

  LDF_ALL     = (LDF_DEVICES or LDF_VOLUMES or LDF_ASSIGNS);


const  // Checked OK 04.08.2011 ALB
  REPORT_STREAM = 0;
  REPORT_TASK   = 1;
  REPORT_LOCK   = 2;
  REPORT_VOLUME = 3;
  REPORT_INSERT = 4;

const  // Checked OK 04.08.2011 ALB
  ABORT_DISK_ERROR = 296;
  ABORT_BUSY       = 288;

const  // Checked OK 04.08.2011 ALB
  RUN_EXECUTE       = -1;
  RUN_SYSTEM        = -2;
  RUN_SYSTEM_ASYNCH = -3;

const  // Checked OK 04.08.2011 ALB
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


type  // Checked OK 04.08.2011 ALB
  PAChain = ^TAChain;
  TAChain = record
    an_Child : PAChain;
    an_Parent: PAChain;
    an_Lock  : BPTR;
    an_Info  : TFileInfoBlock;
    an_Flags : ShortInt;
    an_String: array[0..0] of Char;
    { * an_String continues * }
  end;

type  // Checked OK 04.08.2011 ALB
  PAnchorPath = ^TAnchorPath;
  TAnchorPath = record
    case Byte of                        // its not defined in AROS struct just by defines... 
    0 : ( ap_First:  PAChain;
          ap_Last:   PAChain;
          ap_L1:     LongInt;
          ap_L2:     LongInt;
          ap_Length: ShortInt;
        );
    1 : ( ap_Base:       PAChain;
          ap_Current:    PAChain;
          ap_BreakBits:  LongInt;
          ap_FoundBreak: LongInt;
          ap_Flags:      ShortInt;
          ap_Reserved:   ShortInt;
          ap_Strlen:     SmallInt;
          ap_Info:       TFileInfoBlock;
          ap_Buf:        array[0..0] of char;
          { * an_Buf continues * }
        );
  end;


const  // Checked OK 04.08.2011 ALB
  // Please check for wildcards in supplied string.
  APB_DOWILD       = 0;                     
  APF_DOWILD       = (1 Shl APB_DOWILD);
  // There is actually a wildcard in the supplied string. READ-ONLY
  APB_ITSWILD      = 1;                     
  APF_ITSWILD      = (1 Shl APB_ITSWILD);
  // Set, if a directory is to be entered. Applications may clear this bit to prohibit the
  // matching-functions from entering a directory.
  APB_DODIR        = 2;
  APF_DODIR        = (1 Shl APB_DODIR);
  // Set, if directory was already searched. READ-ONLY
  APB_DIDDIR       = 3;
  APF_DIDDIR       = (1 Shl APB_DIDDIR);
  // Set, if function was out of memory. READ-ONLY
  APB_NOMEMERR     = 4;
  APF_NOMEMERR     = (1 Shl APB_NOMEMERR);
  // '.' may refer to the current directory (unix-style).
  APB_DODOT        = 5;
  APF_DODOT        = (1 Shl APB_DODOT);
  // Directory changed since last call.
  APB_DirChanged   = 6;
  APF_DirChanged   = (1 Shl APB_DirChanged);
  // Follow hardlinks, too.
  APB_FollowHLinks = 7;
  APF_FollowHLinks = (1 Shl APB_FollowHLinks);

const  // Checked OK 04.08.2011 ALB
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

const // Checked OK 04.08.2011 ALB
  P_ANY      = $80; // Matches everything ('#?' and '*') 
  P_SINGLE   = $81; // Any character ('?')
  P_ORSTART  = $82; // Opening parenthesis for OR'ing ('(')
  P_ORNEXT   = $83; // Field delimiter for OR'ing ('|')
  P_OREND    = $84; // Closing parenthesis for OR'ing (')')
  P_NOT      = $85; // Inversion ('~')
  P_NOTEND   = $86; // Inversion end 
  P_NOTCLASS = $87; // Inversion class ('^') 
  P_CLASS    = $88; // Class ('[' and ']')
  P_REPBEG   = $89; // Beginning of repetition ('[') 
  P_REPEND   = $8A; // End of repetition (']')
  P_STOP     = $8B;

const // Checked OK 04.08.2011 ALB
  COMPLEX_BIT = 1;
  EXAMINE_BIT = 2;

const // Checked OK 04.08.2011 ALB
  ERROR_BUFFER_OVERFLOW = 303;
  ERROR_BREAK           = 304;
  ERROR_NOT_EXECUTABLE  = 305;



{ * dos hunk definitions
  *********************************************************************
  * }


const  // Checked OK 04.08.2011 ALB
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

const  // Checked OK 04.08.2011 ALB  
  HUNKB_ADVISORY = 29;
  HUNKB_CHIP     = 30;
  HUNKB_FAST     = 31;

  HUNKF_ADVISORY = (1 shl HUNKB_ADVISORY);
  HUNKF_CHIP     = (1 shl HUNKB_CHIP);
  HUNKF_FAST     = (1 shl HUNKB_FAST);

const  // Checked OK 04.08.2011 ALB
  EXT_SYMB      = 0;
  EXT_DEF       = 1;
  EXT_ABS       = 2;

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



{ * dos ExAll definitions
  *********************************************************************
  * }


const  // Checked OK 04.08.2011 ALB
  ED_NAME       = 1;
  ED_TYPE       = 2;
  ED_SIZE       = 3;
  ED_PROTECTION = 4;
  ED_DATE       = 5;
  ED_COMMENT    = 6;
  ED_OWNER      = 7;


type  // Checked OK 04.08.2011 ALB
  PExAllData = ^TExAllData;
  TExAllData = record
    ed_Next    : PExAllData;
    ed_Name    : PChar;
    ed_Type    : LongInt;
    ed_Size    : Cardinal;
    ed_Prot    : Cardinal;
    ed_Days    : Cardinal;
    ed_Mins    : Cardinal;
    ed_Ticks   : Cardinal;
    ed_Comment : PChar;
    ed_OwnerUID: Word;
    ed_OwnerGID: Word;
  end;

type  // Checked OK 04.08.2011 ALB
  PExAllControl = ^TExAllControl;
  TexAllControl = record
    eac_Entries    : Cardinal;
    eac_LastKey    : PtrUInt;  // private
    eac_MatchString: PChar;
    eac_MatchFunc  : PHook;
  end;



{ * dos record definitions
  *********************************************************************
  * }


const  // Checked OK 04.08.2011 ALB
  REC_EXCLUSIVE       = 0;
  REC_EXCLUSIVE_IMMED = 1;
  REC_SHARED          = 2;
  REC_SHARED_IMMED    = 3;


type  // Checked OK 04.08.2011 ALB
  PRecordLock = ^TRecordLock;
  TRecordLock = record
    rec_FH    : PFileHandle;
    rec_Offset: Cardinal;
    rec_Length: Cardinal;
    rec_Mode  : Cardinal;
  end;



{ * dos tag definitions
  *********************************************************************
  * }


const  // Checked OK 04.08.2011 ALB
  SYS_Dummy       = (TAG_USER + 32);
  SYS_Input       = (SYS_Dummy + 1);
  SYS_Output      = (SYS_Dummy + 2);
  SYS_Asynch      = (SYS_Dummy + 3);
  SYS_UserShell   = (SYS_Dummy + 4);
  SYS_CustomShell = (SYS_Dummy + 5);

  { *** Aros Specific *** }
  SYS_Error       = (SYS_Dummy + 10);
  SYS_ScriptInput = (SYS_Dummy + 11);
  SYS_Background  = (SYS_Dummy + 12);
  SYS_CliNumPtr   = (SYS_Dummy + 13);

  SYS_DupStream = 1;

const  // Checked OK 04.08.2011 ALB
  NP_Dummy         = (TAG_USER + 1000);
  NP_Seglist       = (NP_Dummy + 1);
  NP_FreeSeglist   = (NP_Dummy + 2);
  NP_Entry         = (NP_Dummy + 3);
  NP_Input         = (NP_Dummy + 4);
  NP_Output        = (NP_Dummy + 5);
  NP_CloseInput    = (NP_Dummy + 6);
  NP_CloseOutput   = (NP_Dummy + 7);
  NP_Error         = (NP_Dummy + 8);
  NP_CloseError    = (NP_Dummy + 9);
  NP_CurrentDir    = (NP_Dummy + 10);
  NP_StackSize     = (NP_Dummy + 11);
  NP_Name          = (NP_Dummy + 12);
  NP_Priority      = (NP_Dummy + 13);
  NP_ConsoleTask   = (NP_Dummy + 14);
  NP_WindowPtr     = (NP_Dummy + 15);
  NP_HomeDir       = (NP_Dummy + 16);
  NP_CopyVars      = (NP_Dummy + 17);
  NP_Cli           = (NP_Dummy + 18);
  NP_Path          = (NP_Dummy + 19);
  NP_CommandName   = (NP_Dummy + 20);
  NP_Arguments     = (NP_Dummy + 21);

  NP_NotifyOnDeath = (NP_Dummy + 22);
  NP_Synchronous   = (NP_Dummy + 23);
  NP_ExitCode      = (NP_Dummy + 24);
  NP_ExitData      = (NP_Dummy + 25);

  { *** AROS Specific *** }
  NP_UserData      = (NP_Dummy + 26);

const  // Checked OK 04.08.2011 ALB
  ADO_Dummy       = (TAG_USER + 2000);
  ADO_FH_Mode     = (ADO_Dummy + 1);

  ADO_DirLen      = (ADO_Dummy + 2);
  ADO_CommNameLen = (ADO_Dummy + 3);
  ADO_CommFileLen = (ADO_Dummy + 4);
  ADO_PromptLen   = (ADO_Dummy + 5);



{ * dos stdio definitions
  *********************************************************************
  * }


const  // Checked OK 04.08.2011 ALB
 BUF_LINE = 0;
 BUF_FULL = 1;
 BUF_NONE = 2;

const  // Checked OK 04.08.2011 ALB
 ENDSTREAMCH = -1;



{ * dos env-var definitions
  *********************************************************************
  * }


type  // Checked OK 04.08.2011 ALB
  PLocalVar = ^TLocalVar;
  TLocalVar = record
    lv_Node : TNode;
    lv_Flags: Word;
    lv_Value: PChar;
    lv_Len  : Cardinal;
  end;

type  // Checked OK 04.08.2011 ALB
  PScanVarsMsg = ^TScanVarsMsg;
  TScanVarsMsg = record 
    sv_SVMSize: Cardinal;  // Size of ScanVarsMsg structure
    sv_Flags: Cardinal;    // The flags parameter given to ScanVars() 
    sv_GDir: PChar;        // Directory patch for global variables or empty string "\0" for local variables 
    sv_Name: PChar;        // Name of the variable 
    sv_Var: PChar;         // Pointer to the contents of the variable 
    sv_VarLen: Cardinal;   // Size of the variable 
  end;

const   // Checked OK 04.08.2011 ALB
  LV_VAR   = 0;
  LV_ALIAS = 1;

const   // Checked OK 04.08.2011 ALB
  LVB_IGNORE         = 7;
  LVF_IGNORE         = (1 Shl LVB_IGNORE);

  GVB_GLOBAL_ONLY    = 8;
  GVF_GLOBAL_ONLY    = (1 Shl GVB_GLOBAL_ONLY);

  GVB_LOCAL_ONLY     = 9;
  GVF_LOCAL_ONLY     = (1 Shl GVB_LOCAL_ONLY);

  GVB_BINARY_VAR     = 10;
  GVF_BINARY_VAR     = (1 Shl GVB_BINARY_VAR);

  GVB_DONT_NULL_TERM = 11;
  GVF_DONT_NULL_TERM = (1 Shl GVB_DONT_NULL_TERM);

  GVB_SAVE_VAR       = 12;
  GVF_SAVE_VAR       = (1 Shl GVB_SAVE_VAR);



{ * dos ReadArgs definitions
  *********************************************************************
  * }


type  // Checked OK 04.08.2011 ALB
  PCSource = ^TCSource;
  TCSource = record
    CS_Buffer: PChar;
    CS_Length: LongInt;
    CS_CurChr: LongInt;
  end;

type  // Checked OK 04.08.2011 ALB
  PRDArgs = ^TRDArgs;
  TRDArgs = record
    RDA_Source : TCSource;
    RDA_DAList : PtrUInt;
    RDA_Buffer : PChar;
    RDA_BufSiz : LongInt;
    RDA_ExtHelp: PChar;
    RDA_Flags  : LongInt;
  end;


const  // Checked OK 04.08.2011 ALB
  RDAB_STDIN    = 0;
  RDAF_STDIN    = (1 Shl RDAB_STDIN);

  RDAB_NOALLOC  = 1;
  RDAF_NOALLOC  = (1 Shl RDAB_NOALLOC);

  RDAB_NOPROMPT = 2;
  RDAF_NOPROMPT = (1 Shl RDAB_NOPROMPT);

const  // Checked OK 04.08.2011 ALB
  MAX_TEMPLATE_ITEMS = 100;
  MAX_MULTIARGS      = 128;



{ * dos filehandler definitions
  *********************************************************************
  * }


type  // Checked OK 04.08.2011 ALB
  PDosEnvec = ^TDosEnvec;
  TDosEnvec = record
    de_TableSize     : PtrUInt;
    de_SizeBlock     : PtrUInt;
    de_SecOrg        : PtrUInt;
    de_Surfaces      : PtrUInt;
    de_SectorPerBlock: PtrUInt;
    de_BlocksPerTrack: PtrUInt;
    de_Reserved      : PtrUInt;
    de_PreAlloc      : PtrUInt;
    de_Interleave    : PtrUInt;
    de_LowCyl        : PtrUInt;
    de_HighCyl       : PtrUInt;
    de_NumBuffers    : PtrUInt;
    de_BufMemType    : PtrUInt;
    de_MaxTransfer   : PtrUInt;
    de_Mask          : PtrUInt;
    de_BootPri       : PtrInt;
    de_DosType       : PtrUInt;
    de_Baud          : PtrUInt;
    de_Control       : PtrUInt;
    de_BootBlocks    : PtrUInt;
  end;


const  // Checked OK 04.08.2011 ALB
  DE_TABLESIZE    = 0;
  DE_SIZEBLOCK    = 1;
  DE_BLOCKSIZE    = 2;
  DE_NUMHEADS     = 3;
  DE_SECSPERBLOCK = 4;
  DE_BLKSPERTRACK = 5;
  DE_RESERVEDBLKS = 6;
  DE_PREFAC       = 7;
  DE_INTERLEAVE   = 8;
  DE_LOWCYL       = 9;
  DE_HIGHCYL      = 10;
  DE_UPPERCYL     = DE_HIGHCYL;
  DE_NUMBUFFERS   = 11;
  DE_BUFMEMTYPE   = 12;
  DE_MEMBUFTYPE   = DE_BUFMEMTYPE;
  DE_MAXTRANSFER  = 13;
  DE_MASK         = 14;
  DE_BOOTPRI      = 15;
  DE_DOSTYPE      = 16;
  DE_BAUD         = 17;
  DE_CONTROL      = 18;
  DE_BOOTBLOCKS   = 19;


type  // Checked OK 04.08.2011 ALB
  PFileSysStartupMsg = ^TFileSysStartupMsg;
  TFileSysStartupMsg = record
    fssm_Unit   : PtrUInt;
    fssm_Device : BSTR;
    fssm_Environ: BPTR;
    fssm_Flags  : Cardinal;
  end;

type  // Checked TODO 04.08.2011 ALB
  PDeviceNode = ^TDeviceNode;
  TDeviceNode = record
    dn_Next     : PDeviceNode;
    dn_Type     : Cardinal;
    dn_Task     : PMsgPort;
    dn_Lock     : BPTR;
    dn_Handler  : BSTR;
    dn_StackSize: Cardinal;
    dn_Priority : LongInt;
    dn_Startup  : BPTR;
    dn_SegList  : BPTR;
    dn_GlobalVec: BPTR;
    dn_Name     : BSTR;
    // Missing Extension DosListAROSExt
  end;

{ * dos notification definitions
  *********************************************************************
  * }


const  // Checked OK 04.08.2011 ALB
  NOTIFY_CLASS = $40000000;
  NOTIFY_CODE  = $1234;


type  // Checked OK 04.08.2011 ALB
  PNotifyRequest = ^TNotifyRequest;
  TNotifyRequest = record
    nr_Name    : PChar;
    nr_FullName: PChar;
    nr_UserData: Cardinal;
    nr_Flags   : Cardinal;
    nr_stuff : record
      case Byte of
      0 : ( nr_Msg : record
              nr_Port: PMsgPort;
            end );
      1 : ( nr_Signal : record
              nr_Task     : PTask;
              nr_SignalNum: Byte;
              nr_pad      : array[0..2] of Byte;
            end );
    end;
    nr_Reserved: array[0..3] of Cardinal;
    nr_MsgCount: Cardinal;
    nr_Handler : PMsgPort;
  end;

type    // Checked OK 04.08.2011 ALB
  PNotifyMessage = ^TNotifyMessage;
  TNotifyMessage = record
    nm_ExecMessage: TMessage;
    nm_Class      : Cardinal;
    nm_Code       : Word;
    nm_NReq       : PNotifyRequest;
    nm_DoNotTouch : PtrUInt;
    nm_DoNotTouch2: PtrUInt;
  end;


const  // Checked OK 04.08.2011 ALB
  NRB_SEND_MESSAGE   = 0;
  NRB_SEND_SIGNAL    = 1;
  NRB_WAIT_REPLY     = 3;
  NRB_NOTIFY_INITIAL = 4;
  NRB_MAGIC          = 31;
  NRF_SEND_MESSAGE   = (1 shl NRB_SEND_MESSAGE);
  NRF_SEND_SIGNAL    = (1 shl NRB_SEND_SIGNAL);
  NRF_WAIT_REPLY     = (1 shl NRB_WAIT_REPLY);
  NRF_NOTIFY_INITIAL = (1 shl NRB_NOTIFY_INITIAL);
  NRF_MAGIC          = (1 shl NRB_MAGIC);

  NR_HANDLER_FLAGS = $ffff0000;




