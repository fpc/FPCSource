{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Haiku POSIX compliant error codes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const

  LONG_MIN = -2147483648;

//----- Error baselines ---------------------------------------
  B_GENERAL_ERROR_BASE      = LONG_MIN;
  B_OS_ERROR_BASE           = B_GENERAL_ERROR_BASE + $1000;
  B_APP_ERROR_BASE          = B_GENERAL_ERROR_BASE + $2000;
  B_INTERFACE_ERROR_BASE    = B_GENERAL_ERROR_BASE + $3000;
  B_MEDIA_ERROR_BASE        = B_GENERAL_ERROR_BASE + $4000; // - 0x41ff
  B_TRANSLATION_ERROR_BASE  = B_GENERAL_ERROR_BASE + $4800; // - 0x48ff
  B_MIDI_ERROR_BASE         = B_GENERAL_ERROR_BASE + $5000;
  B_STORAGE_ERROR_BASE      = B_GENERAL_ERROR_BASE + $6000;
  B_POSIX_ERROR_BASE        = B_GENERAL_ERROR_BASE + $7000;
  B_MAIL_ERROR_BASE         = B_GENERAL_ERROR_BASE + $8000;
  B_PRINT_ERROR_BASE        = B_GENERAL_ERROR_BASE + $9000;
  B_DEVICE_ERROR_BASE       = B_GENERAL_ERROR_BASE + $a000;

  //--- Developer-defined errors start at (B_ERRORS_END+1)
  B_ERRORS_END = B_GENERAL_ERROR_BASE + $ffff;

//----- General Errors ----------------------------------------
  B_NO_MEMORY         = B_GENERAL_ERROR_BASE;
  B_IO_ERROR          = B_GENERAL_ERROR_BASE + 1;
  B_PERMISSION_DENIED = B_GENERAL_ERROR_BASE + 2;
  B_BAD_INDEX         = B_GENERAL_ERROR_BASE + 3;
  B_BAD_TYPE          = B_GENERAL_ERROR_BASE + 4;
  B_BAD_VALUE         = B_GENERAL_ERROR_BASE + 5;
  B_MISMATCHED_VALUES = B_GENERAL_ERROR_BASE + 6;
  B_NAME_NOT_FOUND    = B_GENERAL_ERROR_BASE + 7;
  B_NAME_IN_USE       = B_GENERAL_ERROR_BASE + 8;
  B_TIMED_OUT         = B_GENERAL_ERROR_BASE + 9;
  B_INTERRUPTED       = B_GENERAL_ERROR_BASE + 10;
  B_WOULD_BLOCK       = B_GENERAL_ERROR_BASE + 11;
  B_CANCELED          = B_GENERAL_ERROR_BASE + 12;
  B_NO_INIT           = B_GENERAL_ERROR_BASE + 13;
  B_BUSY              = B_GENERAL_ERROR_BASE + 14;
  B_NOT_ALLOWED       = B_GENERAL_ERROR_BASE + 15;

  B_ERROR = -1;
  B_OK = 0;
  B_NO_ERROR = 0;

//----- Kernel Kit Errors -------------------------------------
  B_BAD_SEM_ID   = B_OS_ERROR_BASE;
  B_NO_MORE_SEMS = B_OS_ERROR_BASE + 1;

  B_BAD_THREAD_ID    = B_OS_ERROR_BASE + $100;
  B_NO_MORE_THREADS  = B_BAD_THREAD_ID + 1;
  B_BAD_THREAD_STATE = B_BAD_THREAD_ID + 2;
  B_BAD_TEAM_ID      = B_BAD_THREAD_ID + 3;
  B_NO_MORE_TEAMS    = B_BAD_THREAD_ID + 4;

  B_BAD_PORT_ID   = B_OS_ERROR_BASE + $200;
  B_NO_MORE_PORTS = B_BAD_PORT_ID + 1;

  B_BAD_IMAGE_ID      = B_OS_ERROR_BASE + $300;
  B_BAD_ADDRESS       = B_BAD_IMAGE_ID + 1;
  B_NOT_AN_EXECUTABLE = B_BAD_IMAGE_ID + 2;
  B_MISSING_LIBRARY   = B_BAD_IMAGE_ID + 3;
  B_MISSING_SYMBOL    = B_BAD_IMAGE_ID + 4;

  B_DEBUGGER_ALREADY_INSTALLED = B_OS_ERROR_BASE + $400;

//----- Application Kit Errors --------------------------------
  B_BAD_REPLY                         = B_APP_ERROR_BASE;
  B_DUPLICATE_REPLY                   = B_APP_ERROR_BASE + 1;
  B_MESSAGE_TO_SELF                   = B_APP_ERROR_BASE + 2;
  B_BAD_HANDLER                       = B_APP_ERROR_BASE + 3;
  B_ALREADY_RUNNING                   = B_APP_ERROR_BASE + 4;
  B_LAUNCH_FAILED                     = B_APP_ERROR_BASE + 5;
  B_AMBIGUOUS_APP_LAUNCH              = B_APP_ERROR_BASE + 6;
  B_UNKNOWN_MIME_TYPE                 = B_APP_ERROR_BASE + 7;
  B_BAD_SCRIPT_SYNTAX                 = B_APP_ERROR_BASE + 8;
  B_LAUNCH_FAILED_NO_RESOLVE_LINK     = B_APP_ERROR_BASE + 9;
  B_LAUNCH_FAILED_EXECUTABLE          = B_APP_ERROR_BASE + 10;
  B_LAUNCH_FAILED_APP_NOT_FOUND       = B_APP_ERROR_BASE + 11;
  B_LAUNCH_FAILED_APP_IN_TRASH        = B_APP_ERROR_BASE + 12;
  B_LAUNCH_FAILED_NO_PREFERRED_APP    = B_APP_ERROR_BASE + 13;
  B_LAUNCH_FAILED_FILES_APP_NOT_FOUND = B_APP_ERROR_BASE + 14;
  B_BAD_MIME_SNIFFER_RULE             = B_APP_ERROR_BASE + 15;

//----- Storage Kit/File System Errors ------------------------
  B_FILE_ERROR          = B_STORAGE_ERROR_BASE;
  B_FILE_NOT_FOUND      = B_STORAGE_ERROR_BASE + 1; // discouraged; use B_ENTRY_NOT_FOUND in new code
  B_FILE_EXISTS         = B_STORAGE_ERROR_BASE + 2;
  B_ENTRY_NOT_FOUND     = B_STORAGE_ERROR_BASE + 3;
  B_NAME_TOO_LONG       = B_STORAGE_ERROR_BASE + 4;
  B_NOT_A_DIRECTORY     = B_STORAGE_ERROR_BASE + 5;
  B_DIRECTORY_NOT_EMPTY = B_STORAGE_ERROR_BASE + 6;
  B_DEVICE_FULL         = B_STORAGE_ERROR_BASE + 7;
  B_READ_ONLY_DEVICE    = B_STORAGE_ERROR_BASE + 8;
  B_IS_A_DIRECTORY      = B_STORAGE_ERROR_BASE + 9;
  B_NO_MORE_FDS         = B_STORAGE_ERROR_BASE + 10;
  B_CROSS_DEVICE_LINK   = B_STORAGE_ERROR_BASE + 11;
  B_LINK_LIMIT          = B_STORAGE_ERROR_BASE + 12;
  B_BUSTED_PIPE         = B_STORAGE_ERROR_BASE + 13;
  B_UNSUPPORTED         = B_STORAGE_ERROR_BASE + 14;
  B_PARTITION_TOO_SMALL = B_STORAGE_ERROR_BASE + 15;

//----- POSIX Errors ------------------------------------------
  E2BIG           = B_POSIX_ERROR_BASE + 1;
  ECHILD          = B_POSIX_ERROR_BASE + 2;
  EDEADLK         = B_POSIX_ERROR_BASE + 3;
  EFBIG           = B_POSIX_ERROR_BASE + 4;
  EMLINK          = B_POSIX_ERROR_BASE + 5;
  ENFILE          = B_POSIX_ERROR_BASE + 6;
  ENODEV          = B_POSIX_ERROR_BASE + 7;
  ENOLCK          = B_POSIX_ERROR_BASE + 8;
  ENOSYS          = B_POSIX_ERROR_BASE + 9;
  ENOTTY          = B_POSIX_ERROR_BASE + 10;
  ENXIO           = B_POSIX_ERROR_BASE + 11;
  ESPIPE          = B_POSIX_ERROR_BASE + 12;
  ESRCH           = B_POSIX_ERROR_BASE + 13;
  EFPOS           = B_POSIX_ERROR_BASE + 14;
  ESIGPARM        = B_POSIX_ERROR_BASE + 15;
  EDOM            = B_POSIX_ERROR_BASE + 16;
  ERANGE          = B_POSIX_ERROR_BASE + 17;
  EPROTOTYPE      = B_POSIX_ERROR_BASE + 18;
  EPROTONOSUPPORT = B_POSIX_ERROR_BASE + 19;
  EPFNOSUPPORT    = B_POSIX_ERROR_BASE + 20;
  EAFNOSUPPORT    = B_POSIX_ERROR_BASE + 21;
  EADDRINUSE      = B_POSIX_ERROR_BASE + 22;
  EADDRNOTAVAIL   = B_POSIX_ERROR_BASE + 23;
  ENETDOWN        = B_POSIX_ERROR_BASE + 24;
  ENETUNREACH     = B_POSIX_ERROR_BASE + 25;
  ENETRESET       = B_POSIX_ERROR_BASE + 26;
  ECONNABORTED    = B_POSIX_ERROR_BASE + 27;
  ECONNRESET      = B_POSIX_ERROR_BASE + 28;
  EISCONN         = B_POSIX_ERROR_BASE + 29;
  ENOTCONN        = B_POSIX_ERROR_BASE + 30;
  ESHUTDOWN       = B_POSIX_ERROR_BASE + 31;
  ECONNREFUSED    = B_POSIX_ERROR_BASE + 32;
  EHOSTUNREACH    = B_POSIX_ERROR_BASE + 33;
  ENOPROTOOPT     = B_POSIX_ERROR_BASE + 34;
  ENOBUFS         = B_POSIX_ERROR_BASE + 35;
  EINPROGRESS     = B_POSIX_ERROR_BASE + 36;
  EALREADY        = B_POSIX_ERROR_BASE + 37;
  EILSEQ          = B_POSIX_ERROR_BASE + 38;
  ENOMSG          = B_POSIX_ERROR_BASE + 39;
  ESTALE          = B_POSIX_ERROR_BASE + 40;
  EOVERFLOW       = B_POSIX_ERROR_BASE + 41;
  EMSGSIZE        = B_POSIX_ERROR_BASE + 42;
  EOPNOTSUPP      = B_POSIX_ERROR_BASE + 43;                      
  ENOTSOCK        = B_POSIX_ERROR_BASE + 44;

  ENOMEM       = B_NO_MEMORY;
  EACCES       = B_PERMISSION_DENIED;
  EINTR        = B_INTERRUPTED;
  EIO          = B_IO_ERROR;
  EBUSY        = B_BUSY;
  EFAULT       = B_BAD_ADDRESS;
  ETIMEDOUT    = B_TIMED_OUT;
  EAGAIN       = B_WOULD_BLOCK; // SysV compatibility
  EWOULDBLOCK  = B_WOULD_BLOCK; // BSD compatibility
  EBADF        = B_FILE_ERROR;
  EEXIST       = B_FILE_EXISTS;
  EINVAL       = B_BAD_VALUE;
  ENAMETOOLONG = B_NAME_TOO_LONG;
  ENOENT       = B_ENTRY_NOT_FOUND;
  EPERM        = B_NOT_ALLOWED;
  ENOTDIR      = B_NOT_A_DIRECTORY;
  EISDIR       = B_IS_A_DIRECTORY;
  ENOTEMPTY    = B_DIRECTORY_NOT_EMPTY;
  ENOSPC       = B_DEVICE_FULL;
  EROFS        = B_READ_ONLY_DEVICE;
  EMFILE       = B_NO_MORE_FDS;
  EXDEV        = B_CROSS_DEVICE_LINK;
  ELOOP        = B_LINK_LIMIT;
  ENOEXEC      = B_NOT_AN_EXECUTABLE;
  EPIPE        = B_BUSTED_PIPE;

//----- Media Kit Errors --------------------------------------
  B_STREAM_NOT_FOUND       = B_MEDIA_ERROR_BASE;
  B_SERVER_NOT_FOUND       = B_MEDIA_ERROR_BASE + 1;
  B_RESOURCE_NOT_FOUND     = B_MEDIA_ERROR_BASE + 2;
  B_RESOURCE_UNAVAILABLE   = B_MEDIA_ERROR_BASE + 3;
  B_BAD_SUBSCRIBER         = B_MEDIA_ERROR_BASE + 4;
  B_SUBSCRIBER_NOT_ENTERED = B_MEDIA_ERROR_BASE + 5;
  B_BUFFER_NOT_AVAILABLE   = B_MEDIA_ERROR_BASE + 6;
  B_LAST_BUFFER_ERROR      = B_MEDIA_ERROR_BASE + 7;

//----- Mail Kit Errors ---------------------------------------
  B_MAIL_NO_DAEMON      = B_MAIL_ERROR_BASE;
  B_MAIL_UNKNOWN_USER   = B_MAIL_ERROR_BASE + 1;
  B_MAIL_WRONG_PASSWORD = B_MAIL_ERROR_BASE + 2;
  B_MAIL_UNKNOWN_HOST   = B_MAIL_ERROR_BASE + 3;
  B_MAIL_ACCESS_ERROR   = B_MAIL_ERROR_BASE + 4;
  B_MAIL_UNKNOWN_FIELD  = B_MAIL_ERROR_BASE + 5;
  B_MAIL_NO_RECIPIENT   = B_MAIL_ERROR_BASE + 6;
  B_MAIL_INVALID_MAIL   = B_MAIL_ERROR_BASE + 7;

//----- Printing Errors --------------------------------------
  B_NO_PRINT_SERVER = B_PRINT_ERROR_BASE;

//----- Device Kit Errors -------------------------------------
  B_DEV_INVALID_IOCTL          = B_DEVICE_ERROR_BASE;
  B_DEV_NO_MEMORY              = B_DEVICE_ERROR_BASE + 1;
  B_DEV_BAD_DRIVE_NUM          = B_DEVICE_ERROR_BASE + 2;
  B_DEV_NO_MEDIA               = B_DEVICE_ERROR_BASE + 3;
  B_DEV_UNREADABLE             = B_DEVICE_ERROR_BASE + 4;
  B_DEV_FORMAT_ERROR           = B_DEVICE_ERROR_BASE + 5;
  B_DEV_TIMEOUT                = B_DEVICE_ERROR_BASE + 6;
  B_DEV_RECALIBRATE_ERROR      = B_DEVICE_ERROR_BASE + 7;
  B_DEV_SEEK_ERROR             = B_DEVICE_ERROR_BASE + 8;
  B_DEV_ID_ERROR               = B_DEVICE_ERROR_BASE + 9;
  B_DEV_READ_ERROR             = B_DEVICE_ERROR_BASE + 10;
  B_DEV_WRITE_ERROR            = B_DEVICE_ERROR_BASE + 11;
  B_DEV_NOT_READY              = B_DEVICE_ERROR_BASE + 12;
  B_DEV_MEDIA_CHANGED          = B_DEVICE_ERROR_BASE + 13;
  B_DEV_MEDIA_CHANGE_REQUESTED = B_DEVICE_ERROR_BASE + 14;
  B_DEV_RESOURCE_CONFLICT      = B_DEVICE_ERROR_BASE + 15;
  B_DEV_CONFIGURATION_ERROR    = B_DEVICE_ERROR_BASE + 16;
  B_DEV_DISABLED_BY_USER       = B_DEVICE_ERROR_BASE + 17;
  B_DEV_DOOR_OPEN              = B_DEVICE_ERROR_BASE + 18;

//-------------------------------------------------------------
(*
{----- Error baselines ---------------------------------------}

    B_GENERAL_ERROR_BASE        =   -2147483647-1;
    B_OS_ERROR_BASE             =   B_GENERAL_ERROR_BASE + $1000;
    B_APP_ERROR_BASE            =   B_GENERAL_ERROR_BASE + $2000;
    B_INTERFACE_ERROR_BASE      =   B_GENERAL_ERROR_BASE + $3000;
    B_MEDIA_ERROR_BASE          =   B_GENERAL_ERROR_BASE + $4000; {* - $41ff *}
    B_TRANSLATION_ERROR_BASE    =   B_GENERAL_ERROR_BASE + $4800; {* - $48ff *}
    B_MIDI_ERROR_BASE           =   B_GENERAL_ERROR_BASE + $5000;
    B_STORAGE_ERROR_BASE        =   B_GENERAL_ERROR_BASE + $6000;
    B_POSIX_ERROR_BASE          =   B_GENERAL_ERROR_BASE + $7000;
    B_MAIL_ERROR_BASE           =   B_GENERAL_ERROR_BASE + $8000;
    B_PRINT_ERROR_BASE          =   B_GENERAL_ERROR_BASE + $9000;
    B_DEVICE_ERROR_BASE         =   B_GENERAL_ERROR_BASE + $a000;

{--- Developer-defined errors start at (B_ERRORS_END+1)----}

    B_ERRORS_END        =       (B_GENERAL_ERROR_BASE + $ffff);

type
{----- General Errors ----------------------------------------}
tgeneralerrors=  (
	B_NO_MEMORY := B_GENERAL_ERROR_BASE,
	B_IO_ERROR,
	B_PERMISSION_DENIED,
	B_BAD_INDEX,
	B_BAD_TYPE,
	B_BAD_VALUE,
	B_MISMATCHED_VALUES,
	B_NAME_NOT_FOUND,
	B_NAME_IN_USE,
	B_TIMED_OUT,
    B_INTERRUPTED,
	B_WOULD_BLOCK,
    B_CANCELED,
	B_NO_INIT,
	B_BUSY,
	B_NOT_ALLOWED,

	B_ERROR := -1,
	B_OK := 0,
	B_NO_ERROR := 0
);

{----- Kernel Kit Errors -------------------------------------}
tkernelerror  = (
	B_BAD_SEM_ID := B_OS_ERROR_BASE,
	B_NO_MORE_SEMS,

	B_BAD_THREAD_ID := B_OS_ERROR_BASE + $100,
	B_NO_MORE_THREADS,
	B_BAD_THREAD_STATE,
	B_BAD_TEAM_ID,
	B_NO_MORE_TEAMS,

	B_BAD_PORT_ID := B_OS_ERROR_BASE + $200,
	B_NO_MORE_PORTS,

	B_BAD_IMAGE_ID := B_OS_ERROR_BASE + $300,
	B_BAD_ADDRESS,
	B_NOT_AN_EXECUTABLE,
	B_MISSING_LIBRARY,
	B_MISSING_SYMBOL,

	B_DEBUGGER_ALREADY_INSTALLED := B_OS_ERROR_BASE + $400
);


{----- Application Kit Errors --------------------------------}
tapperrors =
(
	B_BAD_REPLY := B_APP_ERROR_BASE,
	B_DUPLICATE_REPLY,
	B_MESSAGE_TO_SELF,
	B_BAD_HANDLER,
	B_ALREADY_RUNNING,
	B_LAUNCH_FAILED,
	B_AMBIGUOUS_APP_LAUNCH,
	B_UNKNOWN_MIME_TYPE,
	B_BAD_SCRIPT_SYNTAX,
	B_LAUNCH_FAILED_NO_RESOLVE_LINK,
	B_LAUNCH_FAILED_EXECUTABLE,
	B_LAUNCH_FAILED_APP_NOT_FOUND,
	B_LAUNCH_FAILED_APP_IN_TRASH,
	B_LAUNCH_FAILED_NO_PREFERRED_APP,
	B_LAUNCH_FAILED_FILES_APP_NOT_FOUND
);


{----- Storage Kit/File System Errors ------------------------}
tfserrors= (
	B_FILE_ERROR :=B_STORAGE_ERROR_BASE,
	B_FILE_NOT_FOUND,       { discouraged; use B_ENTRY_NOT_FOUND in new code }
	B_FILE_EXISTS,
	B_ENTRY_NOT_FOUND,
	B_NAME_TOO_LONG,
	B_NOT_A_DIRECTORY,
	B_DIRECTORY_NOT_EMPTY,
	B_DEVICE_FULL,
	B_READ_ONLY_DEVICE,
	B_IS_A_DIRECTORY,
	B_NO_MORE_FDS,
	B_CROSS_DEVICE_LINK,
	B_LINK_LIMIT,
	B_BUSTED_PIPE,
	B_UNSUPPORTED,
	B_PARTITION_TOO_SMALL
);

*)
const

{***********************************************************************}
{                       POSIX ERROR DEFINITIONS                         }
{***********************************************************************}

    { The following constants are system dependent but must all exist }
    ESysE2BIG       = (B_POSIX_ERROR_BASE + 1);
    ESysEACCES      = ord(B_PERMISSION_DENIED);
    ESysEAGAIN      = ord(B_WOULD_BLOCK);
    ESysEBADF       = ord(B_FILE_ERROR);
    ESysEBUSY       = ord(B_BUSY);
    ESysECHILD      = (B_POSIX_ERROR_BASE + 2);
    ESysEDEADLK     = (B_POSIX_ERROR_BASE + 3);
    ESysEDOM        = (B_POSIX_ERROR_BASE + 16);
    ESysEEXIST      = ord(B_FILE_EXISTS);
    ESysEFAULT      = ord(B_BAD_ADDRESS);
    ESysEFBIG       = (B_POSIX_ERROR_BASE + 4);
    ESysEINTR       = ord(B_INTERRUPTED);
    ESysEINVAL      = ord(B_BAD_VALUE);
    ESysEIO         = ord(B_IO_ERROR);
    ESysEISDIR      = ord(B_IS_A_DIRECTORY);
    ESysEMFILE      = ord(B_NO_MORE_FDS);
    ESysEMLINK      = (B_POSIX_ERROR_BASE + 5);
    ESysENAMETOOLONG= ord(B_NAME_TOO_LONG);
    ESysENFILE      = (B_POSIX_ERROR_BASE + 6);
    ESysENODEV      = (B_POSIX_ERROR_BASE + 7);
    ESysENOENT      = ord(B_ENTRY_NOT_FOUND);
    ESysENOEXEC     = ord(B_NOT_AN_EXECUTABLE);
    ESysENOLCK      = (B_POSIX_ERROR_BASE + 8);
    ESysENOMEM      = ord(B_NO_MEMORY);
    ESysENOSPC      = ord(B_DEVICE_FULL);
    ESysENOSYS      = (B_POSIX_ERROR_BASE + 9);
    ESysENOTDIR     = ord(B_NOT_A_DIRECTORY);
    ESysENOTEMPTY   = ord(B_DIRECTORY_NOT_EMPTY);
    ESysENOTTY      = (B_POSIX_ERROR_BASE + 10);
    ESysENXIO       = (B_POSIX_ERROR_BASE + 11);
    ESysEPERM       = ord(B_NOT_ALLOWED);
    ESysEPIPE       = ord(B_BUSTED_PIPE);
    ESysERANGE      = (B_POSIX_ERROR_BASE + 17);
    ESysEROFS       = ord(B_READ_ONLY_DEVICE);
    ESysESPIPE      = (B_POSIX_ERROR_BASE + 12);
    ESysESRCH       = (B_POSIX_ERROR_BASE + 13);
    ESysETIMEDOUT   = ord(B_TIMED_OUT);
    ESysEXDEV       = ord(B_CROSS_DEVICE_LINK);
    ESysEMSGSIZE     = EMSGSIZE;
    ESysEOPNOTSUPP   = EOPNOTSUPP;

    {ESysEBADMSG     =    realtime extension POSIX only   }
    {ESysECANCELED   =    async. I/O extension POSIX only }
    {ESysEINPROGRESS =    async. I/O extension POSIX only }

{***********************************************************************}
{                   NON POSIX ERROR DEFINITIONS                         }
{***********************************************************************}
     EsysEFPOS           = (B_POSIX_ERROR_BASE + 14);
     EsysESIGPARM        = (B_POSIX_ERROR_BASE + 15);
     EsysEPROTOTYPE      = (B_POSIX_ERROR_BASE + 18);
     EsysEPROTONOSUPPORT = (B_POSIX_ERROR_BASE + 19);
     EsysEPFNOSUPPORT    = (B_POSIX_ERROR_BASE + 20);
     EsysEAFNOSUPPORT    = (B_POSIX_ERROR_BASE + 21);
     EsysEADDRINUSE      = (B_POSIX_ERROR_BASE + 22);
     EsysEADDRNOTAVAIL   = (B_POSIX_ERROR_BASE + 23);
     EsysENETDOWN        = (B_POSIX_ERROR_BASE + 24);
     EsysENETUNREACH     = (B_POSIX_ERROR_BASE + 25);
     EsysENETRESET       = (B_POSIX_ERROR_BASE + 26);
     EsysECONNABORTED    = (B_POSIX_ERROR_BASE + 27);
     EsysECONNRESET       = (B_POSIX_ERROR_BASE + 28);

     EsysEISCONN      = (B_POSIX_ERROR_BASE + 29);
     EsysENOTCONN     = (B_POSIX_ERROR_BASE + 30);
     EsysESHUTDOWN    = (B_POSIX_ERROR_BASE + 31);
     EsysECONNREFUSED = (B_POSIX_ERROR_BASE + 32);
     EsysEHOSTUNREACH = (B_POSIX_ERROR_BASE + 33);
     EsysENOPROTOOPT  = (B_POSIX_ERROR_BASE + 34);
     EsysENOBUFS      = (B_POSIX_ERROR_BASE + 35);
     EsysEINPROGRESS  = (B_POSIX_ERROR_BASE + 36);
     EsysEALREADY     = (B_POSIX_ERROR_BASE + 37);

     EsysEWOULDBLOCK  = ord(B_WOULD_BLOCK);  {* BSD compatibility *}
     EsysELOOP        = ord(B_LINK_LIMIT);

