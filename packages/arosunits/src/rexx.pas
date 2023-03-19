{
    This file is part of the Free Pascal run time library.

    A file in AROS system run time library.
    Copyright (c) 1998-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$PACKRECORDS 2}

unit rexx;

interface


uses
  exec;

{ === rexx/storage.h ==================================================
 *
 * Copyright (c) 1986, 1987 by William S. Hawes (All Rights Reserved)
 *
 * =====================================================================
 * Header file to define ARexx data structures.
}


{ The NexxStr structure is used to maintain the internal strings in REXX.
 * It includes the buffer area for the string and associated attributes.
 * This is actually a variable-length structure; it is allocated for a
 * specific length string, and the length is never modified thereafter
 * (since it's used for recycling).}
type
  PNexxStr = ^TNexxStr;
  TNexxStr = record
    ns_Ivalue: LongInt;  // integer value
    ns_Length: Word;     // length in bytes (excl null)
    ns_Flags: Byte;      // attribute flags
    ns_Hash: Byte;       // hash code
    ns_Buff: array [0..7] of Byte; // buffer area for strings
  end; // size: 16 bytes (minimum)
const
  NXADDLEN = 9; // offset plus null byte
// String attribute flag bit definitions
  NSB_KEEP    = 0; // permanent string?
  NSB_STRING  = 1; // string form valid?
  NSB_NOTNUM  = 2; // non-numeric?
  NSB_NUMBER  = 3; // a valid number?
  NSB_BINARY  = 4; // integer value saved?
  NSB_FLOAT   = 5; // floating point format?
  NSB_EXT     = 6; // an external string?
  NSB_SOURCE  = 7; // part of the program source?
// The flag form of the string attributes
  NSF_KEEP    = 1;
  NSF_STRING  = 2;
  NSF_NOTNUM  = 4;
  NSF_NUMBER  = 8;
  NSF_BINARY  = 16;
  NSF_FLOAT   = 32;
  NSF_EXT     = 64;
  NSF_SOURCE  = 128;
// Combinations of flags
  NSF_INTNUM = NSF_NUMBER or NSF_BINARY or NSF_STRING;
  NSF_DPNUM = NSF_NUMBER or NSF_FLOAT;
  NSF_ALPHA = NSF_NOTNUM or NSF_STRING;
  NSF_OWNED = NSF_SOURCE or NSF_EXT or NSF_KEEP;
  KEEPSTR = NSF_STRING or NSF_SOURCE or NSF_NOTNUM;
  KEEPNUM = NSF_STRING or NSF_SOURCE or NSF_NUMBER or NSF_BINARY;

{ The RexxArg structure is identical to the NexxStr structure, but
  is allocated from system memory rather than from internal storage.
  This structure is used for passing arguments to external programs.
  It is usually passed as an "argstring", a pointer to the string buffer.}
type
  PRexxArg = ^TRexxArg;
  TRexxArg = record
    ra_Size: LongInt;  // total allocated length
    ra_Length: Word;   // length of string
    ra_Flags: Byte;    // attribute flags
    ra_Hash: Byte;     // hash code
    ra_Buff: array [0..7] of Byte; // buffer area
  end; // size: 16 bytes (minimum)

{ The RexxMsg structure is used for all communications with REXX
  programs.  It is an EXEC message with a parameter block appended.}
  PRexxMsg = ^TRexxMsg;
  TRexxMsg = record
    rm_Node: TMessage;     // EXEC message structure
    rm_TaskBlock: Pointer; // global structure (private)
    rm_LibBase: Pointer;   // library base (private)
    rm_Action: Longint;    // command (action) code
    rm_Result1: Longint;   // primary result (return code)
    rm_Result2: Longint;   // secondary result
    rm_Args: array [0..15] of STRPTR; // argument block (ARG0-ARG15)
    rm_PassPort: PMsgPort; // forwarding port
    rm_CommAddr: STRPTR;   // host address (port name)
    rm_FileExt: STRPTR;    // file extension
    rm_Stdin: BPTR;        // input stream (filehandle)
    rm_Stdout: BPTR;       // output stream (filehandle)
    rm_avail: Longint;     // future expansion
    end; // size: 128 bytes

const
  MAXRMARG = 15; // maximum arguments
// Command (action) codes for message packets
  RXCOMM   = $01000000; // a command-level invocation
  RXFUNC   = $02000000; // a function call
  RXCLOSE  = $03000000; // close the REXX server
  RXQUERY  = $04000000; // query for information
  RXADDFH  = $07000000; // add a function host
  RXADDLIB = $08000000; // add a function library
  RXREMLIB = $09000000; // remove a function library
  RXADDCON = $0A000000; // add/update a ClipList string
  RXREMCON = $0B000000; // remove a ClipList string
  RXTCOPN  = $0C000000; // open the trace console
  RXTCCLS  = $0D000000; // close the trace console
// Command modifier flag bits
  RXFB_NOIO   = 16; // suppress I/O inheritance?
  RXFB_RESULT = 17; // result string expected?
  RXFB_STRING = 18; // program is a "string file"?
  RXFB_TOKEN  = 19; // tokenize the command line?
  RXFB_NONRET = 20; // a "no-return" message?
// The flag form of the command modifiers
  RXFF_NOIO   = $00010000;
  RXFF_RESULT = $00020000;
  RXFF_STRING = $00040000;
  RXFF_TOKEN  = $00080000;
  RXFF_NONRET = $00100000;

  RXCODEMASK  = $FF000000;
  RXARGMASK   = $0000000F;

{ The RexxRsrc structure is used to manage global resources.  Each node
  has a name string created as a RexxArg structure, and the total size
  of the node is saved in the "rr_Size" field.  The REXX systems library
  provides functions to allocate and release resource nodes.  If special
  deletion operations are required, an offset and base can be provided in
  "rr_Func" and "rr_Base", respectively.  This "autodelete" function will
  be called with the base in register A6 and the node in A0.}
type
  PRexxRsrc = ^TRexxRsrc;
  TRexxRsrc = record
    rr_Node: TNode;
    rr_Func: SmallInt; // "auto-delete" offset
    rr_Base: Pointer;  // "auto-delete" base
    rr_Size: Longint;  // total size of node
    rr_Arg1: Longint;  // available ...
    rr_Arg2: Longint;  // available ...
  end; // size: 32 bytes

const
// Resource node types
  RRT_ANY  = 0; // any node type ...
  RRT_LIB  = 1; // a function library
  RRT_PORT = 2; // a public port
  RRT_FILE = 3; // a file IoBuff
  RRT_HOST = 4; // a function host
  RRT_CLIP = 5; // a Clip List node

{ The RexxTask structure holds the fields used by REXX to communicate with
  external processes, including the client task.  It includes the global
  data structure (and the base environment).  The structure is passed to
  the newly-created task in its "wake-up" message.}
  GLOBALSZ = 200; // total size of GlobalData

type
  PRexxTask = ^TRexxTask;
  TRexxTask = record
    rt_Global: array [0..GLOBALSZ - 1] of Byte;
              // global data structure
    rt_MsgPort: TMsgPort; // global message port
    rt_Flags: Byte;      // task flag bits                }
    rt_SigBit: Shortint; // signal bit                    }

    rt_ClientID: Pointer; // the client's task ID          }
    rt_MsgPkt: Pointer;   // the packet being processed    }
    rt_TaskID: Pointer;   // our task ID                   }
    rt_RexxPort: Pointer; // the REXX public port          }

    rt_ErrTrap: Pointer;  // Error trap address            }
    rt_StackPtr: Pointer; // stack pointer for traps       }

    rt_Header1: TList; // Environment list              }
    rt_Header2: TList; // Memory freelist               }
    rt_Header3: TList; // Memory allocation list        }
    rt_Header4: TList; // Files list                    }
    rt_Header5: TList; // Message Ports List            }
    end;

const
// Definitions for RexxTask flag bits
  RTFB_TRACE = 0; // external trace flag
  RTFB_HALT  = 1; // external halt flag
  RTFB_SUSP  = 2; // suspend task?
  RTFB_TCUSE = 3; // trace console in use?
  RTFB_WAIT  = 6; // waiting for reply?
  RTFB_CLOSE = 7; // task completed?
// Definitions for memory allocation constants
  MEMQUANT = 16;        // quantum of memory space
  MEMMASK  = $FFFFFFF0; // mask for rounding the size

  MEMQUICK = 1;         // EXEC flags: MEMF_PUBLIC
  MEMCLEAR = $00010000; // EXEC flags: MEMF_CLEAR
{ The SrcNode is a temporary structure used to hold values destined for
  a segment array.  It is also used to maintain the memory freelist.}
type
  PSrcNode = ^TSrcNode;
  TSrcNode = record
    sn_Succ: PSrcNode; // next node
    sn_Pred: PSrcNode; // previous node
    sn_Ptr: Pointer;   // pointer value
    sn_Size: LongInt;  // size of object
    end; // size: 16 bytes

{ === rexx/rexxio.h ====================================================
 *
 * Copyright (c) 1986, 1987 by William S. Hawes.  All Rights Reserved.
 *
 * ======================================================================
 * Header file for ARexx Input/Output related structures}
const
  RXBUFFSZ = 204; // buffer length

{ The IoBuff is a resource node used to maintain the File List.  Nodes
  are allocated and linked into the list whenever a file is opened.}
type
  PIoBuff = ^TIoBuff;
  TIoBuff = record
    iobNode: tRexxRsrc; // structure for files/strings
    iobRpt: Pointer;    // read/write pointer
    iobRct: Longint;    // character count
    iobDFH: Longint;    // DOS filehandle
    iobLock: Longint;   // DOS lock
    iobBct: Longint;    // buffer length
    iobArea: array [0..RXBUFFSZ - 1] of Byte; // buffer area
  end; // size: 256 bytes

const
// Access mode definitions
  RXIO_EXIST  = -1; // an external filehandle
  RXIO_STRF   = 0;  // a "string file"
  RXIO_READ   = 1;  // read-only access
  RXIO_WRITE  = 2;  // write mode
  RXIO_APPEND = 3;  // append mode (existing file)
// Offset anchors for SeekF()
  RXIO_BEGIN = -1; // relative to start
  RXIO_CURR  = 0;  // relative to current position
  RXIO_END   = 1;  // relative to end

{ A message port structure, maintained as a resource node.  The ReplyList
  holds packets that have been received but haven't been replied.}
type
  PRexxMsgPort = ^TRexxMsgPort;
  TRexxMsgPort = record
    rmp_Node: TRexxRsrc;  // linkage node
    rmp_Port: TMsgPort;   // the message port
    rmp_ReplyList: TList; // messages awaiting reply
    end;

const
// DOS Device types
  DT_DEV = 0; // a device
  DT_DIR = 1; // an ASSIGNed directory
  DT_VOL = 2; // a volume

// Private DOS packet types
  ACTION_STACK = 2002; // stack a line
  ACTION_QUEUE = 2003; // queue a line

{ === rexx/rxslib.h ===================================================
 *
 * Copyright (c) 1986, 1987, 1989 by William S. Hawes (All Rights Reserved)
 *
 * =====================================================================
 * The header file for the REXX Systems Library
}

// Some macro definitions

const
  RXSNAME: PChar = 'rexxsyslib.library';
  RXSID: PChar = 'rexxsyslib 1.06 (07 MAR 88)';
  RXSDIR: PChar = 'REXX';
  RXSTNAME: PChar = 'ARexx';

{ The REXX systems library structure.  This should be considered as
  semi-private and read-only, except for documented exceptions.}
type
  PRxsLib = ^TRxsLib;
  TRxsLib = record
    rl_Node: TLibrary;  // EXEC library node
    rl_Flags: Byte;     // global flags
    rl_pad: Byte;
    rl_SysBase: Pointer;  // EXEC library base
    rl_DOSBase: Pointer;  // DOS library base
    rl_IeeeDPBase: Pointer;  // IEEE DP math library base
    rl_SegList: BPTR;     // library seglist
    rl_NIL: Pointer;      // global NIL: filehandle
    rl_Chunk: Longint;    // allocation quantum
    rl_MaxNest: Longint;  // maximum expression nesting
    rl_NULL: PNexxStr;    // static string: NULL
    rl_FALSE: PNexxStr;   // static string: FALSE
    rl_TRUE: PNexxStr;    // static string: TRUE
    rl_REXX: PNexxStr;    // static string: REXX
    rl_COMMAND: PNexxStr; // static string: COMMAND
    rl_STDIN: PNexxStr;   // static string: STDIN
    rl_STDOUT: PNexxStr;  // static string: STDOUT
    rl_STDERR: PNexxStr;  // static string: STDERR
    rl_Version: STRPTR;   // version/configuration string

    rl_TaskName: STRPTR;   // name string for tasks
    rl_TaskPri: Longint;   // starting priority
    rl_TaskSeg: Longint;   // startup seglist
    rl_StackSize: Longint; //  stack size
    rl_RexxDir: STRPTR;    // REXX directory
    rl_CTABLE: STRPTR;     // character attribute table
    rl_Notice: STRPTR;     // copyright notice

    rl_RexxPort: TMsgPort; // REXX public port
    rl_ReadLock: Word;     // lock count
    rl_TraceFH: Longint;   // global trace console
    rl_TaskList: TList;    // REXX task list
    rl_NumTask: SmallInt;  // task count
    rl_LibList: TList;     // Library List header
    rl_NumLib: SmallInt;   // library count
    rl_ClipList: TList;    // ClipList header
    rl_NumClip: SmallInt;  // clip node count
    rl_MsgList: TList;     // pending messages
    rl_NumMsg: SmallInt;   // pending count
    rl_PgmList: TList;     // cached programs
    rl_NumPgm: SmallInt;   // program count

    rl_TraceCnt: Word;     // usage count for trace console
    rl_avail: SmallInt;
  end;

const
// Global flag bit definitions for RexxMaster
  RLFB_TRACE = RTFB_TRACE; // interactive tracing?
  RLFB_HALT  = RTFB_HALT;  // halt execution?
  RLFB_SUSP  = RTFB_SUSP;  // suspend execution?
  RLFB_STOP  = 6;          // deny further invocations
  RLFB_CLOSE = 7;          // close the master

  RLFMASK = (1 shl RLFB_TRACE) or (1 shl RLFB_HALT) or (1 shl RLFB_SUSP);

// Initialization constants
  RXSVERS  = 34;      // main version
  RXSREV   = 7;       // revision
  RXSALLOC = $800000; // maximum allocation
  RXSCHUNK = 1024;    // allocation quantum
  RXSNEST  = 32;      // expression nesting limit
  RXSTPRI  = 0;       // task priority
  RXSSTACK = 4096;    // stack size
  RXSLISTH = 5;       // number of list headers

// Character attribute flag bits used in REXX.
  CTB_SPACE   = 0; // white space characters
  CTB_DIGIT   = 1; // decimal digits 0-9
  CTB_ALPHA   = 2; // alphabetic characters
  CTB_REXXSYM = 3; // REXX symbol characters
  CTB_REXXOPR = 4; // REXX operator characters
  CTB_REXXSPC = 5; // REXX special symbols
  CTB_UPPER   = 6; // UPPERCASE alphabetic
  CTB_LOWER   = 7; // lowercase alphabetic

// Attribute flags
  CTF_SPACE   = 1;
  CTF_DIGIT   = 2;
  CTF_ALPHA   = 4;
  CTF_REXXSYM = 8;
  CTF_REXXOPR = 16;
  CTF_REXXSPC = 32;
  CTF_UPPER   = 64;
  CTF_LOWER   = 128;


var
  RexxSysBase: PLibrary = nil;

const
  REXXSYSLIBNAME: PChar = 'rexxsyslib.library';

procedure ClearRexxMsg(MsgPtr: PRexxMsg; Count: LongWord); syscall RexxSysBase 26;
function CreateArgstring(const ArgString: PChar; Length: LongWord): PChar; syscall RexxSysBase 21;
function CreateRexxMsg(const Port: PMsgPort; const Extension: PChar; Host: PChar): PRexxMsg; syscall RexxSysBase 24;
procedure DeleteArgstring(ArgString: PChar); syscall RexxSysBase 22;
procedure DeleteRexxMsg(Packet: PRexxMsg); syscall RexxSysBase 25;
function FillRexxMsg(MsgPtr: PRexxMsg; Count: LongWord; Mask: LongWord): LongBool; syscall RexxSysBase 27;
function IsRexxMsg(const MsgPtr: PRexxMsg): LongBool; syscall RexxSysBase 28;
function LengthArgstring(const ArgString: PChar): LongWord; syscall RexxSysBase 23;
procedure LockRexxBase(Resource: LongWord); syscall RexxSysBase 75;
procedure UnlockRexxBase(Resource: LongWord); syscall RexxSysBase 76;

implementation

const
  // Change VERSION and LIBVERSION to proper values
  VERSION: string[2] = '0';
  LIBVERSION: longword = 0;

initialization
  RexxSysBase := OpenLibrary(REXXSYSLIBNAME, LIBVERSION);
finalization
  if Assigned(RexxSysBase) then
    CloseLibrary(RexxSysBase);
end.



