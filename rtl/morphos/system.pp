{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Karoly Balogh for Genesi Sarl

    System unit for MorphOS/PowerPC
  
    Uses parts of the Amiga/68k port by Carl Eric Codere 
    and Nils Sjoholm

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit {$ifdef VER1_0}SysMorph{$else}System{$endif};

interface

{$define FPC_IS_SYSTEM}

{$I systemh.inc}

type 
  THandle = LongInt;

{$I heaph.inc}

const
  LineEnding = #10;
  LFNSupport = True;
  DirectorySeparator = '/';
  DriveSeparator = ':';
  PathSeparator = ';';

const
  UnusedHandle    : LongInt = -1;
  StdInputHandle  : LongInt = 0;
  StdOutputHandle : LongInt = 0;
  StdErrorHandle  : LongInt = 0;

  FileNameCaseSensitive : Boolean = False;

  sLineBreak : string[1] = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

  BreakOn : Boolean = True;


{*****************************************************************************
                           MorphOS structures
*****************************************************************************}

type
  TDateStamp = packed record
    ds_Days   : LongInt;      { Number of days since Jan. 1, 1978 }
    ds_Minute : LongInt;      { Number of minutes past midnight }
    ds_Tick   : LongInt;      { Number of ticks past minute }
  end;
  PDateStamp = ^TDateStamp;

  PFileInfoBlock = ^TFileInfoBlock;
  TFileInfoBlock = packed record
    fib_DiskKey      : LongInt;
    fib_DirEntryType : LongInt;
    { Type of Directory. If < 0, then a plain file. If > 0 a directory }
    fib_FileName     : Array [0..107] of Char;
    { Null terminated. Max 30 chars used for now }
    fib_Protection   : LongInt;
    { bit mask of protection, rwxd are 3-0. }
    fib_EntryType    : LongInt;
    fib_Size         : LongInt;      { Number of bytes in file }
    fib_NumBlocks    : LongInt;      { Number of blocks in file }
    fib_Date         : TDateStamp; { Date file last changed }
    fib_Comment      : Array [0..79] of Char;
    { Null terminated comment associated with file }
    fib_Reserved     : Array [0..35] of Char;
  end;

  PNode = ^TNode;
  TNode = packed record
    ln_Succ,             { Pointer to next (successor) }
    ln_Pred: pNode;      { Pointer to previous (predecessor) }
    ln_Type: Byte;
    ln_Pri : Shortint;   { Priority, for sorting }
    ln_Name: PChar;      { ID string, null terminated }
  end;  { Note: Integer aligned }
 
  PMinNode = ^TMinNode;
  tMinNode = packed record
    mln_Succ,
    mln_Pred: pMinNode;
  end;

  PList = ^TList;
  tList = packed record
    lh_Head    : pNode;
    lh_Tail    : pNode;
    lh_TailPred: pNode;
    lh_Type    : Byte;
    l_pad      : Byte;
  end;

  PMinList = ^TMinList;
  TMinList = packed record
    mlh_Head    : PMinNode;
    mlh_Tail    : PMinNode;
    mlh_TailPred: PMinNode;
  end;

  PMsgPort = ^TMsgPort;
  TMsgPort = packed record
    mp_Node   : TNode;
    mp_Flags  : Byte;
    mp_SigBit : Byte;     { signal bit number    }
    mp_SigTask: Pointer;  { task to be signalled (TaskPtr) }
    mp_MsgList: TList;    { message linked list  }
  end;

  PMessage = ^TMessage;
  TMessage = packed record
    mn_Node     : TNode;
    mn_ReplyPort: PMsgPort;
    mn_Length   : Word;
  end;

  PTask = ^TTask;
  TTask = packed record
    tc_Node      : TNode;
    tc_Flags     : Byte;
    tc_State     : Byte;
    tc_IDNestCnt : Shortint;  { intr disabled nesting         }
    tc_TDNestCnt : Shortint;  { task disabled nesting         }
    tc_SigAlloc  : DWord;     { sigs allocated                }
    tc_SigWait   : DWord;     { sigs we are waiting for       }
    tc_SigRecvd  : DWord;     { sigs we have received         }
    tc_SigExcept : DWord;     { sigs we will take excepts for }
    tc_TrapAlloc : Word;      { traps allocated               }
    tc_TrapAble  : Word;      { traps enabled                 }
    tc_ExceptData: Pointer;   { points to except data         }
    tc_ExceptCode: Pointer;   { points to except code         }
    tc_TrapData  : Pointer;   { points to trap data           }
    tc_TrapCode  : Pointer;   { points to trap code           }
    tc_SPReg     : Pointer;   { stack pointer                 }
    tc_SPLower   : Pointer;   { stack lower bound             }
    tc_SPUpper   : Pointer;   { stack upper bound + 2         }
    tc_Switch    : Pointer;   { task losing CPU               }
    tc_Launch    : Pointer;   { task getting CPU              }
    tc_MemEntry  : TList;     { allocated memory              }
    tc_UserData  : Pointer;   { per task data                 }
  end;

  PProcess = ^TProcess;
  TProcess = packed record
    pr_Task          : TTask;
    pr_MsgPort       : TMsgPort;  { This is BPTR address from DOS functions    }
    pr_Pad           : Word;      { Remaining variables on 4 byte boundaries   }
    pr_SegList       : Pointer;   { Array of seg lists used by this process    }
    pr_StackSize     : Longint;   { Size of process stack in bytes             }
    pr_GlobVec       : Pointer;   { Global vector for this process (BCPL)      }
    pr_TaskNum       : Longint;   { CLI task number of zero if not a CLI       }
    pr_StackBase     : DWord;     { Ptr to high memory end of process stack    }
    pr_Result2       : Longint;   { Value of secondary result from last call   }
    pr_CurrentDir    : DWord;     { Lock associated with current directory     }
    pr_CIS           : DWord;     { Current CLI Input Stream                   }
    pr_COS           : DWord;     { Current CLI Output Stream                  }
    pr_ConsoleTask   : Pointer;   { Console handler process for current window } 
    pr_FileSystemTask: Pointer;   { File handler process for current drive     }
    pr_CLI           : DWord;     { pointer to ConsoleLineInterpreter          }
    pr_ReturnAddr    : Pointer;   { pointer to previous stack frame            }
    pr_PktWait       : Pointer;   { Function to be called when awaiting msg    }
    pr_WindowPtr     : Pointer;   { Window for error printing }
    { following definitions are new with 2.0 }
    pr_HomeDir       : DWord;     { Home directory of executing program      }
    pr_Flags         : Longint;   { flags telling dos about process          }
    pr_ExitCode      : Pointer;   { code to call on exit of program OR NULL  }
    pr_ExitData      : Longint;   { Passed as an argument to pr_ExitCode.    }
    pr_Arguments     : PChar;     { Arguments passed to the process at start }
    pr_LocalVars     : TMinList;  { Local environment variables              }
    pr_ShellPrivate  : Longint;   { for the use of the current shell         }
    pr_CES           : DWord;     { Error stream - IF NULL, use pr_COS       }
  end;

var
  MOS_ExecBase: Pointer; external name '_ExecBase';
  MOS_DOSBase : Pointer;

  MOS_heapPool: Pointer; { pointer for the OS pool for growing the heap }
  MOS_origDir : LongInt; { original directory on startup }
  MOS_ambMsg  : PMessage; 
  MOS_ConName : PChar ='CON:10/30/620/100/FPC Console Output/AUTO/CLOSE/WAIT';
  

{*****************************************************************************
                           MorphOS functions
*****************************************************************************}

{ exec.library functions }

function exec_OpenLibrary(libname: PChar location 'a1'; 
                          libver: LongInt location 'd0'): Pointer; SysCall MOS_ExecBase 552;
procedure exec_CloseLibrary(libhandle: Pointer location 'a1'); SysCall MOS_ExecBase 414;

function exec_CreatePool(memflags: LongInt location 'd0'; 
                         puddleSize: LongInt location 'd1'; 
                         threshSize: LongInt location 'd2'): Pointer; SysCall MOS_ExecBase 696;
procedure exec_DeletePool(poolHeader: Pointer location 'a0'); SysCall MOS_ExecBase 702;
function exec_AllocPooled(poolHeader: Pointer location 'a0';
                          memSize: LongInt location 'd0'): Pointer; SysCall MOS_ExecBase 708;
function exec_SetSignal(newSignals: LongInt location 'd0';
                        signalMask: LongInt location 'd1'): LongInt; SysCall MOS_ExecBase 306;

function exec_FindTask(tname: PChar location 'a1'): PTask; SysCall MOS_ExecBase 294;
function exec_GetMsg(port: PMsgPort location 'a0'): PMessage; SysCall MOS_ExecBase 372;
function exec_WaitPort(port: PMsgPort location 'a0'): PMessage; SysCall MOS_ExecBase 384;

{ dos.library functions }

function dos_Output: LongInt; SysCall MOS_DOSBase 60;
function dos_Input: LongInt; SysCall MOS_DOSBase 54;
function dos_IoErr: LongInt; SysCall MOS_DOSBase 132;

function dos_Open(fname: PChar location 'd1';
                  accessMode: LongInt location 'd2'): LongInt; SysCall MOS_DOSBase 30;
function dos_Close(fileh: LongInt location 'd1'): Boolean; SysCall MOS_DOSBase 36;

function dos_Seek(fileh: LongInt location 'd1';
                  position: LongInt location 'd2';
                  posmode: LongInt location 'd3'): LongInt; SysCall MOS_DOSBase 66;
function dos_SetFileSize(fileh: LongInt location 'd1';
                         position: LongInt location 'd2';
                         posmode: LongInt location 'd3'): LongInt; SysCall MOS_DOSBase 456;

function dos_Read(fileh: LongInt location 'd1'; 
                  buffer: Pointer location 'd2'; 
                  length: LongInt location 'd3'): LongInt; SysCall MOS_DOSBase 40;
function dos_Write(fileh: LongInt location 'd1'; 
                   buffer: Pointer location 'd2'; 
                   length: LongInt location 'd3'): LongInt; SysCall MOS_DOSBase 48;
function dos_WriteChars(buf: PChar location 'd1'; 
                        buflen: LongInt location 'd2'): LongInt; SysCall MOS_DOSBase 942;

function dos_Rename(oldName: PChar location 'd1';
                    newName: PChar location 'd2'): Boolean; SysCall MOS_DOSBase 78;
function dos_DeleteFile(fname: PChar location 'd1'): Boolean; SysCall MOS_DOSBase 72;

function dos_GetCurrentDirName(buf: PChar location 'd1';
                               len: LongInt location 'd2'): Boolean; SysCall MOS_DOSBase 564;

function dos_Lock(lname: PChar location 'd1';
                  accessMode: LongInt location 'd2'): LongInt; SysCall MOS_DOSBase 84;
procedure dos_Unlock(lock: LongInt location 'd1'); SysCall MOS_DOSBase 90;
function dos_CurrentDir(lock: LongInt location 'd1'): LongInt; SysCall MOS_DOSBase 126;
function dos_Examine(lock: LongInt location 'd1';
                     FileInfoBlock: Pointer location 'd2'): Boolean; SysCall MOS_DOSBase 102;
function dos_CreateDir(dname: PChar location 'd1'): LongInt; SysCall MOS_DOSBase 120;
function dos_DateStamp(var ds: TDateStamp location 'd1'): LongInt; SysCall MOS_DOSBase 192;


implementation

{$I system.inc}

{*****************************************************************************
                    System Dependent Structures/Consts
*****************************************************************************}

{ Errors from dos_IoErr(), etc. }
const
  ERROR_NO_FREE_STORE              = 103;
  ERROR_TASK_TABLE_FULL            = 105;
  ERROR_BAD_TEMPLATE               = 114;
  ERROR_BAD_NUMBER                 = 115;
  ERROR_REQUIRED_ARG_MISSING       = 116;
  ERROR_KEY_NEEDS_ARG              = 117;
  ERROR_TOO_MANY_ARGS              = 118;
  ERROR_UNMATCHED_QUOTES           = 119;
  ERROR_LINE_TOO_LONG              = 120;
  ERROR_FILE_NOT_OBJECT            = 121;
  ERROR_INVALID_RESIDENT_LIBRARY   = 122;
  ERROR_NO_DEFAULT_DIR             = 201;
  ERROR_OBJECT_IN_USE              = 202;
  ERROR_OBJECT_EXISTS              = 203;
  ERROR_DIR_NOT_FOUND              = 204;
  ERROR_OBJECT_NOT_FOUND           = 205;
  ERROR_BAD_STREAM_NAME            = 206;
  ERROR_OBJECT_TOO_LARGE           = 207;
  ERROR_ACTION_NOT_KNOWN           = 209;
  ERROR_INVALID_COMPONENT_NAME     = 210;
  ERROR_INVALID_LOCK               = 211;
  ERROR_OBJECT_WRONG_TYPE          = 212;
  ERROR_DISK_NOT_VALIDATED         = 213;
  ERROR_DISK_WRITE_PROTECTED       = 214;
  ERROR_RENAME_ACROSS_DEVICES      = 215;
  ERROR_DIRECTORY_NOT_EMPTY        = 216;
  ERROR_TOO_MANY_LEVELS            = 217;
  ERROR_DEVICE_NOT_MOUNTED         = 218;
  ERROR_SEEK_ERROR                 = 219;
  ERROR_COMMENT_TOO_BIG            = 220;
  ERROR_DISK_FULL                  = 221;
  ERROR_DELETE_PROTECTED           = 222;
  ERROR_WRITE_PROTECTED            = 223;
  ERROR_READ_PROTECTED             = 224;
  ERROR_NOT_A_DOS_DISK             = 225;
  ERROR_NO_DISK                    = 226;
  ERROR_NO_MORE_ENTRIES            = 232;
  { added for AOS 1.4 }
  ERROR_IS_SOFT_LINK               = 233;
  ERROR_OBJECT_LINKED              = 234;
  ERROR_BAD_HUNK                   = 235;
  ERROR_NOT_IMPLEMENTED            = 236;
  ERROR_RECORD_NOT_LOCKED          = 240;
  ERROR_LOCK_COLLISION             = 241;
  ERROR_LOCK_TIMEOUT               = 242;
  ERROR_UNLOCK_ERROR               = 243;

{ DOS file offset modes }
const
  OFFSET_BEGINNING = -1;
  OFFSET_CURRENT   = 0;
  OFFSET_END       = 1;

{ Lock AccessMode }
const
  SHARED_LOCK      = -2;
  ACCESS_READ      = SHARED_LOCK;
  EXCLUSIVE_LOCK   = -1;
  ACCESS_WRITE     = EXCLUSIVE_LOCK;

{ Memory flags }
const
  MEMF_ANY      = 0;
  MEMF_PUBLIC   = 1 Shl 0;
  MEMF_CHIP     = 1 Shl 1;
  MEMF_FAST     = 1 Shl 2;
  MEMF_LOCAL    = 1 Shl 8;
  MEMF_24BITDMA = 1 Shl 9;
  MEMF_KICK     = 1 Shl 10;
  
  MEMF_CLEAR    = 1 Shl 16;
  MEMF_LARGEST  = 1 Shl 17;
  MEMF_REVERSE  = 1 Shl 18;
  MEMF_TOTAL    = 1 Shl 19;

  MEMF_NO_EXPUNGE = 1 Shl 31;

const
  CTRL_C           = 20;      { Error code on CTRL-C press }
  SIGBREAKF_CTRL_C = $1000;   { CTRL-C signal flags }


{*****************************************************************************
                  MorphOS File-handling Support Functions
*****************************************************************************}
type
  { AmigaOS does not automatically close opened files on exit back to  }
  { the operating system, therefore as a precuation we close all files }
  { manually on exit.                                                  }
  PFileList = ^TFileList;
  TFileList = record { no packed, must be correctly aligned }
    handle : LongInt;      { Handle to file    }
    next   : PFileList;      { Next file in list }
  end;

var
  MOS_fileList: PFileList; { List pointer to opened files }

{ Function to be called at program shutdown, to close all opened files }
procedure CloseList(l: PFileList);
var 
  tmpNext   : PFileList;
  tmpHandle : LongInt;
begin
  if l=nil then exit;

  { First, close all tracked files }
  tmpNext:=l^.next;
  while tmpNext<>nil do begin
    tmpHandle:=tmpNext^.handle;
    if (tmpHandle<>StdInputHandle) and (tmpHandle<>StdOutputHandle) 
       and (tmpHandle<>StdErrorHandle) then begin
      dos_Close(tmpHandle);
    end;
    tmpNext:=tmpNext^.next;
  end;
 
  { Next, erase the linked list }
  while l<>nil do begin
    tmpNext:=l;
    l:=l^.next;
    dispose(tmpNext);
  end;
end;

{ Function to be called to add a file to the opened file list }
procedure AddToList(var l: PFileList; h: LongInt);
var
  p     : PFileList;
  inList: Boolean;
begin
  inList:=False;
  if l<>nil then begin
    { if there is a valid filelist, search for the value }
    { in the list to avoid double additions }
    p:=l;
    while (p^.next<>nil) and (not inList) do
      if p^.next^.handle=h then inList:=True
                           else p:=p^.next;
    p:=nil;
  end else begin
    { if the list is not yet allocated, allocate it. }
    New(l);
    l^.next:=nil;
  end;

  if not inList then begin
    New(p);
    p^.handle:=h;
    p^.next:=l^.next;
    l^.next:=p;
  end;
end;

{ Function to be called to remove a file from the list }
procedure RemoveFromList(var l: PFileList; h: longint);
var
  p     : PFileList;
  inList: Boolean;
begin
  if l=nil then exit;

  inList:=False;
  p:=l;
  while (p^.next<>nil) and (not inList) do
    if p^.next^.handle=h then inList:=True
                         else p:=p^.next;

  if p^.next<>nil then begin
    dispose(p^.next);
    p^.next:=p^.next^.next;
  end;
end;


{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

procedure haltproc(e:longint);cdecl;external name '_haltproc';

procedure System_exit;
begin
  { We must remove the CTRL-C FALG here because halt }
  { may call I/O routines, which in turn might call  }
  { halt, so a recursive stack crash                 }
  if BreakOn then begin
    if (exec_SetSignal(0,0) and SIGBREAKF_CTRL_C)<>0 then
      exec_SetSignal(0,SIGBREAKF_CTRL_C);
  end;

  { Closing opened files }
  CloseList(MOS_fileList);

  if MOS_DOSBase<>nil then exec_CloseLibrary(MOS_DOSBase);
  if MOS_heapPool<>nil then exec_DeletePool(MOS_heapPool);
  haltproc(ExitCode);
end;

{ Converts a MorphOS dos.library error code to a TP compatible error code }
{ Based on 1.0.x Amiga RTL }
procedure dosError2InOut(errno: LongInt);
begin
  case errno of
    ERROR_BAD_NUMBER,
    ERROR_ACTION_NOT_KNOWN,
    ERROR_NOT_IMPLEMENTED : InOutRes := 1;

    ERROR_OBJECT_NOT_FOUND : InOutRes := 2;
    ERROR_DIR_NOT_FOUND :  InOutRes := 3;
    ERROR_DISK_WRITE_PROTECTED : InOutRes := 150;
    ERROR_OBJECT_WRONG_TYPE : InOutRes := 151;

    ERROR_OBJECT_EXISTS,
    ERROR_DELETE_PROTECTED,
    ERROR_WRITE_PROTECTED,
    ERROR_READ_PROTECTED,
    ERROR_OBJECT_IN_USE,
    ERROR_DIRECTORY_NOT_EMPTY : InOutRes := 5;

    ERROR_NO_MORE_ENTRIES : InOutRes := 18;
    ERROR_RENAME_ACROSS_DEVICES : InOutRes := 17;
    ERROR_DISK_FULL : InOutRes := 101;
    ERROR_INVALID_RESIDENT_LIBRARY : InoutRes := 153;
    ERROR_BAD_HUNK : InOutRes := 153;
    ERROR_NOT_A_DOS_DISK : InOutRes := 157;

    ERROR_NO_DISK,
    ERROR_DISK_NOT_VALIDATED,
    ERROR_DEVICE_NOT_MOUNTED : InOutRes := 152;

    ERROR_SEEK_ERROR : InOutRes := 156;

    ERROR_LOCK_COLLISION,
    ERROR_LOCK_TIMEOUT,
    ERROR_UNLOCK_ERROR,
    ERROR_INVALID_LOCK,
    ERROR_INVALID_COMPONENT_NAME,
    ERROR_BAD_STREAM_NAME,
    ERROR_FILE_NOT_OBJECT : InOutRes := 6;
   else
    InOutres := errno;
  end;
end;

{ Used for CTRL_C checking in I/O calls }
procedure checkCTRLC;
begin
  if BreakOn then begin
    if (exec_SetSignal(0,0) And SIGBREAKF_CTRL_C)<>0 then begin
      { Clear CTRL-C signal }
      exec_SetSignal(0,SIGBREAKF_CTRL_C);
      Halt(CTRL_C);
    end;
  end;
end;


{*****************************************************************************
                             ParamStr/Randomize
*****************************************************************************}

{ number of args }
function paramcount : longint;
begin
  {paramcount := argc - 1;}
  paramcount:=0;
end;

{ argument number l }
function paramstr(l : longint) : string;
begin
  {if (l>=0) and (l+1<=argc) then
   paramstr:=strpas(argv[l])
  else}
   paramstr:='';
end;

{ set randseed to a new pseudo random value }
procedure randomize;
var tmpTime: TDateStamp;
begin
  dos_DateStamp(tmpTime);
  randseed:=tmpTime.ds_tick;
end;


{*****************************************************************************
                              Heap Management
*****************************************************************************}

var
  int_heap     : LongInt; external name 'HEAP';
  int_heapsize : LongInt; external name 'HEAPSIZE';

{ first address of heap }
function getheapstart:pointer;
begin
  getheapstart:=@int_heap;
end;

{ current length of heap }
function getheapsize:longint;
begin
  getheapsize:=int_heapsize;
end;

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or nil if fail }
function Sbrk(size : longint):pointer;
begin
  Sbrk:=exec_AllocPooled(MOS_heapPool,size);
end;

{$I heap.inc}


{*****************************************************************************
                           Directory Handling
*****************************************************************************}
procedure mkdir(const s : string);[IOCheck];
var
  buffer : array[0..255] of char;
  j : Integer;
  tmpStr : string;
  tmpLock : LongInt;
begin
  checkCTRLC;
  if (s='') or (InOutRes<>0) then exit;
  tmpStr:=s;

  for j:=1 to length(tmpStr) do
    if tmpStr[j]='\' then tmpStr[j]:='/';
  move(tmpStr[1],buffer,length(tmpStr));
  buffer[length(tmpStr)]:=#0;

  tmpLock:=dos_CreateDir(buffer);
  if tmpLock=0 then begin
    dosError2InOut(dos_IoErr);
    exit;
  end;
  dos_UnLock(tmpLock);
end;

procedure rmdir(const s : string);[IOCheck];
var
  buffer : array[0..255] of char;
  j : Integer;
  tmpStr : string;
begin
  checkCTRLC;
  if (s='.') then InOutRes:=16;
  If (s='') or (InOutRes<>0) then exit;
  tmpStr:=s;
  for j:=1 to length(tmpStr) do
    if tmpStr[j] = '\' then tmpStr[j] := '/';
  move(tmpStr[1],buffer,length(tmpStr));
  buffer[length(tmpStr)]:=#0;
  if not dos_DeleteFile(buffer) then
    dosError2InOut(dos_IoErr);
end;

procedure chdir(const s : string);[IOCheck];
var
  buffer : array[0..255] of char;
  alock : LongInt;
  FIB : PFileInfoBlock;
  j : Integer;
  tmpStr : string;
begin
  checkCTRLC;
  If (s='') or (InOutRes<>0) then exit;
  tmpStr:=s;

  for j:=1 to length(tmpStr) do
    if tmpStr[j]='\' then tmpStr[j]:='/';

  { Return parent directory }
  if s='..' then begin
    getdir(0,tmpStr);
    j:=length(tmpStr);
    { Look through the previous paths }
    while (tmpStr[j]<>'/') and (tmpStr[j]<>':') and (j>0) do
      dec(j);
    if j>0 then
      tmpStr:=copy(tmpStr,1,j);
  end;
  alock:=0;

  move(tmpStr[1],buffer,length(tmpStr));
  buffer[length(tmpStr)]:=#0;
  { Changing the directory is a pretty complicated affair }
  {   1) Obtain a lock on the directory                   }
  {   2) CurrentDir the lock                              }
  alock:=dos_Lock(buffer,SHARED_LOCK);
  if alock=0 then begin
    dosError2InOut(dos_IoErr);
    exit;
  end;

  FIB:=nil;
  new(FIB);
 
  if (dos_Examine(alock,FIB)=True) and (FIB^.fib_DirEntryType>0) then begin
    alock := dos_CurrentDir(alock);
    if MOS_OrigDir=0 then begin
      MOS_OrigDir:=alock;
      alock:=0;
    end;
  end;

  if alock<>0 then dos_Unlock(alock);
  if assigned(FIB) then dispose(FIB)
end;

procedure GetDir (DriveNr: byte; var Dir: ShortString);
var tmpbuf: array[0..255] of char;
begin
  checkCTRLC;
  Dir:='';
  if not dos_GetCurrentDirName(tmpbuf,256) then
    dosError2InOut(dos_IoErr)
  else
    Dir:=strpas(tmpbuf);
end;


{****************************************************************************
                        Low level File Routines
               All these functions can set InOutRes on errors
****************************************************************************}

{ close a file from the handle value }
procedure do_close(handle : longint);
begin
  RemoveFromList(MOS_fileList,handle);
  { Do _NOT_ check CTRL_C on Close, because it will conflict 
    with System_Exit! }
  if not dos_Close(handle) then
    dosError2InOut(dos_IoErr);
end;

procedure do_erase(p : pchar);
begin
  checkCTRLC;
  if not dos_DeleteFile(p) then
    dosError2InOut(dos_IoErr);
end;

procedure do_rename(p1,p2 : pchar);
begin
  checkCTRLC;
  if not dos_Rename(p1,p2) then
    dosError2InOut(dos_IoErr);
end;

function do_write(h:longint; addr: pointer; len: longint) : longint;
var dosResult: LongInt;
begin
  checkCTRLC;
  do_write:=0; 
  if len<=0 then exit; 
  
  dosResult:=dos_Write(h,addr,len);
  if dosResult<0 then begin
    dosError2InOut(dos_IoErr);
  end else begin
    do_write:=dosResult;
  end;
end;

function do_read(h:longint; addr: pointer; len: longint) : longint;
var dosResult: LongInt;
begin
  checkCTRLC;
  do_read:=0; 
  if len<=0 then exit; 
  
  dosResult:=dos_Write(h,addr,len);
  if dosResult<0 then begin
    dosError2InOut(dos_IoErr);
  end else begin
    do_read:=dosResult;
  end
end;

function do_filepos(handle : longint) : longint;
var dosResult: LongInt;
begin
  checkCTRLC;
  do_filepos:=0;
  
  { Seeking zero from OFFSET_CURRENT to find out where we are }
  dosResult:=dos_Seek(handle,0,OFFSET_CURRENT);
  if dosResult<0 then begin
    dosError2InOut(dos_IoErr);
  end else begin
    do_filepos:=dosResult;
  end;
end;

procedure do_seek(handle,pos : longint);
begin
  checkCTRLC;
  { Seeking from OFFSET_BEGINNING }
  if dos_Seek(handle,pos,OFFSET_BEGINNING)<0 then
    dosError2InOut(dos_IoErr);
end;

function do_seekend(handle:longint):longint;
var dosResult: LongInt;
begin
  checkCTRLC;
  do_seekend:=0;
  
  { Seeking to OFFSET_END }
  dosResult:=dos_Seek(handle,0,OFFSET_END);
  if dosResult<0 then begin
    dosError2InOut(dos_IoErr);
  end else begin
    do_seekend:=dosResult;
  end
end;

function do_filesize(handle : longint) : longint;
var currfilepos: longint;
begin
  checkCTRLC;
  currfilepos:=do_filepos(handle);
  { We have to do this twice, because seek returns the OLD position }
  do_filesize:=do_seekend(handle);
  do_filesize:=do_seekend(handle);
  do_seek(handle,currfilepos)
end;

{ truncate at a given position }
procedure do_truncate (handle,pos:longint);
begin
  checkCTRLC;
  { Seeking from OFFSET_BEGINNING }
  if dos_SetFileSize(handle,pos,OFFSET_BEGINNING)<0 then
    dosError2InOut(dos_IoErr);
end;

procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $10)   the file will be append
  when (flags and $100)  the file will be truncate/rewritten
  when (flags and $1000) there is no check for close (needed for textfiles)
}
var
  i,j : LongInt;
  openflags : LongInt;
  path : String;
  buffer : array[0..255] of Char;
  index : Integer;
  s : String;
begin
  path:=strpas(p);
  for index:=1 to length(path) do
    if path[index]='\' then path[index]:='/';
  { remove any dot characters and replace by their current }
  { directory equivalent.                                  }

  { look for parent directory }
  if pos('../',path) = 1 then
    begin
      delete(path,1,3);
      getdir(0,s);
      j:=length(s);
      while (s[j]<>'/') and (s[j]<>':') and (j>0) do
        dec(j);
      if j > 0 then
        s:=copy(s,1,j);
      path:=s+path;
    end
  else

  { look for current directory }
  if pos('./',path) = 1 then
    begin
      delete(path,1,2);
      getdir(0,s);
      if (s[length(s)]<>'/') and (s[length(s)]<>':') then
        s:=s+'/';
      path:=s+path;
    end;

  move(path[1],buffer,length(path));
  buffer[length(path)]:=#0;

   { close first if opened }
  if ((flags and $10000)=0) then 
    begin
      case filerec(f).mode of
        fminput,fmoutput,fminout : Do_Close(filerec(f).handle);
        fmclosed : ;
        else begin
          inoutres:=102; {not assigned}
          exit;
        end;
      end;
    end;

  { reset file handle }
  filerec(f).handle:=UnusedHandle;

  { convert filemode to filerec modes }
  { READ/WRITE on existing file }
  { RESET/APPEND                }
  openflags := 1005;
  case (flags and 3) of
    0 : filerec(f).mode:=fminput;
    1 : filerec(f).mode:=fmoutput;
    2 : filerec(f).mode:=fminout;
  end;

  { rewrite (create a new file) }
  if (flags and $1000)<>0 then openflags := 1006;

  { empty name is special }
  if p[0]=#0 then 
    begin
      case filerec(f).mode of
        fminput : 
          filerec(f).handle:=StdInputHandle;
        fmappend,
        fmoutput : begin
          filerec(f).handle:=StdOutputHandle;
          filerec(f).mode:=fmoutput; {fool fmappend}
        end;
      end;
      exit;
    end;
  
  i:=dos_Open(buffer,openflags);
  if i=0 then 
    begin
      dosError2InOut(dos_IoErr);
    end else begin
      AddToList(MOS_fileList,i);
      filerec(f).handle:=i;
    end;

  { append mode }
  if ((Flags and $100)<>0) and (FileRec(F).Handle<>UnusedHandle) then
    begin
      do_seekend(filerec(f).handle);
      filerec(f).mode:=fmoutput; {fool fmappend}
    end;
end;

function do_isdevice(handle:longint):boolean;
begin
  if (handle=StdOutputHandle) or (handle=StdInputHandle) or
     (handle=StdErrorHandle) then
    do_isdevice:=True
  else
    do_isdevice:=False;
end;

{*****************************************************************************
                          UnTyped File Handling
*****************************************************************************}

{$i file.inc}

{*****************************************************************************
                           Typed File Handling
*****************************************************************************}

{$i typefile.inc}

{*****************************************************************************
                           Text File Handling
*****************************************************************************}

{$I text.inc}


{ MorphOS specific startup }
procedure SysInitMorphOS;
var self: PProcess;
begin
 self:=PProcess(exec_FindTask(nil));
 if self^.pr_CLI<>0 then begin
   { if we're running from Ambient, we catch its message }
   exec_WaitPort(@self^.pr_MsgPort);
   MOS_ambMsg:=exec_GetMsg(@self^.pr_MsgPort);   
 end;

 MOS_DOSBase:=exec_OpenLibrary('dos.library',50);
 if MOS_DOSBase=nil then Halt(1);

 { Creating the memory pool for growing heap }
 MOS_heapPool:=exec_CreatePool(MEMF_FAST,growheapsize2,growheapsize1);
 if MOS_heapPool=nil then Halt(1);

 StdInputHandle:=dos_Input;
 StdOutputHandle:=dos_Output;
end;


procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
 
  { * MorphOS doesn't have a separate stderr, just like AmigaOS (???) * }
  StdErrorHandle:=StdOutputHandle;
  // OpenStdIO(StdErr,fmOutput,StdErrorHandle);
end;


{procedure SysInitExecPath;
var
  hs   : string[16];
  link : string;
  i    : longint;
begin
  str(Fpgetpid,hs);
  hs:='/proc/'+hs+'/exe'#0;
  i:=Fpreadlink(@hs[1],@link[1],high(link));
  { it must also be an absolute filename, linux 2.0 points to a memory
    location so this will skip that }
  if (i>0) and (link[1]='/') then
   begin
     link[0]:=chr(i);
     ExecPathStr:=link;
   end;
end;
}

Begin
  IsConsole := TRUE;
  IsLibrary := FALSE;
  StackLength := InitialStkLen;
  StackBottom := Sptr - StackLength;
{ OS specific startup }
  MOS_ambMsg:=nil;
  MOS_origDir:=0;
  MOS_fileList:=nil;
  SysInitMorphOS;
{ Set up signals handlers }
//  InstallSignals;
{ Setup heap }
  InitHeap;
//  SysInitExceptions;
{ Arguments }
//  SetupCmdLine;
//  SysInitExecPath;
{ Setup stdin, stdout and stderr }
  SysInitStdIO;
{ Reset IO Error }
  InOutRes:=0;
(* This should be changed to a real value during *)
(* thread driver initialization if appropriate.  *)
  ThreadID := 1;
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}
End.

{
  $Log$
  Revision 1.6  2004-05-09 14:42:59  karoly
    * again, few more new things added

  Revision 1.5  2004/05/09 02:02:42  karoly
    * more things got implemented

  Revision 1.4  2004/05/02 02:06:57  karoly
    + most of file I/O calls implemented

  Revision 1.3  2004/05/01 15:09:47  karoly
    * first working system unit (very limited yet)

  Revision 1.2  2004/04/08 06:28:29  karoly 
    * first steps to have a morphos system unit

  Revision 1.1  2004/02/13 07:19:53  karoly
    * quick hack from Linux system unit
}
