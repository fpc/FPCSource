{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Karoly Balogh for Genesi S.a.r.l.

    System unit for MorphOS/PowerPC
  
    Uses parts of the Commodore Amiga/68k port by Carl Eric Codere 
    and Nils Sjoholm

    MorphOS port was done on a free Pegasos II/G4 machine 
    provided by Genesi S.a.r.l. <www.genesi.lu>

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

{$include execd.inc}

type
  PClockData = ^TClockData;
  TClockData = packed Record
    sec  : Word;
    min  : Word;
    hour : Word;
    mday : Word;
    month: Word;
    year : Word;
    wday : Word;
  end;

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

  PInfoData = ^TInfoData;
  TInfoData = packed record
    id_NumSoftErrors: LongInt;      { number of soft errors on disk }
    id_UnitNumber   : LongInt;      { Which unit disk is (was) mounted on }
    id_DiskState    : LongInt;      { See defines below }
    id_NumBlocks    : LongInt;      { Number of blocks on disk }
    id_NumBlocksUsed: LongInt;      { Number of block in use }
    id_BytesPerBlock: LongInt;
    id_DiskType     : LongInt;      { Disk Type code }
    id_VolumeNode   : LongInt;      { BCPL pointer to volume node }
    id_InUse        : LongInt;      { Flag, zero if not in use }
  end;

  PChain = ^TChain;
  TChain = packed record
    an_Child : PChain;
    an_Parent: PChain;
    an_Lock  : LongInt;
    an_info  : TFileInfoBlock;
    an_Flags : ShortInt;
    an_string: Array[0..0] of char;
  end;

  PAnchorPath = ^TAnchorPath;
  TAnchorPath = packed record
    ap_Base     : PChain;     { pointer to first anchor  }
    ap_First    : PChain;     { pointer to last anchor   }
    ap_BreakBits: LongInt;    { Bits we want to break on }
    ap_FondBreak: LongInt;    { Bits we broke on. Also returns ERROR_BREAK }
    ap_Flags    : ShortInt;   { New use for extra word.  }
    ap_reserved : Byte;
    ap_StrLen   : Word;
    ap_Info     : TFileInfoBlock;
    ap_Buf      : array[0..0] of Char; { Buffer for path name, allocated by user }
  end;

  PDOSList = ^TDOSList;
  TDOSList = packed record
    dol_Next: LongInt;  { bptr to next device on list }
    dol_Type: LongInt;  { see DLT below }
    dol_Task: Pointer;  { ptr to handler task }
    dol_Lock: LongInt;
    dol_Misc: array[0..23] of ShortInt;
    dol_Name: LongInt;  { bptr to bcpl name }
  end;


var
  MOS_ExecBase   : Pointer; external name '_ExecBase';
  MOS_DOSBase    : Pointer;
  MOS_UtilityBase: Pointer;

  MOS_heapPool : Pointer; { pointer for the OS pool for growing the heap }
  MOS_origDir  : LongInt; { original directory on startup }
  MOS_ambMsg   : PMessage; 
  MOS_ConName  : PChar ='CON:10/30/620/100/FPC Console Output/AUTO/CLOSE/WAIT';
  MOS_ConHandle: LongInt;
  
  argc: LongInt;
  argv: PPChar;
  envp: PPChar;


{*****************************************************************************
                           MorphOS functions
*****************************************************************************}

{ dos.library functions }

function dos_Output: LongInt; SysCall MOS_DOSBase 60;
function dos_Input: LongInt; SysCall MOS_DOSBase 54;
function dos_IoErr: LongInt; SysCall MOS_DOSBase 132;
function dos_GetArgStr: PChar; SysCall MOS_DOSBase 534;

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
                  length: LongInt location 'd3'): LongInt; SysCall MOS_DOSBase 42;
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
function dos_NameFromLock(lock: LongInt location 'd1';
                          buffer: PChar location 'd2';
                          len: LongInt location 'd3'): Boolean; SysCall MOS_DOSBase 402;
function dos_Info(lock: LongInt location 'd1';
                  parameterBlock: PInfoData location 'd2'): Boolean; SysCall MOS_DOSBase 114;

function dos_CreateDir(dname: PChar location 'd1'): LongInt; SysCall MOS_DOSBase 120;
function dos_DateStamp(var ds: TDateStamp location 'd1'): LongInt; SysCall MOS_DOSBase 192;

function dos_SystemTagList(command: PChar location 'd1';
                           tags: Pointer location 'd2'): LongInt; SysCall MOS_DOSBase 606;
function dos_GetVar(vname: PChar location 'd1';
                    buffer: PChar location 'd2';
                    size: LongInt location 'd3';
                    flags: LongInt location 'd4'): LongInt; SysCall MOS_DOSBase 906;
function dos_MatchFirst(pat: PChar location 'd1';
                        anchor: PAnchorPath location 'd2'): LongInt; SysCall MOS_DOSBase 822;
function dos_MatchNext(anchor: PAnchorPath location 'd1'): LongInt; SysCall MOS_DOSBase 828;
procedure dos_MatchEnd(anchor: PAnchorPath location 'd1') SysCall MOS_DOSBase 834;

function dos_LockDosList(flags: LongInt location 'd1'): PDOSList; SysCall MOS_DOSBase 654;
procedure dos_UnLockDosList(flags: LongInt location 'd1'); SysCall MOS_DOSBase 660;
function dos_NextDosEntry(dlist: PDOSList location 'd1';
                          flags: LongInt location 'd2'): PDOSList; SysCall MOS_DOSBase 690;

function dos_SetProtection(name: PChar location 'd1';
                           mask: LongInt location 'd2'): Boolean; SysCall MOS_DOSBase 186;
function dos_SetFileDate(name: PChar location 'd1';
                         date: PDateStamp location 'd2'): Boolean; SysCall MOS_DOSBase 396;

function dos_GetProgramDir: LongInt; SysCall MOS_DOSBase 600;
function dos_GetProgramName(buf: PChar location 'd1';
                            len: LongInt location 'd2'): Boolean; SysCall MOS_DOSBase 576;


{ utility.library functions }

function util_Date2Amiga(date: PClockData location 'a0'): LongInt; SysCall MOS_UtilityBase 126;
procedure util_Amiga2Date(amigatime: LongInt location 'd0';
                          resultat: PClockData location 'a0'); SysCall MOS_UtilityBase 120;


implementation

{$I system.inc}


{*****************************************************************************
                           MorphOS functions
*****************************************************************************}

{ exec.library functions }

{$include execf.inc}


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
    if (SetSignal(0,0) and SIGBREAKF_CTRL_C)<>0 then
      SetSignal(0,SIGBREAKF_CTRL_C);
  end;

  { Closing opened files }
  CloseList(MOS_fileList);

  if MOS_UtilityBase<>nil then CloseLibrary(MOS_UtilityBase);
  if MOS_DOSBase<>nil then CloseLibrary(MOS_DOSBase);
  if MOS_heapPool<>nil then DeletePool(MOS_heapPool);
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
    if (SetSignal(0,0) And SIGBREAKF_CTRL_C)<>0 then begin
      { Clear CTRL-C signal }
      SetSignal(0,SIGBREAKF_CTRL_C);
      Halt(CTRL_C);
    end;
  end;
end;

{ Generates correct argument array on startup } 
procedure GenerateArgs;
var
  argvlen : longint;

  procedure allocarg(idx,len:longint);
    var
      i,oldargvlen : longint;
    begin
      if idx>=argvlen then
        begin
          oldargvlen:=argvlen;
          argvlen:=(idx+8) and (not 7);
          sysreallocmem(argv,argvlen*sizeof(pointer));
          for i:=oldargvlen to argvlen-1 do
            argv[i]:=nil;
        end;
      { use realloc to reuse already existing memory }
      sysreallocmem(argv[idx],len+1);
    end;

var
  count: word;
  start: word;
  localindex: word;
  p : pchar;
  temp : string;

begin
  p:=dos_GetArgStr;
  argvlen:=0;

  { Set argv[0] }
  temp:=paramstr(0);
  allocarg(0,length(temp));
  move(temp[1],argv[0]^,length(temp));
  argv[0][length(temp)]:=#0;

  { check if we're started from Ambient }
  if MOS_ambMsg<>nil then 
    begin
      argc:=0;
      exit;
    end;

  { Handle the other args }
  count:=0;
  { first index is one }
  localindex:=1;
  while (p[count]<>#0) do
    begin
      while (p[count]=' ') or (p[count]=#9) or (p[count]=LineEnding) do inc(count);
      start:=count;
      while (p[count]<>#0) and (p[count]<>' ') and (p[count]<>#9) and (p[count]<>LineEnding) do inc(count);
      if (count-start>0) then
        begin
          allocarg(localindex,count-start);
          move(p[start],argv[localindex]^,count-start);
          argv[localindex][count-start]:=#0;
          inc(localindex);
        end;
    end;
  argc:=localindex;
end;

function GetProgramDir: String;
var
  s1     : String;
  alock  : LongInt;
  counter: Byte;
begin
  GetProgramDir:='';
  FillChar(s1,255,#0);
  { GetLock of program directory }
  alock:=dos_GetProgramDir;
  if alock<>0 then begin
    if dos_NameFromLock(alock,@s1[1],255) then begin
      counter:=1;
      while (s1[counter]<>#0) and (counter<>0) do Inc(counter);
      s1[0]:=Char(counter-1);
      GetProgramDir:=s1;
    end;
  end;
end;

function GetProgramName: String;
{ Returns ONLY the program name }
var
  s1     : String;
  counter: Byte;
begin
  GetProgramName:='';
  FillChar(s1,255,#0);
  if dos_GetProgramName(@s1[1],255) then begin

      { now check out and assign the length of the string }
      counter := 1;
      while (s1[counter]<>#0) and (counter<>0) do Inc(counter);
      s1[0]:=Char(counter-1);

      { now remove any component path which should not be there }
      for counter:=length(s1) downto 1 do
          if (s1[counter] = '/') or (s1[counter] = ':') then break;
      { readjust counterv to point to character }
      if counter<>1 then Inc(counter);

      GetProgramName:=copy(s1,counter,length(s1));
  end;
end;


{*****************************************************************************
                             ParamStr/Randomize
*****************************************************************************}

{ number of args }
function paramcount : longint;
begin
  if MOS_ambMsg<>nil then
    paramcount:=0
  else
    paramcount:=argc-1;
end;

{ argument number l }
function paramstr(l : longint) : string;
var
  s1: String;
begin
  paramstr:='';  
  if MOS_ambMsg<>nil then exit;

  if l=0 then begin
    s1:=GetProgramDir;
    if s1[length(s1)]=':' then paramstr:=s1+GetProgramName
                          else paramstr:=s1+'/'+GetProgramName;
  end else begin
    if (l>0) and (l+1<=argc) then paramstr:=strpas(argv[l]);
  end;
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
  Sbrk:=AllocPooled(MOS_heapPool,size);
end;

{*****************************************************************************
      OS Memory allocation / deallocation 
 ****************************************************************************}

function SysOSAlloc(size: ptrint): pointer;
begin
  result := sbrk(size);
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptrint);
begin
  fpmunmap(p, size);
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
  
  dosResult:=dos_Read(h,addr,len);
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
 self:=PProcess(FindTask(nil));
 if self^.pr_CLI=0 then begin
   { if we're running from Ambient/Workbench, we catch its message }
   WaitPort(@self^.pr_MsgPort);
   MOS_ambMsg:=GetMsg(@self^.pr_MsgPort);
 end;

 MOS_DOSBase:=OpenLibrary('dos.library',50);
 if MOS_DOSBase=nil then Halt(1);
 MOS_UtilityBase:=OpenLibrary('utility.library',50);
 if MOS_UtilityBase=nil then Halt(1);

 { Creating the memory pool for growing heap }
 MOS_heapPool:=CreatePool(MEMF_FAST,growheapsize2,growheapsize1);
 if MOS_heapPool=nil then Halt(1);

 if MOS_ambMsg=nil then begin
   StdInputHandle:=dos_Input;
   StdOutputHandle:=dos_Output;
 end else begin
   MOS_ConHandle:=dos_Open(MOS_ConName,1005);
   if MOS_ConHandle<>0 then begin
     StdInputHandle:=MOS_ConHandle;
     StdOutputHandle:=MOS_ConHandle;
   end else
     Halt(1);
 end;
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


begin
  IsConsole := TRUE;
  IsLibrary := FALSE;
  StackLength := InitialStkLen;
  StackBottom := Sptr - StackLength;
{ OS specific startup }
  MOS_ambMsg:=nil;
  MOS_origDir:=0;
  MOS_fileList:=nil;
  envp:=nil;
  SysInitMorphOS;
{ Set up signals handlers }
//  InstallSignals;
{ Setup heap }
  InitHeap;
  SysInitExceptions;
{ Setup stdin, stdout and stderr }
  SysInitStdIO;
{ Reset IO Error }
  InOutRes:=0;
{ Arguments }
  GenerateArgs;
(* This should be changed to a real value during *)
(* thread driver initialization if appropriate.  *)
  ThreadID := 1;
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}
end.

{
  $Log$
  Revision 1.14  2004-06-17 16:16:14  peter
    * New heapmanager that releases memory back to the OS, donated
      by Micha Nelissen

  Revision 1.13  2004/06/13 22:50:47  karoly
    * cleanup and changes to use new includes

  Revision 1.12  2004/06/06 23:31:13  karoly
    * fixed dos_UnLockDosList from being nonsense, and some cleanup

  Revision 1.11  2004/06/06 19:18:05  karoly
    + added support for paramstr(0)

  Revision 1.10  2004/06/05 19:49:19  karoly
    + added console I/O support when running from Ambient

  Revision 1.9  2004/05/12 23:18:54  karoly
    * fixed do_read and dos_Read from being nonsense

  Revision 1.8  2004/05/12 20:26:04  karoly
    + added syscalls and structures necessary for DOS unit

  Revision 1.7  2004/05/12 15:34:16  karoly
    * fixed startup code from endless wait when not started from Ambient

  Revision 1.6  2004/05/09 14:42:59  karoly
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
