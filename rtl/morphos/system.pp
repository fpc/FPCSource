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
  maxExitCode = 255;

const
  UnusedHandle    : LongInt = -1;
  StdInputHandle  : LongInt = 0;
  StdOutputHandle : LongInt = 0;
  StdErrorHandle  : LongInt = 0;

  FileNameCaseSensitive : Boolean = False;

  sLineBreak : string[1] = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

  BreakOn : Boolean = True;


var
  MOS_ExecBase   : Pointer; external name '_ExecBase';
  MOS_DOSBase    : Pointer;
  MOS_UtilityBase: Pointer;

  MOS_heapPool : Pointer; { pointer for the OS pool for growing the heap }
  MOS_origDir  : LongInt; { original directory on startup }
  MOS_ambMsg   : Pointer;
  MOS_ConName  : PChar ='CON:10/30/620/100/FPC Console Output/AUTO/CLOSE/WAIT';
  MOS_ConHandle: LongInt;

  argc: LongInt;
  argv: PPChar;
  envp: PPChar;


implementation

{$I system.inc}


{*****************************************************************************
                           MorphOS structures
*****************************************************************************}

{$include execd.inc}
{$include timerd.inc}
{$include doslibd.inc}


{*****************************************************************************
                           MorphOS functions
*****************************************************************************}

{ exec.library functions }

{$include execf.inc}
{$include doslibf.inc}


{*****************************************************************************
                    System Dependent Structures/Consts
*****************************************************************************}

const
  CTRL_C           = 20;      { Error code on CTRL-C press }


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
    next   : PFileList;    { Next file in list }
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
      dosClose(tmpHandle);
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

  { Changing back to original directory if changed }
  if MOS_origDir<>0 then begin
    CurrentDir(MOS_origDir);
  end;

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
  p:=GetArgStr;
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

function GetProgDir: String;
var
  s1     : String;
  alock  : LongInt;
  counter: Byte;
begin
  GetProgDir:='';
  FillChar(s1,255,#0);
  { GetLock of program directory }
  alock:=GetProgramDir;
  if alock<>0 then begin
    if NameFromLock(alock,@s1[1],255) then begin
      counter:=1;
      while (s1[counter]<>#0) and (counter<>0) do Inc(counter);
      s1[0]:=Char(counter-1);
      GetProgDir:=s1;
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
  if GetProgramName(@s1[1],255) then begin
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

{ Converts an Unix-like path to Amiga-like path }
function PathConv(path: string): string; alias: 'PATHCONV'; [public];
var tmppos: longint;
begin
  { check for short paths }
  if length(path)<=2 then begin
    if (path='.') or (path='./') then begin path:=''; exit; end;
    if path='..' then begin path:='/'; exit; end;
    if path='*' then begin path:='#?'; exit; end;
  end else begin
    { convert parent directories }
    tmppos:=pos('../',path);
    while tmppos<>0 do begin
      { delete .. to have / as parent dir sign }
      delete(path,tmppos,2); 
      tmppos:=pos('../',path);
    end;
    { convert current directories }
    tmppos:=pos('./',path);
    while tmppos<>0 do begin
      { delete ./ since we doesn't need to sign current directory }
      delete(path,tmppos,2);
      tmppos:=pos('./',path);
    end;
    { convert wildstart to #? }
    tmppos:=pos('*',path);
    while tmppos<>0 do begin
      delete(path,tmppos,1);
      insert('#?',path,tmppos);
      tmppos:=pos('*',path);
    end;
  end;
  PathConv:=path;
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
    s1:=GetProgDir;
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
  DateStamp(@tmpTime);
  randseed:=tmpTime.ds_tick;
end;


{*****************************************************************************
      OS Memory allocation / deallocation
 ****************************************************************************}

function SysOSAlloc(size: ptrint): pointer;
begin
  result := AllocPooled(MOS_heapPool,size);
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptrint);
begin
  FreePooled(MOS_heapPool,p,size);
end;

{$I heap.inc}


{*****************************************************************************
                           Directory Handling
*****************************************************************************}
procedure mkdir(const s : string);[IOCheck];
var
  tmpStr : array[0..255] of char;
  tmpLock: LongInt;
begin
  checkCTRLC;
  if (s='') or (InOutRes<>0) then exit;
  tmpStr:=PathConv(s)+#0;
  tmpLock:=CreateDir(@tmpStr);
  if tmpLock=0 then begin
    dosError2InOut(IoErr);
    exit;
  end;
  UnLock(tmpLock);
end;

procedure rmdir(const s : string);[IOCheck];
var
  tmpStr : array[0..255] of Char;
begin
  checkCTRLC;
  if (s='.') then InOutRes:=16;
  If (s='') or (InOutRes<>0) then exit;
  tmpStr:=PathConv(s)+#0;
  if not DeleteFile(@tmpStr) then
    dosError2InOut(IoErr);
end;

procedure chdir(const s : string);[IOCheck];
var
  tmpStr : array[0..255] of Char;
  tmpLock: LongInt;
  FIB    : PFileInfoBlock;
begin
  checkCTRLC;
  If (s='') or (InOutRes<>0) then exit;
  tmpStr:=PathConv(s)+#0;
  tmpLock:=0;

  { Changing the directory is a pretty complicated affair }
  {   1) Obtain a lock on the directory                   }
  {   2) CurrentDir the lock                              }
  tmpLock:=Lock(@tmpStr,SHARED_LOCK);
  if tmpLock=0 then begin
    dosError2InOut(IoErr);
    exit;
  end;

  FIB:=nil;
  new(FIB);

  if (Examine(tmpLock,FIB)=True) and (FIB^.fib_DirEntryType>0) then begin
    tmpLock := CurrentDir(tmpLock);
    if MOS_OrigDir=0 then begin
      MOS_OrigDir:=tmpLock;
      tmpLock:=0;
    end;
  end;

  if tmpLock<>0 then Unlock(tmpLock);
  if assigned(FIB) then dispose(FIB);
end;

procedure GetDir (DriveNr: byte; var Dir: ShortString);
var tmpbuf: array[0..255] of char;
begin
  checkCTRLC;
  Dir:='';
  if not GetCurrentDirName(tmpbuf,256) then
    dosError2InOut(IoErr)
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
  if not dosClose(handle) then
    dosError2InOut(IoErr);
end;

procedure do_erase(p : pchar);
begin
  checkCTRLC;
  if not DeleteFile(p) then
    dosError2InOut(IoErr);
end;

procedure do_rename(p1,p2 : pchar);
begin
  checkCTRLC;
  if not dosRename(p1,p2) then
    dosError2InOut(IoErr);
end;

function do_write(h:longint; addr: pointer; len: longint) : longint;
var dosResult: LongInt;
begin
  checkCTRLC;
  do_write:=0;
  if len<=0 then exit;

  dosResult:=dosWrite(h,addr,len);
  if dosResult<0 then begin
    dosError2InOut(IoErr);
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

  dosResult:=dosRead(h,addr,len);
  if dosResult<0 then begin
    dosError2InOut(IoErr);
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
  dosResult:=dosSeek(handle,0,OFFSET_CURRENT);
  if dosResult<0 then begin
    dosError2InOut(IoErr);
  end else begin
    do_filepos:=dosResult;
  end;
end;

procedure do_seek(handle,pos : longint);
begin
  checkCTRLC;
  { Seeking from OFFSET_BEGINNING }
  if dosSeek(handle,pos,OFFSET_BEGINNING)<0 then
    dosError2InOut(IoErr);
end;

function do_seekend(handle:longint):longint;
var dosResult: LongInt;
begin
  checkCTRLC;
  do_seekend:=0;

  { Seeking to OFFSET_END }
  dosResult:=dosSeek(handle,0,OFFSET_END);
  if dosResult<0 then begin
    dosError2InOut(IoErr);
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
  if SetFileSize(handle,pos,OFFSET_BEGINNING)<0 then
    dosError2InOut(IoErr);
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
  handle   : LongInt;
  openflags: LongInt;
  tmpStr   : array[0..255] of Char;
begin
  tmpStr:=PathConv(strpas(p))+#0;

  { close first if opened }
  if ((flags and $10000)=0) then begin
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
  if p[0]=#0 then begin
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

  handle:=Open(@tmpStr,openflags);
  if handle=0 then begin
    dosError2InOut(IoErr);
  end else begin
    AddToList(MOS_fileList,handle);
    filerec(f).handle:=handle;
  end;

  { append mode }
  if ((Flags and $100)<>0) and 
      (FileRec(F).Handle<>UnusedHandle) then begin
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
   StdInputHandle:=dosInput;
   StdOutputHandle:=dosOutput;
 end else begin
   MOS_ConHandle:=Open(MOS_ConName,1005);
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
  // OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
end;

function GetProcessID: SizeUInt;
begin
 GetProcessID := 1;
{$WARNING Implementation of GetProcessID missing!}
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
  Revision 1.24  2004-12-06 20:09:55  karoly
    * added a public alias to PathConv for use in DOS unit

  Revision 1.23  2004/12/05 14:36:37  hajny
    + GetProcessID added

  Revision 1.22  2004/11/15 23:18:16  karoly
   * Reworked path handling to be less messy

  Revision 1.21  2004/11/04 09:32:31  peter
  ErrOutput added

  Revision 1.20  2004/10/25 15:38:59  peter
    * compiler defined HEAP and HEAPSIZE removed

  Revision 1.19  2004/09/03 19:26:15  olle
    + added maxExitCode to all System.pp
    * constrained error code to be below maxExitCode in RunError et. al.

  Revision 1.18  2004/08/09 00:12:40  karoly
    * changes to work with updated doslib includes

  Revision 1.17  2004/08/03 15:59:41  karoly
    * more cleanup & more includes

  Revision 1.16  2004/06/26 20:48:24  karoly
    * more cleanup + changes to use new includes

  Revision 1.15  2004/06/23 13:27:32  karoly
    * fixed system unit for the new heap manager

  Revision 1.14  2004/06/17 16:16:14  peter
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
