{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ no stack check in system }
{$S-}
unit system;

interface

{$define StdErrToConsole}
{$define useLongNamespaceByDefault}
{$define autoHeapRelease}

{$ifdef SYSTEMDEBUG}
  {$define SYSTEMEXCEPTIONDEBUG}
{$endif SYSTEMDEBUG}

{$ifdef cpui386}
  {$define Set_i386_Exception_handler}
{$endif cpui386}

{ include system-independent routine headers }

{$I systemh.inc}

type THandle = DWord;

{Platform specific information}
const
 LineEnding = #13#10;
 LFNSupport : boolean = false;
 DirectorySeparator = '/';
 DriveSeparator = ':';
 PathSeparator = ';';
{ FileNameCaseSensitive is defined separately below!!! }
 maxExitCode = 255;

{ include heap support headers }
{$I heaph.inc}

CONST
  { Default filehandles }
   UnusedHandle    : THandle = -1;
   StdInputHandle  : THandle = 0;
   StdOutputHandle : THandle = 0;
   StdErrorHandle  : THandle = 0;

   FileNameCaseSensitive : boolean = false;

   sLineBreak = LineEnding;
   DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

TYPE
   TNWCheckFunction = procedure (var code : longint);

VAR
   ArgC   : INTEGER;
   ArgV   : ppchar;
   NetwareCheckFunction    : TNWCheckFunction;
   NetwareMainThreadGroupID: longint;
   NetwareCodeStartAddress : dword;

CONST
   envp   : ppchar = nil;   {dummy to make heaptrc happy}


PROCEDURE ConsolePrintf (FormatStr : PCHAR; Param : LONGINT); CDecl;
PROCEDURE ConsolePrintf3 (FormatStr : PCHAR; P1,P2,P3 : LONGINT);  CDecl;
PROCEDURE ConsolePrintf (FormatStr : PCHAR);  CDecl;

type 
  TSysCloseAllRemainingSemaphores = procedure;
  TSysReleaseThreadVars = procedure;
  TSysSetThreadDataAreaPtr = function (newPtr:pointer):pointer;
  
procedure NWSysSetThreadFunctions (crs:TSysCloseAllRemainingSemaphores;
                                   rtv:TSysReleaseThreadVars;
				   stdata:TSysSetThreadDataAreaPtr);

function NWGetCodeStart : pointer;  // needed for lineinfo

implementation
{ Indicate that stack checking is taken care by OS}
{$DEFINE NO_GENERIC_STACK_CHECK}

{ include system independent routines }
{$I system.inc}

{ some declarations for Netware API calls }
{$I nwsys.inc}
{$I errno.inc}


var 
  CloseAllRemainingSemaphores : TSysCloseAllRemainingSemaphores = nil;
  ReleaseThreadVars : TSysReleaseThreadVars = nil;
  SetThreadDataAreaPtr : TSysSetThreadDataAreaPtr = nil;

procedure NWSysSetThreadFunctions (crs:TSysCloseAllRemainingSemaphores;
                                   rtv:TSysReleaseThreadVars;
				   stdata:TSysSetThreadDataAreaPtr);
begin
  CloseAllRemainingSemaphores := crs;
  ReleaseThreadVars := rtv;
  SetThreadDataAreaPtr := stdata;
end;  




procedure PASCALMAIN;external name 'PASCALMAIN';
procedure fpc_do_exit;external name 'FPC_DO_EXIT';


{*****************************************************************************
                         Startup
*****************************************************************************}

    function __GetBssStart : pointer; external name '__getBssStart';
    function __getUninitializedDataSize : longint; external name '__getUninitializedDataSize';
    //function __getDataStart : longint; external name '__getDataStart';
    function __GetTextStart : longint; external name '__getTextStart';

PROCEDURE nlm_main (_ArgC : LONGINT; _ArgV : ppchar); CDECL; [public,alias: '_nlm_main'];
BEGIN
  // Initialize BSS
  if __getUninitializedDataSize > 0 then
    fillchar (__getBssStart^,__getUninitializedDataSize,0);
  NetwareCodeStartAddress := __GetTextStart;
  ArgC := _ArgC;
  ArgV := _ArgV;
  fpc_threadvar_relocate_proc := nil;
  PASCALMAIN;
END;

function NWGetCodeStart : pointer;  // needed for lineinfo
begin
  NWGetCodeStart := pointer(NetwareCodeStartAddress);
end;


{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}

{$ifdef autoHeapRelease}
procedure FreeSbrkMem; forward;
{$endif}

var SigTermHandlerActive : boolean;

Procedure system_exit;
begin
  if assigned (CloseAllRemainingSemaphores) then CloseAllRemainingSemaphores;
  if assigned (ReleaseThreadVars) then ReleaseThreadVars;

  {$ifdef autoHeapRelease}
  FreeSbrkMem;            { free memory allocated by heapmanager }
  {$endif}

  if not SigTermHandlerActive then
  begin
    if ExitCode <> 0 Then   { otherwise we dont see runtime-errors }
      _SetAutoScreenDestructionMode (false);

    _exit (ExitCode);
  end;
end;

{*****************************************************************************
                         Stack check code
*****************************************************************************}

const StackErr : boolean = false;

procedure int_stackcheck(stack_size:Cardinal);[saveregisters,public,alias:'FPC_STACKCHECK'];
{
  called when trying to get local stack if the compiler directive $S
  is set this function must preserve all registers

  With a 2048 byte safe area used to write to StdIo without crossing
  the stack boundary
}
begin
  if StackErr then exit;  // avoid recursive calls
  if _stackavail > stack_size + 2048 THEN EXIT;
  StackErr := true;
  HandleError (202);
end;
{*****************************************************************************
                              ParamStr/Randomize
*****************************************************************************}

{ number of args }
function paramcount : longint;
begin
  paramcount := argc - 1;
end;

{ argument number l }
function paramstr(l : longint) : string;
begin
  if (l>=0) and (l+1<=argc) then
  begin
    paramstr:=strpas(argv[l]);
    if l = 0 then  // fix nlm path
    begin
      for l := 1 to length (paramstr) do
        if paramstr[l] = '\' then paramstr[l] := '/';
    end;
  end else
   paramstr:='';
end;

{ set randseed to a new pseudo random value }
procedure randomize;
begin
  randseed := _time (NIL);
end;

{*****************************************************************************
                              Heap Management
*****************************************************************************}

var
  heap : longint;external name 'HEAP';
  intern_heapsize : longint;external name 'HEAPSIZE';

{ first address of heap }
function getheapstart:pointer;
assembler;
asm
        leal    HEAP,%eax
end ['EAX'];

{ current length of heap }
function getheapsize:longint;
assembler;
asm
        movl    intern_HEAPSIZE,%eax
end ['EAX'];

{$ifdef autoHeapRelease}

const HeapInitialMaxBlocks = 32;
type THeapSbrkBlockList = array [1.. HeapInitialMaxBlocks] of pointer;
var  HeapSbrkBlockList : ^THeapSbrkBlockList = nil;
     HeapSbrkLastUsed  : dword = 0;
     HeapSbrkAllocated : dword = 0;

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or nil if fail }
{ for netware all allocated blocks are saved to free them at }
{ exit (to avoid message "Module did not release xx resources") }
Function Sbrk(size : longint):pointer;
var P2 : POINTER;
    i  : longint;
begin
  Sbrk := _malloc (size);
  if Sbrk <> nil then begin
    if HeapSbrkBlockList = nil then
    begin
      Pointer (HeapSbrkBlockList) := _malloc (sizeof (HeapSbrkBlockList^));
      if HeapSbrkBlockList = nil then
      begin
        _free (Sbrk);
        Sbrk := nil;
        exit;
      end;
      fillchar (HeapSbrkBlockList^,sizeof(HeapSbrkBlockList^),0);
      HeapSbrkAllocated := HeapInitialMaxBlocks;
    end;
    if (HeapSbrkLastUsed > 0) then
      for i := 1 to HeapSbrkLastUsed do
        if (HeapSbrkBlockList^[i] = nil) then
        begin  // reuse free slot
	  HeapSbrkBlockList^[i] := Sbrk;
	  exit;
        end;
    if (HeapSbrkLastUsed = HeapSbrkAllocated) then
    begin  { grow }
      p2 := _realloc (HeapSbrkBlockList, (HeapSbrkAllocated + HeapInitialMaxBlocks) * sizeof(pointer));
      if p2 = nil then  // should we better terminate with error ?
      begin
        _free (Sbrk);
         Sbrk := nil;
         exit;
      end;
      HeapSbrkBlockList := p2;
      inc (HeapSbrkAllocated, HeapInitialMaxBlocks);
    end;
    inc (HeapSbrkLastUsed);
    HeapSbrkBlockList^[HeapSbrkLastUsed] := Sbrk;
  end;
end;


procedure FreeSbrkMem;
var i : longint;
begin
  if HeapSbrkBlockList <> nil then
  begin
    for i := 1 to HeapSbrkLastUsed do
      if (HeapSbrkBlockList^[i] <> nil) then
        _free (HeapSbrkBlockList^[i]);
    _free (HeapSbrkBlockList);
    HeapSbrkAllocated := 0;
    HeapSbrkLastUsed := 0;
    HeapSbrkBlockList := nil;
  end;
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
var i : longint;
begin
//fpmunmap(p, size);
  if (HeapSbrkLastUsed > 0) then
    for i := 1 to HeapSbrkLastUsed do
      if (HeapSbrkBlockList^[i] = p) then
      begin
	_free (p);
	HeapSbrkBlockList^[i] := nil;
	exit;
      end;
  HandleError (204);  // invalid pointer operation
end;

{$else autoHeapRelease}

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptrint);
begin
  _free (p);
end;

function SysOSAlloc(size: ptrint): pointer;
begin
  SysOSAlloc := _malloc (size);
end;

{$endif autoHeapRelease}

{ include standard heap management }
{$I heap.inc}


{****************************************************************************
                        Low level File Routines
       All these functions can set InOutRes on errors
 ****************************************************************************}


PROCEDURE NW2PASErr (Err : LONGINT);
BEGIN
  if Err = 0 then { Else it will go through all the cases }
   exit;
  case Err of
   Sys_ENFILE,
   Sys_EMFILE : Inoutres:=4;
   Sys_ENOENT : Inoutres:=2;
    Sys_EBADF : Inoutres:=6;
   Sys_ENOMEM,
   Sys_EFAULT : Inoutres:=217;
   Sys_EINVAL : Inoutres:=218;
    Sys_EPIPE,
    Sys_EINTR,
      Sys_EIO,
   Sys_EAGAIN,
   Sys_ENOSPC : Inoutres:=101;
 Sys_ENAMETOOLONG,
    Sys_ELOOP,
  Sys_ENOTDIR : Inoutres:=3;
    Sys_EROFS,
   Sys_EEXIST,
   Sys_EACCES : Inoutres:=5;
  Sys_EBUSY   : Inoutres:=162;
  end;
END;

FUNCTION errno : LONGINT;
BEGIN
  errno := __get_errno_ptr^;
END;

PROCEDURE Errno2Inoutres;
BEGIN
  NW2PASErr (errno);
END;

PROCEDURE SetFileError (VAR Err : LONGINT);
BEGIN
  IF Err >= 0 THEN
    InOutRes := 0
  ELSE
  BEGIN
    Err := errno;
    NW2PASErr (Err);
    Err := 0;
  END;
END;

{ close a file from the handle value }
procedure do_close(handle : thandle);
VAR res : LONGINT;
begin
  res := _close (handle);
  IF res <> 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

procedure do_erase(p : pchar);
VAR res : LONGINT;
begin
  res := _unlink (p);
  IF Res < 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

procedure do_rename(p1,p2 : pchar);
VAR res : LONGINT;
begin
  res := _rename (p1,p2);
  IF Res < 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0
end;

function do_write(h:thandle;addr:pointer;len : longint) : longint;
VAR res : LONGINT;
begin
  res := _write (h,addr,len);
  IF res > 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
  do_write := res;
end;

function do_read(h:thandle;addr:pointer;len : longint) : longint;
VAR res : LONGINT;
begin
  res := _read (h,addr,len);
  IF res > 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
  do_read := res;
end;


function do_filepos(handle : thandle) : longint;
VAR res : LONGINT;
begin
  InOutRes:=1;
  res := _tell (handle);
  IF res < 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
  do_filepos := res;
end;

CONST SEEK_SET = 0; // Seek from beginning of file.
      SEEK_CUR = 1; // Seek from current position.
      SEEK_END = 2; // Seek from end of file.


procedure do_seek(handle:thandle;pos : longint);
VAR res : LONGINT;
begin
  res := _lseek (handle,pos, SEEK_SET);
  IF res >= 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
end;

function do_seekend(handle:thandle):longint;
VAR res : LONGINT;
begin
  res := _lseek (handle,0, SEEK_END);
  IF res >= 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
  do_seekend := res;
end;


function do_filesize(handle : thandle) : longint;
VAR res     : LONGINT;
begin
  res := _filelength (handle);
  IF res < 0 THEN
  BEGIN
    SetFileError (Res);
    do_filesize := -1;
  END ELSE
  BEGIN
    InOutRes := 0;
    do_filesize := res;
  END;
end;

{ truncate at a given position }
procedure do_truncate (handle:thandle;pos:longint);
VAR res : LONGINT;
begin
  res := _chsize (handle,pos);
  IF res <> 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

// mostly stolen from syslinux
procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $10)   the file will be append
  when (flags and $100)  the file will be truncate/rewritten
  when (flags and $1000) there is no check for close (needed for textfiles)
}
var
  oflags : longint;
Begin
{ close first if opened }
  if ((flags and $10000)=0) then
   begin
     case FileRec(f).mode of
      fminput,fmoutput,fminout : Do_Close(FileRec(f).Handle);
      fmclosed : ;
     else
      begin
        inoutres:=102; {not assigned}
        exit;
      end;
     end;
   end;
{ reset file Handle }
  FileRec(f).Handle:=UnusedHandle;

{ We do the conversion of filemodes here, concentrated on 1 place }
  case (flags and 3) of
   0 : begin
         oflags := O_RDONLY;
         filerec(f).mode := fminput;
       end;
   1 : begin
         oflags := O_WRONLY;
         filerec(f).mode := fmoutput;
       end;
   2 : begin
         oflags := O_RDWR;
         filerec(f).mode := fminout;
       end;
  end;
  if (flags and $1000)=$1000 then
   oflags:=oflags or (O_CREAT or O_TRUNC)
  else
   if (flags and $100)=$100 then
    oflags:=oflags or (O_APPEND);
{ empty name is special }
  if p[0]=#0 then
   begin
     case FileRec(f).mode of
       fminput :
         FileRec(f).Handle:=StdInputHandle;
       fminout, { this is set by rewrite }
       fmoutput :
         FileRec(f).Handle:=StdOutputHandle;
       fmappend :
         begin
           FileRec(f).Handle:=StdOutputHandle;
           FileRec(f).mode:=fmoutput; {fool fmappend}
         end;
     end;
     exit;
   end;
{ real open call }
  FileRec(f).Handle := _open(p,oflags,438);
  //WriteLn ('_open (',p,') returned ',ErrNo, 'Handle: ',FileRec(f).Handle);
  // errno does not seem to be set on succsess ??
  IF FileRec(f).Handle < 0 THEN
    if (ErrNo=Sys_EROFS) and ((OFlags and O_RDWR)<>0) then
    begin  // i.e. for cd-rom
      Oflags:=Oflags and not(O_RDWR);
      FileRec(f).Handle := _open(p,oflags,438);
    end;
  IF FileRec(f).Handle < 0 THEN
    Errno2Inoutres
  ELSE
    InOutRes := 0;
End;

function do_isdevice(handle:THandle):boolean;
begin
  do_isdevice := (_isatty (handle) > 0);
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

{ should we consider #26 as the  end of a file ? }
{?? $DEFINE EOF_CTRLZ}

{$i text.inc}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}
procedure mkdir(const s : string);[IOCheck];
VAR S2 : STRING;
    Res: LONGINT;
BEGIN
  S2 := S;
  IF Length (S2) = 255 THEN DEC (BYTE(S2[0]));
  S2 := S2 + #0;
  Res := _mkdir (@S2[1]);
  IF Res = 0 THEN
    InOutRes:=0
  ELSE
    SetFileError (Res);
END;

procedure rmdir(const s : string);[IOCheck];
VAR S2 : STRING;
    Res: LONGINT;
BEGIN
  S2 := S;
  IF Length (S2) = 255 THEN DEC (BYTE(S2[0]));
  S2 := S2 + #0;
  Res := _rmdir (@S2[1]);
  IF Res = 0 THEN
    InOutRes:=0
  ELSE
    SetFileError (Res);
end;

procedure chdir(const s : string);[IOCheck];
VAR S2 : STRING;
    Res: LONGINT;
begin
  S2 := S;
  IF Length (S2) = 255 THEN DEC (BYTE(S2[0]));
  S2 := S2 + #0;
  Res := _chdir (@S2[1]);
  IF Res = 0 THEN
    InOutRes:=0
  ELSE
    SetFileError (Res);
end;

procedure getdir(drivenr : byte;var dir : shortstring);
VAR P : ARRAY [0..255] OF CHAR;
    i : LONGINT;
begin
  P[0] := #0;
  _getcwd (@P, SIZEOF (P));
  i := _strlen (P);
  if i > 0 then
  begin
    Move (P, dir[1], i);
    BYTE(dir[0]) := i;
    For i := 1 to length (dir) do
      if dir[i] = '\' then dir [i] := '/';
    // fix / after volume, the compiler needs that
    // normaly root of a volumes is SERVERNAME/SYS:, change that
    // to SERVERNAME/SYS:/
    i := pos (':',dir);
    if (i > 0) then
      if i = Length (dir) then dir := dir + '/' else
      if dir [i+1] <> '/' then insert ('/',dir,i+1);
  END ELSE
    InOutRes := 1;
end;


{*****************************************************************************
                             Thread Handling
*****************************************************************************}

procedure InitFPU;assembler;

  asm
     fninit
     fldcw   fpucw
  end;


{ if return-value is <> 0, netware shows the message
  Unload Anyway ?
  To Disable unload at all, SetNLMDontUnloadFlag can be used on
  Netware >= 4.0 }
function CheckFunction : longint; CDECL; [public,alias: 'FPC_NW_CHECKFUNCTION'];
var oldTG:longint;
    oldPtr: pointer;
begin
  if assigned (NetwareCheckFunction) then
  begin
    { this function is called without clib context, to allow clib
      calls, we set the thread group id before calling the
      user-function }
    oldTG := _SetThreadGroupID (NetwareMainThreadGroupID);
    { to allow use of threadvars, we simply set the threadvar-memory
      from the main thread }
    if assigned (SetThreadDataAreaPtr) then 
      oldPtr := SetThreadDataAreaPtr (NIL);  { nil means main threadvars }
    result := 0;
    NetwareCheckFunction (result);
    if assigned (SetThreadDataAreaPtr) then
      SetThreadDataAreaPtr (oldPtr);
      
    _SetThreadGroupID (oldTG);
  end else
    result := 0;
end;



{$ifdef StdErrToConsole}
var ConsoleBuff : array [0..512] of char;

Function ConsoleWrite(Var F: TextRec): Integer;
var
  i : longint;
Begin
  if F.BufPos>0 then
  begin
     if F.BufPos>sizeof(ConsoleBuff)-1 then
       i:=sizeof(ConsoleBuff)-1
     else
       i:=F.BufPos;
     Move(F.BufPtr^,ConsoleBuff,i);
     ConsoleBuff[i] := #0;
     ConsolePrintf(@ConsoleBuff[0]);
  end;
  F.BufPos:=0;
  ConsoleWrite := 0;
End;


Function ConsoleClose(Var F: TextRec): Integer;
begin
  ConsoleClose:=0;
end;


Function ConsoleOpen(Var F: TextRec): Integer;
Begin
  TextRec(F).InOutFunc:=@ConsoleWrite;
  TextRec(F).FlushFunc:=@ConsoleWrite;
  TextRec(F).CloseFunc:=@ConsoleClose;
  ConsoleOpen:=0;
End;


procedure AssignStdErrConsole(Var T: Text);
begin
  Assign(T,'');
  TextRec(T).OpenFunc:=@ConsoleOpen;
  Rewrite(T);
end;
{$endif}


{ this will be called if the nlm is unloaded. It will NOT be
  called if the program exits i.e. with halt.
  Halt (or _exit) can not be called from this callback procedure }
procedure TermSigHandler (Sig:longint); CDecl;
var oldTG : longint;
    oldPtr: pointer;
begin
  oldTG := _SetThreadGroupID (NetwareMainThreadGroupID); { this is only needed for nw 3.11 }

  { _GetThreadDataAreaPtr will not be valid because the signal
    handler is called by netware with a differnt thread. To avoid
    problems in the exit routines, we set the data of the main thread
    here }
  if assigned (SetThreadDataAreaPtr) then
    oldPtr := SetThreadDataAreaPtr (NIL);  { nil means main thread }
  SigTermHandlerActive := true;  { to avoid that system_exit calls _exit }
  do_exit;                       { calls finalize units }
  if assigned (SetThreadDataAreaPtr) then
    SetThreadDataAreaPtr (oldPtr);  
  _SetThreadGroupID (oldTG);
end;


procedure SysInitStdIO;
begin
{ Setup stdin, stdout and stderr }
  StdInputHandle := _fileno (LONGINT (_GetStdIn^));    // GetStd** returns **FILE !
  StdOutputHandle:= _fileno (LONGINT (_GetStdOut^));
  StdErrorHandle := _fileno (LONGINT (_GetStdErr^));

  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  
  {$ifdef StdErrToConsole}
  AssignStdErrConsole(StdErr);
  {$else}
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
  {$endif}
end;


{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

Begin
  StackBottom := SPtr - StackLength;
  SigTermHandlerActive := false;
  NetwareCheckFunction := nil;
  NetwareMainThreadGroupID := _GetThreadGroupID;

  _Signal (_SIGTERM, @TermSigHandler);

  {$ifdef useLongNamespaceByDefault}
  if _getenv ('FPC_DISABLE_LONG_NAMESPACE') = nil then
  begin
    if _SetCurrentNameSpace (NW_NS_LONG) <> 255 then
    begin
      if _SetTargetNamespace (NW_NS_LONG) <> 255 then
        LFNSupport := true
      else
        _SetCurrentNameSpace (NW_NS_DOS);
    end;
  end;  
  {$endif useLongNamespaceByDefault}

{ Setup heap }
  InitHeap;
  SysInitExceptions;

{ Reset IO Error }
  InOutRes:=0;

(* This should be changed to a real value during *)
(* thread driver initialization if appropriate.  *)
  ThreadID := 1;
  
  SysInitStdIO;  

{Delphi Compatible}
  IsLibrary := FALSE;
  IsConsole := TRUE;
  ExitCode  := 0;
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}
End.
{
  $Log$
  Revision 1.26  2004-09-17 18:29:07  armin
  * added NWGetCodeStart, needed for lineinfo

  Revision 1.25  2004/09/03 19:26:27  olle
    + added maxExitCode to all System.pp
    * constrained error code to be below maxExitCode in RunError et. al.

  Revision 1.24  2004/08/01 20:02:48  armin
  * changed dir separator from \ to /
  * long namespace by default
  * dos.exec implemented
  * getenv ('PATH') is now supported
  * changed FExpand to global version
  * fixed heaplist growth error
  * support SysOSFree
  * stackcheck was without saveregisters
  * fpc can compile itself on netware

  Revision 1.23  2004/07/30 15:05:25  armin
  make netware rtl compilable under 1.9.5

  Revision 1.22  2004/06/17 16:16:14  peter
    * New heapmanager that releases memory back to the OS, donated
      by Micha Nelissen

  Revision 1.21  2004/01/20 23:11:20  hajny
    * ExecuteProcess fixes, ProcessID and ThreadID added

  Revision 1.20  2003/10/25 23:43:59  hajny
    * THandle in sysutils common using System.THandle

  Revision 1.19  2003/10/17 22:12:02  olle
    * changed i386 to cpui386

  Revision 1.18  2003/09/27 11:52:35  peter
    * sbrk returns pointer

  Revision 1.17  2003/03/25 18:17:54  armin
  * support for fcl, support for linking without debug info
  * renamed winsock2 to winsock for win32 compatinility
  * new sockets unit for netware
  * changes for compiler warnings

  Revision 1.16  2003/02/15 19:12:54  armin
  * changes for new threadvar support

  Revision 1.15  2002/10/13 09:28:45  florian
    + call to initvariantmanager inserted

  Revision 1.14  2002/09/07 16:01:21  peter
    * old logs removed and tabs fixed

  Revision 1.13  2002/07/01 16:29:05  peter
    * sLineBreak changed to normal constant like Kylix

  Revision 1.12  2002/04/15 18:47:34  carl
  + reinstate novell stack checking

  Revision 1.11  2002/04/12 17:40:11  carl
  + generic stack checking

  Revision 1.10  2002/04/01 15:20:08  armin
  + unload module no longer shows: Module did not release...
  + check-function will no longer be removed when smartlink is on

  Revision 1.9  2002/04/01 10:47:31  armin
  makefile.fpc for netware
  stderr to netware console
  free all memory (threadvars and heap) to avoid error message while unloading nlm

  Revision 1.8  2002/03/30 09:09:47  armin
  + support check-function for netware

  Revision 1.7  2002/03/17 17:57:33  armin
  + threads and winsock2 implemented

}
