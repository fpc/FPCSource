{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    System.pp for Netware libc environment
 **********************************************************************}
{ no stack check in system }
{$S-}
unit system;

interface

{$define netware}
{$define netware_libc}

{$define StdErrToConsole}
{$define autoHeapRelease}
{$define IOpossix}
{$define DisableArrayOfConst}

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
 maxExitCode = $ffff;


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

type
   TNWCheckFunction = procedure (var code : longint);
   TDLL_Process_Entry_Hook = function (dllparam : longint) : longbool;
   TDLL_Entry_Hook = procedure (dllparam : longint);

VAR
   ArgC                : INTEGER;
   ArgV                : ppchar;
   NetwareCheckFunction: TNWCheckFunction;
   NWLoggerScreen      : pointer = nil;

const
  Dll_Process_Attach_Hook : TDLL_Process_Entry_Hook = nil;
  Dll_Process_Detach_Hook : TDLL_Entry_Hook = nil;
  Dll_Thread_Attach_Hook  : TDLL_Entry_Hook = nil;
  Dll_Thread_Detach_Hook  : TDLL_Entry_Hook = nil;
  envp : ppchar = nil;



type
  //TSysCloseAllRemainingSemaphores = procedure;
  TSysReleaseThreadVars = procedure;
  TSysSetThreadDataAreaPtr = function (newPtr:pointer):pointer;

procedure NWSysSetThreadFunctions (atv:TSysReleaseThreadVars;
                                   rtv:TSysReleaseThreadVars;
                                   stdata:TSysSetThreadDataAreaPtr);


procedure __ConsolePrintf (s :shortstring);
procedure __EnterDebugger; cdecl;

function NWGetCodeStart : pointer;  // needed for Lineinfo
function NWGetCodeLength : dword;
function NWGetDataStart : pointer;
function NWGetDataLength : dword;

implementation
{ Indicate that stack checking is taken care by OS}
{$DEFINE NO_GENERIC_STACK_CHECK}

{ include system independent routines }
{$I system.inc}

{ some declarations for Netware API calls }
{ I nwlibc.inc}
{$I errno.inc}
{$define INCLUDED_FROM_SYSTEM}
{$I libc.pp}

var
  {$ifdef autoHeapRelease}
  HeapListAllocResourceTag,
  {$endif}
  HeapAllocResourceTag : rtag_t;
  NLMHandle : pointer;
  ReleaseThreadVars : TSysReleaseThreadVars = nil;
  AllocateThreadVars: TSysReleaseThreadVars = nil;
  SetThreadDataAreaPtr : TSysSetThreadDataAreaPtr = nil;

procedure NWSysSetThreadFunctions (atv:TSysReleaseThreadVars;
                                   rtv:TSysReleaseThreadVars;
                                   stdata:TSysSetThreadDataAreaPtr);
begin
  AllocateThreadVars := atv;
  ReleaseThreadVars := rtv;
  SetThreadDataAreaPtr := stdata;
end;


procedure PASCALMAIN;external name 'PASCALMAIN';
procedure fpc_do_exit;external name 'FPC_DO_EXIT';


{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}

{$ifdef autoHeapRelease}
procedure FreeSbrkMem; forward;
{$endif}

var SigTermHandlerActive : boolean;

Procedure system_exit;
begin
  //__ConsolePrintf ('system_exit');
  if assigned (ReleaseThreadVars) then ReleaseThreadVars;

  {$ifdef autoHeapRelease}
  FreeSbrkMem;              { free memory allocated by heapmanager }
  {$endif}

  if not SigTermHandlerActive then
  begin
    if Erroraddr <> nil then   { otherwise we dont see runtime-errors }
      SetScreenMode (0);

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

  With a 5k byte safe area used to write to StdIo and some libc
  functions without crossing the stack boundary
}
begin
  if StackErr then exit;  // avoid recursive calls
  if stackavail > stack_size + 5120 then exit;  // we really need that much, at least on nw6.5
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
  randseed := time (NIL);
end;

{*****************************************************************************
                              Heap Management
*****************************************************************************}

var
  int_heap : pointer;external name 'HEAP';
  int_heapsize : longint;external name 'HEAPSIZE';

{ first address of heap }
function getheapstart:pointer;
begin
  getheapstart := int_heap;
end;

{ current length of heap }
function getheapsize:longint;
begin
  getheapsize := int_heapsize;
end;

{$ifdef autoHeapRelease}

const HeapInitialMaxBlocks = 32;
type THeapSbrkBlockList = array [1.. HeapInitialMaxBlocks] of pointer;
var  HeapSbrkBlockList : ^THeapSbrkBlockList = nil;
     HeapSbrkLastUsed  : dword = 0;
     HeapSbrkAllocated : dword = 0;
     HeapSbrkReleased : boolean = false;

{ function to allocate size bytes more for the program }
{ must return the first address of new data space or nil if fail }
{ for netware all allocated blocks are saved to free them at }
{ exit (to avoid message "Module did not release xx resources") }
Function SysOSAlloc(size : longint):pointer;
var P2 : POINTER;
    i  : longint;
    Slept : longint;
begin
  if HeapSbrkReleased then
  begin
    __ConsolePrintf ('Error: SysOSFree called after all heap memory was released');
    exit(nil);
  end;
  SysOSAlloc := _Alloc (size,HeapAllocResourceTag);
  if SysOSAlloc <> nil then begin
    if HeapSbrkBlockList = nil then
    begin
      Pointer (HeapSbrkBlockList) := _Alloc (sizeof (HeapSbrkBlockList^),HeapListAllocResourceTag);
      if HeapSbrkBlockList = nil then
      begin
        _free (SysOSAlloc);
        SysOSAlloc := nil;
        exit;
      end;
      fillchar (HeapSbrkBlockList^,sizeof(HeapSbrkBlockList^),0);
      HeapSbrkAllocated := HeapInitialMaxBlocks;
    end;
    if (HeapSbrkLastUsed > 0) then
      for i := 1 to HeapSbrkLastUsed do
        if (HeapSbrkBlockList^[i] = nil) then
        begin  // reuse free slot
          HeapSbrkBlockList^[i] := SysOSAlloc;
          exit;
        end;
    if (HeapSbrkLastUsed = HeapSbrkAllocated) then
    begin  { grow }
      slept := 0;
      p2 := _ReallocSleepOK (HeapSbrkBlockList, (HeapSbrkAllocated + HeapInitialMaxBlocks) * sizeof(pointer),HeapListAllocResourceTag,Slept);
      if p2 = nil then  // should we better terminate with error ?
      begin
        _free (SysOSAlloc);
        SysOSAlloc := nil;
        exit;
      end;
      HeapSbrkBlockList := p2;
      inc (HeapSbrkAllocated, HeapInitialMaxBlocks);
    end;
    inc (HeapSbrkLastUsed);
    HeapSbrkBlockList^[HeapSbrkLastUsed] := SysOSAlloc;
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
  HeapSbrkReleased := true;
  {ReturnResourceTag(HeapAllocResourceTag,1);
  ReturnResourceTag(HeapListAllocResourceTag,1);  not in netware.imp, seems to be not needed}
end;

{*****************************************************************************
      OS Memory allocation / deallocation
 ****************************************************************************}

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptrint);
var i : longint;
begin
  if HeapSbrkReleased then
  begin
    __ConsolePrintf ('Error: SysOSFree called after all heap memory was released');
  end else
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
  SysOSAlloc := _Alloc(size,HeapAllocResourceTag);
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
  Sys_EBUSY   : Inoutres:=162
  else begin
    Writeln (stderr,'NW2PASErr: unknown error ',err);
    libc_perror('NW2PASErr');
    Inoutres := Err;
  end;
  end;
END;


procedure Errno2Inoutres;
begin
  NW2PASErr (___errno^);
end;

procedure SetFileError (VAR Err : LONGINT);
begin
  if Err >= 0 then
    InOutRes := 0
  else begin
    // libc_perror ('SetFileError');
    Err := ___errno^;
    NW2PASErr (Err);
    Err := 0;
  end;
end;

{ close a file from the handle value }
procedure do_close(handle : thandle);
VAR res : LONGINT;
begin
  {$ifdef IOpossix}
  res := FpClose (handle);
  {$else}
  res := _fclose (_TFILE(handle));
  {$endif}
  IF res <> 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

procedure do_erase(p : pchar);
VAR res : LONGINT;
begin
  res := unlink (p);
  IF Res < 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

procedure do_rename(p1,p2 : pchar);
VAR res : LONGINT;
begin
  res := rename (p1,p2);
  IF Res < 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0
end;

function do_write(h:thandle;addr:pointer;len : longint) : longint;
var res : LONGINT;
begin
  {$ifdef IOpossix}
  res := Fpwrite (h,addr,len);
  {$else}
  res := _fwrite (addr,1,len,_TFILE(h));
  {$endif}
  if res > 0 then
    InOutRes := 0
  else
    SetFileError (res);
  do_write := res;
  NXThreadYield;
end;

function do_read(h:thandle;addr:pointer;len : longint) : longint;
VAR res : LONGINT;
begin
  {$ifdef IOpossix}
  res := Fpread (h,addr,len);
  {$else}
  res := _fread (addr,1,len,_TFILE(h));
  {$endif}
  IF res > 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
  do_read := res;
  NXThreadYield;
end;


function do_filepos(handle : thandle) : longint;
var res : LONGINT;
begin
  InOutRes:=1;
  {$ifdef IOpossix}
  res := Fptell (handle);
  {$else}
  res := _ftell (_TFILE(handle));
  {$endif}
  if res < 0 THEN
    SetFileError (res)
  else
    InOutRes := 0;
  do_filepos := res;
end;


procedure do_seek(handle:thandle;pos : longint);
VAR res : LONGINT;
begin
  {$ifdef IOpossix}
  res := Fplseek (handle,pos, SEEK_SET);
  {$else}
  res := _fseek (_TFILE(handle),pos, SEEK_SET);
  {$endif}
  IF res >= 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
end;

function do_seekend(handle:thandle):longint;
VAR res : LONGINT;
begin
  {$ifdef IOpossix}
  res := Fplseek (handle,0, SEEK_END);
  {$else}
  res := _fseek (_TFILE(handle),0, SEEK_END);
  {$endif}
  IF res >= 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
  do_seekend := res;
end;


function do_filesize(handle : thandle) : longint;
VAR res     : LONGINT;
    statbuf : TStat;
begin
  {$ifdef IOpossix}
  res := fstat (handle, statbuf);
  {$else}
  res := _fstat (_fileno (_TFILE(handle)), statbuf);  // was _filelength for clib
  {$endif}
  if res <> 0 then
  begin
    SetFileError (Res);
    do_filesize := -1;
  end else
  begin
    InOutRes := 0;
    do_filesize := statbuf.st_size;
  end;
end;

{ truncate at a given position }
procedure do_truncate (handle:thandle;pos:longint);
VAR res : LONGINT;
begin
  {$ifdef IOpossix}
  res := ftruncate (handle,pos);
  {$else}
  res := _ftruncate (_fileno (_TFILE(handle)),pos);
  {$endif}
  IF res <> 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

{$ifdef IOpossix}
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
  ___errno^ := 0;
  FileRec(f).Handle := open(p,oflags,438);
  { open somtimes returns > -1 but errno was set }
  if (___errno^ <> 0) or (longint(FileRec(f).Handle) < 0) then
    if (___errno^=Sys_EROFS) and ((OFlags and O_RDWR)<>0) then
    begin  // i.e. for cd-rom
      Oflags:=Oflags and not(O_RDWR);
      FileRec(f).Handle := open(p,oflags,438);
    end;
  if (___errno^ <> 0) or (longint(FileRec(f).Handle) < 0) then
    Errno2Inoutres
  else
    InOutRes := 0;
end;


{$else}
procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $10)   the file will be append
  when (flags and $100)  the file will be truncate/rewritten
  when (flags and $1000) there is no check for close (needed for textfiles)
}
var
  oflags : string[10];
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
         oflags := 'rb'#0;
         filerec(f).mode := fminput;
       end;
   1 : begin
         if (flags and $1000)=$1000 then
	   oflags := 'w+b' else
           oflags := 'wb';
         filerec(f).mode := fmoutput;
       end;
   2 : begin
         if (flags and $1000)=$1000 then
	   oflags := 'w+' else
           oflags := 'r+';
         filerec(f).mode := fminout;
       end;
  end;
  {if (flags and $1000)=$1000 then
   oflags:=oflags or (O_CREAT or O_TRUNC)
  else
   if (flags and $100)=$100 then
    oflags:=oflags or (O_APPEND);}
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
  FileRec(f).Handle := THandle (_fopen (p,@oflags[1]));//_open(p,oflags,438);
  //WriteLn ('_open (',p,') returned ',ErrNo, 'Handle: ',FileRec(f).Handle);
  // errno does not seem to be set on succsess ??
  {IF FileRec(f).Handle < 0 THEN
    if (ErrNo=Sys_EROFS) and ((OFlags and O_RDWR)<>0) then
    begin  // i.e. for cd-rom
      Oflags:=Oflags and not(O_RDWR);
      FileRec(f).Handle := _open(p,oflags,438);
    end;}
  if FileRec(f).Handle = 0 then
    Errno2Inoutres
  else
    InOutRes := 0;
End;
{$endif}

function do_isdevice(handle:THandle):boolean;
begin
  {$ifdef IOpossix}
  do_isdevice := (Fpisatty (handle) > 0);
  {$else}
  do_isdevice := (isatty (_fileno(_TFILE(handle))) > 0);
  {$endif}
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

{$i text.inc}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}
procedure mkdir(const s : string);[IOCheck];
var S2 : STRING;
    Res: LONGINT;
BEGIN
  S2 := S;
  IF Length (S2) = 255 THEN DEC (BYTE(S2[0]));
  S2 := S2 + #0;
  Res := FpMkdir (@S2[1],S_IRWXU);
  if Res = 0 then
    InOutRes:=0
  else
    SetFileError (Res);
end;

procedure rmdir(const s : string);[IOCheck];
VAR S2 : STRING;
    Res: LONGINT;
BEGIN
  S2 := S;
  IF Length (S2) = 255 THEN DEC (BYTE(S2[0]));
  S2 := S2 + #0;
  Res := FpRmdir (@S2[1]);
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
  Res := FpChdir (@S2[1]);
  IF Res = 0 THEN
    InOutRes:=0
  ELSE
    SetFileError (Res);
end;

procedure getdir(drivenr : byte;var dir : shortstring);
var P : array [0..255] of CHAR;
    i : LONGINT;
begin
  P[0] := #0;
  getcwdpath(@P,nil,0);   // getcwd does not return volume, getcwdpath does
  i := libc_strlen (P);
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
  end else
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

function CheckFunction : longint; CDECL; [public,alias: '_NonAppCheckUnload'];
var oldPtr : pointer;
begin
  //__ConsolePrintf ('CheckFunction');
  if assigned (NetwareCheckFunction) then
  begin
    if assigned (SetThreadDataAreaPtr) then
      oldPtr := SetThreadDataAreaPtr (NIL);  { nil means main thread }

    result := 0;
    NetwareCheckFunction (result);

    if assigned (SetThreadDataAreaPtr) then
      SetThreadDataAreaPtr (oldPtr);

  end else
    result := 0;
end;


procedure __ConsolePrintf (s : shortstring);
begin
  if length(s) > 252 then
    byte(s[0]) := 252;
  s := s + #13#10#0;
  if NWLoggerScreen = nil then
    NWLoggerScreen := getnetwarelogger;
  screenprintf (NWLoggerScreen,@s[1]);
end;


procedure __EnterDebugger;cdecl;external '!netware' name 'EnterDebugger';

var NWUts : Tutsname;

procedure getCodeAddresses;
begin
  if uname(NWUts) < 0 then
    FillChar(NWuts,sizeof(NWUts),0);
end;

function NWGetCodeStart : pointer;
begin
  NWGetCodeStart := NWUts.codeoffset;
  NXThreadYield;
end;

function NWGetCodeLength : dword;
begin
  NWGetCodeLength := NWUts.codelength;
  NXThreadYield;
end;

function NWGetDataStart : pointer;
begin
  NWGetDataStart := NWUts.dataoffset;
  NXThreadYield;
end;

function NWGetDataLength : dword;
begin
  NWGetDataLength := NWUts.datalength;
  NXThreadYield;
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
     screenprintf (NWLoggerScreen,@ConsoleBuff);
  end;
  F.BufPos:=0;
  ConsoleWrite := 0;
  NXThreadYield;
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
var oldPtr : pointer;
begin
  { Threadvar Pointer will not be valid because the signal
    handler is called by netware with a differnt thread. To avoid
    problems in the exit routines, we set the data of the main thread
    here }
  if assigned (SetThreadDataAreaPtr) then
    oldPtr := SetThreadDataAreaPtr (NIL);  { nil means main thread }
  SigTermHandlerActive := true;  { to avoid that system_exit calls _exit }
  do_exit;                       { calls finalize units }
  if assigned (SetThreadDataAreaPtr) then
    SetThreadDataAreaPtr (oldPtr);
end;


procedure SysInitStdIO;
begin
{ Setup stdin, stdout and stderr }
  {$ifdef IOpossix}
  StdInputHandle := THandle (fileno (___stdin^));    // GetStd** returns **FILE !
  StdOutputHandle:= THandle (fileno (___stdout^));
  StdErrorHandle := THandle (fileno (___stderr^));
  {$else}
  StdInputHandle := THandle (___stdin^);    // GetStd** returns **FILE !
  StdOutputHandle:= THandle (___stdout^);
  StdErrorHandle := THandle (___stderr^);
  {$endif}

  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);

  {$ifdef StdErrToConsole}
  AssignStdErrConsole(StdErr);
  {$else}
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
  {$endif}
end;

// this is called by main.as, setup args and call PASCALMAIN
procedure nlm_main (_ArgC : LONGINT; _ArgV : ppchar); cdecl; [public,alias: '_FPC_NLM_Entry'];
BEGIN
  ArgC := _ArgC;
  ArgV := _ArgV;
  isLibrary := false;
  PASCALMAIN;
  do_exit;    // currently not needed
END;

function _DLLMain (hInstDLL:pointer; fdwReason:dword; DLLParam:longint):longbool; cdecl;
[public, alias : '_FPC_DLL_Entry'];
var res : longbool;
begin
  __ConsolePrintf ('_FPC_DLL_Entry called');
  _DLLMain := false;
  isLibrary := true;
  case fdwReason of
    DLL_ACTUAL_DLLMAIN  : _DLLMain := true;
    DLL_NLM_STARTUP     : begin
                            //__ConsolePrintf ('DLL_NLM_STARTUP');
                            if assigned(Dll_Process_Attach_Hook) then
                            begin
                              res:=Dll_Process_Attach_Hook(DllParam);
                              if not res then
                                exit(false);
                            end;
                            PASCALMAIN;
                            _DLLMain := true;
                          end;
    DLL_NLM_SHUTDOWN    : begin
                            //__ConsolePrintf ('DLL_NLM_SHUTDOWN');
                            TermSigHandler(0);
                            _DLLMain := true;
                          end;
     { standard DllMain() messages...  }
    DLL_THREAD_ATTACH,
    DLL_PROCESS_ATTACH  : begin
                            //__ConsolePrintf ('DLL_PROCESS/THREAD_ATTACH');
                            if assigned(AllocateThreadVars) then
                              AllocateThreadVars;
                            if assigned(Dll_Thread_Attach_Hook) then
                              Dll_Thread_Attach_Hook(DllParam);

                            _DLLMain := true;
                          end;
    DLL_THREAD_DETACH,
    DLL_PROCESS_DETACH  : begin
                            //__ConsolePrintf ('DLL_PROCESS/THREAD_DETACH');
                            if assigned(Dll_Thread_Detach_Hook) then
                              Dll_Thread_Detach_Hook(DllParam);
                            if assigned(ReleaseThreadVars) then
                              ReleaseThreadVars;
                            _DLLMain := true;
                          end;
  end;
end;



{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

Begin
  getCodeAddresses;
  StackBottom := SPtr - StackLength;
  SigTermHandlerActive := false;
  NetwareCheckFunction := nil;
  {$ifdef StdErrToConsole}
  NWLoggerScreen := getnetwarelogger;
  {$endif}
  CheckFunction;  // avoid check function to be removed by the linker

  envp := ____environ^;
  NLMHandle := getnlmhandle;
  { allocate resource tags to see what kind of memory i forgot to release }
  HeapAllocResourceTag :=
    AllocateResourceTag(NLMHandle,'Heap Memory',AllocSignature);
  {$ifdef autoHeapRelease}
  HeapListAllocResourceTag :=
    AllocateResourceTag(NLMHandle,'Heap Memory List',AllocSignature);
  {$endif}
  Signal (SIGTERM, @TermSigHandler);

{ Setup heap }
  InitHeap;
  SysInitExceptions;

{ Reset IO Error }
  InOutRes:=0;

  ThreadID := dword(pthread_self);

  SysInitStdIO;

{Delphi Compatible}
  IsConsole := TRUE;
  ExitCode  := 0;
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}
End.
{
  $Log$
  Revision 1.3  2004-09-19 20:06:37  armin
  * removed get/free video buf from video.pp
  * implemented sockets
  * basic library support
  * threadvar memory leak removed
  * fixes (ide now starts and editor is usable)
  * support for lineinfo

  Revision 1.2  2004/09/12 20:51:22  armin
  * added keyboard and video
  * a lot of fixes

  Revision 1.1  2004/09/05 20:58:47  armin
  * first rtl version for netwlibc

}
