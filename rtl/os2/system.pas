{
 $Id$
 ****************************************************************************

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2002 by Free Pascal development team

    Free Pascal - OS/2 runtime library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

****************************************************************************}

unit {$ifdef VER1_0}sysos2{$else}System{$endif};

interface

{$ifdef SYSTEMDEBUG}
  {$define SYSTEMEXCEPTIONDEBUG}
  {.$define IODEBUG}
  {.$define DEBUGENVIRONMENT}
  {.$define DEBUGARGUMENTS}
{$endif SYSTEMDEBUG}

{$DEFINE EOF_CTRLZ}

{ $DEFINE OS2EXCEPTIONS}

{$I systemh.inc}

{$IFDEF OS2EXCEPTIONS}
(* Types and constants for exception handler support *)
type
{x}   PEXCEPTION_FRAME = ^TEXCEPTION_FRAME;
{x}   TEXCEPTION_FRAME = record
{x}     next : PEXCEPTION_FRAME;
{x}     handler : pointer;
{x}   end;

{$ENDIF OS2EXCEPTIONS}

const
  LineEnding = #13#10;
{ LFNSupport is defined separately below!!! }
  DirectorySeparator = '\';
  DriveSeparator = ':';
  PathSeparator = ';';
{ FileNameCaseSensitive is defined separately below!!! }
  MaxExitCode = 65535;

type    Tos=(osDOS,osOS2,osDPMI);

const   os_mode: Tos = osOS2;
        first_meg: pointer = nil;

{$IFDEF OS2EXCEPTIONS}
{x}  System_exception_frame : PEXCEPTION_FRAME =nil;
{$ENDIF OS2EXCEPTIONS}

type    TByteArray = array [0..$ffff] of byte;
        PByteArray = ^TByteArray;

        TSysThreadIB = record
            TID,
            Priority,
            Version: cardinal;
            MCCount,
            MCForceFlag: word;
        end;
        PSysThreadIB = ^TSysThreadIB;

        TThreadInfoBlock = record
            PExChain,
            Stack,
            StackLimit: pointer;
            TIB2: PSysThreadIB;
            Version,
            Ordinal: cardinal;
        end;
        PThreadInfoBlock = ^TThreadInfoBlock;
        PPThreadInfoBlock = ^PThreadInfoBlock;

        TProcessInfoBlock = record
            PID,
            ParentPid,
            Handle: cardinal;
            Cmd,
            Env: PByteArray;
            Status,
            ProcType: cardinal;
        end;
        PProcessInfoBlock = ^TProcessInfoBlock;
        PPProcessInfoBlock = ^PProcessInfoBlock;

const   UnusedHandle=-1;
        StdInputHandle=0;
        StdOutputHandle=1;
        StdErrorHandle=2;

        LFNSupport: boolean = true;
        FileNameCaseSensitive: boolean = false;

        sLineBreak = LineEnding;
        DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

var
{ C-compatible arguments and environment }
  argc  : longint;
  argv  : ppchar;
  envp  : ppchar;
  EnvC: cardinal;

(* Pointer to the block of environment variables - used e.g. in unit Dos. *)
  Environment: PChar;


var
(* Type / run mode of the current process: *)
(* 0 .. full screen OS/2 session           *)
(* 1 .. DOS session                        *)
(* 2 .. VIO windowable OS/2 session        *)
(* 3 .. Presentation Manager OS/2 session  *)
(* 4 .. detached (background) OS/2 process *)
  ApplicationType: cardinal;

(* Is allocation of memory above 512 MB address limit allowed? Initialized *)
(* during initialization of system unit according to capabilities of the   *)
(* underlying OS/2 version, can be overridden by user - heap is allocated  *)
(* for all threads, so the setting isn't declared as a threadvar and       *)
(* should be only changed at the beginning of the main thread if needed.   *)
  UseHighMem: boolean;



procedure SetDefaultOS2FileType (FType: ShortString);

procedure SetDefaultOS2Creator (Creator: ShortString);



implementation

{$I system.inc}


{****************************************************************************

                    Miscellaneous related routines.

****************************************************************************}

procedure system_exit;
begin
  DosFreeThreadLocalMemory (DataIndex);
  DosExit(1{process}, exitcode);
end;

{$ASMMODE ATT}

function paramcount:longint;assembler;
asm
    movl argc,%eax
    decl %eax
end {['EAX']};

function args:pointer;assembler;
asm
  movl argv,%eax
end {['EAX']};


function paramstr(l:longint):string;

var p:^Pchar;

begin
  if (l>=0) and (l<=paramcount) then
  begin
    p:=args;
    paramstr:=strpas(p[l]);
  end
    else paramstr:='';
end;

procedure randomize;
var
  dt: TSysDateTime;
begin
  // Hmm... Lets use timer
  DosGetDateTime(dt);
  randseed:=dt.hour+(dt.minute shl 8)+(dt.second shl 16)+(dt.sec100 shl 32);
end;

{$ASMMODE ATT}


{*****************************************************************************

                        System unit initialization.

****************************************************************************}

{****************************************************************************
                    Error Message writing using messageboxes
****************************************************************************}

type
  TWinMessageBox = function (Parent, Owner: cardinal;
         BoxText, BoxTitle: PChar; Identity, Style: cardinal): cardinal; cdecl;
  TWinInitialize = function (Options: cardinal): cardinal; cdecl;
  TWinCreateMsgQueue = function (Handle: cardinal; cmsg: longint): cardinal;
                                                                         cdecl;

const
  ErrorBufferLength = 1024;
  mb_OK = $0000;
  mb_Error = $0040;
  mb_Moveable = $4000;
  MBStyle = mb_OK or mb_Error or mb_Moveable;
  WinInitialize: TWinInitialize = nil;
  WinCreateMsgQueue: TWinCreateMsgQueue = nil;
  WinMessageBox: TWinMessageBox = nil;
  EnvSize: cardinal = 0;

var
  ErrorBuf: array [0..ErrorBufferLength] of char;
  ErrorLen: longint;
  PMWinHandle: cardinal;

function ErrorWrite (var F: TextRec): integer;
{
  An error message should always end with #13#10#13#10
}
var
  P: PChar;
  I: longint;
begin
  if F.BufPos > 0 then
   begin
     if F.BufPos + ErrorLen > ErrorBufferLength then
       I := ErrorBufferLength - ErrorLen
     else
       I := F.BufPos;
     Move (F.BufPtr^, ErrorBuf [ErrorLen], I);
     Inc (ErrorLen, I);
     ErrorBuf [ErrorLen] := #0;
   end;
  if ErrorLen > 3 then
   begin
     P := @ErrorBuf [ErrorLen];
     for I := 1 to 4 do
      begin
        Dec (P);
        if not (P^ in [#10, #13]) then
          break;
      end;
   end;
   if ErrorLen = ErrorBufferLength then
     I := 4;
   if (I = 4) then
    begin
      WinMessageBox (0, 0, @ErrorBuf, PChar ('Error'), 0, MBStyle);
      ErrorLen := 0;
    end;
  F.BufPos := 0;
  ErrorWrite := 0;
end;

function ErrorClose (var F: TextRec): integer;
begin
  if ErrorLen > 0 then
   begin
     WinMessageBox (0, 0, @ErrorBuf, PChar ('Error'), 0, MBStyle);
     ErrorLen := 0;
   end;
  ErrorLen := 0;
  ErrorClose := 0;
end;

function ErrorOpen (var F: TextRec): integer;
begin
  TextRec(F).InOutFunc := @ErrorWrite;
  TextRec(F).FlushFunc := @ErrorWrite;
  TextRec(F).CloseFunc := @ErrorClose;
  ErrorOpen := 0;
end;


procedure AssignError (var T: Text);
begin
  Assign (T, '');
  TextRec (T).OpenFunc := @ErrorOpen;
  Rewrite (T);
end;

procedure SysInitStdIO;
begin
  { Setup stdin, stdout and stderr, for GUI apps redirect stderr,stdout to be
    displayed in a messagebox }
(*
  StdInputHandle := longint(GetStdHandle(cardinal(STD_INPUT_HANDLE)));
  StdOutputHandle := longint(GetStdHandle(cardinal(STD_OUTPUT_HANDLE)));
  StdErrorHandle := longint(GetStdHandle(cardinal(STD_ERROR_HANDLE)));

  if not IsConsole then
    begin
      if (DosLoadModule (nil, 0, 'PMWIN', PMWinHandle) = 0) and
       (DosQueryProcAddr (PMWinHandle, 789, nil, pointer (WinMessageBox)) = 0)
                                                                           and
       (DosQueryProcAddr (PMWinHandle, 763, nil, pointer (WinInitialize)) = 0)
                                                                           and
       (DosQueryProcAddr (PMWinHandle, 716, nil, pointer (WinCreateMsgQueue))
                                                                           = 0)
        then
          begin
            WinInitialize (0);
            WinCreateMsgQueue (0, 0);
          end
        else
          HandleError (2);
     AssignError (StdErr);
     AssignError (StdOut);
     Assign (Output, '');
     Assign (Input, '');
   end
  else
   begin
*)
     OpenStdIO (Input, fmInput, StdInputHandle);
     OpenStdIO (Output, fmOutput, StdOutputHandle);
     OpenStdIO (ErrOutput, fmOutput, StdErrorHandle);
     OpenStdIO (StdOut, fmOutput, StdOutputHandle);
     OpenStdIO (StdErr, fmOutput, StdErrorHandle);
(*
   end;
*)
end;


function strcopy(dest,source : pchar) : pchar;assembler;
var
  saveeax,saveesi,saveedi : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
{$ifdef REGCALL}
        movl    %eax,saveeax
        movl    %edx,%edi
{$else}
        movl    source,%edi
{$endif}
        testl   %edi,%edi
        jz      .LStrCopyDone
        leal    3(%edi),%ecx
        andl    $-4,%ecx
        movl    %edi,%esi
        subl    %edi,%ecx
{$ifdef REGCALL}
        movl    %eax,%edi
{$else}
        movl    dest,%edi
{$endif}
        jz      .LStrCopyAligned
.LStrCopyAlignLoop:
        movb    (%esi),%al
        incl    %edi
        incl    %esi
        testb   %al,%al
        movb    %al,-1(%edi)
        jz      .LStrCopyDone
        decl    %ecx
        jnz     .LStrCopyAlignLoop
        .balign  16
.LStrCopyAligned:
        movl    (%esi),%eax
        movl    %eax,%edx
        leal    0x0fefefeff(%eax),%ecx
        notl    %edx
        addl    $4,%esi
        andl    %edx,%ecx
        andl    $0x080808080,%ecx
        jnz     .LStrCopyEndFound
        movl    %eax,(%edi)
        addl    $4,%edi
        jmp     .LStrCopyAligned
.LStrCopyEndFound:
        testl   $0x0ff,%eax
        jz      .LStrCopyByte
        testl   $0x0ff00,%eax
        jz      .LStrCopyWord
        testl   $0x0ff0000,%eax
        jz      .LStrCopy3Bytes
        movl    %eax,(%edi)
        jmp     .LStrCopyDone
.LStrCopy3Bytes:
        xorb     %dl,%dl
        movw     %ax,(%edi)
        movb     %dl,2(%edi)
        jmp     .LStrCopyDone
.LStrCopyWord:
        movw    %ax,(%edi)
        jmp     .LStrCopyDone
.LStrCopyByte:
        movb    %al,(%edi)
.LStrCopyDone:
{$ifdef REGCALL}
        movl    saveeax,%eax
{$else}
        movl    dest,%eax
{$endif}
        movl    saveedi,%edi
        movl    saveesi,%esi
end;


{$ifdef HASTHREADVAR}
threadvar
{$else HASTHREADVAR}
var
{$endif HASTHREADVAR}
  DefaultCreator: ShortString;
  DefaultFileType: ShortString;


procedure SetDefaultOS2FileType (FType: ShortString);
begin
{$WARNING Not implemented yet!}
  DefaultFileType := FType;
end;


procedure SetDefaultOS2Creator (Creator: ShortString);
begin
{$WARNING Not implemented yet!}
  DefaultCreator := Creator;
end;


procedure InitEnvironment;
var env_count : longint;
    dos_env,cp : pchar;
begin
  env_count:=0;
  cp:=environment;
  while cp ^ <> #0 do
    begin
    inc(env_count);
    while (cp^ <> #0) do inc(longint(cp)); { skip to NUL }
    inc(longint(cp)); { skip to next character }
    end;
  envp := sysgetmem((env_count+1) * sizeof(pchar));
  envc := env_count;
  if (envp = nil) then exit;
  cp:=environment;
  env_count:=0;
  while cp^ <> #0 do
  begin
    envp[env_count] := sysgetmem(strlen(cp)+1);
    strcopy(envp[env_count], cp);
{$IfDef DEBUGENVIRONMENT}
    Writeln(stderr,'env ',env_count,' = "',envp[env_count],'"');
{$EndIf}
    inc(env_count);
    while (cp^ <> #0) do
      inc(longint(cp)); { skip to NUL }
    inc(longint(cp)); { skip to next character }
  end;
  envp[env_count]:=nil;
end;

procedure InitArguments;
var
  arglen,
  count   : longint;
  argstart,
  pc,arg  : pchar;
  quote   : char;
  argvlen : longint;

  procedure allocarg(idx,len:longint);
  begin
    if idx>=argvlen then
     begin
       argvlen:=(idx+8) and (not 7);
       sysreallocmem(argv,argvlen*sizeof(pointer));
     end;
    { use realloc to reuse already existing memory }
    { always allocate, even if length is zero, since }
    { the arg. is still present!                     }
    sysreallocmem(argv[idx],len+1);
  end;

begin
  count:=0;
  argv:=nil;
  argvlen:=0;

  // Get argv[0]
  pc:=cmdline;
  Arglen:=0;
  repeat
    Inc(Arglen);
  until (pc[Arglen]=#0);
  allocarg(count,arglen);
  move(pc^,argv[count]^,arglen);

  { ReSetup cmdline variable }
  repeat
    Inc(Arglen);
  until (pc[Arglen]=#0);
  Inc(Arglen);
  pc:=GetMem(ArgLen);
  move(cmdline^, pc^, arglen);
  Arglen:=0;
  repeat
    Inc(Arglen);
  until (pc[Arglen]=#0);
  pc[Arglen]:=' '; // combine argv[0] and command line
  CmdLine:=pc;

  { process arguments }
  pc:=cmdline;
{$IfDef DEBUGARGUMENTS}
  Writeln(stderr,'GetCommandLine is #',pc,'#');
{$EndIf }
  while pc^<>#0 do
   begin
     { skip leading spaces }
     while pc^ in [#1..#32] do
      inc(pc);
     if pc^=#0 then
      break;
     { calc argument length }
     quote:=' ';
     argstart:=pc;
     arglen:=0;
     while (pc^<>#0) do
      begin
        case pc^ of
          #1..#32 :
            begin
              if quote<>' ' then
               inc(arglen)
              else
               break;
            end;
          '"' :
            begin
              if quote<>'''' then
               begin
                 if pchar(pc+1)^<>'"' then
                  begin
                    if quote='"' then
                     quote:=' '
                    else
                     quote:='"';
                  end
                 else
                  inc(pc);
               end
              else
               inc(arglen);
            end;
          '''' :
            begin
              if quote<>'"' then
               begin
                 if pchar(pc+1)^<>'''' then
                  begin
                    if quote=''''  then
                     quote:=' '
                    else
                     quote:='''';
                  end
                 else
                  inc(pc);
               end
              else
               inc(arglen);
            end;
          else
            inc(arglen);
        end;
        inc(pc);
      end;
     { copy argument }
     { Don't copy the first one, it is already there.}
     If Count<>0 then
      begin
        allocarg(count,arglen);
        quote:=' ';
        pc:=argstart;
        arg:=argv[count];
        while (pc^<>#0) do
         begin
           case pc^ of
             #1..#32 :
               begin
                 if quote<>' ' then
                  begin
                    arg^:=pc^;
                    inc(arg);
                  end
                 else
                  break;
               end;
             '"' :
               begin
                 if quote<>'''' then
                  begin
                    if pchar(pc+1)^<>'"' then
                     begin
                       if quote='"' then
                        quote:=' '
                       else
                        quote:='"';
                     end
                    else
                     inc(pc);
                  end
                 else
                  begin
                    arg^:=pc^;
                    inc(arg);
                  end;
               end;
             '''' :
               begin
                 if quote<>'"' then
                  begin
                    if pchar(pc+1)^<>'''' then
                     begin
                       if quote=''''  then
                        quote:=' '
                       else
                        quote:='''';
                     end
                    else
                     inc(pc);
                  end
                 else
                  begin
                    arg^:=pc^;
                    inc(arg);
                  end;
               end;
             else
               begin
                 arg^:=pc^;
                 inc(arg);
               end;
           end;
           inc(pc);
         end;
        arg^:=#0;
      end;
 {$IfDef DEBUGARGUMENTS}
     Writeln(stderr,'dos arg ',count,' #',arglen,'#',argv[count],'#');
 {$EndIf}
     inc(count);
   end;
  { get argc and create an nil entry }
  argc:=count;
  allocarg(argc,0);
  { free unused memory }
  sysreallocmem(argv,(argc+1)*sizeof(pointer));
end;

function GetFileHandleCount: longint;
var L1: longint;
    L2: cardinal;
begin
    L1 := 0; (* Don't change the amount, just check. *)
    if DosSetRelMaxFH (L1, L2) <> 0 then GetFileHandleCount := 50
                                                 else GetFileHandleCount := L2;
end;

var TIB: PThreadInfoBlock;
    PIB: PProcessInfoBlock;
    RC: cardinal;
    ErrStr: string;
    P: pointer;

begin
    IsLibrary := FALSE;

    (* Initialize the amount of file handles *)
    FileHandleCount := GetFileHandleCount;
    DosGetInfoBlocks (@TIB, @PIB);
    StackBottom := TIB^.Stack;

    {Set type of application}
    ApplicationType := PIB^.ProcType;
    ProcessID := PIB^.PID;
    ThreadID := TIB^.TIB2^.TID;
    IsConsole := ApplicationType <> 3;

    ExitProc := nil;

    {Initialize the heap.}
    (* Logic is following:
       The heap is initially restricted to low address space (< 512 MB).
       If underlying OS/2 version allows using more than 512 MB per process
       (OS/2 WarpServer for e-Business, eComStation, possibly OS/2 Warp 4.0
       with FP13 and above as well), use of this high memory is allowed for
       future memory allocations at the end of System unit initialization.
       The consequences are that the compiled application can allocate more
       memory, but it must make sure to use direct DosAllocMem calls if it
       needs a memory block for some system API not supporting high memory.
       This is probably no problem for direct calls to these APIs, but
       there might be situations when a memory block needs to be passed
       to a 3rd party DLL which in turn calls such an API call. In case
       of problems usage of high memory can be turned off by setting
       UseHighMem to false - the program should change the setting at its
       very beginning (e.g. in initialization section of the first unit
       listed in the "uses" section) to avoid having preallocated memory
       from the high memory region before changing value of this variable. *)
    InitHeap;

    { ... and exceptions }
    SysInitExceptions;

    { ... and I/O }
    SysInitStdIO;

    { no I/O-Error }
    inoutres:=0;

    {Initialize environment (must be after InitHeap because allocates memory)}
    Environment := pointer (PIB^.Env);
    InitEnvironment;

    CmdLine := pointer (PIB^.Cmd);
    InitArguments;
    DefaultCreator := '';
    DefaultFileType := '';

    InitSystemThreads;
{$ifdef HASVARIANT}
    initvariantmanager;
{$endif HASVARIANT}

{$IFDEF EXTDUMPGROW}
{    Int_HeapSize := high (cardinal);}
{$ENDIF EXTDUMPGROW}
    RC := DosAllocMem (P, 4096, $403);
    if RC = 87 then
(* Using of high memory address space (> 512 MB) *)
(* is not supported on this system.              *)
     UseHighMem := false
    else
     begin
      UseHighMem := true;
      if RC <> 0 then
       begin
        Str (RC, ErrStr);
        ErrStr := 'Error during heap initialization (DosAllocMem - ' + ErrStr + ')!!'#13#10;
        DosWrite (2, @ErrStr [1], Length (ErrStr), RC);
        HandleError (204);
       end
      else
       DosFreeMem (P);
     end;
end.
{
  $Log$
  Revision 1.78  2005-02-06 16:57:18  peter
    * threads for go32v2,os,emx,netware

  Revision 1.77  2004/12/05 14:36:38  hajny
    + GetProcessID added

  Revision 1.76  2004/11/04 09:32:31  peter
  ErrOutput added

  Revision 1.75  2004/10/25 15:38:59  peter
    * compiler defined HEAP and HEAPSIZE removed

  Revision 1.74  2004/09/18 11:12:09  hajny
    * handle type changed to thandle in do_isdevice

  Revision 1.73  2004/09/11 19:43:11  hajny
    * missing MaxExitCode added

  Revision 1.72  2004/07/18 15:20:38  hajny
    + Memory allocation routines changed to support the new memory manager

  Revision 1.71  2004/05/16 18:51:20  peter
    * use thandle in do_*

  Revision 1.70  2004/04/22 21:10:56  peter
    * do_read/do_write addr argument changed to pointer

  Revision 1.69  2004/03/24 19:23:09  hajny
    * misleading warning removed

  Revision 1.68  2004/03/24 19:15:59  hajny
    * heap management modified to be able to grow heap as needed

  Revision 1.67  2004/02/22 15:01:49  hajny
    * lots of fixes (regcall, THandle, string operations in sysutils, longint2cardinal according to OS/2 docs, dosh.inc, ...)

  Revision 1.66  2004/02/16 22:18:44  hajny
    * LastDosExitCode changed back from threadvar temporarily

  Revision 1.65  2004/02/02 03:24:09  yuri
  - prt1.as removed
  - removed tmporary code/comments
  - prt1 compilation error workaround removed

  Revision 1.64  2004/01/25 21:41:48  hajny
    * reformatting of too long comment lines - not accepted by FP IDE

  Revision 1.63  2004/01/21 14:15:42  florian
    * fixed win32 compilation

  Revision 1.62  2004/01/20 23:11:20  hajny
    * ExecuteProcess fixes, ProcessID and ThreadID added

  Revision 1.61  2003/12/04 21:22:38  peter
    * regcall updates (untested)

  Revision 1.60  2003/11/23 07:21:16  yuri
  * native heap

  Revision 1.59  2003/11/19 18:21:11  yuri
  * Memory allocation bug fixed

  Revision 1.58  2003/11/19 16:50:21  yuri
  * Environment and arguments initialization now native

  Revision 1.57  2003/11/06 17:20:44  yuri
  * Unused constants removed

  Revision 1.56  2003/11/03 09:42:28  marco
   * Peter's Cardinal<->Longint fixes patch

  Revision 1.55  2003/11/02 00:51:17  hajny
    * corrections for do_open and os_mode back

  Revision 1.54  2003/10/28 14:57:31  yuri
  * do_* functions now native

  Revision 1.53  2003/10/27 04:33:58  yuri
  * os_mode removed (not required anymore)

  Revision 1.52  2003/10/25 22:45:37  hajny
    * file handling related fixes

  Revision 1.51  2003/10/19 12:13:41  hajny
    * UnusedHandle value made the same as with other targets

  Revision 1.50  2003/10/19 09:37:00  hajny
    * minor fix in non-default sbrk code

  Revision 1.49  2003/10/19 09:06:28  hajny
    * fix for terrible long-time bug in do_open

  Revision 1.48  2003/10/18 16:58:39  hajny
    * stdcall fixes again

  Revision 1.47  2003/10/16 15:43:13  peter
    * THandle is platform dependent

  Revision 1.46  2003/10/14 21:10:06  hajny
    * another longint2cardinal fix

  Revision 1.45  2003/10/13 21:17:31  hajny
    * longint to cardinal corrections

  Revision 1.44  2003/10/12 18:07:30  hajny
    * wrong use of Intel syntax

  Revision 1.43  2003/10/12 17:59:40  hajny
    * wrong use of Intel syntax

  Revision 1.42  2003/10/12 17:52:28  hajny
    * wrong use of Intel syntax

  Revision 1.41  2003/10/12 10:45:36  hajny
    * sbrk error handling corrected

  Revision 1.40  2003/10/07 21:26:35  hajny
    * stdcall fixes and asm routines cleanup

  Revision 1.39  2003/10/06 16:58:27  yuri
  * Another set of native functions.

  Revision 1.38  2003/10/06 14:22:40  yuri
  * Some emx code removed. Now withous so stupid error as with dos ;)

  Revision 1.37  2003/10/04 08:30:59  yuri
  * at&t syntax instead of intel syntax was used

  Revision 1.36  2003/10/03 21:46:41  peter
    * stdcall fixes

  Revision 1.35  2003/10/01 18:42:49  yuri
  * Unclosed comment

  Revision 1.34  2003/09/29 18:39:59  hajny
    * append fix applied to GO32v2, OS/2 and EMX

  Revision 1.33  2003/09/27 11:52:36  peter
    * sbrk returns pointer

  Revision 1.32  2003/03/30 09:20:30  hajny
    * platform extension unification

  Revision 1.31  2003/01/15 22:16:12  hajny
    * default sharing mode changed to DenyNone

  Revision 1.30  2002/12/15 22:41:41  hajny
    * First_Meg fixed + Environment initialization under Dos

  Revision 1.29  2002/12/08 16:39:58  hajny
    - WriteLn in GUI mode support commented out until fixed

  Revision 1.28  2002/12/07 19:17:14  hajny
    * GetEnv correction, better PM support, ...

  Revision 1.27  2002/11/17 22:31:02  hajny
    * type corrections (longint x cardinal)

  Revision 1.26  2002/10/27 14:29:00  hajny
    * heap management (hopefully) fixed

  Revision 1.25  2002/10/14 19:39:17  peter
    * threads unit added for thread support

  Revision 1.24  2002/10/13 09:28:45  florian
    + call to initvariantmanager inserted

  Revision 1.23  2002/09/07 16:01:25  peter
    * old logs removed and tabs fixed

  Revision 1.22  2002/07/01 16:29:05  peter
    * sLineBreak changed to normal constant like Kylix

  Revision 1.21  2002/04/21 15:54:20  carl
  + initialize some global variables

  Revision 1.20  2002/04/12 17:42:16  carl
  + generic stack checking

  Revision 1.19  2002/03/11 19:10:33  peter
    * Regenerated with updated fpcmake

  Revision 1.18  2002/02/10 13:46:20  hajny
    * heap management corrected (heap_brk)

}
