{
 ****************************************************************************

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Free Pascal development team

    Free Pascal - OS/2 runtime library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

****************************************************************************}

unit system;

interface

{$ifdef SYSTEMDEBUG}
  {$define SYSTEMEXCEPTIONDEBUG}
  {.$define IODEBUG}
  {.$define DEBUGENVIRONMENT}
  {.$define DEBUGARGUMENTS}
{$endif SYSTEMDEBUG}

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
  MaxPathLen = 256;
  
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
        CtrlZMarksEOF: boolean = true; (* #26 is considered as end of file *)

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
  DosExit (1{process}, exitcode);
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


threadvar
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

var
(* Initialized by system unit initialization *)
    PIB: PProcessInfoBlock;


procedure InitArguments;
var
  arglen,
  count   : PtrInt;
  argstart,
  pc,arg  : pchar;
  quote   : char;
  argvlen : PtrInt;

  procedure allocarg(idx,len: PtrInt);
    var
      oldargvlen : PtrInt;
    begin
      if idx>=argvlen then
       begin
         oldargvlen:=argvlen;
         argvlen:=(idx+8) and (not 7);
         sysreallocmem(argv,argvlen*sizeof(pointer));
{         fillchar(argv[oldargvlen],(argvlen-oldargvlen)*sizeof(pointer),0);}
       end;
      { use realloc to reuse already existing memory }
      { always allocate, even if length is zero, since }
      { the arg. is still present!                     }
      ArgV [Idx] := SysAllocMem (Succ (Len));
    end;

begin
  CmdLine := SysAllocMem (MaxPathLen);

  ArgV := SysAllocMem (8 * SizeOf (pointer));

  ArgLen := StrLen (PChar (PIB^.Cmd));
  Inc (ArgLen);

  if DosQueryModuleName (PIB^.Handle, MaxPathLen, CmdLine) = 0 then
   ArgVLen := Succ (StrLen (CmdLine))
  else
(* Error occurred - use program name from command line as fallback. *)
   begin
    Move (PIB^.Cmd^, CmdLine, ArgLen);
    ArgVLen := ArgLen;
   end;

{ Get ArgV [0] }
  ArgV [0] := SysAllocMem (ArgVLen);
  Move (CmdLine^, ArgV [0]^, ArgVLen);
  Count := 1;

(* PC points to leading space after program name on command line *)
  PC := PChar (PIB^.Cmd) + ArgLen;

(* ArgLen contains size of command line arguments including leading space. *)
  ArgLen := Succ (StrLen (PC));

  SysReallocMem (CmdLine, ArgVLen + ArgLen);

  Move (PC^, CmdLine [ArgVLen], Succ (ArgLen));

(* ArgV has space for 8 parameters from the first allocation. *)
  ArgVLen := 8;

  { process arguments }
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

function CheckInitialStkLen (StkLen: SizeUInt): SizeUInt;
begin
  CheckInitialStkLen := StkLen;
end;

var TIB: PThreadInfoBlock;
    RC: cardinal;
    ErrStr: string;
    P: pointer;

begin
    IsLibrary := FALSE;

    (* Initialize the amount of file handles *)
    FileHandleCount := GetFileHandleCount;
    DosGetInfoBlocks (@TIB, @PIB);
    StackBottom := TIB^.Stack;
    StackTop := PtrUInt (TIB^.StackLimit);
    StackLength := CheckInitialStkLen (InitialStkLen);

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

    InitArguments;

    DefaultCreator := '';
    DefaultFileType := '';

    InitSystemThreads;
    InitVariantManager;

{$ifdef HASWIDESTRING}
    InitWideStringManager;
{$endif HASWIDESTRING}

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
        if IsConsole then
         DosWrite (2, @ErrStr [1], Length (ErrStr), RC);
        HandleError (204);
       end
      else
       DosFreeMem (P);
     end;
end.
