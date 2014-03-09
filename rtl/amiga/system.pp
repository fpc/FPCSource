{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004-2006 by Karoly Balogh

    System unit for AmigaOS 3.x/4.x

    Uses parts of the Free Pascal 1.0.x for Commodore Amiga/68k port
    by Carl Eric Codere and Nils Sjoholm

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit System;

interface

{$define FPC_IS_SYSTEM}

{$I systemh.inc}

{$ifdef cpum68k}
{$define fpc_softfpu_interface}
{$i softfpu.pp}
{$undef fpc_softfpu_interface}
{$endif cpum68k}

const
  LineEnding = #10;
  LFNSupport = True;
  DirectorySeparator = '/';
  DriveSeparator = ':';
  ExtensionSeparator = '.';
  PathSeparator = ';';
  AllowDirectorySeparators : set of char = ['\','/'];
  AllowDriveSeparators : set of char = [':'];
  maxExitCode = 255;
  MaxPathLen = 256;
  AllFilesMask = '*';

const
  UnusedHandle    : LongInt = -1;
  StdInputHandle  : LongInt = 0;
  StdOutputHandle : LongInt = 0;
  StdErrorHandle  : LongInt = 0;

  FileNameCaseSensitive : Boolean = False;
  FileNameCasePreserving: boolean = false;
  CtrlZMarksEOF: boolean = false; (* #26 not considered as end of file *)

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

  BreakOn : Boolean = True;


var
  AOS_ExecBase   : Pointer; external name '_ExecBase';
  AOS_DOSBase    : Pointer;
  AOS_UtilityBase: Pointer;

{$IFDEF AMIGAOS4}
{$WARNING iExec, iDOS and iUtility should be typed pointer later}
var
  IExec : Pointer; external name '_IExec';
  IDOS : Pointer;
  IUtility : Pointer;
{$ENDIF}

  AOS_heapPool : Pointer; { pointer for the OS pool for growing the heap }
  AOS_origDir  : LongInt; { original directory on startup }
  AOS_wbMsg    : Pointer;
  AOS_ConName  : PChar ='CON:10/30/620/100/FPC Console Output/AUTO/CLOSE/WAIT';
  AOS_ConHandle: LongInt;

  argc: LongInt;
  argv: PPChar;
  envp: PPChar;


implementation

{$ifdef cpum68k}
{$define fpc_softfpu_implementation}
{$i softfpu.pp}
{$undef fpc_softfpu_implementation}

{ we get these functions and types from the softfpu code }
{$define FPC_SYSTEM_HAS_float64}
{$define FPC_SYSTEM_HAS_float32}
{$define FPC_SYSTEM_HAS_flag}
{$define FPC_SYSTEM_HAS_extractFloat64Frac0}
{$define FPC_SYSTEM_HAS_extractFloat64Frac1}
{$define FPC_SYSTEM_HAS_extractFloat64Exp}
{$define FPC_SYSTEM_HAS_extractFloat64Sign}
{$define FPC_SYSTEM_HAS_ExtractFloat32Frac}
{$define FPC_SYSTEM_HAS_extractFloat32Exp}
{$define FPC_SYSTEM_HAS_extractFloat32Sign}
{$endif cpum68k}

{$I system.inc}

{$IFDEF MOSFPC_FILEDEBUG}
{$WARNING Compiling with file debug enabled!}
{$ENDIF}

{$IFDEF MOSFPC_MEMDEBUG}
{$WARNING Compiling with memory debug enabled!}
{$ENDIF}


{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

procedure haltproc(e:longint);cdecl;external name '_haltproc';

procedure System_exit;
begin
  { We must remove the CTRL-C FLAG here because halt }
  { may call I/O routines, which in turn might call  }
  { halt, so a recursive stack crash                 }
  if BreakOn then begin
    if (SetSignal(0,0) and SIGBREAKF_CTRL_C)<>0 then
      SetSignal(0,SIGBREAKF_CTRL_C);
  end;

  { Closing opened files }
  CloseList(AOS_fileList);

  { Changing back to original directory if changed }
  if AOS_origDir<>0 then begin
    CurrentDir(AOS_origDir);
  end;

{$IFDEF AMIGAOS4}
  if iDOS<>nil then DropInterface(iDOS);
  if iUtility<>nil then DropInterface(iUtility);
{$ENDIF}

  if AOS_UtilityBase<>nil then CloseLibrary(AOS_UtilityBase);
  if AOS_DOSBase<>nil then CloseLibrary(AOS_DOSBase);
  if AOS_heapPool<>nil then DeletePool(AOS_heapPool);

  { If in Workbench mode, replying WBMsg }
  if AOS_wbMsg<>nil then begin
    Forbid;
    ReplyMsg(AOS_wbMsg);
  end;

  haltproc(ExitCode);
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
      ArgV [Idx] := SysAllocMem (Succ (Len));
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
  if AOS_wbMsg<>nil then
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


{*****************************************************************************
                             ParamStr/Randomize
*****************************************************************************}

{ number of args }
function paramcount : longint;
begin
  if AOS_wbMsg<>nil then
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
  if AOS_wbMsg<>nil then exit;

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


{ AmigaOS specific startup }
procedure SysInitAmigaOS;
var self: PProcess;
begin
  self:=PProcess(FindTask(nil));
  if self^.pr_CLI=0 then begin
    { if we're running from Ambient/Workbench, we catch its message }
    WaitPort(@self^.pr_MsgPort);
    AOS_wbMsg:=GetMsg(@self^.pr_MsgPort);
  end;

  AOS_DOSBase:=OpenLibrary('dos.library',50);
  if AOS_DOSBase=nil then Halt(1);
  AOS_UtilityBase:=OpenLibrary('utility.library',50);
  if AOS_UtilityBase=nil then Halt(1);

{$IFDEF AMIGAOS4}
  { Open main interfaces on OS4 }
  iDOS := GetInterface(AOS_DOSBase,'main',1,nil);
  iUtility := GetInterface(AOS_UtilityBase,'main',1,nil);
{$ENDIF}

  { Creating the memory pool for growing heap }
  AOS_heapPool:=CreatePool(MEMF_FAST,growheapsize2,growheapsize1);
  if AOS_heapPool=nil then Halt(1);

  if AOS_wbMsg=nil then begin
    StdInputHandle:=dosInput;
    StdOutputHandle:=dosOutput;
  end else begin
    AOS_ConHandle:=Open(AOS_ConName,MODE_OLDFILE);
    if AOS_ConHandle<>0 then begin
      StdInputHandle:=AOS_ConHandle;
      StdOutputHandle:=AOS_ConHandle;
    end else
      Halt(1);
  end;
end;


procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);

  { * AmigaOS doesn't have a separate stderr * }

  StdErrorHandle:=StdOutputHandle;
  //OpenStdIO(StdErr,fmOutput,StdErrorHandle);
  //OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
end;

function GetProcessID: SizeUInt;
begin
  GetProcessID:=SizeUInt(FindTask(NIL));
end;

function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;
begin
  result := stklen;
end;


begin
  IsConsole := TRUE;
  SysResetFPU;
  if not(IsLibrary) then
    SysInitFPU;
  StackLength := CheckInitialStkLen(InitialStkLen);
  StackBottom := Sptr - StackLength;
{ OS specific startup }
  AOS_wbMsg:=nil;
  AOS_origDir:=0;
  AOS_fileList:=nil;
  envp:=nil;
  SysInitAmigaOS;
{ Set up signals handlers }
//  InstallSignals;
{ Setup heap }
  InitHeap;
  SysInitExceptions;
  initunicodestringmanager;
{ Setup stdin, stdout and stderr }
  SysInitStdIO;
{ Reset IO Error }
  InOutRes:=0;
{ Arguments }
  GenerateArgs;
  InitSystemThreads;
  initvariantmanager;
end.
