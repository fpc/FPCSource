{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004-2006 by Karoly Balogh

    AROS conversion
    Copyright (c) 2011 by Marcus Sackrow

    System unit for AROS

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

{$define DISABLE_NO_THREAD_MANAGER}

{$I systemh.inc}
{$I osdebugh.inc}

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
  AllFilesMask = '#?';

const
  UnusedHandle    : THandle = 0;
  StdInputHandle  : THandle = 0;
  StdOutputHandle : THandle = 0;
  StdErrorHandle  : THandle = 0;

  FileNameCaseSensitive : Boolean = False;
  FileNameCasePreserving: boolean = True;
  CtrlZMarksEOF: Boolean = false; (* #26 not considered as end of file *)

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

  BreakOn : Boolean = True;



var
  AOS_ExecBase   : Pointer; external name '_ExecBase';
  AOS_DOSBase    : Pointer;
  AOS_UtilityBase: Pointer;
  AROS_ThreadLib : Pointer; public name 'AROS_THREADLIB';

  ASYS_heapPool : Pointer; { pointer for the OS pool for growing the heap }
  ASYS_origDir  : LongInt; { original directory on startup }
  AOS_wbMsg    : Pointer;
  AOS_ConName  : PChar ='CON:10/30/620/100/FPC Console Output/AUTO/CLOSE/WAIT';
  AOS_ConHandle: THandle;

  argc: LongInt;
  argv: PPChar;
  envp: PPChar;
  killed : Boolean = False;

function GetLibAdress(Base: Pointer; Offset: LongInt): Pointer;
procedure Debug(s: string);
procedure Debugln(s: string);

implementation

{$I system.inc}
{$I osdebug.inc}
type
    PWBArg = ^TWBArg;
    TWBArg = record
        wa_Lock         : LongInt;      { a lock descriptor }
        wa_Name         : PChar;       { a string relative to that lock }
    end;

    WBArgList = array[1..100] of TWBArg; { Only 1..smNumArgs are valid }
    PWBArgList = ^WBArgList;


    PWBStartup = ^TWBStartup;
    TWBStartup = record
        sm_Message      : TMessage;      { a standard message structure }
        sm_Process      : Pointer;   { the process descriptor for you }
        sm_Segment      : Pointer;     { a descriptor for your code }
        sm_NumArgs      : Longint;      { the number of elements in ArgList }
        sm_ToolWindow   : Pointer;       { description of window }
        sm_ArgList      : PWBArgList; { the arguments themselves }
    end;

{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

procedure haltproc(e:longint); cdecl; external name '_haltproc';

procedure System_exit;
var
  oldDirLock: LongInt;
begin
  if Killed then
    Exit;
  Killed := True;
  { Closing opened files }
  CloseList(ASYS_fileList);
  //
  if AOS_wbMsg <> nil then
    ReplyMsg(AOS_wbMsg);
  { Changing back to original directory if changed }
  if ASYS_OrigDir <> 0 then begin
    oldDirLock:=CurrentDir(ASYS_origDir);
    { unlock our lock if its safe, so we won't leak the lock }
    if (oldDirLock<>0) and (oldDirLock<>ASYS_origDir) then
      Unlock(oldDirLock);
  end;
  if AOS_UtilityBase <> nil then
    CloseLibrary(AOS_UtilityBase);
  if ASYS_heapPool <> nil then
    DeletePool(ASYS_heapPool);
  AOS_UtilityBase := nil;
  ASYS_HeapPool := nil;
  //
  if AOS_DOSBase<>nil then
    CloseLibrary(AOS_DOSBase);
  AOS_DOSBase := nil;
  //
  HaltProc(ExitCode);
end;

{ Generates correct argument array on startup }
procedure GenerateArgs;
var
  ArgVLen: LongInt;

  procedure AllocArg(Idx, Len: LongInt);
  var
    i, OldArgVLen : LongInt;
  begin
    if Idx >= ArgVLen then
    begin
      OldArgVLen := ArgVLen;
      ArgVLen := (Idx + 8) and (not 7);
      SysReAllocMem(Argv, Argvlen * SizeOf(Pointer));
      for i := OldArgVLen to ArgVLen - 1 do
        ArgV[i]:=nil;
    end;
    ArgV[Idx] := SysAllocMem(Succ(Len));
  end;

var
  Count: Word;
  Start: Word;
  Ende: Word;
  LocalIndex: Word;
  P : PChar;
  {$H+}
  Temp : string;
  InQuotes: boolean;
begin
  P := GetArgStr;
  ArgVLen := 0;

  { Set argv[0] }
  Temp := ParamStr(0);
  AllocArg(0, Length(Temp));
  Move(Temp[1], Argv[0]^, Length(Temp));
  Argv[0][Length(Temp)] := #0;

  { check if we're started from Workbench }
  if AOS_wbMsg <> nil then
  begin
    ArgC := 0;
    Exit;
  end;

  InQuotes := False;
  { Handle the other args }
  Count := 0;
  { first index is one }
  LocalIndex := 1;
  while (P[Count] <> #0) do
  begin
    while (p[count]=' ') or (p[count]=#9) or (p[count]=LineEnding) do
      Inc(count);
    if p[count] = '"' then
    begin
      inQuotes := True;
      Inc(Count);
    end;
    start := count;
    if inQuotes then
    begin
      while (p[count]<>#0) and (p[count]<>'"') and (p[count]<>LineEnding) do
      begin
        Inc(Count) 
      end;
    end else
    begin
      while (p[count]<>#0) and (p[count]<>' ') and (p[count]<>#9) and (p[count]<>LineEnding) do
        inc(count);
    end;
    ende := count;
    if not inQuotes then
    begin
      while (p[start]=' ') and (Start < Ende) do
        Inc(Start)
    end;
    if (ende-start>0) then
    begin
      allocarg(localindex,ende-start);
      move(p[start],argv[localindex]^,ende-start);
      argv[localindex][ende-start]:=#0;
      if inQuotes and (argv[localindex][(ende-start) - 1] = '"') then
        argv[localindex][(ende-start)-1] := #0;
      inc(localindex);
    end;
    if inQuotes and (p[count] = '"') then
      Inc(Count);
    inQuotes := False; 
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
  SetLength(s1, 256);
  FillChar(s1,255,#0);
  { GetLock of program directory }

  alock:=GetProgramDir;
  if alock<>0 then begin
    if NameFromLock(alock,@s1[1],255) then begin
      counter:=1;
      while (s1[counter]<>#0) and (counter<>0) do Inc(counter);
      SetLength(s1, counter-1);
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
  SetLength(s1, 256);
  FillChar(s1,255,#0);

  if GetProgramName(@s1[1],255) then begin
    { now check out and assign the length of the string }
    counter := 1;
    while (s1[counter]<>#0) and (counter<>0) do Inc(counter);
    SetLength(s1, counter-1);

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

function GetWBArgsNum: Integer;
var
  startup: PWBStartup;
begin
  GetWBArgsNum := 0;
  Startup := nil;
  Startup := PWBStartup(AOS_wbMsg);
  if Startup <> nil then
  begin
    Result := Startup^.sm_NumArgs - 1;
  end;
end;

function GetWBArg(Idx: Integer): string;
var
  startup: PWBStartup;
  wbarg: PWBArgList;
  Path: array[0..254] of Char;
  strPath: string;
  Len: Integer;
begin
  GetWBArg := '';
  FillChar(Path[0],255,#0);
  Startup := PWBStartup(AOS_wbMsg);
  if Startup <> nil then
  begin
    //if (Idx >= 0) and (Idx < Startup^.sm_NumArgs) then
    begin
      wbarg := Startup^.sm_ArgList;
      if NameFromLock(wbarg^[Idx + 1].wa_Lock,@Path[0],255) then
      begin
        Len := 0;
        while (Path[Len] <> #0) and (Len < 254) do
          Inc(Len);
        if Len > 0 then
          if (Path[Len - 1] <> ':') and (Path[Len - 1] <> '/') then
            Path[Len] := '/';
        strPath := Path;
      end;
      Result := strPath + wbarg^[Idx + 1].wa_Name;
    end;
  end;
end;

{ number of args }
function paramcount : longint;
begin
  if AOS_wbMsg<>nil then
    paramcount:=GetWBArgsNum
  else
    paramcount:=argc-1;
end;

{ argument number l }
function paramstr(l : longint) : string;
var
  s1: String;
begin
  paramstr:='';
  if AOS_wbMsg<>nil then
  begin
    paramstr := GetWBArg(l);
  end else
  begin
    if l=0 then begin
      s1:=GetProgDir;
      if s1[length(s1)]=':' then paramstr:=s1+GetProgramName
                            else paramstr:=s1+'/'+GetProgramName;
    end else begin
      if (l>0) and (l+1<=argc) then paramstr:=strpas(argv[l]);
    end;
 end;
end;

{ set randseed to a new pseudo random value }
procedure Randomize;
var
  tmpTime: TDateStamp;
begin
  DateStamp(@tmpTime);
  randseed := tmpTime.ds_tick;
end;




{ AmigaOS specific startup }
procedure SysInitAmigaOS;
var
  self: PProcess;
begin
  self := PProcess(FindTask(nil));
  if self^.pr_CLI = NIL then begin
    { if we're running from Ambient/Workbench, we catch its message }
    WaitPort(@self^.pr_MsgPort);
    AOS_wbMsg:=GetMsg(@self^.pr_MsgPort);
  end;

  AOS_DOSBase := OpenLibrary('dos.library', 0);
  if AOS_DOSBase = nil then
    Halt(1);
  AOS_UtilityBase := OpenLibrary('utility.library', 0);
  if AOS_UtilityBase = nil then
    Halt(1);
    
  { Creating the memory pool for growing heap }
  ASYS_heapPool := CreatePool(MEMF_ANY or MEMF_SEM_PROTECTED, growheapsize2, growheapsize1);
  if ASYS_heapPool = nil then
    Halt(1);
  
  if AOS_wbMsg = nil then begin
    StdInputHandle := THandle(dosInput);
    StdOutputHandle := THandle(dosOutput);
    StdErrorHandle := THandle(DosError1);
  end else begin
    AOS_ConHandle := Open(AOS_ConName, MODE_OLDFILE);
    if AOS_ConHandle <> 0 then begin
      StdInputHandle := AOS_ConHandle;
      StdOutputHandle := AOS_ConHandle;
      StdErrorHandle := AOS_ConHandle;
    end else
      Halt(1);
  end;
end;


procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
end;

function GetProcessID: SizeUInt;
begin
  GetProcessID := SizeUInt(FindTask(NIL));
end;

function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;
begin
  result := stklen;
end;

begin
  IsConsole := TRUE;
  SysResetFPU;
  if not (IsLibrary) then
    SysInitFPU;
  StackLength := CheckInitialStkLen(InitialStkLen);
  StackBottom := Sptr - StackLength;
{ OS specific startup }
  AOS_wbMsg := nil;
  ASYS_origDir := 0;
  ASYS_fileList := nil;
  envp := nil;
  SysInitAmigaOS;
{ Set up signals handlers }
  //InstallSignals;
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
