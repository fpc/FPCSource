{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}
unit process;

interface

Uses Classes,
     pipes,
{$ifdef Unix}
{$ifdef ver1_0}
     Linux,
{$else}
     unix,
{$endif}     
{$else}          
     Windows,
{$endif}     
     SysUtils;

Type
  TProcessOption = (poRunSuspended,poWaitOnExit,
                    poUsePipes,poStderrToOutPut,
                    poNoConsole,poNewConsole,
                    poDefaultErrorMode,poNewProcessGroup,
                    poDebugProcess,poDebugOnlyThisProcess);

  TShowWindowOptions = (swoNone,swoHIDE,swoMaximize,swoMinimize,swoRestore,swoShow,
                        swoShowDefault,swoShowMaximized,swoShowMinimized,
                        swoshowMinNOActive,swoShowNA,swoShowNoActivate,swoShowNormal);

  TStartupOption = (suoUseShowWindow,suoUseSize,suoUsePosition,
                    suoUseCountChars,suoUseFillAttribute);

  TProcessPriority = (ppHigh,ppIdle,ppNormal,ppRealTime);

  TProcessOptions = Set of TPRocessOption;
  TstartUpoptions = set of TStartupOption;

{$ifdef unix}
Const
  STARTF_USESHOWWINDOW    = 1;    // Ignored
  STARTF_USESIZE          = 2;
  STARTF_USEPOSITION      = 4;
  STARTF_USECOUNTCHARS    = 8;    // Ignored
  STARTF_USEFILLATTRIBUTE = $10;
  STARTF_RUNFULLSCREEN    = $20;  // Ignored
  STARTF_FORCEONFEEDBACK  = $40;  // Ignored
  STARTF_FORCEOFFFEEDBACK = $80;  // Ignored
  STARTF_USESTDHANDLES    = $100; // Ignored
  STARTF_USEHOTKEY        = $200; // Ignored

Type
  PProcessInformation = ^TProcessInformation;
  TProcessInformation = record
    hProcess: THandle;
    hThread: THandle;
    dwProcessId: DWORD;
    dwThreadId: DWORD;
  end;

  PStartupInfo = ^TStartupInfo;
  TStartupInfo = Record
    cb: DWORD;
    lpReserved: Pointer;
    lpDesktop: Pointer;
    lpTitle: Pointer;
    dwX: DWORD;
    dwY: DWORD;
    dwXSize: DWORD;
    dwYSize: DWORD;
    dwXCountChars: DWORD;
    dwYCountChars: DWORD;
    dwFillAttribute: DWORD;
    dwFlags: DWORD;
    wShowWindow: Word;
    cbReserved2: Word;
    lpReserved2: PByte;
    hStdInput: THandle;
    hStdOutput: THandle;
    hStdError: THandle;
  end;
   
  PSecurityAttributes = ^TSecurityAttributes;
  TSecurityAttributes = Record
    nlength : Integer; 
    lpSecurityDescriptor : Pointer;
    BinheritHandle : Boolean;
  end;  

Const piInheritablePipe : TSecurityAttributes = (
                           nlength:SizeOF(TSecurityAttributes);
                           lpSecurityDescriptor:Nil;
                           Binherithandle:True);
      piNonInheritablePipe : TSecurityAttributes = (
                             nlength:SizeOF(TSecurityAttributes);
                             lpSecurityDescriptor:Nil;
                             Binherithandle:False);

{$endif}
Type

  TProcess = Class (TComponent)
  Private
{$ifndef unix}  
    FAccess : Cardinal;
{$endif}    
    FApplicationName : string;
    FChildErrorStream : TOutPutPipeStream;
    FChildInputSTream : TInputPipeStream;
    FChildOutPutStream : TOutPutPipeStream;
    FConsoleTitle : String;
    FProcessOptions : TProcessOptions;
    FStartUpOptions : TStartupOptions;
    FCommandLine : String;
    FCurrentDirectory : String;
    FDeskTop : String;
    FEnvironment : Tstrings;
    FExitCode : Cardinal;
    FHandle : THandle;
    FShowWindow : TShowWindowOptions;
    FInherithandles : LongBool;
    FParentErrorStream : TInputPipeStream;
    FParentInputSTream : TInputPipeStream;
    FParentOutputStream : TOutPutPipeStream;
    FRunning : Boolean;
    FThreadAttributes  : PSecurityAttributes;
    FProcessAttributes : PSecurityAttributes;
    FProcessInformation : TProcessInformation;
    FPRocessPriority : TProcessPriority;
    FStartupInfo : TStartupInfo;
    Procedure FreeStreams;
    Function  GetExitStatus : Integer;
    Function  GetHandle : THandle;
    Function  GetRunning : Boolean;
    Function  GetProcessAttributes : TSecurityAttributes;
    Function  GetThreadAttributes : TSecurityAttributes;
    Procedure SetProcessAttributes (Value : TSecurityAttributes);
    Procedure SetThreadAttributes (Value : TSecurityAttributes);
    Function  GetWindowRect : TRect;
    Procedure SetWindowRect (Value : TRect);
    Procedure SetFillAttribute (Value : Cardinal);
    Procedure SetShowWindow (Value : TShowWindowOptions);
    Procedure SetWindowColumns (Value : Cardinal);
    Procedure SetWindowHeight (Value : Cardinal);
    Procedure SetWindowLeft (Value : Cardinal);
    Procedure SetWindowRows (Value : Cardinal);
    Procedure SetWindowTop (Value : Cardinal);
    Procedure SetWindowWidth (Value : Cardinal);
    procedure CreateStreams;
    function GetCreationFlags: Cardinal;
    function GetStartupFlags: Cardinal;
    procedure SetApplicationname(const Value: String);
    procedure SetPRocessOptions(const Value: TProcessOptions);
    procedure SetActive(const Value: Boolean);
    procedure SetEnvironment(const Value: TStrings);
{$ifdef unix}
    function PeekLinuxExitStatus: Boolean;
{$endif}    
  Public
    Constructor Create (AOwner : TComponent);override;
    Destructor Destroy; override;
    Procedure Execute; virtual;
    Function Resume : Integer; virtual;
    Function Suspend : Integer; virtual;
    Function Terminate (AExitCode : Integer): Boolean; virtual;
    Function WaitOnExit : DWord;
    Property WindowRect : Trect Read GetWindowRect Write SetWindowRect;
    Property StartupInfo : TStartupInfo Read FStartupInfo;
    Property ProcessAttributes : TSecurityAttributes  Read GetProcessAttributes  Write SetProcessAttributes;
    Property ProcessInformation : TProcessInformation Read FPRocessInformation;
    Property Handle : THandle Read FProcessInformation.hProcess;
    Property ThreadHandle : THandle Read FprocessInformation.hThread;
    Property Input  : TOutPutPipeStream Read FParentOutPutStream;
    Property OutPut : TInputPipeStream  Read FParentInputStream;
    Property StdErr : TinputPipeStream  Read FParentErrorStream;
    Property ExitStatus : Integer Read GetExitStatus;
    Property InheritHandles : LongBool Read FInheritHandles Write FInheritHandles;
    Property ThreadAttributes : TSecurityAttributes Read GetThreadAttributes Write SetThreadAttributes;
  Published
    Property Active : Boolean Read Getrunning Write SetActive;
    Property ApplicationName : String Read FApplicationname Write SetApplicationname;
    Property CommandLine : String Read FCommandLine Write FCommandLine;
    Property ConsoleTitle : String Read FConsoleTitle Write FConsoleTitle;
    Property CurrentDirectory : String Read FCurrentDirectory Write FCurrentDirectory;
    Property DeskTop : String Read FDeskTop Write FDeskTop;
    Property Environment : TStrings Read FEnvironment Write SetEnvironment;
    Property FillAttribute : Cardinal Read FStartupInfo.dwFillAttribute Write SetFillAttribute;
    Property Options : TProcessOptions Read FProcessOptions Write SetPRocessOptions;
    Property Priority : TProcessPriority Read FProcessPriority Write FProcessPriority;
    Property StartUpOptions : TStartUpOptions Read FStartUpOptions Write FStartupOptions;
    Property Running : Boolean Read GetRunning;
    Property ShowWindow : TShowWindowOptions Read FShowWindow Write SetShowWindow;
    Property WindowColumns : Cardinal Read FStartupInfo.dwXCountchars Write SetWindowColumns;
    Property WindowHeight : Cardinal Read FStartupInfo.dwYsize Write SetWindowHeight;
    Property WindowLeft : Cardinal Read FStartupInfo.dwx Write SetWindowLeft;
    Property WindowRows : Cardinal Read FStartupInfo.dwYcountChars Write SetWindowRows;
    Property WindowTop : Cardinal Read FStartupInfo.dwy Write SetWindowTop ;
    Property WindowWidth : Cardinal Read FStartupInfo.dwXsize Write SetWindowWidth;
  end;

{$ifdef unix}
Const
  PriorityConstants : Array [TProcessPriority] of Integer =
                      (20,20,0,-20);

Const
  GeometryOption : String = '-geometry';
  TitleOption : String ='-title';

{$else}
Const
  PriorityConstants : Array [TProcessPriority] of Cardinal =
                      (HIGH_PRIORITY_CLASS,IDLE_PRIORITY_CLASS,
                       NORMAL_PRIORITY_CLASS,REALTIME_PRIORITY_CLASS);
{$endif}
implementation

Constructor TProcess.Create (AOwner : TComponent);
begin
  Inherited;
{$ifndef unix}
  FAccess:=PROCESS_ALL_ACCESS;
{$endif}
  FProcessPriority:=ppNormal;
  FShowWindow:=swoNone;
  FStartupInfo.cb:=SizeOf(TStartupInfo);
  FInheritHandles:=True;
  FEnvironment:=TStringList.Create;
end;

Destructor TProcess.Destroy;

begin
  If assigned (FProcessAttributes) then Dispose (FPRocessAttributes);
  If assigned (FThreadAttributes) then Dispose (FThreadAttributes);
  FEnvironment.Free;
  FreeStreams;
  Inherited;
end;

Procedure TProcess.FreeStreams;

var FreedStreams: TList;

  procedure FreeStream(var AnObject: TObject);

  begin
    if FreedStreams.IndexOf(AnObject)<0 then 
      begin
      FreedStreams.Add(AnObject);
      AnObject.Free;
      end;
    AnObject:=nil;
  end;
                              
begin
  FreedStreams:=TList.Create;
  try
    FreeStream(FParentErrorStream);
    FreeStream(FParentInputStream);
    FreeStream(FParentOutputStream);
    FreeStream(FChildErrorStream);
    FreeStream(FChildInputStream);
    FreeStream(FChildOutputStream);
  finally
    FreedStreams.Free;
  end;
end;

Function TProcess.GetExitStatus : Integer;

begin
  If FRunning then
{$ifdef unix}
    PeekLinuxExitStatus;
{$else}
    GetExitCodeProcess(Handle,FExitCode);
{$endif}
  Result:=FExitCode;
end;

Function TProcess.GetHandle : THandle;

begin
{$ifndef unix}
  If FHandle=0 Then
    FHandle:=OpenProcess (FAccess,True,FProcessInformation.dwProcessId);
{$endif}
  Result:=FHandle
end;

Function TProcess.GetProcessAttributes : TSecurityAttributes;

Var P : PSecurityAttributes;

begin
  IF not Assigned(FProcessAttributes) then
    begin
    // Provide empty dummy value;
    New(p);
    Fillchar(p^,Sizeof(TSecurityAttributes),0);
    Result:=p^;
    end
  else
    REsult:=FProcessAttributes^;
end;

{$ifdef unix}
Function TProcess.PeekLinuxExitStatus : Boolean;

begin
  Result:=WaitPID(Handle,@FExitCode,WNOHANG)=Handle;
  If Result then
    FExitCode:=wexitstatus(FExitCode)
  else
    FexitCode:=0;
end;
{$endif}

Function TProcess.GetRunning : Boolean;

begin
  IF FRunning then
    begin
{$ifdef unix}
    FRunning:=Not PeekLinuxExitStatus;
{$else}
    Frunning:=GetExitStatus=Still_Active;
{$endif}
    end;
  Result:=FRunning;
end;

Function TProcess.GetThreadAttributes : TSecurityAttributes;

Var P : PSecurityAttributes;

begin
  IF not Assigned(FThreadAttributes) then
    begin
    // Provide empty dummy value;
    New(p);
    Fillchar(p^,Sizeof(TSecurityAttributes),0);
    Result:=p^;
    end
  else
    Result:=FThreadAttributes^;
end;

Procedure TProcess.SetProcessAttributes (Value : TSecurityAttributes);

begin
  If not Assigned (FProcessAttributes) then
    New(FProcessAttributes);
  FPRocessAttributes^:=VAlue;
end;

Procedure TProcess.SetThreadAttributes (Value : TSecurityAttributes);

begin
  If not Assigned (FThreadAttributes) then
    New(FThreadAttributes);
  FThreadAttributes^:=VAlue;
end;

Procedure TProcess.CreateStreams;

begin
  FreeStreams;
  CreatePipeStreams (FChildInputSTream,FParentOutPutStream); //,@piInheritablePipe,1024);
  CreatePipeStreams (FParentInputStream,FChildOutPutStream); //,@piInheritablePipe,1024);
  if Not (poStdErrToOutPut in FProcessOptions) then
    CreatePipeStreams (FParentErrorStream,FChildErrorStream) //,@piInheritablePipe,1024)
  else
    begin
    FChildErrorStream:=FChildOutPutStream;
    FParentErrorStream:=FParentInputStream;
    end;
  FStartupInfo.dwFlags:=FStartupInfo.dwFlags or Startf_UseStdHandles;
  FStartupInfo.hStdInput:=FChildInputStream.Handle;
  FStartupInfo.hStdOutput:=FChildOutPutStream.Handle;
  FStartupInfo.hStdError:=FChildErrorStream.Handle;
end;

Function TProcess.GetCreationFlags : Cardinal;

begin
  Result:=0;
{$ifndef unix}
  if poNoConsole in FProcessOptions then
    Result:=Result or Detached_Process;
  if poNewConsole in FProcessOptions then
    Result:=Result or Create_new_console;
  if poNewProcessGroup in FProcessOptions then
    Result:=Result or CREATE_NEW_PROCESS_GROUP;
  If poRunSuspended in FProcessOptions Then
    Result:=Result or Create_Suspended;
  if poDebugProcess in FProcessOptions Then
    Result:=Result or DEBUG_PROCESS;
  if poDebugOnlyThisProcess in FProcessOptions Then
    Result:=Result or DEBUG_ONLY_THIS_PROCESS;
  if poDefaultErrorMode in FProcessOptions Then
    Result:=Result or CREATE_DEFAULT_ERROR_MODE;
  result:=result or PriorityConstants[FProcessPriority];
{$endif}
end;

Function TProcess.GetStartupFlags : Cardinal;

begin
  Result:=0;
  if poUsePipes in FProcessOptions then
     Result:=Result or Startf_UseStdHandles;
  if suoUseShowWindow in FStartupOptions then
    Result:=Result or startf_USESHOWWINDOW;
  if suoUSESIZE in FStartupOptions then
    Result:=Result or startf_usesize;
  if suoUsePosition in FStartupOptions then
    Result:=Result or startf_USEPOSITION;
  if suoUSECOUNTCHARS in FStartupoptions then
    Result:=Result or startf_usecountchars;
  if suoUsefIllAttribute in FStartupOptions then
    Result:=Result or startf_USEFILLATTRIBUTE;
end;

Type
{$ifndef unix}
  PPChar = ^PChar;
{$endif}
  TPCharArray = Array[Word] of pchar;
  PPCharArray = ^TPcharArray;


Function StringsToPCharList(List : TStrings) : PPChar;

Var
  I : Integer;
  S : String;

begin
  I:=(List.Count)+1;
  GetMem(Result,I*sizeOf(PChar));
  PPCharArray(Result)^[List.Count]:=Nil;
  For I:=0 to List.Count-1 do
    begin
    S:=List[i];
    Result[i]:=StrNew(PChar(S));
    end;
end;

Procedure FreePCharList(List : PPChar);

Var
  I : integer;

begin
  I:=0;
  While List[i]<>Nil do
    begin
    StrDispose(List[i]);
    Inc(I);
    end;
  FreeMem(List);
end;


{$ifdef unix}
Procedure CommandToList(S : String; List : TStrings);

  Function GetNextWord : String;

  Const
    WhiteSpace = [' ',#8,#10];
    Literals = ['"',''''];

  Var
    Wstart,wend : Integer;
    InLiteral : Boolean;
    LastLiteral : char;

  begin
    WStart:=1;
    While (WStart<=Length(S)) and (S[WStart] in WhiteSpace) do
      Inc(WStart);
    WEnd:=WStart;
    InLiteral:=False;
    LastLiteral:=#0;
    While (Wend<=Length(S)) and (Not (S[Wend] in WhiteSpace) or InLiteral) do
      begin
      if S[Wend] in Literals then
        If InLiteral then
          InLiteral:=Not (S[Wend]=LastLiteral)
        else
          begin
          InLiteral:=True;
          LastLiteral:=S[Wend];
          end;
       inc(wend);
       end;
     Result:=Copy(S,WStart,WEnd-WStart);
     Result:=StringReplace(Result,'"','',[rfReplaceAll]);
     Result:=StringReplace(Result,'''','',[rfReplaceAll]);
     While (WEnd<=Length(S)) and (S[Wend] in WhiteSpace) do
       inc(Wend);
     Delete(S,1,WEnd-1);
     
  end;

Var
  W : String;

begin
  While Length(S)>0 do
    begin
    W:=GetNextWord;
    If (W<>'') then
      List.Add(W);
    end;
end;


Function MakeCommand(Var AppName,CommandLine : String;
                     StartupOptions : TStartUpOptions;
                     ProcessOptions : TProcessOptions;
                     StartupInfo : TStartupInfo) : PPchar;
Const
  SNoCommandLine = 'Cannot execute empty command-line';

Var
  S  : TStringList;
  G : String;

begin
  if (AppName='') then
    begin
    If (CommandLine='') then
      Raise Exception.Create(SNoCommandline)
    end
  else
    begin
    If (CommandLine='') then
      CommandLine:=AppName;
    end;
  S:=TStringList.Create;
  try
    CommandToList(CommandLine,S);
    if poNewConsole in ProcessOptions then
      begin
      S.Insert(0,'-e');
      If (AppName<>'') then
        begin
        S.Insert(0,AppName);
        S.Insert(0,'-title');
        end;
      if suoUseCountChars in StartupOptions then
        With StartupInfo do
          begin
          S.Insert(0,Format('%dx%d',[dwXCountChars,dwYCountChars]));
          S.Insert(0,'-geometry');
          end;
      S.Insert(0,'xterm');
      end;
    if (AppName<>'') then
      begin
      S.Add(TitleOption);
      S.Add(AppName);
      end;
    With StartupInfo do
      begin
      G:='';
      if (suoUseSize in StartupOptions) then
        g:=format('%dx%d',[dwXSize,dwYsize]);
      if (suoUsePosition in StartupOptions) then
        g:=g+Format('+%d+%d',[dwX,dwY]);
      if G<>'' then
        begin
        S.Add(GeometryOption);
        S.Add(g);
        end;
      end;
    Result:=StringsToPcharList(S);
    AppName:=S[0];
  Finally
    S.free;
  end;
end;

Function CreateProcess (PName,PCommandLine,PDir : String;
                        FEnv : PPChar;
                        StartupOptions : TStartupOptions;
                        ProcessOptions : TProcessOptions;
                        const FStartupInfo : TStartupInfo;
                        Var ProcessInfo : TProcessInformation)  : boolean;

Var
  PID : Longint;
  Argv : PPChar;
  fd : Integer;

begin
  Result:=True;
  Argv:=MakeCommand(Pname,PCommandLine,StartupOptions,ProcessOptions,FStartupInfo);
  if (pos('/',PName)<>1) then
    PName:=FileSearch(Pname,GetEnv('PATH'));
  Pid:=fork;
  if Pid=0 then
   begin
   { We're in the child }
   if (PDir<>'') then
     ChDir(PDir);
   if PoUsePipes in ProcessOptions then
     begin
     dup2(FStartupInfo.hStdInput,0);
     dup2(FStartupInfo.hStdOutput,1);
     dup2(FStartupInfo.hStdError,2);
     end
   else if poNoConsole in ProcessOptions then
     begin
     fd:=FileOpen('/dev/null',fmOpenReadWrite);
     dup2(fd,0);
     dup2(fd,1);
     dup2(fd,2);
     end;
   if (poRunSuspended in ProcessOptions) then
     sigraise(SIGSTOP);
   if FEnv<>Nil then
     Execve(PChar(PName),Argv,Fenv)
   else
     Execv(Pchar(PName),argv);
   Halt(127);
   end
 else
   begin
   FreePcharList(Argv);
   // Copy process information.
   ProcessInfo.hProcess:=PID;
   ProcessInfo.hThread:=PID;
   ProcessInfo.dwProcessId:=PID;
   ProcessInfo.dwThreadId:=PID;
   end;
end;
{$endif}

{$ifdef unix}
Function GetLastError : Integer;

begin
  Result:=-1;
end;
{$endif}

Procedure TProcess.Execute;


Var
{$ifndef unix}
  PName,PDir,PCommandLine : PChar;
{$endif}
  FEnv : PPChar;
  FCreationFlags : Cardinal;

begin
  If poUsePipes in FProcessOptions then
    CreateStreams;
  FCreationFlags:=GetCreationFlags;
  FStartupInfo.dwFlags:=GetStartupFlags;
{$ifndef unix}
  PName:=Nil;
  PCommandLine:=Nil;
  PDir:=Nil;
  If FApplicationName<>'' then
    PName:=Pchar(FApplicationName);
  If FCommandLine<>'' then
    PCommandLine:=Pchar(FCommandLine);
  If FCurrentDirectory<>'' then
    PDir:=Pchar(FCurrentDirectory);
{$endif}
  if FEnvironment.Count<>0 then
    FEnv:=StringsToPcharList(FEnvironment)
  else
    FEnv:=Nil;
  FInheritHandles:=True;
{$ifdef unix}
  if Not CreateProcess (FApplicationName,FCommandLine,FCurrentDirectory,FEnv,
                        FStartupOptions,FProcessOptions,FStartupInfo,
                        fProcessInformation) then
{$else}
  If Not CreateProcess (PName,PCommandLine,FProcessAttributes,FThreadAttributes,
                 FInheritHandles,FCreationFlags,FEnv,PDir,FStartupInfo,
                 fProcessInformation) then
{$endif}
    Raise Exception.CreateFmt('Failed to execute %s : %d',[FCommandLine,GetLastError]);
  if POUsePipes in FProcessOptions then
    begin
    FileClose(FStartupInfo.hStdInput);
    FileClose(FStartupInfo.hStdOutput);
    FileClose(FStartupInfo.hStdError);
    end;
{$ifdef unix}
  Fhandle:=fprocessinformation.hProcess;
{$endif}
  FRunning:=True;
  If FEnv<>Nil then
    FreePCharList(FEnv);
  if not (csDesigning in ComponentState) and // This would hang the IDE !
     (poWaitOnExit in FProcessOptions) and
      not (poRunSuspended in FProcessOptions) then
    WaitOnExit;
end;

Function TProcess.WaitOnExit : Dword;

begin
{$ifdef unix}
  Result:=WaitPid(Handle,@FExitCode,0);
  If Result=Handle then
    FExitCode:=WexitStatus(FExitCode);
{$else}
  Result:=WaitForSingleObject (FprocessInformation.hProcess,Infinite);
  If Result<>Wait_Failed then
    GetExitStatus;
{$endif}
  FRunning:=False;
end;

Function TProcess.Suspend : Longint;

begin
{$ifdef unix}
  If kill(Handle,SIGSTOP)<>0 then
    Result:=-1
  else
    Result:=1;
{$else}
  Result:=SuspendThread(ThreadHandle);
{$endif}
end;

Function TProcess.Resume : LongInt;

begin
{$ifdef unix}
  If kill(Handle,SIGCONT)<>0 then
    Result:=-1
  else
    Result:=0;
{$else}
  Result:=ResumeThread(ThreadHandle);
{$endif}
end;

Function TProcess.Terminate(AExitCode : Integer) : Boolean;

begin
  Result:=False;
{$ifdef unix}
  Result:=kill(Handle,SIGTERM)=0;
  If Result then
    begin
    If Running then
      Result:=Kill(Handle,SIGKILL)=0;
    end;
  GetExitStatus;
{$else}
  If ExitStatus=Still_active then
    Result:=TerminateProcess(Handle,AexitCode);
{$endif}
end;

Procedure TProcess.SetFillAttribute (Value : Cardinal);

begin
  FStartupInfo.dwFlags:=FStartupInfo.dwFlags or Startf_UseFillAttribute;
  FStartupInfo.dwFillAttribute:=Value;
end;

Procedure TProcess.SetShowWindow (Value : TShowWindowOptions);

{$ifndef unix}
Const
  SWC : Array [TShowWindowOptions] of Cardinal =
             (0,SW_HIDE,SW_Maximize,SW_Minimize,SW_Restore,SW_Show,
             SW_ShowDefault,SW_ShowMaximized,SW_ShowMinimized,
               SW_showMinNOActive,SW_ShowNA,SW_ShowNoActivate,SW_ShowNormal);
{$endif}

begin
  FShowWindow:=Value;
  if Value<>swoNone then
    FStartupInfo.dwFlags:=FStartupInfo.dwFlags or Startf_UseShowWindow
  else
    FStartupInfo.dwFlags:=FStartupInfo.dwFlags and not Startf_UseShowWindow;
{$ifndef unix}
  FStartupInfo.wShowWindow:=SWC[Value];
{$endif}  
end;

Procedure TProcess.SetWindowColumns (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartUpOptions,suoUseCountChars);
  FStartupInfo.dwXCountChars:=Value;
end;


Procedure TProcess.SetWindowHeight (Value : Cardinal);

begin
  if Value<>0 then
    include(FStartUpOptions,suoUsePosition);
  FStartupInfo.dwYsize:=Value;
end;

Procedure TProcess.SetWindowLeft (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartUpOptions,suoUseSize);
  FStartupInfo.dwx:=Value;
end;

Procedure TProcess.SetWindowTop (Value : Cardinal);

begin
  if Value<>0 then
    Include(FStartUpOptions,suoUsePosition);
  FStartupInfo.dwy:=Value;
end;

Procedure TProcess.SetWindowWidth (Value : Cardinal);
begin
  If (Value<>0) then
    Include(FStartUpOptions,suoUseSize);
  FStartupInfo.dwxsize:=Value;
end;

Function TProcess.GetWindowRect : TRect;
begin
  With Result do
    With FStartupInfo do
      begin
      Left:=dwx;
      Right:=dwx+dwxSize;
      Top:=dwy;
      Bottom:=dwy+dwysize;
      end;
end;

Procedure TProcess.SetWindowRect (Value : Trect);
begin
  Include(FStartupOptions,suouseSize);
  Include(FStartupOptions,suoUsePosition);
  With Value do
    With FStartupInfo do
      begin
      dwx:=Left;
      dwxSize:=Right-Left;
      dwy:=Top;
      dwySize:=Bottom-top;
      end;
end;


Procedure TProcess.SetWindowRows (Value : Cardinal);

begin
  FStartupInfo.dwFlags:=FStartupInfo.dwFlags or Startf_UseCountChars;
  FStartupInfo.dwYCountChars:=Value;
end;

procedure TProcess.SetApplicationname(const Value: String);
begin
  FApplicationname := Value;
  If (csdesigning in ComponentState) and
     (FCommandLine='') then
    FCommandLine:=Value;
end;

procedure TProcess.SetProcessOptions(const Value: TProcessOptions);
begin
  FProcessOptions := Value;
  If poNewConsole in FPRocessOptions then
    Exclude(FProcessoptions,poNoConsole);
  if poRunSuspended in FProcessOptions then
    Exclude(FPRocessoptions,poWaitOnExit);
end;

procedure TProcess.SetActive(const Value: Boolean);
begin
  if (Value<>GetRunning) then
    If Value then
      Execute
    else
      Terminate(0);
end;

procedure TProcess.SetEnvironment(const Value: TStrings);
begin
  FEnvironment.Assign(Value);
end;

end.
{
  $Log$
  Revision 1.12  2001-12-15 20:01:16  michael
  + Applied FreeStreams fix from Mattias Gaertner

  Revision 1.11  2001/12/15 19:53:37  michael
  + Removed DWord and THandle

  Revision 1.10  2001/12/14 07:53:32  michael
    - Removed trect as well.

  Revision 1.9  2001/12/13 18:34:59  michael
    * Removed TPoint declaration, it conflicts with classes definition

  Revision 1.8  2001/12/11 11:15:15  marco
   * ifdef linux -> Unix fix

  Revision 1.7  2001/11/24 20:43:56  carl
  * fix compilation problems under non-linux systems

  Revision 1.6  2001/11/08 13:01:06  michael
  + Fixed win32 compile

  Revision 1.5  2001/11/05 21:45:35  michael
  + unix/linux unit name conflict

  Revision 1.4  2001/11/05 21:07:08  michael
  + Added header and mode switch

}