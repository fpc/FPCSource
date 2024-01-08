{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2008 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFDEF FPC_DOTTEDUNITS}
Uses
  WinApi.Windows;
{$ELSE FPC_DOTTEDUNITS}
Uses
  Windows;
{$ENDIF FPC_DOTTEDUNITS}


Resourcestring
  SNoCommandLine        = 'Cannot execute empty command-line';
  SErrCannotExecute     = 'Failed to execute %s : %d';
{  SErrNoSuchProgram     = 'Executable not found: "%s"';
  SErrNoTerminalProgram = 'Could not detect X-Terminal program';
}

Const
  PriorityConstants : Array [TProcessPriority] of Cardinal =
                      (HIGH_PRIORITY_CLASS,IDLE_PRIORITY_CLASS,
                       NORMAL_PRIORITY_CLASS,REALTIME_PRIORITY_CLASS);

procedure TProcess.CloseProcessHandles;
begin
  if (FProcessHandle<>0) then
    CloseHandle(FProcessHandle);
  if (FThreadHandle<>0) then
    CloseHandle(FThreadHandle);
end;

Function TProcess.PeekExitStatus : Boolean;

begin
  GetExitCodeProcess(ProcessHandle,FExitCode);
  Result:=(FExitCode<>Still_Active);
end;

Function GetStartupFlags (P : TProcess): Cardinal;

begin
  With P do
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
end;

Function GetCreationFlags(P : TProcess) : Cardinal;

begin
  With P do
    begin
    Result:=0;
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
    end;
end;

Function StringsToPWidechars(List : TStrings): pointer;

var
  EnvBlock: Widestring;
  I: Integer;

begin
  EnvBlock := '';
  For I:=0 to List.Count-1 do
    EnvBlock := EnvBlock + List[i] + #0;
  EnvBlock := EnvBlock + #0;
  GetMem(Result, Length(EnvBlock));
  CopyMemory(Result, @EnvBlock[1], Length(EnvBlock));
end;

Procedure InitProcessAttributes(P : TProcess; Var PA : TSecurityAttributes);

begin
  FillChar(PA,SizeOf(PA),0);
  PA.nLength := SizeOf(PA);
end;

Procedure InitThreadAttributes(P : TProcess; Var TA : TSecurityAttributes);

begin
  FillChar(TA,SizeOf(TA),0);
  TA.nLength := SizeOf(TA);
end;

Procedure InitStartupInfo(P : TProcess; Var SI : STARTUPINFO);

Const
  SWC : Array [TShowWindowOptions] of Cardinal =
             (0,SW_HIDE,SW_Maximize,SW_Minimize,SW_Restore,SW_Show,
             SW_ShowDefault,SW_ShowMaximized,SW_ShowMinimized,
               SW_showMinNOActive,SW_ShowNA,SW_ShowNoActivate,SW_ShowNormal);

begin
  FillChar(SI,SizeOf(SI),0);
  With SI do
    begin
    dwFlags:=GetStartupFlags(P);
    if P.FShowWindow<>swoNone then
     dwFlags:=dwFlags or Startf_UseShowWindow
    else
      dwFlags:=dwFlags and not Startf_UseShowWindow;
    wShowWindow:=SWC[P.FShowWindow];
    if (poUsePipes in P.Options) then
      begin
      dwFlags:=dwFlags or Startf_UseStdHandles;
      end;
    if P.FillAttribute<>0 then
      begin
      dwFlags:=dwFlags or Startf_UseFillAttribute;
      dwFillAttribute:=P.FillAttribute;
      end;
     dwXCountChars:=P.WindowColumns;
     dwYCountChars:=P.WindowRows;
     dwYsize:=P.WindowHeight;
     dwXsize:=P.WindowWidth;
     dwy:=P.WindowTop;
     dwX:=P.WindowLeft;
     end;
end;

Procedure CreatePipes(Var HI,HO,HE : Thandle; Var SI : TStartupInfo; CE : Boolean; APipeBufferSize : Cardinal);

begin
  CreatePipeHandles(SI.hStdInput,HI,APipeBufferSize);
  CreatePipeHandles(HO,Si.hStdOutput,APipeBufferSize);
  if CE then
    CreatePipeHandles(HE,SI.hStdError,APipeBufferSize)
  else
    begin
    SI.hStdError:=SI.hStdOutput;
    HE:=HO;
    end;
end;

function WStrAsUniquePWideChar(var s: UnicodeString): PWideChar;
begin
  UniqueString(s);
  if s<>'' then
    Result:=PWideChar(s)
  else
    Result:=nil;
end;

Function StringsToWChars(List : TProcessStrings): pointer;

var
  EnvBlock: UnicodeString;
  I: Integer;

begin
  EnvBlock := '';
  For I:=0 to List.Count-1 do
    EnvBlock := EnvBlock + List[i] + #0;
  EnvBlock := EnvBlock + #0;
  GetMem(Result, Length(EnvBlock)*2);
  CopyMemory(Result, @EnvBlock[1], Length(EnvBlock)*2);
end;

Function MaybeQuoteIfNotQuoted(Const S : TProcessString) : TProcessString;

begin
  If (Pos(' ',S)<>0) and (pos('"',S)=0) then
    Result:='"'+S+'"'
  else
     Result:=S;
end;

Procedure TProcess.SysExecute;

Var
  i : Integer;
  WName,WDir,WCommandLine : UnicodeString;
  PWName,PWDir,PWCommandLine : PWideChar;
  FEnv: pointer;
  FCreationFlags : Cardinal;
  FProcessAttributes : TSecurityAttributes;
  FThreadAttributes : TSecurityAttributes;
  FProcessInformation : TProcessInformation;
  FStartupInfo : STARTUPINFO;
  HI,HO,HE : THandle;
  Cmd : TProcessString;

 begin
  FDescriptors[phtInput].PrepareHandles;
  FDescriptors[phtOutput].PrepareHandles;
  FDescriptors[phtError].PrepareHandles;
  WName:='';
  WCommandLine:='';
  WDir:='';
  if (FApplicationName='') and (FCommandLine='') and (FExecutable='') then
    Raise EProcess.Create(SNoCommandline);
  if (FApplicationName<>'') then
    begin
    WName:=FApplicationName;
    WCommandLine:=FCommandLine;
    end
  else If (FCommandLine<>'') then
    WCommandLine:=FCommandLine
  else if (FExecutable<>'') then
    begin
    Cmd:=MaybeQuoteIfNotQuoted(Executable);
    For I:=0 to Parameters.Count-1 do
      Cmd:=Cmd+' '+MaybeQuoteIfNotQuoted(Parameters[i]);
    WCommandLine:=Cmd;
    end;
  If FCurrentDirectory<>'' then
    WDir:=FCurrentDirectory;
  if FEnvironment.Count<>0 then
    FEnv:=StringsToWChars(FEnvironment)
  else
    FEnv:=Nil;
  Try
    FCreationFlags:=GetCreationFlags(Self);
    InitProcessAttributes(Self,FProcessAttributes);
    InitThreadAttributes(Self,FThreadAttributes);
    InitStartupInfo(Self,FStartUpInfo);
    FStartupInfo.hStdInput:=FDescriptors[phtInput].ResolveProcessHandle;
    FStartupInfo.hStdOutput:=FDescriptors[phtOutput].ResolveProcessHandle;
    if Not(poStdErrToOutPut in Options) then
      FStartupInfo.hStdError:=FDescriptors[phtError].ResolveProcessHandle
    else
      FStartupInfo.hStdError:=FStartupInfo.hStdOutput;
    Try
      // Beware: CreateProcess can alter the strings
      // Beware: nil is not the same as a pointer to a #0
      PWName:=WStrAsUniquePWideChar(WName);
      PWCommandLine:=WStrAsUniquePWideChar(WCommandLine);
      PWDir:=WStrAsUniquePWideChar(WDir);
      If Not CreateProcessW (PWName,PWCommandLine,@FProcessAttributes,@FThreadAttributes,
                   FInheritHandles,FCreationFlags,FEnv,PWDir,FStartupInfo,
                   fProcessInformation) then
        Raise EProcess.CreateFmt(SErrCannotExecute,[FCommandLine,GetLastError]);
      FProcessHandle:=FProcessInformation.hProcess;
      FThreadHandle:=FProcessInformation.hThread;
      FThreadId:=FProcessInformation.dwThreadId;  
      FProcessID:=FProcessINformation.dwProcessID;
    Finally
      FDescriptors[phtInput].CloseTheirHandle;
      FDescriptors[phtOutput].CloseTheirHandle;
      FDescriptors[phtError].CloseTheirHandle;
    end;
    FRunning:=True;
  Finally
    If FEnv<>Nil then
      FreeMem(FEnv);
  end;
  if not (csDesigning in ComponentState) and // This would hang the IDE !
     (poWaitOnExit in Options) and
      not (poRunSuspended in Options) then
    WaitOnExit;
end;


Function TProcess.WaitOnExit : Boolean;
Var
  R : DWord;
begin
  R:=WaitForSingleObject (FProcessHandle,Infinite);
  Result:=(R<>Wait_Failed);
  If Result then
    GetExitStatus;
  FRunning:=False;
end;


Function TProcess.WaitOnExit(Timeout : DWord) : Boolean;
Var
  R : DWord;
begin
  R:=WaitForSingleObject (FProcessHandle,Timeout);
  Result:=R=0;
  If Result then
    begin
      GetExitStatus;
      FRunning:=False;
    end;
end;


Function TProcess.Suspend : Longint;

begin
  Result:=SuspendThread(ThreadHandle);
end;

Function TProcess.Resume : LongInt;

begin
  Result:=ResumeThread(ThreadHandle);
end;

Function TProcess.Terminate(AExitCode : Integer) : Boolean;

begin
  Result:=False;
  If ExitStatus=Still_active then
    Result:=TerminateProcess(Handle,AexitCode);
end;

Procedure TProcess.SetShowWindow (Value : TShowWindowOptions);


begin
  FShowWindow:=Value;
end;

Function TIODescriptor.SysCreateFileNameHandle(const aFileName: string) : THandle;

const
  DefaultRights = 438; // 438 = 666 octal which is rw rw rw
  ModeNames : Array[Boolean] of String = ('Reading','Writing');

var
  FM :  Integer;
  Sec: SECURITY_ATTRIBUTES;

begin
  if (aFileName='') then
    Raise EProcess.Create('No filename set');
  FillByte(sec, SizeOf(sec), 0);
  sec.nLength := SizeOf(Sec);
  sec.bInheritHandle := True;
  case ProcessHandleType of
    phtInput:  Result:=CreateFileW(PWideChar(WideString(aFileName)), GENERIC_READ,
      FILE_SHARE_READ, @sec, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    phtOutput,
    phtError:
      begin
        Result:=CreateFileW(PWideChar(WideString(aFileName)), GENERIC_WRITE,
          FILE_SHARE_READ, @sec, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
        if not(Result=INVALID_HANDLE_VALUE) then
          FileSeek(Result, 0, 2);
      end;
  end;
  if (Result=INVALID_HANDLE_VALUE) then
    Raise EProcess.CreateFmt('Could not open file "%s" for %s',[aFileName,ModeNames[ProcessHandleType<>phtInput]]);
end;



function TIODescriptor.SysPrepareCreatedHandleForProcess(aHandle: THandle): THandle;

var
  oldHandle: THandle;
  Res : Boolean;
  
begin
  if IOType in [iotNone,iotFile] then begin
    Result:=aHandle;
    exit;
  end;
  oldHandle := ahandle;
  ahandle:=THandle(INVALID_HANDLE_VALUE); 
  Res := DuplicateHandle
  ( GetCurrentProcess(),
    oldHandle,
    GetCurrentProcess(),
    @aHandle,
    0,
    true,
    DUPLICATE_SAME_ACCESS
  );
  if Res then
    Res:=CloseHandle(oldHandle);
  if not Res then
    begin
    FileClose(aHandle);
    Raise EProcess.CreateFmt('Could not make handle %d inheritable',[aHandle]);
    end;
  Result:=aHandle;
end;    

function TIODescriptor.SysNullFileName: string;
begin
  result:='NULL';
end;

function TIODescriptor.SysIsTypeSupported(AValue: TIOType): Boolean;
begin
  Result:=True;
end;
