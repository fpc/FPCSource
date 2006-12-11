{
  Win32 Process .inc.
}

uses Windows;

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

Function StringsToPChars(List : TStrings): pointer;

var
  EnvBlock: string;
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

Procedure CreatePipes(Var HI,HO,HE : Thandle; Var SI : TStartupInfo; CE : Boolean);

begin
  CreatePipeHandles(SI.hStdInput,HI);
  CreatePipeHandles(HO,Si.hStdOutput);
  if CE then
    CreatePipeHandles(HE,SI.hStdError)
  else
    begin
    SI.hStdError:=SI.hStdOutput;
    HE:=HO;
    end;
end;


Procedure TProcess.Execute;


Var
  PName,PDir,PCommandLine : PChar;
  FEnv: pointer;
  FCreationFlags : Cardinal;
  FProcessAttributes : TSecurityAttributes;
  FThreadAttributes : TSecurityAttributes;
  FProcessInformation : TProcessInformation;
  FStartupInfo : STARTUPINFO;
  HI,HO,HE : THandle;

begin
  FInheritHandles:=True;
  PName:=Nil;
  PCommandLine:=Nil;
  PDir:=Nil;
  If FApplicationName<>'' then
    PName:=Pchar(FApplicationName);
  If FCommandLine<>'' then
    PCommandLine:=Pchar(FCommandLine);
  If FCurrentDirectory<>'' then
    PDir:=Pchar(FCurrentDirectory);
  if FEnvironment.Count<>0 then
    FEnv:=StringsToPChars(FEnvironment)
  else
    FEnv:=Nil;
  Try
    FCreationFlags:=GetCreationFlags(Self);
    InitProcessAttributes(Self,FProcessAttributes);
    InitThreadAttributes(Self,FThreadAttributes);
    InitStartupInfo(Self,FStartUpInfo);
    If poUsePipes in FProcessOptions then
      CreatePipes(HI,HO,HE,FStartupInfo,Not(poStdErrToOutPut in FProcessOptions));
    Try
      If Not CreateProcess (PName,PCommandLine,@FProcessAttributes,@FThreadAttributes,
                   FInheritHandles,FCreationFlags,FEnv,PDir,FStartupInfo,
                   fProcessInformation) then
        Raise EProcess.CreateFmt('Failed to execute %s : %d',[FCommandLine,GetLastError]);
      FProcessHandle:=FProcessInformation.hProcess;
      FThreadHandle:=FProcessInformation.hThread;
      FProcessID:=FProcessINformation.dwProcessID;
    Finally
      if POUsePipes in FProcessOptions then
        begin
        FileClose(FStartupInfo.hStdInput);
        FileClose(FStartupInfo.hStdOutput);
        if Not (poStdErrToOutPut in FProcessOptions) then
          FileClose(FStartupInfo.hStdError);
        CreateStreams(HI,HO,HE);
        end;
    end;
    FRunning:=True;
  Finally
    If FEnv<>Nil then
      FreeMem(FEnv);
  end;
  if not (csDesigning in ComponentState) and // This would hang the IDE !
     (poWaitOnExit in FProcessOptions) and
      not (poRunSuspended in FProcessOptions) then
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



