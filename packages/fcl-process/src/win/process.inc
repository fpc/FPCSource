{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2008 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Uses
  Windows;


Resourcestring
  SNoCommandLine        = 'Cannot execute empty command-line';
  SErrCannotExecute     = 'Failed to execute %s : %d';
{  SErrNoSuchProgram     = 'Executable not found: "%s"';
  SErrNoTerminalProgram = 'Could not detect X-Terminal program';
}

Const
  PriorityConstants : Array [TProcessPriority] of Cardinal =
                      (HIGH_PRIORITY_CLASS,IDLE_PRIORITY_CLASS,
                       NORMAL_PRIORITY_CLASS,REALTIME_PRIORITY_CLASS,
                       BELOW_NORMAL_PRIORITY_CLASS,ABOVE_NORMAL_PRIORITY_CLASS);

procedure TProcessnamemacro.CloseProcessHandles;
begin
  if (FProcessHandle<>0) then
    CloseHandle(FProcessHandle);
  if (FThreadHandle<>0) then
    CloseHandle(FThreadHandle);
end;

Function TProcessnamemacro.PeekExitStatus : Boolean;
begin
  Result:=GetExitCodeProcess(ProcessHandle,FExitCode) and (FExitCode<>Still_Active);
end;

Function GetStartupFlags (P : TProcessnamemacro): Cardinal;

begin
  With P do
    begin
    Result:=0;
    if poUsePipes in Options then
       Result:=Result or Startf_UseStdHandles;
    if suoUseShowWindow in StartupOptions then
      Result:=Result or startf_USESHOWWINDOW;
    if suoUSESIZE in StartupOptions then
      Result:=Result or startf_usesize;
    if suoUsePosition in StartupOptions then
      Result:=Result or startf_USEPOSITION;
    if suoUSECOUNTCHARS in Startupoptions then
      Result:=Result or startf_usecountchars;
    if suoUsefIllAttribute in StartupOptions then
      Result:=Result or startf_USEFILLATTRIBUTE;
    end;
end;

Function GetCreationFlags(P : TProcessnamemacro) : Cardinal;

begin
  With P do
    begin
    Result:=CREATE_UNICODE_ENVIRONMENT;
    if poNoConsole in Options then
      Result:=Result or CREATE_NO_WINDOW;
    if poNewConsole in Options then
      Result:=Result or Create_new_console;
    if poNewProcessGroup in Options then
      Result:=Result or CREATE_NEW_PROCESS_GROUP;
    If poRunSuspended in Options Then
      Result:=Result or Create_Suspended;
    if poDebugProcess in Options Then
      Result:=Result or DEBUG_PROCESS;
    if poDebugOnlyThisProcess in Options Then
      Result:=Result or DEBUG_ONLY_THIS_PROCESS;
    if poDefaultErrorMode in Options Then
      Result:=Result or CREATE_DEFAULT_ERROR_MODE;
    if poDetached in Options Then
      Result:=Result or DETACHED_PROCESS;

    result:=result or PriorityConstants[FProcessPriority];
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

Procedure InitProcessAttributes(P : TProcessnamemacro; Var PA : TSecurityAttributes);

begin
  FillChar(PA,SizeOf(PA),0);
  PA.nLength := SizeOf(PA);
end;

Procedure InitThreadAttributes(P : TProcessnamemacro; Var TA : TSecurityAttributes);

begin
  FillChar(TA,SizeOf(TA),0);
  TA.nLength := SizeOf(TA);
end;

Procedure InitStartupInfo(P : TProcessnamemacro; Var SI : STARTUPINFOW);

Const
  SWC : Array [TShowWindowOptions] of Cardinal =
             (0,SW_HIDE,SW_Maximize,SW_Minimize,SW_Restore,SW_Show,
             SW_ShowDefault,SW_ShowMaximized,SW_ShowMinimized,
               SW_showMinNOActive,SW_ShowNA,SW_ShowNoActivate,SW_ShowNormal);

begin
  FillChar(SI,SizeOf(SI),0);
  With SI do
    begin
    cb:=SizeOf(SI);
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

{ The handles that are to be passed to the child process must be
  inheritable. On the other hand, only non-inheritable handles
  allow the sending of EOF when the write-end is closed. This
  function is used to duplicate the child process's ends of the
  handles into inheritable ones, leaving the parent-side handles
  non-inheritable.
}
function DuplicateHandleFP(var handle: THandle): Boolean;

var
  oldHandle: THandle;
begin
  oldHandle := handle;
  Result := DuplicateHandle
  ( GetCurrentProcess(),
    oldHandle,
    GetCurrentProcess(),
    @handle,
    0,
    true,
    DUPLICATE_SAME_ACCESS
  );
  if Result then
    Result := CloseHandle(oldHandle);
end;


Procedure CreatePipes(Var HI,HO,HE : Thandle; Var SI : TStartupInfoW; CI, CE : Boolean; APipeBufferSize : Cardinal);

begin
  if CI then
    begin
      CreatePipeHandles(SI.hStdInput,HI, APipeBufferSize);
      DuplicateHandleFP(SI.hStdInput);
    end
  else
    begin
      SI.hStdInput:=StdInputHandle;
    end;
  CreatePipeHandles(HO,Si.hStdOutput, APipeBufferSize);
  DuplicateHandleFP(   Si.hStdOutput);
  if CE then begin
    CreatePipeHandles(HE,SI.hStdError, APipeBufferSize);
    DuplicateHandleFP(   SI.hStdError);
    end
  else
    begin
    SI.hStdError:=SI.hStdOutput;
    HE:=HO;
    end;
end;

{Function MaybeQuote(Const S : String) : String;

begin
  If (Pos(' ',S)<>0) then
    Result:='"'+S+'"'
  else
     Result:=S;
end;
}
Function MaybeQuoteIfNotQuoted(Const S : TProcessString) : TProcessString;

begin
  If (Pos(' ',S)<>0) and (pos('"',S)=0) then
    Result:='"'+S+'"'
  else
     Result:=S;
end;


Procedure TProcessnamemacro.Execute;
Var
  i : Integer;
  WName,WDir,WCommandLine : UnicodeString;
  PWName,PWDir,PWCommandLine : PWideChar;
  FEnv: pointer;
  FCreationFlags : Cardinal;
  FProcessAttributes : TSecurityAttributes;
  FThreadAttributes : TSecurityAttributes;
  FProcessInformation : TProcessInformation;
  FStartupInfo : STARTUPINFOW;
  HI,HO,HE : THandle;
  Cmd : TProcessString;

 begin
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
    If poUsePipes in Options then
      CreatePipes(HI,HO,HE,FStartupInfo,Not(poPassInput in Options), Not(poStdErrToOutPut in Options), FPipeBufferSize);
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
      FProcessID:=FProcessINformation.dwProcessID;
    Finally
      if POUsePipes in Options then
        begin
        if not (poPassInput in Options) then
          FileClose(FStartupInfo.hStdInput);
        FileClose(FStartupInfo.hStdOutput);
        if Not (poStdErrToOutPut in Options) then
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
     (poWaitOnExit in Options) and
      not (poRunSuspended in Options) then
    WaitOnExit;
end;

Function TProcessnamemacro.WaitOnExit : Boolean;
Var
  R : DWord;
begin
  R:=WaitForSingleObject (FProcessHandle,Infinite);
  Result:=(R<>Wait_Failed);
  If Result then
    GetExitStatus;
  FRunning:=False;
end;

Function TProcessnamemacro.WaitOnExit(Timeout : DWord) : Boolean;
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

Function TProcessnamemacro.Suspend : Longint;

begin
  Result:=SuspendThread(ThreadHandle);
end;

Function TProcessnamemacro.Resume : LongInt;

begin
  Result:=ResumeThread(ThreadHandle);
end;

Function TProcessnamemacro.Terminate(AExitCode : Integer) : Boolean;

begin
  Result:=False;
  If ExitStatus=Still_active then
    Result:=TerminateProcess(Handle,AexitCode);
end;

Procedure TProcessnamemacro.SetShowWindow (Value : TShowWindowOptions);

begin
  FShowWindow:=Value;
end;
