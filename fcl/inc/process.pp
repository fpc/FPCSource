unit Process;

{$mode delphi}
{$H+}

interface

Uses Classes,Pipes,Windows;

Type
  TProcessOptions = (poExecuteOnCreate,poRunSuspended,poUsePipes,
                     poNoConsole,poStderrToOutPut,poWaitOnExit);

  TCreateOptions = Set of TPRocessOptions;

  TProcess = Class (TObject)
    Private
      FAccess : Cardinal;
      FApplicationName : string;
      FChildErrorStream : TOutPutPipeStream;
      FChildInputSTream : TInputPipeStream;
      FChildOutPutStream : TOutPutPipeStream;
      FConsoleTitle : String;
      FCreateOptions : TCreateOptions;
      FCreationFlags : Cardinal;
      FCommandLine : String;
      FCurrentDirectory : String;
      FDeskTop : String;
      FEnvironment : Pointer;
      FExitCode : Cardinal;
      FHandle : THandle;
      FInherithandles : LongBool;
      FParentErrorStream : TInputPipeStream;
      FParentInputSTream : TInputPipeStream;
      FParentOutputStream : TOutPutPipeStream;
      FPrepared : Boolean;
      FProcessAttributes : PSecurityAttributes;
      FProcessInformation : TProcessInformation;
      FRunning : Boolean;
      FStartupInfo : TStartupInfo;
      FThreadAttributes  : PSecurityAttributes;
      Procedure FreeStreams;
      Function GetExitStatus : Integer;
      Function GetHandle : THandle;
      Function GetProcessAttributes : TSecurityAttributes;
      Function GetRunning : Boolean;
      Function GetThreadAttributes : TSecurityAttributes;
      Function GetWindowRect : TRect;
      Procedure SetFillAttribute (Value : Cardinal);
      Procedure SetProcessAttributes (Value : TSecurityAttributes);
      Procedure SetShowWindow (Value : Word);
      Procedure SetThreadAttributes (Value : TSecurityAttributes);
      Procedure SetWindowColumns (Value : Cardinal);
      Procedure SetWindowHeight (Value : Cardinal);
      Procedure SetWindowLeft (Value : Cardinal);
      Procedure SetWindowRect (Value : TRect);
      Procedure SetWindowRows (Value : Cardinal);
      Procedure SetWindowTop (Value : Cardinal);
      Procedure SetWindowWidth (Value : Cardinal);
    Public
      Constructor Create (Const ACommandline : String;
                          Options : TCreateOptions);
      Destructor Destroy; override;
      Procedure Execute; virtual;
      Function Resume : Integer; virtual;
      Function Suspend : Integer; virtual;
      Function Terminate (AExitCode : Integer): Boolean; virtual;
      Function WaitOnExit : DWord;

      Property ApplicationName : String Read FApplicationname
                                        Write FApplicationname;
      Property CommandLine : String Read FCommandLine;
      Property ConsoleTitle : String Read FConsoleTitle Write FConsoleTitle;
      Property CurrentDirectory : String Read FCurrentDirectory
                                       Write FCurrentDirectory;
      Property CreateOptions : TCreateOptions Read FCreateOptions;
      Property CreationFlags : Cardinal Read FCreationFlags Write FCreationFlags;
      Property DeskTop : String Read FDeskTop Write FDeskTop;
      Property Environment : Pointer Read FEnvironment Write FEnvironment;
      Property ExitStatus : Integer Read GetExitStatus;
      Property FillAttribute : Cardinal Read FStartupInfo.dwFillAttribute
                                        Write SetFillAttribute;
      Property Handle : THandle Read FProcessInformation.hProcess;
      Property Input : TOutPutPipeStream Read FParentOutPutStream;
      Property InheritHandles : LongBool Read FInheritHandles;
      Property OutPut : TInputPipeStream Read FParentInputStream;
      Property ProcessAttributes : TSecurityAttributes
                                 Read GetProcessAttributes
                                 Write SetProcessAttributes;
      Property ProcessInformation : TProcessInformation
                                    Read FPRocessInformation;
      Property Running : Boolean Read GetRunning;
      Property ShowWindow : Word Read FStartupInfo.wShowWindow
                                 Write SetShowWindow;
      Property StartupInfo : TStartupInfo Read FStartupInfo;
      Property StdErr : TinputPipeStream Read FParentErrorStream;
      Property ThreadAttributes : TSecurityAttributes
                                Read GetThreadAttributes
                                Write SetThreadAttributes;
      Property ThreadHandle : THandle Read FprocessInformation.hThread;
      Property WindowColumns : Cardinal Read FStartupInfo.dwXCountchars
                                       Write SetWindowColumns;
      Property WindowHeight : Cardinal Read FStartupInfo.dwYsize
                                      Write SetWindowHeight;
      Property WindowLeft : Cardinal Read FStartupInfo.dwx
                                    Write SetWindowLeft;
      Property WindowRows : Cardinal Read FStartupInfo.dwYcountChars
                                    Write SetWindowRows;
      Property WindowTop : Cardinal Read FStartupInfo.dwy
                                   Write SetWindowTop ;
      Property WindowWidth : Cardinal Read FStartupInfo.dwXsize
                                     Write SetWindowWidth;
      Property WindowRect : Trect Read GetWindowRect
                                  Write SetWindowRect;

    end;

implementation

Constructor TProcess.Create (Const ACommandline : String;
                    Options : TCreateOptions);
begin
  Inherited create;
  FCreateOptions:=Options;
  FCommandLine:=ACommandLine;
  FAccess:=PROCESS_ALL_ACCESS;
  FStartupInfo.cb:=SizeOf(TStartupInfo);
  FInheritHandles:=True;
  If poExecuteOnCreate in FCreateOptions then
    execute;
end;

Destructor TProcess.Destroy;

begin
  If assigned (FProcessAttributes) then Dispose (FPRocessAttributes);
  If assigned (FThreadAttributes) then Dispose (FThreadAttributes);
  FreeStreams;
end;

Procedure TProcess.FreeStreams;

begin
  FParentErrorStream.Free;
  FParentInputSTream.Free;
  FParentOutputStream.Free;
  FChildErrorStream.free;
  FChildInputSTream.Free;
  FChildOutPutStream.Free;
end;

Function TProcess.GetExitStatus : Integer;

begin
  If FRunning then
    GetExitCodeProcess(Handle,@FExitCode);
  Result:=FExitCode;
end;

Function TProcess.GetHandle : THandle;

begin
  IF FHandle=0 Then
    FHandle:=OpenProcess (FAccess,True,FProcessInformation.dwProcessId);
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

Function TProcess.GetRunning : Boolean;

begin
  IF FRunning then
    Frunning:=GetExitStatus=Still_Active;
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

Procedure TProcess.Execute;

Var PName,PDir : PChar;

begin
  if poNoConsole in FCReateOptions then
    FCreationFlags:=FCreationFlags or Detached_Process;
  If poRunSuspended in FCreateOptions Then
    FCreationFlags:=FCreationFlags or Create_Suspended;
  If poUsePipes in FCreateOptions then
    begin
    FreeStreams;
{  // This construct was supported on Win32 only. The new call takes this as a default.
    CreatePipeStreams (FChildInputSTream,FParentOutPutStream,@piInheritablePipe,1024);
    CreatePipeStreams (FParentInputStream,FChildOutPutStream,@piInheritablePipe,1024);
}
    CreatePipeStreams (FChildInputSTream,FParentOutPutStream);
    CreatePipeStreams (FParentInputStream,FChildOutPutStream);
    if poStdErrToOutPut in FCreateOptions then
{
      CreatePipeStreams (FParentErrorStream,FChildErrorStream,@piInheritablePipe,1024)
}
      CreatePipeStreams (FParentErrorStream,FChildErrorStream)
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
  If FApplicationName<>'' then PName:=Pchar(FApplicationName) else PName:=Nil;
  If FCurrentDirectory<>'' then PName:=Pchar(FCurrentDirectory) else PDir:=Nil;
  CreateProcess (Pname,PChar(FCommandLine),FProcessAttributes,FThreadAttributes,
                 FInheritHandles,FCreationFlags,FEnvironment,PDir,@FStartupInfo,
                 @fProcessInformation);
  FRunning:=True;
  if (poWaitOnExit in FCreateOptions) and
      not (poRunSuspended in FCreateOptions) then
    WaitOnExit;
end;

Function TProcess.WaitOnExit : Dword;

begin
  Result:=WaitForSingleObject (FprocessInformation.hProcess,Infinite);
  If Result<>Wait_Failed then
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

Procedure TProcess.SetFillAttribute (Value : Cardinal);

begin
  FStartupInfo.dwFlags:=FStartupInfo.dwFlags or Startf_UseFillAttribute;
  FStartupInfo.dwFillAttribute:=Value;
end;

Procedure TProcess.SetShowWindow (Value : Word);

begin
  FStartupInfo.dwFlags:=FStartupInfo.dwFlags or Startf_UseShowWindow;
  FStartupInfo.dwXCountChars:=Value;
end;

Procedure TProcess.SetWindowColumns (Value : Cardinal);

begin
  FStartupInfo.dwFlags:=FStartupInfo.dwFlags or Startf_UseCountChars;
  FStartupInfo.dwXCountChars:=Value;
end;


Procedure TProcess.SetWindowHeight (Value : Cardinal);

begin
  FStartupInfo.dwFlags:=FStartupInfo.dwFlags or Startf_UsePosition;
  FStartupInfo.dwYsize:=Value;
end;

Procedure TProcess.SetWindowLeft (Value : Cardinal);

begin
  FStartupInfo.dwFlags:=FStartupInfo.dwFlags or Startf_UseSize;
  FStartupInfo.dwx:=Value;
end;

Procedure TProcess.SetWindowTop (Value : Cardinal);

begin
  FStartupInfo.dwFlags:=FStartupInfo.dwFlags or Startf_UseSize;
  FStartupInfo.dwy:=Value;
end;

Procedure TProcess.SetWindowWidth (Value : Cardinal);
begin
  FStartupInfo.dwFlags:=FStartupInfo.dwFlags or Startf_UsePosition;
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
  FStartupInfo.dwFlags:=FStartupInfo.dwFlags or Startf_UseSize;
  FStartupInfo.dwFlags:=FStartupInfo.dwFlags or Startf_UsePosition;
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

end.
