unit Process;

{$mode delphi}
{$H+}

interface

Uses Classes,Pipes;

Type
   THandle = Longint;

Type
  TProcessOptions = (poExecuteOnCreate,poRunSuspended,poUsePipes,
                     poNoConsole,poStderrToOutPut,poWaitOnExit);

  TCreateOptions = Set of TPRocessOptions;

  TProcess = Class (TObject)
    Private
      FShowWindow : Boolean;
      FFillAttribute,
      FWindowColumns,
      FWindowHeight,
      FWindowLeft,
      FWindowRows,
      FWindowTop,
      FWindowWidth : Longint;
      FWindowRect  : TRect;
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
      FPID : Longint;
      FThreadHandle,
      FHandle : THandle;
      FInherithandles : LongBool;
      FParentErrorStream : TInputPipeStream;
      FParentInputSTream : TInputPipeStream;
      FParentOutputStream : TOutPutPipeStream;
      FPrepared : Boolean;
      FRunning : Boolean;
      Procedure FreeStreams;
      Function GetExitStatus : Integer;
      Function GetRunning : Boolean;
      Function GetWindowRect : TRect;
      Procedure SetWindowRect (Value : TRect);
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
      Property FillAttribute : Longint Read FFillAttribute Write FFillAttribute;
      Property Handle : THandle Read FHandle;
      Property ThreadHandle : THandle Read FThreadHandle;
      Property PID : Longint;
      Property Input : TOutPutPipeStream Read FParentOutPutStream;
      Property InheritHandles : LongBool Read FInheritHandles;
      Property OutPut : TInputPipeStream Read FParentInputStream;
      Property Running : Boolean Read GetRunning;
      Property ShowWindow : Boolean Read FShowWindow Write FShowWindow;
      Property StdErr : TinputPipeStream Read FParentErrorStream;
      Property WindowColumns : Longint Read FWindowColumns Write FWindowColumns;
      Property WindowHeight : Longint Read FWindowHeight Write FWindowHeight;
      Property WindowLeft : Longint Read FWindowLeft Write FWindowLeft;
      Property WindowRows : Longint Read FWindowRows Write FWindowRows;
      Property WindowTop : Longint Read FWindowTop  Write FWindowTop;
      Property WindowWidth : Longint Read FWindowWidth Write FWindowWidth;
      Property WindowRect : Trect Read GetWindowRect  Write SetWindowRect;
    end;

implementation

{$i process.inc}

Constructor TProcess.Create (Const ACommandline : String;
                    Options : TCreateOptions);
begin
  Inherited create;
  FCreateOptions:=Options;
  FCommandLine:=ACommandLine;
  FInheritHandles:=True;
  FFillAttribute := -1;
  FWindowColumns := -1;
  FWindowHeight := -1;
  FWindowLeft := -1;
  FWindowRows := -1;
  FWindowTop := -1;
  FWindowWidth := -1;
  If poExecuteOnCreate in FCreateOptions then
    execute;
end;

Destructor TProcess.Destroy;

begin
  FreeStreams;
end;

Procedure TProcess.FreeStreams;

begin
  if FChildErrorStream<>FChildoutputStream then
    begin
    FChildErrorStream.free;
    FParentErrorStream.free;
    end;
  FParentInputSTream.Free;
  FParentOutputStream.Free;
  FChildInputSTream.Free;
  FChildOutPutStream.Free;
end;

Function TProcess.GetExitStatus : Integer;

begin
{
  If FRunning then
    GetExitCodeProcess(Handle,@FExitCode);
}
  Result:=FExitCode;
end;

Function TProcess.GetWindowRect : TRect;
begin
  With Result do
    begin
    Left:=FWindowLeft;
    Top:=FWindowTop;
    Right:=FWindowLeft+FWindowWidth;
    Bottom:=FWindowTop+FWindowRows;
    end;
end;

Procedure TProcess.SetWindowRect (Value : Trect);
begin
  With Value do
    begin
    FWindowLeft:=Left;
    FWindowWidth:=Right-Left;
    FWindowTop:=Top;
    FWindowRows:=Bottom-top;
    end;
end;

end.
