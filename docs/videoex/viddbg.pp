unit viddbg;

Interface

uses video;


Procedure StartVideoLogging;
Procedure StopVideoLogging;
Function  IsVideoLogging : Boolean;
Procedure  SetVideoLogFileName(FileName : String);

Const
  DetailedVideoLogging : Boolean = False;

Implementation

uses sysutils,keyboard;

var
  NewVideoDriver,
  OldVideoDriver : TVideoDriver;
  Active,Logging : Boolean;
  LogFileName : String;
  VideoLog : Text;

Function TimeStamp : String;

begin
  TimeStamp:=FormatDateTime('hh:nn:ss',Time());
end;

Procedure StartVideoLogging;

begin
  Logging:=True;
  Writeln(VideoLog,'Start logging video operations at: ',TimeStamp);
end;

Procedure StopVideoLogging;

begin
  Writeln(VideoLog,'Stop logging video operations at: ',TimeStamp);
  Logging:=False;
end;

Function IsVideoLogging : Boolean;

begin
  IsVideoLogging:=Logging;
end;

Var
  ColUpd,RowUpd : Array[0..1024] of Integer;

Procedure DumpScreenStatistics(Force : Boolean);

Var
  I,Count : Integer;

begin
  If Force then
    Write(VideoLog,'forced ');
  Writeln(VideoLog,'video update at ',TimeStamp,' : ');
  FillChar(Colupd,SizeOf(ColUpd),#0);
  FillChar(Rowupd,SizeOf(RowUpd),#0);
  Count:=0;
  For I:=0 to VideoBufSize div SizeOf(TVideoCell) do
    begin
    If VideoBuf^[i]<>OldVideoBuf^[i] then
      begin
      Inc(Count);
      Inc(ColUpd[I mod ScreenWidth]);
      Inc(RowUpd[I div ScreenHeight]);
      end;
    end;
  Write(VideoLog,Count,' videocells differed divided over ');
  Count:=0;
  For I:=0 to ScreenWidth-1 do
    If ColUpd[I]<>0 then
      Inc(Count);
  Write(VideoLog,Count,' columns and ');
  Count:=0;
  For I:=0 to ScreenHeight-1 do
    If RowUpd[I]<>0 then
      Inc(Count);
  Writeln(VideoLog,Count,' rows.');
  If DetailedVideoLogging Then
   begin
   For I:=0 to ScreenWidth-1 do
     If (ColUpd[I]<>0) then
       Writeln(VideoLog,'Col ',i,' : ',ColUpd[I]:3,' rows changed');
   For I:=0 to ScreenHeight-1 do
     If (RowUpd[I]<>0) then
       Writeln(VideoLog,'Row ',i,' : ',RowUpd[I]:3,' colums changed');
   end;
end;

Procedure LogUpdateScreen(Force : Boolean);

begin
  If Logging then
    DumpScreenStatistics(Force);
  OldVideoDriver.UpdateScreen(Force);
end;

Procedure LogInitVideo;

begin
  OldVideoDriver.InitDriver();
  Assign(VideoLog,logFileName);
  Rewrite(VideoLog);
  Active:=True;
  StartVideoLogging;
end;

Procedure LogDoneVideo;

begin
  StopVideoLogging;
  Close(VideoLog);
  Active:=False;
  OldVideoDriver.DoneDriver();
end;

Procedure SetVideoLogFileName(FileName : String);

begin
  If Not Active then
    LogFileName:=FileName;
end;

Initialization
  GetVideoDriver(OldVideoDriver);
  NewVideoDriver:=OldVideoDriver;
  NewVideoDriver.UpdateScreen:=@LogUpdateScreen;
  NewVideoDriver.InitDriver:=@LogInitVideo;
  NewVideoDriver.DoneDriver:=@LogDoneVideo;
  LogFileName:='Video.log';
  Logging:=False;
  SetVideoDriver(NewVideoDriver);
end.