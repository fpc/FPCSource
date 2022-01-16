{
  Make sure to set your project's options with, CompilerOptions --> Target "-o" -->Filename Value="fpcdebugserver",
  i.e. the executable name must be the same as the client's const named dbugmsg.DebugServerID.
}

program dbugsrv;

{$MODE OBJFPC}
{$H+}
{$APPTYPE CONSOLE}


uses
  classes,SysUtils,simpleipc,dbugmsg,strutils;


Type

  { THelperToWrite }

  THelperToWrite = class
    private
      Class var StrLogFilename: string;
      Class procedure WriteLnAllParams;
      Class procedure InitParamsDependencies;
      { methods which override standard Write and WriteLn of the console output }
      Class procedure DoWrite(const aBuffer: string);
      Class procedure DoWrite(var aBuffer: string; const aMinimumFieldWidthIndent: integer); overload;
      Class procedure DoWriteLn(const aBuffer: string);
      { methods which write in a log file, too }
      Class procedure WriteNowThisLineInLog(aBuffer: string);
      Class procedure WriteLnNowThisLineInLog(aBuffer: string);
      Class function ReplaceSpecialCharsInLog(const aBuffer: string): string;
    public
    end;


Var
  Srv : TSimpleIPCServer;
  Msg : TDebugMessage;
  StrBuffer : string = '';
  ObjFileStream : TFileStream = Nil;
  

class procedure THelperToWrite.WriteLnAllParams;
Var
  iNumParam: integer;
  sBuffer: string;
begin
  sBuffer := 'ParamCount='+IntToStr(ParamCount)+LineEnding;
  for iNumParam := 0 to ParamCount do
    sBuffer := IfThen(iNumParam<>ParamCount, sBuffer+'ParamStr('+IntToStr(iNumParam)+') = "'+ParamStr(iNumParam)+'"'+LineEnding, sBuffer+'ParamStr('+IntToStr(iNumParam)+') = "'+ParamStr(iNumParam)+'"');
  THelperToWrite.DoWriteLn(sBuffer);
end;

class procedure THelperToWrite.InitParamsDependencies;
begin
  If (ParamCount<>0) then
    if ParamStr(1)<>'' then begin {ord. params: 1st is a log filename}
      THelperToWrite.StrLogFilename:= ParamStr(1);
      ObjFileStream:= TFileStream.Create(THelperToWrite.StrLogFilename, fmCreate or fmOpenWrite or fmShareDenyWrite);
      ObjFileStream.Position:= 0;
    end;
end;

class procedure THelperToWrite.DoWrite(const aBuffer: string);
begin
  Write(aBuffer);
  if Assigned(ObjFileStream) then THelperToWrite.WriteNowThisLineInLog(StrBuffer);
end;

class procedure THelperToWrite.DoWrite(var aBuffer: string; const aMinimumFieldWidthIndent: integer);
begin
  Write(aBuffer:aMinimumFieldWidthIndent,' ');
  if Assigned(ObjFileStream) then THelperToWrite.WriteNowThisLineInLog(StrBuffer);
end;

class procedure THelperToWrite.DoWriteLn(const aBuffer: string);
begin
  WriteLn(aBuffer);
  if Assigned(ObjFileStream) then THelperToWrite.WriteLnNowThisLineInLog(aBuffer+LineEnding)
end;

class procedure THelperToWrite.WriteNowThisLineInLog(aBuffer: string);
var
  sBuffer: string;
begin
  sBuffer:= THelperToWrite.ReplaceSpecialCharsInLog(aBuffer);
  ObjFileStream.Write(sBuffer[1],length(sBuffer));
end;

class procedure THelperToWrite.WriteLnNowThisLineInLog(aBuffer: string);
var
  sBuffer: string;
begin
  aBuffer:= ' '{sep. each field of the msg-record}+aBuffer+LineEnding;
  sBuffer:= THelperToWrite.ReplaceSpecialCharsInLog(aBuffer);
  ObjFileStream.Write(sBuffer[1],length(sBuffer));
end;

class function THelperToWrite.ReplaceSpecialCharsInLog(const aBuffer: string): string;
begin
  Result := StringsReplace(aBuffer, [LineEnding+LineEnding], [LineEnding], [rfReplaceAll]);
end;

ResourceString
  SWelcomeOnSrv = 'IPC server started. Listening for debug messages:';


begin
  Srv:=TSimpleIPCServer.Create(Nil);
  Try
    Srv.ServerID:=DebugServerID;
    Srv.Global:=True;
    Srv.Active:=True;
    Srv.StartServer;
    THelperToWrite.InitParamsDependencies;
    THelperToWrite.WriteLnAllParams;
    StrBuffer:=SWelcomeOnSrv;
    THelperToWrite.DoWriteLn(StrBuffer);
    Repeat
      If Srv.PeekMessage(1,True) then
        begin
        Srv.MsgData.Seek(0,soFrombeginning);
        ReadDebugMessageFromStream(Srv.MsgData,MSg);
        StrBuffer:=FormatDateTime('hh:nn:ss.zzz',Msg.MsgTimeStamp)+': ';
        THelperToWrite.DoWrite(StrBuffer);
        StrBuffer:=DebugMessageName(MSg.MsgType);
        THelperToWrite.DoWrite(StrBuffer,12);
        StrBuffer:=Msg.Msg;
        THelperToWrite.DoWriteLn(StrBuffer);
        end
      else
        Sleep(10);
    Until False;
  Finally
    if Assigned(ObjFileStream) then
       ObjFileStream.Free;
    Srv.Free;
  end;
end.

