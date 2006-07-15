program dbugsrv;

{$MODE OBJFPC}
{$H+}
{$APPTYPE CONSOLE}

uses
  classes,SysUtils,simpleipc,dbugmsg;

Var
  Srv : TSimpleIPCServer;
  S : String;
  Msg : TDebugMessage;
  
begin
  Srv:=TSimpleIPCServer.Create(Nil);
  Try
    Srv.ServerID:=DebugServerID;
    Srv.Global:=True;
    Srv.Active:=True;
    Srv.StartServer;
    Writeln('Server started. Listening for debug messages');
    Repeat
      If Srv.PeekMessage(1,True) then
        begin
        Srv.MsgData.Seek(0,soFrombeginning);
        ReadDebugMessageFromStream(Srv.MsgData,MSg);
        Write(FormatDateTime('hh:nn:ss.zzz',Msg.MsgTimeStamp),': ');
        Write(DebugMessageName(MSg.MsgType):12,' ');
        Writeln(Msg.Msg);
        end
      else
        Sleep(10);
    Until False;
  Finally
    Srv.Free;
  end;
end.

