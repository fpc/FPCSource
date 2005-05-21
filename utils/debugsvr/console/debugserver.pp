{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Console and system log version of debug server.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}

program debugserver;

Uses
  msgintf,debugserverintf,linux,classes,sysutils,getopts,systemlog;

resourcestring
  SUnknownOption = 'Unknown option : %s';
  SMessageFrom   = '%s [%s] : %s ';

Var
  UseSyslog : Boolean;

Const
  LogLevel  : Integer = log_debug;

Procedure LogEvent(Const Event: TDebugEvent);

Var
  S : String;

begin
  With Event do
    begin
    S:=DateTimeToStr(TimeStamp)+' : '+Format(SMessageFrom,[MsgTypes[LogCode],Client.Peer,Event]);
    If UseSysLog then
      Syslog(LogLevel,Pchar(S),[])
    else
      Writeln(S);
    end;
end;

Function GetFDS(Var AFDS : tfdset) : Integer;

Var
  I : Integer;

begin
  Result:=0;
  fd_zero(AFDS);
  For I:=0 to FClients.Count-1 do
    With TClient(FClients[i]) do
      begin
      If Handle>Result then
        Result:=Handle;
      fd_set(Handle,AFDS);
      end;
  Inc(Result);
end;

Procedure StartReading;

Var
  ReadFDS  : tfdset;
  I,maxfds : Integer;
  TimeOut  : TTimeVal;

begin
  Repeat
    maxfds:=GetFDS(ReadFDS);
    TimeOut.sec:=0;
    TimeOut.usec:=10000;
    Maxfds:=Select(maxfds,@ReadFDS,Nil,Nil,@TimeOut);
    If MaxFds>0 then
      begin
      For I:=FClients.Count-1 downto 0 do
        If FD_IsSet(TClient(FClients[i]).Handle,ReadFDS) then
          ReadMessage(TClient(FClients[i]).Handle);
      end;
    // Check for new connection.
    CheckNewConnection;
  Until (FClients.Count=0);
end;

procedure Wait;

Var
  TV,TR : TimeSpec;

begin
  tv.tv_sec:=1;
  tv.tv_nsec:=0;
  nanosleep(tv,tr);
end;

Procedure HandleConnections;

begin
  Repeat
    If CheckNewConnection<>Nil then
      StartReading
    else
      Wait;
  Until quit;
end;

Var
  OldHUPHandler,
  OldINTHandler,
  OldQUITHandler,
  OldTERMHandler : SigActionRec;

Procedure HandleSig(Sig : Longint); Cdecl;

Var
  OH : SignalHandler;

begin
  Quit:=True;
  Case Sig of
    SIGHUP  : OH:=OldHUPHandler.handler.sh;
    SIGTERM : OH:=OldTERMHandler.handler.sh;
    SIGQUIT : OH:=OldQUITHandler.handler.sh;
    SIGINT  : OH:=OldINTHandler.handler.sh;
  else
    OH:=Nil;
  end;
  If (OH<>SignalHandler(SIG_DFL)) then
    OH(Sig);
end;

Procedure SetupSignals;

  Procedure SetupSig (Sig : Longint; Var OH : SigactionRec);

  Var
    Act : SigActionRec;
  begin
    Act.handler.sh:=@HandleSig;
    Act.sa_mask:=0;
    Act.SA_FLAGS:=0;
    Act.Sa_restorer:=Nil;
    SigAction(Sig,@Act,@OH);
    If LinuxError<>0 then
      begin
      Writeln(stderr,SErrFailedToSetSignalHandler);
      Halt(1)
      end;
    end;
begin
  SetupSig(SIGTERM,OldTERMHandler);
  SetupSig(SIGQUIT,OldQUITHandler);
  SetupSig(SIGINT,OldINTHandler);
  SetupSig(SIGHUP,OldHUPHandler);
end;

Procedure Usage;

begin
  Writeln('Usage : debugserver [options]');
  Writeln('where options is one of');
  Writeln(' -h            this help');
  Writeln(' -s socket     use unix socket');
  Writeln(' -l            uses syslog instead of standard output');
  Halt(1);
end;

Procedure ProcessOptions;

Var
  C : Char;
  I : Integer;

begin
  UseSyslog:=False;
  Repeat
    C:=getopt('hl::s:');
    case c of
      'h' : Usage;
      's' : DebugSocket:=OptArg;
      'l' : begin
            UseSysLog:=True;
            LogLevel:=StrToIntdef(OptArg,LogLevel);
            end;
      '?' : begin
            Writeln(Format(SUnknownOption,[OptOpt]));
            Usage;
            end;
    end;
  Until (C=EndOfOptions);
  if OptInd<=ParamCount then
    begin
    For I:=OptInd to ParamCount do
       Writeln(Format(SUnknownOption,[Paramstr(i)]));
    Usage;
    end;
end;

Procedure SetupSysLog;

Var
  Prefix : String;

begin
 prefix:=format('DebugServer[%d] ',[GetPID]);
 OpenLog(pchar(prefix),LOG_NOWAIT,LOG_DEBUG);
end;

Procedure CloseSyslog;

begin
  CloseLog;
end;

begin
  ProcessOptions;
  SetupSignals;
  If UseSysLog then
    SetupSyslog;
  OpenDebugServer;
  DebugLogCallback:=@LogEvent;
  Try
    HandleConnections;
  Finally
    CloseDebugServer;
    If UseSyslog then
      CloseSyslog;
  end;
end.
