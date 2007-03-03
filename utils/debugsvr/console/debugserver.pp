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
  msgintf,debugserverintf,baseunix,classes,sysutils,getopts,systemlog;

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
  fpfd_zero(AFDS);
  For I:=0 to FClients.Count-1 do
    With TClient(FClients[i]) do
      begin
      If Handle>Result then
        Result:=Handle;
      fpfd_set(Handle,AFDS);
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
    TimeOut.tv_sec:=0;
    TimeOut.tv_usec:=10000;
    Maxfds:=fpSelect(maxfds,@ReadFDS,Nil,Nil,@TimeOut);
    If MaxFds>0 then
      begin
      For I:=FClients.Count-1 downto 0 do
        If fpFD_IsSet(TClient(FClients[i]).Handle,ReadFDS)<>0 then
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
  fpnanosleep(@tv,@tr);
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
  OH : Signalhandler;

begin
  Quit:=True;
  Case Sig of
    SIGHUP  : OH:=signalhandler(OldHUPHandler.sa_handler);
    SIGTERM : OH:=signalhandler(OldTERMHandler.sa_handler);
    SIGQUIT : OH:=signalhandler(OldQUITHandler.sa_handler);
    SIGINT  : OH:=signalhandler(OldINTHandler.sa_handler);
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
    signalhandler(Act.sa_handler):=@HandleSig;
    fpsigemptyset(act.sa_mask);
    Act.SA_FLAGS:=0;

{$ifdef linux} // ???
     Act.Sa_restorer:=Nil;
  {$endif}
    if fpSigAction(Sig,@Act,@OH)=-1 then
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
 prefix:=format('DebugServer[%d] ',[fpGetPID]);
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
