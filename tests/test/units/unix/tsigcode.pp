{ %target=linux,darwin,freebsd,netbsd,openbsd,dragonfly }
{ siginfo si_code values SI_USER and CLD_EXITED match what the kernel delivers, issue #37918 }
program tsigcode;

{$mode objfpc}{$h+}

uses
  BaseUnix;

var
  lUsrCode, lChldCode: cint;
  lUsrSeen, lChldSeen: Boolean;

// Return the si_code field of aInfo
function SigCode(aInfo: PSigInfo): cint;

begin
{$ifdef netbsd}
  Result:=aInfo^._info.si_code;
{$else}
  Result:=aInfo^.si_code;
{$endif}
end;


// Record the si_code of SIGUSR1 and SIGCHLD
procedure Handler(aSig: cint; aInfo: PSigInfo; aContext: PSigContext); cdecl;

begin
  if aSig=SIGUSR1 then
    begin
    lUsrCode:=SigCode(aInfo);
    lUsrSeen:=True;
    end
  else if aSig=SIGCHLD then
    begin
    lChldCode:=SigCode(aInfo);
    lChldSeen:=True;
    end;
end;


// Halt with aCode and write aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg);
    halt(aCode);
    end;
end;


// Install Handler with SA_SIGINFO for aSig
procedure Install(aSig: cint);

var
  lAct: SigActionRec;

begin
  FillChar(lAct,SizeOf(lAct),0);
  lAct.sa_handler:=SigActionHandler(@Handler);
  lAct.sa_flags:=SA_SIGINFO;
  Check(1,FpSigAction(aSig,@lAct,nil)=0,'install handler');
end;


var
  lPid: TPid;
  lStatus: cint;
  lWait: TTimeSpec;
  lTries: Integer;

begin
  Install(SIGUSR1);
  Install(SIGCHLD);

  Check(2,FpKill(FpGetPid,SIGUSR1)=0,'kill self');
  Check(3,lUsrSeen,'SIGUSR1 delivered');
  Check(4,lUsrCode=SI_USER,'si_code of kill() is SI_USER');

  lPid:=FpFork;
  if lPid=0 then
    FpExit(3);
  Check(5,lPid>0,'fork');
  while FpWaitPid(lPid,@lStatus,0)<>lPid do
    Check(6,fpgeterrno=ESysEINTR,'waitpid');
  lWait.tv_sec:=0;
  lWait.tv_nsec:=10000000;
  lTries:=0;
  while not lChldSeen and (lTries<100) do
    begin
    FpNanoSleep(@lWait,nil);
    Inc(lTries);
    end;
  Check(7,lChldSeen,'SIGCHLD delivered');
  Check(8,lChldCode=CLD_EXITED,'si_code of an exited child is CLD_EXITED');
  writeln('ok');
end.
