{ %target=linux }
{ FpGetRLimit/FpSetRLimit pass the resource and the full rlim_t range, issue #37345 }
program tgetrlimit;

{$mode objfpc}{$h+}

uses
  BaseUnix, SysUtils;

// Halt with aCode and write aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg,' (errno ',fpgeterrno,')');
    halt(aCode);
    end;
end;


// Return the soft (aHard=False) or hard limit column of /proc/self/limits for the line starting with aName
function ProcLimit(const aName: string; aHard: Boolean): string;

var
  lFile: Text;
  lLine: string;
  lPos: Integer;

begin
  Result:='';
  Assign(lFile,'/proc/self/limits');
  {$i-}
  Reset(lFile);
  {$i+}
  if IOResult<>0 then
    exit;
  while not Eof(lFile) do
    begin
    ReadLn(lFile,lLine);
    if Copy(lLine,1,Length(aName))<>aName then
      continue;
    Delete(lLine,1,Length(aName));
    lLine:=TrimLeft(lLine);
    if aHard then
      begin
      lPos:=Pos(' ',lLine);
      Delete(lLine,1,lPos);
      lLine:=TrimLeft(lLine);
      end;
    lPos:=Pos(' ',lLine);
    if lPos>0 then
      SetLength(lLine,lPos-1);
    Result:=lLine;
    break;
    end;
  Close(lFile);
end;


var
  lLimit, lSaved: TRLimit;

begin
  Check(1,FpGetRLimit(RLIMIT_CORE,@lSaved)=0,'get RLIMIT_CORE');
  lLimit:=lSaved;
  lLimit.rlim_cur:=0;
  Check(2,FpSetRLimit(RLIMIT_CORE,@lLimit)=0,'set RLIMIT_CORE');
  Check(3,FpGetRLimit(RLIMIT_CORE,@lLimit)=0,'get RLIMIT_CORE after set');
  Check(4,lLimit.rlim_cur=0,'RLIMIT_CORE soft limit is 0 after set');
  Check(5,FpGetRLimit(RLIMIT_NOFILE,@lLimit)=0,'get RLIMIT_NOFILE');
  Check(6,(lLimit.rlim_cur>0) and (lLimit.rlim_cur<>High(rlim_t)),'RLIMIT_NOFILE soft limit is a finite non-zero value');
  Check(7,FpGetRLimit(RLIMIT_CORE,nil)=-1,'get with nil fails');
  Check(8,FpSetRLimit(RLIMIT_CORE,nil)=-1,'set with nil fails');

  if ProcLimit('Max file size',True)<>'unlimited' then
    begin
    writeln('hard RLIMIT_FSIZE is not unlimited, skipping the range checks');
    writeln('ok');
    halt(0);
    end;
  Check(10,FpGetRLimit(RLIMIT_FSIZE,@lSaved)=0,'get RLIMIT_FSIZE');
  Check(11,lSaved.rlim_max=High(rlim_t),'unlimited hard RLIMIT_FSIZE is RLIM_INFINITY');
  lLimit:=lSaved;
  lLimit.rlim_cur:=rlim_t($80000001);
  Check(12,FpSetRLimit(RLIMIT_FSIZE,@lLimit)=0,'set RLIMIT_FSIZE above 2 GB');
  Check(13,FpGetRLimit(RLIMIT_FSIZE,@lLimit)=0,'get RLIMIT_FSIZE above 2 GB');
  Check(14,lLimit.rlim_cur=rlim_t($80000001),'RLIMIT_FSIZE soft limit above 2 GB round trip');
  Check(15,lLimit.rlim_max=High(rlim_t),'RLIMIT_FSIZE hard limit stays RLIM_INFINITY');
  lLimit.rlim_cur:=High(rlim_t);
  Check(16,FpSetRLimit(RLIMIT_FSIZE,@lLimit)=0,'set RLIMIT_FSIZE to RLIM_INFINITY');
  Check(17,ProcLimit('Max file size',False)='unlimited','kernel soft RLIMIT_FSIZE is unlimited');
  Check(18,FpGetRLimit(RLIMIT_FSIZE,@lLimit)=0,'get RLIMIT_FSIZE after RLIM_INFINITY');
  Check(19,lLimit.rlim_cur=High(rlim_t),'RLIMIT_FSIZE soft limit RLIM_INFINITY round trip');
  writeln('ok');
end.
