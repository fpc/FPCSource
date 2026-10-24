{ %target=linux }
{ %cpu=i386 }
{ A dynamically linked setgid program starts when the loader removes unsafe environment variables, issue #17806 }
program tw17806;

{$mode objfpc}{$h+}

uses
  cthreads, ctypes, BaseUnix, SysUtils;

const
  AT_SECURE = 23;
  AT_SYSINFO = 32;
  ChildArg = 'child';
  ExitNotSecure = 2;
  ExitWrongSysinfo = 3;

var
  { AT_SYSINFO value found by the system unit }
  psysinfo: LongWord; external name 'TC_$SYSTEM_$$_PSYSINFO';

function getauxval(aType: culong): culong; cdecl; external 'c' name 'getauxval';
function getgroups(aSize: cint; aList: PGid): cint; cdecl; external 'c' name 'getgroups';


// Returns a supplementary group of the process other than its real group, or -1
function OtherGroup: TGid;

var
  lGroups: array[0..255] of TGid;
  lCount, i: cint;

begin
  OtherGroup:=TGid(-1);
  lCount:=getgroups(Length(lGroups),@lGroups[0]);
  for i:=0 to lCount-1 do
    if lGroups[i]<>FpGetgid then
      exit(lGroups[i]);
end;


// Copies the file aSource to aDest
function CopyExe(const aSource, aDest: string): boolean;

var
  lIn, lOut: cint;
  lBuf: array[0..65535] of byte;
  lRead: TSsize;

begin
  CopyExe:=false;
  lIn:=FpOpen(aSource,O_RDONLY);
  if lIn<0 then
    exit;
  lOut:=FpOpen(aDest,O_WRONLY or O_CREAT or O_TRUNC,&700);
  if lOut>=0 then
    begin
      repeat
        lRead:=FpRead(lIn,lBuf,SizeOf(lBuf));
        if lRead>0 then
          FpWrite(lOut,lBuf,lRead);
      until lRead<=0;
      FpClose(lOut);
      CopyExe:=lRead=0;
    end;
  FpClose(lIn);
end;


var
  lExe: string;
  lGroup: TGid;
  lPid: TPid;
  lStatus: cint;
  lArgs: array[0..2] of PAnsiChar;
  lEnv: array[0..4] of PAnsiChar;

begin
  if ParamStr(1)=ChildArg then
    begin
      if getauxval(AT_SECURE)=0 then
        halt(ExitNotSecure);
      writeln('child started in secure mode, psysinfo $',HexStr(psysinfo,8),', AT_SYSINFO $',HexStr(getauxval(AT_SYSINFO),8));
      if psysinfo<>getauxval(AT_SYSINFO) then
        halt(ExitWrongSysinfo);
      halt(0);
    end;
  lGroup:=OtherGroup;
  if lGroup=TGid(-1) then
    begin
      writeln('skipped: no supplementary group');
      halt(0);
    end;
  lExe:=GetTempDir+'tw17806child'+IntToStr(FpGetpid);
  if not CopyExe(ParamStr(0),lExe) then
    begin
      writeln('skipped: cannot copy the program');
      halt(0);
    end;
  if (FpChown(lExe,FpGetuid,lGroup)<>0) or (FpChmod(lExe,&2755)<>0) then
    begin
      DeleteFile(lExe);
      writeln('skipped: cannot make the copy setgid');
      halt(0);
    end;
  lArgs[0]:=PAnsiChar(lExe);
  lArgs[1]:=ChildArg;
  lArgs[2]:=nil;
  lEnv[0]:='LD_LIBRARY_PATH=/nonexistent';
  lEnv[1]:='TW17806=1';
  lEnv[2]:='GCONV_PATH=/nonexistent';
  lEnv[3]:='LOCALDOMAIN=nonexistent';
  lEnv[4]:=nil;
  lPid:=FpFork;
  if lPid=0 then
    begin
      FpExecve(lArgs[0],@lArgs,@lEnv);
      FpExit(127);
    end;
  FpWaitpid(lPid,@lStatus,0);
  DeleteFile(lExe);
  if WIFEXITED(lStatus) and (WEXITSTATUS(lStatus)=ExitNotSecure) then
    begin
      writeln('skipped: the copy did not start in secure mode');
      halt(0);
    end;
  if not WIFEXITED(lStatus) then
    begin
      writeln('child killed by signal ',WTERMSIG(lStatus));
      halt(1);
    end;
  if WEXITSTATUS(lStatus)<>0 then
    begin
      writeln('child exit code ',WEXITSTATUS(lStatus));
      halt(2);
    end;
  writeln('ok');
end.
