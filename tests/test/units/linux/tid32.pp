{ %target=linux }
{ The user and group id calls of BaseUnix and Linux handle ids above 65535 }
program tid32;

{$mode objfpc}{$h+}

uses
  ctypes, BaseUnix, Linux, SysUtils;

const
  CLONE_NEWUSER = $10000000;
  LargeId = 100000;

function c_getuid: cuint32; cdecl; external 'c' name 'getuid';
function c_geteuid: cuint32; cdecl; external 'c' name 'geteuid';
function c_getgid: cuint32; cdecl; external 'c' name 'getgid';
function c_getegid: cuint32; cdecl; external 'c' name 'getegid';
function c_getgroups(aSize: cint; aList: PGid): cint; cdecl; external 'c' name 'getgroups';
function c_unshare(aFlags: cint): cint; cdecl; external 'c' name 'unshare';


// Halts with aCode and writes aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg);
    halt(aCode);
    end;
end;


// Writes aText to the file aName
function WriteProcFile(const aName, aText: string): boolean;

var
  lFd: cint;

begin
  lFd:=FpOpen(aName,O_WRONLY);
  WriteProcFile:=(lFd>=0) and (FpWrite(lFd,aText[1],Length(aText))=Length(aText));
  if lFd>=0 then
    FpClose(lFd);
end;


// Compares the ids and groups of BaseUnix with those of libc
procedure CheckAgainstLibc;

var
  lOurs, lLibc: array[0..255] of TGid;
  lCount, lLibcCount, i: cint;

begin
  Check(1,FpGetuid=c_getuid,Format('FpGetuid %d, getuid %d',[FpGetuid,c_getuid]));
  Check(2,FpGeteuid=c_geteuid,Format('FpGeteuid %d, geteuid %d',[FpGeteuid,c_geteuid]));
  Check(3,FpGetgid=c_getgid,Format('FpGetgid %d, getgid %d',[FpGetgid,c_getgid]));
  Check(4,FpGetegid=c_getegid,Format('FpGetegid %d, getegid %d',[FpGetegid,c_getegid]));
  FillChar(lOurs,SizeOf(lOurs),$FF);
  lCount:=FpGetgroups(Length(lOurs),pGrpArr(@lOurs)^);
  lLibcCount:=c_getgroups(Length(lLibc),@lLibc[0]);
  Check(5,lCount=lLibcCount,Format('FpGetgroups count %d, getgroups count %d',[lCount,lLibcCount]));
  for i:=0 to lCount-1 do
    Check(6,lOurs[i]=lLibc[i],Format('FpGetgroups[%d] = %d, getgroups[%d] = %d',[i,lOurs[i],i,lLibc[i]]));
end;


// Checks the id calls with ids above 65535 in a user namespace; returns False when no namespace is available
function CheckLargeIds: boolean;

var
  lUid, lGid: TUid;
  lFile: string;
  lFd, lRes, lErrno: cint;
  lStat: TStat;

begin
  lUid:=FpGetuid;
  lGid:=FpGetgid;
  if c_unshare(CLONE_NEWUSER)<>0 then
    exit(false);
  if not WriteProcFile('/proc/self/setgroups','deny') or
     not WriteProcFile('/proc/self/uid_map',Format('%d %d 1',[LargeId,lUid])) or
     not WriteProcFile('/proc/self/gid_map',Format('%d %d 1',[LargeId,lGid])) then
    exit(false);
  CheckLargeIds:=true;
  Check(10,FpGetuid=LargeId,Format('FpGetuid in the namespace is %d',[FpGetuid]));
  Check(11,FpGeteuid=LargeId,Format('FpGeteuid in the namespace is %d',[FpGeteuid]));
  Check(12,FpGetgid=LargeId,Format('FpGetgid in the namespace is %d',[FpGetgid]));
  Check(13,FpGetegid=LargeId,Format('FpGetegid in the namespace is %d',[FpGetegid]));
  Check(14,FpSetuid(LargeId)=0,Format('FpSetuid(%d) fails with errno %d',[LargeId,fpgeterrno]));
  Check(15,FpSetgid(LargeId)=0,Format('FpSetgid(%d) fails with errno %d',[LargeId,fpgeterrno]));
  Check(16,setreuid(TUid(-1),LargeId)=0,Format('setreuid(-1,%d) fails with errno %d',[LargeId,fpgeterrno]));
  Check(17,setregid(TUid(-1),LargeId)=0,Format('setregid(-1,%d) fails with errno %d',[LargeId,fpgeterrno]));
  lFile:=GetTempDir+'tid32.'+IntToStr(FpGetpid);
  lFd:=FpOpen(lFile,O_WRONLY or O_CREAT or O_TRUNC,&600);
  Check(18,lFd>=0,'create '+lFile);
  FpClose(lFd);
  lRes:=FpChown(PAnsiChar(lFile),LargeId,LargeId);
  lErrno:=fpgeterrno;
  FillChar(lStat,SizeOf(lStat),0);
  FpStat(lFile,lStat);
  DeleteFile(lFile);
  Check(19,lRes=0,Format('FpChown to %d fails with errno %d',[LargeId,lErrno]));
  Check(20,(lStat.st_uid=LargeId) and (lStat.st_gid=LargeId),Format('owner %d:%d after FpChown',[lStat.st_uid,lStat.st_gid]));
end;


begin
  CheckAgainstLibc;
  if not CheckLargeIds then
    writeln('no user namespace, ids above 65535 not checked');
  writeln('ok');
end.
