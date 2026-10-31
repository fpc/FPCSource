{ %target=linux,freebsd,openbsd,netbsd,darwin }
{ Time zone period before the first transition and renewal of the cached period, issues #42002, #42003 and #42004 }
program ttimezone2;

{$mode objfpc}{$h+}

uses
  SysUtils, BaseUnix, Unix;

type
  TTransition = record
    Time: Int64;
    TypeIdx: Byte;
  end;

var
  gBuf: TBytes;


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg);
    halt(aCode);
    end;
end;


// Appends aValue as a big-endian 32-bit integer to gBuf
procedure AddLong(aValue: Longint);

var
  lLen: Integer;

begin
  lLen:=Length(gBuf);
  SetLength(gBuf,lLen+4);
  gBuf[lLen]:=Byte(aValue shr 24);
  gBuf[lLen+1]:=Byte(aValue shr 16);
  gBuf[lLen+2]:=Byte(aValue shr 8);
  gBuf[lLen+3]:=Byte(aValue);
end;


// Appends aValue as one byte to gBuf
procedure AddByte(aValue: Byte);

begin
  SetLength(gBuf,Length(gBuf)+1);
  gBuf[High(gBuf)]:=aValue;
end;


{ Writes a version 1 TZif file aFileName with type 0 'GMT' (UTC+0), type 1 'CET' (UTC+1)
  and the transitions aTransitions. }
procedure WriteZone(const aFileName: string; const aTransitions: array of TTransition);

const
  cNames = 'GMT'#0'CET'#0;

var
  i: Integer;
  lHandle: THandle;

begin
  gBuf:=nil;
  AddByte(Ord('T'));
  AddByte(Ord('Z'));
  AddByte(Ord('i'));
  AddByte(Ord('f'));
  for i:=0 to 15 do
    AddByte(0);
  AddLong(0);
  AddLong(0);
  AddLong(0);
  AddLong(Length(aTransitions));
  AddLong(2);
  AddLong(Length(cNames));
  for i:=0 to High(aTransitions) do
    AddLong(Longint(aTransitions[i].Time));
  for i:=0 to High(aTransitions) do
    AddByte(aTransitions[i].TypeIdx);
  AddLong(0);
  AddByte(0);
  AddByte(0);
  AddLong(3600);
  AddByte(0);
  AddByte(4);
  for i:=1 to Length(cNames) do
    AddByte(Ord(cNames[i]));
  lHandle:=FileCreate(aFileName);
  Check(1,lHandle<>THandle(-1),'cannot create '+aFileName);
  FileWrite(lHandle,gBuf[0],Length(gBuf));
  FileClose(lHandle);
end;


// Returns a transition record
function Transition(aTime: Int64; aTypeIdx: Byte): TTransition;

begin
  Result.Time:=aTime;
  Result.TypeIdx:=aTypeIdx;
end;


// Returns the offset in seconds that GetLocalTimezone gives for aTimer
function OffsetAt(aTimer: Int64; aIsUTC: Boolean): Longint;

var
  lInfo: TTZInfo;

begin
  lInfo:=Default(TTZInfo);
  Check(2,GetLocalTimezone(aTimer,aIsUTC,lInfo),'GetLocalTimezone fails');
  Result:=lInfo.seconds;
end;


// Returns the offset in seconds that EpochToLocal applies to aEpoch
function EpochToLocalOffset(aEpoch: Int64): Int64;

var
  y, mo, d, h, mi, s: Word;

begin
  EpochToLocal(aEpoch,y,mo,d,h,mi,s);
  Result:=UniversalToEpoch(y,mo,d,h,mi,s)-aEpoch;
end;


// Sets the cached time zone info to a period that ended before now, named 'STALE'
procedure SetStaleTZInfo(aNow: Int64);

var
  lInfo: TTZInfo;
  lInfoEx: TTZInfoEx;

begin
  lInfo:=Default(TTZInfo);
  lInfo.seconds:=7200;
  lInfo.validsince:=aNow-100000;
  lInfo.validuntil:=aNow-100;
  lInfoEx:=Default(TTZInfoEx);
  lInfoEx.name[False]:='STALE';
  lInfoEx.name[True]:='STALE';
  SetTZInfo(lInfo,lInfoEx);
end;


var
  lDir, lFirst, lRenew: string;
  lNow, lT: Int64;
  lInfo: TTZInfo;

begin
  lDir:=IncludeTrailingPathDelimiter(GetTempDir(False));
  lFirst:=lDir+'ttimezone2-first-'+IntToStr(fpgetpid);
  lRenew:=lDir+'ttimezone2-renew-'+IntToStr(fpgetpid);
  lNow:=fptime;

  // The only transition is one hour from now: UTC+0 before it, UTC+1 from it on.
  lT:=lNow+3600;
  WriteZone(lFirst,[Transition(lT,1)]);
  Check(10,ReadTimezoneFile(lFirst),'ReadTimezoneFile of the zone with one future transition');
  lInfo:=TZInfo;
  Check(11,lInfo.seconds=0,'current offset before the first transition');
  Check(12,lInfo.validuntil=lT,'cached period before the first transition ends at that transition');
  Check(13,OffsetAt(lT-60,True)=0,'offset one minute before the first transition');
  Check(14,OffsetAt(lT+60,True)=3600,'offset one minute after the first transition');
  Check(15,OffsetAt(lT+3600+60,False)=3600,'offset of a local time after the first transition');
  Check(16,EpochToLocalOffset(lT+60)=3600,'EpochToLocal after the first transition');

  // UTC+1 from one day ago until one day from now, UTC+0 before and after.
  WriteZone(lRenew,[Transition(lNow-86400,1),Transition(lNow+86400,0)]);
  Check(20,ReadTimezoneFile(lRenew),'ReadTimezoneFile of the zone with a current period');
  SetStaleTZInfo(lNow);
  Check(21,OffsetAt(lNow-2*86400,True)=0,'offset before the current period');
  Check(22,TZInfoEx.name[False]='STALE','a lookup before the expired period keeps the cache');
  Check(23,OffsetAt(lNow+2*86400,True)=0,'offset after the current period');
  Check(24,TZInfoEx.name[False]='STALE','a lookup of a period after now keeps the cache');
  Check(25,EpochToLocalOffset(fptime)=3600,'EpochToLocal of the current time');
  Check(26,TZInfoEx.name[False]='CET','a lookup of the current time renews the expired cache');
  lInfo:=TZInfo;
  Check(27,(lInfo.seconds=3600) and (lInfo.validsince=lNow-86400) and (lInfo.validuntil=lNow+86400),
    'the renewed cache has the current period');
  SetStaleTZInfo(lNow);
  lInfo:=TZInfo;
  Check(30,(lInfo.seconds=3600) and (lInfo.validuntil=lNow+86400),'TZInfo renews a period that ended before now');
  Check(31,TZInfoEx.name[False]='CET','TZInfo renews the names of a period that ended before now');

  DeleteFile(lFirst);
  DeleteFile(lRenew);
  ReReadLocalTime;
  writeln('ok');
end.
