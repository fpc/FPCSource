{ %target=linux,freebsd,openbsd,aix,darwin,netbsd }
uses
  BaseUnix,unix;

function LocalTimeIsDefined(Year, Month, Day, Hour, Minute, Second: word; const UTC: Boolean): Boolean;
var
  UnixTime: Int64;
  lTZInfo: TTZInfo;
begin
  lTZInfo:=Default(TTZInfo);
  UnixTime:=UniversalToEpoch(Year, Month, Day, Hour, Minute, Second);
  Result := GetLocalTimezone(UnixTime,UTC,lTZInfo);
end;

function GetOffset(Year, Month, Day, Hour, Minute, Second: word; const UTC: Boolean): Integer;
var
  UnixTime: Int64;
  lTZInfo: TTZInfo;
begin
  lTZInfo:=Default(TTZInfo);
  UnixTime:=UniversalToEpoch(Year, Month, Day, Hour, Minute, Second);
  if not GetLocalTimezone(UnixTime,UTC,lTZInfo) then
    Halt(1);
  if not UTC then
    UnixTime := UnixTime-lTZInfo.seconds;
  if not ((lTZInfo.validsince<=UnixTime) and (UnixTime<lTZInfo.validuntil)) then
    Halt(2);
  if lTZInfo.seconds mod 3600<>0 then
    Halt(3);
  Result:=lTZInfo.seconds div 3600;
end;

begin
  if not ReadTimezoneFile('Europe/Vienna') then // check against Europe/Vienna file
  begin
    writeln('timezone file not found');
    halt(10);
  end;

  if GetOffset(2019, 03, 31, 1, 59, 0, False)<>1 then Halt(11);
  // 2019-03-31 02:00-03:00 CET is not defined
  if LocalTimeIsDefined(2019, 03, 31, 2, 30, 0, False) then Halt(19);
  if GetOffset(2019, 03, 31, 3, 0, 0, False)<>2 then Halt(12);

  if GetOffset(2019, 10, 27, 1, 59, 0, False)<>2 then Halt(13);
  // 2019-10-27 02:00-03:00 CET is ambiguos, therefore do not check
  if GetOffset(2019, 10, 27, 3, 0, 0, False)<>1 then Halt(14);

  if GetOffset(2019, 03, 31, 0, 59, 0, True)<>1 then Halt(15);
  if GetOffset(2019, 03, 31, 1, 0, 0, True)<>2 then Halt(16);

  if GetOffset(2019, 10, 27, 0, 59, 0, True)<>2 then Halt(17);
  if GetOffset(2019, 10, 27, 1, 0, 0, True)<>1 then Halt(18);

  writeln('ok');
end.
