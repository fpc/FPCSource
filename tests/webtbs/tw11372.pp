program BoolAsEnumTest_FPC;

{$MODE Delphi}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  TypInfo;

procedure Test_GetEnumName;
begin
  writeln('Testing GetEnumName');
  if TypInfo.GetEnumName(TypeInfo(Boolean),Ord(False))<>'False' then
    halt(1);
  if TypInfo.GetEnumName(TypeInfo(Boolean),Ord(True))<>'True' then
    halt(1);
end;


procedure Test_GetEnumValue;
begin
  writeln('Testing GetEnumValue');
  if TypInfo.GetEnumValue(TypeInfo(Boolean),'false')<>0 then
    halt(1);
  if TypInfo.GetEnumValue(TypeInfo(Boolean),'true')<>1 then
    halt(1);
end;


procedure Test_GetEnumCount;
begin
  writeln('Testing GetEnumCount');
  if TypInfo.GetEnumNameCount(TypeInfo(Boolean))<>Ord(High(Boolean))+1 then
    halt(1);    
end;




begin
  Test_GetEnumCount;
  Test_GetEnumValue;
  Test_GetEnumName;
  writeln('Ok');
end.
