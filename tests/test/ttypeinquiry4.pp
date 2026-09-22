{ The modeswitch typeinquiry is enabled by default in mode objfpc.
  Mode extendedpascal is covered by ttypeinquiry5.pp. }
program ttypeinquiry4;

{$mode objfpc}

uses
  utypeinquiry4a, utypeinquiry4b, utypeinquiry4d;

procedure Check(Ok: boolean; Id: longint);
begin
  if not Ok then
    begin
      writeln('failed: ',Id);
      halt(Id);
    end;
end;

var
  b: byte = 0;
  { objfpc has the modeswitch on by default }
  c: type of b;
begin
  b:=1;
  c:=b;
  Check(SizeOf(c)=1,1);
  Check(c=1,2);

  { mode delphi with the modeswitch directive }
  Check(SizeOf(DelphiVar)=2,10);
  Check(DelphiSize=2,11);

  { mode delphiunicode with the modeswitch directive }
  Check(SizeOf(DelphiUnicodeVar)=4,20);
  Check(DelphiUnicodeSize=4,21);

  { mode fpc with the modeswitch directive }
  Check(SizeOf(FpcWithSwitchVar)=2,40);
  Check(FpcWithSwitchSize=2,41);

  writeln('ok');
end.
