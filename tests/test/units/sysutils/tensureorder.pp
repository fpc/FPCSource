{ generic EnsureOrder in SysUtils, issue #40533 }
program tensureorder;

{$mode objfpc}{$h+}

uses
  SysUtils;

var
  lI1, lI2: Integer;
  lQ1, lQ2: Int64;
  lD1, lD2: Double;
  lC1, lC2: Char;
  lS1, lS2: string;

begin
  lI1:=5;
  lI2:=3;
  if not specialize EnsureOrder<Integer>(lI1,lI2) then
    halt(1);
  if (lI1<>3) or (lI2<>5) then
    halt(2);
  if specialize EnsureOrder<Integer>(lI1,lI2) then
    halt(3);
  if (lI1<>3) or (lI2<>5) then
    halt(4);
  lI2:=3;
  if specialize EnsureOrder<Integer>(lI1,lI2) then
    halt(5);
  if (lI1<>3) or (lI2<>3) then
    halt(6);

  lQ1:=High(Int64);
  lQ2:=Low(Int64);
  if not specialize EnsureOrder<Int64>(lQ1,lQ2) then
    halt(11);
  if (lQ1<>Low(Int64)) or (lQ2<>High(Int64)) then
    halt(12);

  lD1:=2.5;
  lD2:=-1.25;
  if not specialize EnsureOrder<Double>(lD1,lD2) then
    halt(21);
  if (lD1<>-1.25) or (lD2<>2.5) then
    halt(22);

  lC1:='z';
  lC2:='a';
  if not specialize EnsureOrder<Char>(lC1,lC2) then
    halt(31);
  if (lC1<>'a') or (lC2<>'z') then
    halt(32);

  lS1:='pear';
  lS2:='apple';
  UniqueString(lS1);
  UniqueString(lS2);
  if not specialize EnsureOrder<string>(lS1,lS2) then
    halt(41);
  if (lS1<>'apple') or (lS2<>'pear') then
    halt(42);
  if specialize EnsureOrder<string>(lS1,lS2) then
    halt(43);
  if (lS1<>'apple') or (lS2<>'pear') then
    halt(44);
  writeln('ok');
end.
