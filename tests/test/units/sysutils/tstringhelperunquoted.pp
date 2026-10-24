{ Tests TStringHelper.IndexOfAnyUnquoted and IndexOfUnquoted, including matches on the last character of the range }
program tstringhelperunquoted;

{$mode objfpc}{$H+}

uses
  SysUtils;

var
  gFailed: Boolean = False;


// Reports a failure when aActual differs from aExpected
procedure CheckEquals(const aMessage: string; aExpected, aActual: SizeInt);

begin
  if aExpected<>aActual then
    begin
    Writeln('failed: ',aMessage,': expected ',aExpected,', got ',aActual);
    gFailed:=True;
    end;
end;


var
  lS: string;

begin
  lS:='a';
  CheckEquals('single character string',0,lS.IndexOfAnyUnquoted(['a'],'"','"'));
  CheckEquals('single character string, different quotes',0,lS.IndexOfAnyUnquoted(['a'],'<','>'));
  CheckEquals('single character string, start index',0,lS.IndexOfAnyUnquoted(['a'],'"','"',0));
  CheckEquals('single character string, no match',-1,lS.IndexOfAnyUnquoted(['b'],'"','"'));
  lS:='"x" a';
  CheckEquals('match on last character',4,lS.IndexOfAnyUnquoted(['a'],'"','"'));
  CheckEquals('start index, match on last character',4,lS.IndexOfAnyUnquoted(['a'],'"','"',3));
  CheckEquals('start index inside quotes reverses quoting',-1,lS.IndexOfAnyUnquoted(['a'],'"','"',1));
  CheckEquals('quoted character is skipped',-1,lS.IndexOfAnyUnquoted(['x'],'"','"'));
  lS:='<x> a';
  CheckEquals('different quotes, match on last character',4,lS.IndexOfAnyUnquoted(['a'],'<','>'));
  lS:='<x <y>>z';
  CheckEquals('nested quotes, match on last character',7,lS.IndexOfAnyUnquoted(['z','y'],'<','>'));
  lS:='"x"abcd';
  CheckEquals('count, match on last character of range',4,lS.IndexOfAnyUnquoted(['b'],'"','"',3,2));
  CheckEquals('count, match after range',-1,lS.IndexOfAnyUnquoted(['c'],'"','"',3,2));
  CheckEquals('count 1, match',3,lS.IndexOfAnyUnquoted(['a'],'"','"',3,1));
  CheckEquals('count 0',-1,lS.IndexOfAnyUnquoted(['a'],'"','"',3,0));
  CheckEquals('count beyond end of string, match on last character',6,lS.IndexOfAnyUnquoted(['d'],'"','"',3,10));
  CheckEquals('start index at end of string',-1,lS.IndexOfAnyUnquoted(['d'],'"','"',7));
  lS:='"x" ab';
  CheckEquals('IndexOfUnquoted, match at end',4,lS.IndexOfUnquoted('ab','"','"'));
  CheckEquals('IndexOfUnquoted, quoted text is skipped',-1,lS.IndexOfUnquoted('x','"','"'));
  if gFailed then
    Halt(1);
  Writeln('ok');
end.
