{ Source provided for Free Pascal Bug Report 3170 }
{ Submitted by "Michalis Kamburelis" on  2004-06-15 }
{ e-mail: michalis@camelot.homedns.org }
{ Simple test of 4 string comparison routines: [Ansi]Compare(Str|Text)
  with AnsiStrings with #0 chars inside.

  FPC 1.9.5 fails only at CompareStr + ('hello', 'hello'#0'bye')
  test, passes all other.
  After applying my small patch to sysstr.inc, FPC 1.9.5 passes
  all tests.

  Delphi note: Kylix 3 does not pass this test with Ansi- functions,
  it seems that in Kylix 3 Ansi- functions consider #0 char to mark
  end of AnsiString (I will not submit this as a compatibility bug,
  as I consider this stupid, FPC behaviour is definitely more
  sensible and consequent here).
}

{$apptype CONSOLE}
{$assertions ON}
{$ifdef FPC} {$mode objfpc} {$endif}
{$longstrings ON}

uses SysUtils;

type
  TCompare2Strings = function(const s1, s2:string):Integer;

procedure Check(CompareFunc:TCompare2Strings);
begin
 Assert(CompareFunc('hello'#0'bye', 'hello'#0'bye') = 0);
 Assert(CompareFunc('hello'#0'seeya', 'hello'#0'bye') > 0);
 Assert(CompareFunc('hello', 'hello'#0'bye') < 0);

{ alternative:
 Writeln(CompareFunc('hello'#0'bye', 'hello'#0'bye'));
 Writeln(CompareFunc('hello'#0'seeya', 'hello'#0'bye'));
 Writeln(CompareFunc('hello', 'hello'#0'bye'));
}
end;

begin
 Writeln('CompareStr');      Check(@CompareStr);
 Writeln('CompareText');     Check(@CompareText);
 Writeln('AnsiCompareStr');  Check(@AnsiCompareStr);
 Writeln('AnsiCompareText'); Check(@AnsiCompareText);
end.
