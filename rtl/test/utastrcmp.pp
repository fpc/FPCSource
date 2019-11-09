unit utastrcmp;
{$mode objfpc}
{$h+}
interface

uses
  {$ifdef unix}
    {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
  {$endif unix}
   SysUtils;

implementation

uses punit,utrtl;

Function checka(ok : boolean; func : string; value : longint) : Boolean;

begin
  Result:=AssertTrue(Func+' failed, result = '+InTToStr(Value),Ok);
end;

Function tastrcmp : string;

var
  a, b: array[0..1] of char;
  tmp : longint;
begin
  Result:='';
  a[0] := #0; a[1] := #1;      //Empty string
  b[0] := #0; b[1] := #0;      //Empty string with different char after end
  tmp:=AnsiStrComp(a, b);      //should be zero because a=b
  if not checka(tmp=0,'AnsiStrComp',tmp) then exit;
  tmp:=AnsiStrIComp(a, b);     //should be zero because a=b
  if not checka(tmp=0,'AnsiStrIComp',tmp) then exit;
end;

Var
  teststr: string;

Function check(b: boolean; testnr: longint) : Boolean;

begin
  Result:=AssertTrue(teststr+' error nr '+IntToStr(testnr),B);
end;

function testAnsiCompareText : string;
begin
  Result:='';
  teststr:='AnsiCompareText';
  if not Check(ansicomparetext('a', 'a') = 0, 1) then exit;
  if not Check(ansicomparetext('a', 'A') = 0, 2) then exit;
  if not Check(ansicomparetext('A', 'a') = 0, 3) then exit;
  if not Check(ansicomparetext('a', 'b') < 0, 4) then exit;
  if not Check(ansicomparetext('c', 'b') > 0, 5) then exit;
  if not Check(ansicomparetext('abc', 'AbC') = 0, 6) then exit;
  if not Check(ansicomparetext('0123456789', '0123456789') = 0, 7) then exit;
  if not Check(ansicomparetext('', '0123456789') < 0, 8) then exit;
  if not Check(ansicomparetext('AbC', '') > 0, 9) then exit;
  if not Check(ansicomparetext('AbC', 'A') > 0, 10) then exit;
  if not Check(ansicomparetext('AbC', 'Ab') > 0, 11) then exit;
  if not Check(ansicomparetext('AbC', 'ab') > 0, 12) then exit;
  if not Check(ansicomparetext('Ab'#0'C', 'ab'#0) > 0, 13) then exit;
end;


function testAnsiStrIComp : string;
begin
  Result:='';
  teststr:='AnsiStrIComp';
  if not Check(ansistricomp('a', 'a') = 0, 1) then exit;
  if not Check(ansistricomp('a', 'A') = 0, 2) then exit;
  if not Check(ansistricomp('A', 'a') = 0, 3) then exit;
  if not Check(ansistricomp('a', 'b') < 0, 4) then exit;
  if not Check(ansistricomp('c', 'b') > 0, 5) then exit;
  if not Check(ansistricomp('abc', 'AbC') = 0, 6) then exit;
  if not Check(ansistricomp('0123456789', '0123456789') = 0, 7) then exit;
  if not Check(ansistricomp('', '0123456789') < 0, 8) then exit;
  if not Check(ansistricomp('AbC', '') > 0, 9) then exit;
  if not Check(ansistricomp('AbC', 'A') > 0, 10) then exit;
  if not Check(ansistricomp('AbC', 'Ab') > 0, 11) then exit;
  if not Check(ansistricomp('AbC', 'ab') > 0, 12) then exit;
  if not Check(ansistricomp('Ab'#0'C', 'ab'#0) = 0, 13) then exit;
end;


Function testAnsiStrLComp : string;

begin
  Result:='';
  teststr:='AnsiStrLComp';
  if not Check (ansistrlcomp ('', '', 0) = 0, 1) then exit; { Trivial case. }
  if not Check (ansistrlcomp ('a', 'a', 1) = 0, 2) then exit;       { Identity. }
  if not Check (ansistrlcomp ('abc', 'abc', 3) = 0, 3) then exit;   { Multicharacter. }
  if not Check (ansistrlcomp ('abc'#0, 'abcd', 4) < 0, 4) then exit;   { Length unequal. }
  if not Check (ansistrlcomp ('abcd', 'abc'#0, 4) > 0, 5) then exit;
  if not Check (ansistrlcomp ('abcd', 'abce', 4) < 0, 6) then exit;  { Honestly unequal. }
  if not Check (ansistrlcomp ('abce', 'abcd', 4) > 0, 7) then exit;
  if not Check (ansistrlcomp ('abce', 'abcd', 3) = 0, 10) then exit; { Count limited. }
  if not Check (ansistrlcomp ('abce', 'abc', 3) = 0, 11) then exit;  { Count = length. }
  if not Check (ansistrlcomp ('abcd', 'abce', 4) < 0, 12) then exit;  { Nudging limit. }
  if not Check (ansistrlcomp ('abc', 'def', 0) = 0, 13) then exit;   { Zero count. }
  if not Check (ansistrlcomp ('abc'#0'e', 'abc'#0'd', 5) > 0, 14) then exit;
end;


function testAnsiCompareStr : string;
begin
  Result:='';
  teststr:='AnsiCompareStr';
  if not Check (ansicomparestr ('', '') = 0, 1) then exit;              { Trivial case. }
  if not Check (ansicomparestr ('a', 'a') = 0, 2) then exit;            { Identity. }
  if not Check (ansicomparestr ('abc', 'abc') = 0, 3) then exit;        { Multicharacter. }
  if not Check (ansicomparestr ('abc', 'abcd') < 0, 4) then exit;        { Length mismatches. }
  if not Check (ansicomparestr ('abcd', 'abc') > 0, 5) then exit;
  if not Check (ansicomparestr ('abcd', 'abce') < 0, 6) then exit;       { Honest miscompares. }
  if not Check (ansicomparestr ('abce', 'abcd') > 0, 7) then exit;
  if not Check (ansicomparestr ('abc'#0'e', 'abc'#0'd') > 0, 8) then exit;
end;


function testAnsiStrComp : string;
begin
  Result:='';
  teststr:='AnsiStrComp';
  if not Check (ansistrcomp ('', '') = 0, 1) then exit;              { Trivial case. }
  if not Check (ansistrcomp ('a', 'a') = 0, 2) then exit;            { Identity. }
  if not Check (ansistrcomp ('abc', 'abc') = 0, 3) then exit;        { Multicharacter. }
  if not Check (ansistrcomp ('abc', 'abcd') < 0, 4) then exit;        { Length mismatches. }
  if not Check (ansistrcomp ('abcd', 'abc') > 0, 5) then exit;
  if not Check (ansistrcomp ('abcd', 'abce') < 0, 6) then exit;       { Honest miscompares. }
  if not Check (ansistrcomp ('abce', 'abcd') > 0, 7) then exit;
  if not Check (ansistrcomp ('abc'#0'e', 'abc'#0'd') = 0, 8) then exit;
end;


Function testAnsiStrLIComp : string;
begin
  Result:='';
  teststr:='AnsiStrLIComp';
  if not Check(ansistrlicomp('a', 'a', 1) = 0, 1) then exit;
  if not Check(ansistrlicomp('a', 'A', 1) = 0, 2) then exit;
  if not Check(ansistrlicomp('A', 'a', 1) = 0, 3) then exit;
  if not Check(ansistrlicomp('a', 'b', 1) < 0, 4) then exit;
  if not Check(ansistrlicomp('c', 'b', 1) > 0, 5) then exit;
  if not Check(ansistrlicomp('abc', 'AbC', 3) = 0, 6) then exit;
  if not Check(ansistrlicomp('0123456789', '0123456789', 10) = 0, 7) then exit;
  if not Check(ansistrlicomp(#0'123456789', #0'123456799', 10) < 0, 8) then exit;
  if not Check(ansistrlicomp(#0'bD', #0'bC', 3) > 0, 9) then exit;
  if not Check(ansistrlicomp('AbC', 'A'#0#0,3) > 0, 10) then exit;
  if not Check(ansistrlicomp('AbC', 'Ab'#0, 3) > 0, 11) then exit;
  if not Check(ansistrlicomp('AbC', 'ab'#0, 3) > 0, 12) then exit;
  if not Check(ansistrlicomp('0123456789', 'AbC', 0) = 0, 13) then exit;
  if not Check(ansistrlicomp('AbC', 'abc', 1) = 0, 14) then exit;
  if not Check(ansistrlicomp('AbC', 'abc', 2) = 0, 15) then exit;
  if not Check(ansistrlicomp('AbC', 'abc', 3) = 0, 16) then exit;
  if not Check(ansistrlicomp('AbC', 'abcd', 3) = 0, 17) then exit;
  if not Check(ansistrlicomp('AbCc', 'abcd', 4) < 0, 18) then exit;
  if not Check(ansistrlicomp('ADC', 'abcd', 1) = 0, 19) then exit;
  if not Check(ansistrlicomp('ADC', 'abcd', 2) > 0, 20) then exit;
  if not Check(ansistrlicomp('abc'#0'e', 'abc'#0'd', 5) > 0, 21) then exit;
end;


begin
  SysutilsTest('testAnsiCompareText',@testAnsiCompareText);
  SysutilsTest('testAnsiStrIComp',@testAnsiStrIComp);
  SysutilsTest('testAnsiStrLComp',@testAnsiStrLComp);
  SysutilsTest('testAnsiCompareStr',@testAnsiCompareStr);
  SysutilsTest('testAnsiStrComp',@testAnsiStrComp);
  SysutilsTest('testAnsiStrLIComp',@testAnsiStrLIComp);
end.
