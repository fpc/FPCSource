program tover1;

const
  RESULT_PCHAR = 'pchar parameter call';
  RESULT_ANSI  = 'ansistring parameter call';
  RESULT_SHORT = 'shortstring parameter call';
  RESULT_WIDE  = 'widestring parameter call';

{ This tests method overloads, to verify
  if they conform to correct type conversion
}
function test_string(s: shortstring): shortstring;
Begin
  test_string := RESULT_SHORT;
end;

{$ifndef ver1_0}
function test_string(s: widestring): shortstring;
Begin
  test_string := RESULT_WIDE;
end;
{$endif}


function test_string(s: ansistring): shortstring;
Begin
  test_string := RESULT_ANSI;
end;

function test_string(p:pchar): shortstring;
begin
  test_string := RESULT_PCHAR;
end;

procedure fail;
 begin
   writeln('Failure!');
   Halt(1);
 end;

var
  short_string : shortstring;
  ansi_string : ansistring;
{$ifndef ver1_0}
  wide_string : widestring;
{$endif}
  p_string : pchar;
  s: shortstring;
Begin
  p_string:=nil;
  ansi_string:='';
  short_string:='';
{$ifndef ver1_0}
  wide_string:='';
{$endif}
  Write('Test of overloaded call to string routines...');
  { test parameter call }
  s:=test_string(short_string);
  if s <> RESULT_SHORT then
    fail;
  s:=test_string(ansi_string);
  if s <> RESULT_ANSI then
    fail;
{$ifndef ver1_0}
  s:=test_string(wide_string);
  if s <> RESULT_WIDE then
    fail;
{$endif}
  s:=test_string(p_string);
  if s <> RESULT_PCHAR then
    fail;
  WriteLn('Success!');
end.
