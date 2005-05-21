{ Program to test system unit setstring routines
  Tested against Delphi 3 and (where possible)
  against Borland Pascal v7.01
  Copyright (c) 2002 Carl Eric Codere
}
program tsetstr;
{$R+}
{$Q+}

{$ifndef MACOS}
{$APPTYPE CONSOLE}
{$else}
{$APPTYPE TOOL}
{$endif}

{$ifdef fpc}
  {$ifndef ver1_0}
    {$define haswidestring}
  {$endif}
{$else}
  {$ifndef ver70}
    {$define haswidestring}
  {$endif}
{$endif}
{$ifdef fpc}
  uses strings;
{$else}
  uses SysUtils;
{$endif}
const
  HELLO_STRING = 'Hello my little world!';
  PCHAR_NULL = nil;
  PCHAR_EMPTY : pchar = #0;
  PCHAR_NORMAL : pchar = HELLO_STRING;


var
   arr: array[0..255] of byte;
   str1 : shortstring;
   str2 : ansistring;
{$ifdef haswidestring}
   str3 : widestring;
{$endif}

procedure fail;
 begin
   WriteLn('Failed!');
   Halt(1);
 end;

procedure test_shortstring;
var
 i: longint;
 _failed : boolean;
begin
  _failed := false;
  write('Testing setstring() with shortstring...');
  { buffer : pchar with #0 character }
  {          pchar = nil             }
  {          pchar = valid value     }
  str1:='';
  setstring(str1, PCHAR_NULL, 0);
  if str1 <> '' then
    _failed := true;
  str1:='';
  setstring(str1,PCHAR_EMPTY,strlen(PCHAR_EMPTY));
  if str1 <> '' then
    _failed := true;
  setstring(str1,PCHAR_NORMAL,strlen(PCHAR_NORMAL));
  if str1 <> HELLO_STRING then
    _failed := true;
  { len = 0, len = normal length, len > 255 }
  str1:='';
  setstring(str1, PCHAR_NORMAL, 0);
  if str1 <> '' then
    _failed := true;
  str1:='';
  fillchar(arr,sizeof(arr),'a');
  setstring(str1,@arr[0],512);
  if length(str1) <> 255 then
    _failed := true;
  for i := 1 to length(str1) do
    if str1[i] <> 'a' then
      _failed := true;
  str1:='';
  setstring(str1,PCHAR_NORMAL,strlen(PCHAR_NORMAL));
  if str1 <> HELLO_STRING then
    _failed := true;
  if _failed then
    fail;
  writeln('Passed!');
end;


procedure test_ansistring;
var
 _failed : boolean;
begin
  _failed := false;
  write('Testing setstring() with ansistring...');
  { buffer : pchar with #0 character }
  {          pchar = nil             }
  {          pchar = valid value     }
  str2:='';
  setstring(str2, PCHAR_NULL, 0);
  if str2 <> '' then
    _failed := true;
  str2:='';
  setstring(str2,PCHAR_EMPTY,strlen(PCHAR_EMPTY));
  if str2 <> '' then
    _failed := true;
  setstring(str2,PCHAR_NORMAL,strlen(PCHAR_NORMAL));
  if str2 <> HELLO_STRING then
    _failed := true;
  { len = 0, len = normal length, len > 255 }
  str2:='';
  setstring(str2, PCHAR_NORMAL, 0);
  if str2 <> '' then
    _failed := true;
  str2:='';
  setstring(str2,PCHAR_NORMAL,strlen(PCHAR_NORMAL));
  if str2 <> HELLO_STRING then
    _failed := true;
  if _failed then
    fail;
  writeln('Passed!');
end;

{$ifdef haswidestring}
procedure test_widestring;
var
 _failed : boolean;
begin
  _failed := false;
  write('Testing setstring() with widestring...');
  { buffer : pchar with #0 character }
  {          pchar = nil             }
  {          pchar = valid value     }
  str3:='';
  setstring(str3, PCHAR_NULL, 0);
  if str3 <> '' then
    _failed := true;
  str3:='';
  setstring(str3,PCHAR_EMPTY,strlen(PCHAR_EMPTY));
  if str3 <> '' then
    _failed := true;
  setstring(str3,PCHAR_NORMAL,strlen(PCHAR_NORMAL));
  if str3 <> HELLO_STRING then
    _failed := true;
  { len = 0, len = normal length, len > 255 }
  str3:='';
  setstring(str3, PCHAR_NORMAL, 0);
  if str3 <> '' then
    _failed := true;
  str3:='';
  setstring(str3,PCHAR_NORMAL,strlen(PCHAR_NORMAL));
  if str3 <> HELLO_STRING then
    _failed := true;
  if _failed then
    fail;
  writeln('Passed!');
end;
{$endif}


Begin
  test_shortstring;
  test_ansistring;
{$ifdef haswidestring}
  test_widestring;
{$endif}
end.
