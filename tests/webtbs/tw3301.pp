{ Source provided for Free Pascal Bug Report 3301 }
{ Submitted by "Alexey Barkovoy" on  2004-09-07 }
{ e-mail: clootie@ixbt.com }
{$APPTYPE CONSOLE}
{$mode delphi}

uses SysUtils;

var
  err : boolean;

procedure WideConstArray(FS: String; const Args: array of const);
var i: Integer; S: String;
begin
  for i:= 0 to High(Args) do
  begin
    if (Args[i].Vtype = vtPointer) or
       (Args[i].Vtype = vtObject) or
       (Args[i].Vtype = vtClass) or
       (Args[i].Vtype = vtVariant)
      then // == 5
    begin
      WriteLn('BAD:  ', i, ' parameter is detected as a pointer one - (',
         Args[i].Vtype, ')');
      err:=true;
    end else
      WriteLn(':  ', i, ' parameter is detected as a some other type - (',
         Args[i].Vtype, ')');
  end;
  S:= Format(FS, Args);
  WriteLn;
  WriteLn('Result is:', S);
end;

var
  w1: WideString;  wc: PWideChar;  cc: WideChar;  S: String;  ch: Char;  pch, pch2: PChar;
begin
  w1 := 'Some other wide string';  wc:= @w1[1];  S:= 'String1';  ch:= 'c';  pch:= @ch;  pch2:= @s[1];  cc:= 'Z';
  // BAD: This will raise exception?
  WideConstArray('%s, %s, %s, %s, %s',    [WideString('wide string'), w1, cc, wc, PWideChar(wc)]);
  if err then
    halt(1);
end.
