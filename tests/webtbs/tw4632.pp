{ Source provided for Free Pascal Bug Report 4632 }
{ Submitted by "Graeme Geldenhuys" on  2005-12-23 }
{ e-mail: graemeg@gmail.com }
program Project1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

var
  err : boolean;

procedure Error(const s:string);
begin
  writeln(s);
  err:=true;
end;

{ Fixed version of ExtractFileName function }
function lExtractFileName(const FileName: string): string;
var
  i: longint;
begin
  I := LastDelimiter(PathDelim + DriveDelim, FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

var
  name, s: string;
  i: integer;

const
  ext = '.txt';

begin
  name := '';
  for i := 1 to 251 do
    name := name + 'a';   // complete name of 255 chars

  s := ExtractFileName(name + ext);
  if Length(s) <> 255 then
    Error('Failed on 1');

  s := lExtractFileName(name + ext);
  if Length(s) <> 255 then
    Error('Failed on 2');

  name := name + 'a';     // complete name on 256 chars
  s := ExtractFileName(name + ext);
  if Length(s) <> 256 then
    Error('Failed on 3');

  s := lExtractFileName(name + ext);
  if Length(s) <> 256 then
    Error('Failed on 4');

  if err then
    halt(1);
end.