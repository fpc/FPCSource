{ Source provided for Free Pascal Bug Report 3010 }
{ Submitted by "C Western" on  2004-03-13 }
{ e-mail: mftq75@dsl.pipex.com }
program bug1;

{$mode objfpc}{$H+}
{$R+}

uses
  Classes;

function Mumble(var p: TPoint; s: TStringList): Char;
begin
  if p.x = Length(s[p.y]) then
    Result := 'a'
  else
    Result := 'b';
end;

var
  p: TPoint;
  s: TStringList;
  c: char;
begin
  p.x := 7;
  p.y := 0;
  s := TStringList.Create;
  s.Add('1234567');
  c:=Mumble(p, s);
  writeln(c);
  if c<>'a' then
    begin
      writeln('ERROR!');
      halt(1);
    end;
end.
