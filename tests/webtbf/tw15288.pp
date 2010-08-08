{ %fail }

program setcrash;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

var
  AString: String='blabla';

procedure GetKey(var Key: Char);
begin
  if Key in ['c', AString] then
    writeln('OK');
end;

var
  AKey: Char;
begin
  AKey := 'c';
  GetKey(AKey);
end.


