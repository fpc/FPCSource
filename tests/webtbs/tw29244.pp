program project1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

Var
  AStr : String = '';
begin
  WriteLn(ConcatPaths(['one', 'two']));
end.
