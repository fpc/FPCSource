{ %fail }

{ Source provided for Free Pascal Bug Report 3790 }
{ Submitted by "C Western" on  2005-03-14 }
{ e-mail: mftq75@dsl.pipex.com }
program test;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

function A(Error: Double):string;
begin
  WriteLn('Hello');
  if Error < 0 then begin
    Result := Str(Error:5);
  end;
end;

begin
  WriteLn(A(-53));
end.
