{ Source provided for Free Pascal Bug Report 3765 }
{ Submitted by "Andrew Haines" on  2005-03-09 }
{ e-mail: andrewd207@aol.com }
program Project1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;
var
  AName, ANumber, ABoolean: Variant;
begin
  AName := 'John Smith';
  ANumber := 12345;
  ABoolean := True;
  WriteLn('Name=',AName, ' Number=', ANumber, ' Boolean=',ABoolean);
end.
