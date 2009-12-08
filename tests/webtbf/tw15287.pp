{ %fail }

program arrayrangeoperator;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  TArrayOfInteger = array of integer;
var
  a, b: TArrayOfInteger;
begin
  SetLength(a,10);
  b:=a[2..4];
end.


