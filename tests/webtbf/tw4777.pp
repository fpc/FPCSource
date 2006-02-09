{ %fail }

{ Source provided for Free Pascal Bug Report 4777 }
{ Submitted by "Phil H." on  2006-02-06 }
{ e-mail: pjhess@purdue.edu }
unit tw4777;

{$mode objfpc}

interface

uses
  SysUtils;

function GetVal(astr : string) : Integer;


implementation

function GetVal(const astr : string) : Integer;
begin
  Result := StrToInt(astr);
end;


end.

