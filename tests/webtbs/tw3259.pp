{ Source provided for Free Pascal Bug Report 3259 }
{ Submitted by "Andreas Hausladen" on  2004-08-18 }
{ e-mail: Andreas.Hausladen@gmx.de }

{$mode objfpc}

unit tw3259;
interface

implementation

function MyFunc(forward: Integer): Integer; forward;

function MyFunc(forward: Integer): Integer;
begin
  Result := forward;
end;

end.
