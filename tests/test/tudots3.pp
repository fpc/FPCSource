{ %FAIL }
{ %OPT=-FNudots }

program tudots3;

uses
  { both reference the same unit }
  Unit1,
  UDots.Unit1;

begin
end.

