{ %FAIL }
{ %OPT=-FNudots }

program tudots4;

uses
  { both reference the same unit }
  UDots.Unit1,
  Unit1;

begin
end.

