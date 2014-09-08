{ %NORUN }

program tw24867;

{$MODE DELPHi}{$H+}
{$APPTYPE CONSOLE}

uses
  fgl;

var
  d: TFPGMap<string, string>; // ok
begin
  d := TFPGMap<string, string>.Create; // Error: This type can't be a generic
end.

