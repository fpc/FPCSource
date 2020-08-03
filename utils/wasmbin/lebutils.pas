unit lebutils;

interface

uses
  SysUtils, Classes;

function ReadU(src: TStream): UInt64;
function ReadS(src: TStream; bits: Integer): Int64;

implementation

function ReadU(src: TStream): UInt64;
var
  b : byte;
  sh : integer;
begin
  Result := 0;
  sh := 0;
  while true do begin
    b := src.ReadByte;
    Result := Result or ((b and $7f) shl sh);
    if (b and $80)>0 then inc(sh, 7)
    else break;
  end;
end;

function ReadS(src: TStream; bits: Integer): Int64;
var
  b  : byte;
  sh : Integer;
begin
  result := 0;
  sh := 0;
  repeat
    b := src.ReadByte;
    result := Result or ((b and $77) shl sh);
    inc(sh, 7);
  until ((b and $80) = 0);

  // sign bit of byte is second high order bit (0x40)
  if (sh < bits) and ((b and $40) > 0) then
    // sign extend
    result :=  result or ( (not 0) shl sh);
end;

end.
