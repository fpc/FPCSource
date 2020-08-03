unit lebutils;

interface

uses
  SysUtils, Classes;

function ReadU(src: TStream): UInt64;
function ReadS(src: TStream; bits: Integer): Int64;

procedure WriteU8 (src: TStream; vl: UInt8);
procedure WriteU16(src: TStream; vl: UInt16);
procedure WriteU32(src: TStream; vl: UInt32);
procedure WriteU64(src: TStream; vl: UInt64);
procedure WriteS8 (src: TStream; vl: Int8);
procedure WriteS16(src: TStream; vl: Int16);
procedure WriteS32(src: TStream; vl: Int32);
procedure WriteS64(src: TStream; vl: Int64);

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

procedure WriteU8(src: TStream; vl: UInt8);
begin

end;

procedure WriteU16(src: TStream; vl: UInt16);
begin

end;

procedure WriteU32(src: TStream; vl: UInt32);
begin

end;

procedure WriteU64(src: TStream; vl: UInt64);
begin
end;

procedure WriteS8 (src: TStream; vl: Int8);
begin
end;

procedure WriteS16(src: TStream; vl: Int16);
begin
end;

procedure WriteS32(src: TStream; vl: Int32);
begin
end;

procedure WriteS64(src: TStream; vl: Int64);
begin
end;

end.
