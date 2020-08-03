unit lebutils;

interface

uses
  SysUtils, Classes, wasmbin;

function ReadU(src: TStream): UInt64;
function ReadS(src: TStream; bits: Integer): Int64;

procedure ReadFuncTypesArray(src: TStream; var arr: TFuncTypeArray);
procedure ReadFuncType(src: TStream; var ft: TFuncType);

procedure ReadCodeEntry(src: TStream; var en: TCodeEntry);
procedure ReadCodeSection(src: TStream; var sc: TCodeSection);

implementation

procedure ReadFuncType(src: TStream; var ft: TFuncType);
var
  c: integer;
begin
  // vector of t1
  c:=ReadU(src);
  SetLength(ft.param, c);
  src.Read(ft.param[0], c);

  // vector of t2
  c:=ReadU(src);
  SetLength(ft.result, c);
  src.Read(ft.result[0], c);
end;

procedure ReadFuncTypesArray(src: TStream; var arr: TFuncTypeArray);
var
  cnt : integer;
  i   : Integer;
begin
  cnt := ReadU(src);
  SetLength(arr.funTypes, cnt);
  for i:=0 to cnt-1 do begin
    if src.ReadByte = func_type then
      ReadFuncType(src, arr.funTypes[i]);
  end;
end;

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

procedure ReadCodeEntry(src: TStream; var en: TCodeEntry);
var
  sz  : integer; // size in bytes
  //pos : int64;
  cnt : Integer;
  i   : integer;
begin
  sz := ReadU(src);

  cnt := ReadU(src);
  SetLength(en.locals, cnt);
  for i:=0 to cnt-1 do begin
    en.locals[i].count := ReadU(src);
    en.locals[i].valtyp := src.ReadByte;
  end;


end;

procedure ReadCodeSection(src: TStream; var sc: TCodeSection);
var
  cnt : integer;
  i   : integer;
begin
  cnt := ReadU(src);
  SetLength(sc.entries, cnt);
  for i:= 0 to cnt-1 do
    ReadCodeEntry(src, sc.entries[i]);
end;

end.
