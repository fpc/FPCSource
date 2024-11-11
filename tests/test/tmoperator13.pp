{$Mode ObjFpc}{$H+}
{$ModeSwitch AdvancedRecords}

uses SysUtils;

type
  TMyRec = record
    i: Integer;
    class operator :=(const rhs: Integer): TMyRec;
    class operator Copy(constref src: TMyRec; var dst: TMyRec);
  end;

class operator TMyRec.:=(const rhs: Integer): TMyRec;
begin
  Result.i:=rhs;
end;

var
  CopyCount: Integer = 0;
class operator TMyRec.Copy(constref src: TMyRec; var dst: TMyRec);
begin
  Inc(CopyCount);
  dst.i:=src.i;
end;

var
  r1, r2, r3: TMyRec;
begin
  r1 := 42;
  r2 := 32;
  specialize Swap<TMyRec>(r1,r2);
  if (r1.i<>32) or (r2.i<>42) then
    Halt(1);
  r3 := specialize Extract<TMyRec>(r1);
  if (r3.i<>32) then
    Halt(2);
  if CopyCount <> 0 then
    Halt(3);
  WriteLn('Ok');
end.
