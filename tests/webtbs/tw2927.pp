{$mode objfpc}

uses
  Classes;

procedure ReadTest(const p: Pointer); overload;
begin
end;

procedure ReadTest(const p: TStream); overload;
begin
end;

var
  f: TFileStream;
begin
  ReadTest(f);
end.
