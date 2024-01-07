program trtti26;

{$mode objfpc}{$H+}

uses
  TypInfo;

type
  {$M+}
  TTest = class
  published
    procedure Test(aArg1: LongInt);
    procedure Test2(aArg1: String);
  end;

procedure TTest.Test(aArg1: LongInt);
begin

end;

procedure TTest.Test2(aArg1: String);
begin

end;

var
  vmt: PVmt;
  mt: PVmtMethodTable;
  me: PVmtMethodEntry;
  m: TMethod;
  t: TTest;
begin
  t := TTest.Create;
  vmt := PVmt(TTest);
  mt := PVmtMethodTable(vmt^.vMethodTable);
  if mt^.Count <> 2 then
    Halt(1);
  me := mt^.Entry[0];
  if me^.Name^ <> 'Test' then
    Halt(2);
  m := TMethod(@t.Test);
  if me^.CodeAddress <> m.Code then
    Halt(3);
  if t.MethodAddress('Test') <> m.Code then
    Halt(4);
  me := mt^.Entry[1];
  if me^.Name^ <> 'Test2' then
    Halt(5);
  m := TMethod(@t.Test2);
  if me^.CodeAddress <> m.Code then
    Halt(6);
  if t.MethodAddress('Test2') <> m.Code then
    Halt(7);
end.
