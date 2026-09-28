{ Record composition: the RTTI of a record with an unnamed composition of a
  record with an AnsiString field lists the composed field flattened with its
  type and offset, and the init RTTI has one managed field }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

uses
  TypInfo;

type
  TChildRec = record
    C: Integer;
    S: AnsiString;
  end;

  TComposed = record
    A: Integer;
    contains TChildRec;
  end;

var
  ti: PTypeInfo;
  td: PTypeData;
  mf: PManagedField;
  r: TComposed;
begin
  ti := TypeInfo(TComposed);
  if ti^.Kind<>tkRecord then
    halt(1);
  td := GetTypeData(ti);
  WriteLn('TotalFieldCount: ', td^.TotalFieldCount);
  if td^.TotalFieldCount<>3 then
    halt(2);
  mf := @td^.TotalFieldCount;
  Inc(Pointer(mf), SizeOf(td^.TotalFieldCount));
  if mf[0].TypeRef<>TypeInfo(Integer) then
    halt(3);
  if mf[0].FldOffset<>(UIntPtr(@r.A)-UIntPtr(@r)) then
    halt(4);
  if mf[1].TypeRef<>TypeInfo(Integer) then
    halt(5);
  if mf[1].FldOffset<>(UIntPtr(@r.C)-UIntPtr(@r)) then
    halt(6);
  if mf[2].TypeRef<>TypeInfo(AnsiString) then
    halt(7);
  if mf[2].FldOffset<>(UIntPtr(@r.S)-UIntPtr(@r)) then
    halt(8);
  WriteLn('ManagedFieldCount: ', td^.RecInitData^.ManagedFieldCount);
  if td^.RecInitData^.ManagedFieldCount<>1 then
    halt(9);
  WriteLn('ok');
end.
