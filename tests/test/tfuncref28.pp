program tfuncref28;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

uses
  TypInfo;

type
  {$M+}
  TFunc = reference to function: LongInt;
  {$M-}
  TFunc2 = reference to function: LongInt;

var
  ti: PTypeInfo;
  td: PTypeData;
  intf: PInterfaceData;
  methods: PIntfMethodTable;
begin
  ti := PTypeInfo(TypeInfo(TFunc));
  td := GetTypeData(ti);
  intf := PInterfaceData(td);
  methods := intf^.MethodTable;
  if methods^.Count <> 1 then
    Halt(1);
  if methods^.RTTICount <> 1 then
    Halt(2);
  ti := PTypeInfo(TypeInfo(TFunc2));
  td := GetTypeData(ti);
  intf := PInterfaceData(td);
  methods := intf^.MethodTable;
  if methods^.Count <> 1 then
    Halt(3);
  if methods^.RTTICount <> High(Word) then
    Halt(4);
end.
