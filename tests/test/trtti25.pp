program trtti25;

{$mode objfpc}{$H+}

uses
  TypInfo;

type
  {$M+}
  TSub1 = class

  end;

  TSub2 = class

  end;

  TTest = class
  published
    Field1: TSub1;
    Field2: TSub2;
    Field3: TSub1;
  end;

var
  vmt: PVmt;
  vft: PVmtFieldTable;
  vfe: PVmtFieldEntry;
  vfct: PVmtFieldClassTab;
  t: TTest;
begin
  t := TTest.Create;

  vmt := PVmt(TTest);
  vft := PVmtFieldTable(vmt^.vFieldTable);
  if vft^.Count <> 3 then
    Halt(1);
  vfct := PVmtFieldClassTab(vft^.ClassTab);
  if not Assigned(vfct) then
    Halt(2);

  vfe := vft^.Field[0];
  if vfe^.Name <> 'Field1' then
    Halt(3);
  if vfe^.TypeIndex > vfct^.Count then
    Halt(4);
  if vfct^.ClassRef[vfe^.TypeIndex - 1]^.ClassName <> 'TSub1' then
    Halt(5);
  if t.FieldAddress('Field1') <> PByte(t) + vfe^.FieldOffset then
    Halt(6);

  vfe := vft^.Field[1];
  if vfe^.Name <> 'Field2' then
    Halt(7);
  if vfe^.TypeIndex > vfct^.Count then
    Halt(8);
  if vfct^.ClassRef[vfe^.TypeIndex - 1]^.ClassName <> 'TSub2' then
    Halt(9);
  if t.FieldAddress('Field2') <> PByte(t) + vfe^.FieldOffset then
    Halt(10);

  vfe := vft^.Field[2];
  if vfe^.Name <> 'Field3' then
    Halt(11);
  if vfe^.TypeIndex > vfct^.Count then
    Halt(12);
  if vfct^.ClassRef[vfe^.TypeIndex - 1]^.ClassName <> 'TSub1' then
    Halt(13);
  if t.FieldAddress('Field3') <> PByte(t) + vfe^.FieldOffset then
    Halt(14);
end.
