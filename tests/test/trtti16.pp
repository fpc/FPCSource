program trtti16;

{$mode delphi}

uses
  TypInfo;

type
  TFoo = class
    B: Byte;
    W: Word;
    L: LongWord;
    S: string;
    I: IInterface;
    A: TArray<byte>;
  end;

  TBoo = object
    B: Byte;
    W: Word;
    L: LongWord;
    S: string;
    I: IInterface;
    A: TArray<byte>;
  end;

  TBoo2 = object(TBoo)
    S2: string;
    B2: Byte;
  end;

var
  td: PTypeData;
  vmt: PVmt;
  rid: PRecInitData;
begin
  td := GetTypeData(TypeInfo(TFoo));
  vmt := PVmt(td^.ClassType);
  rid := PRecInitData(GetTypeData(vmt.vInitTable));
  if rid^.ManagedFieldCount <> 3 then
    Halt(1);

  td := GetTypeData(TypeInfo(TBoo));
  if td^.TotalFieldCount <> 6 then
    Halt(2);

  rid := td.RecInitData;
  if rid^.ManagedFieldCount <> 3 then
    Halt(3);

  td := GetTypeData(TypeInfo(TBoo2));
  if td^.TotalFieldCount <> 3 then
    Halt(4);

  rid := td.RecInitData;
  if rid^.ManagedFieldCount <> 2 then
    Halt(5);
end.
