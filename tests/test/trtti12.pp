program trtti12;

{$MODE DELPHI}

uses
  TypInfo;

type
  PFoo = ^TFoo;
  TFoo = packed record
  public
    B: Byte;
    W: Word;
    L: LongWord;
    S: string;
    I: IInterface;
    A: TArray<byte>;
  end;

var
  td: PTypeData;
  id: PRecInitData;
begin
  td := GetTypeData(TypeInfo(TFoo));

  id := td.RecInitData;
  if id.Terminator <> nil then
    Halt(1);

  if td.ManagedFldCount <> 6 then
    Halt(2);

  if id.ManagedFieldCount <> 3 then
    Halt(3);
end.
