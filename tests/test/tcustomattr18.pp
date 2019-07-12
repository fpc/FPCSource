program tcustomattr18;

{$mode objfpc}
{$modeswitch prefixedattributes}

uses
  TypInfo;

type
  [TCustomAttribute]
  TTestRec = record

  end;

  [TCustomAttribute]
  TEnum = (
    eOne
  );

  [TCustomAttribute]
  TSet = set of TEnum;

  [TCustomAttribute]
  TPtr = ^LongInt;

  [TCustomAttribute]
  TLongInt = type LongInt;

  [TCustomAttribute]
  TMyMethod = procedure of object;

  [TCustomAttribute]
  TMyProc = procedure;

  [TCustomAttribute]
  TMyStaticArray = array[0..3] of Integer;

  [TCustomAttribute]
  TMyDynArray = array of Integer;

  [TCustomAttribute]
  IMyIntf = interface

  end;

  [TCustomAttribute]
  TString8 = String[8];

  [TCustomAttribute]
  TStringCP = type AnsiString(1234);

var
  typeinfos: array of PTypeInfo;
  i: SizeInt;
  at: PAttributeTable;
  attr: TCustomAttribute;
begin
  typeinfos := [
    TypeInfo(TTestRec),
    TypeInfo(TEnum),
    TypeInfo(TSet),
    TypeInfo(TPtr),
    TypeInfo(TLongInt),
    TypeInfo(TMyMethod),
    TypeInfo(TMyProc),
    TypeInfo(TMyStaticArray),
    TypeInfo(TMyDynArray),
    TypeInfo(IMyIntf),
    TypeInfo(TString8),
    TypeInfo(TStringCP)
  ];

  for i := 0 to High(typeinfos) do begin
    at := GetAttributeTable(typeinfos[i]);
    if not Assigned(at) then
      Halt(i * 10);
    if at^.AttributeCount <> 1 then
      Halt(i * 10 + 1);
    attr := GetAttribute(at, 0);
    if not Assigned(attr) then
      Halt(i * 10 + 2);
    if attr.ClassType <> TCustomAttribute then
      Halt(i * 20 + 3);
  end;

  Writeln('ok');
end.
