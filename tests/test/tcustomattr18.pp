program tcustomattr18;

{$mode objfpc}
{$modeswitch prefixedattributes}

uses
  TypInfo;

type
  TAttr = class(TCustomAttribute)
    constructor Create;
  end;

  [TAttr]
  TTestRec = record

  end;

  [TAttr]
  TEnum = (
    eOne
  );

  [TAttr]
  TSet = set of TEnum;

  [TAttr]
  TPtr = ^LongInt;

  [TAttr]
  TLongInt = type LongInt;

  [TAttr]
  TMyMethod = procedure of object;

  [TAttr]
  TMyProc = procedure;

  [TAttr]
  TMyStaticArray = array[0..3] of Integer;

  [TAttr]
  TMyDynArray = array of Integer;

  [TAttr]
  IMyIntf = interface

  end;

  [TAttr]
  TString8 = String[8];

  [TAttr]
  TStringCP = type AnsiString(1234);

constructor TAttr.Create;
begin

end;

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
    if attr.ClassType <> TAttr then
      Halt(i * 20 + 3);
  end;

  Writeln('ok');
end.
