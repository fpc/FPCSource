program trtti6;

{$mode objfpc}{$H+}

uses
  typinfo;

type
  {$M+}
  TReferredClass = class
  end;
  {$M-}

  TClassRef = class of TReferredClass;

  {$M+}
  TClass = class
  private
    FRef: TClassRef;
  published
    property Ref: TClassRef read FRef;
  end;
  {$M-}

  TPtr = ^UnicodeString;

var
  Info: PTypeInfo;
  Data: PTypeData;
begin
  // first check TClass.Ref property
  Info := GetPropInfo(PTypeInfo(TClass.ClassInfo), 'Ref')^.PropType;
  if Info^.Kind <> tkClassRef then
    halt(1);
  Data := GetTypeData(Info);
  if Data^.RefType <> TReferredClass.ClassInfo then
    halt(2);
  // next check TRefferedClass.P method
  Info := TypeInfo(TPtr);
  if Info^.Kind <> tkPointer then
    halt(3);
  Data := GetTypeData(Info);
  if Data^.RefType <> TypeInfo(UnicodeString) then
    halt(4);
end.

