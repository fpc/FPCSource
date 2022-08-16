program RTTITest;

{$mode delphi}{$h+}

uses
  SysUtils, Classes, TypInfo;

type
  TMyEnum = (meOne=1, meThree=3, meFive=5, meSix);

  TMyClass = class(TPersistent)
  private
    FEnum: TMyEnum;
  Published
    property Enum: TMyEnum read FEnum write FEnum;
  end;

var
  PI: PPropInfo;
  aClass: TMyClass;
  TypeData: PTypeData;
begin
  aClass:=TMyClass.Create;
  TypeData:=GetTypeData(aClass.ClassInfo);
  if TypeData^.PropCount<>0 then
    Halt(1);

  PI:=GetPropInfo(aClass,'Enum',tkAny);
  if Assigned(PI) then
    Halt(2);
  aClass.Free;
end.
