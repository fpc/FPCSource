program tclassattribute6;

{$mode objfpc}{$H+}

uses
  typinfo;

type

  { tmyt }

  TMyt = class(TCustomAttribute)
    constructor create;
  end;

type

  { TMyObject }

  TMyObject = class(TObject)
  private
    FInt: integer;
  published
    [TMyt]
    property PublicInt: integer read FInt;
  end;

constructor TMyt.create;
begin

end;


var
  pi: PPropInfo;
  AClassAttribute: TCustomAttribute;

begin
  pi := GetPropInfo(TMyObject.ClassInfo,'PublicInt');
  if pi^.AttributeCount<>1 then
    halt(1);

  AClassAttribute := GetPropAttribute(pi,0) as TCustomAttribute;
  if AClassAttribute = nil then
    halt(2);

  writeln('ok');

end.

