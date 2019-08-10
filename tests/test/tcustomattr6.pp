program tcustomattr6;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

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
  if not Assigned(pi^.AttributeTable) then
    Halt(1);
  if pi^.AttributeTable^.AttributeCount <> 1 then
    Halt(2);

  AClassAttribute := GetPropAttribute(pi,0) as TCustomAttribute;
  if AClassAttribute = nil then
    halt(3);

  writeln('ok');

end.

