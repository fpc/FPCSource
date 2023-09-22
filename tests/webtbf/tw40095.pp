{ %FAIL }

program DelphiAttrCreate;

{$mode delphi}
{$ModeSwitch prefixedattributes}

uses
  Classes, TypInfo;

type
  MyAttr = class(TCustomAttribute)
  public
    constructor Create(const A: Boolean);
  end;

  TMyObj = class
  private
    fProp1: string;
  published
    [MyAttr]
    property Prop1: string read fProp1 write fProp1;
  end;

{ MyAttr }

constructor MyAttr.Create(const A: Boolean);
begin

end;

var
  O: TMyObj;
  TypeData: TTypeData;
  PropList: PPropList;
  PropInfo: PPropInfo;
  I, A: Integer;
  Attribute: TCustomAttribute;
  AttrFound: array of TClass;
begin
  AttrFound := nil;
  O := TMyObj.Create;
  TypeData := GetTypeData(O.ClassInfo)^;
  if TypeData.PropCount>0 then
  begin
    GetMem(PropList, TypeData.PropCount*SizeOf(Pointer));
    GetPropInfos(O.ClassInfo, PropList);
    for I := 0 to TypeData.PropCount-1 do
    begin
      PropInfo := PropList^[I];
      if Assigned(PropInfo.AttributeTable) then
      begin
        for A := 0 to PropInfo.AttributeTable^.AttributeCount-1 do
        begin
          Attribute := PropInfo.AttributeTable^.AttributesList[I].AttrProc;
          // Writeln(Attribute.ClassName);
          AttrFound := AttrFound + [Attribute.ClassType];
          Attribute.Free;
        end;
      end;
    end;
    FreeMem(PropList, TypeData.PropCount*SizeOf(Pointer));
  end;

  if not((Length(AttrFound)=1) and (AttrFound[0]=MyAttr.ClassType)) then
    Halt(1);
end.

