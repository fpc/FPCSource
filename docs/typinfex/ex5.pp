program example5;

{ This program demonstrates the GetObjectProp function }

{$mode objfpc}

uses rttiobj,typinfo;

Var
  O : TMyTestObject;
  PI : PPropInfo;
  NO1,NO2 : TNamedObject;

begin
  O:=TMyTestObject.Create;
  NO1:=TNamedObject.Create;
  NO1.ObjectName:='First named object';
  NO2:=TNamedObject.Create;
  NO2.ObjectName:='Second named object';
  O.ObjField:=NO1;
  Writeln('Object property : ');
  PI:=GetPropInfo(O,'ObjField');
  Write('Property class     : ');
  Writeln(GetObjectPropClass(O,'ObjField').ClassName);
  Write('Value              : ');
  Writeln((O.ObjField as TNamedObject).ObjectName);
  Write('Get (name)         : ');
  Writeln((GetObjectProp(O,'ObjField') As TNamedObject).ObjectName);
  Write('Get (propinfo)     : ');
  Writeln((GetObjectProp(O,PI,TObject) as TNamedObject).ObjectName);
  SetObjectProp(O,'ObjField',NO2);
  Write('Set (name,NO2)     : ');
  Writeln((O.ObjField as TNamedObject).ObjectName);
  SetObjectProp(O,PI,NO1);
  Write('Set (propinfo,NO1) : ');
  Writeln((O.ObjField as TNamedObject).ObjectName);
  O.Free;
end.