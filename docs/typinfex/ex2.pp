program example2;

{ This program demonstrates the GetEnumProp function }

{$mode objfpc}

uses rttiobj,typinfo;

Var
  O : TMyTestObject;
  PI : PPropInfo;
  TI : PTypeInfo;

begin
  O:=TMyTestObject.Create;
  PI:=GetPropInfo(O,'MyEnumField');
  TI:=PI^.PropType;
  Writeln('Enum property    : ');
  Writeln('Value                   : ',GetEnumName(TI,Ord(O.MyEnumField)));
  Writeln('Get (name)              : ',GetEnumProp(O,'MyEnumField'));
  Writeln('Get (propinfo)          : ',GetEnumProp(O,PI));
  SetEnumProp(O,'MyEnumField','meFirst');
  Writeln('Set (name,meFirst)      : ',GetEnumName(TI,Ord(O.MyEnumField)));
  SetEnumProp(O,PI,'meSecond');
  Writeln('Set (propinfo,meSecond) : ',GetEnumName(TI,Ord(O.MyEnumField)));
  O.Free;
end.