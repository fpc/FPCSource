program example7;

{ This program demonstrates the GetSetProp function }

{$mode objfpc}

uses rttiobj,typinfo;

Var
  O : TMyTestObject;
  PI : PPropInfo;

Function SetAsString (ASet : TMyEnums) : String;

Var
  i : TmyEnum;

begin
  result:='';
  For i:=mefirst to methird do
    If i in ASet then
      begin
      If (Result<>'') then
        Result:=Result+',';
      Result:=Result+MyEnumNames[i];
      end;
end;

Var
  S : TMyEnums;

begin
  O:=TMyTestObject.Create;
  O.SetField:=[mefirst,meSecond,meThird];
  Writeln('Set property    : ');
  Writeln('Value                        : ',SetAsString(O.SetField));
  Writeln('Ord(Value)                   : ',Longint(O.SetField));
  Writeln('Get (name)                   : ',GetSetProp(O,'SetField'));
  PI:=GetPropInfo(O,'SetField');
  Writeln('Get (propinfo)               : ',GetSetProp(O,PI,false));
  S:=[meFirst,meThird];
  SetOrdProp(O,'SetField',Integer(S));
  Write('Set (name,[mefirst,methird]) : ');
  Writeln(SetAsString(O.SetField));
  S:=[meSecond];
  SetOrdProp(O,PI,Integer(S));
  Write('Set (propinfo,[meSecond])    : ');
  Writeln(SetAsString(O.SetField));
  O.Free;
end.