Program example12;

{ This program demonstrates the GetPropInfos function }

uses
  rttiobj,typinfo;


Var
  O : TMyTestObject;
  PT : PTypeData;
  PI : PTypeInfo;
  I,J : Longint;
  PP : PPropList;
  prI : PPropInfo;

begin
  O:=TMyTestObject.Create;
  PI:=O.ClassInfo;
  PT:=GetTypeData(PI);
  Writeln('Property Count : ',PT^.PropCount);
  GetMem (PP,PT^.PropCount*SizeOf(Pointer));
  GetPropInfos(PI,PP);
  For I:=0 to PT^.PropCount-1 do
    begin
    With PP^[i]^ do
      begin
      Write('Property ',i+1:3,': ',name:30);
      writeln('  Type: ',TypeNames[typinfo.PropType(O,Name)]);
      end;
    end;
  FreeMem(PP);
  O.Free;
end.

