Program example13;

{ This program demonstrates the FindPropInfo function }

{$mode objfpc}

uses
  rttiobj,typinfo,sysutils;


Var
  O : TMyTestObject;
  PT : PTypeData;
  PI : PPropInfo;
  I,J : Longint;
  PP : PPropList;
  prI : PPropInfo;

begin
  O:=TMyTestObject.Create;
  PI:=FindPropInfo(O,'BooleanField');
  Writeln('FindPropInfo(Instance,BooleanField) : ',PI^.Name);
  PI:=FindPropInfo(O.ClassType,'ByteField');
  Writeln('FindPropInfo(Class,ByteField)       : ',PI^.Name);
  Write  ('FindPropInfo(Class,NonExistingProp) : ');
  Try
    PI:=FindPropInfo(O,'NonExistingProp');
  except
    On E: Exception do
      Writeln('Caught exception "',E.ClassName,'" with message : ',E.Message);
  end;
  O.Free;
end.
