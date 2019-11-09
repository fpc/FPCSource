unit uttypinfo;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, punit, utrtl, typinfo;

implementation

Type
  TMyEnum = (one,two,three);
  TMyInt = Integer;

Var
  MyEnumInfo : PtypeInfo;

Function RegisterAliasesNotEnumerated : TtestString;

begin
  Result:='';
  ExpectException('Type information points to non-enumerated type',EArgumentException);
  AddEnumElementAliases(TypeInfo(TMyInt),['a','b','c'],0)
end;

Function RegisterAliasesNoElements : TTestString;

begin
  Result:='';
  ExpectException('Invalid number of enumerated values',EArgumentException);
  AddEnumElementAliases(MyEnumInfo,[],0)
end;

Function RegisterAliasesTooManyElements : TTestString;

begin
  Result:='';
  ExpectException('Invalid number of enumerated values',EArgumentException);
  AddEnumElementAliases(MyEnumInfo,['a','b','c','d'],0)
end;

Function RegisterAliasesTooManyElementsOffset : TTestString;

begin
  Result:='';
  ExpectException('Invalid number of enumerated values',EArgumentException);
  AddEnumElementAliases(MyEnumInfo,['a','b','c'],2)
end;

Function RegisterAliasesDuplicate : TTestString;


begin
  Result:='';
  ExpectException('Duplicate alias for enumerated value',EArgumentException);
  AddEnumElementAliases(MyEnumInfo,['a','b','a'],2)
end;

function TestGetEnumeratedAliasValue : TTestString;

begin
  Result:='';
  AddEnumElementAliases(MyEnumInfo,['a','b','c']);
  if not AssertEquals('Correct value',0,GetEnumeratedAliasValue(MyEnumInfo,'a')) then
    exit;
  if not AssertEquals('Correct value',1,GetEnumeratedAliasValue(MyEnumInfo,'b')) then
    exit;
  if not AssertEquals('Correct value',2,GetEnumeratedAliasValue(MyEnumInfo,'c')) then
    exit;
end;

function TestGetRemoveEnumeratedAliases : TTestString;

begin
  Result:='';
  RemoveEnumElementAliases(MyEnumInfo);
  AddEnumElementAliases(MyEnumInfo,['a','b','c']);
  if not AssertEquals('Correct value',0,GetEnumeratedAliasValue(MyEnumInfo,'a')) then
    exit;
  RemoveEnumElementAliases(MyEnumInfo);
  if not AssertEquals('Correct value',-1,GetEnumeratedAliasValue(MyEnumInfo,'a')) then
    exit;
end;

function TestGetRemoveEnumeratedAliasesOffset : TTestString;

begin
  Result:='';
  RemoveEnumElementAliases(MyEnumInfo);
  AddEnumElementAliases(MyEnumInfo,['b','c'],1);
  if not AssertEquals('Correct value',-1,GetEnumeratedAliasValue(MyEnumInfo,'a')) then
    exit;
  if not AssertEquals('Correct value',1,GetEnumeratedAliasValue(MyEnumInfo,'b')) then
    exit;
  if not AssertEquals('Correct value',2,GetEnumeratedAliasValue(MyEnumInfo,'c')) then
    exit;
end;

function TestGetEnumeratedValue : TTestString;

begin
  Result:='';
  RemoveEnumElementAliases(MyEnumInfo);
  AddEnumElementAliases(MyEnumInfo,['b','c'],1);
  if not AssertEquals('Correct value',-1,GetEnumValue(MyEnumInfo,'a')) then
    exit;
  if not AssertEquals('Correct value',0,GetEnumValue(MyEnumInfo,'one')) then
    exit;
  if not AssertEquals('Correct value',1,GetEnumValue(MyEnumInfo,'two')) then
    exit;
  if not AssertEquals('Correct value',1,GetEnumValue(MyEnumInfo,'b')) then
    exit;
  if not AssertEquals('Correct value',2,GetEnumValue(MyEnumInfo,'three')) then
    exit;
  if not AssertEquals('Correct value',2,GetEnumValue(MyEnumInfo,'c')) then
    exit;
end;


Procedure RegisterTests;

Var
  P : Psuite;
begin
  P:=EnsureSuite('TypInfo');
  AddTest('RegisterAliasesNotEnumerated',@RegisterAliasesNoElements,P);
  AddTest('RegisterAliasesNoElements',@RegisterAliasesNoElements,P);
  AddTest('RegisterAliasesTooManyElements',@RegisterAliasesTooManyElements,P);
  AddTest('RegisterAliasesTooManyElementsOffset',@RegisterAliasesTooManyElementsOffset,P);
  AddTest('RegisterAliasesDuplicate',@RegisterAliasesDuplicate,P);
  AddTest('TestGetEnumeratedAliasValue',@TestGetEnumeratedAliasValue,P);
  AddTest('TestGetRemoveEnumeratedAliases',@TestGetRemoveEnumeratedAliases,P);
  AddTest('TestGetRemoveEnumeratedAliasesOffset',@TestGetRemoveEnumeratedAliasesOffset,P);
  AddTest('TestGetEnumeratedValue',@TestGetEnumeratedValue,P);
end;

begin
  MyEnumInfo:=TypeInfo(TMyEnum);
  RegisterTests;
end.

