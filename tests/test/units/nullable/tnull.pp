program testnullable;

{$mode objfpc}
{$h+}

uses sysutils, nullable;

Const
  Val1  = 'Value 1';
  Val2  = 'Value 2';

Function Testinit : String;

Var
  A : specialize TNullable<String>;

begin
  Result:='';
  If a.HasValue then
    Exit('May not have a value at start');
  If Not a.IsNull then
    Exit('May not have a value at start (null)');
end;

Function TestSetValue : String;

Var
  A : specialize TNullable<String>;

begin
  Result:='';
  a.Value:=Val1;
  If Not a.HasValue then
    Exit('Setting value does not result in hasvalue');
  If a.Value<>Val1 then
    Exit('Setting value does not result in correct value stored');
end;

Function TestIsnull : String;

Var
  A : specialize TNullable<String>;

begin
  Result:='';
  If Not a.IsNull then
    Exit('Not null on init');
  a.Value:=Val1;
  If a.IsNull then
    Exit('Setting value does not result in Not isNull');
end;

Function TestClear : String;

Var
  A : specialize TNullable<String>;

begin
  Result:='';
  a.Value:=Val1;
  If Not a.HasValue then
    Exit('Setting value does not result in hasvalue');
  A.Clear;
  If a.HasValue then
    Exit('Clear does not result in no value');
end;

Function TestGetEmptyValue : String;

Var
  A : specialize TNullable<String>;
  B : String;

begin
  Result:='';
  try
    B:=a.Value;
    Exit('Getting empty value does not result in exception : '+B);
  except
    on E : Exception do
      if not (E is EConvertError) then
        Exit('Getting empty value does not result in correct exception class');
  end;
end;

Function TestGetEmptyValueOrDefault : String;

Var
  A : specialize TNullable<String>;
  B : String;

begin
  Result:='';
  try
    B:=a.ValueOrDefault;
    if B<>'' then
      Exit('Getting empty value does not get empty value');
    a.Value:=Val2;
    B:=a.ValueOrDefault;
    if B<>Val2 then
      Exit('Getting set value does not get empty value');
  except
    on E : Exception do
     Exit('Getting empty value or default results in exception !');
  end;
end;


Function TestSetHasValue : String;

Var
  A : specialize TNullable<String>;

begin
  Result:='';
  a.HasValue:=true;
  if Not A.HasValue then
    Exit('Setting hasvalue to true does not result in correct hasvalue value');
  if Not (A.Value='') then
    Exit('Setting hasvalue does not result in correct empty value');
  A.HasValue:=False;
  if A.HasValue then
    Exit('Setting hasvalue to false does not result in correct hasvalue value');
end;


Function TestTypecast1 : string;

Var
  A : specialize TNullable<String>;
  B : String;
begin
  Result:='';
  a.Value:=Val1;
  B:=String(A);
  If not (B=Val1) then
    Exit('Typecast not correct');
  A.clear;
  try
    B:=String(A);
    Exit('No exception raised');
  Except
    on E : Exception do
      if not (E is EConvertError) then
        Exit('Getting empty value does not result in correct exception class');
  end;
end;

Function TestTypecast2 : string;

Var
  A : specialize TNullable<String>;
  B : String;
begin
  Result:='';
  B:=Val1;
  A:=specialize TNullable<String>(B);
  If Not (A.HasValue and (A.Value=Val1)) then
    Exit('Typecast not correct');
end;

Function TestAssign : string;

Var
  A : specialize TNullable<String>;
  B : String;
begin
  Result:='';
  B:=Val1;
  A:=B;
  If Not (A.HasValue and (A.Value=Val1)) then
    Exit('Assign not correct');
end;

Function TestAssign2 : string;

Var
  A : specialize TNullable<String>;
  B : String;
begin
  Result:='';
  A.Value:=Val1;
  B:=A;
  If Not (B=Val1) then
    Exit('Assign not correct');
  A.Clear;
  try
    B:=A;
    Exit('No exception raised');
  Except
    on E : Exception do
      if not (E is EConvertError) then
        Exit('Getting empty value does not result in correct exception class');
  end;
end;


Procedure DoTest(aTest,aResult : String);

begin
  if aResult<>'' then
    begin
    writeln(aTest,' failed : ',aResult);
    Halt(1);
    end
  else
    Writeln(aTest,' OK.');
end;


begin
  DoTest('TestInit',TestInit);
  DoTest('TestSetValue',TestSetValue);
  DoTest('TestClear',TestClear);
  DoTest('TestSetHasValue',TestSetHasValue);
  DoTest('TestIsNull',TestIsNull);
  DoTest('TestTypeCast1',TestTypecast1);
  DoTest('TestTypeCast2',TestTypecast2);
  DoTest('TestAssign',TestAssign);
  DoTest('TestAssign2',TestAssign2);
  DoTest('TestGetEmptyValue',TestGetEmptyValue);
  DoTest('TestGetEmptyValueOrDefault',TestGetEmptyValueOrDefault);
end.

