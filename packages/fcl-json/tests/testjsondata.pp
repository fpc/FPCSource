{
    This file is part of the Free Component Library

    JSON FPCUNit test for data structures
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit testjsondata; 

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, fpjson;

type


  { TTestJSONString }

  TTestJSONString = Class(TTestCase)
  Private
    Procedure TestTo(Const Src,Dest : String);
    Procedure TestFrom(Const Src,Dest : String);
  Published
    Procedure TestJSONStringToString;
    Procedure TestStringToJSONString;
  end;
  
  { TTestJSON }
  
  TTestJSON = Class(TTestCase)
  Protected
    Procedure TestItemCount(J : TJSONData;Expected : Integer);
    Procedure TestJSONType(J : TJSONData;Expected : TJSONType);
    Procedure TestJSON(J : TJSONData;Expected : String);
    Procedure TestIsNull(J : TJSONData;Expected : Boolean);
    Procedure TestAsBoolean(J : TJSONData;Expected : Boolean; ExpectError : boolean = False);
    Procedure TestAsInteger(J : TJSONData; Expected : Integer; ExpectError : boolean = False);
    Procedure TestAsString(J : TJSONData; Expected : String; ExpectError : boolean = False);
    Procedure TestAsFloat(J : TJSONData; Expected : TJSONFloat; ExpectError : boolean = False);
  end;
  
  { TTestNull }

  TTestNull = class(TTestJSON)
  published
    procedure TestNull;
  end;
  
  { TTestBoolean }

  TTestBoolean = class(TTestJSON)
  published
    procedure TestTrue;
    procedure TestFalse;
  end;
  
  { TTestInteger }

  TTestInteger = class(TTestJSON)
  Private
    Procedure DoTest(I : Integer);
  published
    procedure TestPositive;
    procedure TestNegative;
    procedure TestZero;
  end;
  
  { TTestFloat }

  TTestFloat = class(TTestJSON)
  Private
    Procedure DoTest(F : TJSONFloat);
  published
    procedure TestPositive;
    procedure TestNegative;
    procedure TestZero;
  end;

  { TTestString }

  TTestString = class(TTestJSON)
  private
    procedure DoTestFloat(F: TJSOnFloat; S: String; OK: Boolean);
  published
    procedure TestString;
    procedure TestInteger;
    procedure TestNegativeInteger;
    procedure TestFloat;
    procedure TestNegativeFloat;
    Procedure TestBooleanTrue;
    Procedure TestBooleanFalse;
  end;
  
  { TTestArray }

  TTestArray = class(TTestJSON)
  private
    procedure TestAddBoolean(B : Boolean);
  published
    Procedure TestCreate;
    Procedure TestCreateString;
    Procedure TestCreatePchar;
    procedure TestCreateStrings;
    procedure TestCreateInteger;
    procedure TestCreateInt64;
    procedure TestCreateFloat;
    procedure TestCreateBoolean;
    procedure TestCreateObject;
    procedure TestCreateJSONString;
    procedure TestCreateJSONObject;
    procedure TestCreateNilPointer;
    procedure TestCreatePointer;
    procedure TestAddInteger;
    procedure TestAddFloat;
    procedure TestAddBooleanTrue;
    procedure TestAddBooleanFalse;
    procedure TestAddString;
    procedure TestAddNull;
    procedure TestAddObject;
    procedure TestAddArray;
    procedure TestDelete;
    procedure TestRemove;
  end;
  
  { TTestObject }

  TTestObject = class(TTestJSON)
  private
    procedure TestAddBoolean(B : Boolean);
  published
    Procedure TestCreate;
    Procedure TestCreateString;
    Procedure TestCreatePchar;
    procedure TestCreateStrings;
    procedure TestCreateInteger;
    procedure TestCreateInt64;
    procedure TestCreateFloat;
    procedure TestCreateBoolean;
    procedure TestCreateObject;
    procedure TestCreateJSONString;
    procedure TestCreateJSONObject;
    procedure TestCreateNilPointer;
    procedure TestCreatePointer;
    procedure TestAddInteger;
    procedure TestAddFloat;
    procedure TestAddBooleanTrue;
    procedure TestAddBooleanFalse;
    procedure TestAddString;
    procedure TestAddNull;
    procedure TestAddObject;
    procedure TestAddArray;
    procedure TestDelete;
    procedure TestRemove;
  end;


implementation

{ TTestJSON }

procedure TTestJSON.TestItemCount(J: TJSONData; Expected: Integer);
begin
  AssertEquals(J.ClassName+'.ItemCount',Expected,J.Count);
end;

procedure TTestJSON.TestJSONType(J: TJSONData; Expected: TJSONType);
begin
  AssertEquals(J.ClassName+'.JSONType',Ord(Expected),Ord(J.JSONType));
end;

procedure TTestJSON.TestJSON(J: TJSONData; Expected: String);
begin
  AssertEquals(J.ClassName+'.AsJSON',Expected,J.AsJSON);
end;

procedure TTestJSON.TestIsNull(J: TJSONData; Expected: Boolean);
begin
  AssertEquals(J.ClassName+'.IsNull',Expected,J.IsNull);
end;

procedure TTestJSON.TestAsBoolean(J: TJSONData; Expected: Boolean; ExpectError: boolean = False);

Var
  B : Boolean;
  AssignOK : Boolean;
  Msg : String;
  
begin
  AssignOK:=False;
  Try
    B:=J.AsBoolean;
    AssignOK:=True;
    If Not ExpectError then
      AssertEquals(J.Classname+'.AsBoolean',Expected,B);
  except
    On E : Exception do
      begin
      AssignOK:=False;
      Msg:=E.Message;
      end;
  end;
  If ExpectError then
    begin
    If AssignOK then
      Fail(J.ClassName+'.AsBoolean must raise error');
    end
  else
    begin
    If not AssignOK then
      Fail(J.ClassName+'.AsBoolean raised unexpected exception: '+Msg)
    end;
end;

procedure TTestJSON.TestAsInteger(J: TJSONData; Expected: Integer;
  ExpectError: boolean);

Var
  I : Integer;
  AssignOK : Boolean;
  Msg : String;

begin
  AssignOK:=False;
  Try
    I:=J.AsInteger;
    AssignOK:=True;
    If Not ExpectError then
      AssertEquals(J.Classname+'.AsInteger',Expected,I);
  except
    On E : Exception do
      begin
      AssignOK:=False;
      Msg:=E.Message;
      end;
  end;
  If ExpectError then
    begin
    If AssignOK then
      Fail(J.ClassName+'.AsInteger must raise error');
    end
  else
    begin
    If not AssignOK then
      Fail(J.ClassName+'.AsInteger raised unexpected exception: '+Msg)
    end;
end;

procedure TTestJSON.TestAsString(J: TJSONData; Expected: String;
  ExpectError: boolean);
  
Var
  S : String;
  AssignOK : Boolean;
  Msg : String;

begin
  AssignOK:=False;
  Try
    S:=J.AsString;
    AssignOK:=True;
    If Not ExpectError then
      AssertEquals(J.Classname+'.AsString',Expected,S);
  except
    On E : Exception do
      begin
      AssignOK:=False;
      Msg:=E.Message;
      end;
  end;
  If ExpectError then
    begin
    If AssignOK then
      Fail(J.ClassName+'.AsString must raise error');
    end
  else
    begin
    If not AssignOK then
      Fail(J.ClassName+'.AsString raised unexpected exception: '+Msg)
    end;
end;

procedure TTestJSON.TestAsFloat(J: TJSONData; Expected: TJSONFloat;
  ExpectError: boolean);
  
Var
  F : TJSONFloat;
  AssignOK : Boolean;
  Msg : String;

begin
  AssignOK:=False;
  Try
    F:=J.AsFloat;
    AssignOK:=True;
    If Not ExpectError then
      AssertEquals(J.Classname+'.AsFloat',Expected,F);
  except
    On E : Exception do
      begin
      AssignOK:=False;
      Msg:=E.Message;
      end;
  end;
  If ExpectError then
    begin
    If AssignOK then
      Fail(J.ClassName+'.AsFloat must raise error');
    end
  else
    begin
    If not AssignOK then
      Fail(J.ClassName+'.AsFloat raised unexpected exception: '+Msg)
    end;
end;

{ TTestBoolean }

procedure TTestBoolean.TestTrue;

Var
  J : TJSONBoolean;

begin
  J:=TJSONBoolean.Create(True);
  try
    TestJSONType(J,jtBoolean);
    TestItemCount(J,0);
    TestJSON(J,'True');
    TestIsNull(J,False);
    TestAsBoolean(J,True);
    TestAsInteger(J,1);
    TestAsString(J,BoolToStr(True));
    TestAsFloat(J,1.0);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestBoolean.TestFalse;

Var
  J : TJSONBoolean;

begin
  J:=TJSONBoolean.Create(False);
  try
    TestJSONType(J,jtBoolean);
    TestItemCount(J,0);
    TestJSON(J,'False');
    TestIsNull(J,False);
    TestAsBoolean(J,False);
    TestAsInteger(J,0);
    TestAsString(J,BoolToStr(False));
    TestAsFloat(J,0.0);
  finally
    FreeAndNil(J);
  end;
end;



{ TTestNull }

procedure TTestNull.TestNull;

Var
  J : TJSONNull;

begin
  J:=TJSONNull.Create;
  try
    TestJSONType(J,jtNull);
    TestItemCount(J,0);
    TestJSON(J,'Null');
    TestIsNull(J,True);
    TestAsBoolean(J,False,True);
    TestAsInteger(J,0,true);
    TestAsString(J,BoolToStr(False),true);
    TestAsFloat(J,0.0,true);
  finally
    FreeAndNil(J);
  end;
end;


{ TTestString }

procedure TTestString.TestString;

Const
  S = 'A string';

Var
  J : TJSONString;

begin
  J:=TJSONString.Create(S);
  try
    TestJSONType(J,jtString);
    TestItemCount(J,0);
    TestJSON(J,'"'+S+'"');
    TestIsNull(J,False);
    TestAsBoolean(J,False,True);
    TestAsInteger(J,0,true);
    TestAsString(J,S);
    TestAsFloat(J,0.0,true);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestString.TestInteger;

Const
  S = '1';

Var
  J : TJSONString;

begin
  J:=TJSONString.Create(S);
  try
    TestJSONType(J,jtString);
    TestItemCount(J,0);
    TestJSON(J,'"'+S+'"');
    TestIsNull(J,False);
    TestAsBoolean(J,True,False);
    TestAsInteger(J,1,False);
    TestAsString(J,S);
    TestAsFloat(J,1.0,False);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestString.TestNegativeInteger;

Const
  S = '-1';

Var
  J : TJSONString;

begin
  J:=TJSONString.Create(S);
  try
    TestJSONType(J,jtString);
    TestItemCount(J,0);
    TestJSON(J,'"'+S+'"');
    TestIsNull(J,False);
    TestAsBoolean(J,True,False);
    TestAsInteger(J,-1,False);
    TestAsString(J,S);
    TestAsFloat(J,-1.0,False);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestString.TestFloat;

begin
  DoTestFloat(1.0,'1.0',True);
  DoTestFloat(1.0,'1',True);
  DoTestFloat(1.0,'1e0',True);
  DoTestFloat(1.2,'1.2',True);
  DoTestFloat(12.0,'1.2e1',True);
end;

procedure TTestString.TestNegativeFloat;
begin
  DoTestFloat(-1.0,'-1.0',True);
  DoTestFloat(-1.0,'-1',True);
  DoTestFloat(-1.0,'-1e0',True);
  DoTestFloat(-1.2,'-1.2',True);
  DoTestFloat(-12.0,'-1.2e1',True);
end;

procedure TTestString.TestBooleanTrue;

Const
  S = 'True';

Var
  J : TJSONString;

begin
  J:=TJSONString.Create(S);
  try
    TestJSONType(J,jtString);
    TestItemCount(J,0);
    TestJSON(J,'"'+S+'"');
    TestIsNull(J,False);
    TestAsBoolean(J,True,False);
    TestAsInteger(J,-1,True);
    TestAsString(J,S);
    TestAsFloat(J,-1.0,True);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestString.TestBooleanFalse;

Const
  S = 'False';

Var
  J : TJSONString;

begin
  J:=TJSONString.Create(S);
  try
    TestJSONType(J,jtString);
    TestItemCount(J,0);
    TestJSON(J,'"'+S+'"');
    TestIsNull(J,False);
    TestAsBoolean(J,False,False);
    TestAsInteger(J,0,True);
    TestAsString(J,S);
    TestAsFloat(J,0,True);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestString.DoTestFloat(F : TJSOnFloat;S : String; OK : Boolean);

Var
  J : TJSONString;

begin
  J:=TJSONString.Create(S);
  try
    TestJSONType(J,jtString);
    TestItemCount(J,0);
    TestJSON(J,'"'+S+'"');
    TestIsNull(J,False);
    TestAsBoolean(J,(F<>0),Not OK);
    TestAsInteger(J,Round(F),(Pos('.',S)<>0) or (Pos('E',UpperCase(S))<>0));
    TestAsString(J,S);
    TestAsFloat(J,F,Not OK);
  finally
    FreeAndNil(J);
  end;
end;


{ TTestInteger }

procedure TTestInteger.DoTest(I: Integer);

Var
  J : TJSONIntegerNumber;

begin
  J:=TJSONIntegerNumber.Create(I);
  try
    TestJSONType(J,jtNumber);
    TestItemCount(J,0);
    AssertEquals('Numbertype is ntInteger',ord(ntInteger),Ord(J.NumberType));
    TestJSON(J,IntToStr(i));
    TestIsNull(J,False);
    TestAsBoolean(J,(I<>0));
    TestAsInteger(J,I);
    TestAsString(J,IntToStr(I));
    TestAsFloat(J,I);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestInteger.TestPositive;

begin
  DoTest(1);
end;

procedure TTestInteger.TestNegative;
begin
  DoTest(-1);
end;

procedure TTestInteger.TestZero;
begin
  DoTest(0);
end;

{ TTestFloat }

procedure TTestFloat.DoTest(F: TJSONFloat);

Var
  J : TJSONFloatNumber;
  S : String;
  
begin
  Str(F,S);
  J:=TJSONFloatNumber.Create(F);
  try
    TestJSONType(J,jtNumber);
    TestItemCount(J,0);
    AssertEquals('Numbertype is ntFloat',ord(ntFloat),Ord(J.NumberType));
    TestJSON(J,S);
    TestIsNull(J,False);
    TestAsBoolean(J,(F<>0));
    TestAsInteger(J,Round(F));
    TestAsString(J,S);
    TestAsFloat(J,F);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestFloat.TestPositive;
begin
  DoTest(1.0);
  DoTest(1.2);
  DoTest(1.2e1);
  DoTest(1.2e-1);
  DoTest(1.2e10);
  DoTest(1.2e-10);
end;

procedure TTestFloat.TestNegative;
begin
  DoTest(-1.0);
  DoTest(-1.2);
  DoTest(-1.2e1);
  DoTest(-1.2e-1);
  DoTest(-1.2e10);
  DoTest(-1.2e-10);
end;

procedure TTestFloat.TestZero;
begin
  DoTest(0.0);
end;

{ TTestArray }

procedure TTestArray.TestCreate;

Var
  J : TJSONArray;

begin
  J:=TJSonArray.Create;
  try
    TestJSONType(J,jtArray);
    TestItemCount(J,0);
    TestJSON(J,'[]');
    TestIsNull(J,False);
    TestAsBoolean(J,False,True);
    TestAsInteger(J,1,True);
    TestAsString(J,'',True);
    TestAsFloat(J,0.0,True);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestCreateString;

Const
  S = 'A string';

Var
  J : TJSONArray;

begin
  J:=TJSonArray.Create([S]);
  try
    TestJSONType(J,jtArray);
    TestItemCount(J,1);
    TestJSONType(J[0],jtString);
    TestJSON(J,'["'+S+'"]');
    TestIsNull(J,False);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestCreatePChar;

Const
  S = 'A string';

Var
  J : TJSONArray;

begin
  J:=TJSonArray.Create([Pchar(S)]);
  try
    TestJSONType(J,jtArray);
    TestItemCount(J,1);
    TestJSONType(J[0],jtString);
    TestJSON(J,'["'+S+'"]');
    TestIsNull(J,False);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestCreateStrings;

Const
  S = 'A string';
  T = 'B string';
  
Var
  J : TJSONArray;

begin
  J:=TJSONArray.Create([S,T]);
  try
    TestJSONType(J,jtArray);
    TestItemCount(J,2);
    TestJSONType(J[0],jtString);
    TestJSONType(J[1],jtString);
    TestJSON(J,'["'+S+'", "'+T+'"]');
    TestIsNull(J,False);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestCreateInteger;

Const
  S = 3;

Var
  J : TJSONArray;

begin
  J:=TJSonArray.Create([S]);
  try
    TestJSONType(J,jtArray);
    TestItemCount(J,1);
    TestJSONType(J[0],jtNumber);
    TestJSON(J,'[3]');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestCreateFloat;

Const
  S = 1.2;

Var
  J : TJSONArray;
  r : String;
  
begin
  J:=TJSonArray.Create([S]);
  try
    TestJSONType(J,jtArray);
    TestItemCount(J,1);
    TestJSONType(J[0],jtNumber);
    Str(S,R);
    TestJSON(J,'['+R+']');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestCreateInt64;

Const
  S : Int64 = $FFFFFF;

Var
  J : TJSONArray;
  r : String;
  F : TJSONFloat;

begin
  J:=TJSonArray.Create([S]);
  try
    TestJSONType(J,jtArray);
    TestItemCount(J,1);
    TestJSONType(J[0],jtNumber);
    F:=S;
    Str(F,R);
    TestJSON(J,'['+R+']');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestCreateBoolean;

Const
  S = True;

Var
  J : TJSONArray;

begin
  J:=TJSonArray.Create([S]);
  try
    TestJSONType(J,jtArray);
    TestItemCount(J,1);
    TestJSONType(J[0],jtBoolean);
    TestJSON(J,'[True]');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestCreateJSONObject;

Var
  J : TJSONArray;

begin
  J:=TJSonArray.Create([TJSONObject.Create]);
  try
    TestItemCount(J,1);
    TestJSONType(J[0],jtObject);
    TestJSON(J,'[{}]');
  finally
    FreeAndNil(J);
  end;
end;
procedure TTestArray.TestCreateJSONString;

Const
  S = 'A string';

Var
  J : TJSONArray;

begin
  J:=TJSonArray.Create([TJSONString.Create(S)]);
  try
    TestItemCount(J,1);
    TestJSONType(J[0],jtString);
    TestJSON(J,'["'+S+'"]');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestCreateObject;

Var
  J : TJSONArray;
  
begin
  J:=Nil;
  try
    Try
      J:=TJSONArray.Create([TObject.Create]);
      Fail('Array constructor accepts only TJSONData');
    finally
      FreeAndNil(J);
    end;
  except
    // Should be OK.
  end;
end;

procedure TTestArray.TestCreateNilPointer;

Var
  J : TJSONArray;
  P : Pointer;
  
begin
  J:=Nil;
  P:=Nil;
  Try
    J:=TJSONArray.Create([P]);
    TestJSONType(J[0],jtNull);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestCreatePointer;

Var
  J : TJSONArray;
  P : Pointer;
  
begin
  J:=Nil;
  P:=@Self;
  try
    Try
      J:=TJSONArray.Create([P]);
      Fail('Array constructor accepts only NIL pointers');
    finally
      FreeAndNil(J);
    end;
  except
    // Should be OK.
  end;
end;

procedure TTestArray.TestAddInteger;

Var
  J : TJSONArray;

begin
  J:=TJSonArray.Create;
  try
    J.Add(0);
    TestItemCount(J,1);
    TestJSONType(J[0],jtNumber);
    AssertEquals('J[0] is TJSONIntegerNumber',J[0].ClassType,TJSONIntegerNumber);
    AssertEquals('j.Types[0]=jtNumber',ord(J.Types[0]),Ord(jtNumber));
    AssertEquals('J.Integers[0]=0',0,J.integers[0]);
    TestAsInteger(J[0],0);
    TestJSON(J,'[0]');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestAddFloat;

Var
  J : TJSONArray;
  S : String;
  F : TJSONFloat;
begin
  F:=1.2;
  J:=TJSonArray.Create;
  try
    J.Add(F);
    TestItemCount(J,1);
    TestJSONType(J[0],jtNumber);
    AssertEquals('J[0] is TJSONFloatNumber',TJSONfloatNumber,J[0].ClassType);
    AssertEquals('j.Types[0]=jtNumber',Ord(jtNumber),ord(J.Types[0]));
    AssertEquals('J.Floats[0]='+FloatToStr(F),F,J.Floats[0]);
    TestAsFloat(J[0],F);
    Str(F,S);
    TestJSON(J,'['+S+']');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestAddBoolean(B : Boolean);

Var
  J : TJSONArray;

begin
  B:=True;
  J:=TJSonArray.Create;
  try
    J.Add(B);
    TestItemCount(J,1);
    TestJSONType(J[0],jtBoolean);
    AssertEquals('J[0] is TJSONBoolean',TJSONBoolean,J[0].ClassType);
    TestAsBoolean(J[0],B);
    AssertEquals('J.Booleans[0]='+BoolToStr(B)+'"',B,J.Booleans[0]);
    If B then
      TestJSON(J,'[True]')
    else
      TestJSON(J,'[False]');
  finally
    FreeAndNil(J);
  end;

end;

procedure TTestArray.TestAddBooleanTrue;

begin
  TestAddBoolean(True);
end;

procedure TTestArray.TestAddBooleanFalse;

begin
  TestAddBoolean(False);
end;

procedure TTestArray.TestAddString;

Var
  J : TJSONArray;
  S : String;
  F : TJSONFloat;
  
begin
  S:='A string';
  J:=TJSonArray.Create;
  try
    J.Add(S);
    TestItemCount(J,1);
    TestJSONType(J[0],jtString);
    AssertEquals('J[0] is TJSONString',TJSONString,J[0].ClassType);
    TestAsString(J[0],S);
    AssertEquals('J.Strings[0]="'+S+'"',S,J.Strings[0]);
    TestJSON(J,'["'+StringToJSONString(S)+'"]');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestAddNull;

Var
  J : TJSONArray;
  S : String;
  F : TJSONFloat;

begin
  J:=TJSonArray.Create;
  try
    J.Add;
    TestItemCount(J,1);
    TestJSONType(J[0],jtNull);
    AssertEquals('J[0] is TJSONNull',TJSONNull,J[0].ClassType);
    AssertEquals('J.Nulls[0]=True',True,J.Nulls[0]);
    TestIsNull(J[0],true);
    TestJSON(J,'[Null]');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestAddArray;

Var
  J,J2 : TJSONArray;

begin
  J:=TJSonArray.Create;
  try
    J2:=TJSonArray.Create;
    J2.Add(0);
    J2.Add(1);
    J.Add(J2);
    TestItemCount(J,1);
    TestJSONType(J[0],jtArray);
    AssertEquals('J[0] is TJSONArray',TJSONArray,J[0].ClassType);
    AssertEquals('J.Arrays[0] is TJSONArray',TJSONArray,J.Arrays[0].ClassType);
    TestAsInteger(J.Arrays[0][0],0);
    TestAsInteger(J.Arrays[0][1],1);
    TestJSON(J,'[[0, 1]]');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestAddObject;

Const
  A = 'a';
  B = 'b';
  
Var
  J : TJSONArray;
  J2 : TJSONObject;

begin
  J:=TJSonArray.Create;
  try
    J2:=TJSonObject.Create;
    J2.Add(A,0);
    J2.Add(B,1);
    J.Add(J2);
    TestItemCount(J,1);
    TestJSONType(J[0],jtObject);
    AssertEquals('J[0] is TJSONObject',TJSONObject,J[0].ClassType);
    AssertEquals('J.Objects[0] is TJSONObject',TJSONObject,J.Objects[0].ClassType);
    TestAsInteger(J.Objects[0][A],0);
    TestAsInteger(J.Objects[0][B],1);
    TestJSON(J,'[{ "a" : 0, "b" : 1 }]');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestDelete;

Var
  J : TJSONArray;

begin
  J:=TJSonArray.Create;
  try
    J.Add(0);
    J.Add(1);
    TestItemCount(J,2);
    TestJSONType(J[0],jtNumber);
    TestJSONType(J[1],jtNumber);
    TestJSON(J,'[0, 1]');
    J.Delete(1);
    TestItemCount(J,1);
    J.Delete(0);
    TestItemCount(J,0);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestArray.TestRemove;

Var
  J : TJSONArray;
  I : TJSONData;

begin
  J:=TJSonArray.Create;
  try
    J.Add(0);
    J.Add(1);
    J.Add(2);
    TestItemCount(J,3);
    TestJSONType(J[0],jtNumber);
    TestJSONType(J[1],jtNumber);
    TestJSONType(J[2],jtNumber);
    TestJSON(J,'[0, 1, 2]');
    I:=J[1];
    J.Remove(I);
    TestItemCount(J,2);
    TestAsInteger(J[0],0);
    TestAsInteger(J[1],2);
  finally
    FreeAndNil(J);
  end;
end;

{ TTestObject }

procedure TTestObject.TestCreate;

Var
  J : TJSONObject;

begin
  J:=TJSONObject.Create;
  try
    TestJSONType(J,jtObject);
    TestItemCount(J,0);
    TestJSON(J,'{}');
    TestIsNull(J,False);
    TestAsBoolean(J,False,True);
    TestAsInteger(J,1,True);
    TestAsString(J,'',True);
    TestAsFloat(J,0.0,True);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestAddInteger;

Const
  A = 'a';

Var
  J : TJSONObject;

begin
  J:=TJSonObject.Create;
  try
    J.Add(A,0);
    TestItemCount(J,1);
    TestJSONType(J[A],jtNumber);
    AssertEquals('J[''a''] is TJSONIntegerNumber',J[A].ClassType,TJSONIntegerNumber);
    AssertEquals('j.Types[''a'']=jtNumber',ord(J.Types[A]),Ord(jtNumber));
    AssertEquals('J.Integers[''a'']=0',0,J.integers[A]);
    TestAsInteger(J[A],0);
    TestJSON(J,'{ "'+A+'" : 0 }');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestAddFloat;

Const
  A = 'a';

Var
  J : TJSONObject;
  S : String;
  F : TJSONFloat;
begin
  F:=1.2;
  J:=TJSonObject.Create;
  try
    J.Add(A,F);
    TestItemCount(J,1);
    TestJSONType(J[A],jtNumber);
    AssertEquals('J[''a''] is TJSONFloatNumber',TJSONfloatNumber,J[a].ClassType);
    AssertEquals('j.Types[''a'']=jtNumber',Ord(jtNumber),ord(J.Types[a]));
    AssertEquals('J.Floats[''a'']='+FloatToStr(F),F,J.Floats[a]);
    TestAsFloat(J[A],F);
    Str(F,S);
    TestJSON(J,'{ "'+a+'" : '+S+' }');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestAddBoolean(B : Boolean);

Const
  A = 'a';
  
Var
  J : TJSONObject;

begin
  B:=True;
  J:=TJSonObject.Create;
  try
    J.Add(A,B);
    TestItemCount(J,1);
    TestJSONType(J[A],jtBoolean);
    AssertEquals('J[''a''] is TJSONBoolean',TJSONBoolean,J[a].ClassType);
    TestAsBoolean(J[a],B);
    AssertEquals('J.Booleans[''a'']='+BoolToStr(B)+'"',B,J.Booleans[a]);
    If B then
      TestJSON(J,'{ "'+a+'" : True }')
    else
      TestJSON(J,'{ "'+a+'" : False }');
  finally
    FreeAndNil(J);
  end;

end;

procedure TTestObject.TestAddBooleanTrue;

begin
  TestAddBoolean(True);
end;

procedure TTestObject.TestAddBooleanFalse;

begin
  TestAddBoolean(False);
end;

procedure TTestObject.TestAddString;

Const
  A = 'a';

Var
  J : TJSONObject;
  S : String;
  F : TJSONFloat;

begin
  S:='A string';
  J:=TJSonObject.Create;
  try
    J.Add(A,S);
    TestItemCount(J,1);
    TestJSONType(J[a],jtString);
    AssertEquals('J[''a''] is TJSONString',TJSONString,J[A].ClassType);
    TestAsString(J[a],S);
    AssertEquals('J.Strings[''a'']="'+S+'"',S,J.Strings[A]);
    TestJSON(J,'{ "'+a+'" : "'+StringToJSONString(S)+'" }');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestAddNull;

Const
  A = 'a';

Var
  J : TJSONObject;
  S : String;
  F : TJSONFloat;

begin
  J:=TJSonObject.Create;
  try
    J.Add(a);
    TestItemCount(J,1);
    TestJSONType(J[a],jtNull);
    AssertEquals('J[''a''] is TJSONNull',TJSONNull,J[A].ClassType);
    AssertEquals('J.Nulls[''a'']=True',True,J.Nulls[A]);
    TestIsNull(J[a],true);
    TestJSON(J,'{ "'+a+'" : Null }');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestAddObject;

Const
  A = 'a';
  B = 'b';
  C = 'c';

Var
  J,J2 : TJSONObject;

begin
  J:=TJSonObject.Create;
  try
    J2:=TJSonObject.Create;
    J2.Add(B,0);
    J2.Add(C,1);
    J.Add(A,J2);
    TestItemCount(J,1);
    TestJSONType(J[A],jtObject);
    AssertEquals('J[''a''] is TJSONObject',TJSONObject,J[A].ClassType);
    AssertEquals('J.Objects[''a''] is TJSONObject',TJSONObject,J.Objects[A].ClassType);
    TestAsInteger(J.Objects[A][B],0);
    TestAsInteger(J.Objects[A][C],1);
    TestJSON(J,'{ "a" : { "b" : 0, "c" : 1 } }');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestAddArray;

Const
  A = 'a';

Var
  J : TJSONObject;
  J2 : TJSONArray;

begin
  J:=TJSONObject.Create;
  try
    J2:=TJSonArray.Create;
    J2.Add(0);
    J2.Add(1);
    J.Add(A,J2);
    TestItemCount(J,1);
    TestJSONType(J[A],jtArray);
    AssertEquals('J[''a''] is TJSONArray',TJSONArray,J[A].ClassType);
    AssertEquals('J.Arrays[0] is TJSONArray',TJSONArray,J.Arrays[A].ClassType);
    TestAsInteger(J.Arrays[A][0],0);
    TestAsInteger(J.Arrays[A][1],1);
    TestJSON(J,'{ "a" : [0, 1] }');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestDelete;

Const
  A = 'a';
  B = 'b';
  
Var
  J : TJSONObject;

begin
  J:=TJSonObject.Create;
  try
    J.Add(A,0);
    J.Add(B,1);
    TestItemCount(J,2);
    TestJSONType(J[A],jtNumber);
    TestJSONType(J[A],jtNumber);
    TestJSON(J,'{ "a" : 0, "b" : 1 }');
    J.Delete(1);
    TestItemCount(J,1);
    J.Delete(0);
    TestItemCount(J,0);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestRemove;

Const
  A = 'a';
  B = 'b';
  C = 'c';
  
Var
  J : TJSONObject;
  I : TJSONData;

begin
  J:=TJSonObject.Create;
  try
    J.Add(A,1);
    J.Add(B,2);
    J.Add(C,3);
    TestItemCount(J,3);
    TestJSONType(J[A],jtNumber);
    TestJSONType(J[B],jtNumber);
    TestJSONType(J[C],jtNumber);
    TestJSON(J,'{ "a" : 1, "b" : 2, "c" : 3 }');
    I:=J[b];
    J.Remove(I);
    TestItemCount(J,2);
    TestAsInteger(J[a],1);
    TestAsInteger(J[c],3);
  finally
    FreeAndNil(J);
  end;
end;


procedure TTestObject.TestCreateString;

Const
  A = 'A';
  S = 'A string';

Var
  J : TJSONObject;

begin
  J:=TJSONObject.Create([A,S]);
  try
    TestJSONType(J,jtObject);
    TestItemCount(J,1);
    TestJSONType(J[A],jtString);
    TestJSON(J,'{ "A" : "'+S+'" }');
    TestIsNull(J,False);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestCreatePChar;

Const
  A = 'A';
  S = 'A string';

Var
  J : TJSONObject;

begin
  J:=TJSONObject.Create([A,Pchar(S)]);
  try
    TestJSONType(J,jtObject);
    TestItemCount(J,1);
    TestJSONType(J[A],jtString);
    TestJSON(J,'{ "A" : "'+S+'" }');
    TestIsNull(J,False);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestCreateStrings;

Const
  A = 'A';
  B = 'B';
  S = 'A string';
  T = 'B string';

Var
  J : TJSONObject;

begin
  J:=TJSONObject.Create([A,S,B,T]);
  try
    TestJSONType(J,jtObject);
    TestItemCount(J,2);
    TestJSONType(J[A],jtString);
    TestJSONType(J[B],jtString);
    TestJSON(J,'{ "A" : "'+S+'", "B" : "'+T+'" }');
    TestIsNull(J,False);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestCreateInteger;

Const
  A = 'A';
  S = 3;

Var
  J : TJSONObject;

begin
  J:=TJSONObject.Create([A,S]);
  try
    TestJSONType(J,jtObject);
    TestItemCount(J,1);
    TestJSONType(J[A],jtNumber);
    TestJSON(J,'{ "A" : 3 }');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestCreateFloat;

Const
  A = 'A';
  S = 1.2;

Var
  J : TJSONObject;
  r : String;

begin
  J:=TJSONObject.Create([A,S]);
  try
    TestJSONType(J,jtObject);
    TestItemCount(J,1);
    TestJSONType(J[A],jtNumber);
    Str(S,R);
    TestJSON(J,'{ "A" : '+R+' }');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestCreateInt64;

Const
  A = 'A';
  S : Int64 = $FFFFFF;

Var
  J : TJSONObject;
  r : String;
  F : TJSONFloat;

begin
  J:=TJSONObject.Create([A,S]);
  try
    TestJSONType(J,jtObject);
    TestItemCount(J,1);
    TestJSONType(J[A],jtNumber);
    F:=S;
    Str(F,R);
    TestJSON(J,'{ "A" : '+R+' }');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestCreateBoolean;

Const
  A = 'A';
  S = True;

Var
  J : TJSONObject;

begin
  J:=TJSONObject.Create([A,S]);
  try
    TestJSONType(J,jtObject);
    TestItemCount(J,1);
    TestJSONType(J[A],jtBoolean);
    TestJSON(J,'{ "A" : True }');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestCreateJSONObject;

Const
  A = 'A';
  
Var
  J : TJSONObject;

begin
  J:=TJSONObject.Create([A,TJSONObject.Create]);
  try
    TestItemCount(J,1);
    TestJSONType(J[A],jtObject);
    TestJSON(J,'{ "A" : {} }');
  finally
    FreeAndNil(J);
  end;
end;
procedure TTestObject.TestCreateJSONString;

Const
  A = 'A';
  S = 'A string';

Var
  J : TJSONObject;

begin
  J:=TJSONObject.Create([A,TJSONString.Create(S)]);
  try
    TestItemCount(J,1);
    TestJSONType(J[A],jtString);
    TestJSON(J,'{ "A" : "'+S+'" }');
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestCreateObject;

Const
  A = 'A';

Var
  J : TJSONObject;

begin
  J:=Nil;
  try
    Try
      J:=TJSONObject.Create([A,TObject.Create]);
      Fail('Array constructor accepts only TJSONData');
    finally
      FreeAndNil(J);
    end;
  except
    // Should be OK.
  end;
end;

procedure TTestObject.TestCreateNilPointer;

Const
  A = 'A';

Var
  J : TJSONObject;
  P : Pointer;

begin
  J:=Nil;
  P:=Nil;
  Try
    J:=TJSONObject.Create([A,P]);
    TestJSONType(J[A],jtNull);
  finally
    FreeAndNil(J);
  end;
end;

procedure TTestObject.TestCreatePointer;

Const
  A = 'A';

Var
  J : TJSONObject;
  P : Pointer;

begin
  J:=Nil;
  P:=@Self;
  try
    Try
      J:=TJSONObject.Create([A,P]);
      Fail('Array constructor accepts only NIL pointers');
    finally
      FreeAndNil(J);
    end;
  except
    // Should be OK.
  end;
end;

{ TTestJSONString }

procedure TTestJSONString.TestTo(const Src, Dest: String);

Var
  S : String;

begin
  S:='StringToJSONString('''+Src+''')='''+Dest+'''';
  AssertEquals(S,Dest,StringToJSONString(Src));
end;

procedure TTestJSONString.TestFrom(const Src, Dest: String);

Var
  S : String;

begin
  S:='JSONStringToString('''+Src+''')='''+Dest+'''';
  AssertEquals(S,Dest,JSONStringToString(Src));
end;

procedure TTestJSONString.TestJSONStringToString;
begin
  TestFrom('','');
  TestFrom('A','A');
  TestFrom('AB','AB');
  TestFrom('ABC','ABC');
  TestFrom('\\','\');
  TestFrom('\/','/');
  TestFrom('\"','"');
  TestFrom('\b',#8);
  TestFrom('\t',#9);
  TestFrom('\n',#10);
  TestFrom('\f',#12);
  TestFrom('\r',#13);
  TestFrom('\bBC',#8'BC');
  TestFrom('\tBC',#9'BC');
  TestFrom('\nBC',#10'BC');
  TestFrom('\fBC',#12'BC');
  TestFrom('\rBC',#13'BC');
  TestFrom('A\b','A'#8);
  TestFrom('A\t','A'#9);
  TestFrom('A\n','A'#10);
  TestFrom('A\f','A'#12);
  TestFrom('A\r','A'#13);
  TestFrom('A\bBC','A'#8'BC');
  TestFrom('A\tBC','A'#9'BC');
  TestFrom('A\nBC','A'#10'BC');
  TestFrom('A\fBC','A'#12'BC');
  TestFrom('A\rBC','A'#13'BC');
  TestFrom('\\\\','\\');
  TestFrom('\/\/','//');
  TestFrom('\"\"','""');
  TestFrom('\b\b',#8#8);
  TestFrom('\t\t',#9#9);
  TestFrom('\n\n',#10#10);
  TestFrom('\f\f',#12#12);
  TestFrom('\r\r',#13#13);
end;

procedure TTestJSONString.TestStringToJSONString;
begin
  TestTo('','');
  TestTo('A','A');
  TestTo('AB','AB');
  TestTo('ABC','ABC');
  TestTo('\','\\');
  TestTo('/','\/');
  TestTo('"','\"');
  TestTo(#8,'\b');
  TestTo(#9,'\t');
  TestTo(#10,'\n');
  TestTo(#12,'\f');
  TestTo(#13,'\r');
  TestTo(#8'BC','\bBC');
  TestTo(#9'BC','\tBC');
  TestTo(#10'BC','\nBC');
  TestTo(#12'BC','\fBC');
  TestTo(#13'BC','\rBC');
  TestTo('A'#8,'A\b');
  TestTo('A'#9,'A\t');
  TestTo('A'#10,'A\n');
  TestTo('A'#12,'A\f');
  TestTo('A'#13,'A\r');
  TestTo('A'#8'BC','A\bBC');
  TestTo('A'#9'BC','A\tBC');
  TestTo('A'#10'BC','A\nBC');
  TestTo('A'#12'BC','A\fBC');
  TestTo('A'#13'BC','A\rBC');
  TestTo('\\','\\\\');
  TestTo('//','\/\/');
  TestTo('""','\"\"');
  TestTo(#8#8,'\b\b');
  TestTo(#9#9,'\t\t');
  TestTo(#10#10,'\n\n');
  TestTo(#12#12,'\f\f');
  TestTo(#13#13,'\r\r');
end;

initialization
  RegisterTest(TTestJSONString);
  RegisterTest(TTestNull);
  RegisterTest(TTestBoolean);
  RegisterTest(TTestInteger);
  RegisterTest(TTestFloat);
  RegisterTest(TTestString);
  RegisterTest(TTestArray);
  RegisterTest(TTestObject);
end.

