{
    This file is part of the Free Component Library

    JSON FPCUNit test for parser
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit testjsonparser;

interface

uses
  Classes, SysUtils, fpcunit, testregistry,fpjson,
  jsonscanner,jsonParser,testjsondata;

Const
  DefaultOpts = [joUTF8,joStrict];

type

  { TTestParser }

  TTestParser = class(TTestJSON)
  private
    FOptions : TJSONOptions;
    procedure CallNoHandlerStream;
    procedure DoTestError(S: String; Options : TJSONOptions = DefaultOpts);
    procedure DoTestFloat(F: TJSONFloat); overload;
    procedure DoTestFloat(F: TJSONFloat; S: String); overload;
    procedure DoTestObject(S: String; const ElNames: array of String; DoJSONTest : Boolean = True);
    procedure DoTestString(S : String);
    procedure DoTestArray(S: String; ACount: Integer; IgnoreJSON: Boolean=False);
    Procedure DoTestClass(S : String; AClass : TJSONDataClass);
    procedure CallNoHandler;
    procedure DoTrailingCommaErrorArray;
    procedure DoTrailingCommaErrorObject;
  Protected
    Procedure Setup; override;
  published
    procedure TestEmpty;
    procedure TestNull;
    procedure TestTrue;
    procedure TestFalse;
    procedure TestFloat;
    procedure TestInteger;
    procedure TestInt64;
    procedure TestString;
    procedure TestArray;
    procedure TestObject;
    procedure TestObjectError;
    procedure TestTrailingComma;
    procedure TestTrailingCommaErrorArray;
    procedure TestTrailingCommaErrorObject;
    procedure TestMixed;
    Procedure TestComment;
    procedure TestErrors;
    Procedure TestClasses;
    Procedure TestHandler;
    Procedure TestNoHandlerError;
    Procedure TestHandlerResult;
    Procedure TestHandlerResultStream;
  end;

implementation

procedure TTestParser.TestEmpty;

Var
  P : TJSONParser;
  J : TJSONData;
  
begin
  P:=TJSONParser.Create('');
  Try
    J:=P.Parse;
    If (J<>Nil) then
      Fail('Empty returns Nil');
  Finally
    FreeAndNil(J);
    FreeAndNil(P);
  end;
end;

procedure TTestParser.TestInteger;

Var
  P : TJSONParser;
  J : TJSONData;

begin
  P:=TJSONParser.Create('1');
  Try
    J:=P.Parse;
    If (J=Nil) then
      Fail('Parse of 1 fails');
    TestJSONType(J,jtNumber);
    TestAsInteger(J,1);
  Finally
    FreeAndNil(J);
    FreeAndNil(P);
  end;
end;

procedure TTestParser.TestInt64;

Var
  P : TJSONParser;
  J : TJSONData;

begin
  P:=TJSONParser.Create('123456789012345');
  Try
    J:=P.Parse;
    If (J=Nil) then
      Fail('Parse of 123456789012345 fails');
    TestJSONType(J,jtNumber);
    TestAsInt64(J,123456789012345);
  Finally
    FreeAndNil(J);
    FreeAndNil(P);
  end;
end;

procedure TTestParser.TestNull;

Var
  P : TJSONParser;
  J : TJSONData;

begin
  P:=TJSONParser.Create('null');
  Try
    J:=P.Parse;
    If (J=Nil) then
      Fail('Parse of null fails');
    TestJSONType(J,jtNull);
  Finally
    FreeAndNil(J);
    FreeAndNil(P);
  end;
end;

procedure TTestParser.TestTrue;

Var
  P : TJSONParser;
  J : TJSONData;

begin
  P:=TJSONParser.Create('true');
  Try
    J:=P.Parse;
    If (J=Nil) then
      Fail('Parse of True fails');
    TestJSONType(J,jtBoolean);
    TestAsBoolean(J,True);
  Finally
    FreeAndNil(J);
    FreeAndNil(P);
  end;
end;

procedure TTestParser.TestFalse;

Var
  P : TJSONParser;
  J : TJSONData;

begin
  P:=TJSONParser.Create('false');
  Try
    J:=P.Parse;
    If (J=Nil) then
      Fail('Parse of False fails');
    TestJSONType(J,jtBoolean);
    TestAsBoolean(J,False);
  Finally
    FreeAndNil(J);
    FreeAndNil(P);
  end;
end;

procedure TTestParser.TestFloat;


begin
  DoTestFloat(1.2);
  DoTestFloat(-1.2);
  DoTestFloat(0);
  DoTestFloat(1.2e1);
  DoTestFloat(-1.2e1);
  DoTestFloat(0);
  DoTestFloat(1.2,'1.2');
  DoTestFloat(-1.2,'-1.2');
  DoTestFloat(0,'0.0');
end;

procedure TTestParser.TestString;

begin
  DoTestString('A string');
  DoTestString('');
  DoTestString('\"');
end;


procedure TTestParser.TestArray;

Var
  S1,S2,S3 : String;

begin
  DoTestArray('[]',0);
  DoTestArray('[null]',1);
  DoTestArray('[true]',1);
  DoTestArray('[false]',1);
  DoTestArray('[1]',1);
  DoTestArray('[1, 2]',2);
  DoTestArray('[1, 2, 3]',3);
  DoTestArray('[1234567890123456]',1);
  DoTestArray('[1234567890123456, 2234567890123456]',2);
  DoTestArray('[1234567890123456, 2234567890123456, 3234567890123456]',3);
  Str(12/10,S1);
  Delete(S1,1,1);
  Str(34/10,S2);
  Delete(S2,1,1);
  Str(34/10,S3);
  Delete(S3,1,1);
  DoTestArray('['+S1+']',1,true);
  DoTestArray('['+S1+', '+S2+']',2,true);
  DoTestArray('['+S1+', '+S2+', '+S3+']',3,true);
  DoTestArray('["A string"]',1);
  DoTestArray('["A string", "Another string"]',2);
  DoTestArray('["A string", "Another string", "Yet another string"]',3);
  DoTestArray('[null, false]',2);
  DoTestArray('[true, false]',2);
  DoTestArray('[null, 1]',2);
  DoTestArray('[1, "A string"]',2);
  DoTestArray('[1, []]',2);
  DoTestArray('[1, [1, 2]]',2);
end;

procedure TTestParser.TestTrailingComma;
begin
  FOptions:=[joIgnoreTrailingComma];
  DoTestArray('[1, 2,]',2,True);
  DoTestObject('{ "a" : 1, }',['a'],False);
end;

procedure TTestParser.TestTrailingCommaErrorArray;
begin
  AssertException('Need joIgnoreTrailingComma in options to allow trailing comma',EJSONParser,@DoTrailingCommaErrorArray) ;
end;

procedure TTestParser.TestTrailingCommaErrorObject;
begin
  AssertException('Need joIgnoreTrailingComma in options to allow trailing comma',EJSONParser,@DoTrailingCommaErrorObject);
end;

procedure TTestParser.DoTrailingCommaErrorArray;
begin
  DoTestArray('[1, 2,]',2,True);
end;

procedure TTestParser.DoTrailingCommaErrorObject;
begin
  DoTestObject('{ "a" : 1, }',['a'],False);
end;

procedure TTestParser.TestMixed;

Const

  SAddr ='{ "addressbook": { "name": "Mary Lebow", '+
         '  "address": {'+
         '      "street": "5 Main Street",'+LineEnding+
         '        "city": "San Diego, CA",'+LineEnding+
         '        "zip": 91912'+LineEnding+
         '    },'+LineEnding+
         '    "phoneNumbers": [  '+LineEnding+
         '        "619 332-3452",'+LineEnding+
         '        "664 223-4667"'+LineEnding+
         '    ]'+LineEnding+
         ' }'+LineEnding+
         '}';

begin
  DoTestArray('[1, {}]',2);
  DoTestArray('[1, { "a" : 1 }]',2);
  DoTestArray('[1, { "a" : 1 }, 1]',3);
  DoTestObject('{ "a" : [1, 2] }',['a']);
  DoTestObject('{ "a" : [1, 2], "B" : { "c" : "d" } }',['a','B']);
  DoTestObject(SAddr,['addressbook'],False);
end;

procedure TTestParser.TestComment;
begin
  FOptions:=[joComments];
  DoTestArray('/* */ [1, {}]',2,True);
  DoTestArray('//'+sLineBreak+'[1, { "a" : 1 }]',2,True);
  DoTestArray('/* '+sLineBreak+' */ [1, {}]',2,True);
  DoTestArray('/*'+sLineBreak+'*/ [1, {}]',2,True);
  DoTestArray('/*'+sLineBreak+'*/ [1, {}]',2,True);
  DoTestArray('/*'+sLineBreak+'*'+sLineBreak+'*/ [1, {}]',2,True);
  DoTestArray('/**'+sLineBreak+'**'+sLineBreak+'**/ [1, {}]',2,True);
  DoTestArray('/* */ [1, {}]',2,True);
  DoTestArray('[1, { "a" : 1 }]//'+sLineBreak,2,True);
  DoTestArray('[1, {}]/* '+sLineBreak+' */ ',2,True);
  DoTestArray('[1, {}]/*'+sLineBreak+'*/ ',2,True);
  DoTestArray('[1, {}]/*'+sLineBreak+'*/ ',2,True);
  DoTestArray('[1, {}]/*'+sLineBreak+'*'+sLineBreak+'*/ ',2,True);
  DoTestArray(' [1, {}]/**'+sLineBreak+'**'+sLineBreak+'**/',2,True);
end;

procedure TTestParser.TestObject;
begin
  DoTestObject('{}',[]);
  DoTestObject('{ "a" : 1 }',['a']);
  DoTestObject('{ "a" : 1, "B" : "String" }',['a','B']);
  DoTestObject('{ "a" : 1, "B" : {} }',['a','B']);
  DoTestObject('{ "a" : 1, "B" : { "c" : "d" } }',['a','B']);
end;

procedure TTestParser.TestObjectError;
begin

  DoTestError('{ "name" : value }',[joUTF8]);
end;


procedure TTestParser.DoTestObject(S: String; const ElNames: array of String;
  DoJSONTest: Boolean);

Var
  P : TJSONParser;
  J : TJSONData;
  O : TJSONObject;
  I : Integer;

begin
  J:=Nil;
  P:=TJSONParser.Create(S);
  Try
    P.Options:=FOptions;
    J:=P.Parse;
    If (J=Nil) then
      Fail('Parse of object "'+S+'" fails');
    TestJSONType(J,jtObject);
    TestItemCount(J,High(ElNames)-Low(ElNames)+1);
    O:=TJSONObject(J);
    For I:=Low(ElNames) to High(ElNames) do
      AssertEquals(Format('Element %d name',[I-Low(Elnames)])
                   ,ElNames[i], O.Names[I-Low(ElNames)]);
    If DoJSONTest then
      self.TestJSON(J,S);
  Finally
    FreeAndNil(J);
    FreeAndNil(P);
  end;
end;


procedure TTestParser.DoTestArray(S : String; ACount : Integer; IgnoreJSON : Boolean = False);

Var
  P : TJSONParser;
  J : TJSONData;

begin
  J:=Nil;
  P:=TJSONParser.Create(S,[joComments]);
  Try
    P.Options:=FOptions;
    J:=P.Parse;
    If (J=Nil) then
      Fail('Parse of array "'+S+'" fails');
    TestJSONType(J,jtArray);
    TestItemCount(J,ACount);
    if not IgnoreJSON then
      TestJSON(J,S);
  Finally
    FreeAndNil(J);
    FreeAndNil(P);
  end;
end;

procedure TTestParser.DoTestClass(S: String; AClass: TJSONDataClass);

Var
  P : TJSONParser;
  D : TJSONData;

begin
  P:=TJSONParser.Create(S);
  try
    D:=P.Parse;
    try
      AssertEquals('Correct class for '+S+' : ',AClass,D.ClassType);
    finally
      D.Free
    end;
  finally
    P.Free;
  end;
end;

procedure TTestParser.TestErrors;

begin

  DoTestError('a');
  DoTestError('"b');
  DoTestError('1Tru');

  DoTestError('b"');
  DoTestError('{"a" : }');
  DoTestError('{"a" : ""');
  DoTestError('{"a : ""');

  DoTestError('[1,]');
  DoTestError('[,]');
  DoTestError('[,,]');
  DoTestError('[1,,]');

end;

procedure TTestParser.TestClasses;
begin
  SetMyInstanceTypes;
  DoTestClass('null',TMyNull);
  DoTestClass('true',TMyBoolean);
  DoTestClass('1',TMyInteger);
  DoTestClass('1.2',TMyFloat);
  DoTestClass('123456789012345',TMyInt64);
  DoTestClass('"tata"',TMyString);
  DoTestClass('{}',TMyObject);
  DoTestClass('[]',TMyArray);
end;

procedure TTestParser.CallNoHandler;

begin
  GetJSON('1',True).Free;
end;

procedure TTestParser.Setup;
begin
  inherited Setup;
  FOptions:=[];
end;

procedure TTestParser.CallNoHandlerStream;

Var
  S : TStringStream;

begin
  S:=TstringStream.Create('1');
  try
    GetJSON(S,True).Free;
  finally
    S.Free;
  end;
end;

procedure TTestParser.TestHandler;
begin
  AssertNotNull('Handler installed',GetJSONParserHandler);
end;

procedure TTestParser.TestNoHandlerError;

Var
  H : TJSONParserHandler;

begin
  H:=GetJSONParserHandler;
  try
    AssertSame('SetJSONParserHandler returns previous handler',H,SetJSONParserHandler(Nil));
    AssertException('No handler raises exception',EJSON,@CallNoHandler);
    AssertException('No handler raises exception',EJSON,@CallNoHandlerStream);
  finally
    SetJSONParserHandler(H);
  end;
end;

procedure TTestParser.TestHandlerResult;

Var
  D : TJSONData;

begin
  D:=GetJSON('"123"');
  try
    AssertEquals('Have correct string','123',D.AsString);
  finally
    D.Free;
  end;
end;

procedure TTestParser.TestHandlerResultStream;
Var
  D : TJSONData;
  S : TStream;

begin
  S:=TStringStream.Create('"123"');
  try
    D:=GetJSON(S);
    try
      AssertEquals('Have correct string','123',D.AsString);
    finally
      D.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TTestParser.DoTestError(S : String; Options : TJSONOptions = DefaultOpts);

Var
  P : TJSONParser;
  J : TJSONData;
  ParseOK : Boolean;
  N : String;

begin
  ParseOK:=False;
  P:=TJSONParser.Create(S);
  P.OPtions:=Options;
  J:=Nil;
  Try
    Try
      Repeat
        FreeAndNil(J);
        J:=P.Parse;
        ParseOK:=True;
        If (J<>Nil) then
          N:=J.ClassName;
      Until (J=Nil)
    Finally
      FreeAndNil(J);
      FreeAndNil(P);
    end;
  except
    ParseOk:=False;
  end;
  If ParseOK then
    Fail('Parse of JSON string "'+S+'" should fail, but returned '+N);
end;

procedure TTestParser.DoTestString(S: String);

Var
  P : TJSONParser;
  J : TJSONData;

begin
  P:=TJSONParser.Create('"'+S+'"');
  Try
    J:=P.Parse;
    If (J=Nil) then
      Fail('Parse of string "'+S+'" fails');
    TestJSONType(J,jtString);
    TestAsString(J,JSONStringToString(S));
    TestJSON(J,'"'+S+'"');
  Finally
    FreeAndNil(J);
    FreeAndNil(P);
  end;

end;

procedure TTestParser.DoTestFloat(F : TJSONFloat);

Var
  S : String;

begin
  Str(F,S);
  DoTestFloat(F,S);
end;

procedure TTestParser.DoTestFloat(F : TJSONFloat; S : String);

Var
  P : TJSONParser;
  J : TJSONData;

begin
  P:=TJSONParser.Create(S);
  Try
    J:=P.Parse;
    If (J=Nil) then
      Fail('Parse of float '+S+' fails');
    TestJSONType(J,jtNumber);
    TestAsFloat(J,F);
  Finally
    FreeAndNil(J);
    FreeAndNil(P);
  end;
end;


initialization
  RegisterTest(TTestParser);
end.

