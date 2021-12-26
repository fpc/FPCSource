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
    procedure DoDuplicate;
    procedure DoTestError(S: String; Options : TJSONOptions = DefaultOpts);
    procedure DoTestFloat(F: TJSONFloat); overload;
    procedure DoTestFloat(F: TJSONFloat; S: String); overload;
    procedure DoTestObject(S: String; const ElNames: array of String; DoJSONTest : Boolean = True);
    procedure DoTestString(S : String; AResult : String);
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
    Procedure TestEmptyLine;
    Procedure TestStartEmptyLine;
    Procedure TestObjectEmptyLine;
    Procedure TestCommentLine;
    Procedure TestFirstLineComment;
    Procedure TestMultiLineComment;
    Procedure TestIgnoreDuplicates;
    Procedure TestNoIgnoreDuplicates;
  end;

implementation

procedure TTestParser.TestEmpty;

Var
  P : TJSONParser;
  J : TJSONData;
  
begin
  P:=TJSONParser.Create('',[joUTF8]);
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
  P:=TJSONParser.Create('1',[joUTF8]);
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
  P:=TJSONParser.Create('123456789012345',[joUTF8]);
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
  P:=TJSONParser.Create('null',[joUTF8]);
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
  P:=TJSONParser.Create('true',[joUTF8]);
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
  P:=TJSONParser.Create('false',[joUTF8]);
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

Const
  // Glowing star in UTF8
  GlowingStar = #$F0#$9F#$8C#$9F;

begin
  DoTestString('A string');
  DoTestString('');
  DoTestString('\"');
  DoTestString('\u00f8','ø'); // this is ø
  DoTestString('\u00f8\"','ø"'); // this is ø"
//  Writeln(GlowingStar);
  DoTestString('\ud83c\udf1f',GlowingStar);
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
  P:=TJSONParser.Create(S,[joUTF8]);
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
  P:=TJSONParser.Create(S,[joUTF8]);
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

  DoTestError('1Tru');
  DoTestError('a');
  DoTestError('"b');

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
  HS : TJSONStringParserHandler;

begin
  H:=GetJSONParserHandler;
  HS:=GetJSONStringParserHandler;
  try
    AssertSame('SetJSONParserHandler returns previous handler',H,SetJSONParserHandler(Nil));
    AssertSame('SetJSONStringParserHandler returns previous handler',HS,SetJSONStringParserHandler(Nil));
    AssertException('No handler raises exception',EJSON,@CallNoHandler);
    AssertException('No handler raises exception',EJSON,@CallNoHandlerStream);
  finally
    SetJSONParserHandler(H);
    SetJSONStringParserHandler(HS);
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

procedure TTestParser.TestEmptyLine;
// Bug report 36037
Const MyJSON =
'  {'+sLineBreak+
'  "pylib__linux" : "libpython3.7m.so.1.0",'+sLineBreak+
'  "ui_toolbar_theme": "default_24x24",'+sLineBreak+
'  "ui_toolbar_show" : true,'+sLineBreak+
'  "font_name__linux" : "DejaVu Sans Mono",'+sLineBreak+
'  "font_size__linux" : 10,'+sLineBreak+
'    "ui_listbox_fuzzy": false,'+sLineBreak+
'    "ui_max_size_lexer": 5,'+sLineBreak+
'    "find_separate_form": false,'+sLineBreak+sLineBreak+
'}';
  var
    J : TJSONData;
begin
  With TJSONParser.Create(MyJSON,[joUTF8,joIgnoreTrailingComma]) do
  Try
    J:=Parse;
    J.Free;
  Finally
    Free;
  end;
end;

procedure TTestParser.TestStartEmptyLine;

// Bug ID 37352: case 1

const
  ENDLINE = #$0d#$0a;

Const
  MyJSON = ENDLINE+
    '{'+ENDLINE+
      '"version":100,'+ENDLINE+
//      '//comment'+ENDLINE+
      '"value":200'+ENDLINE+
    '}'+ENDLINE;

var
  J : TJSONData;

begin
  With TJSONParser.Create(MyJSON,[joComments]) do
    Try
      J:=Parse;
      J.Free;
    Finally
      Free;
    end;
end;

procedure TTestParser.TestObjectEmptyLine;

// Bug ID 37352: case 2

const
  ENDLINE = #$0d#$0a;


Const
  MyJSON = '{'+ENDLINE+
        ''+ENDLINE+
        '"version":100, //comment'+ENDLINE+
        '"value":200'+ENDLINE+
      '}'+ENDLINE;
var
  J : TJSONData;

begin
  With TJSONParser.Create(MyJSON,[joComments]) do
    Try
      J:=Parse;
      J.Free;
    Finally
      Free;
    end;
end;

procedure TTestParser.TestCommentLine;

// Bug ID 37352: case 3

const
  ENDLINE = #$0d#$0a;


Const
  MyJSON =
        ENDLINE+
            '{'+ENDLINE+
              '"version":100, //comment'+ENDLINE+
              '"value":200'+ENDLINE+
            '}'+ENDLINE;

var
  J : TJSONData;

begin
  With TJSONParser.Create(MyJSON,[joComments]) do
    Try
      J:=Parse;
      J.Free;
    Finally
      Free;
    end;
end;

procedure TTestParser.TestFirstLineComment;

// New case
const
  ENDLINE = #$0d#$0a;


Const
  MyJSON =
        '//comment1'+ENDLINE+
              '{'+ENDLINE+
              '"version":100, //comment2'+ENDLINE+
              '"value":200'+ENDLINE+
            '}'+ENDLINE;

var
  J : TJSONData;

begin
  With TJSONParser.Create(MyJSON,[joComments]) do
    Try
      J:=Parse;
      J.Free;
    Finally
      Free;
    end;

end;

procedure TTestParser.TestMultiLineComment;

// Issue  37367

const
  ENDLINE = #$0d#$0a;


Const
  MyJSON =
        '/* long comment'+ENDLINE+
        ''+ENDLINE+
        '  error'+ENDLINE+
        '*/'+ENDLINE+
        '{'+ENDLINE+
        '  "version":100, //coment2 without comment2 works well '+ENDLINE+
        '  "valor":200   /*comment 3'+ENDLINE+
        '    line 2'+ENDLINE+
        '   */'+ENDLINE+
        '}'+ENDLINE;

var
  J : TJSONData;

begin
  With TJSONParser.Create(MyJSON,[joComments]) do
    Try
      J:=Parse;
      J.Free;
    Finally
      Free;
    end;
end;

procedure TTestParser.TestIgnoreDuplicates;

Const
  MyJSON =
        '{ "a":100, "b": 20, "a":300} ';

var
  J : TJSONData;

begin
  With TJSONParser.Create(MyJSON,[joIgnoreDuplicates]) do
    Try
      J:=Parse;
      AssertEquals('Correct class',TJSONObject,J.ClassType);
      AssertEquals('Correct value',100,TJSONObject(J).Get('a',0));
      J.Free;
    Finally
      Free;
    end;
end;

procedure TTestParser.DoDuplicate;

Const
  MyJSON =
        '{ "a":100, "b": 20, "a":300} ';

var
  J : TJSONData;

begin
  With TJSONParser.Create(MyJSON,[]) do
    Try
      J:=Parse;
      J.Free;
    Finally
      Free;
    end;
end;

procedure TTestParser.TestNoIgnoreDuplicates;

begin
  AssertException('No duplicates allowed',EJSON,@DoDuplicate);
end;

procedure TTestParser.DoTestError(S : String; Options : TJSONOptions = DefaultOpts);

Var
  P : TJSONParser;
  J : TJSONData;
  ParseOK : Boolean;
  N : String;

begin
  ParseOK:=False;
  P:=TJSONParser.Create(S,[joUTF8]);
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

begin
  DoTestString(S,JSONStringToString(S));
end;

procedure TTestParser.DoTestString(S: String; AResult : String);

Var
  P : TJSONParser;
  J : TJSONData;

begin
  P:=TJSONParser.Create('"'+S+'"',[joUTF8]);
  Try
    J:=P.Parse;
    If (J=Nil) then
      Fail('Parse of string "'+S+'" fails');
    TestJSONType(J,jtString);
    TestAsString(J,aResult);
    if Pos('\u',S)=0 then
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
  P:=TJSONParser.Create(S,[joUTF8]);
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

