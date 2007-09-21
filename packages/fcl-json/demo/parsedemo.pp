{
    This file is part of the Free Component Library

    JSON Parser demo
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program parsedemo;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpjson,jsonparser;

Procedure DoParse(P : TJSONParser);

Var
  J : TJSONData;
  
begin
  Try
    J:=P.Parse;
    Try
      Writeln('Parse succesful. Dumping JSON data : ');
      If Assigned(J) then
        begin
        Writeln('Returned JSON structure has class : ',J.ClassName);
        Writeln(J.AsJSON)
        end
      else
        Writeln('No JSON data available');
    Finally
      FreeAndNil(J);
    end;
  except
    On E : Exception do
      Writeln('An Exception occurred when parsing : ',E.Message);
  end;
end;


Procedure ParseFile (FileName : String);

Var
  F : TFileStream;
  P : TJSONParser;

begin
  F:=TFileStream.Create(FileName,fmopenRead);
  try
    // Create parser with Stream as source.
    P:=TJSONParser.Create(F);
    try
      DoParse(P);
    finally
      FreeAndNil(P);
    end;
  finally
    F.Destroy;
  end;
end;

Procedure ParseString(S : String);

Var
  P : TJSONParser;
begin
  // Create parser with Stream as source.
  P:=TJSONParser.Create(S);
  try
    DoParse(P);
  finally
    FreeAndNil(P);
  end;
end;

Procedure DefaultParsing;

Const
  // From JSON website
 SAddr ='{ "addressbook": { "name": "Mary Lebow", '+
         '  "address": {'+
         '      "street": "5 Main Street",'+LineEnding+
         '        "city": "San Diego, CA",'+LineEnding+
         '        "zip": 91912,'+LineEnding+
         '    },'+LineEnding+
         '    "phoneNumbers": [  '+LineEnding+
         '        "619 332-3452",'+LineEnding+
         '        "664 223-4667"'+LineEnding+
         '    ]'+LineEnding+
         ' }'+LineEnding+
         '}';


begin
  ParseString('');
  ParseString('NULL');
  ParseString('1');
  ParseString('2.3');
  ParseString('True');
  ParseString('False');
  ParseString('"A string"');
  ParseString('[ Null, False, 1 , 2.3,  "a" , { "b" : 1 }]');
  ParseString('{ "a" : 1, "b" : "Something" }');
  ParseString(SAddr);
end;

Procedure Usage;

begin
  Writeln('Usage : parsedemo arg1 [arg2 [arg3 ...[argN]]]');
  Writeln('  ArgN can be the name of an existing file, or a JSON string');
end;

Var
  I : Integer;
  
begin
  If (ParamCount=0) then
    DefaultParsing
  else if (ParamCount=1) and ((Paramstr(1)='-h') or (ParamStr(1)='--help')) then
    Usage
  else
    For I:=1 to ParamCount do
      If FileExists(Paramstr(i)) then
        ParseFile(ParamStr(I))
      else
        ParseString(Paramstr(I));
end.

