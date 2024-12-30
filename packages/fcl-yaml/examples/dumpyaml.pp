{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    YAML Parser & Dumping data demo

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program dumpyaml;

uses sysutils, fpyaml.types, fpyaml.data, fpyaml.parser;

var
  minimal : Boolean;

Procedure DumpYAMLValue(Y : TYAMLData; aPrefix : String); forward;

function YAMLToString(Y : TYAMLData) : String;

begin
  if Minimal then
    begin
    if (Y is TYAMLScalar) then
      Result:=Y.AsString
    else
      Result:=Y.ClassName;
    if TYAMLTagType.FromString(Y.Tag)=yttString then
      Result:='"'+Result+'"';
    end
  else
    if (Y is TYAMLScalar) then
      Result:=Format('scalar<%s> (%s)',[Y.AsString,Y.Tag])
    else
      Result:=Format('%s<%s>',[Y.ClassName,Y.Tag]);
end;


Procedure DumpYAMLMapping(Y : TYAMLMapping; aPrefix : String);

Var
  I : Integer;
  lKey : String;
begin
  if Minimal then
    Writeln('{')
  else
    Writeln('Mapping {');
  For I:=0 to Y.Count-1 do
    begin
    lKey:=YamlToString(Y.Key[I]);
    if not Minimal then
      lKey:=format('Key[%d] %s',[i,lKey]);
    Write(aPrefix+'  ',lKey+': ');
    DumpYAMLValue(Y[i],'  '+aPrefix);
    end;
  Writeln(aPrefix,'}');
end;

Procedure DumpYAMLSequence(Y : TYAMLSequence; aPrefix : String);

Var
  I : Integer;

begin
  if Minimal then
    Writeln('[')
  else
    Writeln('Sequence [');
  For I:=0 to Y.Count-1 do
    begin
    Write(aPrefix+'  ');
    if Minimal then
      Write('- ' )
    else
      Write(format('Item[%d] :',[i]));
    DumpYAMLValue(Y[i],'  '+aPrefix);
    end;
  Writeln(aPrefix,']');
end;


Procedure DumpYAMLValue(Y : TYAMLData; aPrefix : String);

begin
  if (Y is TYAMLMapping) then
    DumpYAMLMapping(TYAMLMapping(Y),aPrefix)
  else if (Y is TYAMLSequence) then
    DumpYAMLSequence(TYAMLSequence(Y),aPrefix)
  else
    Writeln(YAMLToString(Y));
end;

Procedure DumpDocument(Y : TYAMLDocument);
var
  I : Integer;

begin
  For I:=0 to Y.Count-1 do
    DumpYAMLValue(Y[i],'');
end;

Procedure DumpYAML(Yaml : TYAMLStream);

var
  Y : TYAMLData;
  I : Integer;

begin
  Writeln('YAML Stream with ',YAML.Count,' items');
  For I:=0 to YAML.Count-1 do
    begin
    Y:=YAML[I];
    if Y is TYAMLDocument then
      begin
      Writeln('Document ',I,':');
      DumpDocument(TYAMLDocument(Y));
      end
    else
      Writeln('Item ',I,' : ',Y.ClassName,' : ',YAMLToString(Y));
    end;
end;

var
  YAML: TYAMLStream;
  P : TYAMLParser;

begin
  MiniMal:=ParamStr(1)='-m';
  if (ParamStr(1+Ord(Minimal))='') or (ParamStr(1)='-h') then
    begin
    Writeln('Usage : ',ExtractFileName(ParamStr(0)),' [-m] inputfile');
    Writeln('-m : output minimal structural info');
    Halt(Ord(ParamStr(1)<>'-h'));
    end;
  YAML:=Nil;
  P:=TYAMLParser.Create(Paramstr(1+Ord(Minimal)));
  try
    YAML:=P.Parse;
    DumpYaml(YAML);
  finally
    YAML.Free;
    P.Free;
  end;
end.

