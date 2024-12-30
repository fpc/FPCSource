{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    YAML-To-JSON conversion demo.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}
program yaml2json;

uses sysutils, fpyaml.data, fpyaml.parser, fpyaml.json, fpjson;

var
  YAML : TYAMLStream;
  JSON : TJSONData;
  MiniMal : Boolean;

begin
  JSON:=Nil;
  YAML:=nil;
  Minimal:=ParamStr(1)='-m';
  if (Paramstr(1+Ord(Minimal))='') or (ParamStr(1)='-h') then
    begin
    Writeln('Usage: ',ExtractFilePath(ParamStr(0)),' [-m] inputfile');
    Writeln('-m : minimal output. Default is to format output.');
    Halt(Ord(ParamStr(1)<>'-h'));
    end;
  With TYAMLParser.Create(Paramstr(1+Ord(Minimal))) do
    try
      YAML:=Parse;
    finally
      Free;
    end;
  try
    JSON:=YAMLToJSON(YAML);
    if Minimal then
      Writeln(JSON.AsJSON)
    else
      Writeln(JSON.FormatJSON());
  finally
    YAML.Free;
    JSON.Free;
  end;
end.
