{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    YAML to JSON converter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpyaml.json;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, FpJson.Data,
{$ELSE}
  SysUtils, fpjson,
{$ENDIF}
  fpyaml.data, fpyaml.types;

function YamlToJSON(aYaml : TYAMLData) : TJSONData;

implementation

uses fpyaml.strings;

function YAMLValueToJSONValue(aYaml : TYAMLData) : TJSONData; forward;

Function YAMLScalarToJSON(aYAML : TYAMLScalar) : TJSONData;

var
  TT : TYAMLTagType;

begin
  TT:=TYAMLTagType.FromString(aYAML.Tag);
  case TT of
    yttNull :
      Result:=TJSONNull.Create;
    yttInteger :
      if aYAML.AsInt64>MaxInt then
        Result:=TJSONInt64Number.Create(aYAML.AsInt64)
      else
        Result:=TJSONIntegerNumber.Create(aYAML.AsInteger);
    yttFloat :
      Result:=TJSONFloatNumber.Create(aYAML.AsDouble);
    yttBoolean :
      Result:=TJSONBoolean.Create(aYAML.AsBoolean);
  else
    Result:=TJSONString.Create(aYAML.AsString);
  end;
end;

Function YAMLMappingToJSONObject(aYAML : TYAMLMapping) : TJSONObject;

var
  I : Integer;
  lKey : String;

begin
  Result:=TJSONObject.Create;
  try
    For I:=0 to aYAML.Count-1 do
      begin
      if Not (aYAML.Key[i] is TYAMLScalar) then
        Raise EConvertError.Create(SErrOnlyScalarKeys);
      if (TYAMLSCalar(aYAML.Key[i]).Tag=yttNull.ToString) then
        Raise EConvertError.Create(SErrOnlyScalarKeys);
      lKey:=TYAMLScalar(aYAML.Key[i]).Value;
      Result.Add(lKey,YAMLValueToJSONValue(aYAML[I]));
      end;
  except
    Result.Free;
    Raise;
  end;
end;

Function YAMLSequenceToJSONArray(aYAML : TYAMLSequence) : TJSONArray;

var
  I : Integer;

begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to aYAML.Count-1 do
      Result.Add(YAMLValueToJSONValue(aYAML[I]));
  except
    Result.Free;
    Raise;
  end;
end;


function YAMLValueToJSONValue(aYaml : TYAMLData) : TJSONData;

begin
  if aYAML is TYAMLMapping then
    Result:=YAMLMappingToJSONObject(TYAMLMapping(aYAML))
  else if aYAML is TYAMLSequence then
    Result:=YAMLSequenceToJSONArray(TYAMLSequence(aYAML))
  else if (aYAML is TYAMLScalar) then
    Result:=YAMLScalarToJSON(TYAMLScalar(aYAML))
  else
    begin
    if assigned(aYAML) then
      Raise EConvertError.CreateFmt(SErrUnknownYAMLtype,[aYAML.ClassName])
    else
      Raise EConvertError.CreateFmt(SErrUnknownYAMLtype,['<NIL>']);
    end;
end;

function YamlToJSON(aYaml : TYAMLData) : TJSONData;

begin
  if aYAML is TYAMLStream then
    begin
    if TYAMLStream(aYAML).DocumentCount<>1 then
      Raise EConvertError.Create(SErrOnlySingleDocument);
    aYAML:=TYAMLStream(aYAML).Documents[0];
    end;
  if aYAML is TYAMLDocument then
    begin
    if TYAMLDocument(aYAML).Count<>1 then
      Raise EConvertError.Create(SErrOnlySingleValue);
    aYAML:=TYAMLDocument(aYAML).Items[0];
    end;
  Result:=YAMLValueToJSONValue(aYAML);
end;

end.

