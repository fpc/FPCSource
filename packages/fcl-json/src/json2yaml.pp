{
    This file is part of the Free Component Library
    Copyright (c) 2017 by Michael Van Canneyt michael@freepascal.org

    JSON To YAML syntax converter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit json2yaml;

{$MODE OBJFPC}
{$H+}

interface

uses classes,fpjson;

Type

  { TJSONToYaml }

  TJSONToYaml = class
  Private
    FAddHeader: Boolean;
    FIndent : String;
    FIndentSize: integer;
    FOutput : TStream;
    FLineBreak : String;
    FIndentAdd : String;
  Protected
    Function Indent(S :String): Integer;
    Procedure ConvertNull;
    Procedure ConvertBoolean(JSON : TJSONBoolean);
    Procedure ConvertArray(JSON : TJSONArray);
    procedure ConvertObject(JSON: TJSONObject);
    Procedure ConvertNumber(JSON : TJSONNumber);
    Procedure ConvertString(JSON : TJSONString);
    Procedure DoConvert(JSON : TJSONData);
    procedure Push(S: TJSONStringType);
    Property TheOutput : TStream Read Foutput;
  Public
    Procedure Convert(JSON : TJSONData; aOutput : TStream);
    Property AddHeader : Boolean Read FAddHeader Write FAddHeader;
    Property IndentSize: Integer Read FIndentSize Write FIndentSize;
  end;

implementation

procedure TJSONToYaml.Push(S : TJSONStringType);

begin
  if (Findent<>'') then
    Foutput.WriteBuffer(Findent[1],Length(Findent));
  S:=S+FLineBreak;
  Foutput.WriteBuffer(S[1],Length(S));
  Findent:=StringOfChar(' ',Length(FIndent));
end;

procedure TJSONToYaml.ConvertNumber(JSON: TJSONNumber);

begin
  Push(JSON.AsString);
end;

procedure TJSONToYaml.ConvertString(JSON: TJSONString);
begin
  Push(JSON.AsString);
end;

function TJSONToYaml.Indent(S: String): Integer;
begin
  Result:=Length(FIndent);
  if (S='') then
    S:=FIndentAdd;
  Findent:=Findent+S;
end;

procedure TJSONToYaml.ConvertNull;
begin
  Push('null');
end;

procedure TJSONToYaml.ConvertBoolean(JSON: TJSONBoolean);
begin
  If JSON.AsBoolean then
    Push('true')
  else
    Push('false');
end;

procedure TJSONToYaml.ConvertArray(JSON: TJSONArray);

Var
  l,i : Integer;

begin
  L:=Length(Findent);
  try
    For I:=0 to JSON.Count-1 do
      begin
      Indent('- ');
      DoConvert(JSON.Items[i]);
      SetLength(Findent,L);
      end;
  finally
    SetLength(Findent,L);
  end;
end;

procedure TJSONToYaml.ConvertObject(JSON: TJSONObject);

Var
  I,L : Integer;
  D : TJSONData;
  N : TJSONStringType;

begin
  L:=Length(FIndent);
  try
    For I:=0 to JSON.Count-1 do
      begin
      D:=JSON.Items[I];
      N:=JSON.Names[I];
      if D.JSONType in fpjson.StructuredJSONTypes then
        begin
        Push(N+': ');
        Indent('');
        end
      else
        Indent(N+': ');
      DoConvert(D);
      SetLength(Findent,L);
      end
  finally
    SetLength(Findent,L);
  end;
end;

procedure TJSONToYaml.Convert(JSON: TJSONData; aOutput: TStream);

begin
  If FIndentSize=0 then
    FIndentSize:=2;
  Findent:='';
  FIndentAdd:=StringOfChar(' ',FIndentSize);
  FLineBreak:=sLineBreak;
  FOutput:=AOUtput;
  if AddHeader then
    Push('---');
  DoConvert(JSON);
end;

procedure TJSONToYaml.DoConvert(JSON: TJSONData);
begin
  Case JSON.JSONType of
    jtArray : convertArray(JSON as TJSONArray);
    jtObject : convertObject(JSON as TJSONObject);
    jtString : convertString(JSON as TJSONString);
    jtNull : ConvertNull;
    jtNumber : ConvertNumber(JSON as TJSONNumber);
    jtBoolean : ConvertBoolean(JSON as TJSONBoolean);
  end;
end;

end.
