{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    YAML basic types

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpyaml.types;

{$mode ObjFPC}
{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, fpyaml.strings;
{$ELSE}
uses SysUtils, fpyaml.strings;
{$ENDIF}

Type
  EYAML = class(Exception);

  TYAMLString = AnsiString;

  { TYAMLVersion }

  TYAMLVersion = record
    Major: Integer;
    Minor: Integer;
    Constructor Create(const aMajor, aMinor : Integer);
    function ToString : String;
    function IsEmpty : Boolean;
  end;

  { TYAMLTag }

  TYAMLTag = record
    Handle: TYAMLString;
    Prefix: TYAMLString;
    constructor Create(const aHandle, aPrefix: TYAMLString);
  end;
  TYAMLTagArray = Array of TYamlTag;

  TYAMLTagType = (yttCustom,yttNull,yttBoolean,yttString,yttInteger,yttFloat,yttTimeStamp,yttSequence,yttMap);

  { TYAMLTagTypeHelper }

  TYAMLTagTypeHelper = Type helper for TYAMLTagType
  private
    procedure SetAsString(const AValue: String);
  public
    class function FromString(const aString : String) : TYAMLTagType; static;
    function ToString: String;
    Property AsString : String Read ToString Write SetAsString;
  end;

  TYAMLScalarKind = (
    yskPlain,
    yskSingle,
    yskDouble,
    yskLiteral,
    yskFolded);

  TYAMLCollectionKind = (
    yckBlock,
    yckFlow);

Const
  YAMLTagNames : Array[TYAMLTagType] of string = (
  '',
  'tag:yaml.org,2002:null',
  'tag:yaml.org,2002:bool',
  'tag:yaml.org,2002:str',
  'tag:yaml.org,2002:int',
  'tag:yaml.org,2002:float',
  'tag:yaml.org,2002:timestamp',
  'tag:yaml.org,2002:seq',
  'tag:yaml.org,2002:map');

implementation


{ TYAMLVersion }

constructor TYAMLVersion.Create(const aMajor, aMinor: Integer);
begin
  Major:=aMajor;
  Minor:=aMinor;
end;

function TYAMLVersion.ToString: String;
begin
  Result:=Format('%d.%d',[Major,Minor]);
end;

function TYAMLVersion.IsEmpty: Boolean;
begin
  Result:=(Major=0) and (Minor=0);
end;

{ TYAMLTag }

constructor TYAMLTag.Create(const aHandle, aPrefix: TYAMLString);
begin
  Handle:=aHandle;
  Prefix:=aPrefix;
end;

{ TYAMLTagTypeHelper }

procedure TYAMLTagTypeHelper.SetAsString(const AValue: String);
var
  T : TYAMLTagType;

begin
  Self:=yttCustom;
  for T in TYAMLTagType do
    if T.ToString=aValue then
      begin
      Self:=T;
      exit;
      end;
end;

class function TYAMLTagTypeHelper.FromString(const aString: String): TYAMLTagType;
begin
  Result:=Default(TYAMLTagType);
  Result.AsString:=aString;
end;

function TYAMLTagTypeHelper.ToString: String;
begin
  Result:=YAMLTagNames[Self];
end;

end.

