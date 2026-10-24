{ Google API Compatibility Unit Generator

  Generates a compatibility unit that provides the old-style API
  (type names, resource classes) while delegating to the new generated code.

  Copyright (C) 2024 Michael Van Canneyt michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}

unit googleapi.compat.generator;

{$mode objfpc}
{$h+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Contnrs,
  {$ELSE}
  SysUtils, Classes, Contnrs,
  {$ENDIF}
  fpjson.schema.pascaltypes,
  fpopenapi.types,
  fpopenapi.pascaltypes;

type
  TCompatLogEvent = procedure(EventType: TEventType; const aMessage: string) of object;

  { TTypeMapping }
  TTypeMapping = class
    OldName: string;
    NewName: string;
    IsAlias: boolean;  // True if simple alias, False if wrapper needed
  end;

  { TMethodMapping }
  TMethodMapping = class
    OldMethodName: string;
    NewMethodName: string;
    ServiceName: string;
    PathParams: TStringList;      // Path parameters (required)
    QueryParams: TStringList;     // Query parameters (optional)
    InterfaceParams: TStringList; // Parameters in interface order (for call generation)
    RequestBodyType: string;      // Request body type (if any)
    RequestBodyParamName: string; // Name for the request body parameter
    ResultType: string;
    HasOptionalParams: boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { TResourceMapping }
  TResourceMapping = class
    OldResourceName: string;
    NewInterfaceName: string;
    Methods: TFPObjectList;
    constructor Create;
    destructor Destroy; override;
  end;

  { TCompatibilityUnitGenerator }

  TCompatibilityUnitGenerator = class
  private
    FAPIData: TAPIData;
    FServiceName: string;
    FTypeMappings: TFPObjectList;
    FResourceMappings: TFPObjectList;
    FOutput: TStringList;
    FIndent: integer;
    FOnLog: TCompatLogEvent;
    FNewDtoUnit: string;
    FNewIntfUnit: string;
    FNewImplUnit: string;
    procedure Log(const aMessage: string);
    procedure Log(const aFmt: string; const aArgs: array of const);
    procedure AddLine(const aLine: string = '');
    procedure AddFmt(const aFmt: string; const aArgs: array of const);
    procedure IncIndent;
    procedure DecIndent;
    function IndentStr: string;
    // Analysis
    procedure AnalyzeTypes;
    procedure AnalyzeServices;
    function GenerateOldTypeName(const aSchemaName: string; aTypeData: TPascalTypeData): string;
    function GenerateOldArrayTypeName(const aElementTypeName, aPropertyContext: string): string;
    function GenerateOldResourceName(const aServiceName: string): string;
    function BuildOldStyleParamList(MethMapping: TMethodMapping): string;
    function BuildNewStyleParamCall(MethMapping: TMethodMapping): string;
    // Generation
    procedure GenerateHeader;
    procedure GenerateInterface;
    procedure GenerateTypeAliases;
    procedure GenerateArrayTypeAliases;
    procedure GenerateResourceClasses;
    procedure GenerateImplementation;
    procedure GenerateResourceImplementations;
  public
    constructor Create(aAPIData: TAPIData; const aServiceName: string);
    destructor Destroy; override;
    procedure Generate(const aOutputFileName: string);
    procedure SetNewUnits(const aDtoUnit, aIntfUnit, aImplUnit: string);
    property OnLog: TCompatLogEvent read FOnLog write FOnLog;
  end;

implementation

{ TMethodMapping }

constructor TMethodMapping.Create;
begin
  inherited Create;
  PathParams := TStringList.Create;
  QueryParams := TStringList.Create;
  InterfaceParams := TStringList.Create;
end;

destructor TMethodMapping.Destroy;
begin
  PathParams.Free;
  QueryParams.Free;
  InterfaceParams.Free;
  inherited Destroy;
end;

{ TResourceMapping }

constructor TResourceMapping.Create;
begin
  inherited Create;
  Methods := TFPObjectList.Create(True);
end;

destructor TResourceMapping.Destroy;
begin
  Methods.Free;
  inherited Destroy;
end;

{ TCompatibilityUnitGenerator }

constructor TCompatibilityUnitGenerator.Create(aAPIData: TAPIData;
  const aServiceName: string);
begin
  inherited Create;
  FAPIData := aAPIData;
  FServiceName := aServiceName;
  FTypeMappings := TFPObjectList.Create(True);
  FResourceMappings := TFPObjectList.Create(True);
  FOutput := TStringList.Create;
  FIndent := 0;
end;

destructor TCompatibilityUnitGenerator.Destroy;
begin
  FOutput.Free;
  FResourceMappings.Free;
  FTypeMappings.Free;
  inherited Destroy;
end;

procedure TCompatibilityUnitGenerator.Log(const aMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(etInfo, aMessage);
end;

procedure TCompatibilityUnitGenerator.Log(const aFmt: string;
  const aArgs: array of const);
begin
  Log(Format(aFmt, aArgs));
end;

procedure TCompatibilityUnitGenerator.AddLine(const aLine: string);
begin
  if aLine = '' then
    FOutput.Add('')
  else
    FOutput.Add(IndentStr + aLine);
end;

procedure TCompatibilityUnitGenerator.AddFmt(const aFmt: string;
  const aArgs: array of const);
begin
  AddLine(Format(aFmt, aArgs));
end;

procedure TCompatibilityUnitGenerator.IncIndent;
begin
  Inc(FIndent, 2);
end;

procedure TCompatibilityUnitGenerator.DecIndent;
begin
  Dec(FIndent, 2);
  if FIndent < 0 then
    FIndent := 0;
end;

function TCompatibilityUnitGenerator.IndentStr: string;
begin
  Result := StringOfChar(' ', FIndent);
end;

procedure TCompatibilityUnitGenerator.SetNewUnits(const aDtoUnit, aIntfUnit,
  aImplUnit: string);
begin
  FNewDtoUnit := aDtoUnit;
  FNewIntfUnit := aIntfUnit;
  FNewImplUnit := aImplUnit;
end;

function TCompatibilityUnitGenerator.GenerateOldTypeName(
  const aSchemaName: string; aTypeData: TPascalTypeData): string;
begin
  // Old naming convention: T + SchemaName (no modifications except T prefix)
  // The old generator did not add suffixes or escape much
  Result := 'T' + aSchemaName;
end;

function TCompatibilityUnitGenerator.GenerateOldArrayTypeName(
  const aElementTypeName, aPropertyContext: string): string;
begin
  // Old naming: if in property context, use TParentTypePropertyArray
  // Otherwise use TElementTypeArray
  if aPropertyContext <> '' then
    Result := aPropertyContext + 'Array'
  else
    Result := aElementTypeName + 'Array';
end;

function TCompatibilityUnitGenerator.GenerateOldResourceName(
  const aServiceName: string): string;
begin
  // Old naming: T + ServiceName + Resource
  // e.g., Acl -> TAclResource
  Result := 'T' + aServiceName + 'Resource';
end;

procedure TCompatibilityUnitGenerator.AnalyzeTypes;
var
  I: integer;
  TypeData: TPascalTypeData;
  Mapping: TTypeMapping;
  OldName, NewName, SchemaName: string;
begin
  Log('Analyzing %d types...', [FAPIData.TypeCount]);

  for I := 0 to FAPIData.TypeCount - 1 do
  begin
    TypeData := FAPIData.Types[I];
    SchemaName := TypeData.SchemaName;

    // Skip built-in/alias types
    if TypeData.Pascaltype in [ptString, ptInteger, ptInt64, ptFloat32, ptFloat64, ptBoolean, ptDateTime] then
      Continue;

    // Skip internal array types (schema names starting with '[')
    if (Length(SchemaName) > 0) and (SchemaName[1] = '[') then
      Continue;

    // Skip types without valid schema names
    if SchemaName = '' then
      Continue;

    NewName := TypeData.PascalName;
    OldName := GenerateOldTypeName(SchemaName, TypeData);

    // Only create mapping if names differ and old name is valid
    if (OldName <> '') and not SameText(OldName, NewName) then
    begin
      Mapping := TTypeMapping.Create;
      Mapping.OldName := OldName;
      Mapping.NewName := NewName;
      Mapping.IsAlias := True;
      FTypeMappings.Add(Mapping);
      Log('Type mapping: %s -> %s', [OldName, NewName]);
    end;
  end;
end;

function StripParamPrefix(const aName: string): string;
begin
  // Convert aCalendarId -> calendarId for old-style compatibility
  if (Length(aName) > 1) and (aName[1] = 'a') and (aName[2] in ['A'..'Z']) then
    Result := LowerCase(aName[2]) + Copy(aName, 3, Length(aName))
  else
    Result := aName;
end;

procedure TCompatibilityUnitGenerator.AnalyzeServices;
var
  I, J, K: integer;
  Service: TAPIService;
  Method: TAPIServiceMethod;
  ResMapping: TResourceMapping;
  MethMapping: TMethodMapping;
  Param: TAPIServiceMethodParam;
  ParamName, ParamType: string;
begin
  Log('Analyzing %d services...', [FAPIData.ServiceCount]);

  for I := 0 to FAPIData.ServiceCount - 1 do
  begin
    Service := FAPIData.Services[I];

    ResMapping := TResourceMapping.Create;
    ResMapping.OldResourceName := GenerateOldResourceName(Service.ServiceName);
    ResMapping.NewInterfaceName := Service.ServiceInterfaceName;

    Log('Resource mapping: %s -> %s', [ResMapping.OldResourceName, ResMapping.NewInterfaceName]);

    for J := 0 to Service.MethodCount - 1 do
    begin
      Method := Service.Methods[J];

      MethMapping := TMethodMapping.Create;
      MethMapping.OldMethodName := Method.MethodName;
      MethMapping.NewMethodName := Method.MethodName;
      MethMapping.ServiceName := Service.ServiceName;
      MethMapping.ResultType := Method.ResultDtoType;
      MethMapping.HasOptionalParams := Method.HasOptionalParams;

      // Check for request body
      if Assigned(Method.RequestBodyDataType) then
      begin
        MethMapping.RequestBodyType := Method.RequestBodyDataType.PascalName;
        // Generate old-style parameter name (e.g., aAclRule for TAclRule)
        ParamName := Method.RequestBodyDataType.PascalName;
        if (Length(ParamName) > 1) and (ParamName[1] = 'T') then
          MethMapping.RequestBodyParamName := 'a' + Copy(ParamName, 2, Length(ParamName))
        else
          MethMapping.RequestBodyParamName := 'aRequest';
      end;

      // Collect parameters by location and store interface order
      // Interface signature order is:
      // 1. Non-optional parameters (no default value), alphabetically
      // 2. Request body (aRequest)
      // 3. Optional parameters (with default values), alphabetically

      // First pass: non-optional parameters
      for K := 0 to Method.ParamCount - 1 do
      begin
        Param := Method.Param[K];
        if Param.DefaultValue <> '' then
          Continue; // Skip optional params in first pass

        ParamName := StripParamPrefix(Param.Name);
        ParamType := Param.TypeName;

        // Store mapping from interface param name to old-style param name
        MethMapping.InterfaceParams.Add(Param.Name + '=' + ParamName);

        if Param.Location = plPath then
          MethMapping.PathParams.Add(Format('%s: %s', [ParamName, ParamType]))
        else
          MethMapping.QueryParams.Add(Format('%s: %s', [ParamName, ParamType]));
      end;

      // Add request body parameter (comes after non-optional params)
      if MethMapping.RequestBodyType <> '' then
        MethMapping.InterfaceParams.Add('aRequest=' + MethMapping.RequestBodyParamName);

      // Second pass: optional parameters (with default values)
      for K := 0 to Method.ParamCount - 1 do
      begin
        Param := Method.Param[K];
        if Param.DefaultValue = '' then
          Continue; // Skip non-optional params in second pass

        ParamName := StripParamPrefix(Param.Name);
        ParamType := Param.TypeName;

        // Store mapping from interface param name to old-style param name
        MethMapping.InterfaceParams.Add(Param.Name + '=' + ParamName);

        // Optional params are still query params for the old-style signature
        MethMapping.QueryParams.Add(Format('%s: %s', [ParamName, ParamType]));
      end;

      ResMapping.Methods.Add(MethMapping);
    end;

    FResourceMappings.Add(ResMapping);
  end;
end;

procedure TCompatibilityUnitGenerator.GenerateHeader;
begin
  AddLine('{ Compatibility unit for google' + LowerCase(FServiceName));
  AddLine('');
  AddLine('  This unit provides backwards compatibility with the old API style.');
  AddLine('  It re-exports types and provides resource wrapper classes that');
  AddLine('  delegate to the new interface-based implementation.');
  AddLine('');
  AddLine('  Auto-generated by discovery2pas compatibility generator.');
  AddLine('}');
  AddLine('');
  AddFmt('unit google%s;', [LowerCase(FServiceName)]);
  AddLine('');
  AddLine('{$mode objfpc}');
  AddLine('{$h+}');
  AddLine('');
end;

procedure TCompatibilityUnitGenerator.GenerateInterface;
begin
  AddLine('interface');
  AddLine('');
  AddLine('uses');
  AddLine('  SysUtils, Classes,');
  AddLine('  googlebase, googleservice,');
  AddFmt('  %s, %s, %s;', [FNewDtoUnit, FNewIntfUnit, FNewImplUnit]);
  AddLine('');
  AddLine('type');
  AddLine('');
end;

procedure TCompatibilityUnitGenerator.GenerateTypeAliases;
var
  I: integer;
  Mapping: TTypeMapping;
begin
  if FTypeMappings.Count = 0 then
    Exit;

  AddLine('  { Type aliases for backwards compatibility }');
  AddLine('');

  for I := 0 to FTypeMappings.Count - 1 do
  begin
    Mapping := TTypeMapping(FTypeMappings[I]);
    AddFmt('  %s = %s.%s;', [Mapping.OldName, FNewDtoUnit, Mapping.NewName]);
  end;

  AddLine('');
end;

procedure TCompatibilityUnitGenerator.GenerateArrayTypeAliases;
var
  I: integer;
  TypeData: TPascalTypeData;
  OldArrayName, NewArrayName: string;
begin
  AddLine('  { Array type aliases }');
  AddLine('');

  for I := 0 to FAPIData.TypeCount - 1 do
  begin
    TypeData := FAPIData.Types[I];

    if TypeData.Pascaltype = ptArray then
    begin
      NewArrayName := TypeData.PascalName;
      // Generate old-style array name
      if Assigned(TypeData.ElementTypeData) then
      begin
        OldArrayName := GenerateOldArrayTypeName(
          TypeData.ElementTypeData.PascalName, '');

        if not SameText(OldArrayName, NewArrayName) then
          AddFmt('  %s = %s.%s;', [OldArrayName, FNewDtoUnit, NewArrayName]);
      end;
    end;
  end;

  AddLine('');
end;

function TCompatibilityUnitGenerator.BuildOldStyleParamList(MethMapping: TMethodMapping): string;
var
  K: integer;
begin
  Result := '';

  // Path parameters first (required)
  for K := 0 to MethMapping.PathParams.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + '; ';
    Result := Result + MethMapping.PathParams[K];
  end;

  // Request body parameter (if any)
  if MethMapping.RequestBodyType <> '' then
  begin
    if Result <> '' then
      Result := Result + '; ';
    Result := Result + MethMapping.RequestBodyParamName + ': ' + MethMapping.RequestBodyType;
  end;

  // Query parameters (optional in old API, but required in signature here for simplicity)
  for K := 0 to MethMapping.QueryParams.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + '; ';
    Result := Result + MethMapping.QueryParams[K];
  end;
end;

procedure TCompatibilityUnitGenerator.GenerateResourceClasses;
var
  I, J: integer;
  ResMapping: TResourceMapping;
  MethMapping: TMethodMapping;
  ParamList: string;
begin
  AddLine('  { Resource wrapper classes }');
  AddLine('');

  for I := 0 to FResourceMappings.Count - 1 do
  begin
    ResMapping := TResourceMapping(FResourceMappings[I]);

    AddFmt('  %s = class(TGoogleResource)', [ResMapping.OldResourceName]);
    AddLine('  private');
    AddFmt('    FService: %s;', [ResMapping.NewInterfaceName]);
    AddLine('  public');
    AddLine('    constructor Create(AOwner: TComponent); override;');
    AddLine('    class function ResourceName: string; override;');
    AddLine('    class function DefaultAPI: TGoogleAPIClass; override;');

    // Method declarations
    for J := 0 to ResMapping.Methods.Count - 1 do
    begin
      MethMapping := TMethodMapping(ResMapping.Methods[J]);

      // Build parameter list in old-style order
      ParamList := BuildOldStyleParamList(MethMapping);

      if MethMapping.ResultType <> '' then
      begin
        if ParamList <> '' then
          AddFmt('    function %s(%s): %s;',
            [MethMapping.OldMethodName, ParamList, MethMapping.ResultType])
        else
          AddFmt('    function %s: %s;',
            [MethMapping.OldMethodName, MethMapping.ResultType]);
      end
      else
      begin
        if ParamList <> '' then
          AddFmt('    procedure %s(%s);',
            [MethMapping.OldMethodName, ParamList])
        else
          AddFmt('    procedure %s;', [MethMapping.OldMethodName]);
      end;
    end;

    AddLine('  end;');
    AddLine('');
  end;
end;

procedure TCompatibilityUnitGenerator.GenerateImplementation;
begin
  AddLine('implementation');
  AddLine('');
end;

function TCompatibilityUnitGenerator.BuildNewStyleParamCall(MethMapping: TMethodMapping): string;
var
  K, EqPos: integer;
  OldParamName: string;
begin
  Result := '';

  // Use InterfaceParams which stores parameters in interface order
  // Each entry is in format "aInterfaceName=oldStyleName"
  for K := 0 to MethMapping.InterfaceParams.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ';
    // Extract the old-style parameter name (after the equals sign)
    EqPos := Pos('=', MethMapping.InterfaceParams[K]);
    if EqPos > 0 then
      OldParamName := Copy(MethMapping.InterfaceParams[K], EqPos + 1, Length(MethMapping.InterfaceParams[K]))
    else
      OldParamName := MethMapping.InterfaceParams[K];
    Result := Result + OldParamName;
  end;
end;

procedure TCompatibilityUnitGenerator.GenerateResourceImplementations;
var
  I, J: integer;
  ResMapping: TResourceMapping;
  MethMapping: TMethodMapping;
  ParamList, ParamCall: string;
  ServiceVarName: string;
begin
  for I := 0 to FResourceMappings.Count - 1 do
  begin
    ResMapping := TResourceMapping(FResourceMappings[I]);
    ServiceVarName := Copy(ResMapping.NewInterfaceName, 2, Length(ResMapping.NewInterfaceName)); // Remove 'I' prefix

    AddLine('{ ' + ResMapping.OldResourceName + ' }');
    AddLine('');

    // Constructor
    AddFmt('constructor %s.Create(AOwner: TComponent);', [ResMapping.OldResourceName]);
    AddLine('begin');
    AddLine('  inherited Create(AOwner);');
    AddLine('  // FService := T' + ServiceVarName + 'Impl.Create(...);');
    AddLine('  // Note: Service initialization depends on your client setup');
    AddLine('end;');
    AddLine('');

    // ResourceName
    AddFmt('class function %s.ResourceName: string;', [ResMapping.OldResourceName]);
    AddLine('begin');
    AddFmt('  Result := ''%s'';', [ServiceVarName]);
    AddLine('end;');
    AddLine('');

    // DefaultAPI
    AddFmt('class function %s.DefaultAPI: TGoogleAPIClass;', [ResMapping.OldResourceName]);
    AddLine('begin');
    AddLine('  Result := nil; // Override in your application');
    AddLine('end;');
    AddLine('');

    // Method implementations
    for J := 0 to ResMapping.Methods.Count - 1 do
    begin
      MethMapping := TMethodMapping(ResMapping.Methods[J]);

      // Build old-style parameter list for declaration
      ParamList := BuildOldStyleParamList(MethMapping);
      // Build new-style parameter call for service invocation
      ParamCall := BuildNewStyleParamCall(MethMapping);

      if MethMapping.ResultType <> '' then
      begin
        if ParamList <> '' then
          AddFmt('function %s.%s(%s): %s;',
            [ResMapping.OldResourceName, MethMapping.OldMethodName, ParamList, MethMapping.ResultType])
        else
          AddFmt('function %s.%s: %s;',
            [ResMapping.OldResourceName, MethMapping.OldMethodName, MethMapping.ResultType]);
        AddLine('var');
        AddLine('  LResult: ' + MethMapping.ResultType + 'ServiceResult;');
        AddLine('begin');
        if ParamCall <> '' then
          AddFmt('  LResult := FService.%s(%s);', [MethMapping.NewMethodName, ParamCall])
        else
          AddFmt('  LResult := FService.%s;', [MethMapping.NewMethodName]);
        AddLine('  if LResult.Success then');
        AddLine('    Result := LResult.Value');
        AddLine('  else');
        AddLine('    raise EGoogleAPI.Create(LResult.ErrorText);');
        AddLine('end;');
      end
      else
      begin
        if ParamList <> '' then
          AddFmt('procedure %s.%s(%s);',
            [ResMapping.OldResourceName, MethMapping.OldMethodName, ParamList])
        else
          AddFmt('procedure %s.%s;',
            [ResMapping.OldResourceName, MethMapping.OldMethodName]);
        AddLine('begin');
        if ParamCall <> '' then
          AddFmt('  FService.%s(%s);', [MethMapping.NewMethodName, ParamCall])
        else
          AddFmt('  FService.%s;', [MethMapping.NewMethodName]);
        AddLine('end;');
      end;
      AddLine('');
    end;
  end;
end;

procedure TCompatibilityUnitGenerator.Generate(const aOutputFileName: string);
begin
  Log('Generating compatibility unit: %s', [aOutputFileName]);

  FOutput.Clear;
  FTypeMappings.Clear;
  FResourceMappings.Clear;

  // Analyze the API data
  AnalyzeTypes;
  AnalyzeServices;

  // Generate the unit
  GenerateHeader;
  GenerateInterface;
  GenerateTypeAliases;
  GenerateArrayTypeAliases;
  GenerateResourceClasses;
  GenerateImplementation;
  GenerateResourceImplementations;

  AddLine('end.');

  // Save to file
  FOutput.SaveToFile(aOutputFileName);
  Log('Compatibility unit saved to: %s', [aOutputFileName]);
end;

end.
