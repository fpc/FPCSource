unit fpmakeParseJSon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpmkunit,
  jsonparser, fpjson;

function ParseFpmake(AJsonData : TJSONData) : TPackages;
function ParseFpmakeFile(AFileName: string) : TPackages;

function ExtStringToOSes(AString: String) : TOSes;
function ExtOSesToString(AOSes: TOSes) : string;

function ExtStringToCPUs(AString: String) : TCpus;
function ExtCPUsToString(ACPUs: TCPUs) : string;


implementation

var GSetStrings: TStringList;

function SetStrings: TstringList;
begin
  if not assigned(GSetStrings) then
    GSetStrings := TStringList.Create;
  result := GSetStrings;
end;

function ExtStringToOSes(AString: String) : TOSes;
var
  i: dword;
begin
  try
    result := OSesToString(AString);
  except
    i := SetStrings.Add(AString)+1;
    result:=TOSes(dword(dword(AllOSes)+dword(i)));
  end;
end;

function ExtOSesToString(AOSes: TOSes) : string;
var
  i: dword;
begin
  if DWord(AOSes) < DWord(AllOSes) then
    result := '[' + OSesToString(AOSes) + ']'
  else
    begin
    i := (dword(AOSes) - dword(AllOSes)) -1;
    if i < SetStrings.Count then
      result := SetStrings[i]
    else
      raise exception.Create('Invalid set of OSes.');
    end;
end;

function ExtStringToCPUs(AString: String) : TCpus;
var
  i: dword;
begin
  try
    result := StringToCPUS(AString);
  except
    i := SetStrings.Add(AString)+1;
    result:=TCPUS(dword(dword(AllCPUs)+dword(i)));
  end;
end;

function ExtCPUsToString(ACPUs: TCPUs) : string;
var
  i: dword;
begin
  if DWord(ACPUs) < DWord(AllCPUs) then
    result := '[' + CPUSToString(ACPUs) + ']'
  else
    begin
    i := (dword(ACPUs) - dword(AllCPUs)) -1;
    if i < SetStrings.Count then
      result := SetStrings[i]
    else
      raise exception.Create('Invalid set of CPUs.');
    end;
end;

procedure ParseConditionalString(ADependency: TConditionalString; AJsonData: TJSonData; ValueCaption: string);
var
  AJsonObject: TJSONObject;
  m: Integer;
begin

  if AJsonData.JSONType = jtString then
    begin
    ADependency.Value := AJsonData.AsString;
    end
  else if AJsonData.JSONType = jtObject then
    begin
    AJsonObject := AJsonData as TJSONObject;

    for m := 0 to AJsonObject.Count-1 do
      begin
      case AJsonObject.Names[m] of
        'oses'       : ADependency.oses := ExtStringToOSes(AJsonObject.Items[m].AsString);
        'cpus'       : ADependency.CPUs := ExtStringToCPUS(AJsonObject.Items[m].AsString);
      else if AJsonObject.Names[m] = ValueCaption then
        ADependency.Value := AJsonObject.Items[m].AsString
      else
        raise Exception.CreateFmt('Unknown conditional property ''%s''.',[AJsonObject.Names[m]]);
      end {case}
      end;

    end
  else
    raise Exception.CreateFmt('Invalid conditional. (%s)',[AJsonData.AsString]);
end;

procedure ParseConditionalArray(ACondStrings: TConditionalStrings; AJsonData: TJSonData; ValueCaption: string);
var
  AJSonArray: TJSONArray;
  n: Integer;
begin
  if AJsonData.JSONType = jtArray then
    begin
    AJSonArray := AJsonData as TJSONArray;
    for n := 0 to AJSonArray.Count-1 do
      ParseConditionalString(ACondStrings.add(''), AJSonArray.Items[n], ValueCaption);
    end
  else
    ParseConditionalString(ACondStrings.add(''), AJsonData, ValueCaption);
end;

procedure ParseDependenciesArray(ACondStrings: TDependencies; AJsonData: TJSonData; ValueCaption: string; aDepType: TDependencyType);
var
  AJSonArray: TJSONArray;
  n: Integer;

  function GetDep: TDependency;
  begin
    if aDepType=depInclude then
      result := ACondStrings.AddInclude('')
    else if aDepType=depUnit then
      result := ACondStrings.AddUnit('')
    else
      result := ACondStrings.Add('');
  end;

begin
  if AJsonData.JSONType = jtArray then
    begin
    AJSonArray := AJsonData as TJSONArray;
    for n := 0 to AJSonArray.Count-1 do
      ParseConditionalString(GetDep, AJSonArray.Items[n], ValueCaption);
    end
  else
    ParseConditionalString(GetDep, AJsonData, ValueCaption);
end;

procedure ParseDependencies(aDependencies: TDependencies; aJSONData: TJSONData);
var
  AJsonObject: TJSONObject;
  m: Integer;
begin
  if aJSONData.JSONType<>jtObject then
    raise exception.create('A target''s dependency has to be an object which encapsulated the different types of dependencies.')
  else
    begin
    AJsonObject := aJSONData as TJSONObject;
    for m := 0 to AJsonObject.Count-1 do
      begin
      case AJsonObject.Names[m] of
        'includefiles'    : ParseDependenciesArray(aDependencies, AJsonObject.items[m],'filename', depInclude);
        'units'           : ParseDependenciesArray(aDependencies, AJsonObject.items[m],'filename', depUnit);
      else
        raise Exception.CreateFmt('Unknown dependency property ''%s''.',[AJsonObject.Names[m]]);
      end {case}
      end;
    end
end;

procedure ParseUnitTarget(aTarget: TTarget; aJSONData: TJSONData);
var
  AJsonObject: TJSONObject;
  m: Integer;
begin
  if aJSONData.JSONType=jtString then
    aTarget.Name := aJSONData.AsString
  else if aJSONData.JSONType=jtObject then
    begin
    AJsonObject := aJSONData as TJSONObject;
    for m := 0 to AJsonObject.Count-1 do
      begin
      case AJsonObject.Names[m] of
        'name'            : aTarget.name := AJsonObject.items[m].asstring;
        'resourcestrings' : atarget.ResourceStrings := (AJsonObject.items[m] as TJSONBoolean).AsBoolean;
        'oses'            : aTarget.OSes := ExtStringToOSes(AJsonObject.Items[m].AsString);
        'cpus'            : aTarget.cpus := ExtStringToCPUs(AJsonObject.Items[m].AsString);
        'dependencies'    : ParseDependencies(aTarget.Dependencies, AJsonObject.Items[m]);
      else
        raise Exception.CreateFmt('Unknown targets property ''%s''.',[AJsonObject.Names[m]]);
      end {case}
      end;
    end
  else
    raise Exception.CreateFmt('Invalid target ''%s''',[aJSONData.AsString]);
end;

procedure ParseUnitTargets(aTargets: TTargets; aJSONData: TJSONData);
var
  AJsonArray: TJSONArray;
  AJsonObject: TJSONObject;
  AResourceStrings: boolean;
  ACPUs: TCPUS;
  AOSes: TOSes;
  m: Integer;
  LastTargetItem: integer;
  ATarget: ttarget;
begin
  if aJSONData.JSONType=jtArray then
    begin
    AJsonArray := aJSONData as TJSONArray;
    for m := 0 to AJsonArray.Count-1 do
      ParseUnitTarget(aTargets.AddUnit(''), AJsonArray.Items[m]);
    end
  else if aJSONData.JSONType=jtObject then
    begin
    AJsonObject := aJSONData as TJSONObject;
    AresourceStrings:=false;
    ACpus:=AllCPUs;
    AOses:=AllOSes;
    LastTargetItem:=aTargets.Count;
    for m := 0 to AJsonObject.Count-1 do
      begin
      case AJsonObject.Names[m] of
        'resourcestrings' : AresourceStrings := (AJsonObject.items[m] as TJSONBoolean).AsBoolean;
        'cpus'            : ACPUs := ExtStringToCPUs(AJsonObject.items[m].AsString);
        'oses'            : AOSes := ExtStringToOSes(AJsonObject.items[m].AsString);
        'targets'         : ParseUnitTargets(aTargets, AJsonObject.items[m])
      else
        raise Exception.CreateFmt('Unknown targets property ''%s''.',[AJsonObject.Names[m]]);
      end {case}
      end;
    for m := LastTargetItem to aTargets.Count-1 do
      begin
      aTarget := aTargets.Items[m] as TTarget;
      if AresourceStrings then
        aTarget.ResourceStrings := AresourceStrings;
      if ACPUs<>AllCPUs then
        ATarget.CPUs := ACpus;
      if AOSes<>AllOSes then
        ATarget.OSes := AOses;
      end;
    end
  else
    raise Exception.CreateFmt('Invalid unit target ''%s''',[aJSONData.AsString]);
end;

procedure ParseTargets(aTargets: TTargets; aJSONData: TJSONData);
var
  AJsonObject: TJSONObject;
  m: Integer;
begin
  if aJSONData.JSONType<>jtObject then
    raise Exception.Create('Invalid targets');
  AJsonObject := aJSONData as TJSONObject;
  for m := 0 to AJsonObject.Count-1 do
    begin
    case AJsonObject.Names[m] of
      'units' : ParseUnitTargets(aTargets, AJsonObject.items[m]);
    else
      raise Exception.CreateFmt('Unknown targets property ''%s''.',[AJsonObject.Names[m]]);
    end {case}
    end;
end;

function ParseFpmakeFile(AFileName: string) : TPackages;
var
  AJsonData : TJSONData;
  F : TFileStream;
  P: TJSONParser;
begin
  result := nil;
  // Parse the JSON-file
  F:=TFileStream.Create(AFileName,fmopenRead);
  try
    P:=TJSONParser.Create(F);
    try
      try
        AJsonData := P.Parse;
      except
        on E: Exception do
          begin
          writeln(Format('Error: Syntax of JSON-file %s is incorrect (%s)', [AFileName, e.Message]));
          exit;
          end;
      end;
    finally
      P.Free;
    end;
  finally
    F.Free;
  end;

  try
    result := ParseFpmake(AJsonData);
  except
    on E: Exception do
      begin
      writeln(Format('Error, problem in file %s: %s',[AFileName,e.Message]));
      exit;
      end;
  end;

end;

function ParseFpmake(AJsonData : TJSONData) : TPackages;
Var
  P : TJSONParser;

  MainObject : TJSONObject;
  ItemObject : TJSONObject;
  i: Integer;
  APackages: TPackages;
  n: Integer;

  procedure AddDependencies(APackage: TPackage; AJsonDependencies: TJSONData);
  var
    AJsonDependenciesObj: TJSONObject;
    m,n,o: Integer;
    ADependency: TDependency;
  begin
    if AJsonDependencies.JSONType<>jtObject then
      raise Exception.CreateFmt('Invalid dependencies for package %s',[APackage.Name]);
    AJsonDependenciesObj := AJsonDependencies as TJSONObject;
    for m := 0 to AJsonDependenciesObj.Count-1 do
      begin
      case AJsonDependenciesObj.Names[m] of
        'packages' : ParseConditionalArray(APackage.Dependencies, AJsonDependenciesObj.items[m],'name');
      else
        raise Exception.CreateFmt('Unknown dependency property ''%s''.',[ItemObject.Names[m]]);
      end {case}
      end;
  end;

var
  APackage: TPackage;

begin
  // Convert the JSON-Data to a packages-class
  if AJsonData.JSONType <> jtObject then
    raise Exception.Create('File does not contain any objects.');

  APackages := TPackages.Create(TPackage);
  try
    MainObject := AJsonData as TJSONObject;
    for i := 0 to MainObject.Count-1 do
      begin
      if MainObject.Names[i]='package' then
        begin
        AJsonData := MainObject.Items[i];
        if (AJsonData.JSONType <> jtObject) then
          raise Exception.Create('File does not contain any objects.');
        ItemObject := AJsonData as TJSONObject;

        APackage := APackages.AddPackage('');
        for n := 0 to ItemObject.Count-1 do
          begin
          case ItemObject.Names[n] of
            'title'        : APackage.Name := ItemObject.items[n].AsString;
            'directory'    : APackage.Directory := ItemObject.items[n].AsString;
            'version'      : APackage.version := ItemObject.items[n].AsString;
            'oses'         : APackage.OSes := ExtStringToOSes(ItemObject.items[n].AsString);
            'cpus'         : APackage.CPUs := ExtStringToCPUs(ItemObject.items[n].AsString);
            'dependencies' : AddDependencies(APackage, ItemObject.items[n]);
            'sourcepaths'  : ParseConditionalArray(APackage.SourcePath, ItemObject.items[n],'path');
            'targets'      : ParseTargets(APackage.Targets, ItemObject.items[n]);
            'email'        : APackage.Email := ItemObject.items[n].AsString;
            'author'       : APackage.Author := ItemObject.items[n].AsString;
            'license'      : APackage.License := ItemObject.items[n].AsString;
            'homepageurl'  : APackage.HomepageURL := ItemObject.items[n].AsString;
            'description'  : APackage.Description := ItemObject.items[n].AsString;
            'needlibc'     : APackage.NeedLibC := (ItemObject.items[n] as TJSONBoolean).AsBoolean;
          else
            raise Exception.CreateFmt('Unknown package property ''%s''.',[ItemObject.Names[n]]);
          end {case}
          end;

        end;
      end;
    Result := APackages;
  except
    APackages.Free;
    raise;
  end;
end;

initialization
  GSetStrings := nil;
finalization
  GSetStrings.Free;
end.

