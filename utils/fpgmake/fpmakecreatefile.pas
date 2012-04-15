unit fpmakecreatefile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpmakeParseJSon,
  fpTemplate,
  fpmkunit;

procedure CreateFile(AOutputFileName: string; ATemplate: TStringList; APackages: TPackages; ASkipBackup, ACreateDir: boolean);
function TemplateParser: TTemplateParser;

implementation

type

  { TfpmakeTemplateParser }

  TfpmakeTemplateParser = class(TTemplateParser)
  public
    constructor Create;
    Procedure OnGetParamProc(Sender : TObject; Const TagString : String; TagParams:TStringList; Out ReplaceText : String);
  end;

var
  GTemplateParser: TTemplateParser;

resourcestring
  SErrDelBackupFailed = 'Error: Delete of old backup file "%s" failed.';
  SErrCreateDirFailed = 'Error: Could not create the directory for file "%s".';
  SErrNoSuchDirectory = 'Error: Directory of file "%s" does not exists. User -p to force creation.';
  SErrBackupFailed    = 'Error: Backup of file "%s" to "%s" failed.';
  SBackupCreated      = 'Saved old "%s" to "%s"';


function GetConditionalAdd(const Value: string; CPUs: TCPUS; OSes: TOSes; const AddName: string): string;
begin
  if (CPUs <> AllCPUs) and (OSes <> AllOSes) then
    result := result + '    '+AddName+'('''+Value+''','+ExtCPUsToString(CPUs)+','+ExtOSesToString(OSes)+');' + LineEnding
  else if (CPUs <> AllCPUs) then
    result := result + '    '+AddName+'('''+Value+''','+ExtCPUsToString(CPUs)+');' + LineEnding
  else if (OSes <> AllOSes) then
    result := result + '    '+AddName+'('''+Value+''','+ExtOSesToString(OSes)+');' + LineEnding
  else
    result := result + '    '+AddName+'('''+Value+''');' + LineEnding;
end;

function GetConditionalStringsMacro(ACondStrings: TConditionalStrings; APropName: string): string;
var
  ADependency: TConditionalString;
  i: Integer;
begin
  if ACondStrings.Count=0 then
    Exit;
  if ACondStrings.Count=1 then
    begin
    ADependency := ACondStrings[0];
    result := result + GetConditionalAdd(ADependency.Value, ADependency.CPUs, ADependency.OSes,APropName+'.Add');
    end
  else
    begin
    result := '    with '+APropName+' do' + LineEnding +
              '      begin'+LineEnding;
    for i := 0 to ACondStrings.Count-1 do
      begin
      ADependency := ACondStrings[i];
      result := result + GetConditionalAdd(ADependency.Value, ADependency.CPUs, ADependency.OSes,'  Add');
      end;
    result := result +
              '      end;' + LineEnding;

    end;
end;


function GetConditionalPackagePropertiesMacro(APackage: TPackage): string;
begin
  result := '';
  if APackage.CPUs<>AllCPUs then
    result := result + '    P.CPUs := '+ExtCPUSToString(APackage.CPUs)+';'+LineEnding;
  if APackage.OSes<>AllOSes then
    result := result + '    P.OSes := '+ExtOSesToString(APackage.OSes)+';'+LineEnding;
end;

function GetTargetsMacro(aTargets: TTargets): string;
var
  ATarget: TTarget;
  i: Integer;
  d: integer;
begin
  if aTargets.Count=0 then
    Exit;
  result := '    with P.Targets do' + LineEnding +
            '      begin'+LineEnding;
  for i := 0 to aTargets.Count-1 do
    begin
    ATarget := aTargets.Items[i] as TTarget;
    result := result + GetConditionalAdd(ATarget.Name + ATarget.Extension, ATarget.CPUs, ATarget.OSes,'  T := AddUnit');
    if atarget.ResourceStrings then
      result := result + '      T.Resourcestrings := True;'+LineEnding;
    for d := 0 to aTarget.Dependencies.Count-1 do
      begin
      if ATarget.Dependencies[d].DependencyType=depInclude then
        result := result + '      T.Dependencies.AddInclude('''+ATarget.Dependencies[d].Value+''');'+LineEnding
      else if ATarget.Dependencies[d].DependencyType=depUnit then
        result := result + '      T.Dependencies.AddUnit('''+ATarget.Dependencies[d].Value+''');'+LineEnding
      else
        result := result + '      T.Dependencies.Add('''+ATarget.Dependencies[d].Value+''');'+LineEnding;
      end;
    end;
  result := result +
            '      end;';
end;


procedure CreateFile(AOutputFileName: string; ATemplate: TStringList; APackages: TPackages; ASkipBackup, ACreateDir: boolean);

Var
  Fout : Text;
  S,BFN : String;
  I : Integer;
  PackageNr: Integer;
  APackage: TPackage;

begin
  If (AOutputFileName<>'')
     and FileExists(AOutputFileName)
     and not ASkipBackup then
    begin
    BFN:=ChangeFileExt(AOutputFileName,'.bak');
    If FileExists(BFN) and not DeleteFile(BFN) then
      begin
      Writeln(StdErr,Format(SErrDelBackupFailed,[BFN]));
      Halt(1);
      end;
    If not RenameFile(AOutputFileName,BFN) then
      begin
      Writeln(StdErr,Format(SErrBackupFailed,[AOutputFileName,BFN]));
      Halt(1);
      end
    else
      Writeln(Format(SBackupCreated,[ExtractFileName(AOutputFileName),ExtractFileName(BFN)]));
    end;
  if (AOutputFileName<>'') and (ExtractFilePath(AOutputFileName)<>'') and not DirectoryExists(ExtractFilePath(AOutputFileName)) then
    begin
    if ACreateDir then
      begin
      if not ForceDirectories(ExtractFilePath(AOutputFileName)) then
        begin
        Writeln(StdErr,Format(SErrCreateDirFailed,[AOutputFileName]));
        Halt(1);
        end;
      end
    else
      begin
      Writeln(StdErr,Format(SErrNoSuchDirectory,[AOutputFileName]));
      Halt(1);
      end;
    end;
  Assign(Fout,AOutputFileName);
  Rewrite(FOut);
  Try
    for PackageNr := 0 to APackages.Count-1 do
      begin
      APackage := APackages.Items[PackageNr] as TPackage;

      TemplateParser.Values['packagename'] := APackage.Name;
      TemplateParser.Values['directory'] := APackage.Directory;
      TemplateParser.Values['version'] := APackage.Version;
      TemplateParser.Values['author'] := APackage.Author;
      TemplateParser.Values['license'] := APackage.License;
      TemplateParser.Values['homepageurl'] := APackage.HomepageURL;
      TemplateParser.Values['downloadurl'] := APackage.DownloadURL;
      TemplateParser.Values['email'] := APackage.Email;
      TemplateParser.Values['description'] := APackage.Description;
      TemplateParser.Values['needlibc'] := BoolToStr(APackage.NeedLibC,'true','false');
      TemplateParser.Values['conditionalpackageproperties'] := GetConditionalPackagePropertiesMacro(APackage);
      TemplateParser.Values['packagedependencies'] := GetConditionalStringsMacro(APackage.Dependencies, 'P.Dependencies');
      TemplateParser.Values['packagesourcepaths'] := GetConditionalStringsMacro(APackage.SourcePath, 'P.SourcePath');
      TemplateParser.Values['targets'] := GetTargetsMacro(APackage.Targets);

      For I:=0 to ATemplate.Count-1 do
        begin
        S:=ATemplate[i];
        S := TemplateParser.ParseString(S);
        Writeln(FOut,S);
        end;

      end;
  Finally
    Close(Fout);
  end;
end;

function TemplateParser: TTemplateParser;
begin
  if not assigned(GTemplateParser) then
    begin
    GTemplateParser := TfpmakeTemplateParser.Create;
    GTemplateParser.StartDelimiter:='%';
    GTemplateParser.EndDelimiter:='%';
    GTemplateParser.ParamStartDelimiter:='(';
    GTemplateParser.ParamEndDelimiter:=')';
    GTemplateParser.Values['PWD'] := GetCurrentDir;
    GTemplateParser.Values['BUILDDATE'] := DateToStr(Date);
    GTemplateParser.Values['BUILDTIME'] := TimeToStr(Time);
    end;
  result := GTemplateParser;
end;

{ TfpmakeTemplateParser }

constructor TfpmakeTemplateParser.Create;
begin
  inherited create;
  AllowTagParams := True;
  OnReplaceTag := @OnGetParamProc;
end;

procedure TfpmakeTemplateParser.OnGetParamProc(Sender : TObject; Const TagString : String; TagParams:TStringList; Out ReplaceText : String);
var
  i: Integer;
  s: string;
begin
  if TagString = 'quotedstr' then
    begin
    i := TagParams.Count;
    ReplaceText:='';
    for i := 0 to TagParams.Count-1 do
      begin
      GetParam(TagParams[i],s);
      ReplaceText:=ReplaceText + quotedstr(s);
      end;
    end
  else
    GetParam(TagString,ReplaceText);
end;

initialization
  GTemplateParser := nil
finalization
  GTemplateParser.Free;
end.

