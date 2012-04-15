unit mgrfpdocproj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpdocproj, fpdocxmlopts;

Type
  { TFPDocProjectManager }

  TFPDocProjectManager = Class(TComponent)
  Private
    FProject : TFPDocProject;
    FPackage : TFPDocPackage;
    FExpandMacros: Boolean;
    FMacros: TStrings;
    procedure SetMacros(AValue: TStrings);
  protected
    Procedure CheckPackage;
    procedure GetItemsFromDirectory(AList: TStrings; ADirectory, AMask: String; ARecurse: Boolean);
    procedure DoMacro(Sender: TObject; const TagString: String; TagParams: TStringList; out ReplaceText: String); virtual;
    function ExpandMacrosInFile(AFileName: String): TStream; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    procedure AddDescrFilesFromDirectory(Const ADirectory, AMask : String; ARecurse: Boolean);
    procedure AddInputFilesFromDirectory(Const ADirectory, AMask, AOptions: String; ARecurse: Boolean);
    procedure AddInputFile(Const AFile : String; AOptions : String = '');
    procedure AddImportFile(Const AFile,APrefix : String);
    procedure AddDescrFile(Const AFile : String);
    procedure RemoveInputFile(Const AFile : String);
    procedure RemoveDescrFile(Const AFile : String);
    procedure WriteOptionFile(const AFileName: String);
    procedure ReadOptionFile(const AFileName: String);
    Procedure Selectpackage(Const APackageName : String);
    Procedure AddPackage (Const APackageName : String);
    procedure SetOption(Const AOption : String; Enable : Boolean = True);
    Property Project : TFPDocProject Read FProject;
    Property SelectedPackage : TFPDocPackage Read FPackage;
    Property Macros : TStrings Read FMacros Write SetMacros;
    Property ExpandMacros : Boolean Read FExpandMacros Write FExpandMacros;
  end;
  EMgrFPDoc = Class(Exception);

implementation

uses dom,xmlread,fptemplate;

procedure TFPDocProjectManager.SetMacros(AValue: TStrings);
begin
  if FMacros=AValue then Exit;
  FMacros.Assign(AValue);
end;

procedure TFPDocProjectManager.DoMacro(Sender: TObject; const TagString: String;
  TagParams: TStringList; out ReplaceText: String);
begin
  ReplaceText:=FMacros.Values[TagString];
end;


Procedure TFPDocProjectManager.GetItemsFromDirectory(AList : TStrings; ADirectory,AMask : String; ARecurse : Boolean);

Var
  D : String;
  Info : TSearchRec;

begin
  D:=ADirectory;
  if (D='.') then
    D:='';
  if (D<>'') then
    D:=includeTrailingPathDelimiter(D);
  If FindFirst(D+AMask,0,info)=0 then
    try
      Repeat
      if ((Info.Attr and faDirectory)=0) then
        AList.add(D+Info.Name);
      Until (FindNext(Info)<>0);
    finally
      FindClose(Info);
    end;
  If ARecurse and (FindFirst(ADirectory+AMask,0,info)=0) then
    try
      Repeat
      if ((Info.Attr and faDirectory)<>0) then
        GetItemsFromDirectory(Alist,IncludeTrailingPathDelimiter(D+Info.Name),AMask,ARecurse);
      Until (FindNext(Info)<>0);
    finally
      FindClose(Info);
    end;
end;

constructor TFPDocProjectManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProject:=TFPDocProject.Create(Self);
  FMacros:=TStringList.Create;
end;

destructor TFPDocProjectManager.Destroy;
begin
  FreeAndNil(FMacros);
  FreeAndNil(FProject);
  inherited Destroy;
end;

Function TFPDocProjectManager.ExpandMacrosInFile(AFileName : String) : TStream;

Var
  F : TFileStream;
  T : TTemplateParser;

begin
  F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    Result:=TMemoryStream.Create;
    try
      T:=TTemplateParser.Create;
      try
        T.StartDelimiter:='$(';
        T.EndDelimiter:=')';
        T.AllowTagParams:=true;
        T.OnReplaceTag:=@DoMacro;
        T.ParseStream(F,Result);
      finally
        T.Free;
      end;
      Result.Position:=0;
    except
      FreeAndNil(Result);
      Raise;
    end;
  finally
    F.Free;
  end;
end;

Procedure TFPDocProjectManager.AddDescrFilesFromDirectory(const ADirectory,AMask : String; ARecurse : Boolean);

Var
  L : TStringList;
  M : String;

begin
  CheckPackage;
  M:=AMask;
  if (M='') then
    M:='*.xml';
  L:=TStringList.Create;
  try
    GetItemsFromDirectory(L,ADirectory,M,ARecurse);
    FPackage.Descriptions.AddStrings(L);
  finally
    L.Free;
  end;
end;

Procedure TFPDocProjectManager.AddInputFilesFromDirectory(Const ADirectory,AMask,AOptions : String; ARecurse : Boolean);

Var
  L : TStringList;
  I : integer;
  M : String;

begin
  CheckPackage;
  M:=AMask;
  if (M='') then
    M:='*.pp';
  L:=TStringList.Create;
  try
    GetItemsFromDirectory(L,ADirectory,M,ARecurse);
    For I:=0 to L.Count-1 do
      AddInputFile(L[i],AOPtions);
  finally
    L.Free;
  end;
end;

procedure TFPDocProjectManager.AddInputFile(const AFile: String; AOptions : String = '');

Var
  S : String;

begin
  CheckPackage;
  S:=AFile;
  If (AOptions<>'') then
    S:=AOptions+' '+S;
  FPackage.Inputs.Add(S);
end;

procedure TFPDocProjectManager.AddImportFile(const AFile, APrefix: String);

begin
  CheckPackage;
  FPackage.Imports.Add(AFile+','+APrefix);
end;

procedure TFPDocProjectManager.AddDescrFile(const AFile: String);

begin
  CheckPackage;
  if FPackage.Descriptions.IndexOf(AFile)<>-1 then
    Raise EMgrFPDoc.Createfmt('Duplicate description file : "%s"',[AFile]);
  FPackage.Descriptions.Add(AFile);
end;

procedure TFPDocProjectManager.RemoveInputFile(const AFile: String);

Var
  I : Integer;

begin
  I:=FPackage.Inputs.IndexOf(AFile);
  If (I<>-1) then
    FPackage.Inputs.Delete(I);
end;

procedure TFPDocProjectManager.RemoveDescrFile(const AFile: String);

Var
  I : Integer;

begin
  I:=FPackage.Descriptions.IndexOf(AFile);
  If (I<>-1) then
    FPackage.Descriptions.Delete(I);
end;

procedure TFPDocProjectManager.ReadOptionFile(Const AFileName : String);

Var
  XML : TXMLDocument;
  S : TStream;

begin
  With TXMLFPDocOptions.Create(Self) do
    try
      if not (ExpandMacros) then
        LoadOptionsFromFile(FProject,AFileName)
      else
        begin
        S:=ExpandMacrosInFile(AFileName);
        try
          ReadXMLFile(XML,S,AFileName);
          try
            LoadFromXml(FProject,XML)
          finally
            XML.Free;
          end;
        finally
          S.Free;
        end;
        end;
    finally
      Free;
    end;
end;

procedure TFPDocProjectManager.Selectpackage(const APackageName: String);
begin
  FPackage:=FProject.Packages.FindPackage(APackageName);
  If (FPackage=Nil) then
    Raise EMgrFPDoc.CreateFmt('Unknown package : "%s"',[APackageName]);
end;

procedure TFPDocProjectManager.AddPackage(const APackageName: String);
begin
  if FProject.Packages.FindPackage(APackageName)<>Nil then
    Raise EMgrFPDoc.CreateFmt('Duplicate package : "%s"',[APackageName]);
  FPackage:=FProject.Packages.Add as TFPDocPackage;
  FPackage.Name:=APackageName;
end;

procedure TFPDocProjectManager.SetOption(const AOption: String;
  Enable: Boolean = true);

Var
  O,V : String;
  P : Integer;
  EO : TEngineOptions;

begin
  V:=LowerCase(AOption);
  P:=Pos('=',V);
  If (P=0) then
    P:=Length(V)+1;
  O:=Copy(V,1,P-1);
  Delete(V,1,P);
  EO:=FProject.Options;
  Case IndexOfString(o,OptionNames) of
    0 : EO.HideProtected:=Enable;
    1 : EO.WarnNoNode:=Enable;
    2 : EO.ShowPrivate:=Enable;
    3 : EO.StopOnParseError:=Enable;
    4 : EO.ostarget:=v;
    5 : EO.cputarget:=v;
    6 : EO.MoDir:=V;
    7 : EO.InterfaceOnly:=Not Enable;
    8 : EO.Backend:=V;
    9 : EO.Language:=v;
    10 : EO.DefaultPackageName:=V;
    11 : EO.DontTrim:=Enable;
  else
    EO.BackendOptions.add('--'+O);
    EO.BackendOptions.add(v);
  end;
end;

procedure TFPDocProjectManager.WriteOptionFile(Const AFileName : String);

begin
  With TXMLFPDocOptions.Create(Self) do
    try
      SaveOptionsToFile(FProject,AFileName);
    finally
      Free;
    end;
end;

procedure TFPDocProjectManager.CheckPackage;

begin
  if (FPackage=Nil) then
    Raise EMgrFPDoc.Create('Error: No package selected');
end;



end.

