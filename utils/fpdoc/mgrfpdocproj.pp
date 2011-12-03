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
  protected
    Procedure CheckPackage;
    procedure GetItemsFromDirectory(AList: TStrings; ADirectory, AMask: String; ARecurse: Boolean);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    procedure AddDescrFilesFromDirectory(Const ADirectory, AMask : String; ARecurse: Boolean);
    procedure AddInputFilesFromDirectory(Const ADirectory, AMask, AOptions: String; ARecurse: Boolean);
    procedure AddInputFile(Const AFile : String; AOptions : String = '');
    procedure AddDescrFile(Const AFile : String);
    procedure RemoveInputFile(Const AFile : String);
    procedure RemoveDescrFile(Const AFile : String);
    procedure WriteOptionFile(const AFileName: String);
    procedure ReadOptionFile(const AFileName: String; AMacros : TStrings = Nil);
    Procedure Selectpackage(Const APackageName : String);
    Procedure AddPackage (Const APackageName : String);
    procedure SetOption(Const AOption : String; Enable : Boolean = True);
    Property Project : TFPDocProject Read FProject;
    Property SelectedPackage : TFPDocPackage Read FPackage;
  end;

implementation
Procedure TFPDocProjectManager.GetItemsFromDirectory(AList : TStrings; ADirectory,AMask : String; ARecurse : Boolean);

Var
  D : String;
  Info : TSearchRec;

begin
  D:=ADirectory;
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
end;

destructor TFPDocProjectManager.Destroy;
begin
  FreeAndNil(FProject);
  inherited Destroy;
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
    GetItemsFromDirectory(L,ADirectory,AMask,ARecurse);
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

procedure TFPDocProjectManager.AddDescrFile(const AFile: String);

begin
  CheckPackage;
  if FPackage.Descriptions.IndexOf(AFile)<>-1 then
    Raise Exception.Createfmt('Duplicate description file : "%s"',[AFile]);
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

procedure TFPDocProjectManager.ReadOptionFile(Const AFileName : String; AMacros : TStrings = Nil);

begin
  With TXMLFPDocOptions.Create(Self) do
    try
      if (AMacros<>Nil) then
        begin
        Macros.Assign(AMacros);
        ExpandMacros:=true;
        end;
      LoadOptionsFromFile(FProject,AFileName);
    finally
      Free;
    end;
end;

procedure TFPDocProjectManager.Selectpackage(const APackageName: String);
begin
  FPackage:=FProject.Packages.FindPackage(APackageName);
  If (FPackage=Nil) then
    Raise Exception.CreateFmt('Unknown package : "%s"',[APackageName]);
end;

procedure TFPDocProjectManager.AddPackage(const APackageName: String);
begin
  if FProject.Packages.FindPackage(APackageName)<>Nil then
    Raise Exception.CreateFmt('Duplicate package : "%s"',[APackageName]);
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
    Raise Exception.Create('Error: No package selected');
end;



end.

