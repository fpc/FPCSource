program mkfpdocproj;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpdocproj, fpdocxmlopts, CustApp;

type

  { TManageFPDocProjectApplication }

  TManageFPDocProjectApplication = class(TCustomApplication)
  private
    procedure ParseOptions;
  protected
    FRecurse : boolean;
    FDirectory,
    FMask,
    FPackageName,
    FInputFileName,
    FOutputFileName : String;
    FProject : TFPDocProject;
    FPackage : TFPDocPackage;
    procedure ReadOptionFile(const AFileName: String);
    procedure Usage(AExitCode: Integer);
    procedure WriteOptionFile(const AFileName: String);
    procedure AddFilesFromDirectory(ADirectory, AMask: String; ARecurse: Boolean);
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

{ TManageFPDocProjectApplication }

procedure TManageFPDocProjectApplication.Usage(AExitCode : Integer);

begin
  // to be filled
  Halt(AExitCode);
end;

procedure TManageFPDocProjectApplication.ParseOptions;

Var
  PN : String;

begin
  FInputFileName:=GetOptionValue('i','input');
  FOutputFileName:=GetOptionValue('o','output');
  FPackageName:=GetOptionValue('p','package');
  if (FOutputFileName='') then
    FOutputFileName:=FInputFileName;
  FDirectory:=GetOptionValue('d','directory');
  FMask:=GetOptionValue('m','mask');
  FRecurse:=HasOption('r','recurse');
  if HasOption('h','help') then
    Usage(0);
end;

Procedure TManageFPDocProjectApplication.AddFilesFromDirectory(ADirectory,AMask : String; ARecurse : Boolean);

Var
  Info : TSearchRec;
  D : String;

begin
  if (AMask='') then
    AMask:='*.xml';
  D:=ADirectory;
  if (D<>'') then
    D:=includeTrailingPathDelimiter(D);
  If FindFirst(D+AMask,0,info)=0 then
    try
      Repeat
      if ((Info.Attr and faDirectory)=0) then
        FPackage.Descriptions.add(D+Info.Name);
      Until (FindNext(Info)<>0);
    finally
      FindClose(Info);
    end;
  If ARecurse and (FindFirst(ADirectory+AMask,0,info)=0) then
    try
      Repeat
      if ((Info.Attr and faDirectory)<>0) then
        AddFilesFromDirectory(IncludeTrailingPathDelimiter(D+Info.Name),AMask,ARecurse);
      Until (FindNext(Info)<>0);
    finally
      FindClose(Info);
    end;
end;

procedure TManageFPDocProjectApplication.ReadOptionFile(Const AFileName : String);

begin
  With TXMLFPDocOptions.Create(Self) do
    try
      LoadOptionsFromFile(FProject,AFileName);
    finally
      Free;
    end;
end;

procedure TManageFPDocProjectApplication.WriteOptionFile(Const AFileName : String);

begin
  With TXMLFPDocOptions.Create(Self) do
    try
      SaveOptionsToFile(FProject,AFileName);
    finally
      Free;
    end;
end;

procedure TManageFPDocProjectApplication.DoRun;

begin
  ParseOptions;
  ReadOptionFile(FInputFileName);
  FPackage:=FProject.Packages.FindPackage(FPackageName);
  If (FDirectory<>'') or (FMask<>'') then
    AddFilesFromDirectory(FDirectory,FMask, FRecurse);
  WriteOptionFile(FOutputFileName);
  Terminate;
end;

constructor TManageFPDocProjectApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FProject:=TFPDocProject.Create(Self);
end;

var
  Application: TManageFPDocProjectApplication;
begin
  Application:=TManageFPDocProjectApplication.Create(nil);
  Application.Title:='Program to manipulate FPDoc project files';
  Application.Run;
  Application.Free;
end.

