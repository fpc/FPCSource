unit mkfpdoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglobals, fpdocxmlopts, dwriter, pscanner, pparser, fpdocproj;

const
  DefOSTarget    = {$I %FPCTARGETOS%};
  DefCPUTarget   = {$I %FPCTARGETCPU%};
  DefFPCVersion  = {$I %FPCVERSION%};
  DefFPCDate     = {$I %FPCDATE%};

Type

  { TFPDocCreator }

  TFPDocCreator = Class(TComponent)
  Private
    FOnLog: TPasParserLogHandler;
    FPParserLogEvents: TPParserLogEvents;
    FProject : TFPDocProject;
    FScannerLogEvents: TPScannerLogEvents;
    FVerbose: Boolean;
    function GetOptions: TEngineOptions;
    function GetPackages: TFPDocPackages;
  Protected
    procedure SetVerbose(AValue: Boolean); virtual;
    Procedure DoLog(Const Msg : String);
    procedure DoLog(Const Fmt : String; Args : Array of Const);
    procedure CreateOutput(APackage: TFPDocPackage; Engine: TFPDocEngine); virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure CreateDocumentation(APackage : TFPDocPackage; ParseOnly : Boolean); virtual;
    Procedure CreateProjectFile(Const AFileName : string);
    Procedure LoadProjectFile(Const AFileName: string);
    Property Project : TFPDocProject Read FProject;
    Property ScannerLogEvents : TPScannerLogEvents Read FScannerLogEvents Write FScannerLogEvents;
    Property ParserLogEvents : TPParserLogEvents Read FPParserLogEvents Write FPParserLogEvents;
    Property Verbose : Boolean Read FVerbose Write SetVerbose;
    Property OnLog : TPasParserLogHandler Read FOnLog Write FOnLog;
    // Easy access
    Property Options : TEngineOptions Read GetOptions;
    Property Packages : TFPDocPackages Read GetPackages;

  end;

implementation

{ TFPDocCreator }

procedure TFPDocCreator.SetVerbose(AValue: Boolean);
begin
  if FVerbose=AValue then Exit;
  FVerbose:=AValue;
  if FVerbose then
    begin
    ScannerLogEvents:=[sleFile];
    ParserLogEvents:=[];
    end
  else
    begin
    ScannerLogEvents:=[];
    ParserLogEvents:=[];
    end;
end;

procedure TFPDocCreator.DoLog(const Msg: String);
begin
  If Assigned(OnLog) then
    OnLog(Self,Msg);
end;

procedure TFPDocCreator.DoLog(const Fmt: String; Args: array of const);
begin
  DoLog(Format(Fmt,Args));
end;

function TFPDocCreator.GetOptions: TEngineOptions;
begin
  Result:=FProject.Options;
end;

function TFPDocCreator.GetPackages: TFPDocPackages;
begin
  Result:=FProject.Packages;
end;

constructor TFPDocCreator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProject:=TFPDocProject.Create(Self);
  FProject.Options.StopOnParseError:=False;
  FProject.Options.CPUTarget:=DefCPUTarget;
  FProject.Options.OSTarget:=DefOSTarget;
end;

destructor TFPDocCreator.Destroy;
begin
  FreeAndNil(FProject);
  inherited Destroy;
end;

procedure TFPDocCreator.CreateOutput(APackage: TFPDocPackage;Engine : TFPDocEngine);

Var
  WriterClass : TFPDocWriterClass;
  Writer : TFPDocWriter;
  I : Integer;
  Cmd,Arg : String;

begin
  WriterClass:=GetWriterClass(Options.Backend);
  Writer:=WriterClass.Create(Engine.Package,Engine);
  With Writer do
    Try
      If FVerbose then
        DoLog('Writing documentation');
      OnLog:=Self.OnLog;
      If Options.BackendOptions.Count>0 then
        for I:=0 to ((Options.BackendOptions.Count-1) div 2) do
          begin
          Cmd:=Options.BackendOptions[I*2];
          Arg:=Options.BackendOptions[I*2+1];
          If not InterPretOption(Cmd,Arg) then
            DoLog(SCmdLineInvalidOption,[Cmd+'='+Arg]);
          end;
      WriteDoc;
    Finally
      Free;
    end;
  if Length(APackage.ContentFile) > 0 then
    Engine.WriteContentFile(APackage.ContentFile);
end;

procedure TFPDocCreator.CreateDocumentation(APackage: TFPDocPackage; ParseOnly : Boolean);

var
  i,j: Integer;
  Engine : TFPDocEngine;
  Cmd,Arg : String;

begin
  Engine:=TFPDocEngine.Create;
  try
    For J:=0 to Apackage.Imports.Count-1 do
      begin
      Arg:=Apackage.Imports[j];
      i := Pos(',', Arg);
      Engine.ReadContentFile(Copy(Arg,1,i-1),Copy(Arg,i+1,Length(Arg)));
      end;
    for i := 0 to APackage.Descriptions.Count - 1 do
      Engine.AddDocFile(APackage.Descriptions[i],Options.donttrim);
    Engine.SetPackageName(APackage.Name);
    Engine.Output:=APackage.Output;
    Engine.OnLog:=Self.OnLog;
    Engine.ScannerLogEvents:=Self.ScannerLogEvents;
    Engine.ParserLogEvents:=Self.ParserLogEvents;
    Engine.HideProtected:=Options.HideProtected;
    Engine.HidePrivate:=Not Options.ShowPrivate;
    if Length(Options.Language) > 0 then
      TranslateDocStrings(Options.Language);
    for i := 0 to APackage.Inputs.Count - 1 do
      try
        ParseSource(Engine, APackage.Inputs[i], Options.OSTarget, Options.CPUTarget);
      except
        on e: EParserError do
          If Options.StopOnParseError then
            Raise
          else
            DoLog('%s(%d,%d): %s',[e.Filename, e.Row, e.Column, e.Message]);
      end;
    if Not ParseOnly then
      CreateOutput(APackage,Engine);
  finally
    FreeAndNil(Engine);
  end;
end;

procedure TFPDocCreator.CreateProjectFile(Const AFileName: string);
begin
  With TXMLFPDocOptions.Create(Self) do
  try
    SaveOptionsToFile(FProject,AFileName);
  finally
    Free;
  end;
end;

procedure TFPDocCreator.LoadProjectFile(const AFileName: string);
begin
  With TXMLFPDocOptions.Create(self) do
    try
      LoadOptionsFromFile(FProject,AFileName);
    finally
      Free;
    end;
end;

end.

