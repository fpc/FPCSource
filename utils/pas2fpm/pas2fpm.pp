program pas2fpm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, passrcutil;

type

  { TPas2FPMakeApp }

  TPas2FPMakeApp = class(TCustomApplication)
  private
    procedure AddLine(const ALine: String);
    function CheckParams : boolean;
    procedure CreateSources;
    function GetUnitProps(const FN: String; out Res: Boolean; U: TStrings
      ): Boolean;
    procedure WriteProgEnd;
    procedure WriteProgStart;
    procedure WriteSources;
  protected
    FFiles,
    FSrc,
    FUnits: TStrings;
    FOutputFile : string;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TPas2FPMakeApp }

Function TPas2FPMakeApp.CheckParams : Boolean;

Var
  I : Integer;
  S : String;

begin
  Result:=True;
  I:=1;
  While I<=ParamCount do
    begin
    S:=Paramstr(i);
    if (S<>'') then
      begin
      if S[1]<>'-' then
        begin
        FFiles.Add(S);
        FUnits.Add(ChangeFileExt(ExtractFileName(S),''));
        end
      else
        begin
        If (s='-o') then
        else
          begin
          Result:=False;
          exit;
          end;
        end;
      end;
    Inc(i);
    end;
  Result:=(FFiles.Count>0);
end;

procedure TPas2FPMakeApp.AddLine(Const ALine : String);

begin
  FSrc.Add(ALine);
end;

Function TPas2FPMakeApp.GetUnitProps(Const FN : String; Out Res : Boolean; U : TStrings) : Boolean;

Var
  I : Integer;
  A : TPasSrcAnalysis;

begin
  Result:=False;
  try
    A:=TPasSrcAnalysis.Create(Self);
    try
      A.FileName:=FN;
      Res:=A.HasResourcestrings;
      A.GetUsedUnits(U);
      For I:=U.Count-1 downto 0 do
        if FUnits.IndexOf(U[i])=-1 then
          U.Delete(i);
    finally
      A.Free;
    end;
    Result:=True;
  except
    // Ignore
  end;
end;

procedure TPas2FPMakeApp.WriteProgStart;

begin
  AddLine('program fpmake;');
  AddLine('');
  AddLine('uses fpmkunit;');
  AddLine('');
  AddLine('Var');
  AddLine('  T : TTarget;');
  AddLine('  P : TPackage;');
  AddLine('begin');
  AddLine('  With Installer do');
  AddLine('    begin');
  AddLine('    P.Version:=''0.0'';');
//  AddLine('    P.Dependencies.Add('fcl-base');
  AddLine('    P.Author := ''Your name'';');
  AddLine('    P.License := ''LGPL with modification'';');
  AddLine('    P.HomepageURL := ''www.yourcompany.com'';');
  AddLine('    P.Email := ''yourmail@yourcompany.com'';');
  AddLine('    P.Description := ''Your very nice program'';');
  AddLine('    // P.NeedLibC:= false;');
end;

procedure TPas2FPMakeApp.WriteProgEnd;

begin
  AddLine('    Run;');
  AddLine('    end;');
  AddLine('end.');
end;

procedure TPas2FPMakeApp.CreateSources;


Var
  I,j : Integer;
  U : TStrings;
  FN : String;
  R : Boolean;

begin
  WriteProgStart;
  For I:=0 to FFiles.Count-1 do
    begin
    FN:=FFiles[i];
    AddLine('    T:=P.Targets.AddUnit('''+FN+''');');
    U:=TStringList.Create;
    if not GetUnitProps(Fn,R,U) then
      AddLine('    // Failed to analyse unit '+FN)
    else
      begin
      if R then
        AddLine('    T.ResourceStrings := True;');
      if (U.Count>0) then
        begin
        AddLine('    with T.Dependencies do');
        AddLine('      begin');
        For J:=0 to U.Count-1 do
          AddLine('      AddUnit('''+U[j]+''');');
        AddLine('      end;');
        end;
      end;
    end;
  WriteProgEnd;
end;

procedure TPas2FPMakeApp.WriteSources;

Var
  F : Text;

begin
  AssignFile(F,FOutputFile);
  Rewrite(F);
  try
    Write(F,FSrc.Text);
  finally
    CloseFile(F);
  end;
end;

procedure TPas2FPMakeApp.DoRun;

var
  ErrorMsg: String;

begin
  // parse parameters
  if HasOption('h','help') or Not CheckParams then
    begin
    WriteHelp;
    Terminate;
    exit;
    end;
  TStringList(FUnits).Sorted:=True;
  CreateSources;
  WriteSources;
  // stop program loop
  Terminate;
end;

constructor TPas2FPMakeApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FFiles:=TStringList.Create;
  FSrc:=TStringList.Create;
  FUnits:=TStringList.Create;
end;

destructor TPas2FPMakeApp.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FSrc);
  FreeAndNil(FUnits);
  inherited Destroy;
end;

procedure TPas2FPMakeApp.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' [-h] [-o outputfile] file1 .. filen');
end;

var
  Application: TPas2FPMakeApp;
begin
  Application:=TPas2FPMakeApp.Create(nil);
  Application.Title:='Pascal to FPMake application';
  Application.Run;
  Application.Free;
end.

