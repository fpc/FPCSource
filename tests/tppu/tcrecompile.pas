unit tcrecompile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tstppuutils;

type

  { TTestRecompile }

  TTestRecompile = class(TTestCase)
  private
    FCompiled: TStringList;
    FMainSrc: string;
    FOutDir: string;
    FPP: string;
    FStep: string;
    FUnitPath: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CleanOutputDir; overload;
    procedure CleanOutputDir(Dir: string); overload;
    procedure Compile;
    procedure CheckCompiled(const Expected: TStringArray);
    property PP: string read FPP write FPP;
    property UnitPath: string read FUnitPath write FUnitPath;
    property OutDir: string read FOutDir write FOutDir;
    property MainSrc: string read FMainSrc write FMainSrc;
    property Compiled: TStringList read FCompiled write FCompiled;
    property Step: string read FStep write FStep;
  public
    constructor Create; override;
    procedure GetCompiler;
    procedure CheckCompiler;
  published
    procedure TestTwoUnits;
    procedure TestImplInline1;
  end;


implementation


{ TTestRecompile }

procedure TTestRecompile.SetUp;
begin
  inherited SetUp;
  UnitPath:='';
  OutDir:='';
  MainSrc:='';
end;

procedure TTestRecompile.TearDown;
begin
  FreeAndNil(FCompiled);
  inherited TearDown;
end;

procedure TTestRecompile.CleanOutputDir;
begin
  CleanOutputDir(OutDir);
end;

procedure TTestRecompile.CleanOutputDir(Dir: string);
var
  Info: TRawByteSearchRec;
  Filename: String;
  r: LongInt;
begin
  if Dir='' then
    Fail('TTestRecompile.CleanOutputDir: missing Dir');
  if Dir[length(Dir)]=PathDelim then
    Delete(Dir,length(Dir),1);

  if not DirectoryExists(Dir) then
    if not CreateDir(Dir) then
      Fail('unable to create output directory "'+Dir+'"');

  writeln('CleanOutputDir ',Dir);
  r:=FindFirst(Dir+PathDelim+AllFilesMask,faAnyFile,Info);
  try
    if r<>0 then exit;
    repeat
      case Info.Name of
      '','.','..': continue;
      end;
      if faDirectory and Info.Attr>0 then
        continue; // keep directories
      if Info.Name[1]='.' then
        continue; // keep hidden files
      case lowercase(ExtractFileExt(Info.Name)) of
      '.txt': continue; // keep txt files
      end;

      Filename:=Dir+PathDelim+Info.Name;
      if not DeleteFile(Filename) then
        Fail('unable to delete "'+Filename+'"');
    until FindNext(Info)<>0;
  finally
    FindClose(Info);
  end;
end;

procedure TTestRecompile.Compile;
var
  Params, Lines: TStringList;
  i: Integer;
  Line, Filename: String;
begin
  if UnitPath='' then
    Fail('missing UnitPath, Step='+Step);

  if OutDir='' then
    Fail('missing OutDir, Step='+Step);
  if not DirectoryExists(OutDir) then
    Fail('OutDir not found "'+OutDir+'", Step='+Step);

  if MainSrc='' then
    Fail('missing MainSrc, Step='+Step);
  if not FileExists(MainSrc) then
    Fail('main src file not found "'+MainSrc+'", Step='+Step);

  Lines:=nil;
  Compiled:=TStringList.Create;
  Params:=TStringList.Create;
  try
    Params.Add('-Fu'+UnitPath);
    Params.Add('-FE'+OutDir);
    Params.Add(MainSrc);
    if not RunTool(PP,Params,'',false,true,Lines) then
      Fail('compile failed, Step='+Step);

    for i:=0 to Lines.Count-1 do
    begin
      Line:=Lines[i];
      if LeftStr(Line,length('Compiling '))='Compiling ' then
      begin
        Filename:=copy(Line,length('Compiling ')+1,length(Line));
        writeln('Compiling: ',Filename);
        Filename:=ExtractFileName(Filename);
        if Compiled.IndexOf(Filename)<0 then
          Compiled.Add(Filename);
      end;
    end;
  finally
    Lines.Free;
    Params.Free;
  end;
end;

procedure TTestRecompile.CheckCompiled(const Expected: TStringArray);
var
  i, j: Integer;
begin
  for i:=0 to length(Expected)-1 do
    if Compiled.IndexOf(Expected[i])<0 then
      Fail('missing compiling "'+Expected[i]+'", Step='+Step);
  for i:=0 to Compiled.Count-1 do
  begin
    j:=length(Expected)-1;
    while (j>=0) and (Expected[j]<>Compiled[i]) do dec(j);
    if j<0 then
      Fail('unexpected compiling "'+Compiled[i]+'", Step='+Step);
  end;
end;

constructor TTestRecompile.Create;
begin
  inherited Create;

  GetCompiler;
end;

procedure TTestRecompile.GetCompiler;
begin
  PP:=GetEnvironmentVariable(String('PP'));
  if PP>'' then
  begin
    CheckCompiler;
    exit;
  end;

  raise Exception.Create('I need environment var "PP"');
end;

procedure TTestRecompile.CheckCompiler;

  procedure E(Msg: string);
  begin
    writeln('TTestRecompile.CheckCompiler: '+Msg);
    raise Exception.Create('TTestRecompile.CheckCompiler: '+Msg);
  end;

begin
  if PP='' then
    E('missing compiler');
  if not FileIsExecutable(PP) then
    E('compiler not executable: "'+PP+'"');
end;

procedure TTestRecompile.TestTwoUnits;
begin
  UnitPath:='twounits';
  OutDir:='twounits'+PathDelim+'ppus';
  MainSrc:='twounits'+PathDelim+'tppu_twounits_ant.pas';

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['tppu_twounits_ant.pas','tppu_twounits_bird.pas']);

  Step:='Second compile';
  Compile;
  CheckCompiled(['tppu_twounits_ant.pas']);
end;

procedure TTestRecompile.TestImplInline1;
begin
  UnitPath:='implinline1';
  OutDir:='implinline1'+PathDelim+'ppus';
  MainSrc:='implinline1'+PathDelim+'implinline1_ant.pas';

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['implinline1_ant.pas','implinline1_bird.pas']);

  Step:='Second compile';
  Compile;
  CheckCompiled(['implinline1_ant.pas']);
end;

initialization
  RegisterTests([TTestRecompile]);

end.

