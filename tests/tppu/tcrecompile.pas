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
    procedure TestTwoUnits; // 2 units
    procedure TestChangeLeaf1; // prog+2 units, change leaf
    procedure TestChangeInner1; // prog+2 units, change inner unit, keep leaf
    procedure TestChangeInlineBody; // prog+1 unit plus a package of 2 units, change of inline body should change crc

    // inline modifier in implementation (not in interface)
    procedure TestImplInline1; // 2 units, cycle, impl inline
    procedure TestImplInline2; // program + 2 units cycle, impl inline
    procedure TestImplInline_Bug41291; // program plus 3 cycles
    procedure TestImplInline3; // program + 2 units cycle, impl inline, implementation changed
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
    if (Compiled=nil) or (Compiled.IndexOf(Expected[i])<0) then
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
  // the bird ppu does not depend on ant, so it is kept
  CheckCompiled(['tppu_twounits_ant.pas']);
end;

procedure TTestRecompile.TestChangeLeaf1;
begin
  UnitPath:='changeleaf1;changeleaf1'+PathDelim+'src1';
  OutDir:='changeleaf1'+PathDelim+'ppus';
  MainSrc:='changeleaf1'+PathDelim+'changeleaf1_prg.pas';

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['changeleaf1_prg.pas','changeleaf1_ant.pas','changeleaf1_bird.pas']);

  Step:='Second compile';
  UnitPath:='changeleaf1;changeleaf1'+PathDelim+'src2';
  Compile;
  // the main src is always compiled, bird changed, so all ant must be recompiled as well
  CheckCompiled(['changeleaf1_prg.pas','changeleaf1_ant.pas','changeleaf1_bird.pas']);
end;

procedure TTestRecompile.TestChangeInner1;
begin
  UnitPath:='changeinner1;changeinner1'+PathDelim+'src1';
  OutDir:='changeinner1'+PathDelim+'ppus';
  MainSrc:='changeinner1'+PathDelim+'changeinner1_prg.pas';

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['changeinner1_prg.pas','changeinner1_ant.pas','changeinner1_bird.pas']);

  Step:='Second compile';
  UnitPath:='changeinner1;changeinner1'+PathDelim+'src2';
  Compile;
  // the main src is always compiled, ant changed, bird is kept
  CheckCompiled(['changeinner1_prg.pas','changeinner1_ant.pas']);
end;

procedure TTestRecompile.TestChangeInlineBody;
var
  ProgDir, PkgDir, PkgOutDir: String;
begin
  // unit testcib_elk uses an inline function of unit testcib_bird
  // elk belongs to the program, bird to the package, so they are compiled separately
  // when the inline body of bird changes, the elk.ppu must be rebuilt too

  ProgDir:='changeinlinebody'+PathDelim;
  PkgDir:=ProgDir+'pkg';
  PkgOutDir:=PkgDir+PathDelim+'lib';

  // compile package containing testcib_ant.pas and testcib_bird.pas
  Step:='Compile original package';
  UnitPath:=PkgDir+';'+ProgDir+'original';
  OutDir:=PkgOutDir;
  MainSrc:=PkgDir+PathDelim+'testcib_ant.pas';
  CleanOutputDir;
  Compile;
  CheckCompiled(['testcib_ant.pas','testcib_bird.pas']);

  // compile program
  Step:='Compile program with original package ppus';
  UnitPath:=ProgDir+';'+PkgOutDir;
  OutDir:=ProgDir+'lib';
  MainSrc:=ProgDir+'testcib_prog.pas';
  CleanOutputDir;
  Compile;
  CheckCompiled(['testcib_prog.pas','testcib_elk.pas']);

  // recompile package with changed testcib_bird.pas
  Step:='Compile changed package';
  UnitPath:=PkgDir+';'+ProgDir+'changed';
  OutDir:=PkgOutDir;
  MainSrc:=PkgDir+PathDelim+'testcib_ant.pas';
  Compile;
  CheckCompiled(['testcib_ant.pas','testcib_bird.pas']);

  // recompile program
  Step:='Compile program with changed package ppus';
  UnitPath:=ProgDir+';'+PkgOutDir;
  OutDir:=ProgDir+'lib';
  MainSrc:=ProgDir+'testcib_prog.pas';
  Compile;
  CheckCompiled(['testcib_prog.pas','testcib_elk.pas']);
end;

procedure TTestRecompile.TestImplInline1;
// unit ant uses bird
// unit bird impl uses ant and has a function with inline modifier in implementation
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
  // the main src is always compiled, and since bird ppu depends on ant, it is always compiled as well
  CheckCompiled(['implinline1_ant.pas','implinline1_bird.pas']);
end;

procedure TTestRecompile.TestImplInline2;
// prg uses ant
// unit ant uses bird
// unit bird impl uses ant and has a function with inline modifier in implementation
begin
  UnitPath:='implinline2';
  OutDir:='implinline2'+PathDelim+'ppus';
  MainSrc:='implinline2'+PathDelim+'implinline2_prg.pas';

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['implinline2_prg.pas','implinline2_ant.pas','implinline2_bird.pas']);

  Step:='Second compile';
  Compile;
  // the main src is always compiled, the two ppus of ant and bird are kept
  CheckCompiled(['implinline2_prg.pas']);
end;

procedure TTestRecompile.TestImplInline_Bug41291;
begin
  UnitPath:='bug41291';
  OutDir:='bug41291'+PathDelim+'ppus';
  MainSrc:='bug41291'+PathDelim+'bug41291_app.pas';

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['bug41291_app.pas','bug41291_mclasses.pas','bug41291_mseapplication.pas',
    'bug41291_mseclasses.pas','bug41291_mseeditglob.pas','bug41291_mseifiglob.pas']);

  Step:='Second compile';
  Compile;
  // the main src is always compiled, the other ppus are kept
  CheckCompiled(['bug41291_app.pas']);
end;

procedure TTestRecompile.TestImplInline3;
begin
  UnitPath:='implinline3;implinline3'+PathDelim+'src1';
  OutDir:='implinline3'+PathDelim+'ppus';
  MainSrc:='implinline3'+PathDelim+'implinline3_prg.pas';

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['implinline3_prg.pas','implinline3_ant.pas','implinline3_bird.pas']);

  Step:='Second compile';
  UnitPath:='implinline3;implinline3'+PathDelim+'src2';
  Compile;
  // the main src is always compiled, and the ant impl changed, so bird is also compiled
  CheckCompiled(['implinline3_prg.pas','implinline3_ant.pas','implinline3_bird.pas']);
end;

initialization
  RegisterTests([TTestRecompile]);

end.

