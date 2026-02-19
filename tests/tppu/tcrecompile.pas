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
    FOptionUr: boolean;
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
    procedure CheckCompiled(const Must: TStringArray; const Optional: TStringArray = []);
    procedure TouchFile(const aFilename: string);
    procedure MakeDateDiffer(const File1, File2: string);
    property PP: string read FPP write FPP;
    property UnitPath: string read FUnitPath write FUnitPath;
    property OutDir: string read FOutDir write FOutDir;
    property MainSrc: string read FMainSrc write FMainSrc;
    property Compiled: TStringList read FCompiled write FCompiled;
    property OptionUr: boolean read FOptionUr write FOptionUr;
    property Step: string read FStep write FStep;
  public
    constructor Create; override;
    procedure GetCompiler;
    procedure CheckCompiler;
  published
    procedure TestTwoUnits; // 2 units, recompile first
    procedure TestChangeLeaf1; // prog->ant->bird, change bird, recompile ant as well
    procedure TestChangeInner1; // prog->ant->bird, change ant, keep bird.ppu

    procedure TestCycle2_ChangeB; // prog->ant->bird, bird.impl->ant, change bird, same crc
    procedure TestCycle3_ChangeC; // prog->ant->bird->cat, cat.impl->ant, change cat same crc
    procedure TestCycle3_ChangeC_Intf; // prog->ant->bird->cat, cat.impl->ant, change cat interface crc
    procedure TestCycleImpl3_ChangeC; // prog->ant.impl->bird.impl->cat, cat.impl->ant, change cat
    procedure TestCycle2_ChangeB_CRC; // prog->ant->bird, bird.impl->ant, change bird crc
    procedure TestCycle22_ChangeC_CRC; // prog->ant->bird->cat, bird.impl->ant, cat.impl->bird, change cat crc
    procedure TestCycle32_ChangeC_CRC; // prog->ant->bird->cat, bird.impl->ant, cat.impl->bird+ant, change cat crc

    // -Ur Generate release unit files (never automatically recompiled)
    procedure TestUr_cycle2;

    procedure TestChangeInlineBodyBug; // Bug: prog+1 unit plus a package of 2 units, change of inline body should change crc, but does not

    procedure TestBug41457; // two cycles of size 2 and 3

    // inline modifier in implementation (not in interface)
    procedure TestImplInline1; // 2 units, cycle, impl inline
    procedure TestImplInline2; // program + 2 units cycle, impl inline
    procedure TestImplInline_Bug41291; // program plus 3 cycles
    procedure TestImplInline3; // program + 2 units cycle, impl inline, implementation changed

    // inherited
    procedure TestAncestor_ChangeInheritedMethod; // changing inherited method in an indirectly used unit
    // ToDo: TestAncestor_ChangeInheritedMethodParamType; // changing inherited method para type in an indirectly used unit
    //         the indirect_crc does not yet support this. 16th Feb 2026

    // generics
    procedure TestGeneric_IndirectUses; // specialization of an inherited class in an indirectly used unit
    procedure TestGeneric_Cycle1; // prg->ant->bird, bird.impl->ant, TAnt->TBird
    procedure TestGeneric_Cycle2; // prg->ant.impl->bird, bird.impl->ant, TAnt->TBird
  end;


implementation


{ TTestRecompile }

procedure TTestRecompile.SetUp;
begin
  inherited SetUp;
  UnitPath:='';
  OutDir:='';
  MainSrc:='';
  OptionUr:=false;
  Compiled:=TStringList.Create;
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
  Compiled.Clear;
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
  Params:=TStringList.Create;
  try
    Params.Add('-Fu'+UnitPath);
    Params.Add('-FE'+OutDir);
    if OptionUr then
      Params.Add('-Ur');
    Params.Add(MainSrc);
    if not RunTool(PP,Params,'',false,true,Lines) then
      Fail('compile failed, Step='+Step);

    for i:=0 to Lines.Count-1 do
    begin
      Line:=Lines[i];
      if LeftStr(Line,length('Compiling '))='Compiling ' then
      begin
        Filename:=copy(Line,length('Compiling ')+1,length(Line));
        writeln('Compiling ',Filename);
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

procedure TTestRecompile.CheckCompiled(const Must: TStringArray; const Optional: TStringArray);
var
  i, j: Integer;
begin
  // check that all Must are compiled
  for i:=0 to length(Must)-1 do
    if (Compiled=nil) or (Compiled.IndexOf(Must[i])<0) then
      Fail('missing compiling "'+Must[i]+'", Step='+Step);

  // check that only Must and Optional are compiled
  for i:=0 to Compiled.Count-1 do
  begin
    j:=length(Must)-1;
    while (j>=0) and (Must[j]<>Compiled[i]) do dec(j);
    if j>=0 then continue;
    j:=length(Optional)-1;
    while (j>=0) and (Optional[j]<>Compiled[i]) do dec(j);
    if j<0 then
      Fail('unexpected compiling "'+Compiled[i]+'", Step='+Step);
  end;
end;

procedure TTestRecompile.TouchFile(const aFilename: string);
var
  Age1: Int64;
begin
  Age1:=FileAge(aFilename);
  if Age1<0 then
    Fail('file not found "'+aFilename+'"');
  FileSetDate(aFilename,Age1+2);
end;

procedure TTestRecompile.MakeDateDiffer(const File1, File2: string);
var
  Age1, Age2: Int64;
begin
  Age1:=FileAge(File1);
  if Age1<0 then
    Fail('file not found "'+File1+'"');
  Age2:=FileAge(File2);
  if Age2<0 then
    Fail('file not found "'+File2+'"');
  if Age1<>Age2 then exit;
  FileSetDate(File2,Age2-2);
end;

constructor TTestRecompile.Create;
begin
  inherited Create;

  GetCompiler;
end;

procedure TTestRecompile.GetCompiler;
begin
  PP:=GetEnvironmentVariable(String('TEST_FPC'));
  if PP='' then
    PP:=GetEnvironmentVariable(String('PP'));
  if not FileExists(PP) then
    PP:=ExeSearch(PP,'');

  if PP<>'' then
  begin
    CheckCompiler;
    exit;
  end;

  raise Exception.Create('I need environment var "TEST_FPC" or "PP"');
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
// prog->ant->bird, change bird
var
  Dir: String;
begin
  Dir:='changeleaf1';
  UnitPath:=Dir+';'+Dir+PathDelim+'src1';
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'changeleaf1_prg.pas';
  MakeDateDiffer(
    Dir+PathDelim+'src1'+PathDelim+'changeleaf1_bird.pas',
    Dir+PathDelim+'src2'+PathDelim+'changeleaf1_bird.pas');

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['changeleaf1_prg.pas','changeleaf1_ant.pas','changeleaf1_bird.pas']);

  Step:='Second compile';
  UnitPath:=Dir+';'+Dir+PathDelim+'src2';
  Compile;
  // the main src is always compiled, bird changed
  CheckCompiled(['changeleaf1_prg.pas','changeleaf1_bird.pas']);
end;

procedure TTestRecompile.TestChangeInner1;
// prog->ant->bird, change ant
var
  Dir: String;
begin
  Dir:='changeinner1';
  UnitPath:=Dir+';'+Dir+PathDelim+'src1';
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'changeinner1_prg.pas';
  MakeDateDiffer(
    Dir+PathDelim+'src1'+PathDelim+'changeinner1_ant.pas',
    Dir+PathDelim+'src2'+PathDelim+'changeinner1_ant.pas');

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['changeinner1_prg.pas','changeinner1_ant.pas','changeinner1_bird.pas']);

  Step:='Second compile';
  UnitPath:=Dir+';'+Dir+PathDelim+'src2';
  Compile;
  // the main src is always compiled, ant changed, bird is kept
  CheckCompiled(['changeinner1_prg.pas','changeinner1_ant.pas']);
end;

procedure TTestRecompile.TestCycle2_ChangeB;
// prog->ant->bird, bird.impl->ant, change bird same crc
var
  Dir: String;
begin
  Dir:='cycle2_changeb';
  UnitPath:=Dir+';'+Dir+PathDelim+'src1';
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'cycle2_changeb_prg.pas';
  MakeDateDiffer(
    Dir+PathDelim+'src1'+PathDelim+'cycle2_changeb_bird.pas',
    Dir+PathDelim+'src2'+PathDelim+'cycle2_changeb_bird.pas');

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['cycle2_changeb_prg.pas','cycle2_changeb_ant.pas','cycle2_changeb_bird.pas']);

  Step:='Second compile';
  UnitPath:=Dir+';'+Dir+PathDelim+'src2';
  Compile;
  // the main src is always compiled, bird changed same crc, so ant is kept
  CheckCompiled(['cycle2_changeb_prg.pas','cycle2_changeb_bird.pas']);
end;

procedure TTestRecompile.TestCycle3_ChangeC;
// prog->ant->bird->cat, cat.impl->ant, change cat same crc
var
  Dir: String;
begin
  Dir:='cycle3_changec';
  UnitPath:=Dir+';'+Dir+PathDelim+'src1';
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'cycle3_changec_prg.pas';
  MakeDateDiffer(
    Dir+PathDelim+'src1'+PathDelim+'cycle3_changec_cat.pas',
    Dir+PathDelim+'src2'+PathDelim+'cycle3_changec_cat.pas');

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['cycle3_changec_prg.pas','cycle3_changec_ant.pas','cycle3_changec_bird.pas','cycle3_changec_cat.pas']);

  Step:='Second compile';
  UnitPath:=Dir+';'+Dir+PathDelim+'src2';
  Compile;
  // the main src is always compiled, cat changed but not crc,
  CheckCompiled(['cycle3_changec_prg.pas','cycle3_changec_cat.pas']);
end;

procedure TTestRecompile.TestCycle3_ChangeC_Intf;
// prog->ant->bird->cat, cat.impl->ant, change cat interface crc
var
  Dir: String;
begin
  Dir:='cycle3_changec_intf';
  UnitPath:=Dir+';'+Dir+PathDelim+'src1';
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'cycle3_changec_intf_prg.pas';
  MakeDateDiffer(
    Dir+PathDelim+'src1'+PathDelim+'cycle3_changec_intf_cat.pas',
    Dir+PathDelim+'src2'+PathDelim+'cycle3_changec_intf_cat.pas');

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['cycle3_changec_intf_prg.pas','cycle3_changec_intf_ant.pas',
    'cycle3_changec_intf_bird.pas','cycle3_changec_intf_cat.pas']);

  Step:='Second compile';
  UnitPath:=Dir+';'+Dir+PathDelim+'src2';
  Compile;
  // the main src is always compiled, cat changed interface crc, so bird must be recompiled
  CheckCompiled(['cycle3_changec_intf_prg.pas','cycle3_changec_intf_bird.pas','cycle3_changec_intf_cat.pas']);
end;

procedure TTestRecompile.TestCycleImpl3_ChangeC;
// prog->ant.impl->bird.impl->cat, cat.impl->ant, change cat impl
var
  Dir: String;
begin
  Dir:='cycleimpl3_changec';
  UnitPath:=Dir+';'+Dir+PathDelim+'src1';
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'cycleimpl3_changec_prg.pas';
  MakeDateDiffer(
    Dir+PathDelim+'src1'+PathDelim+'cycleimpl3_changec_cat.pas',
    Dir+PathDelim+'src2'+PathDelim+'cycleimpl3_changec_cat.pas');

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['cycleimpl3_changec_prg.pas','cycleimpl3_changec_ant.pas','cycleimpl3_changec_bird.pas','cycleimpl3_changec_cat.pas']);

  Step:='Second compile';
  UnitPath:=Dir+';'+Dir+PathDelim+'src2';
  Compile;
  // the main src is always compiled, cat changed but same crc
  CheckCompiled(['cycleimpl3_changec_prg.pas','cycleimpl3_changec_cat.pas']);
end;

procedure TTestRecompile.TestCycle2_ChangeB_CRC;
// prog->ant->bird, bird.impl->ant, change bird
var
  Dir: String;
begin
  Dir:='cycle2_changeb_crc';
  UnitPath:=Dir+';'+Dir+PathDelim+'src1';
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'cycle2_changeb_crc_prg.pas';
  MakeDateDiffer(
    Dir+PathDelim+'src1'+PathDelim+'cycle2_changeb_crc_bird.pas',
    Dir+PathDelim+'src2'+PathDelim+'cycle2_changeb_crc_bird.pas');

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['cycle2_changeb_crc_prg.pas','cycle2_changeb_crc_ant.pas','cycle2_changeb_crc_bird.pas']);

  Step:='Second compile';
  UnitPath:=Dir+';'+Dir+PathDelim+'src2';
  Compile;
  // the main src is always compiled, bird's crc changed, so ant must be recompiled as well
  CheckCompiled(['cycle2_changeb_crc_prg.pas','cycle2_changeb_crc_ant.pas','cycle2_changeb_crc_bird.pas']);
end;

procedure TTestRecompile.TestCycle22_ChangeC_CRC;
// prog->ant->bird->cat, bird.impl->ant, cat.impl->bird, change cat crc
var
  Dir: String;
begin
  Dir:='cycle22_changec';
  UnitPath:=Dir+';'+Dir+PathDelim+'src1';
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'cycle22_changec_prg.pas';
  MakeDateDiffer(
    Dir+PathDelim+'src1'+PathDelim+'cycle22_changec_cat.pas',
    Dir+PathDelim+'src2'+PathDelim+'cycle22_changec_cat.pas');

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['cycle22_changec_prg.pas','cycle22_changec_ant.pas',
                 'cycle22_changec_bird.pas','cycle22_changec_cat.pas']);

  Step:='Second compile';
  UnitPath:=Dir+';'+Dir+PathDelim+'src2';
  Compile;
  // the main src is always compiled, cat changed crc, so bird must be recompiled same crc,
  // so ant is kept
  CheckCompiled(['cycle22_changec_prg.pas',
                 'cycle22_changec_bird.pas','cycle22_changec_cat.pas']);
end;

procedure TTestRecompile.TestCycle32_ChangeC_CRC;
// prog->ant->bird->cat, bird.impl->ant, cat.impl->bird+ant, change cat crc
var
  Dir: String;
begin
  Dir:='cycle32_changec';
  UnitPath:=Dir+';'+Dir+PathDelim+'src1';
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'cycle32_changec_prg.pas';
  MakeDateDiffer(
    Dir+PathDelim+'src1'+PathDelim+'cycle32_changec_cat.pas',
    Dir+PathDelim+'src2'+PathDelim+'cycle32_changec_cat.pas');

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['cycle32_changec_prg.pas','cycle32_changec_ant.pas',
                 'cycle32_changec_bird.pas','cycle32_changec_cat.pas']);

  Step:='Second compile';
  UnitPath:=Dir+';'+Dir+PathDelim+'src2';
  Compile;
  // the main src is always compiled, cat changed crc, so bird must be recompiled same crc,
  // so ant either needs a reload or a recompile
  CheckCompiled(['cycle32_changec_prg.pas','cycle32_changec_ant.pas',
                 'cycle32_changec_bird.pas','cycle32_changec_cat.pas']);
end;

procedure TTestRecompile.TestUr_cycle2;
// ant->bird, bird.impl->ant
var
  Dir: String;
begin
  Dir:='ur_cycle2';
  UnitPath:=Dir;
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'ur_cycle2_ant.pas';
  OptionUr:=true;

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['ur_cycle2_ant.pas','ur_cycle2_bird.pas']);

  Step:='Second compile, only ant';
  Compile;
  // the main src is always compiled, bird is kept even though it is part of the cycle
  CheckCompiled(['ur_cycle2_ant.pas']);

  Step:='Third compile, only bird';
  MainSrc:=Dir+PathDelim+'ur_cycle2_bird.pas';
  Compile;
  // the main src is always compiled, ant is kept even though it is part of the cycle
  CheckCompiled(['ur_cycle2_bird.pas']);
end;

procedure TTestRecompile.TestChangeInlineBodyBug;
var
  ProgDir, PkgDir, PkgOutDir: String;
begin
  // unit elk uses an inline function of unit bird
  // elk belongs to the program, bird to the package, so they are compiled separately
  // when the inline body of bird changes, the elk.ppu must be rebuilt too

  ProgDir:='changeinlinebody'+PathDelim;
  PkgDir:=ProgDir+'pkg';
  PkgOutDir:=PkgDir+PathDelim+'lib';
  MakeDateDiffer(
    ProgDir+'original'+PathDelim+'testcib_bird.pas',
    ProgDir+'changed'+PathDelim+'testcib_bird.pas');

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
  // fpc should compile elk:
  //CheckCompiled(['testcib_prog.pas','testcib_elk.pas']);
  // But it does not:
  CheckCompiled(['testcib_prog.pas']);
end;

procedure TTestRecompile.TestBug41457;
// ant: intf->bird
// bird: intf->seagull,eagle, impl->ant
// seagull: impl->bird
// eagle: intf->hawk
// hawk: impl->bird
begin
  UnitPath:='bug41457';
  OutDir:=UnitPath+PathDelim+'ppus';
  MainSrc:=UnitPath+PathDelim+'bug41457_ant.pas';

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['bug41457_ant.pas',
    'bug41457_bird.pas',
    'bug41457_eagle.pas',
    'bug41457_hawk.pas',
    'bug41457_seagull.pas']);

  Step:='Second compile';
  Compile;
  CheckCompiled(['bug41457_ant.pas']);
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
  // the main src is always compiled
  CheckCompiled(['implinline1_ant.pas']);
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
var
  Dir: String;
begin
  Dir:='implinline3';
  UnitPath:=Dir+';'+Dir+PathDelim+'src1';
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'implinline3_prg.pas';
  MakeDateDiffer(
    Dir+PathDelim+'src1'+PathDelim+'implinline3_ant.pas',
    Dir+PathDelim+'src2'+PathDelim+'implinline3_ant.pas');
  MakeDateDiffer(
    Dir+PathDelim+'src1'+PathDelim+'implinline3_bird.pas',
    Dir+PathDelim+'src2'+PathDelim+'implinline3_bird.pas');

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['implinline3_prg.pas','implinline3_ant.pas','implinline3_bird.pas']);

  Step:='Second compile';
  UnitPath:=Dir+';'+Dir+PathDelim+'src2';
  Compile;
  // the main src is always compiled, and the ant impl changed, so bird is also compiled
  CheckCompiled(['implinline3_prg.pas','implinline3_ant.pas','implinline3_bird.pas']);
end;

procedure TTestRecompile.TestAncestor_ChangeInheritedMethod;
// ant->bird->cat->eagle, change method in eagle used by bird
var
  Dir: String;
begin
  Dir:='ancestorchange1';
  UnitPath:=Dir+';'+Dir+PathDelim+'src1';
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'ancestorchange1_ant.pas';
  MakeDateDiffer(
    Dir+PathDelim+'src1'+PathDelim+'ancestorchange1_eagle.pas',
    Dir+PathDelim+'src2'+PathDelim+'ancestorchange1_eagle.pas');

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['ancestorchange1_ant.pas','ancestorchange1_bird.pas','ancestorchange1_cat.pas',
    'ancestorchange1_eagle.pas']);

  Step:='Second compile';
  UnitPath:=Dir+';'+Dir+PathDelim+'src2';
  Compile;
  // the main src is always compiled, cat intf class TCat changed, so bird and ant are recompiled
  CheckCompiled(['ancestorchange1_ant.pas','ancestorchange1_bird.pas','ancestorchange1_cat.pas',
    'ancestorchange1_eagle.pas']);
end;

procedure TTestRecompile.TestGeneric_IndirectUses;
// prog->ant.impl->bird->cat, ant specializes cat, change the generic func of cat
var
  Dir: String;
begin
  Dir:='generic_indirectuses';
  UnitPath:=Dir+';'+Dir+PathDelim+'src1';
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'generic_indirectuses_prg.pas';
  MakeDateDiffer(
    Dir+PathDelim+'src1'+PathDelim+'generic_indirectuses_cat.pas',
    Dir+PathDelim+'src2'+PathDelim+'generic_indirectuses_cat.pas');

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['generic_indirectuses_prg.pas','generic_indirectuses_ant.pas',
     'generic_indirectuses_bird.pas','generic_indirectuses_cat.pas']);

  Step:='Second compile';
  UnitPath:=Dir+';'+Dir+PathDelim+'src2';
  Compile;
  // the main src is always compiled, cat impl of the generic changed, so specialization in ant changed
  CheckCompiled(['generic_indirectuses_prg.pas','generic_indirectuses_ant.pas','generic_indirectuses_cat.pas']);
end;

procedure TTestRecompile.TestGeneric_Cycle1;
// prg->ant->bird, bird.impl->ant, TAnt->TBird
var
  Dir: String;
begin
  Dir:='generic_cycle1';
  UnitPath:=Dir;
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'generic_cycle1_prg.pas';

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['generic_cycle1_prg.pas','generic_cycle1_ant.pas','generic_cycle1_bird.pas']);

  Step:='Second compile';
  Compile;
  // the main src is always compiled
  CheckCompiled(['generic_cycle1_prg.pas']);
end;

procedure TTestRecompile.TestGeneric_Cycle2;
// prg->ant.impl->bird, bird.impl->ant, TAnt->TBird
var
  Dir: String;
begin
  Dir:='generic_cycle1';
  UnitPath:=Dir;
  OutDir:=Dir+PathDelim+'ppus';
  MainSrc:=Dir+PathDelim+'generic_cycle1_prg.pas';

  Step:='First compile';
  CleanOutputDir;
  Compile;
  CheckCompiled(['generic_cycle1_prg.pas','generic_cycle1_ant.pas','generic_cycle1_bird.pas']);

  Step:='Second compile';
  Compile;
  // the main src is always compiled
  CheckCompiled(['generic_cycle1_prg.pas']);
end;

initialization
  RegisterTests([TTestRecompile]);

end.

