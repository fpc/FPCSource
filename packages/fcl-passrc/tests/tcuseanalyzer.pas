{
  Examples:
    ./testpassrc --suite=TTestResolver.TestEmpty
}
unit tcuseanalyzer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit,
  PasTree, PScanner, PasResolver,
  tcbaseparser, testregistry, strutils, tcresolver, PasUseAnalyzer;

type

  { TCustomTestUseAnalyzer }

  TCustomTestUseAnalyzer = Class(TCustomTestResolver)
  private
    FAnalyzer: TPasAnalyzer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure AnalyzeProgram; virtual;
    procedure ParseProgram; override;
    procedure CheckUsedMarkers; virtual;
  public
    property Analyzer: TPasAnalyzer read FAnalyzer;
  end;

  TTestUseAnalyzer = Class(TCustomTestUseAnalyzer)
  published
    // single module
    procedure TestM_ProgramLocalVar;
    procedure TestM_AssignStatement;
    procedure TestM_ForLoopStatement;
    procedure TestM_AsmStatement;
    procedure TestM_CaseOfStatement;
    procedure TestM_IfThenElseStatement;
    procedure TestM_WhileDoStatement;
    procedure TestM_RepeatUntilStatement;
    procedure TestM_TryFinallyStatement;
    procedure TestM_TypeAlias;
    // ToDo: unary
    procedure TestM_Record;
    procedure TestM_Array;
    procedure TestM_NestedFuncResult;
    procedure TestM_Class;
    procedure TestM_Class_Property;
    procedure TestM_Class_PropertyOverride;
    procedure TestM_Class_MethodOverride;
    procedure TestM_Class_MethodOverride2;
    procedure TestM_HintLocalVarNotUsed;

    // whole program optimization
  end;

implementation

{ TCustomTestUseAnalyzer }

procedure TCustomTestUseAnalyzer.SetUp;
begin
  inherited SetUp;
  FAnalyzer:=TPasAnalyzer.Create;
  FAnalyzer.Resolver:=ResolverEngine;
end;

procedure TCustomTestUseAnalyzer.TearDown;
begin
  FreeAndNil(FAnalyzer);
  inherited TearDown;
end;

procedure TCustomTestUseAnalyzer.AnalyzeProgram;
begin
  ParseProgram;
  Analyzer.AnalyzeModule(Module);
  CheckUsedMarkers;
end;

procedure TCustomTestUseAnalyzer.ParseProgram;
begin
  try
    inherited ParseProgram;
  except
    on E: EPasAnalysis do
      begin
      raise E;
      end;
  end;
end;

procedure TCustomTestUseAnalyzer.CheckUsedMarkers;
var
  aMarker: PSrcMarker;
  p: SizeInt;
  Postfix: String;
  Elements: TFPList;
  i: Integer;
  El: TPasElement;
  ExpectedUsed: Boolean;
  FoundEl: TPAElement;
begin
  aMarker:=FirstSrcMarker;
  while aMarker<>nil do
    begin
    writeln('TCustomTestUseAnalyzer.CheckUsedMarkers ',aMarker^.Identifier,' ',aMarker^.StartCol,' ',aMarker^.EndCol);
    p:=RPos('_',aMarker^.Identifier);
    if p>1 then
      begin
      Postfix:=copy(aMarker^.Identifier,p+1);

      if Postfix='used' then
        ExpectedUsed:=true
      else if Postfix='notused' then
        ExpectedUsed:=false
      else
        RaiseErrorAtSrcMarker('TCustomTestUseAnalyzer.CheckUsedMarkers unknown postfix "'+Postfix+'"',aMarker);

      Elements:=FindElementsAt(aMarker);
      try
        FoundEl:=nil;
        for i:=0 to Elements.Count-1 do
          begin
          El:=TPasElement(Elements[i]);
          writeln('TCustomTestUseAnalyzer.CheckUsedMarkers ',aMarker^.Identifier,' ',i,'/',Elements.Count,' El=',GetObjName(El),' ',GetObjName(El.CustomData));
          FoundEl:=Analyzer.FindElement(El);
          if FoundEl<>nil then break;
          end;
        if FoundEl<>nil then
          begin
          if not ExpectedUsed then
            RaiseErrorAtSrcMarker('expected element to be *not* used, but it is marked',aMarker);
          end
        else
          begin
          if ExpectedUsed then
            RaiseErrorAtSrcMarker('expected element to be used, but it is not marked',aMarker);
          end;
      finally
        Elements.Free;
      end;
      end;
    aMarker:=aMarker^.Next;
    end;

end;

{ TTestUseAnalyzer }

procedure TTestUseAnalyzer.TestM_ProgramLocalVar;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('var {#l_notused}l: longint;');
  Add('begin');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_AssignStatement;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('var');
  Add('  {#a_notused}a: longint;');
  Add('  {#b_used}b: longint;');
  Add('  {#c_used}c: longint;');
  Add('begin');
  Add('  b:=c;');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_ForLoopStatement;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('var');
  Add('  {#a_used}a: longint;');
  Add('  {#b_used}b: longint;');
  Add('  {#c_used}c: longint;');
  Add('  {#d_used}d: longint;');
  Add('begin');
  Add('  for a:=b to c do d:=a;');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_AsmStatement;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('begin');
  Add('  asm end;');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_CaseOfStatement;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('const');
  Add('  {#a_used}a = 1;');
  Add('  {#b_used}b = 2;');
  Add('var');
  Add('  {#c_used}c: longint;');
  Add('  {#d_used}d: longint;');
  Add('begin');
  Add('  case a of');
  Add('    b: c:=1;');
  Add('  else');
  Add('    d:=2;');
  Add('  end;');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_IfThenElseStatement;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('var');
  Add('  {#a_used}a: longint;');
  Add('  {#b_used}b: longint;');
  Add('  {#c_used}c: longint;');
  Add('begin');
  Add('  if a=0 then b:=1 else c:=2;');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_WhileDoStatement;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('var');
  Add('  {#a_used}a: longint;');
  Add('  {#b_used}b: longint;');
  Add('begin');
  Add('  while a>0 do b:=1;');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_RepeatUntilStatement;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('var');
  Add('  {#a_used}a: longint;');
  Add('  {#b_used}b: longint;');
  Add('begin');
  Add('  repeat a:=1; until b>1;');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_TryFinallyStatement;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('var');
  Add('  {#a_used}a: longint;');
  Add('  {#b_used}b: longint;');
  Add('begin');
  Add('  try');
  Add('    a:=1;');
  Add('  finally');
  Add('    b:=2;');
  Add('  end;');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_TypeAlias;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('type');
  Add('  {#integer_used}integer = longint;');
  Add('var');
  Add('  {#a_used}a: integer;');
  Add('  {#b_used}b: integer;');
  Add('  {#c_notused}c: integer;');
  Add('begin');
  Add('  a:=b;');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_Record;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('type');
  Add('  {#integer_used}integer = longint;');
  Add('  {#trec_used}TRec = record');
  Add('    {#a_used}a: integer;');
  Add('    {#b_notused}b: integer;');
  Add('    {#c_used}c: integer;');
  Add('  end;');
  Add('var');
  Add('  {#r_used}r: TRec;');
  Add('begin');
  Add('  r.a:=3;');
  Add('  with r do c:=4;');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_Array;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('type');
  Add('  {#integer_used}integer = longint;');
  Add('  {#tarrayint_used}TArrayInt = array of integer;');
  Add('var');
  Add('  {#a_used}a: TArrayInt;');
  Add('  {#b_used}b: integer;');
  Add('  {#c_used}c: TArrayInt;');
  Add('  {#d_used}d: integer;');
  Add('  {#e_used}e: TArrayInt;');
  Add('  {#f_used}f: integer;');
  Add('  {#g_used}g: TArrayInt;');
  Add('  {#h_used}h: TArrayInt;');
  Add('  {#i_used}i: TArrayInt;');
  Add('begin');
  Add('  a[b]:=c[d];');
  Add('  SetLength(e,f)');
  Add('  if low(g)=high(h)+length(i) then');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_NestedFuncResult;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('type');
  Add('  {#integer_used}integer = longint;');
  Add('  {#tarrayint_used}TArrayInt = array of integer;');
  Add('  function {#nestedfunc_used}NestedFunc({#b_notused}b: longint): TArrayInt;');
  Add('  begin');
  Add('  end;');
  Add('var');
  Add('  {#d_used}d: longint;');
  Add('begin');
  Add('  NestedFunc(d);');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_Class;
begin
  StartProgram(false);
  Add('type');
  Add('  {#integer_used}integer = longint;');
  Add('  {tobject_used}TObject = class');
  Add('    {#a_used}a: integer;');
  Add('  end;');
  Add('var Obj: TObject;');
  Add('begin');
  Add('  Obj.a:=3;');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_Class_Property;
begin
  StartProgram(false);
  Add('type');
  Add('  {#integer_used}integer = longint;');
  Add('  {tobject_used}TObject = class');
  Add('    {#fa_used}Fa: integer;');
  Add('    {#fb_used}Fb: integer;');
  Add('    {#fc_used}Fc: integer;');
  Add('    {#fd_used}Fd: integer;');
  Add('    {#fe_notused}Fe: integer;');
  Add('    function {#getfc_used}GetFC: integer;');
  Add('    procedure {#setfd_used}SetFD({#setfd_value_used}Value: integer);');
  Add('    property {#A_used}A: integer read Fa write Fb;');
  Add('    property {#C_used}C: integer read GetFC write SetFD;');
  Add('  end;');
  Add('function TObject.GetFC: integer;');
  Add('begin');
  Add('  Result:=Fc;');
  Add('end;');
  Add('procedure TObject.SetFD({#setfd_value_impl_notused}Value: integer);');
  Add('begin');
  Add('  Fd:=Value;');
  Add('end;');
  Add('var Obj: TObject;');
  Add('begin');
  Add('  Obj.A:=Obj.A;');
  Add('  Obj.C:=Obj.C;');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_Class_PropertyOverride;
begin
  StartProgram(false);
  Add('type');
  Add('  {#integer_used}integer = longint;');
  Add('  {tobject_used}TObject = class');
  Add('    {#fa_used}FA: integer;');
  Add('    {#fb_notused}FB: integer;');
  Add('    property {#obj_a_notused}A: integer read FA write FB;');
  Add('  end;');
  Add('  {tmobile_used}TMobile = class(TObject)');
  Add('    {#fc_used}FC: integer;');
  Add('    property {#mob_a_used}A write FC;');
  Add('  end;');
  Add('var {#m_used}M: TMobile;');
  Add('begin');
  Add('  M.A:=M.A;');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_Class_MethodOverride;
begin
  StartProgram(false);
  Add('type');
  Add('  {#integer_used}integer = longint;');
  Add('  {tobject_used}TObject = class');
  Add('    procedure {#obj_doa_used}DoA; virtual; abstract;');
  Add('    procedure {#obj_dob_notused}DoB; virtual; abstract;');
  Add('  end;');
  Add('  {tmobile_used}TMobile = class(TObject)');
  Add('    constructor {#mob_create_used}Create;');
  Add('    procedure {#mob_doa_used}DoA; override;');
  Add('    procedure {#mob_dob_notused}DoB; override;');
  Add('  end;');
  Add('constructor TMobile.Create; begin end;');
  Add('procedure TMobile.DoA; begin end;');
  Add('procedure TMobile.DoB; begin end;');
  Add('var {#o_used}o: TObject;');
  Add('begin');
  Add('  o:=TMobile.Create;'); // use TMobile before o.DoA
  Add('  o.DoA;');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_Class_MethodOverride2;
begin
  StartProgram(false);
  Add('type');
  Add('  {#integer_used}integer = longint;');
  Add('  {tobject_used}TObject = class');
  Add('    procedure {#obj_doa_used}DoA; virtual; abstract;');
  Add('  end;');
  Add('  {tmobile_used}TMobile = class(TObject)');
  Add('    constructor {#mob_create_used}Create;');
  Add('    procedure {#mob_doa_used}DoA; override;');
  Add('  end;');
  Add('constructor TMobile.Create; begin end;');
  Add('procedure TMobile.DoA; begin end;');
  Add('var {#o_used}o: TObject;');
  Add('begin');
  Add('  o.DoA;');
  Add('  o:=TMobile.Create;'); // use TMobile after o.DoA
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_HintLocalVarNotUsed;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('var {#l_notused}l: longint;');
  Add('begin');
  Add('end;');
  Add('begin');
  AnalyzeProgram;
end;

initialization
  RegisterTests([TTestUseAnalyzer]);

end.

