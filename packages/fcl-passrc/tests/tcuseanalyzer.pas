{
  Examples:
    ./testpassrc --suite=TTestResolver.TestEmpty
}
unit tcuseanalyzer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, PasTree, PScanner, PasResolver, tcbaseparser,
  testregistry, strutils, tcresolver, PasUseAnalyzer, PasResolveEval;

type

  { TCustomTestUseAnalyzer }

  TCustomTestUseAnalyzer = Class(TCustomTestResolver)
  private
    FAnalyzer: TPasAnalyzer;
    FPAMessages: TFPList; // list of TPAMessage
    FPAGoodMessages: TFPList;
    function GetPAMessages(Index: integer): TPAMessage;
    procedure OnAnalyzerMessage(Sender: TObject; Msg: TPAMessage);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure AnalyzeModule; virtual;
    procedure AnalyzeProgram; virtual;
    procedure AnalyzeUnit; virtual;
    procedure AnalyzeWholeProgram; virtual;
    procedure CheckUsedMarkers; virtual;
    procedure CheckUseAnalyzerHint(MsgType: TMessageType; MsgNumber: integer;
      const MsgText: string); virtual;
    procedure CheckUseAnalyzerUnexpectedHints; virtual;
    procedure CheckUnitUsed(const aFilename: string; Used: boolean); virtual;
  public
    property Analyzer: TPasAnalyzer read FAnalyzer;
    function PAMessageCount: integer;
    property PAMessages[Index: integer]: TPAMessage read GetPAMessages;
  end;

  { TTestUseAnalyzer }

  TTestUseAnalyzer = Class(TCustomTestUseAnalyzer)
  published
    // single module
    procedure TestM_ProgramLocalVar;
    procedure TestM_AssignStatement;
    procedure TestM_BeginBlock;
    procedure TestM_ForLoopStatement;
    procedure TestM_AsmStatement;
    procedure TestM_CaseOfStatement;
    procedure TestM_IfThenElseStatement;
    procedure TestM_WhileDoStatement;
    procedure TestM_RepeatUntilStatement;
    procedure TestM_TryFinallyStatement;
    procedure TestM_TypeAlias;
    procedure TestM_Unary;
    procedure TestM_Const;
    procedure TestM_Record;
    procedure TestM_Array;
    procedure TestM_NestedFuncResult;
    procedure TestM_Enums;
    procedure TestM_ProcedureType;
    procedure TestM_Params;
    procedure TestM_Class;
    procedure TestM_ClassForward;
    procedure TestM_Class_Property;
    procedure TestM_Class_PropertyOverride;
    procedure TestM_Class_MethodOverride;
    procedure TestM_Class_MethodOverride2;
    procedure TestM_TryExceptStatement;

    // single module hints
    procedure TestM_Hint_UnitNotUsed;
    procedure TestM_Hint_UnitNotUsed_No_OnlyExternal;
    procedure TestM_Hint_ParameterNotUsed;
    procedure TestM_Hint_ParameterNotUsed_Abstract;
    procedure TestM_Hint_ParameterNotUsedTypecast;
    procedure TestM_Hint_LocalVariableNotUsed;
    procedure TestM_Hint_ForVar_No_LocalVariableNotUsed;
    procedure TestM_Hint_InterfaceUnitVariableUsed;
    procedure TestM_Hint_ValueParameterIsAssignedButNeverUsed;
    procedure TestM_Hint_LocalVariableIsAssignedButNeverUsed;
    procedure TestM_Hint_LocalXYNotUsed;
    procedure TestM_Hint_PrivateFieldIsNeverUsed;
    procedure TestM_Hint_PrivateFieldIsAssignedButNeverUsed;
    procedure TestM_Hint_PrivateMethodIsNeverUsed;
    procedure TestM_Hint_LocalDestructor_No_IsNeverUsed;
    procedure TestM_Hint_PrivateTypeNeverUsed;
    procedure TestM_Hint_PrivateConstNeverUsed;
    procedure TestM_Hint_PrivatePropertyNeverUsed;
    procedure TestM_Hint_LocalClassInProgramNotUsed;
    procedure TestM_Hint_LocalMethodInProgramNotUsed;
    procedure TestM_Hint_AssemblerParameterIgnored;
    procedure TestM_Hint_AssemblerDelphiParameterIgnored;
    procedure TestM_Hint_FunctionResultDoesNotSeemToBeSet;
    procedure TestM_Hint_FunctionResultDoesNotSeemToBeSet_Abstract;
    procedure TestM_Hint_FunctionResultRecord;
    procedure TestM_Hint_FunctionResultPassRecordElement;
    procedure TestM_Hint_OutParam_No_AssignedButNeverUsed;
    procedure TestM_Hint_ArgPassed_No_ParameterNotUsed;

    // whole program optimization
    procedure TestWP_LocalVar;
    procedure TestWP_UnitUsed;
    procedure TestWP_UnitNotUsed;
    procedure TestWP_UnitInitialization;
    procedure TestWP_UnitFinalization;
    procedure TestWP_CallInherited;
    procedure TestWP_ProgramPublicDeclarations;
    procedure TestWP_ClassDefaultProperty;
    procedure TestWP_Published;
    procedure TestWP_PublishedSetType;
    procedure TestWP_PublishedArrayType;
    procedure TestWP_PublishedClassOfType;
    procedure TestWP_PublishedRecordType;
    procedure TestWP_PublishedProcType;
    procedure TestWP_PublishedProperty;
    procedure TestWP_BuiltInFunctions;
    procedure TestWP_TypeInfo;
  end;

implementation

{ TCustomTestUseAnalyzer }

procedure TCustomTestUseAnalyzer.OnAnalyzerMessage(Sender: TObject;
  Msg: TPAMessage);
begin
  Msg.AddRef;
  FPAMessages.Add(Msg);
end;

function TCustomTestUseAnalyzer.GetPAMessages(Index: integer): TPAMessage;
begin
  Result:=TPAMessage(FPAMessages[Index]);
end;

procedure TCustomTestUseAnalyzer.SetUp;
begin
  inherited SetUp;
  FPAMessages:=TFPList.Create;
  FPAGoodMessages:=TFPList.Create;
  FAnalyzer:=TPasAnalyzer.Create;
  FAnalyzer.Resolver:=ResolverEngine;
  Analyzer.OnMessage:=@OnAnalyzerMessage;
end;

procedure TCustomTestUseAnalyzer.TearDown;
var
  i: Integer;
begin
  FreeAndNil(FPAGoodMessages);
  for i:=0 to FPAMessages.Count-1 do
    TPAMessage(FPAMessages[i]).Release;
  FreeAndNil(FPAMessages);
  FreeAndNil(FAnalyzer);
  inherited TearDown;
end;

procedure TCustomTestUseAnalyzer.AnalyzeModule;
begin
  Analyzer.AnalyzeModule(Module);
  Analyzer.EmitModuleHints(Module);
  CheckUsedMarkers;
end;

procedure TCustomTestUseAnalyzer.AnalyzeProgram;
begin
  ParseProgram;
  AnalyzeModule;
end;

procedure TCustomTestUseAnalyzer.AnalyzeUnit;
begin
  ParseUnit;
  AnalyzeModule;
end;

procedure TCustomTestUseAnalyzer.AnalyzeWholeProgram;
begin
  ParseProgram;
  Analyzer.AnalyzeWholeProgram(Module as TPasProgram);
  CheckUsedMarkers;
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
    writeln('TCustomTestUseAnalyzer.CheckUsedMarkers ',aMarker^.Identifier,' Line=',aMarker^.Row,' StartCol=',aMarker^.StartCol,' EndCol=',aMarker^.EndCol);
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

procedure TCustomTestUseAnalyzer.CheckUseAnalyzerHint(MsgType: TMessageType;
  MsgNumber: integer; const MsgText: string);
var
  i: Integer;
  Msg: TPAMessage;
  s: string;
begin
  i:=PAMessageCount-1;
  while i>=0 do
    begin
    Msg:=PAMessages[i];
    if (Msg.MsgNumber=MsgNumber) then
      begin
      if (Msg.MsgType=MsgType) and (Msg.MsgText=MsgText) then
        begin
        FPAGoodMessages.Add(Msg);
        exit;
        end;
      end;
    dec(i);
    end;
  // mismatch
  writeln('TCustomTestUseAnalyzer.CheckHasHint: ');
  for i:=0 to PAMessageCount-1 do
    begin
    Msg:=PAMessages[i];
    writeln('  ',i,'/',PAMessageCount,': [',Msg.Id,'] ',Msg.MsgType,': (',Msg.MsgNumber,') {',Msg.MsgText,'}');
    end;
  s:='';
  str(MsgType,s);
  Fail('Analyzer Message not found: '+s+': ('+IntToStr(MsgNumber)+') {'+MsgText+'}');
end;

procedure TCustomTestUseAnalyzer.CheckUseAnalyzerUnexpectedHints;
var
  i: Integer;
  Msg: TPAMessage;
  s: String;
begin
  for i:=0 to PAMessageCount-1 do
    begin
    Msg:=PAMessages[i];
    if FPAGoodMessages.IndexOf(Msg)>=0 then continue;
    s:='';
    str(Msg.MsgType,s);
    Fail('Unexpected analyzer message found ['+IntToStr(Msg.Id)+'] '+s+': ('+IntToStr(Msg.MsgNumber)+') {'+Msg.MsgText+'}');
    end;
end;

procedure TCustomTestUseAnalyzer.CheckUnitUsed(const aFilename: string;
  Used: boolean);
var
  aResolver: TTestEnginePasResolver;
  PAEl: TPAElement;
begin
  aResolver:=FindModuleWithFilename(aFilename);
  AssertNotNull('unit not found "'+aFilename+'"',aResolver);
  AssertNotNull('unit module not found "'+aFilename+'"',aResolver.Module);
  PAEl:=Analyzer.FindElement(aResolver.Module);
  if PAEl<>nil then
    begin
    // unit is used
    if not Used then
      Fail('expected unit "'+aFilename+'" not used, but it is used');
    end
  else
    begin
    // unit is not used
    if Used then
      Fail('expected unit "'+aFilename+'" used, but it is not used');
    end;
end;

function TCustomTestUseAnalyzer.PAMessageCount: integer;
begin
  Result:=FPAMessages.Count;
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
  Add('  DoIt;');
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
  Add('  DoIt;');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_BeginBlock;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('var');
  Add('  {#a_used}a: longint;');
  Add('begin');
  Add('  begin');
  Add('  a:=1;');
  Add('  end;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
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
  Add('  DoIt;');
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
  Add('  DoIt;');
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
  Add('  DoIt;');
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
  Add('  DoIt;');
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
  Add('  DoIt;');
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
  Add('  DoIt;');
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
  Add('  DoIt;');
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
  Add('  DoIt;');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_Unary;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('var');
  Add('  {#a_used}a: longint;');
  Add('  {#b_used}b: longint;');
  Add('  {#c_used}c: longint;');
  Add('  {#d_used}d: longint;');
  Add('begin');
  Add('  a:=+b;');
  Add('  a:=c+d;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_Const;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('var');
  Add('  {#a_used}a: longint;');
  Add('  {#b_used}b: boolean;');
  Add('  {#c_used}c: array of longint;');
  Add('  {#d_used}d: string;');
  Add('begin');
  Add('  a:=+1;');
  Add('  b:=true;');
  Add('  c:=nil;');
  Add('  d:=''foo'';');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
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
  Add('  DoIt;');
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
  Add('  DoIt;');
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
  Add('  DoIt;');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_Enums;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt(const o);');
  Add('type');
  Add('  {#TEnum_used}TEnum = (red,blue);');
  Add('  {#TEnums_used}TEnums = set of TEnum;');
  Add('var');
  Add('  {#a_used}a: TEnum;');
  Add('  {#b_used}b: TEnums;');
  Add('  {#c_used}c: TEnum;');
  Add('  {#d_used}d: TEnums;');
  Add('  {#e_used}e: TEnums;');
  Add('  {#f_used}f: TEnums;');
  Add('  {#g_used}g: TEnum;');
  Add('  {#h_used}h: TEnum;');
  Add('begin');
  Add('  b:=[a];');
  Add('  if c in d then;');
  Add('  if low(e)=high(f) then;');
  Add('  if pred(g)=succ(h) then;');
  Add('end;');
  Add('var {#s_used}s: string;');
  Add('begin');
  Add('  DoIt(s);');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_ProcedureType;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt;');
  Add('type');
  Add('  {#TProc_used}TProc = procedure;');
  Add('  {#TFunc_used}TFunc = function(): longint;');
  Add('var');
  Add('  {#p_used}p: TProc;');
  Add('  {#f_used}f: TFunc;');
  Add('begin');
  Add('  p:=nil;');
  Add('  f:=nil;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_Params;
begin
  StartProgram(false);
  Add('procedure {#DoIt_used}DoIt(const o);');
  Add('type');
  Add('  {#TEnum_used}TEnum = (red,blue);');
  Add('var');
  Add('  {#a_used}a: longint;');
  Add('  {#b_used}b: string;');
  Add('  {#c_used}c: longint;');
  Add('  {#d_used}d: TEnum;');
  Add('begin');
  Add('  DoIt(a);');
  Add('  DoIt(b[c]);');
  Add('  DoIt([d]);');
  Add('  DoIt(red);');
  Add('end;');
  Add('var {#s_used}s: string;');
  Add('begin');
  Add('  DoIt(s);');
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

procedure TTestUseAnalyzer.TestM_ClassForward;
begin
  StartProgram(false);
  Add('type');
  Add('  {#integer_notused}integer = longint;');
  Add('  {#TObject_used}TObject = class end;');
  Add('  TFelidae = class;');
  Add('  {#TCheetah_used}TCheetah = class');
  Add('  public');
  Add('    {#i_notused}i: integer;');
  Add('    {#f_used}f: TFelidae;');
  Add('  end;');
  Add('  {TFelidae_used}TFelidae = class');
  Add('  end;');
  Add('var {#c_used}c: TCheetah;');
  Add('begin');
  Add('  c.f:=nil;');
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
  Add('  {tobject_used}TObject = class');
  Add('    procedure {#obj_doa_used}DoA; virtual; abstract;');
  Add('    procedure {#obj_dob_notused}DoB; virtual; abstract;');
  Add('  end;');
  Add('  {tmobile_used}TMobile = class(TObject)');
  Add('    constructor {#mob_create_used}Create;');
  Add('    procedure {#mob_doa_used}DoA; override;');
  Add('    procedure {#mob_dob_used}DoB; override;');
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

procedure TTestUseAnalyzer.TestM_TryExceptStatement;
begin
  StartProgram(false);
  Add('type');
  Add('  {tobject_used}TObject = class');
  Add('    constructor Create; external name ''create'';');
  Add('  end;');
  Add('  {texception_used}Exception = class(TObject);');
  Add('  {tdivbyzero_used}EDivByZero = class(Exception);');
  Add('procedure {#DoIt_used}DoIt;');
  Add('var');
  Add('  {#a_used}a: Exception;');
  Add('  {#b_used}b: Exception;');
  Add('  {#c_used}c: Exception;');
  Add('  {#d_used}d: Exception;');
  Add('  {#f_used}f: Exception;');
  Add('begin');
  Add('  try');
  Add('    a:=nil;');
  Add('  except');
  Add('    raise b;');
  Add('  end;');
  Add('  try');
  Add('    if Assigned(c) then ;');
  Add('  except');
  Add('    on {#e1_used}E1: Exception do raise;');
  Add('    on {#e2_notused}E2: EDivByZero do raise d;');
  Add('    else f:=nil;');
  Add('  end;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  AnalyzeProgram;
end;

procedure TTestUseAnalyzer.TestM_Hint_UnitNotUsed;
begin
  AddModuleWithIntfImplSrc('unit2.pp',
    LinesToStr([
    'var i: longint;',
    'procedure DoIt;',
    '']),
    LinesToStr([
    'procedure DoIt; begin end;']));

  StartProgram(true);
  Add('uses unit2;');
  Add('begin');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPAUnitNotUsed,'Unit "unit2" not used in afile');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_UnitNotUsed_No_OnlyExternal;
begin
  AddModuleWithIntfImplSrc('unit2.pp',
    LinesToStr([
    'var State: longint; external name ''state'';',
    'procedure DoIt; external name ''doing'';',
    '']),
    LinesToStr([
    ]));

  StartProgram(true);
  Add('uses unit2;');
  Add('begin');
  Add('  State:=3;');
  Add('  DoIt;');
  AnalyzeProgram;

  // unit hints: no hint, even though no code is actually used
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_ParameterNotUsed;
begin
  StartProgram(true);
  Add('procedure DoIt(i: longint);');
  Add('begin end;');
  Add('begin');
  Add('  DoIt(1);');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPAParameterNotUsed,'Parameter "i" not used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_ParameterNotUsed_Abstract;
begin
  StartProgram(true);
  Add('type');
  Add('  TObject = class');
  Add('    class procedure DoIt(i: longint); virtual; abstract;');
  Add('  end;');
  Add('begin');
  Add('  TObject.DoIt(3);');
  AnalyzeProgram;
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_ParameterNotUsedTypecast;
begin
  StartProgram(true);
  Add('type');
  Add('  TObject = class end;');
  Add('  TSortCompare = function(a,b: Pointer): integer;');
  Add('  TObjCompare = function(a,b: TObject): integer;');
  Add('procedure Sort(const Compare: TSortCompare);');
  Add('begin');
  Add('  Compare(nil,nil);');
  Add('end;');
  Add('procedure DoIt(const Compare: TObjCompare);');
  Add('begin');
  Add('  Sort(TSortCompare(Compare));');
  Add('end;');
  Add('begin');
  Add('  DoIt(nil);');
  AnalyzeProgram;
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_LocalVariableNotUsed;
begin
  StartProgram(true);
  Add('procedure DoIt;');
  Add('const');
  Add('  a = 13;');
  Add('  b: longint = 14;');
  Add('var');
  Add('  c: char;');
  Add('  d: longint = 15;');
  Add('begin end;');
  Add('begin');
  Add('  DoIt;');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local constant "a" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local constant "b" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalVariableNotUsed,'Local variable "c" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalVariableNotUsed,'Local variable "d" not used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_ForVar_No_LocalVariableNotUsed;
begin
  StartProgram(false);
  Add([
  'procedure DoIt;',
  'var i: longint;',
  'begin',
  '  for i:=1 to 2 do ;',
  'end;',
  'begin',
  '  DoIt;',
  '']);
  AnalyzeProgram;
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_InterfaceUnitVariableUsed;
begin
  StartUnit(true);
  Add('interface');
  Add('const {#a_used}a = 1;');
  Add('const {#b_used}b: longint = 2;');
  Add('var {#c_used}c: longint = 3;');
  Add('type');
  Add('  {#TColor_used}TColor = longint;');
  Add('  {#TFlag_used}TFlag = (red,green);');
  Add('  {#TFlags_used}TFlags = set of TFlag;');
  Add('  {#TArrInt_used}TArrInt = array of integer;');
  Add('implementation');
  Add('const {#d_notused}d = 1;');
  Add('const {#e_notused}e: longint = 2;');
  Add('var {#f_notused}f: longint = 3;');
  Add('type');
  Add('  {#ImpTColor_notused}ImpTColor = longint;');
  Add('  {#ImpTFlag_notused}ImpTFlag = (red,green);');
  Add('  {#ImpTFlags_notused}ImpTFlags = set of TFlag;');
  Add('  {#ImpTArrInt_notused}ImpTArrInt = array of integer;');
  AnalyzeUnit;
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local constant "d" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local constant "e" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalVariableNotUsed,'Local variable "f" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local alias type "ImpTColor" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local enumeration type "ImpTFlag" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local set type "ImpTFlags" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local array type "ImpTArrInt" not used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_ValueParameterIsAssignedButNeverUsed;
begin
  StartProgram(true);
  Add('procedure DoIt(i: longint);');
  Add('begin');
  Add('  i:=3;');
  Add('end;');
  Add('begin');
  Add('  DoIt(1);');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPAValueParameterIsAssignedButNeverUsed,
    'Value parameter "i" is assigned but never used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_LocalVariableIsAssignedButNeverUsed;
begin
  StartProgram(true);
  Add('procedure DoIt;');
  Add('const');
  Add('  a: longint = 14;');
  Add('var');
  Add('  b: char;');
  Add('  c: longint = 15;');
  Add('begin');
  Add('  a:=16;');
  Add('  b:=#65;');
  Add('  c:=17;');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPALocalVariableIsAssignedButNeverUsed,
    'Local variable "a" is assigned but never used');
  CheckUseAnalyzerHint(mtHint,nPALocalVariableIsAssignedButNeverUsed,
    'Local variable "b" is assigned but never used');
  CheckUseAnalyzerHint(mtHint,nPALocalVariableIsAssignedButNeverUsed,
    'Local variable "c" is assigned but never used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_LocalXYNotUsed;
begin
  StartProgram(true);
  Add('procedure DoIt;');
  Add('type');
  Add('  TColor = longint;');
  Add('  TFlag = (red,green);');
  Add('  TFlags = set of TFlag;');
  Add('  TArrInt = array of integer;');
  Add('  procedure Sub; begin end;');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  DoIt;');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local alias type "TColor" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local enumeration type "TFlag" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local set type "TFlags" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local array type "TArrInt" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local procedure "Sub" not used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_PrivateFieldIsNeverUsed;
begin
  StartProgram(true,[supTObject]);
  Add('type');
  Add('  TMobile = class');
  Add('  private');
  Add('    a: longint;');
  Add('  end;');
  Add('var m: TMobile;');
  Add('begin');
  Add('  m:=nil;');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPAPrivateFieldIsNeverUsed,
    'Private field "TMobile.a" is never used');
  CheckUseAnalyzerHint(mtHint,nPALocalVariableIsAssignedButNeverUsed,
    'Local variable "m" is assigned but never used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_PrivateFieldIsAssignedButNeverUsed;
begin
  StartProgram(true,[supTObject]);
  Add('type');
  Add('  TMobile = class');
  Add('  private');
  Add('    a: longint;');
  Add('  public');
  Add('    constructor Create;');
  Add('  end;');
  Add('constructor TMobile.Create;');
  Add('begin');
  Add('  a:=3;');
  Add('end;');
  Add('begin');
  Add('  TMobile.Create;');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPAPrivateFieldIsAssignedButNeverUsed,
    'Private field "TMobile.a" is assigned but never used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_PrivateMethodIsNeverUsed;
begin
  StartProgram(true,[supTObject]);
  Add('type');
  Add('  TMobile = class');
  Add('  private');
  Add('    procedure DoSome; external name ''foo'';');
  Add('  public');
  Add('    constructor Create;');
  Add('  end;');
  Add('constructor TMobile.Create;');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  TMobile.Create;');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPAPrivateMethodIsNeverUsed,
    'Private method "TMobile.DoSome" is never used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_LocalDestructor_No_IsNeverUsed;
begin
  StartProgram(true,[supTObject]);
  Add('type');
  Add('  TMobile = class');
  Add('  private');
  Add('  public');
  Add('    constructor Create;');
  Add('    destructor Destroy; override;');
  Add('  end;');
  Add('var DestroyCount: longint = 0;');
  Add('constructor TMobile.Create;');
  Add('begin');
  Add('end;');
  Add('destructor TMobile.Destroy;');
  Add('begin');
  Add('  inc(DestroyCount);');
  Add('  inherited;');
  Add('end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o:=TMobile.Create;');
  Add('  o.Destroy;');
  AnalyzeProgram;
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_PrivateTypeNeverUsed;
begin
  StartProgram(true,[supTObject]);
  Add('type');
  Add('  TMobile = class');
  Add('  private');
  Add('    type t = longint;');
  Add('  public');
  Add('    constructor Create;');
  Add('  end;');
  Add('constructor TMobile.Create;');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  TMobile.Create;');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPAPrivateTypeXNeverUsed,
    'Private type "TMobile.t" never used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_PrivateConstNeverUsed;
begin
  StartProgram(true,[supTObject]);
  Add('type');
  Add('  TMobile = class');
  Add('  private');
  Add('    const c = 3;');
  Add('  public');
  Add('    constructor Create;');
  Add('  end;');
  Add('constructor TMobile.Create;');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  TMobile.Create;');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPAPrivateConstXNeverUsed,
    'Private const "TMobile.c" never used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_PrivatePropertyNeverUsed;
begin
  StartProgram(true,[supTObject]);
  Add('type');
  Add('  TMobile = class');
  Add('  private');
  Add('    FA: longint;');
  Add('    property A: longint read FA;');
  Add('  public');
  Add('    constructor Create;');
  Add('  end;');
  Add('constructor TMobile.Create;');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  TMobile.Create;');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPAPrivatePropertyXNeverUsed,
    'Private property "TMobile.A" never used');
  CheckUseAnalyzerHint(mtHint,nPAPrivateFieldIsNeverUsed,
    'Private field "TMobile.FA" is never used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_LocalClassInProgramNotUsed;
begin
  StartProgram(true,[supTObject]);
  Add('type');
  Add('  TMobile = class');
  Add('  public');
  Add('    constructor Create;');
  Add('  end;');
  Add('constructor TMobile.Create;');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  m: TMobile;');
  Add('begin');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local class "TMobile" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalVariableNotUsed,'Local variable "m" not used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_LocalMethodInProgramNotUsed;
begin
  StartProgram(true,[supTObject]);
  Add('type');
  Add('  TMobile = class');
  Add('  public');
  Add('    constructor Create;');
  Add('  end;');
  Add('constructor TMobile.Create;');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  m: TMobile;');
  Add('begin');
  Add('  if m=nil then ;');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local constructor "Create" not used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_AssemblerParameterIgnored;
begin
  StartProgram(true);
  Add('procedure DoIt(i: longint); assembler;');
  Add('type');
  Add('  {#tcolor_notused}TColor = longint;');
  Add('  {#tflag_notused}TFlag = (red,green);');
  Add('  {#tflags_notused}TFlags = set of TFlag;');
  Add('  {#tarrint_notused}TArrInt = array of integer;');
  Add('const');
  Add('  {#a_notused}a = 13;');
  Add('  {#b_notused}b: longint = 14;');
  Add('var');
  Add('  {#c_notused}c: char;');
  Add('  {#d_notused}d: longint = 15;');
  Add('  procedure {#sub_notused}Sub; begin end;');
  Add('asm end;');
  Add('begin');
  Add('  DoIt(1);');
  AnalyzeProgram;
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_AssemblerDelphiParameterIgnored;
begin
  StartProgram(true);
  Add([
  '{$mode Delphi}',
  'procedure DoIt(i: longint);',
  'type',
  '  {#tcolor_notused}TColor = longint;',
  '  {#tflag_notused}TFlag = (red,green);',
  '  {#tflags_notused}TFlags = set of TFlag;',
  '  {#tarrint_notused}TArrInt = array of integer;',
  'const',
  '  {#a_notused}a = 13;',
  '  {#b_notused}b: longint = 14;',
  'var',
  '  {#c_notused}c: char;',
  '  {#d_notused}d: longint = 15;',
  '  procedure {#sub_notused}Sub; begin end;',
  'asm end;',
  'begin',
  '  DoIt(1);',
  '']);
  AnalyzeProgram;
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_FunctionResultDoesNotSeemToBeSet;
begin
  StartProgram(true);
  Add('function DoIt: longint;');
  Add('begin end;');
  Add('begin');
  Add('  DoIt();');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPAFunctionResultDoesNotSeemToBeSet,
    sPAFunctionResultDoesNotSeemToBeSet);
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_FunctionResultDoesNotSeemToBeSet_Abstract;
begin
  StartProgram(true);
  Add('type');
  Add('  TObject = class');
  Add('    class function DoIt: longint; virtual; abstract;');
  Add('  end;');
  Add('begin');
  Add('  TObject.DoIt;');
  AnalyzeProgram;
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_FunctionResultRecord;
begin
  StartProgram(true);
  Add('type');
  Add('  TPoint = record X,Y:longint; end;');
  Add('function Point(Left: longint): TPoint;');
  Add('begin');
  Add('  Result.X:=Left;');
  Add('end;');
  Add('begin');
  Add('  Point(1);');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPALocalVariableIsAssignedButNeverUsed,
    'Local variable "X" is assigned but never used');
  CheckUseAnalyzerHint(mtHint,nPALocalVariableNotUsed,'Local variable "Y" not used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_FunctionResultPassRecordElement;
begin
  StartProgram(true);
  Add('type');
  Add('  TPoint = record X,Y:longint; end;');
  Add('procedure Three(out x: longint);');
  Add('begin');
  Add('  x:=3;');
  Add('end;');
  Add('function Point(): TPoint;');
  Add('begin');
  Add('  Three(Result.X)');
  Add('end;');
  Add('begin');
  Add('  Point();');
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPALocalVariableNotUsed,'Local variable "Y" not used');
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_OutParam_No_AssignedButNeverUsed;
begin
  StartProgram(true);
  Add('procedure DoIt(out x: longint);');
  Add('begin');
  Add('  x:=3;');
  Add('end;');
  Add('var i: longint;');
  Add('begin');
  Add('  DoIt(i);');
  AnalyzeProgram;
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestM_Hint_ArgPassed_No_ParameterNotUsed;
begin
  StartProgram(false);
  Add([
  'procedure AssertTrue(b: boolean);',
  'begin',
  '  if b then ;',
  'end;',
  'procedure AssertFalse(b: boolean);',
  'begin',
  '  AssertTrue(not b);',
  'end;',
  'begin',
  '  AssertFalse(true);',
  '']);
  AnalyzeProgram;
  CheckUseAnalyzerUnexpectedHints;
end;

procedure TTestUseAnalyzer.TestWP_LocalVar;
begin
  StartProgram(false);
  Add('var {#a_notused}a: longint;');
  Add('var {#b_used}b: longint;');
  Add('var {#c_used}c: longint;');
  Add('begin');
  Add('  b:=2;');
  Add('  afile.c:=3;');
  AnalyzeWholeProgram;
end;

procedure TTestUseAnalyzer.TestWP_UnitUsed;
begin
  AddModuleWithIntfImplSrc('unit2.pp',
    LinesToStr([
    'var i: longint;',
    'procedure DoIt;',
    '']),
    LinesToStr([
    'procedure DoIt; begin end;']));

  StartProgram(true);
  Add('uses unit2;');
  Add('begin');
  Add('  i:=3;');
  AnalyzeWholeProgram;

  CheckUnitUsed('unit2.pp',true);
end;

procedure TTestUseAnalyzer.TestWP_UnitNotUsed;
begin
  AddModuleWithIntfImplSrc('unit2.pp',
    LinesToStr([
    'var i: longint;',
    'procedure DoIt;',
    '']),
    LinesToStr([
    'procedure DoIt; begin end;']));

  StartProgram(true);
  Add('uses unit2;');
  Add('begin');
  AnalyzeWholeProgram;

  CheckUnitUsed('unit2.pp',false);
end;

procedure TTestUseAnalyzer.TestWP_UnitInitialization;
begin
  AddModuleWithIntfImplSrc('unit1.pp',
    LinesToStr([
    'uses unit2;',
    '']),
    LinesToStr([
    'initialization',
    'i:=2;']));

  AddModuleWithIntfImplSrc('unit2.pp',
    LinesToStr([
    'var i: longint;',
    '']),
    LinesToStr([
    '']));

  StartProgram(true);
  Add('uses unit1;');
  Add('begin');
  AnalyzeWholeProgram;

  CheckUnitUsed('unit1.pp',true);
  CheckUnitUsed('unit2.pp',true);
end;

procedure TTestUseAnalyzer.TestWP_UnitFinalization;
begin
  AddModuleWithIntfImplSrc('unit1.pp',
    LinesToStr([
    'uses unit2;',
    '']),
    LinesToStr([
    'finalization',
    'i:=2;']));

  AddModuleWithIntfImplSrc('unit2.pp',
    LinesToStr([
    'var i: longint;',
    '']),
    LinesToStr([
    '']));

  StartProgram(true);
  Add('uses unit1;');
  Add('begin');
  AnalyzeWholeProgram;

  CheckUnitUsed('unit1.pp',true);
  CheckUnitUsed('unit2.pp',true);
end;

procedure TTestUseAnalyzer.TestWP_CallInherited;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TObject_used}TObject = class');
  Add('    procedure {#TObjectDoA_used}DoA;');
  Add('    procedure {#TObjectDoB_used}DoB;');
  Add('  end;');
  Add('  {#TMobile_used}TMobile = class');
  Add('    procedure {#TMobileDoA_used}DoA;');
  Add('    procedure {#TMobileDoC_used}DoC;');
  Add('  end;');
  Add('procedure TObject.DoA; begin end;');
  Add('procedure TObject.DoB; begin end;');
  Add('procedure TMobile.DoA;');
  Add('begin');
  Add('  inherited;');
  Add('end;');
  Add('procedure TMobile.DoC;');
  Add('begin');
  Add('  inherited DoB;');
  Add('end;');
  Add('var o: TMobile;');
  Add('begin');
  Add('  o.DoA;');
  Add('  o.DoC;');
  AnalyzeWholeProgram;
end;

procedure TTestUseAnalyzer.TestWP_ProgramPublicDeclarations;
begin
  StartProgram(false);
  Add('var');
  Add('  {#vPublic_used}vPublic: longint; public;');
  Add('  {#vPrivate_notused}vPrivate: longint;');
  Add('procedure {#DoPublic_used}DoPublic; public; begin end;');
  Add('procedure {#DoPrivate_notused}DoPrivate; begin end;');
  Add('begin');
  AnalyzeWholeProgram;
end;

procedure TTestUseAnalyzer.TestWP_ClassDefaultProperty;
begin
  StartProgram(false);
  Add('type');
  Add('  {#tobject_used}TObject = class');
  Add('    function {#getitems_notused}Getitems(Index: longint): string;');
  Add('    procedure {#setitems_used}Setitems(Index: longint; Value: String);');
  Add('    property {#items_used}Items[Index: longint]: string read GetItems write SetItems; default;');
  Add('  end;');
  Add('function TObject.Getitems(Index: longint): string; begin end;');
  Add('procedure TObject.Setitems(Index: longint; Value: String); begin end;');
  Add('var');
  Add('  {#l_used}L: TObject;');
  Add('begin');
  Add('  L[0]:=''birdy'';');
  AnalyzeWholeProgram;
end;

procedure TTestUseAnalyzer.TestWP_Published;
begin
  StartProgram(false);
  Add('type');
  Add('  {#tobject_used}TObject = class');
  Add('  private');
  Add('    {#fcol_used}FCol: string;');
  Add('    {#fbird_notused}FBird: string;');
  Add('  published');
  Add('    {#fielda_used}FieldA: longint;');
  Add('    procedure {#doit_used}ProcA; virtual; abstract;');
  Add('    property {#col_used}Col: string read FCol;');
  Add('  end;');
  Add('var');
  Add('  {#o_used}o: TObject;');
  Add('begin');
  Add('  o:=nil;');
  AnalyzeWholeProgram;
end;

procedure TTestUseAnalyzer.TestWP_PublishedSetType;
begin
  StartProgram(false);
  Add('type');
  Add('  {#tflag_used}TFlag = (red, green);');
  Add('  {#tflags_used}TFlags = set of TFlag;');
  Add('  {#tobject_used}TObject = class');
  Add('  published');
  Add('    {#fielda_used}FieldA: TFlag;');
  Add('    {#fieldb_used}FieldB: TFlags;');
  Add('  end;');
  Add('var');
  Add('  {#o_used}o: TObject;');
  Add('begin');
  Add('  o:=nil;');
  AnalyzeWholeProgram;
end;

procedure TTestUseAnalyzer.TestWP_PublishedArrayType;
begin
  StartProgram(false);
  Add('type');
  Add('  {#tdynarr_used}TDynArr = array of longint;');
  Add('  {#tstatarr_used}TStatArr = array[boolean] of longint;');
  Add('  {#tobject_used}TObject = class');
  Add('  published');
  Add('    {#fielda_used}FieldA: TDynArr;');
  Add('    {#fieldb_used}FieldB: TStatArr;');
  Add('  end;');
  Add('var');
  Add('  {#o_used}o: TObject;');
  Add('begin');
  Add('  o:=nil;');
  AnalyzeWholeProgram;
end;

procedure TTestUseAnalyzer.TestWP_PublishedClassOfType;
begin
  StartProgram(false);
  Add('type');
  Add('  {#tobjectclass_used}TObjectClass = class of TObject;');
  Add('  {#tobject_used}TObject = class');
  Add('  published');
  Add('    {#fielda_used}FieldA: TObjectClass;');
  Add('  end;');
  Add('  {#tclass_used}TClass = class of TObject;');
  Add('var');
  Add('  {#c_used}c: TClass;');
  Add('begin');
  Add('  c:=nil;');
  AnalyzeWholeProgram;
end;

procedure TTestUseAnalyzer.TestWP_PublishedRecordType;
begin
  StartProgram(false);
  Add('type');
  Add('  {#trec_used}TRec = record');
  Add('    {treci_used}i: longint;');
  Add('  end;');
  Add('  {#tobject_used}TObject = class');
  Add('  published');
  Add('    {#fielda_used}FieldA: TRec;');
  Add('  end;');
  Add('var');
  Add('  {#o_used}o: TObject;');
  Add('begin');
  Add('  o:=nil;');
  AnalyzeWholeProgram;
end;

procedure TTestUseAnalyzer.TestWP_PublishedProcType;
begin
  StartProgram(false);
  Add('type');
  Add('  {#ta_used}ta = array of longint;');
  Add('  {#tb_used}tb = array of longint;');
  Add('  {#tproca_used}TProcA = procedure;');
  Add('  {#tfunca_used}TFuncA = function: ta;');
  Add('  {#tprocb_used}TProcB = procedure(a: tb);');
  Add('  {#tobject_used}TObject = class');
  Add('  published');
  Add('    {#fielda_used}FieldA: TProcA;');
  Add('    {#fieldb_used}FieldB: TFuncA;');
  Add('    {#fieldc_used}FieldC: TProcB;');
  Add('  end;');
  Add('var');
  Add('  {#o_used}o: TObject;');
  Add('begin');
  Add('  o:=nil;');
  AnalyzeWholeProgram;
end;

procedure TTestUseAnalyzer.TestWP_PublishedProperty;
begin
  StartProgram(false);
  Add('const');
  Add('  {#defcol_used}DefCol = 3;');
  Add('  {#defsize_notused}DefSize = 43;');
  Add('type');
  Add('  {#tobject_used}TObject = class');
  Add('  private');
  Add('    {#fcol_used}FCol: longint;');
  Add('    {#fsize_used}FSize: longint;');
  Add('    {#fbird_notused}FBird: string;');
  Add('    {#fcolstored_used}FColStored: boolean;');
  Add('    {#fsizestored_notused}FSizeStored: boolean;');
  Add('  public');
  Add('    property {#size_used}Size: longint read FSize stored FSizeStored default DefSize;');
  Add('  published');
  Add('    property {#col_used}Col: longint read FCol stored FColStored default DefCol;');
  Add('  end;');
  Add('var');
  Add('  {#o_used}o: TObject;');
  Add('begin');
  Add('  if o.Size=13 then ;');
  AnalyzeWholeProgram;
end;

procedure TTestUseAnalyzer.TestWP_BuiltInFunctions;
begin
  StartProgram(false);
  Add([
  'type',
  '  {#tordenum_used}TOrdEnum = (ordenum1,ordenum2);',
  'begin',
  '  if ord(ordenum1)=1 then ;',
  '']);
  AnalyzeWholeProgram;
end;

procedure TTestUseAnalyzer.TestWP_TypeInfo;
begin
  StartProgram(false);
  Add([
  'type',
  '  {#integer_used}integer = longint;',
  '  {#trec_used}TRec = record',
  '    {#trecv_used}v: integer;',
  '  end;',
  '  {#tclass_used}TClass = class of TObject;',
  '  {#tobject_used}TObject = class',
  '    class function {#tobject_classtype_used}ClassType: TClass; virtual; abstract;',
  '  end;',
  '  {#tbirds_used}TBirds = class of TBird;',
  '  {#tbird_used}TBird = class',
  '  end;',
  'function {#getbirdclass_used}GetBirdClass: TBirds;',
  'begin',
  '  Result:=nil;',
  'end;',
  'var',
  '  {#i_used}i: integer;',
  '  {#s_used}s: string;',
  '  {#p_used}p: pointer;',
  '  {#r_used}r: TRec;',
  '  {#o_used}o: TObject;',
  '  {#c_used}c: TClass;',
  'begin',
  '  p:=typeinfo(integer);',
  '  p:=typeinfo(longint);',
  '  p:=typeinfo(i);',
  '  p:=typeinfo(s);',
  '  p:=typeinfo(p);',
  '  p:=typeinfo(r.v);',
  '  p:=typeinfo(TObject.ClassType);',
  '  p:=typeinfo(o.ClassType);',
  '  p:=typeinfo(o);',
  '  p:=typeinfo(c);',
  '  p:=typeinfo(c.ClassType);',
  '  p:=typeinfo(GetBirdClass);',
  '']);
  AnalyzeWholeProgram;
end;

initialization
  RegisterTests([TTestUseAnalyzer]);

end.

