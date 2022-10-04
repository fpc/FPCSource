unit TCPas2JSAnalyzer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, StrUtils, TCModules, PasTree,
  PScanner, PasResolver, PasUseAnalyzer, PasResolveEval, Pas2jsUseAnalyzer;

type

  { TCustomTestPas2jsAnalyzer }

  TCustomTestPas2jsAnalyzer = class(TCustomTestModule)
  private
    FAnalyzer: TPas2JSAnalyzer;
    FPAMessages: TFPList; // list of TPAMessage
    FPAGoodMessages: TFPList;
    FProcAnalyzer: TPas2JSAnalyzer;
    function GetPAMessages(Index: integer): TPAMessage;
    procedure OnAnalyzerMessage(Sender: TObject; Msg: TPAMessage);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ParseModule; override;
    procedure AnalyzeModule; virtual;
    procedure AnalyzeProgram; virtual;
    procedure AnalyzeUnit; virtual;
    procedure AnalyzeWholeProgram; virtual;
    procedure CheckUsedMarkers; virtual;
    procedure CheckUseAnalyzerHint(MsgType: TMessageType; MsgNumber: integer;
      const MsgText: string); virtual;
    procedure CheckUseAnalyzerUnexpectedHints; virtual;
    procedure CheckUnitUsed(const aFilename: string; Used: boolean); virtual;
    procedure CheckScopeReferences(const ScopeName: string;
      const RefNames: array of string);
  public
    property Analyzer: TPas2JSAnalyzer read FAnalyzer;
    property ProcAnalyzer: TPas2JSAnalyzer read FProcAnalyzer;
    function PAMessageCount: integer;
    property PAMessages[Index: integer]: TPAMessage read GetPAMessages;
  end;

  { TTestPas2jsAnalyzer }

  TTestPas2jsAnalyzer = class(TCustomTestPas2jsAnalyzer)
  Published
    procedure TestM_ProgramLocalVar;
    procedure TestM_PassRecordToJSValue;
    procedure TestM_StaticArrayDim2;
  end;


implementation

{ TTestPas2jsAnalyzer }

procedure TTestPas2jsAnalyzer.TestM_ProgramLocalVar;
begin
  StartProgram(false);
  Add([
  'procedure {#DoIt_used}DoIt;',
  'var {#l_notused}l: longint;',
  'begin',
  'end;',
  'begin',
  '  DoIt;',
  'end.']);
  AnalyzeProgram;
end;

procedure TTestPas2jsAnalyzer.TestM_PassRecordToJSValue;
begin
  StartProgram(false);
  Add([
  'type',
  '  {#trec_used}TRec = record',
  '    {#x_used}x: word;',
  '  end;',
  '  {#tbig_used}TBig = record',
  '    {#r_used}r: TRec;',
  '  end;',
  '  {#tnope_used}TNope = record',
  '    {#a_notused}a: word;',
  '    {#b_used}b: word;',
  '  end;',
  'procedure DoIt(v: JSValue);',
  'begin',
  'end;',
  'var big: TBig;',
  '  n: TNope;',
  'begin',
  '  DoIt(big);',
  '  DoIt(n.b);',
  'end.']);
  AnalyzeProgram;
end;

procedure TTestPas2jsAnalyzer.TestM_StaticArrayDim2;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class',
  '  end;',
  'var',
  '  oa: array [1..10, 1..10] of TObject;',
  'begin',
  'end.']);
  AnalyzeProgram;
  CheckUseAnalyzerHint(mtHint,nPALocalXYNotUsed,'Local class "TObject" not used');
  CheckUseAnalyzerHint(mtHint,nPALocalVariableNotUsed,'Local variable "oa" not used');
  CheckUseAnalyzerUnexpectedHints;
end;

{ TCustomTestPas2jsAnalyzer }

function TCustomTestPas2jsAnalyzer.GetPAMessages(Index: integer): TPAMessage;
begin
  Result:=TPAMessage(FPAMessages[Index]);
end;

procedure TCustomTestPas2jsAnalyzer.OnAnalyzerMessage(Sender: TObject;
  Msg: TPAMessage);
begin
  Msg.AddRef;
  FPAMessages.Add(Msg);
end;

procedure TCustomTestPas2jsAnalyzer.SetUp;
begin
  inherited SetUp;
  FPAMessages:=TFPList.Create;
  FPAGoodMessages:=TFPList.Create;
  FAnalyzer:=TPas2JSAnalyzer.Create;
  FAnalyzer.Resolver:=ResolverEngine;
  Analyzer.OnMessage:=@OnAnalyzerMessage;
end;

procedure TCustomTestPas2jsAnalyzer.TearDown;
var
  i: Integer;
begin
  FreeAndNil(FPAGoodMessages);
  for i:=0 to FPAMessages.Count-1 do
    TPAMessage(FPAMessages[i]).Release;
  FreeAndNil(FPAMessages);
  FreeAndNil(FAnalyzer);
  FreeAndNil(FProcAnalyzer);
  inherited TearDown;
end;

procedure TCustomTestPas2jsAnalyzer.ParseModule;
begin
  inherited ParseModule;
  if SkipTests then exit;
  CheckReferenceDirectives;
end;

procedure TCustomTestPas2jsAnalyzer.AnalyzeModule;
begin
  Analyzer.AnalyzeModule(Module);
  Analyzer.EmitModuleHints(Module);
  CheckUsedMarkers;
end;

procedure TCustomTestPas2jsAnalyzer.AnalyzeProgram;
begin
  ParseProgram;
  AnalyzeModule;
end;

procedure TCustomTestPas2jsAnalyzer.AnalyzeUnit;
begin
  ParseUnit;
  AnalyzeModule;
end;

procedure TCustomTestPas2jsAnalyzer.AnalyzeWholeProgram;
begin
  ParseProgram;
  Analyzer.AnalyzeWholeProgram(Module as TPasProgram);
  CheckUsedMarkers;
end;

procedure TCustomTestPas2jsAnalyzer.CheckUsedMarkers;
type
  TUsed = (
    uUsed,
    uNotUsed,
    uTypeInfo,
    uNoTypeinfo
    );
var
  aMarker: PSrcMarker;
  p: SizeInt;
  Postfix: String;
  Elements: TFPList;
  i: Integer;
  El, FoundEl: TPasElement;
  ExpectedUsed: TUsed;
begin
  aMarker:=FirstSrcMarker;
  while aMarker<>nil do
    begin
    {$IFDEF VerbosePasAnalyzer}
    writeln('TCustomTestPas2jsAnalyzer.CheckUsedMarkers ',aMarker^.Identifier,' Line=',aMarker^.Row,' StartCol=',aMarker^.StartCol,' EndCol=',aMarker^.EndCol);
    {$ENDIF}
    p:=RPos('_',aMarker^.Identifier);
    if p>1 then
      begin
      Postfix:=copy(aMarker^.Identifier,p+1);

      if Postfix='used' then
        ExpectedUsed:=uUsed
      else if Postfix='notused' then
        ExpectedUsed:=uNotUsed
      else if Postfix='typeinfo' then
        ExpectedUsed:=uTypeInfo
      else if Postfix='notypeinfo' then
        ExpectedUsed:=uNoTypeInfo
      else
        RaiseErrorAtSrcMarker('TCustomTestPas2jsAnalyzer.CheckUsedMarkers unknown postfix "'+Postfix+'"',aMarker);

      Elements:=FindElementsAt(aMarker);
      try
        FoundEl:=nil;
        for i:=0 to Elements.Count-1 do
          begin
          El:=TPasElement(Elements[i]);
          {$IFDEF VerbosePasAnalyzer}
          writeln('TCustomTestPas2jsAnalyzer.CheckUsedMarkers ',aMarker^.Identifier,' ',i,'/',Elements.Count,' El=',GetObjName(El),' ',GetObjName(El.CustomData));
          {$ENDIF}
          case ExpectedUsed of
          uUsed,uNotUsed:
            if Analyzer.IsUsed(El) then
              begin
              FoundEl:=El;
              break;
              end;
          uTypeInfo,uNoTypeinfo:
            if Analyzer.IsTypeInfoUsed(El) then
              begin
              FoundEl:=El;
              break;
              end;
          end;
          end;
        if FoundEl<>nil then
          case ExpectedUsed of
          uNotUsed:
            RaiseErrorAtSrcMarker('expected element to be *not* used, but it is marked',aMarker);
          uNoTypeinfo:
            RaiseErrorAtSrcMarker('expected element to have *no* typeinfo, but it is marked',aMarker);
          end
        else
          case ExpectedUsed of
          uUsed:
            RaiseErrorAtSrcMarker('expected element to be used, but it is not marked',aMarker);
          uTypeInfo:
            RaiseErrorAtSrcMarker('expected element to have typeinfo, but it is not marked',aMarker);
          end;
      finally
        Elements.Free;
      end;
      end;
    aMarker:=aMarker^.Next;
    end;
end;

procedure TCustomTestPas2jsAnalyzer.CheckUseAnalyzerHint(MsgType: TMessageType;
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
  writeln('TCustomTestPas2jsAnalyzer.CheckHasHint: ');
  for i:=0 to PAMessageCount-1 do
    begin
    Msg:=PAMessages[i];
    writeln('  ',i,'/',PAMessageCount,': [',Msg.Id,'] ',Msg.MsgType,': (',Msg.MsgNumber,') {',Msg.MsgText,'}');
    end;
  s:='';
  str(MsgType,s);
  Fail('Analyzer Message not found: '+s+': ('+IntToStr(MsgNumber)+') {'+MsgText+'}');
end;

procedure TCustomTestPas2jsAnalyzer.CheckUseAnalyzerUnexpectedHints;
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

procedure TCustomTestPas2jsAnalyzer.CheckUnitUsed(const aFilename: string;
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

procedure TCustomTestPas2jsAnalyzer.CheckScopeReferences(
  const ScopeName: string; const RefNames: array of string);
type
  TEntry = record
    Name: string;
    Access: TPSRefAccess;
  end;

var
  Entries: array of TEntry;

  procedure CheckRefs(ScopeRefs: TPasScopeReferences; const Prefix: string);

    procedure DumpRefsAndFail(Refs: TFPList; const Msg: string);
    var
      i: Integer;
      Ref: TPasScopeReference;
    begin
      {$IFDEF VerbosePasAnalyzer}
      if Refs.Count=0 then
        writeln('DumpRefsAndFail ',Prefix,' NO REFS');
      {$ENDIF}
      for i:=0 to Refs.Count-1 do
        begin
        Ref:=TPasScopeReference(Refs[i]);
        if Ref=nil then break;
        {$IFDEF VerbosePasAnalyzer}
        writeln('DumpRefsAndFail ',Prefix,' ',i,' ',GetObjName(Ref.Element),' ',Ref.Access);
        {$ENDIF}
        end;
      Fail(Prefix+': '+Msg);
    end;

  var
    Refs: TFPList;
    j, i: Integer;
    o: TObject;
    Ref: TPasScopeReference;
  begin
    if ScopeRefs=nil then
      Refs:=TFPList.Create
    else
      Refs:=ScopeRefs.GetList;
    try
      // check that Refs only contains TPasProcScopeReference
      for i:=0 to Refs.Count-1 do
        begin
        o:=TObject(Refs[i]);
        if not (o is TPasScopeReference) then
          Fail(Prefix+': Refs['+IntToStr(i)+'] '+GetObjName(o));
        end;
      // check that all Entries are referenced
      for i:=0 to length(Entries)-1 do
        begin
        j:=Refs.Count-1;
        while (j>=0)
            and (CompareText(Entries[i].Name,TPasScopeReference(Refs[j]).Element.Name)<>0) do
          dec(j);
        if j<0 then
          DumpRefsAndFail(Refs,'Missing reference "'+Entries[i].Name+'"');
        Ref:=TPasScopeReference(Refs[j]);
        if (Entries[i].Access<>psraNone) and (Ref.Access<>Entries[i].Access) then
          DumpRefsAndFail(Refs,'Wrong reference access "'+Entries[i].Name+'",'
            +' expected '+dbgs(Entries[i].Access)+', but got '+dbgs(Ref.Access));
        end;
      // check that no other references are in Refs
      for i:=0 to Refs.Count-1 do
        begin
        Ref:=TPasScopeReference(Refs[i]);
        j:=length(Entries)-1;
        while (j>=0)
            and (CompareText(Ref.Element.Name,Entries[j].Name)<>0) do
          dec(j);
        if j<0 then
          DumpRefsAndFail(Refs,'Unneeded reference "'+GetObjName(Ref.Element)+'"');
        end;
    finally
      Refs.Free;
    end;
  end;

  function FindProc(Section: TPasSection): boolean;
  var
    i: Integer;
    El: TPasElement;
    Proc: TPasProcedure;
    Scope: TPasProcedureScope;
  begin
    for i:=0 to Section.Declarations.Count-1 do
      begin
      El:=TPasElement(Section.Declarations[i]);
      if CompareText(El.Name,ScopeName)<>0 then continue;
      if not (El is TPasProcedure) then
        Fail('El is not proc '+GetObjName(El));
      Proc:=TPasProcedure(El);
      Scope:=Proc.CustomData as TPasProcedureScope;
      if Scope.DeclarationProc<>nil then continue;

      // check references created by AnalyzeModule
      CheckRefs(Scope.References,'AnalyzeModule');

      exit(true);
      end;
    Result:=false;
  end;

  procedure CheckInitialFinalization(El: TPasImplBlock);
  var
    Scope: TPasInitialFinalizationScope;
  begin
    Scope:=El.CustomData as TPasInitialFinalizationScope;
    CheckRefs(Scope.References,'AnalyzeModule');
  end;

var
  i: Integer;
begin
  Entries:=nil;
  SetLength(Entries,High(RefNames)-low(RefNames)+1);
  for i:=low(RefNames) to high(RefNames) do
    begin
    Entries[i].Name:=RefNames[i];
    Entries[i].Access:=psraNone;
    end;

  if Module is TPasProgram then
    begin
    if CompareText(ScopeName,'begin')=0 then
      begin
      // check begin-block references created by AnalyzeModule
      CheckInitialFinalization(Module.InitializationSection);
      exit;
      end
    else if FindProc(TPasProgram(Module).ProgramSection) then
      exit;
    end
  else if Module is TPasLibrary then
    begin
    if CompareText(ScopeName,'begin')=0 then
      begin
      // check begin-block references created by AnalyzeModule
      CheckInitialFinalization(Module.InitializationSection);
      exit;
      end
    else if FindProc(TPasLibrary(Module).LibrarySection) then
      exit;
    end
  else if Module.ClassType=TPasModule then
    begin
    if CompareText(ScopeName,'initialization')=0 then
      begin
      // check initialization references created by AnalyzeModule
      CheckInitialFinalization(Module.InitializationSection);
      exit;
      end
    else if CompareText(ScopeName,'finalization')=0 then
      begin
      // check finalization references created by AnalyzeModule
      CheckInitialFinalization(Module.FinalizationSection);
      exit;
      end
    else if FindProc(Module.InterfaceSection) then
      exit
    else if FindProc(Module.ImplementationSection) then
      exit;
    end;
  Fail('missing proc '+ScopeName);
end;

function TCustomTestPas2jsAnalyzer.PAMessageCount: integer;
begin
  Result:=FPAMessages.Count;
end;

Initialization
  RegisterTests([TTestPas2jsAnalyzer]);
end.

