{%MainUnit pasresolver.pp}
{ **********************************************************************

    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - Statements
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }

procedure TPasResolver.EmitTypeHints(PosEl: TPasElement; aType: TPasType);
begin
  while aType<>nil do
    begin
    if EmitElementHints(PosEl,aType) then
      exit; // give only hints for the nearest
    if aType.InheritsFrom(TPasAliasType) then
      aType:=TPasAliasType(aType).DestType
    else if aType.ClassType=TPasPointerType then
      aType:=TPasPointerType(aType).DestType
    else if (aType.ClassType=TPasClassType) and TPasClassType(aType).IsForward
        and (aType.CustomData is TResolvedReference) then
      aType:=TPasType(TResolvedReference(aType.CustomData).Declaration)
    else
      exit;
    end;
end;

function TPasResolver.EmitElementHints(PosEl, El: TPasElement): boolean;
begin
  if IsElementSkipped(El) then
    RaiseNotYetImplemented(20170927160030,PosEl,GetObjName(El));
  if El.Hints=[] then exit(false);
  Result:=true;
  if hDeprecated in El.Hints then
    begin
    if El.HintMessage<>'' then
      LogMsg(20170422160807,mtWarning,nSymbolXIsDeprecatedY,sSymbolXIsDeprecatedY,
        [El.Name,El.HintMessage],PosEl)
    else
      LogMsg(20170419190434,mtWarning,nSymbolXIsDeprecated,sSymbolXIsDeprecated,
        [El.Name],PosEl);
    end;
  if hLibrary in El.Hints then
    LogMsg(20170419190426,mtWarning,nSymbolXBelongsToALibrary,sSymbolXBelongsToALibrary,
      [El.Name],PosEl);
  if hPlatform in El.Hints then
    LogMsg(20170419185916,mtWarning,nSymbolXIsNotPortable,sSymbolXIsNotPortable,
      [El.Name],PosEl);
  if hExperimental in El.Hints then
    LogMsg(20170419190111,mtWarning,nSymbolXIsExperimental,sSymbolXIsExperimental,
      [El.Name],PosEl);
  if hUnimplemented in El.Hints then
    LogMsg(20170419190317,mtWarning,nSymbolXIsNotImplemented,sSymbolXIsNotImplemented,
      [El.Name],PosEl);
end;

procedure TPasResolver.StoreScannerFlagsInProc(ProcScope: TPasProcedureScope);
var
  ModScope: TPasModuleScope;
begin
  if ppsfIsSpecialized in ProcScope.Flags then exit;
  ProcScope.BoolSwitches:=CurrentParser.Scanner.CurrentBoolSwitches;
  if bsRangeChecks in ProcScope.BoolSwitches then
    begin
    ModScope:=RootElement.CustomData as TPasModuleScope;
    Include(ModScope.Flags,pmsfRangeErrorNeeded);
    end;
end;

procedure TPasResolver.ReplaceProcScopeImplArgsWithDeclArgs(
  ImplProcScope: TPasProcedureScope);
var
  DeclProc, ImplProc: TPasProcedure;
  DeclArgs, ImplArgs, ImplTemplates, DeclTemplates: TFPList;
  i, j: Integer;
  DeclArg, ImplArg: TPasArgument;
  Identifier: TPasIdentifier;
  ImplNameParts: TProcedureNameParts;
  ImplNamePart: TProcedureNamePart;
  ImplTemplType, DeclTemplType: TPasGenericTemplateType;
  FuncType: TPasFunctionType;
begin
  ImplProc:=ImplProcScope.Element as TPasProcedure;
  DeclProc:=ImplProcScope.DeclarationProc;

  // redirect impl generic template types with declaration types
  ImplNameParts:=ImplProc.NameParts;
  if ImplNameParts<>nil then
    begin
    // For example: "procedure TA<T>.Fly<U>;"

    // The generic type templates (e.g. "T") are in the class
    // -> remove generic type templates from proc scope
    for i:=0 to ImplNameParts.Count-2 do
      begin
      ImplNamePart:=TProcedureNamePart(ImplNameParts[i]);
      ImplTemplates:=ImplNamePart.Templates;
      if ImplTemplates=nil then continue;
      for j:=0 to ImplTemplates.Count-1 do
        begin
        ImplTemplType:=TPasGenericTemplateType(ImplTemplates[j]);
        ImplProcScope.RemoveLocalIdentifier(ImplTemplType);
        end;
      end;
    // redirect implproc parameters to declproc parameters
    ImplTemplates:=GetProcTemplateTypes(ImplProc);
    DeclTemplates:=GetProcTemplateTypes(DeclProc);
    if ImplTemplates<>nil then
      begin
      if (DeclTemplates=nil) or (ImplTemplates.Count<>DeclTemplates.Count) then
        RaiseNotYetImplemented(20190912153602,ImplProc); // inconsistency
      for i:=0 to ImplTemplates.Count-1 do
        begin
        DeclTemplType:=TPasGenericTemplateType(DeclTemplates[i]);
        ImplTemplType:=TPasGenericTemplateType(ImplTemplates[i]);
        Identifier:=ImplProcScope.FindLocalIdentifier(ImplTemplType.Name);
        if Identifier.Element<>ImplTemplType then
          RaiseInternalError(20190912154009,GetObjName(DeclTemplType)+' '+GetObjName(ImplTemplType));
        Identifier.Element:=DeclTemplType;
        Identifier.Identifier:=DeclTemplType.Name;
        end;
      end
    else if DeclTemplates<>nil then
      // declproc is parameterized, implproc is not
      RaiseNotYetImplemented(20190912153439,ImplProc); // inconsistency
    end;

  // redirect impl arguments to declaration args
  ImplArgs:=ImplProc.ProcType.Args;
  DeclArgs:=DeclProc.ProcType.Args;
  for i:=0 to DeclArgs.Count-1 do
    begin
    DeclArg:=TPasArgument(DeclArgs[i]);
    if i<ImplArgs.Count then
      begin
      ImplArg:=TPasArgument(ImplArgs[i]);
      Identifier:=ImplProcScope.FindLocalIdentifier(DeclArg.Name);
      //writeln('TPasResolver.ReplaceProcScopeImplArgsWithDeclArgs i=',i,' replacing ',GetObjName(ImplArg),' with ',GetObjName(DeclArg));
      if Identifier.Element<>ImplArg then
        RaiseInternalError(20170203161659,GetObjName(DeclArg)+' '+GetObjName(ImplArg));
      Identifier.Element:=DeclArg;
      Identifier.Identifier:=DeclArg.Name;
      end
    else
      begin
      // e.g. when Delphi mode omits ImplProc signature
      AddIdentifier(ImplProcScope,DeclArg.Name,DeclArg,pikSimple);
      end;
    end;
  if DeclProc.ProcType is TPasFunctionType then
    begin
    // redirect implementation 'Result' to declaration FuncType.ResultEl
    FuncType:=TPasFunctionType(DeclProc.ProcType);
    if FuncType.ResultEl<>nil then
      begin
      Identifier:=ImplProcScope.FindLocalIdentifier(ResolverResultVar);
      if Identifier=nil then
        begin
        // e.g. when Delphi mode omits ImplProc signature
        AddIdentifier(ImplProcScope,ResolverResultVar,FuncType.ResultEl,pikSimple);
        end
      else if Identifier.Element is TPasResultElement then
        Identifier.Element:=FuncType.ResultEl;
      // redirect named result variable (e.g. "c" in operators) to declaration ResultEl
      if (FuncType.ResultEl.Name <> '') and
         not SameText(FuncType.ResultEl.Name, ResolverResultVar) then
        begin
        Identifier:=ImplProcScope.FindLocalIdentifier(FuncType.ResultEl.Name);
        if Identifier=nil then
          AddIdentifier(ImplProcScope,FuncType.ResultEl.Name,FuncType.ResultEl,pikSimple)
        else if Identifier.Element is TPasResultElement then
          Identifier.Element:=FuncType.ResultEl;
        end;
      end;
    end;
end;

function TPasResolver.CreateClassIntfMap(El: TPasClassType; Index: integer
  ): TPasClassIntfMap;
var
  IntfType: TPasClassType;
  Map: TPasClassIntfMap;
  ClassScope: TPasClassScope;
begin
  ClassScope:=El.CustomData as TPasClassScope;
  if ClassScope.Interfaces[Index]<>nil then
    RaiseInternalError(20180322141916,GetElementDbgPath(El)+' '+IntToStr(Index)+' '+GetObjName(TObject(ClassScope.Interfaces[Index])));
  IntfType:=TPasClassType(ResolveAliasType(TPasType(El.Interfaces[Index])));
  Map:=nil;
  while IntfType<>nil do
    begin
    if Map=nil then
      begin
      Map:=TPasClassIntfMap.Create;
      Map.Element:=El;
      Result:=Map;
      ClassScope.Interfaces[Index]:=Map;
      end
    else
      begin
      Map.AncestorMap:=TPasClassIntfMap.Create;
      Map:=Map.AncestorMap;
      Map.Element:=El;
      end;
    Map.Intf:=IntfType;
    Map.Procs:=TFPList.Create;
    Map.Procs.Count:=IntfType.Members.Count;
    IntfType:=GetPasClassAncestor(IntfType,true) as TPasClassType;
    end;
end;

procedure TPasResolver.CheckConditionExpr(El: TPasExpr;
  const ResolvedEl: TPasResolverResult);
begin
  if ResolvedEl.BaseType=btBoolean then exit;
  if IsGenericTemplType(ResolvedEl) then exit;
  RaiseXExpectedButYFound(20170216152135,
    BaseTypeNames[btBoolean],BaseTypeNames[ResolvedEl.BaseType],El);
end;

function TPasResolver.SameConstParamType(DeclTempl,
  ImplTempl: TPasGenericTemplateType): boolean;
// Whether two const generic parameters are annotated with the same type. The
// annotation is the parameter's single constraint; a missing one on either side
// means there is nothing to disagree about.
var
  DeclType, ImplType: TPasType;
begin
  Result:=true;
  if (length(DeclTempl.Constraints)=0) or (length(ImplTempl.Constraints)=0) then
    exit;
  if not (DeclTempl.Constraints[0] is TPasType) then exit;
  if not (ImplTempl.Constraints[0] is TPasType) then exit;
  DeclType:=ResolveAliasType(TPasType(DeclTempl.Constraints[0]));
  ImplType:=ResolveAliasType(TPasType(ImplTempl.Constraints[0]));
  Result:=DeclType=ImplType;
end;

procedure TPasResolver.CheckProcSignatureMatch(DeclProc,
  ImplProc: TPasProcedure; IsOverride: boolean);

  function CovariantResultAllowed(DeclType, ImplType: TPasType): boolean;
  // An OVERRIDE may narrow a CLASS result to a descendant - real FPC accepts
  // that covariant return, and rtl-generics' TDictionaryEnumerable overrides
  // DoGetEnumerator with its own enumerator class. A result that is still a
  // generic template type can only be judged at specialization, so it passes
  // here and is checked again there.
  var
    D, I: TPasType;
  begin
    Result:=false;
    if (DeclType=nil) or (ImplType=nil) then exit;
    D:=ResolveAliasType(DeclType);
    I:=ResolveAliasType(ImplType);
    if (D is TPasGenericTemplateType) or (I is TPasGenericTemplateType) then
      exit(true);
    // an interface result may narrow to a descendant interface as well
    if (D is TPasClassType) and (I is TPasClassType)
        and (TPasClassType(D).ObjKind in [okClass,okInterface])
        and (TPasClassType(I).ObjKind=TPasClassType(D).ObjKind) then
      Result:=CheckClassIsClass(I,D)<>cIncompatible;
  end;

var
  i: Integer;
  DeclArgs, ImplArgs, ImplTemplates, DeclTemplates: TFPList;
  DeclName, ImplName: String;
  ImplResult, DeclResult: TPasType;
  ImplTemplType, DeclTemplType: TPasGenericTemplateType;
  NewImplPTMods, DeclPTMods, ImplPTMods: TProcTypeModifiers;
  ptm: TProcTypeModifier;
  NewImplProcMods: TProcedureModifiers;
  pm: TProcedureModifier;
  ImplOmitsArgs: boolean;

begin
  if ImplProc.ClassType<>DeclProc.ClassType then
    RaiseXExpectedButYFound(20170216151729,DeclProc.TypeName,ImplProc.TypeName,ImplProc);

  DeclArgs:=DeclProc.ProcType.Args;
  ImplArgs:=ImplProc.ProcType.Args;
  // FPC allows an implementation to OMIT the parameter list entirely; it then
  // inherits the declaration's arguments (ReplaceProcScopeImplArgsWithDeclArgs adds
  // them to the impl scope). Such an impl has 0 args — skip the per-argument checks.
  ImplOmitsArgs:=(ImplArgs.Count=0) and (DeclArgs.Count>0) and not IsOverride;
  if (DeclArgs.Count<>ImplArgs.Count) and not ImplOmitsArgs then
    RaiseNotYetImplemented(20190912110642,ImplProc);

  DeclPTMods:=DeclProc.ProcType.Modifiers;
  ImplPTMods:=ImplProc.ProcType.Modifiers;

  DeclTemplates:=GetProcTemplateTypes(DeclProc);
  ImplTemplates:=GetProcTemplateTypes(ImplProc);
  if DeclTemplates<>nil then
    begin
    // DeclProc has templates
    if IsOverride then
      RaiseNotYetImplemented(20190912113857,ImplProc); // inconsistency
    if ImplTemplates=nil then
      RaiseMsg(20190912144529,nDeclOfXDiffersFromPrevAtY,sDeclOfXDiffersFromPrevAtY,
        [GetProcName(ImplProc),GetElementSourcePosStr(DeclProc)],ImplProc);

    // declaration proc has template type aka is parameterized
    // -> check template types
    if ImplTemplates.Count<>DeclTemplates.Count then
      RaiseMsg(20190912145320,nDeclOfXDiffersFromPrevAtY,sDeclOfXDiffersFromPrevAtY,
        [GetProcName(ImplProc),GetElementSourcePosStr(TPasElement(DeclTemplates[0]))],ImplProc);
    for i:=0 to DeclTemplates.Count-1 do
      begin
      DeclTemplType:=TPasGenericTemplateType(DeclTemplates[i]);
      ImplTemplType:=TPasGenericTemplateType(ImplTemplates[i]);
      if not SameText(DeclTemplType.Name,ImplTemplType.Name) then
        RaiseMsg(20190912150311,nDeclOfXDiffersFromPrevAtY,sDeclOfXDiffersFromPrevAtY,
          [GetProcName(ImplProc),GetElementSourcePosStr(TPasElement(DeclTemplType))],ImplTemplType);
      // Where the declaration was decides what the implementation must say,
      // and the two rules are opposites. Verified against ppcx64:
      //
      //   declared in the INTERFACE - the implementation must NOT repeat the
      //     annotations; `<A; const N: LongInt>` there is implemented as
      //     plain `<A; N>` (tgenconst19/20), and repeating leaves the
      //     declaration unsolved (tgenconst27/28);
      //   declared FORWARD in the implementation section - the implementation
      //     MUST repeat them, and they must MATCH (tgenfunc20/21).
      //
      // So the two checks below belong to the forward case only.
      if DeclProc.IsForward then
        begin
        // A CONST generic parameter and a plain type parameter are different
        // kinds of parameter, not two spellings of one: forward
        // `<A; const N: LongInt>` implemented as `<A; N>` is a different
        // routine (tgenconst25/26).
        if DeclTemplType.IsConst<>ImplTemplType.IsConst then
          RaiseMsg(20260808150001,nDeclOfXDiffersFromPrevAtY,sDeclOfXDiffersFromPrevAtY,
            [GetProcName(ImplProc),GetElementSourcePosStr(TPasElement(DeclTemplType))],ImplTemplType);
        // Repeating is allowed, but only with the SAME type: forward
        // `const N: LongInt` implemented as `const N: String` is a different
        // routine (tgenconst23/24).
        if DeclTemplType.IsConst and (length(ImplTemplType.Constraints)>0)
            and not SameConstParamType(DeclTemplType,ImplTemplType) then
          RaiseMsg(20260808150002,nDeclOfXDiffersFromPrevAtY,sDeclOfXDiffersFromPrevAtY,
            [GetProcName(ImplProc),GetElementSourcePosStr(TPasElement(DeclTemplType))],ImplTemplType);
        end;
      // Only a FORWARD declaration's implementation may repeat the annotation
      // at all. A routine declared in the INTERFACE section must not repeat it,
      // const or otherwise (tgenconst27/28 - ppcx64 answers "Forward
      // declaration not solved") - the same rule that keeps a class method's
      // implementation from repeating its constraints (tgenfunc13).
      if (length(ImplTemplType.Constraints)>0)
          and not (DeclProc.IsForward and AllowImplRepeatConstraints) then
        RaiseMsg(20190912150739,nImplMustNotRepeatConstraints,sImplMustNotRepeatConstraints,[],ImplTemplType);
      end;
    end
  else if ImplTemplates<>nil then
    begin
    // ImplProc has templates, DeclProc does not
    RaiseMsg(20190912113857,nDeclOfXDiffersFromPrevAtY,sDeclOfXDiffersFromPrevAtY,
        [GetProcName(ImplProc),GetElementSourcePosStr(DeclProc)],ImplProc);
    end;

  if (not IsOverride) and (not ImplOmitsArgs) then
    begin
    // check argument names
    for i:=0 to DeclArgs.Count-1 do
      begin
      DeclName:=TPasArgument(DeclArgs[i]).Name;
      ImplName:=TPasArgument(ImplArgs[i]).Name;
      if CompareText(DeclName,ImplName)<>0 then
        RaiseMsg(20170216151738,nFunctionHeaderMismatchForwardVarName,
          sFunctionHeaderMismatchForwardVarName,[DeclProc.Name,DeclName,ImplName],ImplProc);
      end;
    end;

  if ImplProc.ProcType is TPasFunctionType then
    begin
    // check result type
    ImplResult:=TPasFunctionType(ImplProc.ProcType).ResultEl.ResultType;
    DeclResult:=TPasFunctionType(DeclProc.ProcType).ResultEl.ResultType;

    // cAliasExact is "the same actual type under another name" - $H+ String vs
    // AnsiString, or two AnsiString-family types differing only in codepage.
    // Restating the result that way is allowed; rtl-objpas' rtti.pp and
    // fcl-base's streamex both declare `function ToString: String; override;`
    // over a TObject.ToString that returns AnsiString.
    if (CheckElTypeCompatibility(ImplResult,DeclResult,prraSimple)>cAliasExact)
        and not (IsOverride and CovariantResultAllowed(DeclResult,ImplResult))
        and not ResultsMatchAsAnsiStrings(ImplResult,DeclResult)
        and not (IsOverride and IntegerTypeFitsSameStorage(ImplResult,DeclResult)) then
      begin
      RaiseIncompatibleType(20170216151734,nResultTypeMismatchExpectedButFound,
        [],DeclResult,ImplResult,ImplProc);
      end;
    end;

  // calling convention
  if ImplProc.CallingConvention<>DeclProc.CallingConvention then
    begin
    // An implementation/override that does not restate the calling convention
    // inherits it from the declaration (FPC: directives need not be repeated in
    // the implementation). Only a *conflicting* explicit convention is an error.
    if ImplProc.CallingConvention=ccDefault then
      ImplProc.CallingConvention:=DeclProc.CallingConvention
    else
      RaiseMsg(20170216151731,nCallingConventionMismatch,sCallingConventionMismatch,[],ImplProc);
    end;

  // modifiers
  if IsOverride then
    begin
    // override/class-intf-impl: calling conventions must match
    NewImplPTMods:=ImplPTMods><DeclPTMods;
    for ptm in NewImplPTMods do
      RaiseMsg(20201227213020,nXModifierMismatchY,sXModifierMismatchY,
        ['procedure type',ProcTypeModifiers[ptm]],ImplProc.ProcType);
    end
  else
    begin
    // implementation proc must not add modifiers, except "assembler", "public",
    // "external" (an external impl completes an interface/forward declaration), and
    // "inline" — FPC tolerates an implementation repeating/adding `inline` even when
    // the interface declaration omits it (it is only a hint). The unix sysutils
    // TOrdinalHelper declares ToBinString without inline but its shared macro
    // implementation (syshelpo.inc) has `inline`.
    NewImplProcMods:=ImplProc.Modifiers-DeclProc.Modifiers-[pmAssembler,pmPublic,pmExternal,pmInline];
    if NewImplProcMods<>[] then
      for pm in NewImplProcMods do
        RaiseMsg(20200518182445,nDirectiveXNotAllowedHere,sDirectiveXNotAllowedHere,
          [ModifierNames[pm]],ImplProc.ProcType);
    // implementation proc must not add modifiers
    NewImplPTMods:=ImplPTMods-DeclPTMods;
    if NewImplPTMods<>[] then
      for ptm in NewImplPTMods do
        RaiseMsg(20200425154821,nDirectiveXNotAllowedHere,sDirectiveXNotAllowedHere,
          [ProcTypeModifiers[ptm]],ImplProc.ProcType);
    end;
end;

procedure TPasResolver.ResolveImplBlock(Block: TPasImplBlock);
var
  i: Integer;
begin
  if Block=nil then exit;
  for i:=0 to Block.Elements.Count-1 do
    ResolveImplElement(TPasImplElement(Block.Elements[i]));
end;

procedure TPasResolver.ResolveImplElement(El: TPasImplElement);
var
  C: TClass;
begin
  //writeln('TPasResolver.ResolveImplElement ',GetObjName(El));
  if El=nil then exit;
  C:=El.ClassType;
  if C=TPasImplBeginBlock then
    ResolveImplBlock(TPasImplBeginBlock(El))
  else if C=TPasImplAssign then
    ResolveImplAssign(TPasImplAssign(El))
  else if C=TPasImplSimple then
    ResolveImplSimple(TPasImplSimple(El))
  else if C=TPasImplBlock then
    ResolveImplBlock(TPasImplBlock(El))
  else if C=TPasImplRepeatUntil then
    begin
    ResolveImplBlock(TPasImplBlock(El));
    ResolveStatementConditionExpr(TPasImplRepeatUntil(El).ConditionExpr);
    end
  else if C=TPasImplIfElse then
    begin
    ResolveStatementConditionExpr(TPasImplIfElse(El).ConditionExpr);
    ResolveImplElement(TPasImplIfElse(El).IfBranch);
    ResolveImplElement(TPasImplIfElse(El).ElseBranch);
    end
  else if C=TPasImplWhileDo then
    begin
    ResolveStatementConditionExpr(TPasImplWhileDo(El).ConditionExpr);
    ResolveImplElement(TPasImplWhileDo(El).Body);
    end
  else if C=TPasImplCaseOf then
    ResolveImplCaseOf(TPasImplCaseOf(El))
  else if C=TPasImplLabelMark then
    ResolveImplLabelMark(TPasImplLabelMark(El))
  else if C=TPasImplForLoop then
    // the header was already resolved
    ResolveImplElement(TPasImplForLoop(El).Body)
  else if C=TPasImplTry then
    begin
    ResolveImplBlock(TPasImplTry(El));
    ResolveImplBlock(TPasImplTry(El).FinallyExcept);
    ResolveImplBlock(TPasImplTry(El).ElseBranch);
    end
  else if C=TPasImplExceptOn then
    // handled in FinishExceptOnStatement
  else if C=TPasImplRaise then
    ResolveImplRaise(TPasImplRaise(El))
  else if C=TPasImplCommand then
    begin
    if TPasImplCommand(El).Command<>'' then
      RaiseNotYetImplemented(20160922163442,El,'TPasResolver.ResolveImplElement');
    end
  else if C=TPasImplAsmStatement then
    ResolveImplAsm(TPasImplAsmStatement(El))
  else if C=TPasImplWithDo then
    ResolveImplWithDo(TPasImplWithDo(El))
  else
    RaiseNotYetImplemented(20160922163445,El,'TPasResolver.ResolveImplElement');
end;

procedure TPasResolver.ResolveImplCaseOf(CaseOf: TPasImplCaseOf);
var
  LabelLists: TFPList;
  i: Integer;
  El: TPasElement;
begin
  LabelLists:=TFPList.Create;
  try
    for i:=0 to CaseOf.Elements.Count-1 do
      begin
      El:=TPasElement(CaseOf.Elements[i]);
      if El.ClassType=TPasImplCaseStatement then
        LabelLists.Add(TPasImplCaseStatement(El).Expressions)
      else if El.ClassType<>TPasImplCaseElse then
        RaiseNotYetImplemented(20160922163448,El);
      end;
    ResolveCaseLabels(CaseOf.CaseExpr,LabelLists,CaseOf.ElseBranch,false);
  finally
    LabelLists.Free;
  end;
  for i:=0 to CaseOf.Elements.Count-1 do
    begin
    El:=TPasElement(CaseOf.Elements[i]);
    if El.ClassType=TPasImplCaseStatement then
      ResolveImplElement(TPasImplCaseStatement(El).Body)
    else
      ResolveImplBlock(TPasImplCaseElse(El));
    end;
end;

procedure TPasResolver.ResolveCaseLabels(CaseExpr: TPasExpr;
  LabelLists: TFPList; ElseEl: TPasElement; IsExpression: boolean);
// LabelLists: list of TFPList of TPasExpr
// ElseEl: nil if there is no else-branch
type
  TRangeItem = record
    RangeStart, RangeEnd: TMaxPrecInt;
    Expr: TPasExpr;
    aString: UnicodeString;
    aStringHigh: UnicodeString; // for multi-char string ranges
    // Note: for case-of-string:
    //  single values are stored in aString and RangeStart=1, RangeEnd=0
    //  char ranges are stored as aString='', RangeStart, RangeEnd
    //  multi-char string ranges: aString=low, aStringHigh=high, RangeStart=2, RangeEnd=0
  end;
  PRangeItem = ^TRangeItem;

  function CreateValues(const ResolvedEl: TPasResolverResult;
    var ValueSet: TResEvalSet): boolean;
  var
    CaseExprType: TPasType;
    bt: TResolverBaseType;
    ElTypeResolved: TPasResolverResult;
  begin
    Result:=false;
    bt:=ResolvedEl.BaseType;
    if bt in btAllStrings then
      exit(true)
    else if bt=btRange then
      bt:=ResolvedEl.SubType;
    if bt in btAllInteger then
      begin
      ValueSet:=TResEvalSet.CreateEmpty(revskInt);
      Result:=true;
      end
    else if bt in btAllBooleans then
      begin
      ValueSet:=TResEvalSet.CreateEmpty(revskBool);
      Result:=true;
      end
    else if bt in btAllChars then
      begin
      ValueSet:=TResEvalSet.CreateEmpty(revskChar);
      Result:=true;
      end
    else if bt=btContext then
      begin
      CaseExprType:=ResolvedEl.LoTypeEl;
      if CaseExprType.ClassType=TPasEnumType then
        begin
        ValueSet:=TResEvalSet.CreateEmpty(revskEnum,CaseExprType);
        Result:=true;
        end
      else if CaseExprType.ClassType=TPasRangeType then
        begin
        ComputeElement(TPasRangeType(CaseExprType).RangeExpr.Left,ElTypeResolved,[rcConstant]);
        Result:=CreateValues(ElTypeResolved,ValueSet);
        end;
      end;
  end;

  function AddRangeItem(Values: TFPList; const RangeStart, RangeEnd: TMaxPrecInt;
    Expr: TPasExpr): PRangeItem;
  begin
    New(Result);
    Result^.RangeStart:=RangeStart;
    Result^.RangeEnd:=RangeEnd;
    Result^.Expr:=Expr;
    Values.Add(Result);
  end;

  function AddValue(Value: TResEvalValue; Values: TFPList; ValueSet: TResEvalSet;
    Expr: TPasExpr): boolean;

    function AddString(const s: UnicodeString): boolean;
    var
      Dupl: TPasExpr;
      i, o: Integer;
      Item: PRangeItem;
    begin
      if length(s)=1 then
        o:=ord(s[1])
      else
        o:=-1;
      for i:=0 to Values.Count-1 do
        begin
        Item:=PRangeItem(Values[i]);
        if (Item^.aString=s)
            or ((o>=Item^.RangeStart) and (o<=Item^.RangeEnd)) then
          begin
          Dupl:=PRangeItem(Values[i])^.Expr;
          RaiseMsg(20180424220139,nDuplicateCaseValueXatY,sDuplicateCaseValueXatY,
            ['string',GetElementSourcePosStr(Dupl)],Expr);
          end;
        end;
      Item:=AddRangeItem(Values,1,0,Expr);
      Item^.aString:=s;
      Result:=true;
    end;

    function AddStringRange(CharStart, CharEnd: TMaxPrecInt): boolean;
    var
      i, o: Integer;
      s: UnicodeString;
      Item: PRangeItem;
      Dupl: TPasExpr;
    begin
      if CharEnd>$ffff then
        RaiseNotYetImplemented(20180501221359,Expr,Value.AsDebugString);
      for i:=0 to Values.Count-1 do
        begin
        Item:=PRangeItem(Values[i]);
        s:=Item^.aString;
        if length(s)=1 then
          o:=ord(s[1])
        else
          o:=-1;
        if ((o>=CharStart) and (o<=CharEnd))
            or ((Item^.RangeStart<=CharEnd) and (Item^.RangeEnd>=CharStart)) then
          begin
          Dupl:=PRangeItem(Values[i])^.Expr;
          RaiseMsg(20180501223914,nDuplicateCaseValueXatY,sDuplicateCaseValueXatY,
            ['string',GetElementSourcePosStr(Dupl)],Expr);
          end;
        end;
      AddRangeItem(Values,CharStart,CharEnd,Expr);
      Result:=true;
    end;

  var
    RangeStart, RangeEnd: TMaxPrecInt;
    i: Integer;
    Item: PRangeItem;
  begin
    {$IFDEF VerbosePasResolver}
    //writeln('TPasResolver.ResolveImplCaseOf.AddValue Value={',Value.AsDebugString,'} Values.Count=',Values.Count);
    {$ENDIF}
    Result:=true;
    case Value.Kind of
    revkBool:
      begin
      RangeStart:=ord(TResEvalBool(Value).B);
      RangeEnd:=RangeStart;
      end;
    revkInt:
      begin
      RangeStart:=TResEvalInt(Value).Int;
      RangeEnd:=RangeStart;
      end;
    revkUInt:
      begin
      // When FPC compares an int64 with a qword it converts the qword to an
      // int64, so a label above High(Int64) is NOT an error - it is that same
      // bit pattern read as signed. ppcx64 accepts `case sizeVar of size_t(-2)`
      // (rtl/unix/cwstring.pp tests mbrtowc's size_t(-1)/size_t(-2) results);
      // erroring here rejected the whole unit. Duplicate detection is unaffected:
      // equal patterns still compare equal.
      {$R-}
      RangeStart:=TMaxPrecInt(TResEvalUInt(Value).UInt);
      {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
      RangeEnd:=RangeStart;
      end;
    {$ifdef FPC_HAS_CPSTRING}
    revkString:
      if ValueSet=nil then
        exit(AddString(ExprEvaluator.GetUnicodeStr(TResEvalString(Value).S,Expr)))
      else
        begin
        RangeStart:=fExprEvaluator.StringToOrd(Value,nil);
        if RangeStart>$ffff then
          exit(false);
        RangeEnd:=RangeStart;
        end;
    {$endif}
    revkUnicodeString:
      if ValueSet=nil then
        exit(AddString(TResEvalUTF16(Value).S))
      else
        begin
        if length(TResEvalUTF16(Value).S)<>1 then
          exit(false);
        RangeStart:=ord(TResEvalUTF16(Value).S[1]);
        RangeEnd:=RangeStart;
        end;
    revkEnum:
      begin
      RangeStart:=TResEvalEnum(Value).Index;
      RangeEnd:=RangeStart;
      end;
    revkRangeInt:
      if ValueSet=nil then
        exit(AddStringRange(TResEvalRangeInt(Value).RangeStart,TResEvalRangeInt(Value).RangeEnd))
      else
        begin
        RangeStart:=TResEvalRangeInt(Value).RangeStart;
        RangeEnd:=TResEvalRangeInt(Value).RangeEnd;
        end;
    revkRangeUInt:
      begin
      // Same as revkUInt above: read the pattern as signed rather than refusing.
      {$R-}
      RangeStart:=TMaxPrecInt(TResEvalRangeUInt(Value).RangeStart);
      RangeEnd:=TMaxPrecInt(TResEvalRangeUInt(Value).RangeEnd);
      {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
      end;
    else
      Result:=false;
    end;

    if ValueSet=nil then
      RaiseNotYetImplemented(20180424215728,Expr,Value.AsDebugString);
    i:=ValueSet.Intersects(RangeStart,RangeEnd);
    if i<0 then
      begin
      ValueSet.Add(RangeStart,RangeEnd);
      AddRangeItem(Values,RangeStart,RangeEnd,Expr);
      exit(true);
      end;
    // duplicate value -> show where
    for i:=0 to Values.Count-1 do
      begin
      Item:=PRangeItem(Values[i]);
      if (Item^.RangeStart>RangeEnd) or (Item^.RangeEnd<RangeStart) then continue;
      RaiseMsg(20180424214305,nDuplicateCaseValueXatY,sDuplicateCaseValueXatY,
        [Value.AsString,GetElementSourcePosStr(Item^.Expr)],Expr);
      end;
    Result:=false;
  end;

  function GetEvalString(V: TResEvalValue): UnicodeString;
  begin
    case V.Kind of
    {$ifdef FPC_HAS_CPSTRING}
    revkString:
      Result:=fExprEvaluator.GetUnicodeStr(TResEvalString(V).S,nil);
    {$endif}
    revkUnicodeString:
      Result:=TResEvalUTF16(V).S;
    else
      Result:='';
    end;
  end;

  procedure CheckStringRangeOverlap(OfExpr: TPasExpr; Values: TFPList);
  // Check multi-char string range against existing string values/ranges for overlap
  var
    k: Integer;
    Item: PRangeItem;
    NewLow, NewHigh: UnicodeString;
    LeftV, RightV: TResEvalValue;
    BinExpr: TBinaryExpr;
  begin
    if not (OfExpr is TBinaryExpr) then exit;
    BinExpr:=TBinaryExpr(OfExpr);
    if BinExpr.Kind<>pekRange then exit;

    // Evaluate the range bounds
    LeftV:=Eval(BinExpr.Left,[refConstExt]);
    RightV:=Eval(BinExpr.Right,[refConstExt]);
    if (LeftV=nil) or (RightV=nil) then
      begin
      ReleaseEvalValue(LeftV);
      ReleaseEvalValue(RightV);
      exit;
      end;
    NewLow:=GetEvalString(LeftV);
    NewHigh:=GetEvalString(RightV);
    ReleaseEvalValue(LeftV);
    ReleaseEvalValue(RightV);

    // Check overlap against existing items
    for k:=0 to Values.Count-1 do
      begin
      Item:=PRangeItem(Values[k]);
      if Item^.aStringHigh<>'' then
        begin
        // Existing multi-char string range: check overlap
        // Overlap if NewLow <= ExistingHigh AND NewHigh >= ExistingLow
        if (NewLow<=Item^.aStringHigh) and (NewHigh>=Item^.aString) then
          RaiseMsg(20180424220139,nDuplicateCaseValueXatY,sDuplicateCaseValueXatY,
            ['string',GetElementSourcePosStr(Item^.Expr)],OfExpr);
        end
      else if Item^.aString<>'' then
        begin
        // Existing single string value: check if it falls in new range
        if (Item^.aString>=NewLow) and (Item^.aString<=NewHigh) then
          RaiseMsg(20180424220139,nDuplicateCaseValueXatY,sDuplicateCaseValueXatY,
            ['string',GetElementSourcePosStr(Item^.Expr)],OfExpr);
        end;
      end;

    // Add this range to the values list
    Item:=AddRangeItem(Values,2,0,OfExpr);
    Item^.aString:=NewLow;
    Item^.aStringHigh:=NewHigh;
  end;

  procedure CheckStringValueOverlap(OfExpr: TPasExpr; Values: TFPList);
  // Check single string value against existing multi-char string ranges
  var
    k: Integer;
    Item: PRangeItem;
    SVal: UnicodeString;
    V: TResEvalValue;
  begin
    V:=Eval(OfExpr,[refConstExt]);
    if V=nil then exit;
    SVal:=GetEvalString(V);
    ReleaseEvalValue(V);
    if SVal='' then exit;

    // Check against existing multi-char string ranges only
    // (single values and char ranges are checked by AddValue already)
    for k:=0 to Values.Count-1 do
      begin
      Item:=PRangeItem(Values[k]);
      if Item^.aStringHigh<>'' then
        begin
        // Multi-char string range: check if value falls in range
        if (SVal>=Item^.aString) and (SVal<=Item^.aStringHigh) then
          RaiseMsg(20180424220139,nDuplicateCaseValueXatY,sDuplicateCaseValueXatY,
            ['string',GetElementSourcePosStr(Item^.Expr)],OfExpr);
        end;
      end;
  end;

var
  i, j: Integer;
  Labels: TFPList;
  CaseExprResolved, OfExprResolved: TPasResolverResult;
  OfExpr: TPasExpr;
  ok: Boolean;
  Values: TFPList; // list of PRangeItem
  ValueSet: TResEvalSet;
  Value: TResEvalValue;
  Item: PRangeItem;
  HasElse, IsISO, RangeCovered, AllCovered: Boolean;
  RLow, RHigh: TMaxPrecInt;
  CaseValue, RangeVal: TResEvalValue;
begin
  ResolveExpr(CaseExpr,rraRead);
  ComputeElement(CaseExpr,CaseExprResolved,[rcSetReferenceFlags]);
  ok:=false;
  Values:=TFPList.Create;
  ValueSet:=nil;
  Value:=nil;
  try
    if (rrfReadable in CaseExprResolved.Flags) then
      ok:=CreateValues(CaseExprResolved,ValueSet);
    if not ok then
      begin
      if not IsGenericTemplType(CaseExprResolved) then
        RaiseXExpectedButYFound(20170216151952,'ordinal expression',
                   GetTypeDescription(CaseExprResolved.LoTypeEl),CaseExpr);
      end;

    for i:=0 to LabelLists.Count-1 do
      begin
      Labels:=TFPList(LabelLists[i]);
      for j:=0 to Labels.Count-1 do
        begin
        OfExpr:=TPasExpr(Labels[j]);
        ResolveExpr(OfExpr,rraRead);
        ComputeElement(OfExpr,OfExprResolved,[rcConstant,rcSetReferenceFlags]);
        if OfExprResolved.BaseType=btRange then
          ConvertRangeToElement(OfExprResolved);
        if not ok then
          continue;
        CheckEqualResCompatibility(CaseExprResolved,OfExprResolved,OfExpr,true);

        Value:=Eval(OfExpr,[refConstExt]);
        if Value<>nil then
          begin
          if Value.Kind=revkExternal then
            begin
            // external const
            end
          else if not AddValue(Value,Values,ValueSet,OfExpr) then
            RaiseIncompatibleTypeRes(20180424210815,nIncompatibleTypesGotExpected,
              [],OfExprResolved,CaseExprResolved,OfExpr);
          // Also check single string values against multi-char string ranges
          if CaseExprResolved.BaseType in btAllStrings then
            CheckStringValueOverlap(OfExpr,Values);
          ReleaseEvalValue(Value);
          end
        else if CaseExprResolved.BaseType in btAllStrings then
          // Multi-char string range: check for overlaps
          CheckStringRangeOverlap(OfExpr,Values)
        else
          RaiseMsg(20180518102047,nConstantExpressionExpected,sConstantExpressionExpected,[],OfExpr);
        end;
      end;
    // Case coverage analysis
    HasElse := ElseEl <> nil;
    if IsExpression and not HasElse
        and (CaseExprResolved.BaseType in btAllStrings) then
      // case-of-string expression needs an else
      RaiseMsg(20260913120100,nCaseExprNotCovered,sCaseExprNotCovered,[],CaseExpr);
    if (ValueSet <> nil) and not (CaseExprResolved.BaseType in btAllStrings) then
      begin
      IsISO := [msIso, msExtpas] * GetElModeSwitches(CaseExpr) <> [];

      // Check 1: constant case expression - is the value covered?
      // A case-expression without else must cover the whole type range.
      if IsExpression then
        CaseValue := nil
      else
        CaseValue := Eval(CaseExpr, []);
      if CaseValue <> nil then
        begin
        try
          if CaseValue.Kind = revkInt then
            begin
            if (not HasElse) and (ValueSet.Intersects(TResEvalInt(CaseValue).Int,
                                    TResEvalInt(CaseValue).Int) < 0) then
              begin
              // Constant value not in any case label and no else/otherwise branch and no else/otherwise branch
              if IsISO then
                RaiseMsg(20260225200001,nCaseStatementNotCovered,
                  sCaseStatementNotCovered,[],CaseExpr)
              else
                LogMsg(20260225200002,mtWarning,nCaseStatementNotCovered,
                  sCaseStatementNotCovered,[],CaseExpr);
              end;
            end;
        finally
          ReleaseEvalValue(CaseValue);
        end;
        end
      else
        begin
        // Check 2: variable case expression - check type range coverage
        RangeCovered := false;
        RLow := 0;
        RHigh := 0;
        if CaseExprResolved.BaseType in btAllBooleans then
          begin
          RLow := 0;
          RHigh := 1;
          RangeCovered := true;
          end
        else if CaseExprResolved.LoTypeEl is TPasRangeType then
          begin
          RangeVal := Eval(TPasRangeType(CaseExprResolved.LoTypeEl).RangeExpr, []);
          if (RangeVal <> nil) and (RangeVal.Kind = revkRangeInt) then
            begin
            RLow := TResEvalRangeInt(RangeVal).RangeStart;
            RHigh := TResEvalRangeInt(RangeVal).RangeEnd;
            RangeCovered := true;
            end;
          ReleaseEvalValue(RangeVal);
          end
        else if CaseExprResolved.LoTypeEl is TPasEnumType then
          begin
          RLow := GetEnumMinOrdinal(TPasEnumType(CaseExprResolved.LoTypeEl));
          RHigh := GetEnumMaxOrdinal(TPasEnumType(CaseExprResolved.LoTypeEl));
          RangeCovered := true;
          end;

        if RangeCovered then
          begin
          // Check if ValueSet covers [RLow..RHigh] completely
          AllCovered := (Length(ValueSet.Ranges) = 1)
            and (ValueSet.Ranges[0].RangeStart <= RLow)
            and (ValueSet.Ranges[0].RangeEnd >= RHigh);

          if AllCovered and HasElse then
            // All values covered but else exists - unreachable else warning
            LogMsg(20260225200003,mtWarning,nCaseElseUnreachable,
              sCaseElseUnreachable,[],ElseEl)
          else if not AllCovered and not HasElse then
            begin
            // Not all values covered and no else
            if IsExpression then
              RaiseMsg(20260913120101,nCaseExprNotCovered,
                sCaseExprNotCovered,[],CaseExpr)
            else if IsISO then
              RaiseMsg(20260225200004,nCaseStatementNotCovered,
                sCaseStatementNotCovered,[],CaseExpr)
            else
              LogMsg(20260225200005,mtWarning,nCaseStatementNotCovered,
                sCaseStatementNotCovered,[],CaseExpr);
            end;
          end
        else if not HasElse and IsExpression then
          // case-expression without else on unbounded type, e.g. integer
          RaiseMsg(20260913120102,nCaseExprNotCovered,
            sCaseExprNotCovered,[],CaseExpr)
        else if not HasElse and IsISO then
          begin
          // ISO/ExtPascal: case without else on unbounded type - warning
          LogMsg(20260225200006,mtWarning,nCaseStatementNotCovered,
            sCaseStatementNotCovered,[],CaseExpr);
          end;
        end;
      end;

  finally
    ReleaseEvalValue(Value);
    ValueSet.Free;
    for i:=0 to Values.Count-1 do
      begin
      Item:=PRangeItem(Values[i]);
      Dispose(Item);
      end;
    Values.Free;
  end;
end;

procedure TPasResolver.ResolveImplLabelMark(Mark: TPasImplLabelMark);
begin
  RaiseNotYetImplemented(20161014141636,Mark);
end;

procedure TPasResolver.ResolveImplWithDo(El: TPasImplWithDo);
// Note: the expressions were already resolved during parsing
//  and the scopes were already stored in a TPasWithScope.
//  -> simply push them onto the scope stack
var
  i: Integer;
  WithScope: TPasWithScope;
  ExprScope: TPasWithExprScope;
begin
  if not (El.CustomData is TPasWithScope) then
    RaiseInternalError(20181210175349);
  WithScope:=TPasWithScope(El.CustomData);
  PushScope(WithScope);
  for i:=0 to WithScope.ExpressionScopes.Count-1 do
    begin
    ExprScope:=TPasWithExprScope(WithScope.ExpressionScopes[i]);
    PushScope(ExprScope);
    end;
  // A "with" over a generic-template-typed value cannot have its body resolved
  // in the template: the member set is unknown until specialization. Skip it
  // here; the specialized copy resolves the body normally.
  if (WithScope.ExpressionScopes.Count=0)
      or not (wesfDeferredTemplate in
              TPasWithExprScope(WithScope.ExpressionScopes[WithScope.ExpressionScopes.Count-1]).Flags) then
    ResolveImplElement(El.Body);
  PopWithScope(El);
end;

procedure TPasResolver.ResolveImplAsm(El: TPasImplAsmStatement);
begin
  if El=nil then ;
end;

function TPasResolver.RefineProcAddrForTarget(
  const TargetResolved: TPasResolverResult; AddrExpr: TPasExpr;
  DoChange: boolean): boolean;
// @overloadedProc (or @Obj.overloadedMethod) meeting a typed procvar: the plain
// name lookup picks the overload by scope order, ignoring the target type. If
// that pick does not fit the target proc type, re-select the overload whose
// signature does. Purely additive - only runs when the current pick is already
// incompatible, so it can never change the meaning of code that already
// resolves. True when another overload fits; it is only made the reference
// when DoChange is set.
var
  RefEl: TPasExpr;
  Ref: TResolvedReference;
  CurProc: TPasProcedure;
  TargetType: TPasType;
  FindData: TPRFindProcAddrData;
  Abort, IsMember: boolean;
  Owner, Member: TPasElement;
  i: Integer;
  LeftRes: TPasResolverResult;
begin
  Result:=false;
  if not IsProcedureType(TargetResolved,true) then exit;
  TargetType:=ResolveAliasType(TargetResolved.LoTypeEl);
  if not (TargetType is TPasProcedureType) then exit;
  if not (AddrExpr is TUnaryExpr) then exit;
  if TUnaryExpr(AddrExpr).OpCode<>eopAddress then exit;
  RefEl:=TUnaryExpr(AddrExpr).Operand;
  IsMember:=(RefEl is TBinaryExpr) and (TBinaryExpr(RefEl).OpCode=eopSubIdent);
  if IsMember then
    RefEl:=TBinaryExpr(RefEl).Right; // @Obj.Method
  if RefEl=nil then exit;
  if not (RefEl.CustomData is TResolvedReference) then exit;
  Ref:=TResolvedReference(RefEl.CustomData);
  if not (Ref.Declaration is TPasProcedure) then exit;
  CurProc:=TPasProcedure(Ref.Declaration);
  if CurProc.ProcType=nil then exit;
  // current pick already fits the target -> nothing to refine
  if CheckProcTypeCompatibility(TPasProcedureType(TargetType),CurProc.ProcType,true,nil,false) then exit;
  if IsMember then
    begin
    // the overloads of that name in the object's class and its ancestors: the
    // pick may be an ancestor's method while the one that fits is declared
    // lower down (the LCL's `@Self.Changed` in TCustomRadioGroup, where
    // TControl.Changed has no parameter)
    ComputeElement(TBinaryExpr(TUnaryExpr(AddrExpr).Operand).Left,LeftRes,[]);
    Owner:=ResolveAliasType(LeftRes.LoTypeEl);
    if Owner is TPasClassOfType then
      Owner:=ResolveAliasType(TPasClassOfType(Owner).DestType);
    if not (Owner is TPasMembersType) then
      Owner:=CurProc.Parent;
    while Owner is TPasMembersType do
      begin
      for i:=0 to TPasMembersType(Owner).Members.Count-1 do
        begin
        Member:=TPasElement(TPasMembersType(Owner).Members[i]);
        if (Member is TPasProcedure) and (Member<>CurProc)
            and SameText(Member.Name,CurProc.Name)
            and (TPasProcedure(Member).ProcType<>nil)
            and CheckProcTypeCompatibility(TPasProcedureType(TargetType),
                  TPasProcedure(Member).ProcType,true,nil,false) then
          begin
          if DoChange then
            Ref.Declaration:=Member;
          exit(true);
          end;
        end;
      if not (Owner is TPasClassType) then break;
      Owner:=ResolveAliasType(GetPasClassAncestor(TPasClassType(Owner),true));
      end;
    exit;
    end;
  // walk every visible overload of the name (interface + implementation + used
  // units) and keep the first whose signature fits the target proc type.
  FindData:=Default(TPRFindProcAddrData);
  FindData.TargetType:=TPasProcedureType(TargetType);
  Abort:=false;
  IterateElements(CurProc.Name,@OnFindProcAddrForType,@FindData,Abort);
  if (FindData.Found=nil) or (FindData.Found=CurProc) then
    // While the argument list of `TSomeClass.Create(@Proc)` is checked, the
    // scope is that class's: look next to the routine's own declaration.
    FindData.Found:=FindProcOverloadFor(CurProc,TPasProcedureType(TargetType));
  if (FindData.Found<>nil) and (FindData.Found<>CurProc) then
    begin
    if DoChange then
      Ref.Declaration:=FindData.Found;
    Result:=true;
    end;
end;

function TPasResolver.FindProcOverloadFor(Proc: TPasProcedure;
  TargetType: TPasProcedureType): TPasProcedure;
// A same-named routine declared in the same declarations as Proc, or in the
// other section of its unit, whose type fits TargetType; nil if none.

  function SearchIn(Decls: TPasDeclarations): TPasProcedure;
  var
    i: Integer;
    El: TPasElement;
  begin
    Result:=nil;
    if Decls=nil then exit;
    for i:=0 to Decls.Declarations.Count-1 do
      begin
      El:=TPasElement(Decls.Declarations[i]);
      if (El is TPasProcedure) and (El<>Proc) and SameText(El.Name,Proc.Name)
          and (TPasProcedure(El).ProcType<>nil)
          and CheckProcTypeCompatibility(TargetType,TPasProcedure(El).ProcType,
                true,nil,false) then
        exit(TPasProcedure(El));
      end;
  end;

  // a method: the same-named members of its class and the ancestors (the LCL's
  // bare `@WriteData` with WriteData(Stream) and WriteData(Stream; Boolean))
  function SearchMembers(Owner: TPasElement): TPasProcedure;
  var
    i: Integer;
    El: TPasElement;
  begin
    Result:=nil;
    while Owner is TPasMembersType do
      begin
      for i:=0 to TPasMembersType(Owner).Members.Count-1 do
        begin
        El:=TPasElement(TPasMembersType(Owner).Members[i]);
        if (El is TPasProcedure) and (El<>Proc) and SameText(El.Name,Proc.Name)
            and (TPasProcedure(El).ProcType<>nil)
            and CheckProcTypeCompatibility(TargetType,TPasProcedure(El).ProcType,
                  true,nil,false) then
          exit(TPasProcedure(El));
        end;
      if not (Owner is TPasClassType) then break;
      Owner:=ResolveAliasType(GetPasClassAncestor(TPasClassType(Owner),true));
      end;
  end;

var
  aModule: TPasModule;
begin
  Result:=nil;
  if Proc.Parent is TPasMembersType then
    exit(SearchMembers(Proc.Parent));
  if Proc.Parent is TPasDeclarations then
    Result:=SearchIn(TPasDeclarations(Proc.Parent));
  if Result<>nil then exit;
  aModule:=Proc.GetModule;
  if aModule=nil then exit;
  Result:=SearchIn(aModule.InterfaceSection);
  if Result=nil then
    Result:=SearchIn(aModule.ImplementationSection);
  if (Result=nil) and (aModule is TPasProgram) then
    Result:=SearchIn(TPasProgram(aModule).ProgramSection);
end;

procedure TPasResolver.ResolveImplAssign(El: TPasImplAssign);
var
  LeftResolved, RightResolved: TPasResolverResult;
  Flags: TPasResolverComputeFlags;
  Access: TResolvedRefAccess;
  Value: TResEvalValue;

  procedure CheckVarPropSetter;
  // {$VARPROPSETTER}: assigning to a property whose setter takes its value by
  // 'var' requires the assigned expression to be a writable variable, not a
  // constant/literal (the setter may write back to it).
  var
    SetterEl: TPasElement;
    SetterProc: TPasProcedure;
    ValArg: TPasArgument;
  begin
    if not (LeftResolved.IdentEl is TPasProperty) then Exit;
    SetterEl := GetPasPropertySetter(TPasProperty(LeftResolved.IdentEl));
    if not (SetterEl is TPasProcedure) then Exit;
    SetterProc := TPasProcedure(SetterEl);
    if (SetterProc.ProcType = nil) or (SetterProc.ProcType.Args.Count = 0) then Exit;
    ValArg := TPasArgument(SetterProc.ProcType.Args[SetterProc.ProcType.Args.Count - 1]);
    if (ValArg.Access in [argVar, argOut])
        and not (rrfWritable in RightResolved.Flags) then
      RaiseMsg(20260612160000,nVariableIdentifierExpected,sVariableIdentifierExpected,
        [],El.Right);
  end;

  procedure RefineOverloadedProcAddr;
  begin
    if RefineProcAddrForTarget(LeftResolved,El.Right) then
      ComputeElement(El.Right,RightResolved,Flags);
  end;

begin
  if El.Kind=akDefault then
    Access:=rraAssign
  else
    Access:=rraReadAndAssign;
  ResolveExpr(El.Left,Access);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveImplAssign Kind=',El.Kind,' left=',GetObjName(El.Left),' right=',GetObjName(el.Right));
  {$ENDIF}
  // check LHS can be assigned
  ComputeElement(El.Left,LeftResolved,[rcNoImplicitProc,rcSetReferenceFlags]);
  CheckCanBeLHS(LeftResolved,true,GetRightMostExpr(El.Left));

  // compute RHS
  ResolveExpr(El.Right,rraRead);
  // A target member left unresolved for the specialization (its owner's type
  // still depends on a type parameter) is type checked there too.
  if (GetRightMostExpr(El.Left) is TPrimitiveExpr)
      and (TPrimitiveExpr(GetRightMostExpr(El.Left)).Kind=pekIdent)
      and not (GetRightMostExpr(El.Left).CustomData is TResolvedReference) then
    exit;
  Flags:=[rcSetReferenceFlags];
  if IsProcedureType(LeftResolved,true) then
    begin
    if (msDelphi in CurrentParser.CurrentModeswitches) then
      Include(Flags,rcNoImplicitProc) // a proc type can use param less procs
    else
      Include(Flags,rcNoImplicitProcType); // a proc type can use a param less proc type
    end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveImplAssign Left=',GetResolverResultDbg(LeftResolved),' Flags=',dbgs(Flags));
  {$ENDIF}
  ComputeElement(El.Right,RightResolved,Flags);
  // @overloadedProc := to a typed procvar: re-select the matching overload.
  RefineOverloadedProcAddr;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveImplAssign Right=',GetResolverResultDbg(RightResolved));
  {$ENDIF}

  case El.Kind of
  akDefault:
    begin
    // In Delphi mode, when LHS is a proc type and RHS resolved as btProc
    // (proc reference), the signatures may not match — e.g. when the RHS
    // is a parameterless function that RETURNS a compatible proc type
    // (f := t.TestCaptureSelf, tfuncref24/46). Try the proc-reference
    // interpretation first; if incompatible, fall back to auto-call by
    // re-resolving without rcNoImplicitProc (also flips the reference flags
    // from rrfNoImplicitCallWithoutParams to rrfImplicitCallWithoutParams).
    if (rcNoImplicitProc in Flags) and (RightResolved.BaseType=btProc) then
      begin
      if CheckAssignResCompatibility(LeftResolved,RightResolved,El.Right,false)<>cIncompatible then
        // proc reference interpretation succeeded — skip redundant re-check
      else
        begin
        Exclude(Flags,rcNoImplicitProc);
        Exclude(Flags,rcNoImplicitProcType);
        ComputeElement(El.Right,RightResolved,Flags);
        CheckAssignResCompatibility(LeftResolved,RightResolved,El.Right,true);
        end;
      end
    else if (LeftResolved.IdentEl is TPasProperty)
        and (CheckAssignResCompatibility(LeftResolved,RightResolved,El.Right,false)=cIncompatible)
        and (GetPropertySetterForValue(TPasProperty(LeftResolved.IdentEl),El.Right)
             <>GetPasPropertySetter(TPasProperty(LeftResolved.IdentEl))) then
      // an overloaded setter takes this value, though the property type does not
    else
      CheckAssignResCompatibility(LeftResolved,RightResolved,El.Right,true);
    CheckVarPropSetter;
    CheckAssignExprRange(LeftResolved,El.Right);
    if (LeftResolved.BaseType=btContext) and (LeftResolved.LoTypeEl.ClassType=TPasArrayType) then
      MarkArrayExprRecursive(El.Right,TPasArrayType(LeftResolved.LoTypeEl));
    end;
  akAdd, akMinus,akMul,akDivision:
    begin
    if (LeftResolved.BaseType in btAllInteger) and (El.Kind in [akAdd,akMinus,akMul]) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType in btAllInteger) then
        RaiseMsg(20170216152009,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypes[RightResolved.BaseType],BaseTypes[LeftResolved.BaseType]],El.Right);
      end
    else if (LeftResolved.BaseType in btAllStrings) and (El.Kind=akAdd) then
      begin
      // A char POINTER appends as a null-terminated string, exactly as the
      // non-compound `s := s + p` already accepts (RightIsCharPointer above).
      // Without this `s += V.VPChar` was refused - and BaseTypes[] has no name
      // for a btContext operand, so the message came out as the raw template
      // with two empty %s (pastojs' pas2jslogger, 9 corpus units).
      if (not (rrfReadable in RightResolved.Flags))
          or (not (RightResolved.BaseType in btAllStringAndChars)
              and not IsCharPointerRes(RightResolved)) then
        RaiseMsg(20170216152012,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [GetResolverResultDescription(RightResolved,true),
           GetResolverResultDescription(LeftResolved,true)],El.Right);
      end
    else if (LeftResolved.BaseType in btAllFloats)
        and (El.Kind in [akAdd,akMinus,akMul,akDivision]) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType in (btAllInteger+btAllFloats)) then
        RaiseMsg(20170216152107,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypes[RightResolved.BaseType],BaseTypes[LeftResolved.BaseType]],El.Right);
      end
    else if (LeftResolved.BaseType=btSet) and (El.Kind in [akAdd,akMinus,akMul]) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType in [btSet,btArrayOrSet]) then
        RaiseMsg(20170216152110,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypeNames[RightResolved.BaseType],'set of '+BaseTypeNames[LeftResolved.SubType]],El.Right);
      if (LeftResolved.SubType=RightResolved.SubType)
          or ((LeftResolved.SubType in btAllInteger) and (RightResolved.SubType in btAllInteger))
          or ((LeftResolved.SubType in btAllBooleans) and (RightResolved.SubType in btAllBooleans))
      then
      else
        RaiseMsg(20170216152117,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          ['set of '+BaseTypeNames[RightResolved.SubType],'set of '+BaseTypeNames[LeftResolved.SubType]],El.Right);
      end
    else if (LeftResolved.BaseType=btPointer)
        and (El.Kind in [akAdd,akMinus])
        and (rrfReadable in RightResolved.Flags)
        and (RightResolved.BaseType in btAllInteger)
        and (PointerMathBoolSwitchEnabled(El)
          or IsPointerMathType(LeftResolved.LoTypeEl)) then
      begin
      // Ptr+=n / Ptr-=n, the same rule as the binary Ptr+n form above:
      // generics.hashes walks its key with `AKey += 3`.
      end
    else if LeftResolved.BaseType=btContext then
      begin
      if (LeftResolved.LoTypeEl.ClassType=TPasArrayType) and (El.Kind=akAdd)
          and (rrfReadable in RightResolved.Flags)
          and IsDynArray(LeftResolved.LoTypeEl) then
        begin
        // DynArr+=...
        CheckAssignCompatibilityArrayType(LeftResolved,RightResolved,El,true);
        exit;
        end
      else if (LeftResolved.LoTypeEl.ClassType=TPasPointerType)
          and (El.Kind in [akAdd,akMinus])
          and (rrfReadable in RightResolved.Flags)
          and (RightResolved.BaseType in btAllInteger)
          and (PointerMathBoolSwitchEnabled(El)
            or IsPointerMathType(LeftResolved.LoTypeEl)) then
        begin
        // TypedPtr+=n / TypedPtr-=n, in element steps
        end
      else
        RaiseIncompatibleTypeRes(20180615235749,nOperatorIsNotOverloadedAOpB,[AssignKindNames[El.Kind]],LeftResolved,RightResolved,El);
      end
    else
      RaiseIncompatibleTypeRes(20180208115707,nOperatorIsNotOverloadedAOpB,[AssignKindNames[El.Kind]],LeftResolved,RightResolved,El);
    // store const expression result
    Value:=Eval(El.Right,[]);
    ReleaseEvalValue(Value);
    end;
  else
    RaiseNotYetImplemented(20160927143649,El,'AssignKind '+AssignKindNames[El.Kind]);
  end;
end;

procedure TPasResolver.ResolveImplSimple(El: TPasImplSimple);
var
  ExprResolved: TPasResolverResult;
  Expr: TPasExpr;
begin
  Expr:=El.expr;
  ResolveExpr(Expr,rraRead);
  ComputeElement(Expr,ExprResolved,[rcSetReferenceFlags]);
  if (rrfCanBeStatement in ExprResolved.Flags) then
    begin
    // {$X-} (extended syntax off): a function/constructor result cannot be
    // silently discarded as a statement (FPC tnoext1/2 -> "Illegal expression").
    // Only enforce this on a target that actually models the $X switch: pas2js
    // does not allow bsExtendedSyntax, so extended syntax is effectively always
    // on there and a discarded function result must stay legal (upstream behaviour).
    if (rrfReadable in ExprResolved.Flags)
        and (bsExtendedSyntax in CurrentParser.Scanner.AllowedBoolSwitches)
        and not (bsExtendedSyntax in CurrentParser.Scanner.CurrentBoolSwitches) then
      RaiseMsg(20260622170000,nIllegalExpression,sIllegalExpression,[],El);
    exit;
    end;
  // A call on a still-unbound template type is a statement whose callability can
  // only be judged at specialization (`T(Dest^)._Release;` in rtl/objpas/fgl.pp).
  if IsGenericTemplType(ExprResolved) then
    exit;
  // An expression of a parameterless procedural type is an implicit call when it
  // stands alone, e.g. fcl-db's `StdExportRegProcs[f];`.
  if (ExprResolved.BaseType=btContext)
      and (ExprResolved.LoTypeEl is TPasProcedureType)
      and not ProcNeedsParams(TPasProcedureType(ExprResolved.LoTypeEl)) then
    exit;
  // Reading a property whose getter is a METHOD is a CALL: it stands alone as a
  // statement and the result is simply discarded, which is what ppcx64 does -
  // fcl-extra's fileinfo writes `FVersionInfo.FixedInfo;` purely for the
  // getter's side effect. A FIELD-backed property has no call to make and stays
  // illegal, exactly as ppcx64 rejects it.
  if (ExprResolved.IdentEl is TPasProperty)
      and (GetPasPropertyGetter(TPasProperty(ExprResolved.IdentEl)) is TPasProcedure) then
    exit;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveImplSimple El=',GetObjName(El),' El.Expr=',GetObjName(El.Expr),' ExprResolved=',GetResolverResultDbg(ExprResolved));
  {$ENDIF}
  RaiseMsg(20170216152127,nIllegalExpression,sIllegalExpression,[],El);
end;

procedure TPasResolver.ResolveImplRaise(El: TPasImplRaise);
var
  ResolvedEl: TPasResolverResult;
begin
  if El.ExceptObject<>nil then
    begin
    ResolveExpr(El.ExceptObject,rraRead);
    ComputeElement(El.ExceptObject,ResolvedEl,[rcSetReferenceFlags]);
    CheckIsClass(El.ExceptObject,ResolvedEl);
    if ResolvedEl.IdentEl<>nil then
      begin
      if (ResolvedEl.IdentEl is TPasVariable)
          or (ResolvedEl.IdentEl is TPasArgument)
          or (ResolvedEl.IdentEl is TPasResultElement) then
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveImplRaise ',GetResolverResultDbg(ResolvedEl));
        {$ENDIF}
        RaiseXExpectedButYFound(20170216152133,
                 'variable',GetElementTypeName(ResolvedEl.IdentEl),El.ExceptObject);
        end;
      end
    else if ResolvedEl.ExprEl<>nil then
    else
      RaiseXExpectedButYFound(201702303145230,
             'variable',GetResolverResultDbg(ResolvedEl),El.ExceptObject);
    if not (rrfReadable in ResolvedEl.Flags) then
      RaiseMsg(20170303145037,nNotReadable,sNotReadable,[],El.ExceptObject);
    end;
  if El.ExceptAddr<>nil then
    ResolveExpr(El.ExceptAddr,rraRead);
end;

procedure TPasResolver.ResolveExpr(El: TPasExpr; Access: TResolvedRefAccess);
var
  Primitive: TPrimitiveExpr;
  ElClass: TClass;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveExpr ',GetObjName(El),' ',Access);
  {$ENDIF}
  if El=nil then
    RaiseNotYetImplemented(20160922163453,El);
  ElClass:=El.ClassType;
  if ElClass=TPrimitiveExpr then
    begin
    Primitive:=TPrimitiveExpr(El);
    case Primitive.Kind of
    pekIdent: ResolveNameExpr(El,Primitive.Value,Access);
    pekNumber: ;
    pekString,pekStringMultiLine: ;
    pekNil,pekBoolConst: ;
    else
      RaiseNotYetImplemented(20160922163451,El);
    end;
    end
  else if ElClass=TUnaryExpr then
    begin
    // In P^ := x the operand P is READ - only the memory it points at is
    // written. Passing the write access down rejects a function result
    // (GetPtr(i)^ := x), which FPC accepts.
    if TUnaryExpr(El).OpCode=eopTypeOf then
      ResolveTypeOfExpr(TUnaryExpr(El),Access)
    else if TUnaryExpr(El).OpCode=eopDeref then
      ResolveExpr(TUnaryExpr(El).Operand,rraRead)
    else
      ResolveExpr(TUnaryExpr(El).Operand,Access);
    end
  else if ElClass=TBinaryExpr then
    ResolveBinaryExpr(TBinaryExpr(El),Access)
  else if ElClass=TParamsExpr then
    ResolveParamsExpr(TParamsExpr(El),Access)
  else if ElClass=TBoolConstExpr then
  else if ElClass=TNilExpr then
  else if ElClass=TInheritedExpr then
    ResolveInherited(TInheritedExpr(El),Access)
  else if ElClass=TArrayValues then
    begin
    if Access<>rraRead then
      RaiseMsg(20170303205743,nVariableIdentifierExpected,sVariableIdentifierExpected,
        [],El);
    ResolveArrayValues(TArrayValues(El));
    end
  else if ElClass=TRecordValues then
    begin
    if Access<>rraRead then
      RaiseMsg(20180429103024,nVariableIdentifierExpected,sVariableIdentifierExpected,
        [],El);
    ResolveRecordValues(TRecordValues(El));
    end
  else if ElClass=TProcedureExpr then
    // resolved by FinishScope(stProcedure)
  else if ElClass=TInlineSpecializeExpr then
    ResolveInlineSpecializeExpr(TInlineSpecializeExpr(El),Access)
  else if ElClass=TIfExpr then
    ResolveIfExpr(TIfExpr(El),Access)
  else if ElClass=TCaseExpr then
    ResolveCaseExpr(TCaseExpr(El),Access)
  else if ElClass=TTryExceptExpr then
    ResolveTryExceptExpr(TTryExceptExpr(El),Access)
  else
    RaiseNotYetImplemented(20170222184329,El);

  if El.Format1<>nil then
    ResolveExpr(El.Format1,rraRead);
  if El.Format2<>nil then
    ResolveExpr(El.Format2,rraRead);
end;

procedure TPasResolver.ResolveIfExpr(El: TIfExpr; Access: TResolvedRefAccess);
var
  ResolvedEl: TPasResolverResult;
begin
  // an if-expression is a value, it cannot be assigned or passed as var
  if not (Access in [rraNone,rraRead,rraParamToUnknownProc]) then
    RaiseMsg(20260910120200,nVariableIdentifierExpected,sVariableIdentifierExpected,
      [],El);
  ResolveStatementConditionExpr(El.ConditionExpr);
  ResolveExpr(El.ThenExpr,rraRead);
  ResolveExpr(El.ElseExpr,rraRead);
  // check that then- and else-part are compatible
  ComputeIfExpr(El,ResolvedEl,[],El);
end;

procedure TPasResolver.ResolveCaseExpr(El: TCaseExpr;
  Access: TResolvedRefAccess);
var
  ResolvedEl: TPasResolverResult;
  LabelLists: TFPList;
  i: Integer;
begin
  // a case-expression is a value, it cannot be assigned or passed as var
  if not (Access in [rraNone,rraRead,rraParamToUnknownProc]) then
    RaiseMsg(20260913120200,nVariableIdentifierExpected,sVariableIdentifierExpected,
      [],El);
  LabelLists:=TFPList.Create;
  try
    for i:=0 to El.Branches.Count-1 do
      LabelLists.Add(TCaseExprBranch(El.Branches[i]).Labels);
    ResolveCaseLabels(El.CaseExpr,LabelLists,El.ElseExpr,true);
  finally
    LabelLists.Free;
  end;
  for i:=0 to El.Branches.Count-1 do
    ResolveExpr(TCaseExprBranch(El.Branches[i]).Value,rraRead);
  if El.ElseExpr<>nil then
    ResolveExpr(El.ElseExpr,rraRead);
  // check that all values are compatible
  ComputeCaseExpr(El,ResolvedEl,[],El);
end;

procedure TPasResolver.ResolveTryExceptExpr(El: TTryExceptExpr;
  Access: TResolvedRefAccess);
var
  ResolvedEl: TPasResolverResult;
begin
  // a try-expression is a value, it cannot be assigned or passed as var
  if not (Access in [rraNone,rraRead,rraParamToUnknownProc]) then
    RaiseMsg(20260913130000,nVariableIdentifierExpected,sVariableIdentifierExpected,
      [],El);
  ResolveExpr(El.TryExpr,rraRead);
  // Note: the on-branch values were resolved by FinishExceptOnStatement
  ResolveExpr(El.ElseExpr,rraRead);
  // check that all values are compatible
  ComputeTryExceptExpr(El,ResolvedEl,[],El);
end;

procedure TPasResolver.ComputeIfExpr(El: TIfExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
var
  CondResolved, ElseResolved: TPasResolverResult;
begin
  if rcConstant in Flags then
    ComputeElement(El.ConditionExpr,CondResolved,Flags,StartEl);
  ComputeElement(El.ThenExpr,ResolvedEl,Flags,StartEl);
  ComputeElement(El.ElseExpr,ElseResolved,Flags,StartEl);
  CombineStatementExprBranch(El,ResolvedEl,ElseResolved,El.ThenExpr,El.ElseExpr);
end;

procedure TPasResolver.ComputeCaseExpr(El: TCaseExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
var
  AccExpr: TPasExpr;

  procedure AddValue(ValueExpr: TPasExpr);
  var
    ValueResolved: TPasResolverResult;
  begin
    if AccExpr=nil then
      ComputeElement(ValueExpr,ResolvedEl,Flags,StartEl)
    else
      begin
      ComputeElement(ValueExpr,ValueResolved,Flags,StartEl);
      CombineStatementExprBranch(El,ResolvedEl,ValueResolved,AccExpr,ValueExpr);
      end;
    AccExpr:=ValueExpr;
  end;

var
  CaseResolved: TPasResolverResult;
  i: Integer;
begin
  if rcConstant in Flags then
    ComputeElement(El.CaseExpr,CaseResolved,Flags,StartEl);
  AccExpr:=nil;
  for i:=0 to El.Branches.Count-1 do
    AddValue(TCaseExprBranch(El.Branches[i]).Value);
  if El.ElseExpr<>nil then
    AddValue(El.ElseExpr);
  if AccExpr=nil then
    RaiseInternalError(20260913120300);
  // the result is a value, not a variable, except for class references
  ResolvedEl.ExprEl:=El;
  if not (ResolvedEl.IdentEl is TPasType) then
    begin
    ResolvedEl.IdentEl:=nil;
    ResolvedEl.Flags:=[rrfReadable];
    end;
end;

procedure TPasResolver.ComputeTryExceptExpr(El: TTryExceptExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
var
  LastExpr: TPasExpr;

  procedure AddValue(ValueExpr: TPasExpr);
  var
    ValueResolved: TPasResolverResult;
  begin
    ComputeElement(ValueExpr,ValueResolved,Flags,StartEl);
    CombineStatementExprBranch(El,ResolvedEl,ValueResolved,LastExpr,ValueExpr);
    LastExpr:=ValueExpr;
  end;

var
  i: Integer;
begin
  ComputeElement(El.TryExpr,ResolvedEl,Flags,StartEl);
  LastExpr:=El.TryExpr;
  for i:=0 to El.OnBranches.Count-1 do
    AddValue(TTryExceptExprOn(El.OnBranches[i]).Value);
  AddValue(El.ElseExpr);
  // the result is a value, not a variable, except for class references
  ResolvedEl.ExprEl:=El;
  if not (ResolvedEl.IdentEl is TPasType) then
    begin
    ResolvedEl.IdentEl:=nil;
    ResolvedEl.Flags:=[rrfReadable];
    end;
end;

procedure TPasResolver.CombineStatementExprBranch(El: TPasExpr;
  var CombinedResolved: TPasResolverResult; const NextResolved: TPasResolverResult;
  LastExpr, NextExpr: TPasExpr);
// combines the types of two branches of an if- or case-expression
var
  ThenResolved, ElseResolved, ResolvedEl: TPasResolverResult;
  bt: TResolverBaseType;

  procedure RaiseIncompatible;
  var
    ThenDesc, ElseDesc: String;
  begin
    GetIncompatibleTypeDesc(ThenResolved,ElseResolved,ThenDesc,ElseDesc);
    RaiseMsg(20260910130000,nIncompatibleTypesGotExpected,sIncompatibleTypesXAndY,
      [ThenDesc,ElseDesc],El);
  end;

  function IsClassInstance(const ResolvedEl: TPasResolverResult): boolean;
  begin
    Result:=(ResolvedEl.BaseType=btContext)
      and (ResolvedEl.LoTypeEl is TPasClassType)
      and (TPasClassType(ResolvedEl.LoTypeEl).ObjKind=okClass)
      and not (ResolvedEl.IdentEl is TPasType);
  end;

  function GetCommonAncestor(ThenClass, ElseClass: TPasClassType): TPasClassType;
  var
    Ancestor: TPasType;
  begin
    Ancestor:=ThenClass;
    while Ancestor is TPasClassType do
      begin
      if CheckClassIsClass(ElseClass,Ancestor)<>cIncompatible then
        exit(TPasClassType(Ancestor));
      Ancestor:=GetPasClassAncestor(TPasClassType(Ancestor),true);
      end;
    Result:=nil;
  end;

  function GetClassRef(const ResolvedEl: TPasResolverResult): TPasClassType;
  // returns the class, if ResolvedEl is a class reference,
  // e.g. a class type identifier or a class-of value
  var
    TypeEl: TPasType;
  begin
    Result:=nil;
    if ResolvedEl.BaseType<>btContext then exit;
    TypeEl:=ResolvedEl.LoTypeEl;
    if ResolvedEl.IdentEl is TPasType then
      begin
      // type identifier, e.g. TObject
      if not (TypeEl is TPasClassType) then exit;
      end
    else if TypeEl is TPasClassOfType then
      // class-of value, e.g. a variable of type TClass
      TypeEl:=ResolveAliasType(TPasClassOfType(TypeEl).DestType)
    else
      exit;
    if (TypeEl is TPasClassType) and (TPasClassType(TypeEl).ObjKind=okClass) then
      Result:=TPasClassType(TypeEl);
  end;

var
  CommonClass, ThenClassRef, ElseClassRef: TPasClassType;
begin
  ThenResolved:=CombinedResolved;
  ElseResolved:=NextResolved;

  ThenClassRef:=GetClassRef(ThenResolved);
  ElseClassRef:=GetClassRef(ElseResolved);
  if (ThenClassRef<>nil) or (ElseClassRef<>nil) then
    begin
    // class references, e.g. TAnt and TBird -> TAnimal
    if ThenResolved.BaseType=btNil then
      CommonClass:=ElseClassRef
    else if ElseResolved.BaseType=btNil then
      CommonClass:=ThenClassRef
    else if (ThenClassRef=nil) or (ElseClassRef=nil) then
      // class reference and something else, e.g. an instance
      CommonClass:=nil
    else
      CommonClass:=GetCommonAncestor(ThenClassRef,ElseClassRef);
    if CommonClass=nil then
      RaiseIncompatible;

    SetResolverIdentifier(ResolvedEl,btContext,CommonClass,CommonClass,CommonClass,[]);
    ResolvedEl.ExprEl:=El;
    CombinedResolved:=ResolvedEl;
    exit;
    end;

  if (ThenResolved.BaseType=btNil) and (ElseResolved.BaseType=btNil) then
    ResolvedEl:=ThenResolved
  else if ThenResolved.BaseType=btNil then
    begin
    if CheckAssignResCompatibility(ElseResolved,ThenResolved,LastExpr,false)=cIncompatible then
      RaiseIncompatible;
    ResolvedEl:=ElseResolved;
    end
  else if ElseResolved.BaseType=btNil then
    begin
    if CheckAssignResCompatibility(ThenResolved,ElseResolved,NextExpr,false)=cIncompatible then
      RaiseIncompatible;
    ResolvedEl:=ThenResolved;
    end
  else
    begin
    // e.g. char and string -> string, byte and int64 -> int64
    bt:=GetCombinedBaseType(ThenResolved,ElseResolved,El);
    if bt<>btNone then
      begin
      if (ThenResolved.BaseType=btShortString) and (ElseResolved.BaseType=btShortString)
          and (ThenResolved.HiTypeEl<>ElseResolved.HiTypeEl) then
        // two different shortstrings, e.g. string[3] and string[5] -> String
        SetResolverValueExpr(ResolvedEl,btString,FBaseTypes[btString],FBaseTypes[btString],El,[rrfReadable])
      else if bt=ThenResolved.BaseType then
        ResolvedEl:=ThenResolved
      else if bt=ElseResolved.BaseType then
        ResolvedEl:=ElseResolved
      else
        SetResolverValueExpr(ResolvedEl,bt,FBaseTypes[bt],FBaseTypes[bt],El,[rrfReadable]);
      end
    else if CheckAssignResCompatibility(ThenResolved,ElseResolved,NextExpr,false)<>cIncompatible then
      // e.g. TAnimal and TDog -> TAnimal
      ResolvedEl:=ThenResolved
    else if CheckAssignResCompatibility(ElseResolved,ThenResolved,LastExpr,false)<>cIncompatible then
      ResolvedEl:=ElseResolved
    else if IsClassInstance(ThenResolved) and IsClassInstance(ElseResolved) then
      begin
      // e.g. TAnt and TBird -> TAnimal
      CommonClass:=GetCommonAncestor(TPasClassType(ThenResolved.LoTypeEl),
                                     TPasClassType(ElseResolved.LoTypeEl));
      if CommonClass=nil then
        RaiseIncompatible;
      SetResolverValueExpr(ResolvedEl,btContext,CommonClass,CommonClass,El,[rrfReadable]);
      end
    else
      RaiseIncompatible;
    end;
  // the result is a value, not a variable
  ResolvedEl.IdentEl:=nil;
  ResolvedEl.ExprEl:=El;
  ResolvedEl.Flags:=[rrfReadable];
  CombinedResolved:=ResolvedEl;
end;
