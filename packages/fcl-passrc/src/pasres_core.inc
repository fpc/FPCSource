{%MainUnit pasresolver.pp}
{ **********************************************************************
 
    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - Resolver core

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }


constructor TPasResolver.Create;
begin
  inherited Create;
  FDefaultScope:=TPasDefaultScope.Create;
  FPendingForwardProcs:=TFPList.Create;
  FBaseTypeChar:={$ifdef FPC_HAS_CPSTRING}btAnsiChar{$else}btWideChar{$endif};
  FBaseTypeString:={$ifdef FPC_HAS_CPSTRING}btAnsiString{$else}btUnicodeString{$endif};
  FBaseTypeExtended:=btDouble;
  FBaseTypeLength:={$ifdef HasInt64}btInt64{$else}btIntDouble{$endif};
  FDynArrayMinIndex:=0;
  FDynArrayMaxIndex:=High(TMaxPrecInt);

  cTGUIDToString:=cTypeConversion+1;
  cStringToTGUID:=cTypeConversion+1;
  cInterfaceToTGUID:=cTypeConversion+1;
  cInterfaceToString:=cTypeConversion+2;

  FScopeClass_Array:=TPasArrayScope;
  FScopeClass_Class:=TPasClassScope;
  FScopeClass_InitialFinalization:=TPasInitialFinalizationScope;
  FScopeClass_Module:=TPasModuleScope;
  FScopeClass_Proc:=TPasProcedureScope;
  FScopeClass_ProcType:=TPasProcTypeScope;
  FScopeClass_EnumType:=TPasEnumTypeScope;
  FScopeClass_Record:=TPasRecordScope;
  FScopeClass_Section:=TPasSectionScope;
  FScopeClass_WithExpr:=TPasWithExprScope;
  fExprEvaluator:=CreateExprEvaluator;
  fExprEvaluator.OnLog:=@OnExprEvalLog;
  fExprEvaluator.OnEvalIdentifier:=@OnExprEvalIdentifier;
  fExprEvaluator.OnEvalParams:=@OnExprEvalParams;
  fExprEvaluator.OnRangeCheckEl:=@OnRangeCheckEl;
  PushScope(FDefaultScope);
end;

function TPasResolver.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
var
  aScanner: TPascalScanner;
  SrcPos: TPasSourcePos;
begin
  // get source position for good error messages
  aScanner:=CurrentParser.Scanner;
  if (ASourceFilename='') or StoreSrcColumns then
    begin
    SrcPos.FileName:=aScanner.CurFilename;
    SrcPos.Row:=aScanner.CurRow;
    SrcPos.Column:=aScanner.CurColumn;
    end
  else
    begin
    SrcPos.FileName:=ASourceFilename;
    SrcPos.Row:=ASourceLinenumber;
    SrcPos.Column:=0;
    end;
  Result:=CreateElement(AClass,AName,AParent,AVisibility,SrcPos);
end;

function TPasResolver.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASrcPos: TPasSourcePos; TypeParams: TFPList): TPasElement;
var
  El: TPasElement;
  SrcY: integer;
  SectionScope: TPasSectionScope;
begin
  Result:=nil;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CreateElement ',AClass.ClassName,' Name=',AName,' Parent=',GetObjName(AParent),' (',ASrcPos.Row,',',ASrcPos.Column,')');
  {$ENDIF}
  if (AParent=nil) and (FRootElement<>nil) then
    RaiseInternalError(20160922163535,'more than one root element Class="'+AClass.ClassName+'" Root='+GetObjName(FRootElement));

  if ASrcPos.FileName='' then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CreateElement ',AClass.ClassName,' Name=',AName,' Parent=',GetObjName(AParent),' (',ASrcPos.Row,',',ASrcPos.Column,')');
    {$ENDIF}
    RaiseInternalError(20160922163541,'missing filename');
    end;
  SrcY:=ASrcPos.Row;
  if StoreSrcColumns then
    SrcY:=MangleSourceLineNumber(SrcY,ASrcPos.Column);

  if AClass=TSelfExpr then
    RaiseInternalError(20190131154235);

  // create element
  El:=CreateOwnedElement(AClass,AName,AParent);
  FLastElement:=El;
  El.Visibility:=AVisibility;
  El.SourceFilename:=ASrcPos.FileName;
  El.SourceLinenumber:=SrcY;
  if FRootElement=nil then
    begin
    RootElement:=El as TPasModule;
    if FStep=prsInit then
      FStep:=prsParsing;
    end
  else if (AParent is TPasSection) and (TPasSection(AParent).Declarations.Count=0) then
    begin
    // first element of section
    SectionScope:=TPasSectionScope(AParent.CustomData);
    SectionScope.BoolSwitches:=CurrentParser.Scanner.CurrentBoolSwitches;
    SectionScope.ModeSwitches:=CurrentParser.Scanner.CurrentModeSwitches;
    end;

  if IsElementSkipped(El) then exit;

  // create scope
  if AClass.InheritsFrom(TPasExpr) then
    // resolved when finished
  else if (AClass=TPasVariable)
      or (AClass=TPasConst) then
    AddVariable(TPasVariable(El))
  else if AClass=TPasResString then
    AddResourceString(TPasResString(El))
  else if (AClass=TPasProperty) then
    AddProperty(TPasProperty(El))
  else if AClass=TPasArgument then
    AddArgument(TPasArgument(El))
  else if AClass=TPasEnumType then
    AddEnumType(TPasEnumType(El))
  else if AClass=TPasEnumValue then
    AddEnumValue(TPasEnumValue(El))
  else if (AClass=TUnresolvedPendingRef) then
  else if (AClass=TPasAliasType)
      or (AClass=TPasTypeAliasType)
      or (AClass=TPasClassOfType)
      or (AClass=TPasPointerType)
      or (AClass=TPasSetType)
      or (AClass=TPasRangeType)
      or (AClass=TPasFileType)
      or (AClass=TPasSpecializeType)
      or (AClass=TPasTypeOfType) then
    AddType(TPasType(El))
  else if AClass=TPasArrayType then
    AddArrayType(TPasArrayType(El),TypeParams)
  else if (AClass=TPasProcedureType)
      or (AClass=TPasFunctionType) then
    AddProcedureType(TPasProcedureType(El),TypeParams)
  else if AClass=TPasGenericTemplateType then
    AddGenericTemplateType(TPasGenericTemplateType(El))
  else if AClass=TPasStringType then
    begin
    AddType(TPasType(El));
    {$ifdef FPC_HAS_CPSTRING}
    if BaseTypes[btShortString]=nil then
    {$endif}
      RaiseMsg(20170419203043,nIllegalQualifier,sIllegalQualifier,['['],El);
    end
  else if AClass=TPasRecordType then
    AddRecordType(TPasRecordType(El),TypeParams)
  else if AClass=TPasVariant then
    AddRecordVariant(TPasVariant(El))
  else if AClass=TPasClassType then
    AddClassType(TPasClassType(El),TypeParams)
  else if AClass.InheritsFrom(TPasProcedure) then
    AddProcedure(TPasProcedure(El),TypeParams)
  else if AClass=TPasResultElement then
    AddFunctionResult(TPasResultElement(El))
  else if AClass=TProcedureBody then
    AddProcedureBody(TProcedureBody(El))
  else if AClass=TPasMethodResolution then
  else if AClass=TPasImplExceptOn then
    AddExceptOn(TPasImplExceptOn(El))
  else if AClass=TTryExceptExprOn then
    AddTryExceptExprOn(TTryExceptExprOn(El))
  else if AClass=TPasImplWithDo then
    AddWithDo(TPasImplWithDo(El))
  else if AClass=TPasImplLabelMark then
  else if AClass=TPasOverloadedProc then
  else if (AClass=TInterfaceSection)
      or (AClass=TImplementationSection)
      or (AClass=TProgramSection)
      or (AClass=TLibrarySection) then
    AddSection(TPasSection(El))
  else if (AClass=TPasModule)
      or (AClass=TPasProgram)
      or (AClass=TPasLibrary) then
    AddModule(TPasModule(El))
  else if AClass=TPasUsesUnit then
  else if AClass=TInitializationSection then
    AddInitialFinalizationSection(TInitializationSection(El))
  else if AClass=TFinalizationSection then
    AddInitialFinalizationSection(TFinalizationSection(El))
  else if AClass=TPasImplCommand then
  else if AClass=TCaseExprBranch then
    // resolved by ResolveCaseExpr
  else if AClass=TPasLabels then
    // a `label` section declares names only; nothing to put in a scope
  else if AClass.InheritsFrom(TPasImplBlock) then
    // resolved when finished
  else if AClass=TPasAttributes then
  else if AClass=TPasExportSymbol then
    AddExportSymbol(TPasExportSymbol(El))
  else if AClass=TPasContainsAlias then
    // resolved by FinishContainsAlias
  else if AClass=TPasUnresolvedUnitRef then
    RaiseMsg(20171018121900,nCantFindUnitX,sCantFindUnitX,[AName],El)
  else
    RaiseNotYetImplemented(20160922163544,El);

  Result:=El;

  // Record whether this node was parsed while range/overflow checking was
  // active, so a consumer can emit the check. Captured here because the scanner
  // switch state is only correct at parse time; a specialization propagates the
  // flags in SpecializeElement.
  if (CurrentParser<>nil) and (CurrentParser.Scanner<>nil) then
    begin
    // TParamsExpr covers `a[i]`, whose index a consumer range-checks against the
    // array's declared bounds. Its Kind is set by the parser after this point,
    // so every params expression is marked and the consumer picks the indexing
    // ones out. Every TPasImplElement is marked as well, so a consumer emitting
    // a check that belongs to no expression of its own (a nil-instance check on
    // a virtual call, say) can read the state off the enclosing STATEMENT rather
    // than off a scope, whose snapshot is taken once and cannot follow a
    // directive that toggles mid-body.
    if ((El is TPasImplElement) or (El is TParamsExpr))
        and (bsRangeChecks in CurrentParser.Scanner.CurrentBoolSwitches) then
      MarkRangeChecked(El);
    if (El is TBinaryExpr)
        and (bsOverflowChecks in CurrentParser.Scanner.CurrentBoolSwitches) then
      MarkOverflowChecked(El);

    // Capture the active packing directive per type at its declaration. The
    // Set* seams are no-ops in the base (pas2js-safe); a native resolver stores
    // them per element.
    if El is TPasEnumType then
      SetMinEnumSize(El,
        StrToIntDef(CurrentParser.Scanner.CurrentValueSwitch[vsMinEnumSize],0))
    else if El is TPasSetType then
      SetPackSet(El,
        StrToIntDef(CurrentParser.Scanner.CurrentValueSwitch[vsPackSet],0))
    else if El is TPasRecordType then
      SetPackRecords(El,
        StrToIntDef(CurrentParser.Scanner.CurrentValueSwitch[vsPackRecords],0));

    // A pointer type declared while {$POINTERMATH ON} permits pointer arithmetic
    // for the rest of its life, independent of the use-site switch (target-
    // agnostic parse fact; only a native resolver queries it via IsPointerMathType).
    if (El is TPasPointerType)
        and (bsPointerMath in CurrentParser.Scanner.CurrentBoolSwitches) then
      Include(El.States,pesfPointerMath);
    end;
end;

procedure TPasResolver.MarkRangeChecked(El: TPasElement);
begin
  if El<>nil then
    Include(El.States,pesfRangeChecked);
end;

function TPasResolver.IsRangeChecked(El: TPasElement): Boolean;
begin
  Result:=(El<>nil) and (pesfRangeChecked in El.States);
end;

procedure TPasResolver.MarkOverflowChecked(El: TPasElement);
begin
  if El<>nil then
    Include(El.States,pesfOverflowChecked);
end;

function TPasResolver.IsOverflowChecked(El: TPasElement): Boolean;
begin
  Result:=(El<>nil) and (pesfOverflowChecked in El.States);
end;

procedure TPasResolver.SetMinEnumSize(El: TPasElement; ASize: Integer);
begin
  if El is TPasEnumType then
    TPasEnumType(El).MinSize:=ASize;
end;

function TPasResolver.GetMinEnumSize(El: TPasElement): Integer;
begin
  if El is TPasEnumType then
    Result:=TPasEnumType(El).MinSize
  else
    Result:=0; // natural size
end;

procedure TPasResolver.SetPackSet(El: TPasElement; ASize: Integer);
begin
  if El is TPasSetType then
    TPasSetType(El).PackSet:=ASize;
end;

function TPasResolver.GetPackSet(El: TPasElement): Integer;
begin
  if El is TPasSetType then
    Result:=TPasSetType(El).PackSet
  else
    Result:=0;
end;

procedure TPasResolver.SetPackRecords(El: TPasElement; ASize: Integer);
begin
  if (El=nil) or (ASize=0) then ;
end;

function TPasResolver.GetPackRecords(El: TPasElement): Integer;
begin
  Result:=0;
  if El=nil then ;
end;

function TPasResolver.AllowHelperVirtualMethods: Boolean;
begin
  // Default fcl-passrc policy: virtual/override in a helper is unsupported
  // (Delphi 10.3 crashes on them) — TestClassHelper_VirtualDelphiFail expects
  // the rejection. A native/FPC target overrides this to allow it in Delphi mode.
  Result:=False;
end;

function TPasResolver.AllowImplRepeatConstraints: Boolean;
begin
  // Default fcl-passrc policy: the impl of a forward generic proc must not repeat
  // constraints (TestGenProc_ForwardConstraintsRepeatFail). Real FPC allows it.
  Result:=False;
end;

function TPasResolver.InferTemplTypesFromDefaults: Boolean;
begin
  // Default fcl-passrc policy: infer template types from omitted args' default
  // values (TestGenProc_Infer_DefaultValue). Real FPC does not (timpfuncspez16/17).
  Result:=True;
end;

function TPasResolver.AllowRecordGenericSelfReference: Boolean;
begin
  // Default fcl-passrc policy: a generic record must not reference its own
  // unfinished specialization (TestGen_Record_ReferGenericSelfFail). Real FPC
  // allows it in a method signature (tgeneric76).
  Result:=False;
end;

function TPasResolver.SameSpecializeType(SpecA, SpecB: TPasSpecializeType;
  ResolveAlias: TPRResolveAlias): Boolean;
begin
  // Default fcl-passrc policy: two distinct specialize nodes are only the same
  // when they are the identical node (handled by the caller). A native/FPC
  // target overrides this to compare generic + arguments structurally.
  if SpecA=nil then ;
  if SpecB=nil then ;
  if ResolveAlias=prraNone then ;
  Result:=False;
end;

function TPasResolver.StoreProcTypeScopeBoolSwitches: Boolean;
begin
  // Default fcl-passrc policy: a named non-generic proc type gets no scope
  // (matches pristine upstream; keeps pas2js PCU round-trip stable). A native/FPC
  // target overrides this to capture bool switches for funcref {$M+} RTTI.
  Result:=False;
end;

function TPasResolver.PreferNarrowerInferredInteger: Boolean;
begin
  // Default fcl-passrc policy (pristine upstream): widen two integer inferences to
  // a common base type. A native/FPC target overrides to keep the narrower one
  // when the wider fully contains it (timpfuncspez13).
  Result:=False;
end;

function TPasResolver.AllowGenericPublishedMethod: Boolean;
begin
  // Default fcl-passrc policy: reject a generic method with published visibility
  // (sXMethodsCannotHaveTypeParams). Delphi accepts it; a native/FPC target
  // overrides to allow it (GitLab #41410).
  Result:=False;
end;

function TPasResolver.TypeCastKeepsLValue(ToLoType: TPasType;
  const FromRes: TPasResolverResult): Boolean;
begin
  // Default fcl-passrc policy: only the same-size ordinal rule applies.
  Result:=False;
  if ToLoType=nil then ;
  if FromRes.BaseType=btNone then ;
end;

function TPasResolver.WideStringMatchesUnicodeString(BtA,
  BtB: TResolverBaseType): Boolean;
begin
  // Default fcl-passrc policy: the two are separate types.
  Result:=False;
  if BtA=BtB then ;
end;

function TPasResolver.AllowTypeQualifiedMethodAddress: Boolean;
begin
  // Default fcl-passrc policy: visibility is checked, as in Delphi.
  Result:=False;
end;

function TPasResolver.AllowArgumentNamedSelf: Boolean;
begin
  // Default fcl-passrc policy: a duplicate identifier, as in Delphi.
  Result:=False;
end;

function TPasResolver.AllowGenericEnumValuesInSection: Boolean;
begin
  // Default fcl-passrc policy: they stay inside the generic.
  Result:=False;
end;

function TPasResolver.AllowWriteToWithTempRecord: Boolean;
begin
  // Default fcl-passrc policy: such a record is read-only, as in Delphi.
  Result:=False;
end;

function TPasResolver.AllowNilDereference: Boolean;
begin
  // Default fcl-passrc policy: `nil^` is an error.
  Result:=False;
end;

function TPasResolver.AllowPublishedClassMethod: Boolean;
// Whether a CLASS/static method may sit in a published section. FPC allows it
// (fcl-db's TConnectionDef publishes `class function TypeName`), pas2js does
// not, and its suite asserts the rejection - so the base keeps saying no.
begin
  Result := False;
end;


function TPasResolver.AllowPublishedMethodOverload: Boolean;
begin
  // Default fcl-passrc policy: a published method name must be unique.
  Result:=False;
end;

function TPasResolver.AllowTypeQualifiedInstanceField(Found: TPasElement;
  PosEl: TPasElement): Boolean;
begin
  // Default fcl-passrc policy: an instance member always needs an instance.
  Result:=False;
  if Found=nil then ;
  if PosEl=nil then ;
end;

function TPasResolver.IsNameOfArgument(El: TPasElement): boolean;
var
  Parent: TPasElement;
  Params: TParamsExpr;
begin
  Result:=false;
  if FBuiltInProcs[bfNameOf]=nil then exit;
  while (El<>nil) and (El.Parent is TPasExpr) do
    begin
    Parent:=El.Parent;
    if Parent.ClassType=TParamsExpr then
      begin
      Params:=TParamsExpr(Parent);
      Result:=(Params.Kind=pekFuncParams) and (Params.Value<>El)
          and (length(Params.Params)=1)
          and IsNameExpr(Params.Value)
          and SameText(TPrimitiveExpr(Params.Value).Value,'NameOf');
      exit; // some other call -> not a NameOf argument
      end;
    El:=Parent;
    end;
end;

function TPasResolver.UseTentativeImplicitSpecMatch: Boolean;
begin
  // Default fcl-passrc policy: score implicit-spec candidates with the full type
  // check, so multi-overload disambiguation works (TestGenProc_Infer_Overload).
  // A native/FPC target overrides this to accept a tentative match.
  Result:=False;
end;

function TPasResolver.PointerMathBoolSwitchEnabled(El: TPasElement): Boolean;
begin
  // Default: the bool switch stored on El's enclosing scope.
  Result:=ElHasBoolSwitch(El,bsPointerMath);
end;

function TPasResolver.IsPointerMathType(El: TPasType): Boolean;
begin
  // pas2js-safe default: no pointer arithmetic. TPasNativeResolver overrides.
  Result:=False;
  if El=nil then ;
end;

function TPasResolver.AllowIncDecOnPointer(El: TPasType): Boolean;
begin
  // pas2js-safe default: no pointer Inc/Dec. TPasNativeResolver overrides.
  Result:=False;
  if El=nil then ;
end;

function TPasResolver.AllowIncDecOnOrdinal(bt: TResolverBaseType; El: TPasType): Boolean;
begin
  // pas2js-safe default: only integers. TPasNativeResolver overrides.
  Result:=False;
  if (bt=btNone) and (El=nil) then ;
end;

function TPasResolver.AllowInExcludeNonEnumSet: Boolean;
begin
  // pas2js-safe default: only enum/subrange sets. TPasNativeResolver overrides.
  Result:=False;
end;

function TPasResolver.IsBitPackedOrdinalAccess(Expr: TPasExpr): boolean;
begin
  // pas2js-safe default: no bit-packing. TPasNativeResolver overrides this to
  // detect a bit-packed ordinal field/element access.
  Result:=False;
  if Expr=nil then ;
end;

procedure TPasResolver.SpecializePackValues(GenEl, SpecEl: TPasElement);
begin
  // Enum and set pack values are fields, copied by SpecializeEnumType/SpecializeSetType.
  if GetPackRecords(GenEl)>0 then
    SetPackRecords(SpecEl,GetPackRecords(GenEl));
end;

function TPasResolver.CreateOwnedElement(AClass: TPTreeElement;
  const AName: String; AParent: TPasElement): TPasElement;
begin
  Result:=AClass.Create(AName,AParent);
  FOwnedElements.Add(Result);
end;

function TPasResolver.FindModule(const AName: String; NameExpr,
  InFileExpr: TPasExpr): TPasModule;
var
  InFilename, FileUnitName: String;
begin
  if InFileExpr<>nil then
    begin
    InFilename:=GetUsesUnitInFilename(InFileExpr);
    if InFilename='' then
      RaiseXExpectedButYFound(20180222001220,
               'file path','empty string',InFileExpr);
    if msDelphi in CurrentParser.CurrentModeswitches then
      begin
      // in delphi the last unit name must match the filename
      FileUnitName:=ChangeFileExt(ExtractFileName(InFilename),'');
      if CompareText(AName,FileUnitName)<>0 then
        RaiseXExpectedButYFound(20180222230400,AName,FileUnitName,InFileExpr);
      end;
    end;
  Result:=FindUnit(AName,InFilename,NameExpr,InFileExpr);
  if Result=nil then
    begin
    if InFileExpr<>nil then
      RaiseMsg(20180223140434,nCantFindUnitX,sCantFindUnitX,[InFilename],InFileExpr)
    else
      RaiseMsg(20180223140409,nCantFindUnitX,sCantFindUnitX,[AName],NameExpr);
    end;
end;

function TPasResolver.FindElement(const aName: String): TPasElement;
begin
  Result:=FindElementFor(aName,nil,0);
end;

function TPasResolver.FindElementFor(const aName: String; AParent: TPasElement;
  TypeParamCount: integer): TPasElement;
// called by TPasParser for direct types, e.g. type t = ns1.unit1.tobj.tsub
var
  ErrorEl: TPasElement;

  function IsGenericRefWithoutParamsAllowed(GenEl: TPasGenericType): Boolean;
  // called when TypeParamCount=0 - check if reference to a generic type is allowed
  // Returns True if allowed (self-reference), False if not allowed
  var
    i, j: Integer;
    Scope: TPasScope;
    ScopeEl: TPasElement;
    ProcScope: TPasProcedureScope;
    GenScope: TPasGenericScope;
    SpecItem: TPRSpecializedItem;
  begin
    if (GenEl.GenericTemplateTypes=nil) or (GenEl.GenericTemplateTypes.Count=0) then
      exit(True); // not actually a generic type
    // ObjFPC allows referring to the enclosing generic without params (self-ref);
    // Delphi requires the full specialization, so it is forbidden there (the caller
    // may still defer to a pending ref when a same-named non-generic follows).
    if msDelphi in CurrentParser.CurrentModeswitches then
      exit(False);
    // Check AParent
    if (AParent<>nil) and AParent.HasParent(GenEl) then
      exit(True);
    // Check scope chain
    for i:=ScopeCount-1 downto 0 do
      begin
      Scope:=Scopes[i];
      ScopeEl:=Scope.Element;
      if ScopeEl=GenEl then
        exit(True); // we are inside GenEl's scope
      if (ScopeEl<>nil) and ScopeEl.HasParent(GenEl) then
        exit(True); // we are inside a child scope of GenEl
      // Check if scope element is a specialization of GenEl
      if (ScopeEl<>nil) and (ScopeEl.CustomData is TPasGenericScope) then
        begin
        GenScope:=TPasGenericScope(ScopeEl.CustomData);
        if (GenScope.SpecializedFromItem<>nil)
            and (GenScope.SpecializedFromItem.GenericEl=GenEl) then
          begin
          // Inside a specialization of GenEl. Allow ONLY if GenEl is not
          // one of the type arguments (substituted reference, not self-reference).
          SpecItem:=GenScope.SpecializedFromItem;
          Result:=True;
          for j:=0 to Length(SpecItem.Params)-1 do
            if SpecItem.Params[j]=GenEl then
              begin
              Result:=False;
              break;
              end;
          exit; // F8: definitive answer, stop searching
          end;
        end;
      // Check if this is a method body scope whose class is GenEl
      if (Scope is TPasProcedureScope) then
        begin
        ProcScope:=TPasProcedureScope(Scope);
        if (ProcScope.ClassRecScope<>nil)
            and (ProcScope.ClassRecScope.Element=GenEl) then
          exit(True);
        if (ProcScope.ClassRecScope<>nil)
            and (ProcScope.ClassRecScope.Element<>nil)
            and ProcScope.ClassRecScope.Element.HasParent(GenEl) then
          exit(True);
        end;
      end;
    Result:=False;
  end;

  function ResolveOuterTypeSkippingMember(const aName: String;
    Member: TPasElement): TPasType;
  // A type reference (this routine only ever resolves types) resolved to a
  // record/class member whose own type is not set yet — i.e. a sibling field
  // being declared on the current line, whose name equals its type name only
  // by case (e.g. "hWnd: HWND", with type HWND declared in an outer scope).
  // The member was registered in scope before its own type was parsed, so a
  // by-name lookup wrongly picks it. Walk the scope stack for a real type of
  // that name, skipping the member's own members scope.
  begin
    Result:=FindOuterTypeSkippingElement(aName,Member);
  end;

var
  p: SizeInt;
  RightPath, CurName, LeftPath: String;
  NeedPop: Boolean;
  CurScopeEl, NextEl, BestEl, OuterTypeEl: TPasElement;
  CurSection: TPasSection;
  i: Integer;
  UsesUnit: TPasUsesUnit;
  CurScope: TPasDotBaseScope;
  FindData: TPRFindData;
begin
  Result:=nil;
  ErrorEl:=nil; // use nil to use scanner position as error position

  RightPath:=aName;
  LeftPath:='';
  p:=1;
  CurScopeEl:=nil;
  repeat
    p:=Pos('.',RightPath);
    if p<1 then
      begin
      CurName:=RightPath;
      RightPath:='';
      end
    else
      begin
      CurName:=LeftStr(RightPath,p-1);
      Delete(RightPath,1,p);
      if RightPath='' then
        RaiseMsg(20170328003146,nIllegalExpression,sIllegalExpression,[],ErrorEl);
      end;
    if LeftPath='' then
      LeftPath:=CurName
    else
      LeftPath:=LeftPath+'.'+CurName;
    {$IFDEF VerbosePasResolver}
    {AllowWriteln}
    if RightPath<>'' then
      writeln('TPasResolver.FindElement searching scope "',CurName,'" RightPath="',RightPath,'" ...');
    {AllowWriteln-}
    {$ENDIF}
    // Note: CurName can be a non Pascal name, when specializing an autogenerated anonymous type
    //if not IsValidIdent(CurName) then ;
    if CurScopeEl<>nil then
      begin
      NeedPop:=true;
      if CurScopeEl is TPasType then
        begin
        // `TGen.TNested` inside TGen itself names its own nested type (objfpc)
        if (CurScopeEl is TPasGenericType)
            and not IsFullySpecialized(TPasGenericType(CurScopeEl))
            and not IsGenericRefWithoutParamsAllowed(TPasGenericType(CurScopeEl)) then
          RaiseMsg(20200217131215,nGenericsWithoutSpecializationAsType,
            sGenericsWithoutSpecializationAsType,['reference'],ErrorEl);
        if (CurScopeEl is TPasClassType) and TPasClassType(CurScopeEl).IsForward then
          RaiseMsg(20220527113900,nIdentifierNotFound,
            sIdentifierNotFound,[CurName],ErrorEl);
        CurScope:=PushDotScope(TPasType(CurScopeEl));
        if CurScope=nil then
          RaiseMsg(20190122122529,nIllegalQualifierAfter,sIllegalQualifierAfter,
            ['.',LeftPath],ErrorEl);
        CurScope.OnlyTypeMembers:=true;
        end
      else if CurScopeEl is TPasModule then
        PushModuleDotScope(TPasModule(CurScopeEl))
      else
        RaiseMsg(20170504174021,nIllegalQualifierAfter,sIllegalQualifierAfter,
          ['.',LeftPath],ErrorEl);
      end
    else
      NeedPop:=false;

    if (RightPath='') and (TypeParamCount>0) then
      begin
      NextEl:=FindGenericEl(CurName,TypeParamCount,FindData,ErrorEl);
      if (FindData.StartScope<>nil) and (FindData.StartScope.ClassType=ScopeClass_WithExpr)
          and (wesfNeedTmpVar in TPasWithExprScope(FindData.StartScope).Flags) then
        RaiseInternalError(20190801104033); // caller forgot to handle "With"
      end
    else
      begin
      NextEl:=FindElementWithoutParams(CurName,ErrorEl,true,true);
      if (NextEl is TPasGenericType) and (RightPath='') then
        begin
        if not IsGenericRefWithoutParamsAllowed(TPasGenericType(NextEl)) then
          begin
          // A bare (no <>) reference that only found a generic of this name.
          // In a forward pointer/class-of context a non-generic of the same
          // name may still be declared later in the section (Delphi allows a
          // generic and a non-generic to share a name) -> defer to a pending
          // ref, resolved in FinishTypeSectionEl. This deferral only applies to
          // a NAMED type-section member (which FinishTypeSectionEl re-resolves,
          // erroring if only the generic remains); an anonymous inline pointer
          // (const/var/param) is never re-checked, so it must error now.
          if (CurScopeEl=nil) and (LastElement<>nil) and (LastElement.Name<>'')
              and (((LastElement.ClassType=TPasPointerType)
                    and (TPasPointerType(LastElement).DestType=nil))
                or ((LastElement.ClassType=TPasClassOfType)
                    and (TPasClassOfType(LastElement).DestType=nil))) then
            NextEl:=CreateElement(TUnresolvedPendingRef,CurName,LastElement,
                                  visDefault,CurrentParser.CurSourcePos)
          else
            RaiseMsg(20201129005025,nGenericsWithoutSpecializationAsType,sGenericsWithoutSpecializationAsType,['variable'],ErrorEl);
          end;
        end;
      // Unqualified type reference that hit a still-being-declared member field
      // (VarType=nil) whose name matches its own type name only by case: resolve
      // the real type in an outer scope instead (e.g. record field "hWnd: HWND").
      if (RightPath='') and (CurScopeEl=nil)
          and (NextEl is TPasVariable) and (TPasVariable(NextEl).VarType=nil)
          and (NextEl.Parent is TPasMembersType) then
        begin
        OuterTypeEl:=ResolveOuterTypeSkippingMember(CurName,NextEl);
        if OuterTypeEl<>nil then
          NextEl:=OuterTypeEl;
        end;
      // A type reference that landed on a ROUTINE takes the type of that name
      // from an outer scope: real FPC searches for a type symbol only, so in
      // `class function Integer(const A,B: Integer): Integer` every Integer but
      // the method's own name is the type (rtl-generics generics.defaults).
      if (RightPath='') and (CurScopeEl=nil)
          and (NextEl is TPasProcedure)
          and (NextEl.Parent is TPasMembersType) then
        begin
        OuterTypeEl:=ResolveOuterTypeSkippingMember(CurName,NextEl);
        if OuterTypeEl<>nil then
          NextEl:=OuterTypeEl;
        end;
      // A type reference that landed on a PARAMETER takes the type of that name
      // from an outer scope, as FPC and Delphi do: lclproc's
      // `function SendApplicationMessage(Msg: Cardinal; WParam: WParam; ...)`.
      if (RightPath='') and (CurScopeEl=nil)
          and (NextEl is TPasArgument) then
        begin
        OuterTypeEl:=FindOuterTypeSkippingElement(CurName,NextEl);
        if OuterTypeEl<>nil then
          NextEl:=OuterTypeEl;
        end;
      end;
    {$IFDEF VerbosePasResolver}
    //if RightPath<>'' then
    //  writeln('TPasResolver.FindElement searching scope "',CurName,'" RightPath="',RightPath,'" ... NextEl=',GetObjName(NextEl));
    {$ENDIF}
    if NextEl=nil then
      RaiseIdentifierNotFound(20201129004745,CurName,ErrorEl);
    if NextEl is TPasModule then
      begin
      if CurScopeEl is TPasModule then
        RaiseXExpectedButYFound(20170328001619,'class',GetElementTypeName(NextEl)+' '+NextEl.Name,ErrorEl);
      if Pos('.',NextEl.Name)>0 then
        begin
        // dotted module name -> check if the full module name is in aName
        if CompareText(NextEl.Name+'.',LeftStr(aName,length(NextEl.Name)+1))<>0 then
          begin
          if CompareText(NextEl.Name,aName)=0 then
            RaiseXExpectedButYFound(20170504165825,'type',GetElementTypeName(NextEl),ErrorEl)
          else
            RaiseIdentifierNotFound(20170504165412,aName,ErrorEl);
          end;
        RightPath:=copy(aName,length(NextEl.Name)+2,length(aName));
        end;
      CurScopeEl:=NextEl;
      end
    else if NextEl.ClassType=TPasUsesUnit then
      begin
      // the first name of a used unit matches -> find longest match
      CurSection:=NextEl.Parent as TPasSection;
      i:=length(CurSection.UsesClause)-1;
      BestEl:=nil;
      while i>=0 do
        begin
        UsesUnit:=CurSection.UsesClause[i];
        CurName:=UsesUnit.Name;
        if IsDottedIdentifierPrefix(CurName,aName)
            and ((BestEl=nil) or (length(CurName)>length(BestEl.Name))) then
          BestEl:=UsesUnit;
        dec(i);
        if (i<0) and (CurSection.ClassType=TImplementationSection) then
          begin
          CurSection:=(CurSection.Parent as TPasModule).InterfaceSection;
          if CurSection=nil then break;
          i:=length(CurSection.UsesClause)-1;
          end;
        end;
      // check module name too
      CurName:=RootElement.Name;
      if IsDottedIdentifierPrefix(CurName,aName)
          and ((BestEl=nil) or (length(CurName)>length(BestEl.Name))) then
        BestEl:=RootElement;

      if BestEl=nil then
        RaiseIdentifierNotFound(20170504172440,aName,ErrorEl);
      RightPath:=copy(aName,length(BestEl.Name)+2,length(aName));
      if BestEl.ClassType=TPasUsesUnit then
        CurScopeEl:=TPasUsesUnit(BestEl).Module
      else
        CurScopeEl:=BestEl;
      end
    else
      CurScopeEl:=NextEl;

    // restore scope
    if NeedPop then
      PopScope;

    if RightPath='' then
      exit(NextEl);
  until false;

  if AParent=nil then ;;
end;

function TPasResolver.FindElementWithoutParams(const AName: String;
  ErrorPosEl: TPasElement; NoProcsWithArgs, NoGenerics: boolean): TPasElement;
// ErrorPosEl=nil means to use scanner position as error position
var
  Data: TPRFindData;
begin
  Result:=FindElementWithoutParams(AName,Data,ErrorPosEl,NoProcsWithArgs,NoGenerics);
  if Data.Found=nil then exit; // forward type: class-of or ^
  CheckFoundElement(Data,nil);
  if (Data.StartScope<>nil) and (Data.StartScope.ClassType=ScopeClass_WithExpr)
      and (wesfNeedTmpVar in TPasWithExprScope(Data.StartScope).Flags) then
    RaiseInternalError(20160923111727); // caller forgot to handle "With", use the other FindElementWithoutParams instead
end;

function TPasResolver.FindMemberViaAncestorArgs(const AName: String): TPasElement;
// Inside a generic class whose ancestor is a partial specialization, answered
// with the bare generic: when that generic derives from one of its own type
// parameters, the members come from the ARGUMENT given for it. lazarus'
// lazdebuggerintf derives from TDbgExeProcSelectorListTemplate<
// TDbgSynchronizedListExTemplate<_BASE,..>,..>, whose ancestor is its _BASE.

  function SearchType(T: TPasType; Owner: TPasGenericType;
    const OwnerArgs: TPasTypeArray; Depth: integer): TPasElement; forward;

  function SearchClass(Cls: TPasMembersType; const Args: TPasTypeArray;
    Depth: integer): TPasElement;
  var
    i: Integer;
    M: TPasElement;
  begin
    Result:=nil;
    if Depth>8 then exit;
    for i:=0 to Cls.Members.Count-1 do
      begin
      M:=TPasElement(Cls.Members[i]);
      if SameText(M.Name,AName) then
        exit(M);
      end;
    if (Cls is TPasClassType) and (TPasClassType(Cls).AncestorType<>nil) then
      Result:=SearchType(TPasClassType(Cls).AncestorType,Cls,Args,Depth+1);
  end;

  // The arguments of Spec, with the owner's own type parameters replaced by the
  // arguments given for them.
  function ArgsOf(Spec: TPasSpecializeType; Owner: TPasGenericType;
    const OwnerArgs: TPasTypeArray): TPasTypeArray;
  var
    i, k: Integer;
    P: TPasType;
  begin
    Result:=nil;
    SetLength(Result,Spec.Params.Count);
    for i:=0 to Spec.Params.Count-1 do
      begin
      P:=TPasType(Spec.Params[i]);
      if (P is TPasGenericTemplateType) and (Owner<>nil)
          and (Owner.GenericTemplateTypes<>nil) then
        begin
        k:=Owner.GenericTemplateTypes.IndexOf(P);
        if (k>=0) and (k<length(OwnerArgs)) and (OwnerArgs[k]<>nil) then
          P:=OwnerArgs[k];
        end;
      Result[i]:=P;
      end;
  end;

  // T is a member type of the class constraint of one of the owner's type
  // parameters (`class(_B.TBase)` is stored as the constraint's TBase): the
  // same-named member of the argument given for that parameter, else nil.
  function MemberOfBoundArg(T: TPasType; Owner: TPasGenericType;
    const OwnerArgs: TPasTypeArray): TPasType;
  var
    k, j: Integer;
    Constraint: TPasClassType;
    Bound: TPasType;
    M: TPasElement;
  begin
    Result:=nil;
    if (Owner=nil) or (Owner.GenericTemplateTypes=nil)
        or not (T.Parent is TPasClassType) then exit;
    for k:=0 to Owner.GenericTemplateTypes.Count-1 do
      begin
      if k>=length(OwnerArgs) then break;
      if not (TObject(Owner.GenericTemplateTypes[k]) is TPasGenericTemplateType) then continue;
      Constraint:=GetClassTypeConstraint(TPasGenericTemplateType(Owner.GenericTemplateTypes[k]));
      if (Constraint=nil)
          or (CheckClassIsClass(Constraint,TPasClassType(T.Parent))=cIncompatible) then
        continue;
      Bound:=ResolveAliasType(OwnerArgs[k]);
      while Bound is TPasClassType do
        begin
        for j:=0 to TPasClassType(Bound).Members.Count-1 do
          begin
          M:=TPasElement(TPasClassType(Bound).Members[j]);
          if (M is TPasType) and SameText(M.Name,T.Name) then
            exit(TPasType(M));
          end;
        Bound:=ResolveAliasType(TPasClassType(Bound).AncestorType);
        end;
      end;
  end;

  function SearchType(T: TPasType; Owner: TPasGenericType;
    const OwnerArgs: TPasTypeArray; Depth: integer): TPasElement;
  var
    k: Integer;
    Mapped: TPasType;
  begin
    Result:=nil;
    if (T=nil) or (Depth>8) then exit;
    if T is TPasGenericTemplateType then
      begin
      // the ancestor is a type parameter: take the argument given for it
      if (Owner=nil) or (Owner.GenericTemplateTypes=nil) then exit;
      k:=Owner.GenericTemplateTypes.IndexOf(T);
      if (k<0) or (k>=length(OwnerArgs)) then exit;
      Result:=SearchType(OwnerArgs[k],nil,nil,Depth+1);
      exit;
      end;
    Mapped:=MemberOfBoundArg(T,Owner,OwnerArgs);
    if Mapped<>nil then
      exit(SearchType(Mapped,nil,nil,Depth+1));
    if T is TPasSpecializeType then
      begin
      if TPasSpecializeType(T).DestType is TPasMembersType then
        Result:=SearchClass(TPasMembersType(TPasSpecializeType(T).DestType),
          ArgsOf(TPasSpecializeType(T),Owner,OwnerArgs),Depth+1);
      end
    else if T.ClassType=TPasAliasType then
      Result:=SearchType(TPasAliasType(T).DestType,Owner,OwnerArgs,Depth+1)
    else if T is TPasClassType then
      Result:=SearchClass(TPasClassType(T),nil,Depth+1);
  end;

var
  i: Integer;
  Cls: TPasClassType;
begin
  Result:=nil;
  // the innermost class being resolved
  Cls:=nil;
  for i:=ScopeCount-1 downto 0 do
    if Scopes[i] is TPasClassScope then
      begin
      Cls:=TPasClassType(Scopes[i].Element);
      break;
      end
    else if (Scopes[i] is TPasProcedureScope)
        and (TPasProcedureScope(Scopes[i]).ClassRecScope is TPasClassScope) then
      begin
      Cls:=TPasClassType(TPasProcedureScope(Scopes[i]).ClassRecScope.Element);
      break;
      end;
  if (Cls=nil) or (GetTypeParameterCount(Cls)=0) then exit;
  Result:=SearchClass(Cls,nil,0);
end;

function TPasResolver.BoundAncestorArgClass(T: TPasGenericTemplateType
  ): TPasClassType;
// The class given for the type parameter T of a generic ancestor, in the
// ancestor chain of the innermost class being resolved: in
// `TC<..> = class(TG<X, specialize TItem<Y>>)` the class for TG's second
// parameter is TItem. nil when there is none.
var
  i, k, Depth: Integer;
  Cls: TPasMembersType;
  Anc: TPasType;
  Spec: TPasSpecializeType;
  Arg: TPasType;
begin
  Result:=nil;
  Cls:=nil;
  for i:=ScopeCount-1 downto 0 do
    if Scopes[i] is TPasClassScope then
      begin
      Cls:=TPasMembersType(Scopes[i].Element);
      break;
      end
    else if (Scopes[i] is TPasProcedureScope)
        and (TPasProcedureScope(Scopes[i]).ClassRecScope is TPasClassScope) then
      begin
      Cls:=TPasMembersType(TPasProcedureScope(Scopes[i]).ClassRecScope.Element);
      break;
      end;
  Depth:=0;
  while (Cls is TPasClassType) and (Depth<8) do
    begin
    inc(Depth);
    Anc:=TPasClassType(Cls).AncestorType;
    if not (Anc is TPasSpecializeType) then exit;
    Spec:=TPasSpecializeType(Anc);
    if (Spec.DestType=T.Parent) and (Spec.DestType is TPasGenericType)
        and (TPasGenericType(Spec.DestType).GenericTemplateTypes<>nil) then
      begin
      k:=TPasGenericType(Spec.DestType).GenericTemplateTypes.IndexOf(T);
      if (k<0) or (k>=Spec.Params.Count) then exit;
      Arg:=TPasType(Spec.Params[k]);
      if Arg is TPasSpecializeType then
        Arg:=TPasSpecializeType(Arg).DestType
      else
        Arg:=ResolveAliasType(Arg);
      if Arg is TPasClassType then
        Result:=TPasClassType(Arg);
      exit;
      end;
    if not (Spec.DestType is TPasMembersType) then exit;
    Cls:=TPasMembersType(Spec.DestType);
    end;
end;

function TPasResolver.FindElementWithoutParams(const AName: String; out
  Data: TPRFindData; ErrorPosEl: TPasElement; NoProcsWithArgs,
  NoGenerics: boolean): TPasElement;
// ErrorPosEl=nil means to use scanner position as error position
var
  Abort: boolean;
  OuterType: TPasType;
begin
  //writeln('TPasResolver.FindIdentifier Name="',AName,'"');
  Result:=Nil;
  Abort:=false;
  Data:=Default(TPRFindData);
  Data.ErrorPosEl:=ErrorPosEl;
  Data.SkipGenerics:=NoGenerics;
  IterateElements(AName,@OnFindFirst_PreferNoParams,@Data,Abort);
  Result:=Data.Found;
  if Result=nil then
    begin
    if (ErrorPosEl=nil) and (LastElement<>nil) then
      begin
      if (LastElement.ClassType=TPasClassOfType)
          and (TPasClassOfType(LastElement).DestType=nil) then
        begin
        // 'class of' of a not yet defined class
        Result:=CreateElement(TUnresolvedPendingRef,AName,LastElement,visDefault,
                              CurrentParser.CurSourcePos);
        exit;
        end
      else if (LastElement.ClassType=TPasPointerType)
          and (TPasPointerType(LastElement).DestType=nil) then
        begin
        // pointer of a not yet defined type
        Result:=CreateElement(TUnresolvedPendingRef,AName,LastElement,visDefault,
                              CurrentParser.CurSourcePos);
        exit;
        end
      end;
    if FPartialSpecsAsGeneric and (ErrorPosEl<>nil) then
      begin
      Result:=FindMemberViaAncestorArgs(AName);
      if Result<>nil then
        begin
        Data.Found:=Result;
        Data.ViaAncestorArg:=true;
        exit;
        end;
      end;
    RaiseIdentifierNotFound(20170216152722,AName,ErrorPosEl);
    end;
  if NoProcsWithArgs and (Result is TPasVariable) then
    begin
    // Only a TYPE reference passes NoProcsWithArgs, and a member that is not a
    // type does not hide a type of the same name: in fcl-db's db.pas
    // `property Currency: Boolean` stands next to `property MaxValue: Currency`.
    OuterType:=FindOuterTypeSkippingElement(AName,Result);
    if OuterType<>nil then
      begin
      Result:=OuterType;
      Data.Found:=nil; // the caller must not re-check the member it stepped over
      end;
    exit;
    end;
  if NoProcsWithArgs and (Result is TPasProcedure)
      and ProcNeedsParams(TPasProcedure(Result).ProcType)
  then
    begin
    // Only a TYPE reference passes NoProcsWithArgs. Real FPC searches for a type
    // symbol there and steps over anything else, so a routine does not hide a
    // type of its own name: in rtl-generics' generics.defaults
    // `class function Integer(const A,B: Integer): Integer` every Integer but
    // the method name itself is the type.
    OuterType:=FindOuterTypeSkippingElement(AName,Result);
    if OuterType<>nil then
      begin
      Result:=OuterType;
      Data.Found:=nil; // the caller must not re-check the routine it stepped over
      exit;
      end;
    // proc needs parameters
    RaiseMsg(20170216152347,nWrongNumberOfParametersForCallTo,
      sWrongNumberOfParametersForCallTo,[GetProcTypeDescription(TPasProcedure(Result).ProcType)],ErrorPosEl);
    end;
end;


function TPasResolver.FindOuterTypeSkippingElement(const AName: String;
  SkipEl: TPasElement): TPasType;
// A type reference that landed on a non-type member (a field whose own type is
// not set yet, or a routine): look for a real type of that name in the scopes
// outside the member's own owner.
var
  j: Integer;
  Scope: TPasScope;
  El: TPasElement;
begin
  Result:=nil;
  if SkipEl=nil then exit;
  for j:=ScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[j];
    if not (Scope is TPasIdentifierScope) then continue;
    if Scope.Element=SkipEl.Parent then continue; // skip the member's own scope
    El:=TPasIdentifierScope(Scope).FindElement(AName);
    if El is TPasType then
      exit(TPasType(El));
    end;
end;

function TPasResolver.FindFirstEl(const AName: String; out Data: TPRFindData;
  ErrorPosEl: TPasElement): TPasElement;
var
  Abort: boolean;
begin
  Abort:=false;
  Data:=Default(TPRFindData);
  Data.ErrorPosEl:=ErrorPosEl;
  IterateElements(AName,@OnFindFirst,@Data,Abort);
  Result:=Data.Found;
end;

procedure TPasResolver.FindLongestUnitName(var El: TPasElement; Expr: TPasExpr);
// Input: El is TPasUsesUnit
// Output: El is either a TPasUsesUnit or the root module
var
  CurUsesUnit: TPasUsesUnit;
  BestEl: TPasElement;
  aName, CurName: String;
  Clause: TPasUsesClause;
  i: Integer;
  Section: TPasSection;
  CurExpr: TPasExpr;
begin
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.FindLongestUnitName El=',GetObjName(El),' Expr=',GetObjName(Expr));
  {$ENDIF}
  if not (El is TPasUsesUnit) then
    RaiseInternalError(20170503000945);
  aName:=GetNameExprValue(Expr);
  if aName='' then
    RaiseNotYetImplemented(20170503110217,Expr);
  CurExpr:=Expr;
  repeat
    CurExpr:=GetNextDottedExpr(CurExpr);
    if CurExpr=nil then break;
    CurName:=GetNameExprValue(CurExpr);
    if CurName='' then
      RaiseNotYetImplemented(20170502164242,CurExpr);
    aName:=aName+'.'+CurName;
  until false;

  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.FindLongestUnitName Dotted="',aName,'"');
  {$ENDIF}
  // search in uses clause
  BestEl:=nil;
  Section:=TPasUsesUnit(El).Parent as TPasSection;
  repeat
    Clause:=Section.UsesClause;
    for i:=0 to length(Clause)-1 do
      begin
      CurUsesUnit:=Clause[i];
      CurName:=CurUsesUnit.Name;
      if IsDottedIdentifierPrefix(CurName,aName)
          and ((BestEl=nil) or (length(CurName)>length(BestEl.Name))) then
        BestEl:=CurUsesUnit; // a better match
      end;
    if Section is TImplementationSection then
      begin
      // search in interface uses clause too
      Section:=(Section.Parent as TPasModule).InterfaceSection;
      end
    else
      break;
  until Section=nil;
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.FindLongestUnitName LongestUnit="',GetObjName(BestEl),'"');
  {$ENDIF}

  // check module name
  CurName:=El.GetModule.Name;
  if IsDottedIdentifierPrefix(CurName,aName)
      and ((BestEl=nil) or (length(CurName)>length(BestEl.Name))) then
    BestEl:=El.GetModule; // a better match
  if BestEl=nil then
    begin
    // no dotted module name fits the expression
    RaiseIdentifierNotFound(20170503140643,GetNameExprValue(Expr),Expr);
    end;
  El:=BestEl;
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.FindLongestUnitName END Best="',GetObjName(El),'"');
  {$ENDIF}
end;

function TPasResolver.FindGenericEl(const AName: string;
  TemplateCount: integer; out Find: TPRFindData; ErrorPosEl: TPasElement
  ): TPasElement;
var
  Data: TPRFindGenericData;
  Abort: boolean;
begin
  Data:=Default(TPRFindGenericData);
  Data.TemplateCount:=TemplateCount;
  Data.Find.ErrorPosEl:=ErrorPosEl;
  Abort:=false;
  IterateElements(AName,@OnFindFirst_GenericEl,@Data,Abort);
  Find:=Data.Find;
  Result:=Find.Found;
  if Result=nil then
    begin
    {$IFDEF VerbosePasResolver}
    WriteScopesShort('TPasResolver.FindGenericType');
    {$ENDIF}
    RaiseMsg(20190801104759,nIdentifierNotFound,sIdentifierNotFound,[AName+GetGenericParamCommas(TemplateCount)],ErrorPosEl);
    end;
  CheckFoundElement(Find,nil);
end;

procedure TPasResolver.IterateElements(const aName: string;
  const OnIterateElement: TIterateScopeElement; Data: Pointer;
  var Abort: boolean);
var
  i: Integer;
  Scope: TPasScope;
begin
  for i:=FScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    Scope.IterateElements(AName,Scope,OnIterateElement,Data,Abort);
    if Abort then
      exit;
    if Scope is TPasSubExprScope then break;
    end;
end;

procedure TPasResolver.IterateGlobalElements(const aName: string;
  const OnIterateElement: TIterateScopeElement; Data: Pointer;
  var Abort: boolean);
var
  i: Integer;
  Scope: TPasScope;
  C: TClass;
begin
  i:=0;
  while i<FScopeCount do
    begin
    Scope:=Scopes[i];
    C:=Scope.ClassType;
    if (C.InheritsFrom(TPasDefaultScope))
        or (C.InheritsFrom(TPasModuleScope))
        or (C.InheritsFrom(TPasSectionScope)) then
      inc(i)
    else
      break;
    end;
  while i>0 do
    begin
    dec(i);
    Scope:=Scopes[i];
    Scope.IterateElements(AName,Scope,OnIterateElement,Data,Abort);
    if Abort then
      exit;
    end;
end;

procedure TPasResolver.CheckFoundElement(
  const FindData: TPRFindData; Ref: TResolvedReference);
// check visibility rules
// Call this method after finding an element by searching the scopes.

  function IsFieldInheritingConst(aRef: TResolvedReference): boolean;
  // returns true if aRef is a TPasVariable that inherits its const from parent.
  // For example
  //  type TRecord = record
  //    a: word; // inherits const
  //    const b: word = 3; // does not inherit const
  //    class var c: word; // does not inherit const
  //  end;
  //  procedure DoIt(const r:TRecord)
  var
    El: TPasElement;
  begin
    El:=aRef.Declaration;
    Result:=(El.ClassType=TPasVariable)
        and (TPasVariable(El).VarModifiers*[vmClass, vmStatic]=[]);
    //writeln('IsFieldInheritingConst ',GetObjName(El),' ',Result,' vmClass=',vmClass in TPasVariable(El).VarModifiers);
  end;

var
  Proc: TPasProcedure;
  StartScope: TPasScope;
  OnlyTypeMembers, IsClassOf: Boolean;
  C: TClass;
  ClassRecScope: TPasClassOrRecordScope;
  i: Integer;
  AbstractProcs: TArrayOfPasProcedure;
  TypeEl: TPasType;
begin
  StartScope:=FindData.StartScope;
  OnlyTypeMembers:=false;
  IsClassOf:=false;
  if StartScope is TPasDotBaseScope then
    begin
    OnlyTypeMembers:=TPasDotBaseScope(StartScope).OnlyTypeMembers;
    if StartScope.ClassType=TPasDotClassScope then
      IsClassOf:=TPasDotClassScope(StartScope).IsClassOf;
    if Ref<>nil then
      begin
      Include(Ref.Flags,rrfDotScope);
      if TPasDotBaseScope(StartScope).ConstParent
          and IsFieldInheritingConst(Ref) then
        Include(Ref.Flags,rrfConstInherited);
      end;
    end
  else if StartScope.ClassType=FScopeClass_WithExpr then
    begin
    OnlyTypeMembers:=wesfOnlyTypeMembers in TPasWithExprScope(StartScope).Flags;
    IsClassOf:=wesfIsClassOf in TPasWithExprScope(StartScope).Flags;
    if Ref<>nil then
      begin
      Include(Ref.Flags,rrfDotScope);
      if (wesfConstParent in TPasWithExprScope(StartScope).Flags)
          and IsFieldInheritingConst(Ref) then
        Include(Ref.Flags,rrfConstInherited);
      end;
    end
  else if StartScope.ClassType=FScopeClass_Proc then
    begin
    Proc:=TPasProcedureScope(StartScope).Element as TPasProcedure;
    //writeln('TPasResolver.CheckFoundElement ',GetObjName(Proc),' ',IsClassMethod(Proc),' ElScope=',GetObjName(FindData.ElScope));
    if (FindData.ElScope<>StartScope) and IsClassMethod(Proc) then
      OnlyTypeMembers:=true;
    end
  else if StartScope.ClassType=TPasGroupScope then
    OnlyTypeMembers:=TPasGroupScope(StartScope).OnlyTypeMembers;

  //writeln('TPasResolver.CheckFoundElOnStartScope StartScope=',StartScope.ClassName,
  //    ' StartIsDot=',StartScope is TPasDotBaseScope,
  //    ' OnlyTypeMembers=',(StartScope is TPasDotBaseScope)
  //       and TPasDotBaseScope(StartScope).OnlyTypeMembers,
  //    ' FindData.Found=',GetObjName(FindData.Found));
  if OnlyTypeMembers then
    begin
    // only class vars/procs allowed

    //writeln('TPasResolver.CheckFoundElOnStartScope ',GetObjName(FindData.Found),' ',(FindData.Found is TPasVariable)
    //    and (vmClass in TPasVariable(FindData.Found).VarModifiers));
    if FindData.Found.ClassType=TPasConstructor then
      // constructor: ok
    else if IsClassMethod(FindData.Found)
    then
      // class proc: ok
    else if (FindData.Found is TPasProcedure)
        and TPasProcedure(FindData.Found).IsStatic then
      // static method (e.g. TP object static): ok
    else if (FindData.Found is TPasVariable)
        and (TPasVariable(FindData.Found).VarModifiers*[vmClass,vmStatic]<>[]) then
      // class var/const/property: ok. `Field: T; static;` is the other spelling
      // of a class var - every other test in this unit checks the PAIR, and this
      // one had only vmClass, so a static field was refused inside a class
      // method (fcl-web's THPackStaticTable.TableLength).
    else if FindData.Found is TPasType then
      // nested type: ok
    else if FindData.Found is TPasEnumValue then
      // e.g. enumtype.enumvalue: ok
    else if IsNameOfArgument(FindData.ErrorPosEl) then
      // e.g. NameOf(TObject.DoIt): only the declared name is needed, no instance
    else if AllowTypeQualifiedInstanceField(FindData.Found,FindData.ErrorPosEl) then
      // e.g. High(TRec.field): ok
    else
      begin
      RaiseMsg(20170216152348,nInstanceMemberXInaccessible,
        sInstanceMemberXInaccessible,[FindData.Found.Name],FindData.ErrorPosEl);
      end;
    end
  else if (proExtClassInstanceNoTypeMembers in Options)
      and (StartScope is TPasDotClassScope)
      and TPasClassType(TPasDotClassScope(StartScope).ClassRecScope.Element).IsExternal then
    begin
    // e.g. ExtClassInstance.Member
    C:=FindData.Found.ClassType;
    if (C=TPasProcedure) or (C=TPasFunction) then
      // ok
    else if (C=TPasConst) then
      // ok
    else if ((C=TPasVariable) or (C=TPasProperty))
        and (not (vmClass in TPasVariable(FindData.Found).VarModifiers)) then
      // ok
    else if IsHelper(FindData.Found.Parent) then
      // ok
    else
      begin
      RaiseMsg(20170331184224,nExternalClassInstanceCannotAccessStaticX,
        sExternalClassInstanceCannotAccessStaticX,
        [GetElementTypeName(FindData.Found)+' '+FindData.Found.Name],
        FindData.ErrorPosEl);
      end;
    end;

  if (FindData.Found is TPasProcedure) then
    begin
    Proc:=TPasProcedure(FindData.Found);
    if Proc.IsVirtual or Proc.IsOverride then
      begin
      if StartScope.ClassType=TPasInheritedScope then
        begin
        // inherited expr  ->  call directly
        if Proc.IsAbstract then
          RaiseMsg(20170216152352,nAbstractMethodsCannotBeCalledDirectly,
            sAbstractMethodsCannotBeCalledDirectly,[],FindData.ErrorPosEl);
        end
      else
        begin
        // call via virtual method table
        if Ref<>nil then
          Ref.Flags:=Ref.Flags+[rrfVMT];
        end;
      end;

    // constructor: NewInstance or normal call
    //  it is a NewInstance iff the scope is a class/record, e.g. TObject.Create
    if (Proc.ClassType=TPasConstructor)
        and (Ref<>nil) then
      begin
      if OnlyTypeMembers then
        Ref.Flags:=Ref.Flags+[rrfNewInstance]-[rrfConstInherited];
      // store the class in Ref.Context
      if Ref.Context<>nil then
        RaiseInternalError(20170131141936);
      Ref.Context:=TResolvedRefCtxConstructor.Create;
      TypeEl:=nil;
      ClassRecScope:=nil;
      C:=StartScope.ClassType;
      if C.InheritsFrom(TPasDotClassOrRecordScope) then
        ClassRecScope:=TPasDotClassOrRecordScope(StartScope).ClassRecScope
      else if C=ScopeClass_WithExpr then
        begin
        ClassRecScope:=TPasWithExprScope(StartScope).ClassRecScope;
        if ClassRecScope=nil then
          TypeEl:=TPasWithExprScope(StartScope).Scope.Element as TPasType;
        end
      else if C=ScopeClass_Procedure then
        ClassRecScope:=TPasProcedureScope(StartScope).ClassRecScope
      else if C=TPasDotHelperScope then
        TypeEl:=NoNil(TPasDotHelperScope(StartScope).Element) as TPasType
      else
        RaiseInternalError(20170131150855,GetObjName(StartScope));
      // A constructor reached through a generic type-parameter dot scope
      // (T.Create) constructs a T, not TObject — so member access on the result
      // resolves via T's constraints, e.g. T.Create.kek where T:...,IIntf (#41356).
      if (C=TPasDotClassScope) and (TPasDotClassScope(StartScope).TemplType<>nil) then
        TypeEl:=TPasDotClassScope(StartScope).TemplType;
      // the scope may have been borrowed from another type (see WrittenType)
      if (TypeEl=nil) and C.InheritsFrom(TPasDotClassOrRecordScope)
          and (TPasDotClassOrRecordScope(StartScope).WrittenType<>nil) then
        TypeEl:=TPasDotClassOrRecordScope(StartScope).WrittenType;
      if TypeEl<>nil then
        TResolvedRefCtxConstructor(Ref.Context).Typ:=TypeEl
      else
        begin
        if ClassRecScope=nil then
          RaiseInternalError(20190123120156,GetObjName(StartScope));
        TypeEl:=ClassRecScope.Element as TPasMembersType;
        if (TypeEl.ClassType=TPasClassType)
            and (TPasClassType(TypeEl).HelperForType<>nil) then
          TypeEl:=ResolveAliasType(TPasClassType(TypeEl).HelperForType) as TPasType;
        TResolvedRefCtxConstructor(Ref.Context).Typ:=TypeEl;
        if OnlyTypeMembers and (ClassRecScope is TPasClassScope) then
          begin
          if (TypeEl.ClassType=TPasClassType) and TPasClassType(TypeEl).IsAbstract then
            LogMsg(20190224153450,mtWarning,nCreatingAnInstanceOfAbstractClassY,
              sCreatingAnInstanceOfAbstractClassY,[TypeEl.Name],FindData.ErrorPosEl)
          else
            begin
            AbstractProcs:=TPasClassScope(ClassRecScope).AbstractProcs;
            if (length(AbstractProcs)>0) then
              begin
              if IsClassOf then
                // aClass.Create: do not warn
              else
                for i:=0 to length(AbstractProcs)-1 do
                  LogMsg(20171227110746,mtWarning,nConstructingClassXWithAbstractMethodY,
                    sConstructingClassXWithAbstractMethodY,
                    [TypeEl.Name,AbstractProcs[i].Name],FindData.ErrorPosEl);
              end;
            end;
          end;
        end;
      end;
    {$IFDEF VerbosePasResolver}
    {AllowWriteln}
    if (Proc.ClassType=TPasConstructor) then
      begin
      write('TPasResolver.CheckFoundElement ',GetObjName(Proc));
      if Ref=nil then
        write(' no ref!')
      else
        begin
        write(' rrfNewInstance=',rrfNewInstance in Ref.Flags,
          ' StartScope=',GetObjName(StartScope),
          ' OnlyTypeMembers=',OnlyTypeMembers);
        end;
      writeln;
      end;
    {AllowWriteln-}
    {$ENDIF}

    // destructor: FreeInstance or normal call
    // it is a normal call if 'inherited'
    if (Proc.ClassType=TPasDestructor) and (Ref<>nil) then
      if not (StartScope is TPasInheritedScope) then
        Ref.Flags:=Ref.Flags+[rrfFreeInstance];
    {$IFDEF VerbosePasResolver}
    {AllowWriteln}
    if (Proc.ClassType=TPasDestructor) then
      begin
      write('TPasResolver.CheckFoundElement ',GetObjName(Proc));
      if Ref=nil then
        write(' no ref!')
      else
        begin
        write(' rrfFreeInstance=',rrfFreeInstance in Ref.Flags,
          ' StartScope=',GetObjName(StartScope));
        if StartScope is TPasDotClassOrRecordScope then
          write(' InheritedExpr=',StartScope is TPasInheritedScope);
        end;
      writeln;
      end;
    {AllowWriteln-}
    {$ENDIF}
    end;

  CheckFoundElementVisibility(FindData,Ref);
end;

function TPasResolver.StrictProtectedQualifierOk(const FindData: TPRFindData;
  Context: TPasType): boolean;
// For `obj.Member`, whether obj's type is reachable from the accessing class.
// Only a qualified access has a type to judge; a bare `Member` inside the class
// is always its own instance, and there is nothing to check.
var
  Qualifier: TPasType;
  DotScope: TPasDotClassOrRecordScope;
begin
  Result:=true;
  if not (FindData.StartScope is TPasDotClassOrRecordScope) then exit;
  DotScope:=TPasDotClassOrRecordScope(FindData.StartScope);
  if DotScope.ClassRecScope=nil then exit;
  if not (DotScope.ClassRecScope.Element is TPasType) then exit;
  Qualifier:=TPasType(DotScope.ClassRecScope.Element);
  // The qualifier must BE the accessing class or descend from it. A class the
  // accessing class descends from is fine too - that is plain inherited access.
  Result:=(CheckClassIsClass(Qualifier,Context)<>cIncompatible)
       or (CheckClassIsClass(Context,Qualifier)<>cIncompatible);
end;

procedure TPasResolver.CheckFoundElementVisibility(const FindData: TPRFindData;
  Ref: TResolvedReference);
var
  Context: TPasElement;
  FoundContext: TPasMembersType;
  FoundVisibility: TPasMemberVisibility;
  CompItem: TPasCompositionIdentifier;
  CurScope: TPasScope;
  AncProp: TPasProperty;
  {$IFDEF VerbosePasResolver}
  i: Integer;
  {$ENDIF}

  function IsHelperContextForClass(aContext: TPasElement;
    aFound: TPasMembersType): boolean;
  // True when aContext is a class/record helper whose extended type is aFound or
  // a descendant of it. A helper may access (strict) protected members of the
  // type it extends, even across units.
  var
    lHelperFor: TPasType;
  begin
    Result:=false;
    if not (aContext is TPasClassType) then exit;
    if not (TPasClassType(aContext).ObjKind in okAllHelpers) then exit;
    lHelperFor:=TPasClassType(aContext).HelperForType;
    if lHelperFor=nil then exit;
    lHelperFor:=ResolveAliasType(lHelperFor);
    if not (lHelperFor is TPasMembersType) then exit;
    Result:=CheckClassIsClass(lHelperFor,aFound)<>cIncompatible;
  end;

  function IsTypeQualifiedMethodAddress: boolean;
  // `@TClass.Method`: the address of a method named through its class TYPE
  var
    NameEl: TPasElement;
    LeftRes: TPasResolverResult;
  begin
    Result:=false;
    if not (FindData.Found is TPasProcedure) then exit;
    NameEl:=FindData.ErrorPosEl;
    if not ((NameEl<>nil) and (NameEl.Parent is TBinaryExpr)
        and (TBinaryExpr(NameEl.Parent).OpCode=eopSubIdent)
        and (TBinaryExpr(NameEl.Parent).right=NameEl)
        and (NameEl.Parent.Parent is TUnaryExpr)
        and (TUnaryExpr(NameEl.Parent.Parent).OpCode=eopAddress)) then
      exit;
    ComputeElement(TBinaryExpr(NameEl.Parent).left,LeftRes,[rcNoImplicitProc]);
    Result:=(LeftRes.IdentEl is TPasType) and not (rrfReadable in LeftRes.Flags);
  end;

begin
  // check class visibility; a member found through an ancestor's type argument
  // is checked in the specialization, where the real ancestry is known
  if FindData.ViaAncestorArg then exit;
  // FPC does not check the visibility of a method whose address is taken
  // through its class type
  if AllowTypeQualifiedMethodAddress and IsTypeQualifiedMethodAddress then exit;
  // `property Align;` moved into a protected section re-declares a PUBLIC
  // ancestor property unchanged; fpc and Delphi then use the ancestor's (the
  // LCL's ASide.Align in wspairsplitter.pp, over TPairSplitterSide).
  if (Ref<>nil) and (FindData.Found is TPasProperty)
      and (TPasProperty(FindData.Found).VarType=nil)
      and (FindData.Found.Visibility in [visPrivate,visProtected,visStrictPrivate,visStrictProtected]) then
    begin
    AncProp:=GetPasPropertyAncestor(TPasProperty(FindData.Found),true);
    while (AncProp<>nil)
        and (AncProp.Visibility in [visPrivate,visProtected,visStrictPrivate,visStrictProtected]) do
      AncProp:=GetPasPropertyAncestor(AncProp,true);
    if AncProp<>nil then
      begin
      Ref.Declaration:=AncProp;
      exit;
      end;
    end;
  FoundVisibility:=FindData.Found.Visibility;
  FoundContext:=nil;
  if FindData.ElScope is TPasRecordCompositionScope then
    begin
    // record composition: the composed visibility relative to the composing record
    CompItem:=TPasRecordCompositionScope(FindData.ElScope).FindComposition(FindData.Found);
    if CompItem<>nil then
      begin
      FoundVisibility:=CompItem.Visibility;
      FoundContext:=FindData.ElScope.Element as TPasMembersType;
      end;
    end;
  if FoundVisibility in [visPrivate,visProtected,visStrictPrivate,visStrictProtected] then
    begin
    Context:=GetVisibilityContext;
    if FoundContext=nil then
      FoundContext:=FindData.Found.Parent as TPasMembersType;
    case FoundVisibility of
      visPrivate:
        // private members can only be accessed in same module -- EXCEPT an
        // `inherited Create` chaining to a private ancestor constructor (e.g.
        // TCustomAttribute.Create, which both FPC and pas2llvm declare private).
        // FPC resolves such an inherited call to the nearest ACCESSIBLE
        // constructor up the chain; binding the private ancestor ctor directly
        // is functionally equivalent (it only chains initialization) and lets
        // classes/observer.inc compile.
        if (FoundContext.GetModule<>Context.GetModule)
            and not ((FindData.StartScope is TPasInheritedScope)
                     and (FindData.Found is TPasConstructor))
            // A name resolved INSIDE a specialization body is not a fresh access:
            // the generic was checked in its own module and its type arguments
            // where they were written. rtl-generics specializes TComparer with a
            // type private to a class in the other unit.
            and not (IsInsideSpecialization or FInSpecialize) then
          RaiseMsg(20170216152354,nCantAccessXMember,sCantAccessXMember,
            ['private',FindData.Found.Name],FindData.ErrorPosEl);
      visProtected:
        begin
        // protected members can only be accessed in same module
        // or descendant classes
        CurScope:=TopScope;
        if FoundContext.GetModule=Context.GetModule then
          // same module -> ok
        else if (Context is TPasType)
            and (CheckClassIsClass(TPasType(Context),FoundContext)<>cIncompatible) then
          // context in class or descendant
        else if (CurScope is TPasDotClassOrRecordScope)
            and (TPasDotClassOrRecordScope(CurScope).ClassRecScope.Element.GetModule=Context.GetModule) then
          // e.g. aClassInThisModule.identifier
        else if (CurScope is TPasWithExprScope)
            and (TPasWithExprScope(CurScope).Scope.Element<>nil)
            and (TPasWithExprScope(CurScope).Scope.Element.GetModule=Context.GetModule) then
          // e.g. with aClassInThisModule do identifier
        else if IsHelperContextForClass(Context,FoundContext) then
          // class/record helper may access protected members of the extended type
        else if OverrideRootModule(FindData.Found)=Context.GetModule then
          // an OVERRIDE does not narrow access: the member was declared
          // protected in THIS module, and only its override lives elsewhere
        else if IsInsideSpecialization or FInSpecialize then
          // The MIRROR of the visPrivate rule above: a name resolved INSIDE a
          // specialization body is not a fresh access. The generic was checked
          // in its own module and its type arguments where they were written -
          // system.threading specializes generics.defaults' TComparerService
          // with TThreadPool.TBaseWorkerThread, protected in ITS unit.
        else
          RaiseMsg(20170216152356,nCantAccessXMember,sCantAccessXMember,
            ['protected',FindData.Found.Name],FindData.ErrorPosEl);
        end;
      visStrictPrivate:
        // strict private members can only be accessed in their class, which
        // includes the types nested in it
        if (Context<>FoundContext)
            and not ((Context<>nil) and Context.HasParent(FoundContext)) then
          begin
          {$IFDEF VerbosePasResolver}
          {AllowWriteln}
          writeln('TPasResolver.CheckFoundElement Context=',GetElementDbgPath(Context),' FoundContext=',GetElementDbgPath(FoundContext));
          for i:=ScopeCount-1 downto 0 do
            writeln('  ',i,' ',Scopes[i].ClassName,' Element=',GetObjName(Scopes[i].Element),' VisibilityContext=',GetObjName(Scopes[i].VisibilityContext));
          {AllowWriteln-}
          {$ENDIF}
          RaiseMsg(20170216152357,nCantAccessXMember,sCantAccessXMember,
            ['strict private',FindData.Found.Name],FindData.ErrorPosEl);
          end;
      visStrictProtected:
        // strict protected members can only be accessed in their and descendant classes
        if (Context is TPasType)
            and (CheckClassIsClass(TPasType(Context),FoundContext)<>cIncompatible) then
          begin
          // Being a descendant is only half the rule. The INSTANCE it is reached
          // through must be of the accessing class too: a sibling branch of the
          // same ancestor is not "its own", so `f.pmethod` where f is a cousin
          // class is out of reach even from inside a descendant (tsprot).
          if not StrictProtectedQualifierOk(FindData, TPasType(Context)) then
            RaiseMsg(20260808150003,nCantAccessXMember,sCantAccessXMember,
              ['strict protected',FindData.Found.Name],FindData.ErrorPosEl);
          end
        else if IsHelperContextForClass(Context,FoundContext) then
          // class/record helper may access strict protected members of the extended type
        else if IsNestedInClassOrDescendant(Context,FoundContext) then
          // a type nested in the class (or a descendant) is part of it, as for
          // strict private above: fpdebug's public TFpBreakPointMapEnumerationData
          // names the map's strict protected PFpBreakPointMapEntry
        else
          RaiseMsg(20170216152400,nCantAccessXMember,sCantAccessXMember,
            ['strict protected',FindData.Found.Name],FindData.ErrorPosEl);
    end;
    end;
  if Ref=nil then ;
end;

function TPasResolver.PropTypesMatch(A, B: TPasType): boolean;
// The same type, or two set types of the same element type: SynEdit's
// `property WordBreakChars: TSynIdentChars` read by an inherited getter
// returning lazedit's TCharSet, both `set of char`.
var
  SetA, SetB: TPasType;
begin
  if IsSameType(A,B,prraAlias) then exit(true);
  Result:=false;
  if (A=nil) or (B=nil) then exit;
  SetA:=ResolveAliasType(A);
  SetB:=ResolveAliasType(B);
  if not (SetA is TPasSetType) or not (SetB is TPasSetType) then exit;
  if (TPasSetType(SetA).EnumType=nil) or (TPasSetType(SetB).EnumType=nil) then exit;
  Result:=IsSameType(ResolveAliasType(TPasSetType(SetA).EnumType),
    ResolveAliasType(TPasSetType(SetB).EnumType),prraAlias);
end;

function TPasResolver.IsNestedInClassOrDescendant(Context: TPasElement;
  aClass: TPasMembersType): boolean;
var
  El: TPasElement;
begin
  Result:=false;
  if Context=nil then exit;
  El:=Context.Parent;
  while El<>nil do
    begin
    if (El is TPasType)
        and (CheckClassIsClass(TPasType(El),aClass)<>cIncompatible) then
      exit(true);
    El:=El.Parent;
    end;
end;

function TPasResolver.GetVisibilityContext: TPasElement;
var
  i: Integer;
begin
  for i:=ScopeCount-1 downto 0 do
    begin
    Result:=Scopes[i].VisibilityContext;
    if Result<>nil then exit;
    end;
  Result:=nil;
end;

procedure TPasResolver.BeginScope(ScopeType: TPasScopeType; El: TPasElement);
begin
  case ScopeType of
  stSpecializeType:
    begin
    // `A<B>.C`: the parser has read A and B and is about to read C, so build the
    // specialization NOW - the scope pushed below has to be A<B>'s, and without
    // it C binds to a member of the GENERIC A.
    if (El.CustomData=nil) and (El is TPasSpecializeType)
        and (TPasSpecializeType(El).DestType<>nil)
        and (TPasSpecializeType(El).Params.Count>0) then
      FinishSpecializeType(TPasSpecializeType(El));
    PushParserSpecializeType(El as TPasSpecializeType);
    end;
  stWithExpr: PushWithExprScope(El as TPasExpr);
  else
    RaiseMsg(20181210163324,nNotYetImplemented,sNotYetImplemented+' BeginScope',[IntToStr(ord(ScopeType))],nil);
  end;
end;

procedure TPasResolver.FinishScope(ScopeType: TPasScopeType; El: TPasElement);
begin
  if IsElementSkipped(El) then exit;
  case ScopeType of
  stModule: FinishModule(El as TPasModule);
  stUsesClause: FinishUsesClause;
  stTypeSection: FinishTypeSection(El);
  stTypeDef: FinishTypeDef(El as TPasType);
  stResourceString: FinishResourcestring(El as TPasResString);
  stProcedure: FinishProcedure(El as TPasProcedure);
  stProcedureHeader: FinishProcedureType(El as TPasProcedureType);
  stSpecializeType: FinishParserSpecializeType(El as TPasSpecializeType);
  stWithExpr: FinishWithDo(El as TPasImplWithDo);
  stExceptOnExpr: FinishExceptOnExpr;
  stExceptOnStatement: FinishExceptOnStatement;
  stForLoopHeader: FinishForLoopHeader(El as TPasImplForLoop);
  stDeclaration: FinishDeclaration(El);
  stAncestors: FinishAncestors(El as TPasClassType);
  stInitialFinalization: FinishInitialFinalization(El as TPasImplBlock);
  else
    RaiseMsg(20170216152401,nNotYetImplemented,sNotYetImplemented+' FinishScope',[IntToStr(ord(ScopeType))],nil);
  end;
end;

procedure TPasResolver.FinishTypeAlias(var NewType: TPasType);
var
  TypeEl, DestType: TPasType;
  AncestorClass, aClass: TPasClassType;
  Scope: TPasIdentifierScope;
  OldType: TPasTypeAliasType;
  LocalScope: TPasScope;
begin
  DestType:=TPasTypeAliasType(NewType).DestType;
  TypeEl:=ResolveSimpleAliasType(DestType);
  if TypeEl is TPasClassType then
    begin
    // change "=type aClassType" to "=class(aClassType)"
    // or change "=type aInterfaceType" to "=interface(aInterfaceType)"
    AncestorClass := TPasClassType(TypeEl);

    // remove aliastype from scope
    LocalScope:=GetLocalScope;
    Scope:=LocalScope as TPasIdentifierScope;
    Scope.RemoveLocalIdentifier(NewType);

    // create class or interface
    aClass := TPasClassType(CreateElement(TPasClassType,
                              NewType.Name,NewType.Parent,NewType.Visibility,
                              NewType.SourceFilename,NewType.SourceLinenumber));
    aClass.ObjKind := AncestorClass.ObjKind;
    aClass.RTTIVisibility:=AncestorClass.RTTIVisibility;

    // release old alias type
    OldType := TPasTypeAliasType(NewType);
    NewType := aClass;
    TPasTypeAliasType(OldType).DestType:=nil; // clear reference

    // set ancestor
    aClass.AncestorType := DestType;
    FinishScope(stAncestors,aClass);
    end;
end;

function TPasResolver.IsUnitIntfFinished(AModule: TPasModule): boolean;
var
  CurIntf: TInterfaceSection;
begin
  CurIntf:=AModule.InterfaceSection;
  Result:=(CurIntf<>nil)
      and (CurIntf.CustomData is TPasSectionScope)
      and TPasSectionScope(CurIntf.CustomData).Finished;
end;

procedure TPasResolver.NotifyPendingUsedInterfaces;
// called after unit interface is ready to be used by other modules
var
  ModuleScope: TPasModuleScope;
  i: Integer;
  PendingResolver: TPasResolver;
  PendingSection: TPasSection;
begin
  // call all PendingResolvers
  // Note that a waiting resolver might continue parsing
  ModuleScope:=RootElement.CustomData as TPasModuleScope;
  i:=ModuleScope.PendingResolvers.Count-1;
  while i>=0 do
    begin
    PendingResolver:=TObject(ModuleScope.PendingResolvers[i]) as TPasResolver;
    PendingSection:=PendingResolver.GetLastSection;
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.NotifyPendingUsedInterfaces "',ModuleScope.Element.Name,'" Pending="',PendingResolver.RootElement.Name,'"');
    {$ENDIF}
    if PendingSection=nil then
      RaiseInternalError(20180305141421);
    PendingResolver.CheckPendingUsedInterface(PendingSection); // beware: this might alter the ModuleScope.PendingResolvers
    dec(i);
    if i>=ModuleScope.PendingResolvers.Count then
      i:=ModuleScope.PendingResolvers.Count-1;
    end;
end;

function TPasResolver.GetPendingUsedInterface(Section: TPasSection
  ): TPasUsesUnit;
var
  i: Integer;
  UseUnit: TPasUsesUnit;
begin
  Result:=nil;
  for i:=0 to length(Section.UsesClause)-1 do
    begin
    UseUnit:=Section.UsesClause[i];
    if not (UseUnit.Module is TPasModule) then continue;
    if not IsUnitIntfFinished(TPasModule(UseUnit.Module)) then
      exit(UseUnit);
    end;
end;

function TPasResolver.CheckPendingUsedInterface(Section: TPasSection): boolean;
var
  PendingModule: TPasModule;
  PendingModuleScope: TPasModuleScope;
  List: TFPList;
  WasPending: Boolean;
begin
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.CheckPendingUsedInterface START "',RootElement.Name,'" Section.PendingUsedIntf=',Section.PendingUsedIntf<>nil);
  {$ENDIF}
  WasPending:=Section.PendingUsedIntf<>nil;
  if WasPending then
    begin
    PendingModule:=Section.PendingUsedIntf.Module as TPasModule;
    if not IsUnitIntfFinished(PendingModule) then
      exit; // still pending
    // other unit interface is finished
    {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
    writeln('TPasResolver.CheckPendingUsedInterface "',RootElement.Name,'" UnitIntf finished of "',PendingModule.Name,'"');
    {$ENDIF}
    PendingModuleScope:=NoNil(PendingModule.CustomData) as TPasModuleScope;
    PendingModuleScope.PendingResolvers.Remove(Self);
    Section.PendingUsedIntf:=nil;
    end;

  Section.PendingUsedIntf:=GetPendingUsedInterface(Section);
  //writeln('TPasResolver.CheckPendingUsedInterface ',GetObjName(RootElement),' Section=',GetObjName(Section),' PendingUsedIntf=',GetObjName(Section.PendingUsedIntf));
  if Section.PendingUsedIntf<>nil then
    begin
    // module not yet finished due to pending used interfaces
    PendingModule:=Section.PendingUsedIntf.Module as TPasModule;
    PendingModuleScope:=NoNil(PendingModule.CustomData) as TPasModuleScope;
    {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
    writeln('TPasResolver.CheckPendingUsedInterface "',RootElement.Name,'" waiting for unit intf of "',PendingModule.Name,'"');
    {$ENDIF}
    List:=PendingModuleScope.PendingResolvers;
    if List.IndexOf(Self)<0 then
      List.Add(Self);
    Result:=not WasPending;
    end
  else
    begin
    {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
    {AllowWriteln}
    if WasPending then
      writeln('TPasResolver.CheckPendingUsedInterface "',RootElement.Name,'" uses section complete: ',Section.ClassName);
    {AllowWriteln-}
    {$ENDIF}
    Result:=WasPending;
    if Result then
      UsedInterfacesFinished(Section);
    end;
end;

procedure TPasResolver.UsedInterfacesFinished(Section: TPasSection);
// if there is a unit cycle that stopped parsing this unit
// this method is called after the needed used unit interfaces have finished
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.UsesSectionFinished ',Section.ElementTypeName,' "',RootElement.Name,'"...');
  {$ENDIF}
  CurrentParser.ParseContinue;
  if Section=nil then ;
end;

function TPasResolver.NeedArrayValues(El: TPasElement): boolean;
// called by the parser when reading DoParseConstValueExpression
var
  C: TClass;
  V: TPasVariable;
  TypeEl: TPasType;
begin
  Result:=false;
  if El=nil then exit;
  C:=El.ClassType;
  if (C=TPasConst) or (C=TPasVariable) then
    begin
    V:=TPasVariable(El);
    if V.VarType=nil then exit;
    TypeEl:=ResolveAliasType(V.VarType);
    Result:=TypeEl.ClassType=TPasArrayType;
    end;
  //writeln('TPasResolver.NeedArrayValues ',GetObjName(El));
end;

function TPasResolver.GetDefaultClassVisibility(AClass: TPasClassType
  ): TPasMemberVisibility;
var
  ClassScope: TPasClassScope;
begin
  if AClass.CustomData=nil then
    exit(visDefault);
  ClassScope:=(AClass.CustomData as TPasClassScope);
  if pcsfPublished in ClassScope.Flags then
    Result:=visPublished
  else
    Result:=visPublic;
end;

procedure TPasResolver.ModeChanged(Sender: TObject; NewMode: TModeSwitch;
  Before: boolean; var Handled: boolean);
begin
  inherited ModeChanged(Sender, NewMode, Before, Handled);
  if not Before then
    begin
    if LastElement is TPasSection then
      TPasSectionScope(LastElement.CustomData).ModeSwitches:=CurrentParser.CurrentModeswitches;
    end;
end;

class procedure TPasResolver.UnmangleSourceLineNumber(LineNumber: integer; out
  Line, Column: integer);
begin
  Line:=Linenumber;
  Column:=0;
  if Line<0 then begin
    Line:=-Line;
    Column:=Line mod ParserMaxEmbeddedColumn;
    Line:=Line div ParserMaxEmbeddedColumn;
  end;
end;

class function TPasResolver.GetDbgSourcePosStr(El: TPasElement): string;
var
  Line, Column: integer;
begin
  if El=nil then exit('nil');
  UnmangleSourceLineNumber(El.SourceLinenumber,Line,Column);
  Result:=El.SourceFilename+'('+IntToStr(Line);
  if Column>0 then
    Result:=Result+','+IntToStr(Column);
  Result:=Result+')';
end;

function TPasResolver.GetElementSourcePosStr(El: TPasElement): string;
var
  Line, Column: integer;
begin
  if El=nil then exit('nil');
  UnmangleSourceLineNumber(El.SourceLinenumber,Line,Column);
  if (Line=0) then
    begin
    if El is TPasUnresolvedSymbolRef then
      exit('intrinsic');
    end;
  Result:=CurrentParser.Scanner.FormatPath(El.SourceFilename)+'('+IntToStr(Line);
  if Column>0 then
    Result:=Result+','+IntToStr(Column);
  Result:=Result+')';
end;

destructor TPasResolver.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasResolver.Destroy START ',ClassName);
  {$ENDIF}
  Clear;
  FreeAndNil(FRemainingRangeArrays);
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasResolver.Destroy PopScope...');
  {$ENDIF}
  PopScope; // free default scope
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasResolver.Destroy FPendingForwards...');
  {$ENDIF}
  FreeAndNil(FPendingForwardProcs);
  FreeAndNil(FBuildingSpecializations);
  FreeAndNil(FPendingSpecImpls);
  FreeAndNil(FSpecItemsByEl);
  FreeAndNil(fExprEvaluator);
  ClearBuiltInIdentifiers;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasResolver.Destroy END ',ClassName);
  {$ENDIF}
end;

procedure TPasResolver.Clear;
begin
  ClearHelperList(FActiveHelpers);
  RestoreStashedScopes(0);
  // clear stack, keep DefaultScope
  while (FScopeCount>0) and (FTopScope<>DefaultScope) do
    PopScope;
  ClearResolveDataList(lkModule);
end;

procedure TPasResolver.ClearBuiltInIdentifiers;
var
  bt: TResolverBaseType;
  bp: TResolverBuiltInProc;
begin
  ClearResolveDataList(lkBuiltIn);
  for bt in TResolverBaseType do
    FBaseTypes[bt]:=nil;
  for bp in TResolverBuiltInProc do
    FBuiltInProcs[bp]:=nil;
end;

procedure TPasResolver.AddObjFPCBuiltInIdentifiers(
  const TheBaseTypes: TResolveBaseTypes;
  const TheBaseProcs: TResolverBuiltInProcs);
var
  bt: TResolverBaseType;
begin
  for bt in TheBaseTypes do
    AddBaseType(BaseTypeNames[bt],bt);
  if bfLength in TheBaseProcs then
    AddBuiltInProc('Length','function Length(const String or Array): sizeint',
        @BI_Length_OnGetCallCompatibility,@BI_Length_OnGetCallResult,
        @BI_Length_OnEval,nil,bfLength);
  if bfSetLength in TheBaseProcs then
    AddBuiltInProc('SetLength','procedure SetLength(var String or Array; NewLength: sizeint)',
        @BI_SetLength_OnGetCallCompatibility,nil,nil,
        @BI_SetLength_OnFinishParamsExpr,bfSetLength,[bipfCanBeStatement]);
  if bfInclude in TheBaseProcs then
    AddBuiltInProc('Include','procedure Include(var Set of Enum; const Enum)',
        @BI_InExclude_OnGetCallCompatibility,nil,nil,
        @BI_InExclude_OnFinishParamsExpr,bfInclude,[bipfCanBeStatement]);
  if bfExclude in TheBaseProcs then
    AddBuiltInProc('Exclude','procedure Exclude(var Set of Enum; const Enum)',
        @BI_InExclude_OnGetCallCompatibility,nil,nil,
        @BI_InExclude_OnFinishParamsExpr,bfExclude,[bipfCanBeStatement]);
  if bfBreak in TheBaseProcs then
    AddBuiltInProc('Break','procedure Break',
        @BI_Break_OnGetCallCompatibility,nil,nil,nil,bfBreak,[bipfCanBeStatement]);
  if bfContinue in TheBaseProcs then
    AddBuiltInProc('Continue','procedure Continue',
        @BI_Continue_OnGetCallCompatibility,nil,nil,nil,bfContinue,[bipfCanBeStatement]);
  if bfExit in TheBaseProcs then
    AddBuiltInProc('Exit','procedure Exit(result)',
        @BI_Exit_OnGetCallCompatibility,nil,nil,nil,bfExit,[bipfCanBeStatement]);
  if bfInc in TheBaseProcs then
    AddBuiltInProc('Inc','procedure Inc(var Integer; const Incr: Integer = 1)',
        @BI_IncDec_OnGetCallCompatibility,nil,nil,
        @BI_IncDec_OnFinishParamsExpr,bfInc,[bipfCanBeStatement]);
  if bfDec in TheBaseProcs then
    AddBuiltInProc('Dec','procedure Dec(var Integer; const Decr: Integer = 1)',
        @BI_IncDec_OnGetCallCompatibility,nil,nil,
        @BI_IncDec_OnFinishParamsExpr,bfDec,[bipfCanBeStatement]);
  if bfAssigned in TheBaseProcs then
    AddBuiltInProc('Assigned','function Assigned(const Pointer or Class or Class-of): boolean',
        @BI_Assigned_OnGetCallCompatibility,@BI_Assigned_OnGetCallResult,
        nil,@BI_Assigned_OnFinishParamsExpr,bfAssigned);
  if bfChr in TheBaseProcs then
    AddBuiltInProc('Chr','function Chr(const Integer): char',
        @BI_Chr_OnGetCallCompatibility,@BI_Chr_OnGetCallResult,
        @BI_Chr_OnEval,nil,bfChr);
  if bfOrd in TheBaseProcs then
    AddBuiltInProc('Ord','function Ord(const Enum or char): integer',
        @BI_Ord_OnGetCallCompatibility,@BI_Ord_OnGetCallResult,
        @BI_Ord_OnEval,nil,bfOrd);
  if bfLow in TheBaseProcs then
    AddBuiltInProc('Low','function Low(const array or ordinal): ordinal or integer',
        @BI_LowHigh_OnGetCallCompatibility,@BI_LowHigh_OnGetCallResult,
        @BI_LowHigh_OnEval,nil,bfLow);
  if bfHigh in TheBaseProcs then
    AddBuiltInProc('High','function High(const array or ordinal): ordinal or integer',
        @BI_LowHigh_OnGetCallCompatibility,@BI_LowHigh_OnGetCallResult,
        @BI_LowHigh_OnEval,nil,bfHigh);
  if bfPred in TheBaseProcs then
    AddBuiltInProc('Pred','function Pred(const ordinal): ordinal',
        @BI_PredSucc_OnGetCallCompatibility,@BI_PredSucc_OnGetCallResult,
        @BI_PredSucc_OnEval,nil,bfPred);
  if bfSucc in TheBaseProcs then
    AddBuiltInProc('Succ','function Succ(const ordinal): ordinal',
        @BI_PredSucc_OnGetCallCompatibility,@BI_PredSucc_OnGetCallResult,
        @BI_PredSucc_OnEval,nil,bfSucc);
  if bfStrProc in TheBaseProcs then
    AddBuiltInProc('Str','procedure Str(const var; var String)',
        @BI_StrProc_OnGetCallCompatibility,nil,nil,
        @BI_StrProc_OnFinishParamsExpr,bfStrProc,[bipfCanBeStatement]);
  if bfStrFunc in TheBaseProcs then
    AddBuiltInProc('Str','function Str(const var): String',
        @BI_StrFunc_OnGetCallCompatibility,@BI_StrFunc_OnGetCallResult,
        @BI_StrFunc_OnEval,nil,bfStrFunc);
  if bfWriteStr in TheBaseProcs then
    AddBuiltInProc('WriteStr','procedure WriteStr(out String; params...)',
        @BI_WriteStrProc_OnGetCallCompatibility,nil,nil,
        @BI_WriteStrProc_OnFinishParamsExpr,bfWriteStr,[bipfCanBeStatement]);
  if bfVal in TheBaseProcs then
    AddBuiltInProc('Val','procedure Val(const String; var Value: bool|int|float|enum; out Int)',
        @BI_Val_OnGetCallCompatibility,nil,nil,
        @BI_Val_OnFinishParamsExpr,bfVal,[bipfCanBeStatement]);
  if bfLo in TheBaseProcs then
    AddBuiltInProc('Lo','function Lo(X: any integer type): Byte|Word)',
      @BI_LoHi_OnGetCallCompatibility,@BI_LoHi_OnGetCallResult,
      @BI_LoHi_OnEval,nil,bfLo);
  if bfHi in TheBaseProcs then
    AddBuiltInProc('Hi','function Hi(X: any integer type): Byte|Word)',
      @BI_LoHi_OnGetCallCompatibility,@BI_LoHi_OnGetCallResult,
      @BI_LoHi_OnEval,nil,bfHi);
  if bfConcatArray in TheBaseProcs then
    AddBuiltInProc('Concat','function Concat(const Array1, Array2, ...): Array',
        @BI_ConcatArray_OnGetCallCompatibility,@BI_ConcatArray_OnGetCallResult,
        nil,nil,bfConcatArray);
  if bfConcatString in TheBaseProcs then
    AddBuiltInProc('Concat','function Concat(const String1, String2, ...): String',
        @BI_ConcatString_OnGetCallCompatibility,@BI_ConcatString_OnGetCallResult,
        @BI_ConcatString_OnEval,nil,bfConcatString);
  if bfCopyArray in TheBaseProcs then
    AddBuiltInProc('Copy','function Copy(const Array; Start: integer = 0; Count: integer = all): Array',
        @BI_CopyArray_OnGetCallCompatibility,@BI_CopyArray_OnGetCallResult,
        nil,nil,bfCopyArray);
  if bfInsertArray in TheBaseProcs then
    AddBuiltInProc('Insert','procedure Insert(const Element; var Array; Index: integer)',
        @BI_InsertArray_OnGetCallCompatibility,nil,nil,
        @BI_InsertArray_OnFinishParamsExpr,bfInsertArray,[bipfCanBeStatement]);
  if bfDeleteArray in TheBaseProcs then
    AddBuiltInProc('Delete','procedure Delete(var Array; Start, Count: integer)',
        @BI_DeleteArray_OnGetCallCompatibility,nil,nil,
        @BI_DeleteArray_OnFinishParamsExpr,bfDeleteArray,[bipfCanBeStatement]);
  if bfTypeInfo in TheBaseProcs then
    AddBuiltInProc('TypeInfo','function TypeInfo(type or var identifier): Pointer',
        @BI_TypeInfo_OnGetCallCompatibility,@BI_TypeInfo_OnGetCallResult,
        nil,nil,bfTypeInfo);
  if bfGetTypeKind in TheBaseProcs then
    AddBuiltInProc('GetTypeKind','function GetTypeKind(type or var identifier): System.TTypeKind',
        @BI_GetTypeKind_OnGetCallCompatibility,@BI_GetTypeKind_OnGetCallResult,
        @BI_GetTypeKind_OnEval,nil,bfGetTypeKind);
  if bfAssert in TheBaseProcs then
    AddBuiltInProc('Assert','procedure Assert(bool[,string])',
        @BI_Assert_OnGetCallCompatibility,nil,nil,
        @BI_Assert_OnFinishParamsExpr,bfAssert,[bipfCanBeStatement]);
  if bfNew in TheBaseProcs then
    AddBuiltInProc('New','procedure New(out ^record)',
        @BI_New_OnGetCallCompatibility,nil,nil,
        @BI_New_OnFinishParamsExpr,bfNew,[bipfCanBeStatement]);
  if bfDispose in TheBaseProcs then
    AddBuiltInProc('Dispose','procedure Dispose(var ^record)',
        @BI_Dispose_OnGetCallCompatibility,nil,nil,
        @BI_Dispose_OnFinishParamsExpr,bfDispose,[bipfCanBeStatement]);
  if bfDefault in TheBaseProcs then
    AddBuiltInProc('Default','function Default(T): T',
        @BI_Default_OnGetCallCompatibility,@BI_Default_OnGetCallResult,
        @BI_Default_OnEval,nil,bfDefault,[]);
  if bfNameOf in TheBaseProcs then
    AddBuiltInProc('NameOf','function NameOf(identifier): String',
        @BI_NameOf_OnGetCallCompatibility,@BI_NameOf_OnGetCallResult,
        @BI_NameOf_OnEval,nil,bfNameOf);
  if bfIsConstValue in TheBaseProcs then
    AddBuiltInProc('IsConstValue','function IsConstValue(Value): Boolean',
        @BI_IsConstValue_OnGetCallCompatibility,@BI_IsConstValue_OnGetCallResult,
        @BI_IsConstValue_OnEval,nil,bfIsConstValue);
end;

function TPasResolver.AddBaseType(const aName: string; Typ: TResolverBaseType
  ): TResElDataBaseType;
var
  El: TPasUnresolvedSymbolRef;
begin
  El:=TPasUnresolvedSymbolRef(CreateOwnedElement(TPasUnresolvedSymbolRef,aName,nil));
  if not (Typ in [btNone,btCustom]) then
    FBaseTypes[Typ]:=El;
  Result:=TResElDataBaseType.Create;
  Result.BaseType:=Typ;
  AddResolveData(El,Result,lkBuiltIn);
  FDefaultScope.AddIdentifier(aName,El,pikBaseType);
end;

function TPasResolver.AddCustomBaseType(const aName: string;
  aClass: TResElDataBaseTypeClass): TPasUnresolvedSymbolRef;
var
  CustomData: TResElDataBaseType;
begin
  Result:=TPasUnresolvedSymbolRef(CreateOwnedElement(TPasUnresolvedSymbolRef,aName,nil));
  CustomData:=aClass.Create;
  CustomData.BaseType:=btCustom;
  AddResolveData(Result,CustomData,lkBuiltIn);
  FDefaultScope.AddIdentifier(aName,Result,pikBaseType);
end;

function TPasResolver.IsBaseType(aType: TPasType; BaseType: TResolverBaseType;
  ResolveAlias: boolean): boolean;
begin
  Result:=false;
  if aType=nil then exit;
  if ResolveAlias then
    aType:=ResolveAliasType(aType);
  if aType.ClassType<>TPasUnresolvedSymbolRef then exit;
  Result:=CompareText(aType.Name,BaseTypeNames[BaseType])=0;
end;

function TPasResolver.AddBuiltInProc(const aName: string; Signature: string;
  const GetCallCompatibility: TOnGetCallCompatibility;
  const GetCallResult: TOnGetCallResult; const EvalConst: TOnEvalBIFunction;
  const FinishParamsExpr: TOnFinishParamsExpr;
  const BuiltIn: TResolverBuiltInProc; const Flags: TBuiltInProcFlags
  ): TResElDataBuiltInProc;
var
  El: TPasUnresolvedSymbolRef;
begin
  El:=TPasUnresolvedSymbolRef(CreateOwnedElement(TPasUnresolvedSymbolRef,aName,nil));
  Result:=TResElDataBuiltInProc.Create;
  Result.Proc:=El;
  Result.Signature:=Signature;
  Result.BuiltIn:=BuiltIn;
  Result.GetCallCompatibility:=GetCallCompatibility;
  Result.GetCallResult:=GetCallResult;
  Result.Eval:=EvalConst;
  Result.FinishParamsExpression:=FinishParamsExpr;
  Result.Flags:=Flags;
  AddResolveData(El,Result,lkBuiltIn);
  FDefaultScope.AddIdentifier(aName,El,pikBuiltInProc);
  if BuiltIn<>bfCustom then
    FBuiltInProcs[BuiltIn]:=Result;
end;

procedure TPasResolver.AddResolveData(El: TPasElement; Data: TResolveData;
  Kind: TResolveDataListKind);
begin
  if Data.Element<>nil then
    RaiseInternalError(20171111162227);
  if El.CustomData<>nil then
    RaiseInternalError(20171111162236);
  Data.Element:=El;
  Data.Owner:=Self;
  Data.Next:=FLastCreatedData[Kind];
  FLastCreatedData[Kind]:=Data;
  El.CustomData:=Data;
end;

function TPasResolver.CreateReference(DeclEl, RefEl: TPasElement;
  Access: TResolvedRefAccess; FindData: PPRFindData): TResolvedReference;

  procedure RaiseAlreadySet;
  {$IFDEF VerbosePasResolver}
  var
    FormerDeclEl: TPasElement;
  {$ENDIF}
  begin
    {$IFDEF VerbosePasResolver}
    writeln('RaiseAlreadySet RefEl=',GetObjName(RefEl),' DeclEl=',GetObjName(DeclEl));
    writeln('  RefEl at ',GetElementSourcePosStr(RefEl));
    writeln('  RefEl.CustomData=',GetObjName(RefEl.CustomData));
    if RefEl.CustomData is TResolvedReference then
      begin
      FormerDeclEl:=TResolvedReference(RefEl.CustomData).Declaration;
      writeln('  TResolvedReference(RefEl.CustomData).Declaration=',GetObjName(FormerDeclEl),
       ' IsSame=',FormerDeclEl=DeclEl);
      end;
    {$ENDIF}
    RaiseInternalError(20160922163554, 'customdata=' + GetObjName(DeclEl));
  end;

begin
  if RefEl.CustomData<>nil then
    RaiseAlreadySet;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CreateReference RefEl=',GetObjName(RefEl),' DeclEl=',GetObjName(DeclEl));
  {$ENDIF}

  Result:=TResolvedReference.Create;
  if FindData<>nil then
    begin
    if FindData^.StartScope.ClassType=ScopeClass_WithExpr then
      Result.WithExprScope:=TPasWithExprScope(FindData^.StartScope);
    end;
  AddResolveData(RefEl,Result,lkModule);
  Result.Declaration:=DeclEl;
  if FindData<>nil then
    SetCompositionPath(Result,FindData^.ElScope);
  if RefEl is TPasExpr then
    SetResolvedRefAccess(TPasExpr(RefEl),Result,Access);
  EmitElementHints(RefEl,DeclEl);
end;

procedure TPasResolver.SetCompositionPath(Ref: TResolvedReference;
  ElScope: TPasScope);
var
  CompItem: TPasCompositionIdentifier;
begin
  if not (ElScope is TPasRecordCompositionScope) then exit;
  CompItem:=TPasRecordCompositionScope(ElScope).FindComposition(Ref.Declaration);
  if CompItem<>nil then
    Ref.CompositionPath:=CompItem.Path;
end;

procedure TPasResolver.WriteScopesShort(Title: string);
var
  i: Integer;
begin
  {AllowWriteln}
  writeln(Title,' ScopeCount=',ScopeCount,' FStashScopeCount=',FStashScopeCount);
  for i:=0 to FScopeCount-1 do
    writeln('  ',i,'/',FScopeCount,' ',GetObjName(FScopes[i]));
  {AllowWriteln-}
end;

function TPasResolver.CreateScope(El: TPasElement; ScopeClass: TPasScopeClass
  ): TPasScope;
begin
  if not ScopeClass.IsStoredInElement then
    RaiseInternalError(20160923121858);
  if El.CustomData<>nil then
    RaiseInternalError(20160923121849);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CreateScope El=',GetObjName(El),' ScopeClass=',ScopeClass.ClassName);
  {$ENDIF}
  Result:=ScopeClass.Create;
  if Result.FreeOnPop then
    begin
    Result.Element:=El;
    El.CustomData:=Result;
    Result.Owner:=Self;
    end
  else
    // add to free list
    AddResolveData(El,Result,lkModule);
end;

function TPasResolver.CreateGroupScope(HiType: TPasType; WithTopHelpers: boolean
  ): TPasGroupScope;
begin
  Result:=TPasGroupScope.Create;
  Result.Element:=HiType;
  GroupScope_AddTypeAndAncestors(Result,HiType,WithTopHelpers);
end;

function TPasResolver.IsActiveHelperVisible(Helper: TPasClassType): boolean;
// True if a (possibly nested) class/record helper type is accessible from the
// current visibility context. Top-level and default/public/published helpers are
// always accessible; a nested helper obeys the visibility it was declared with
// (private/strict private/protected/strict protected) relative to its enclosing
// type (FPC: tchlp18..21).
var
  Owner: TPasType;
  Context: TPasElement;
  ContextModule: TPasModule;
begin
  Result:=true;
  // Only nested helpers (declared inside a class/record) have an enclosing
  // visibility; a top-level helper is always visible.
  if not (Helper.Parent is TPasMembersType) then exit;
  if Helper.Visibility in [visDefault,visPublic,visPublished] then exit;
  Owner:=TPasType(Helper.Parent); // the enclosing type the helper is nested in
  Context:=GetVisibilityContext;
  ContextModule:=nil;
  if Context<>nil then
    ContextModule:=Context.GetModule;
  case Helper.Visibility of
    visPrivate:
      // accessible only within the same module
      Result:=Owner.GetModule=ContextModule;
    visStrictPrivate:
      // accessible only within the enclosing type itself
      Result:=Context=Owner;
    visProtected:
      // same module, or a descendant of the enclosing type
      Result:=(Owner.GetModule=ContextModule)
        or ((Context is TPasType) and (CheckClassIsClass(TPasType(Context),Owner)<>cIncompatible));
    visStrictProtected:
      // only the enclosing type or a descendant
      Result:=(Context is TPasType) and (CheckClassIsClass(TPasType(Context),Owner)<>cIncompatible);
  end;
end;

function TPasResolver.HelperUsesPriority(Helper: TPasClassType): integer;
// How near a helper is to the code being resolved: one declared in the same
// unit is nearest, then the ones of the units it USES, in the order of the uses
// clause - which is the order that decides which helper wins. -1 when it is not
// reachable from here at all.

var
  i, j: Integer;
  Scope: TPasScope;
  Sect: TPasSectionScope;
  HelperMod: TPasModule;
begin
  Result:=-1;
  HelperMod:=Helper.GetModule;
  if HelperMod=nil then
    exit;
  for i:=FScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    if not (Scope is TPasSectionScope) then
      continue;
    Sect:=TPasSectionScope(Scope);
    if (Sect.Element<>nil) and (Sect.Element.GetModule=HelperMod) then
      exit(High(Integer));
    if Sect.UsesScopes=nil then
      continue;
    for j:=0 to Sect.UsesScopes.Count-1 do
      if (TPasScope(Sect.UsesScopes[j]).Element<>nil)
          and (TPasScope(Sect.UsesScopes[j]).Element.GetModule=HelperMod)
          and (j>Result) then
        Result:=j;
    end;
end;

function TPasResolver.MatchHelperForType(HelperForType, HiType: TPasType): boolean;
var
  LoHelperFor: TPasType;
begin
  Result:=IsSameType(HelperForType,HiType,prraNone);
  if Result then exit;
  // A strict alias of a RECORD (`TSVGColor = type TFPColor`) shares its
  // helpers with the record, as in ppcx64: a helper for either applies to both,
  // and the nearest one wins.
  LoHelperFor:=ResolveAliasType(HelperForType);
  Result:=(LoHelperFor is TPasRecordType) and (LoHelperFor=ResolveAliasType(HiType));
end;


procedure TPasResolver.GroupScope_AddTypeAndAncestors(Scope: TPasGroupScope;
  HiType: TPasType; WithTopHelpers: boolean);
var
  IsClass: Boolean;
  BestEntry: TPRHelperEntry;
  LoType: TPasType;
  AncestorScope, HelperScope: TPasClassScope;
  C: TClass;
begin
  HiType:=ResolveAliasType(HiType,false);
  LoType:=ResolveAliasType(HiType);
  IsClass:=LoType.ClassType=TPasClassType;
  if IsClass and (TPasClassType(LoType).HelperForType<>nil) then
    begin
    // start in a helper
    // Under {$modeswitch multihelpers} the extended type may have OTHER active
    // helpers (siblings / from a used unit). Let the first extended-type pass
    // scan FActiveHelpers so their methods are visible from this helper's body
    // (tmshlp8/9). The current helper is re-added harmlessly (TPasGroupScope.Add
    // dedups). Without multihelpers this stays false (single-helper behavior).
    WithTopHelpers:=msMultiHelpers in CurrentParser.CurrentModeswitches;
    // first add helper and its ancestors
    HelperScope:=TPasClassScope(LoType.CustomData);
    while HelperScope<>nil do
      begin
      Scope.Add(HelperScope);
      HelperScope:=HelperScope.AncestorScope;
      end;
    // then add the HelperForType and its ancestors
    HiType:=ResolveAliasType(TPasClassType(HiType).HelperForType,false);
    LoType:=ResolveAliasType(HiType);
    IsClass:=LoType.ClassType=TPasClassType;
    end;
  repeat
    // first add helper(s)
    if WithTopHelpers then
      begin
      GroupScope_AddHelpersFor(Scope,HiType,BestEntry);
      end
    else
      WithTopHelpers:=true;
    // then add scope of LoType
    C:=LoType.ClassType;
    if (C=TPasClassType) or (C=TPasRecordType) then
      Scope.Add(LoType.CustomData as TPasIdentifierScope);
    // then add the members composed via record composition
    if (C=TPasRecordType) and (LoType.CustomData is TPasRecordScope)
        and (TPasRecordScope(LoType.CustomData).CompositionScope<>nil) then
      Scope.Add(TPasRecordScope(LoType.CustomData).CompositionScope);
    // continue with ancestor
    if not IsClass then break;
    AncestorScope:=(LoType.CustomData as TPasClassScope).AncestorScope;
    if AncestorScope=nil then break;
    HiType:=TPasClassType(AncestorScope.Element);
    LoType:=HiType;
  until LoType=nil;
end;

procedure TPasResolver.GroupScope_AddHelpersFor(Scope: TPasGroupScope;
  ForType: TPasType; out BestEntry: TPRHelperEntry);
// Adds the active helpers for ForType: all of them under {$modeswitch
// multihelpers}, else the one nearest in uses order (BestEntry).
var
  i, Prio, BestPrio, MatchCount: Integer;
  Entry: TPRHelperEntry;
  HelperForType: TPasType;
  HelperScope: TPasClassScope;
  HiType: TPasType;
begin
      HiType:=ForType;
      BestEntry:=nil;
      BestPrio:=-2;
      MatchCount:=0;
      for i:=length(FActiveHelpers)-1 downto 0 do
        begin
        Entry:=FActiveHelpers[i];
        HelperForType:=Entry.HelperForType;
        if MatchHelperForType(HelperForType,HiType) then
          begin
          // A nested helper whose visibility does not permit access from here
          // is not in scope (FPC: tchlp18..21).
          if not IsActiveHelperVisible(Entry.Helper) then
            continue;
          if msMultiHelpers in CurrentParser.CurrentModeswitches then
            begin
            // add Helper and its ancestors
            HelperScope:=TPasClassScope(Entry.Helper.CustomData);
            while HelperScope<>nil do
              begin
              Scope.Add(HelperScope);
              HelperScope:=HelperScope.AncestorScope;
              end;
            continue;
            end;
          inc(MatchCount);
          if (MatchCount>1) and (BestPrio=-2) then
            // Only now is it worth asking which one is nearer: with a single
            // candidate the answer cannot change, and working it out means
            // walking the scope stack and every uses clause on it.
            BestPrio:=HelperUsesPriority(BestEntry.Helper);
          // Single-helper mode: the LAST helper for the type wins, but "last"
          // means last in the USES ORDER of the code being resolved, not last
          // in the resolver's global list. Every unit that uses SysUtils
          // registers TCardinalHelper again, so a body of ANOTHER unit
          // re-resolved later - a generic's, when a consumer specializes it -
          // took whichever helper some unrelated unit had registered most
          // recently (`UInt32.GetSignMask` in rtl-generics, which SysUtils'
          // helper for the same type does not have).
          if MatchCount=1 then
            BestEntry:=Entry
          else
            begin
            Prio:=HelperUsesPriority(Entry.Helper);
            if Prio>BestPrio then
              begin
              BestEntry:=Entry;
              BestPrio:=Prio;
              end;
            end;
          end;
        end;
      if BestEntry<>nil then
        begin
        HelperScope:=TPasClassScope(BestEntry.Helper.CustomData);
        while HelperScope<>nil do
          begin
          Scope.Add(HelperScope);
          HelperScope:=HelperScope.AncestorScope;
          end;
        end;
end;

procedure TPasResolver.PopScope;
var
  Scope: TPasScope;
begin
  if FScopeCount=0 then
    RaiseInternalError(20160922163557);
  {$IFDEF VerbosePasResolver}
  {AllowWriteln}
  //writeln('TPasResolver.PopScope ',FScopeCount,' ',FTopScope<>nil,' IsDefault=',FTopScope=FDefaultScope);
  writeln('TPasResolver.PopScope ',FTopScope.ClassName,' IsStoredInElement=',FTopScope.IsStoredInElement,' Element=',GetObjName(FTopScope.Element),' FreeOnPop=',FTopScope.FreeOnPop);
  {AllowWriteln-}
  {$ENDIF}
  dec(FScopeCount);
  if FTopScope.FreeOnPop then
    begin
    Scope:=FScopes[FScopeCount];
    if (Scope.Element<>nil) and (Scope.Element.CustomData=Scope) then
      Scope.Element.CustomData:=nil;
    if Scope=FDefaultScope then
      FDefaultScope:=nil;
    FScopes[FScopeCount]:=nil;
    Scope.Free;
    end;
  if FScopeCount>0 then
    FTopScope:=FScopes[FScopeCount-1]
  else
    FTopScope:=nil;
end;

procedure TPasResolver.PopWithScope(El: TPasImplWithDo);
var
  WithScope: TPasWithScope;
  i: Integer;
begin
  WithScope:=El.CustomData as TPasWithScope;
  for i:=WithScope.ExpressionScopes.Count-1 downto 0 do
    begin
    CheckTopScope(ScopeClass_WithExpr);
    if TopScope<>WithScope.ExpressionScopes[i] then
      RaiseInternalError(20160923102846);
    PopScope;
    end;
  CheckTopScope(TPasWithScope);
  PopScope;
end;

procedure TPasResolver.PopGenericParamScope(El: TPasGenericType);
var
  TemplType: TPasGenericTemplateType;
begin
  if (El.GenericTemplateTypes<>nil) and (El.GenericTemplateTypes.Count>0) then
    begin
    TemplType:=TPasGenericTemplateType(El.GenericTemplateTypes[0]);
    if not (TopScope is TPasGenericParamsScope) then
      RaiseNotYetImplemented(20190831204109,El,GetObjName(TopScope));
    if TopScope.Element<>TemplType then
      RaiseNotYetImplemented(20190831204134,El,GetObjName(TopScope.Element));
    PopScope;
    end
  else
    begin
    // A non-generic (synthetic $FuncRef$) class has no params scope of its own; a
    // TPasGenericParamsScope on top belongs to an ENCLOSING generic and must not be
    // popped here (it is popped by its own owner).
    end;
end;

procedure TPasResolver.PushScope(Scope: TPasScope);
begin
  if Scope=nil then
    RaiseInternalError(20160922163601);
  if length(FScopes)=FScopeCount then
    SetLength(FScopes,FScopeCount*2+10);
  FScopes[FScopeCount]:=Scope;
  inc(FScopeCount);
  FTopScope:=Scope;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.PushScope ScopeCount=',ScopeCount,' ',GetObjName(FTopScope));
  {$ENDIF}
end;

function TPasResolver.PushScope(El: TPasElement; ScopeClass: TPasScopeClass
  ): TPasScope;
begin
  Result:=CreateScope(El,ScopeClass);
  PushScope(Result);
end;

function TPasResolver.PushGroupScope(HiType: TPasType): TPasGroupScope;
begin
  Result:=CreateGroupScope(HiType);
  PushScope(Result);
end;

function TPasResolver.PushModuleDotScope(aModule: TPasModule): TPasModuleDotScope;
begin
  Result:=TPasModuleDotScope.Create;
  Result.Owner:=Self;
  Result.Module:=aModule;
  if aModule is TPasProgram then
    begin // program
    if TPasProgram(aModule).ProgramSection<>nil then
      Result.InterfaceScope:=
        NoNil(TPasProgram(aModule).ProgramSection.CustomData) as TPasSectionScope;
    end
  else if aModule is TPasLibrary then
    begin // library
    if TPasLibrary(aModule).LibrarySection<>nil then
      Result.InterfaceScope:=
        NoNil(TPasLibrary(aModule).LibrarySection.CustomData) as TPasSectionScope;
    end
  else
    begin // unit
    if aModule.InterfaceSection<>nil then
      Result.InterfaceScope:=
        NoNil(aModule.InterfaceSection.CustomData) as TPasSectionScope;
    if (aModule=RootElement)
        and (aModule.ImplementationSection<>nil)
        and (aModule.ImplementationSection.CustomData<>nil)
    then
      Result.ImplementationScope:=NoNil(aModule.ImplementationSection.CustomData) as TPasSectionScope;
    if CompareText(aModule.Name,'system')=0 then
      Result.SystemScope:=DefaultScope;
    end;

  PushScope(Result);
end;

function TPasResolver.GenericOfBuildingSpecialization(El: TPasElement): TPasElement;
// The generic a specialization is being built from, while that is in progress.
var
  i: Integer;
begin
  Result:=nil;
  if FBuildingSpecializations=nil then exit;
  i:=0;
  while i<FBuildingSpecializations.Count-1 do
    begin
    if TPasElement(FBuildingSpecializations[i])=El then
      exit(TPasElement(FBuildingSpecializations[i+1]));
    inc(i,2);
    end;
end;

function TPasResolver.PushClassDotScope(var CurClassType: TPasClassType;
  WithTopHelpers: boolean): TPasDotClassScope;
var
  ClassScope: TPasClassScope;
  Ref: TResolvedReference;
  GenClassEl: TPasElement;
  ScopeClassType: TPasClassType;
  SpecItem: TPRSpecializedItem;
  SpecRes: TPasResolver;
begin
  if CurClassType.IsForward then
    begin
    Ref:=CurClassType.CustomData as TResolvedReference;
    CurClassType:=Ref.Declaration as TPasClassType;
    end;
  ScopeClassType:=CurClassType;
  if ScopeClassType.CustomData=nil then
    begin
    // A specialization whose interface is still being built has no scope yet. Its
    // members have the same names as the generic's, so the LOOKUP runs against
    // the generic - CurClassType itself is left alone, or the caller would take
    // the raw generic for the type of the expression.
    GenClassEl:=GenericOfBuildingSpecialization(ScopeClassType);
    if (GenClassEl is TPasClassType) and (GenClassEl.CustomData<>nil) then
      ScopeClassType:=TPasClassType(GenClassEl);
    end;
  if ScopeClassType.CustomData=nil then
    begin
    // No scope and not in flight here: its interface was never started, which an
    // owed implementation can reach. Finish it now - the element exists, only
    // its members are missing.
    SpecItem:=FindSpecializedItemOfEl(ScopeClassType);
    if SpecItem=nil then
      begin
      SpecRes:=ResolverOfElement(ScopeClassType);
      if (SpecRes<>nil) and (SpecRes<>Self) then
        SpecItem:=SpecRes.FindSpecializedItemOfEl(ScopeClassType);
      end;
    if SpecItem<>nil then
      begin
      if SpecItem.Step=prssNone then
        // Never started - an owed implementation can reach it. Build it now.
        SpecializeGenericIntf(SpecItem)
      else if (SpecItem.GenericEl is TPasClassType)
          and (SpecItem.GenericEl.CustomData<>nil) then
        // Its interface is BEING built, so look up against the generic - the
        // same fallback as above, which only covers a build running in THIS
        // resolver. A specialization of another unit's generic is built by THAT
        // unit's resolver, so FBuildingSpecializations here is empty and the
        // item is the only way to reach the generic.
        ScopeClassType:=TPasClassType(SpecItem.GenericEl);
      end;
    end;
  if ScopeClassType.CustomData=nil then
    RaiseInternalError(20160922163611);
  ClassScope:=NoNil(ScopeClassType.CustomData) as TPasClassScope;
  Result:=TPasDotClassScope.Create;
  Result.Owner:=Self;
  Result.ClassRecScope:=ClassScope;
  if ScopeClassType<>CurClassType then
    Result.WrittenType:=CurClassType;
  Result.GroupScope:=CreateGroupScope(ScopeClassType,WithTopHelpers);
  PushScope(Result);
end;

function TPasResolver.PushRecordDotScope(CurRecordType: TPasRecordType;
  HiType: TPasType): TPasDotClassOrRecordScope;
var
  RecScope: TPasRecordScope;
begin
  RecScope:=NoNil(CurRecordType.CustomData) as TPasRecordScope;
  Result:=TPasDotClassOrRecordScope.Create;
  Result.Owner:=Self;
  Result.ClassRecScope:=RecScope;
  // A strict alias of the record (`TColor = type TBase`) has helpers of its own.
  if HiType=nil then
    HiType:=CurRecordType;
  Result.GroupScope:=CreateGroupScope(HiType);
  PushScope(Result);
end;

function TPasResolver.PushInheritedScope(ClassOrRec: TPasMembersType;
  WithTopHelpers: boolean; AncestorScope: TPasClassScope): TPasInheritedScope;
begin
  Result:=TPasInheritedScope.Create;
  Result.Owner:=Self;
  Result.ClassRecScope:=NoNil(ClassOrRec.CustomData) as TPasClassOrRecordScope;
  Result.AncestorScope:=AncestorScope;
  Result.GroupScope:=CreateGroupScope(ClassOrRec,WithTopHelpers);
  PushScope(Result);
end;

function TPasResolver.PushEnumDotScope(HiType: TPasType;
  EnumLoType: TPasEnumType): TPasDotEnumTypeScope;
begin
  Result:=TPasDotEnumTypeScope.Create;
  Result.Owner:=Self;
  Result.EnumScope:=NoNil(EnumLoType.CustomData) as TPasEnumTypeScope;
  Result.GroupScope:=CreateGroupScope(HiType);
  PushScope(Result);
end;

function TPasResolver.PushHelperDotScope(HiType: TPasType): TPasDotBaseScope;
var
  Group: TPasGroupScope;
begin
  Group:=CreateGroupScope(HiType);
  if Group.Count=0 then
    begin
    Group.Free;
    exit(nil);
    end;
  Result:=TPasDotHelperScope.Create;
  Result.Element:=HiType;
  Result.Owner:=Self;
  Result.GroupScope:=Group;
  PushScope(Result);
end;

function TPasResolver.PushTemplateDotScope(TemplType: TPasGenericTemplateType;
  ErrorEl: TPasElement): TPasDotBaseScope;

  procedure PushConstraintScope(ConEl: TPasElement);
  var
    ConToken: TToken;
    DotClassScope: TPasDotClassScope;
    MemberType: TPasMembersType;
    GenTempl: TPasGenericTemplateType;
    aClass: TPasClassType;
    aConstructor: TPasConstructor;
    i: Integer;
    ResolvedEl: TPasResolverResult;
  begin
    ConToken:=GetGenericConstraintKeyword(ConEl);
    case ConToken of
    tkrecord: ;
    tkclass, tkconstructor:
      begin
      if not FindSystemClassTypeAndConstructor('system','tobject',aClass,aConstructor,ErrorEl) then
        RaiseIdentifierNotFound(20190831002421,'system.TObject.Create()',ErrorEl);
      if Result=nil then
        begin
        DotClassScope:=TPasDotClassScope.Create;
        Result:=DotClassScope;
        PushScope(Result);
        DotClassScope.Owner:=Self;
        DotClassScope.ClassRecScope:=aClass.CustomData as TPasClassScope;
        DotClassScope.TemplType:=TemplType;
        Result.GroupScope:=CreateGroupScope(aClass,false);
        end
      else
        // Another constraint (e.g. an interface type) already pushed a scope;
        // the "class"/"constructor" keyword just means T is also a class, so add
        // TObject's members (Create etc.) to that group scope instead of failing.
        GroupScope_AddTypeAndAncestors(Result.GroupScope,aClass,false);
      end;
    else
      if not (ConEl is TPasType) then
        RaiseNotYetImplemented(20190914070842,TemplType,GetObjName(ConEl));
      ComputeElement(ConEl,ResolvedEl,[rcType]);
      if ResolvedEl.BaseType<>btContext then
        RaiseNotYetImplemented(20190915183241,ConEl);
      if ResolvedEl.IdentEl=nil then
        RaiseNotYetImplemented(20190831214135,ConEl);
      if ResolvedEl.LoTypeEl is TPasGenericTemplateType then
        begin
        GenTempl:=TPasGenericTemplateType(ResolvedEl.LoTypeEl);
        if ConEl.HasParent(GenTempl) then
          RaiseNotYetImplemented(20190831214258,ConEl);
        for i:=0 to length(GenTempl.Constraints)-1 do
          PushConstraintScope(GenTempl.Constraints[i]);
        end
      else if ResolvedEl.LoTypeEl is TPasMembersType then
        begin
        MemberType:=TPasMembersType(ResolvedEl.LoTypeEl);
        if Result=nil then
          begin
          DotClassScope:=TPasDotClassScope.Create;
          Result:=DotClassScope;
          PushScope(Result);
          DotClassScope.Owner:=Self;
          DotClassScope.ClassRecScope:=MemberType.CustomData as TPasClassScope;
          DotClassScope.TemplType:=TemplType;
          Result.GroupScope:=CreateGroupScope(ResolvedEl.HiTypeEl,false);
          end
        else
          GroupScope_AddTypeAndAncestors(Result.GroupScope,MemberType,false);
        end
      else
        RaiseNotYetImplemented(20190831001450, ConEl);
    end;
  end;

var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to length(TemplType.Constraints)-1 do
    PushConstraintScope(TemplType.Constraints[i]);
end;

function TPasResolver.PushDotScope(HiType: TPasType): TPasDotBaseScope;
var
  C: TClass;
  LoType: TPasType;
begin
  LoType:=ResolveAliasType(HiType);
  C:=LoType.ClassType;
  if C=TPasClassType then
    Result:=PushClassDotScope(TPasClassType(LoType))
  else if C=TPasRecordType then
    Result:=PushRecordDotScope(TPasRecordType(LoType),HiType)
  else if C=TPasEnumType then
    Result:=PushEnumDotScope(HiType,TPasEnumType(LoType))
  else if C=TPasGenericTemplateType then
    Result:=PushTemplateDotScope(TPasGenericTemplateType(LoType),HiType)
  else
    Result:=PushHelperDotScope(HiType);
end;

function TPasResolver.PushParserSpecializeType(SpecType: TPasSpecializeType
  ): TPasDotBaseScope;
var
  aType: TPasType;
begin
  // `A<B>.C` must look C up in the SPECIALIZATION A<B>, not in the generic A.
  // Against the generic, C binds to the generic's own member, so the alias
  // `TDictionaryEnumerator = TDictionary<T,TEmptyRecord>.TKeyEnumerator` in
  // rtl-generics' THashSet<T> came out as the RAW TDictionary.TKeyEnumerator.
  // Before the specialization exists there is nothing better than the generic.
  aType:=SpecType.DestType;
  if SpecType.CustomData is TPasSpecializeTypeData then
    aType:=TPasSpecializeTypeData(SpecType.CustomData).SpecializedType;
  Result:=PushDotScope(aType);
end;

function TPasResolver.PushWithExprScope(Expr: TPasExpr): TPasWithExprScope;
var
  WithEl: TPasImplWithDo;
  WithScope: TPasWithScope;
  ExprResolved: TPasResolverResult;
  ErrorEl: TPasExpr;
  LoType, HiType, DestType: TPasType;
  ExprScope: TPasGroupScope;
  ClassEl: TPasClassType;
  WithExprScope: TPasWithExprScope;
  Flags: TPasWithExprScopeFlags;
  ClassRecScope: TPasClassOrRecordScope;
begin
  if not (Expr.Parent is TPasImplWithDo) then
    RaiseInternalError(20181210163412,GetObjName(Expr.Parent));
  WithEl:=TPasImplWithDo(Expr.Parent);
  if not (WithEl.CustomData is TPasWithScope) then
    RaiseInternalError(20181210175526);
  WithScope:=TPasWithScope(WithEl.CustomData);

  ResolveExpr(Expr,rraRead);
  ComputeElement(Expr,ExprResolved,[rcSetReferenceFlags]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.PushWithExprScope ExprResolved=',GetResolverResultDbg(ExprResolved));
  {$ENDIF}
  ErrorEl:=Expr;
  HiType:=ExprResolved.HiTypeEl;
  LoType:=ExprResolved.LoTypeEl;
  // ToDo: use last element in Expr for error position
  if (LoType<>nil) and ((LoType.ClassType=TPasGenericTemplateType)
      or ((LoType.ClassType=TPasRecordType)
        and IsRecordComposingTemplate(TPasRecordType(LoType)))) then
    begin
    // Inside a generic body, "with v" where v has a template type cannot be
    // resolved until specialization (the member set is unknown). Create a
    // deferred, empty with-expr scope and flag it; ResolveImplWithDo skips the
    // body in the template (it is resolved per-specialization).
    ExprScope:=CreateGroupScope(HiType,false);
    WithExprScope:=ScopeClass_WithExpr.Create;
    WithExprScope.WithScope:=WithScope;
    WithExprScope.Index:=WithEl.Expressions.Count;
    WithExprScope.Expr:=Expr;
    WithExprScope.Scope:=ExprScope;
    WithExprScope.ClassRecScope:=nil;
    WithExprScope.Flags:=[wesfDeferredTemplate];
    WithScope.ExpressionScopes.Add(WithExprScope);
    PushScope(WithExprScope);
    Result:=WithExprScope;
    exit;
    end;
  if LoType=nil then
    RaiseMsg(20170216152004,nExprTypeMustBeClassOrRecordTypeGot,sExprTypeMustBeClassOrRecordTypeGot,
      [BaseTypeNames[ExprResolved.BaseType]],ErrorEl);
  if (ExprResolved.BaseType in btAllIntrinsicTypes) then
    // ok
  else if (ExprResolved.BaseType=btContext) then
    // ok
  else
    RaiseMsg(20190210143257,nExprTypeMustBeClassOrRecordTypeGot,sExprTypeMustBeClassOrRecordTypeGot,
      [BaseTypeNames[ExprResolved.BaseType]],ErrorEl);

  if not (rrfReadable in ExprResolved.Flags) then
    CheckUseAsType(ExprResolved.LoTypeEl,20220210140100,Expr);

  Flags:=[];
  ClassRecScope:=nil;
  ExprScope:=nil;
  if LoType.ClassType=TPasClassOfType then
    begin
    // e.g. with ImageClass do FindHandlerFromExtension()
    DestType:=TPasClassOfType(LoType).DestType;
    ClassEl:=ResolveAliasType(DestType) as TPasClassType;
    ExprScope:=CreateGroupScope(DestType);
    ClassRecScope:=TPasClassOrRecordScope(ClassEl.CustomData);
    Include(Flags,wesfOnlyTypeMembers);
    Include(Flags,wesfIsClassOf);
    end
  else if LoType is TPasMembersType then
    ClassRecScope:=TPasClassOrRecordScope(LoType.CustomData);

  if ExprScope=nil then
    begin
    ExprScope:=CreateGroupScope(HiType);
    if ExprScope.Count=0 then
      begin
      ExprScope.Free;
      RaiseMsg(20170216152007,nExprTypeMustBeClassOrRecordTypeGot,sExprTypeMustBeClassOrRecordTypeGot,
        [GetElementTypeName(LoType)],ErrorEl);
      end;
    if ExprResolved.IdentEl is TPasType then
      // e.g. with TPoint do PointInCircle
      Include(Flags,wesfOnlyTypeMembers);
    end;
  WithExprScope:=ScopeClass_WithExpr.Create;
  WithExprScope.WithScope:=WithScope;
  WithExprScope.Index:=WithEl.Expressions.Count;
  WithExprScope.Expr:=Expr;
  WithExprScope.Scope:=ExprScope;
  WithExprScope.ClassRecScope:=ClassRecScope;
  if not (ExprResolved.IdentEl is TPasType) then
    Include(Flags,wesfNeedTmpVar);
  if (not (rrfWritable in ExprResolved.Flags))
      and (ExprResolved.BaseType=btContext)
      and (ExprResolved.LoTypeEl.ClassType=TPasRecordType)
      and not (AllowWriteToWithTempRecord
               and ((ExprResolved.IdentEl=nil)
                    or (ExprResolved.IdentEl is TPasProperty)
                    or (ExprResolved.IdentEl is TPasProcedure)
                    or (ExprResolved.IdentEl is TPasResultElement))) then
    Include(Flags,wesfConstParent);
  WithExprScope.Flags:=Flags;
  WithScope.ExpressionScopes.Add(WithExprScope);
  PushScope(WithExprScope);
  Result:=WithExprScope;
end;

function TPasResolver.StashScopes(NewScopeCnt: integer): integer;
begin
  Result:=FStashScopeCount;
  if NewScopeCnt>ScopeCount then
    RaiseInternalError(20190728125505);
  while ScopeCount>NewScopeCnt do
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.StashScopes moving ',TopScope.ClassName,' ScopeCount=',ScopeCount,' StashScopeCount=',FStashScopeCount);
    {$ENDIF}
    if FStashScopeCount=length(FStashScopes) then
      SetLength(FStashScopes,FStashScopeCount+4);
    FStashScopes[FStashScopeCount]:=TopScope;
    inc(FStashScopeCount);
    dec(FScopeCount);
    FScopes[FScopeCount]:=nil;
    if FScopeCount>0 then
      FTopScope:=FScopes[FScopeCount-1]
    else
      FTopScope:=nil;
    end;
end;

function TPasResolver.StashSubExprScopes: integer;
// move all subexpr scopes from Scopes to StashScopes
var
  NewScopeCnt: Integer;
begin
  NewScopeCnt:=FScopeCount;
  while (NewScopeCnt>0) and (FScopes[NewScopeCnt-1] is TPasSubExprScope) do
    dec(NewScopeCnt);
  Result:=StashScopes(NewScopeCnt);
end;

procedure TPasResolver.RestoreStashedScopes(StashDepth: integer);
// restore sub scopes
begin
  while FStashScopeCount>StashDepth do
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.RestoreStashScopes moving ',FStashScopes[FStashScopeCount-1].ClassName,' ScopeCount=',ScopeCount,' StashScopeCount=',FStashScopeCount);
    {$ENDIF}
    if FScopeCount=length(FScopes) then
      SetLength(FScopes,FScopeCount+4);
    dec(FStashScopeCount);
    FScopes[FScopeCount]:=FStashScopes[FStashScopeCount];
    FTopScope:=FScopes[FScopeCount];
    FStashScopes[FStashScopeCount]:=nil;
    inc(FScopeCount);
    end;
end;

procedure TPasResolver.DeleteScope(Index: integer);
  {$IF defined(fpc) and (FPC_FULLVERSION<30101)}
  procedure Delete(var A: TPasScopeArray; Index, Count: integer); overload;
  var
    i: Integer;
  begin
    if Index<0 then
      raise Exception.Create('20191014232344');
    if Index+Count>length(A) then
      raise Exception.Create('20191014232345');
    for i:=Index+Count to length(A)-1 do
      A[i-Count]:=A[i];
    SetLength(A,length(A)-Count);
  end;
  {$ENDIF}
begin
  Delete(FScopes,Index,1);
  dec(FScopeCount);
end;

procedure TPasResolver.InsertScope(Scope: TPasScope; Index: integer);
  {$IF defined(fpc) and (FPC_FULLVERSION<30101)}
  procedure Insert(Item: TPasScope; var A: TPasScopeArray; Index: integer); overload;
  var
    i: Integer;
  begin
    if Index<0 then
      raise Exception.Create('20191014232355');
    if Index>length(A) then
      raise Exception.Create('20191014232356');
    SetLength(A,length(A)+1);
    for i:=length(A)-1 downto Index+1 do
      A[i]:=A[i-1];
    A[Index]:=Item;
  end;
  {$ENDIF}
begin
  Insert(Scope,FScopes,Index);
  inc(FScopeCount);
end;

function TPasResolver.GetCurrentProcScope(ErrorEl: TPasElement
  ): TPasProcedureScope;
var
  Scope: TPasScope;
  i: Integer;
begin
  i:=ScopeCount;
  repeat
    dec(i);
    if i<0 then
      RaiseMsg(20171006001229,nIllegalExpression,sIllegalExpression,[],ErrorEl);
    Scope:=Scopes[i];
    if Scope is TPasProcedureScope then
      exit(TPasProcedureScope(Scope));
  until false;
  Result:=nil;
end;

function TPasResolver.GetProcScope(El: TPasElement): TPasProcedureScope;
var
  CurEl: TPasElement;
begin
  CurEl:=El;
  while CurEl<>nil do
    begin
    if CurEl is TPasProcedure then
      exit(TPasProcedureScope(CurEl.CustomData));
    CurEl:=CurEl.Parent;
    end;
  Result:=nil;
end;

function TPasResolver.GetCurrentSelfScope(ErrorEl: TPasElement): TPasProcedureScope;
begin
  Result:=GetCurrentProcScope(ErrorEl);
  Result:=Result.GetSelfScope;
end;

function TPasResolver.GetSelfScope(El: TPasElement): TPasProcedureScope;
begin
  Result:=GetProcScope(El);
  if Result<>nil then
    Result:=Result.GetSelfScope;
end;

procedure TPasResolver.AddHelper(Helper: TPasClassType;
  var List: TPRHelperEntryArray);
var
  NewEntry: TPRHelperEntry;
  Added: Integer;
  HelperForType: TPasType;
begin
  HelperForType:=ResolveAliasType(Helper.HelperForType,false);
  NewEntry:=TPRHelperEntry.Create;
  NewEntry.Helper:=Helper;
  NewEntry.HelperForType:=HelperForType;
  Added:=length(List);
  NewEntry.Added:=Added;
  SetLength(List,Added+1);
  List[Added]:=NewEntry;
end;

procedure TPasResolver.AddActiveHelper(Helper: TPasClassType);
begin
  AddHelper(Helper,FActiveHelpers);
end;

class function TPasResolver.MangleSourceLineNumber(Line, Column: integer
  ): integer;
begin
  if (Column<ParserMaxEmbeddedColumn)
      and (Line<ParserMaxEmbeddedRow) then
    Result:=-(Line*ParserMaxEmbeddedColumn+integer(Column))
  else
    Result:=Line;
end;

procedure TPasResolver.SetLastMsg(const id: TMaxPrecInt; MsgType: TMessageType;
  MsgNumber: integer; const Fmt: String;
  Args: array of const;
  PosEl: TPasElement);
var
{$IFDEF VerbosePasResolver}
  s: string;
{$ENDIF}
  Column, Row: integer;
begin
  FLastMsgId := id;
  FLastMsgType := MsgType;
  FLastMsgNumber := MsgNumber;
  FLastMsgPattern := Fmt;
  FLastMsg := SafeFormat(Fmt,Args);
  FLastElement := PosEl;
  if PosEl=nil then
    FLastSourcePos:=CurrentParser.CurSourcePos
  else
    begin
    FLastSourcePos.FileName:=PosEl.SourceFilename;
    UnmangleSourceLineNumber(PosEl.SourceLinenumber,Row,Column);
    if Row>=0 then
      FLastSourcePos.Row:=Row
    else
      FLastSourcePos.Row:=0;
    if Column>=0 then
      FLastSourcePos.Column:=Column
    else
      FLastSourcePos.Column:=0;
    end;
  CreateMsgArgs(FLastMsgArgs,Args);
  {$IFDEF VerbosePasResolver}
  {AllowWriteln}
  write('TPasResolver.SetLastMsg ',id,' ',GetElementSourcePosStr(PosEl),' ');
  s:='';
  str(MsgType,s);
  write(s);
  writeln(': [',MsgNumber,'] ',FLastMsg);
  {AllowWriteln-}
  {$ENDIF}
end;

procedure TPasResolver.RaiseMsg(const Id: TMaxPrecInt; MsgNumber: integer;
  const Fmt: String; Args: array of const;
  ErrorPosEl: TPasElement);
var
  E: EPasResolve;
begin
  SetLastMsg(Id,mtError,MsgNumber,Fmt,Args,ErrorPosEl);
  E:=EPasResolve.Create(FLastMsg);
  E.Id:=Id;
  E.MsgType:=mtError;
  E.MsgNumber:=MsgNumber;
  E.MsgPattern:=Fmt;
  E.PasElement:=ErrorPosEl;
  E.Args:=FLastMsgArgs;
  E.SourcePos:=FLastSourcePos;
  raise E;
end;

procedure TPasResolver.RaiseNotYetImplemented(id: TMaxPrecInt; El: TPasElement;
  Msg: string);
var
  s: String;
begin
  s:=sNotYetImplemented+' ['+IntToStr(id)+']';
  if Msg<>'' then
    s:=s+' "'+Msg+'"';
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.RaiseNotYetImplemented s="',s,'" El=',GetObjName(El));
  {$ENDIF}
  RaiseMsg(id,nNotYetImplemented,s,[GetObjName(El)],El);
end;

procedure TPasResolver.RaiseInternalError(id: TMaxPrecInt; const Msg: string);
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.RaiseInternalError [',id,'] ',Msg);
  {$ENDIF}
  raise Exception.Create('Internal error: ['+IntToStr(id)+'] '+Msg);
end;

procedure TPasResolver.RaiseInvalidScopeForElement(id: TMaxPrecInt; El: TPasElement;
  const Msg: string);
var
  i: Integer;
  s: String;
begin
  s:='['+IntToStr(id)+'] invalid scope for "'+GetObjName(El)+'": ';
  for i:=0 to ScopeCount-1 do
    begin
    if i>0 then s:=s+',';
    s:=s+Scopes[i].ClassName;
    end;
  if Msg<>'' then
    s:=s+': '+Msg;
  RaiseInternalError(id,s);
end;

procedure TPasResolver.RaiseIdentifierNotFound(id: TMaxPrecInt; Identifier: string;
  El: TPasElement);



begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.RaiseIdentifierNotFound START "',Identifier,'" ErrorEl=',GetObjName(El));
  WriteScopes;
  {$ENDIF}
  RaiseMsg(id,nIdentifierNotFound,sIdentifierNotFound,[Identifier],El);
end;

procedure TPasResolver.RaiseXExpectedButYFound(id: TMaxPrecInt; const X, Y: string;
  El: TPasElement);
begin
  RaiseMsg(id,nXExpectedButYFound,sXExpectedButYFound,[X,Y],El);
end;

procedure TPasResolver.RaiseXExpectedButTypeYFound(id: TMaxPrecInt;
  const X: string; Y: TPasType; El: TPasElement);
begin
  RaiseMsg(id,nXExpectedButYFound,sXExpectedButYFound,
    [x,GetTypeDescription(Y)],El);
end;

procedure TPasResolver.RaiseContextXExpectedButYFound(id: TMaxPrecInt; const C, X,
  Y: string; El: TPasElement);
begin
  RaiseMsg(id,nContextExpectedXButFoundY,sContextExpectedXButFoundY,[C,X,Y],El);
end;

procedure TPasResolver.RaiseContextXInvalidY(id: TMaxPrecInt; const X, Y: string;
  El: TPasElement);
begin
  RaiseMsg(id,nContextXInvalidY,sContextXInvalidY,[X,Y],El);
end;

procedure TPasResolver.RaiseConstantExprExp(id: TMaxPrecInt; ErrorEl: TPasElement);
begin
  RaiseMsg(id,nConstantExpressionExpected,sConstantExpressionExpected,[],ErrorEl);
end;

procedure TPasResolver.RaiseVarExpected(id: TMaxPrecInt; ErrorEl: TPasElement;
  IdentEl: TPasElement);
begin
  if IdentEl is TPasProperty then
    RaiseMsg(id,nNoMemberIsProvidedToAccessProperty,
      sNoMemberIsProvidedToAccessProperty,[],ErrorEl)
  else
    RaiseMsg(id,nVariableIdentifierExpected,sVariableIdentifierExpected,[],ErrorEl);
end;

procedure TPasResolver.RaiseRangeCheck(id: TMaxPrecInt; ErrorEl: TPasElement);
begin
  RaiseMsg(id,nRangeCheckError,sRangeCheckError,[],ErrorEl);
end;

procedure TPasResolver.RaiseIncompatibleTypeDesc(id: TMaxPrecInt; MsgNumber: integer;
  const Args: array of const;
  const GotDesc, ExpDesc: String; ErrorEl: TPasElement);

  function GetString(ArgNo: integer): string;
  begin
    if ArgNo>High(Args) then
      exit('invalid param '+IntToStr(ArgNo));
    case Args[ArgNo].VType of
{$IFDEF PAS2JS}
    vtUnicodeString: Result:=Args[ArgNo].VUnicodeString;
{$ELSE}
    vtUnicodeString: Result:={%H-}UnicodeString(Args[ArgNo].VUnicodeString);
    vtAnsiString: Result:=AnsiString(Args[ArgNo].VAnsiString);
{$ENDIF}
    else
      Result:='invalid param '+IntToStr(Ord(Args[ArgNo].VType));
    end;
  end;

begin
  case MsgNumber of
    nIllegalTypeConversionTo:
      RaiseMsg(id,MsgNumber,sIllegalTypeConversionTo,[GotDesc,ExpDesc],ErrorEl);
    nIncompatibleTypesGotExpected:
      RaiseMsg(id,MsgNumber,sIncompatibleTypesGotExpected,[GotDesc,ExpDesc],ErrorEl);
    nIncompatibleTypeArgNo:
      RaiseMsg(id,MsgNumber,sIncompatibleTypeArgNo,[GetString(0),GotDesc,ExpDesc],ErrorEl);
    nIncompatibleTypeArgNoVarParamMustMatchExactly:
      RaiseMsg(id,MsgNumber,sIncompatibleTypeArgNoVarParamMustMatchExactly,
               [GetString(0),GotDesc,ExpDesc],ErrorEl);
    nResultTypeMismatchExpectedButFound:
      RaiseMsg(id,MsgNumber,sResultTypeMismatchExpectedButFound,[GotDesc,ExpDesc],ErrorEl);
    nXExpectedButYFound:
      RaiseMsg(id,MsgNumber,sXExpectedButYFound,[GotDesc,ExpDesc],ErrorEl);
    nOperatorIsNotOverloadedAOpB:
      RaiseMsg(id,MsgNumber,sOperatorIsNotOverloadedAOpB,[GotDesc,GetString(0),ExpDesc],ErrorEl);
    nTypesAreNotRelatedXY:
      RaiseMsg(id,MsgNumber,sTypesAreNotRelatedXY,[GotDesc,ExpDesc],ErrorEl);
  else
    RaiseInternalError(20170329112911);
  end;
end;

procedure TPasResolver.RaiseIncompatibleType(id: TMaxPrecInt; MsgNumber: integer;
  const Args: array of const;
  GotType, ExpType: TPasType; ErrorEl: TPasElement);
var
  GotDesc, ExpDesc: String;
begin
  GetIncompatibleTypeDesc(GotType,ExpType,GotDesc,ExpDesc);
  RaiseIncompatibleTypeDesc(id,MsgNumber,Args,GotDesc,ExpDesc,ErrorEl);
end;

procedure TPasResolver.RaiseIncompatibleTypeRes(id: TMaxPrecInt; MsgNumber: integer;
  const Args: array of const;
  const GotType, ExpType: TPasResolverResult;
  ErrorEl: TPasElement);
var
  GotDesc, ExpDesc: String;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.RaiseIncompatibleTypeRes Got={',GetResolverResultDbg(GotType),'} Expected={',GetResolverResultDbg(ExpType),'}');
  {$ENDIF}
  GetIncompatibleTypeDesc(GotType,ExpType,GotDesc,ExpDesc);
  RaiseIncompatibleTypeDesc(id,MsgNumber,Args,GotDesc,ExpDesc,ErrorEl);
end;

procedure TPasResolver.RaiseHelpersCannotBeUsedAsType(id: TMaxPrecInt;
  ErrorEl: TPasElement);
begin
  RaiseMsg(id,nHelpersCannotBeUsedAsTypes,sHelpersCannotBeUsedAsTypes,[],ErrorEl);
end;

procedure TPasResolver.RaiseInvalidProcTypeModifier(id: TMaxPrecInt;
  ProcType: TPasProcedureType; ptm: TProcTypeModifier; ErrorEl: TPasElement);
begin
  RaiseMsg(id,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(ProcType),
    ProcTypeModifiers[ptm]],ErrorEl);
end;

procedure TPasResolver.RaiseInvalidProcModifier(id: TMaxPrecInt; Proc: TPasProcedure;
  pm: TProcedureModifier; ErrorEl: TPasElement);
begin
  RaiseMsg(id,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),
    ModifierNames[pm]],ErrorEl);
end;

procedure TPasResolver.LogMsg(const id: TMaxPrecInt; MsgType: TMessageType;
  MsgNumber: integer; const Fmt: String;
  Args: array of const;
  PosEl: TPasElement);
var
  Scanner: TPascalScanner;
  State: TWarnMsgState;
  {$IFDEF VerbosePasResolver}
  s: String;
  {$ENDIF}
begin
  Scanner:=CurrentParser.Scanner;
  if (Scanner<>nil) then
    begin
    if MsgType>=mtWarning then
      State:=Scanner.WarnMsgState[MsgNumber]
    else
      State:=wmsDefault;
    // An explicit {$warn N error} escalation must fire even during parsing,
    // BEFORE the parse-time hint/warning suppression below — otherwise a
    // deprecation/etc. hint emitted while parsing (e.g. a deprecated type used
    // as an array element) would be silently dropped instead of erroring
    // (thintdir3a/3b/4a/4b/5).
    if State=wmsError then
      begin
      RaiseMsg(id,MsgNumber,Fmt,Args,PosEl);
      exit;
      end;
    if (FStep<prsFinishingModule)
        and (Scanner.IgnoreMsgType(MsgType)) then
      exit; // during parsing consider directives like $Hints on|off
    if (MsgType>=mtWarning) and (State=wmsOff) then
      begin
      {$IFDEF VerbosePasResolver}
      {AllowWriteln}
      write('TPasResolver.LogMsg ignoring ',id,' ',GetElementSourcePosStr(PosEl),' ');
      s:='';
      str(MsgType,s);
      write(s);
      writeln(': [',MsgNumber,'] ',SafeFormat(Fmt,Args));
      {AllowWriteln-}
      {$ENDIF}
      exit; // ignore
      end;
    end;

  SetLastMsg(id,MsgType,MsgNumber,Fmt,Args,PosEl);
  if Assigned(OnLog) then
    OnLog(Self,FLastMsg)
  else if Assigned(CurrentParser.OnLog) then
    CurrentParser.OnLog(Self,FLastMsg);
end;
