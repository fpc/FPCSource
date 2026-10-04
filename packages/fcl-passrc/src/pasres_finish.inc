{%MainUnit pasresolver.pp}
{ **********************************************************************

    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - Scope finish

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }

procedure TPasResolver.FinishModule(CurModule: TPasModule);
var
  CurModuleClass: TClass;
  i: Integer;
  ModScope: TPasModuleScope;
begin
  // Only what is ready: a unit used from the INTERFACE of another finishes while
  // that one still has waiting entries. The list frees itself once empty.
  BuildForwardWaitingSpecImpls;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishModule START ',CurModule.Name);
  {$ENDIF}
  FStep:=prsFinishingModule;

  CurModuleClass:=CurModule.ClassType;
  ModScope:=CurModule.CustomData as TPasModuleScope;

  if bsRangeChecks in CurrentParser.Scanner.CurrentBoolSwitches then
    begin
    Include(ModScope.Flags,pmsfRangeErrorNeeded);
    FindRangeErrorConstructors(nil);
    end;

  if (CurModuleClass=TPasProgram) then
    begin
    FinishSection(TPasProgram(CurModule).ProgramSection);
    // resolve begin..end block
    ResolveImplBlock(CurModule.InitializationSection);
    end
  else if (CurModuleClass=TPasLibrary) then
    begin
    FinishSection(TPasLibrary(CurModule).LibrarySection);
    // resolve begin..end block
    ResolveImplBlock(CurModule.InitializationSection);
    ResolveImplBlock(CurModule.FinalizationSection);
    end
  else if (CurModuleClass=TPasModule) then
    begin
    // unit
    FinishSection(CurModule.InterfaceSection);
    if CurModule.ImplementationSection<>nil then
      FinishSection(CurModule.ImplementationSection);
    if CurModule.FinalizationSection<>nil then
      // finalization section finished -> resolve
      ResolveImplBlock(CurModule.FinalizationSection);
    if CurModule.InitializationSection<>nil then
      // initialization section finished -> resolve
      ResolveImplBlock(CurModule.InitializationSection);
    end
  else
    RaiseInternalError(20160922163327); // unknown module

  // check all methods have bodies
  // and all forward classes and pointers are resolved.
  // Under interface-only resolution the module's implementation section is not
  // parsed, so an interface routine/method whose body lives there has no
  // ImplProc; skip the check to avoid a spurious "Forward proc not resolved".
  // Inert by default (InterfaceOnly=False): the loop runs exactly as before.
  for i:=0 to FPendingForwardProcs.Count-1 do
    CheckPendingForwardTypes(TPasElement(FPendingForwardProcs[i]));
  if not InterfaceOnly then
    for i:=0 to FPendingForwardProcs.Count-1 do
      CheckPendingForwardProcs(TPasElement(FPendingForwardProcs[i]));
  FPendingForwardProcs.Clear;

  // close all sections
  while (TopScope<>nil) and (TopScope.ClassType=ScopeClass_Section) do
    PopScope;
  CheckTopScope(FScopeClass_Module);
  PopScope;

  FStep:=prsFinishedModule;

  if (CurrentParser<>nil) and (CurrentParser.Scanner<>nil) then
    begin
    CurrentParser.NextToken;
    if CurrentParser.Scanner.CurToken<>tkEOF then
      LogMsg(20180628131456,mtHint,nTextAfterFinalIgnored,sTextAfterFinalIgnored+' '+CurrentParser.Scanner.FormatCurrentSrcPos,
        [],CurModule);
    end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishModule END ',CurModule.Name);
  {$ENDIF}
end;

procedure TPasResolver.FinishUsesClause;
var
  Section: TPasSection;
  i, j: Integer;
  PublicEl, UseModule: TPasElement;
  Scope: TPasSectionScope;
  UsesScope: TPasSectionScope;
  UseUnit: TPasUsesUnit;
  FirstName: String;
  p: SizeInt;
  OldIdentifier: TPasIdentifier;
  IntfHelpers: TPRHelperEntryArray;
begin
  CheckTopScope(ScopeClass_Section);
  Scope:=TPasSectionScope(TopScope);
  Section:=TPasSection(Scope.Element);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishUsesClause Section=',Section.ClassName,' Section.UsesList.Count=',Section.UsesList.Count);
  {$ENDIF}
  if Scope.UsesFinished then
    RaiseInternalError(20180305145220);
  Scope.UsesFinished:=true;

  for i:=0 to Section.UsesList.Count-1 do
    begin
    UseUnit:=Section.UsesClause[i];
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishUsesClause ',GetObjName(UseUnit));
    {$ENDIF}
    UseModule:=UseUnit.Module;

    // check used unit
    PublicEl:=nil;
    if (UseModule.ClassType=TPasLibrary) then
      PublicEl:=TPasLibrary(UseModule).LibrarySection
    else if (UseModule.ClassType=TPasModule) then
      PublicEl:=TPasModule(UseModule).InterfaceSection
    else
      RaiseXExpectedButYFound(20170503004803,'unit',GetElementTypeName(UseModule),UseUnit);
    if PublicEl=nil then
      RaiseInternalError(20160922163352,'uses element has no interface section: '+GetObjName(UseModule));
    if PublicEl.CustomData=nil then
      RaiseInternalError(20160922163358,'uses element has no resolver data: '
        +UseUnit.Name+'->'+GetObjName(PublicEl));
    if not (PublicEl.CustomData is TPasSectionScope) then
      RaiseInternalError(20160922163403,'uses element has invalid resolver data: '
        +UseUnit.Name+'->'+GetObjName(PublicEl)+'->'+PublicEl.CustomData.ClassName);
    UsesScope:=TPasSectionScope(PublicEl.CustomData);

    // add full uses name
    AddIdentifier(Scope,UseUnit.Name,UseUnit,pikSimple);

    // add scope
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishUsesClause Add UsesScope=',GetObjName(UsesScope));
    {$ENDIF}
    Scope.UsesScopes.Add(UsesScope);

    // add helpers
    IntfHelpers:=UsesScope.Helpers;
    for j:=0 to length(IntfHelpers)-1 do
      AddActiveHelper(TPRHelperEntry(IntfHelpers[j]).Helper);

    EmitElementHints(UseUnit,UseModule);
    end;

  // Add first name of dotted unitname (top level subnamespace) as identifier
  for i:=Section.UsesList.Count-1 downto 0 do
    begin
    UseUnit:=Section.UsesClause[i];
    FirstName:=UseUnit.Name;
    p:=Pos('.',FirstName);
    if p<1 then continue;
    FirstName:=LeftStr(FirstName,p-1);
    OldIdentifier:=Scope.FindLocalIdentifier(FirstName);
    if (OldIdentifier=nil) then
      AddIdentifier(Scope,FirstName,UseUnit,pikNamespace);
    end;
  // Note: a sub identifier (e.g. a class member) hides all unitnames starting
  //       with this identifier
end;

procedure TPasResolver.FinishSection(Section: TPasSection);
// Note: can be called multiple times for a section
var
  Scope: TPasSectionScope;
begin
  Scope:=Section.CustomData as TPasSectionScope;
  if Scope.Finished then exit;
  Scope.Finished:=true;
  if Section is TInterfaceSection then
    FinishInterfaceSection(Section);
end;

procedure TPasResolver.FinishInterfaceSection(Section: TPasSection);
begin
  {$IFDEF VerboseUnitQueue}
  writeln('TPasResolver.FinishInterfaceSection ',GetObjName(RootElement));
  {$ENDIF}
  {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
  if not IsUnitIntfFinished(Section.GetModule) then
    RaiseInternalError(20171214004323,'TPasResolver.FinishInterfaceSection "'+RootElement.Name+'" "'+Section.GetModule.Name+'" IsUnitIntfFinished=false');
  {$ENDIF}
  inc(Hub.FinishedInterfaceCount);
  FFinishedInterfaceIndex:=Hub.FinishedInterfaceCount;
  NotifyPendingUsedInterfaces;
  if Section=nil then ;
end;

procedure TPasResolver.FinishTypeSection(El: TPasElement);

  procedure FinishDeclarations(El: TPasDeclarations);
  var
    i: Integer;
    Decl: TPasElement;
  begin
    for i:=0 to El.Declarations.Count-1 do
      begin
      Decl:=TPasElement(El.Declarations[i]);
      if Decl is TPasType then
        FinishTypeSectionEl(TPasType(Decl));
      end;
  end;

  procedure FinishMembersType(El: TPasMembersType);
  var
    i: Integer;
    Decl: TPasElement;
  begin
    for i:=0 to El.Members.Count-1 do
      begin
      Decl:=TPasElement(El.Members[i]);
      if Decl is TPasType then
        FinishTypeSectionEl(TPasType(Decl));
      end;
  end;

begin
  // resolve pending forwards
  if El is TPasDeclarations then
    FinishDeclarations(TPasDeclarations(El))
  else if El is TPasMembersType then
    FinishMembersType(TPasMembersType(El))
  else
    RaiseNotYetImplemented(20181226105933,El);
  BuildForwardWaitingSpecImpls;
end;

procedure TPasResolver.FinishTypeSectionEl(El: TPasType);

  function ReplaceDestType(Decl: TPasType; var DestType: TPasType;
    const DestName: string; MustExist: boolean; ErrorEl: TPasElement): boolean;
  // returns true if replaces
  var
    Abort: boolean;
    Data: TPRFindData;
    FoundTypeEl: TPasType;
  begin
    Abort:=false;
    Data:=Default(TPRFindData);
    Data.ErrorPosEl:=ErrorEl;
    // A `class of` or a forward pointer names a concrete type, never a generic
    // that still needs specialization. rtl-generics declares THashService and
    // THashService<T> side by side and `class of THashService` is the plain one;
    // taking the first match found the generic.
    Data.SkipGenerics:=true;
    (TopScope as TPasIdentifierScope).IterateElements(DestName,
      TopScope,@OnFindFirst_PreferNoParams,@Data,Abort);
    if Data.Found=nil then
      begin
      // only a generic has that name - let the ordinary path report it
      Abort:=false;
      Data:=Default(TPRFindData);
      Data.ErrorPosEl:=ErrorEl;
      (TopScope as TPasIdentifierScope).IterateElements(DestName,
        TopScope,@OnFindFirst,@Data,Abort);
      end;
    //writeln('ReplaceDestType ',GetObjName(El),' DestType=',GetObjName(DestType),' DestType.Parent=',GetObjName(DestType.Parent),' RefCount=',DestType.RefCount);
    if Data.Found=nil then
      if MustExist then
        begin
        RaiseIdentifierNotFound(20170216151543,DestName,ErrorEl);
        end
      else
        exit(false);
    if Data.Found=DestType then exit;
    if Decl is TPasClassOfType then
      begin
      // An ALIAS of a class is a class reference: fcl-db's bufdataset declares
      // `TDataPacketReader = TDataPacketHandler` and implements methods under
      // that name, so judge the type the alias resolves to.
      FoundTypeEl:=nil;
      if Data.Found is TPasType then
        FoundTypeEl:=ResolveAliasType(TPasType(Data.Found));
      if (FoundTypeEl=nil) or (FoundTypeEl.ClassType<>TPasClassType)
          or (TPasClassType(FoundTypeEl).ObjKind<>okClass) then
        RaiseXExpectedButYFound(20170216151548,'class',GetElementTypeName(Data.Found),ErrorEl);
      end;
    // replace unresolved
    DestType:=TPasType(Data.Found);
    CheckUseAsType(DestType,20190123100649,El);
    // check cycles
    if Decl is TPasPointerType then
      CheckPointerCycle(TPasPointerType(Decl));
    Result:=true;
  end;

var
  C: TClass;
  ClassOfEl: TPasClassOfType;
  TypeEl: TPasType;
  UnresolvedEl: TUnresolvedPendingRef;
  OldClassType: TPasClassType;
  PtrType: TPasPointerType;
begin
  C:=El.ClassType;
  if C=TPasClassType then
    begin
    if TPasClassType(El).IsForward
        and not (TPasClassType(El).CustomData is TResolvedReference) then
      // Delphi requires a forward type to be completed inside the SAME type
      // section, so an unresolved one is final here. ObjFPC allows any later
      // type section of the same declaration section to complete it, and the
      // parser calls this once per intervening non-type declaration (fcl-xml
      // xpath.pp declares procedures between the two) — so there the verdict is
      // deferred to CheckPendingForwardProcs.
      if msDelphi in GetElModeSwitches(El) then
        RaiseMsg(20170216151534,nForwardTypeNotResolved,sForwardTypeNotResolved,[El.Name],El);
    end
  else if (C=TPasClassOfType) then
    begin
    ClassOfEl:=TPasClassOfType(El);
    TypeEl:=ResolveAliasType(ClassOfEl.DestType);
    if (TypeEl.ClassType=TUnresolvedPendingRef) then
      begin
      // forward class-of -> resolve now
      UnresolvedEl:=TUnresolvedPendingRef(TypeEl);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.FinishTypeSection resolving "',ClassOfEl.Name,'" = class of unresolved "',TypeEl.Name,'"');
      {$ENDIF}
      ReplaceDestType(ClassOfEl,ClassOfEl.DestType,TypeEl.Name,true,UnresolvedEl);
      end
    else if TypeEl.ClassType=TPasClassType then
      begin
      // class-of has found a type
      // another later in the same type section has priority -> check
      OldClassType:=TypeEl as TPasClassType;
      if OldClassType.Parent=ClassOfEl.Parent then
        exit; // class in same type section -> ok
      // class not in same type section -> check
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.FinishTypeSection improving "',ClassOfEl.Name,'" = class of resolved "',TypeEl.Name,'"');
      {$ENDIF}
      ReplaceDestType(ClassOfEl,ClassOfEl.DestType,ClassOfEl.DestType.Name,false,ClassOfEl);
      end;
    end
  else if C=TPasPointerType then
    begin
    PtrType:=TPasPointerType(El);
    TypeEl:=ResolveAliasType(PtrType.DestType);
    if (TypeEl.ClassType=TUnresolvedPendingRef) then
      begin
      // forward pointer -> resolve now
      UnresolvedEl:=TUnresolvedPendingRef(TypeEl);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.FinishTypeSection resolving "',PtrType.Name,'" = pointer of unresolved "',TypeEl.Name,'"');
      {$ENDIF}
      ReplaceDestType(PtrType,PtrType.DestType,TypeEl.Name,true,UnresolvedEl);
      end
    else
      begin
      // pointer-of has found a type
      // another later in the same type section has priority -> check
      if TypeEl.Parent=PtrType.Parent then
        exit; // class in same type section -> ok
      // dest not in same type section -> check
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.FinishTypeSection improving "',PtrType.Name,'" = pointer of resolved "',TypeEl.Name,'"');
      {$ENDIF}
      ReplaceDestType(PtrType,PtrType.DestType,TypeEl.Name,false,PtrType);
      end;
    end
  else if C=TPasRecordType then
    // an anonymous `^T` field to a type declared further down the section
    // (fpdebug's `u_ar0 : ^user_regs_struct32` inside user64)
    FinishRecordFieldPointers(TPasRecordType(El));
end;

procedure TPasResolver.FinishRecordFieldPointers(Rec: TPasRecordType);

  procedure Check(VarType: TPasType);
  var
    PtrType: TPasPointerType;
    TypeEl: TPasType;
    Data: TPRFindData;
    Abort: boolean;
  begin
    if not (VarType is TPasPointerType) or (VarType.Name<>'') then exit;
    PtrType:=TPasPointerType(VarType);
    if PtrType.DestType=nil then exit;
    TypeEl:=ResolveAliasType(PtrType.DestType);
    if TypeEl.ClassType<>TUnresolvedPendingRef then exit;
    Abort:=false;
    Data:=Default(TPRFindData);
    Data.ErrorPosEl:=PtrType;
    Data.SkipGenerics:=true;
    (TopScope as TPasIdentifierScope).IterateElements(TypeEl.Name,
      TopScope,@OnFindFirst_PreferNoParams,@Data,Abort);
    if Data.Found is TPasType then
      PtrType.DestType:=TPasType(Data.Found);
  end;

var
  i: Integer;
  Member: TPasElement;
begin
  for i:=0 to Rec.Members.Count-1 do
    begin
    Member:=TPasElement(Rec.Members[i]);
    if Member is TPasVariable then
      Check(TPasVariable(Member).VarType);
    end;
  if Rec.Variants<>nil then
    for i:=0 to Rec.Variants.Count-1 do
      if TPasVariant(Rec.Variants[i]).Members is TPasRecordType then
        FinishRecordFieldPointers(TPasRecordType(TPasVariant(Rec.Variants[i]).Members));
end;

procedure TPasResolver.FinishTypeDef(El: TPasType);
var
  C: TClass;
begin
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.FinishTypeDef El=',GetObjName(El));
  {$ENDIF}
  C:=El.ClassType;
  if C=TPasEnumType then
    FinishEnumType(TPasEnumType(El))
  else if C=TPasSetType then
    FinishSetType(TPasSetType(El))
  else if C=TPasRangeType then
    FinishRangeType(TPasRangeType(El))
  else if C=TPasRecordType then
    FinishRecordType(TPasRecordType(El))
  else if C=TPasClassType then
    begin
    FinishClassType(TPasClassType(El));
    if not TPasClassType(El).IsForward then
      BuildForwardWaitingSpecImpls;
    end
  else if C=TPasClassOfType then
    FinishClassOfType(TPasClassOfType(El))
  else if C=TPasPointerType then
    FinishPointerType(TPasPointerType(El))
  else if C=TPasArrayType then
    FinishArrayType(TPasArrayType(El))
  else if (C=TPasAliasType) or (C=TPasTypeAliasType) then
    FinishAliasType(TPasAliasType(El))
  else if C=TPasTypeOfType then
    FinishTypeOfType(TPasTypeOfType(El))
  else if (C=TPasPointerType) then
    EmitTypeHints(El,TPasPointerType(El).DestType)
  else if C=TPasGenericTemplateType then
    FinishGenericTemplateType(TPasGenericTemplateType(El))
  else if C=TPasSpecializeType then
    FinishSpecializeType(TPasSpecializeType(El));
end;

procedure TPasResolver.FinishEnumType(El: TPasEnumType);
// An explicit member value (`aLast = aTwo`, `cFirst := BASE+1`) is only PARSED,
// never resolved, so an identifier in it had no reference and evaluating the
// ordinal failed. Resolve them here, while the enum's own scope is still on top
// so that an EARLIER member of this same enum is visible (libcurl, gmp, fpalsa).
var
  i: Integer;
  EnumValue: TPasEnumValue;
begin
  for i:=0 to El.Values.Count-1 do
    begin
    EnumValue:=TPasEnumValue(El.Values[i]);
    if (EnumValue.Value<>nil) and (EnumValue.Value.CustomData=nil) then
      ResolveExpr(EnumValue.Value,rraRead);
    end;
  if TopScope.Element=El then
    PopScope;
end;

procedure TPasResolver.FinishSetType(El: TPasSetType);

  function GetEnumTypePosEl: TPasElement;
  begin
    Result:=El.EnumType;
    if Result.Parent<>El then
      Result:=El;
  end;

var
  BaseTypeData: TResElDataBaseType;
  StartResolved, EndResolved: TPasResolverResult;
  RangeExpr: TBinaryExpr;
  C: TClass;
  EnumType: TPasType;
begin
  EnumType:=ResolveAliasType(El.EnumType);
  C:=EnumType.ClassType;
  if C=TPasEnumType then
    begin
    FinishSubElementType(El,EnumType);
    exit;
    end
  else if C=TPasRangeType then
    begin
    RangeExpr:=TPasRangeType(EnumType).RangeExpr;
    if (RangeExpr.Parent=El) and (RangeExpr.CustomData=nil) then
      FinishConstRangeExpr(RangeExpr,StartResolved,EndResolved);
    FinishSubElementType(El,EnumType);
    exit;
    end
  else if C=TPasUnresolvedSymbolRef then
    begin
    if EnumType.CustomData is TResElDataBaseType then
      begin
      BaseTypeData:=TResElDataBaseType(EnumType.CustomData);
      if BaseTypeData.BaseType in (btAllChars+[btBoolean,btByte]) then
        exit;
      RaiseXExpectedButYFound(20170216151553,'Char or boolean',
        GetElementTypeName(EnumType),GetEnumTypePosEl);
      end;
    end;
  RaiseXExpectedButYFound(20170216151557,'enum type',
    GetElementTypeName(EnumType),GetEnumTypePosEl);
end;

procedure TPasResolver.FinishSubElementType(Parent: TPasElement; El: TPasType);
var
  EnumScope: TPasEnumTypeScope;
begin
  EmitTypeHints(Parent,El);
  if (El.Name<>'') or (AnonymousElTypePostfix='') then exit;
  if El.Parent<>Parent then
    RaiseNotYetImplemented(20220320123426,Parent,GetElementTypeName(El));
  DeanonymizeType(El);

  if (El.ClassType=TPasEnumType) and (Parent.ClassType=TPasSetType) then
    begin
    // anonymous enumtype
    EnumScope:=TPasEnumTypeScope(El.CustomData);
    if EnumScope.CanonicalSet<>Parent then
      begin
      // When a TPasEnumType is created a CanonicalSet is created.
      // Release the autocreated CanonicalSet and use the parent.
      EnumScope.CanonicalSet:=TPasSetType(Parent);
      end;
    end;
end;

procedure TPasResolver.FinishRangeType(El: TPasRangeType);
var
  RangeExpr: TBinaryExpr;
  StartResolved, EndResolved: TPasResolverResult;
begin
  RangeExpr:=El.RangeExpr;
  ResolveExpr(RangeExpr.Left,rraRead);
  ResolveExpr(RangeExpr.Right,rraRead);
  FinishConstRangeExpr(RangeExpr,StartResolved,EndResolved);
end;

procedure TPasResolver.FinishConstRangeExpr(RangeExpr: TBinaryExpr; out
  LeftResolved, RightResolved: TPasResolverResult);
// for example Left..Right
var
  RgValue: TResEvalValue;
  Left, Right: TPasExpr;
begin
  Left:=RangeExpr.Left;
  Right:=RangeExpr.Right;
  {$IFDEF VerbosePasResEval}
  writeln('TPasResolver.FinishConstRangeExpr Left=',GetObjName(Left),' Right=',GetObjName(Right));
  {$ENDIF}
  // check type compatibility
  ComputeElement(Left,LeftResolved,[rcConstant]);
  ComputeElement(Right,RightResolved,[rcConstant]);
  CheckSetLitElCompatible(Left,Right,LeftResolved,RightResolved);

  RgValue:=Eval(RangeExpr,[refConst]);
  ReleaseEvalValue(RgValue);
end;

procedure TPasResolver.FinishRecordType(El: TPasRecordType);
var
  Scope: TPasRecordScope;
begin
  if TopScope.Element<>El then
    RaiseNotYetImplemented(20190801232042,El);
  PopScope;

  Scope:=El.CustomData as TPasRecordScope;
  FinishRecordCompositions(El,Scope);
  FinishGenericClassOrRecIntf(Scope);
end;

procedure TPasResolver.FinishRecordCompositions(El: TPasRecordType;
  Scope: TPasRecordScope);
// Record composition: collect the members composed via "contains".
// Own members hide composed members, the first composition wins.

  function VisRank(Vis: TPasMemberVisibility): integer;
  begin
    case Vis of
    visStrictPrivate: Result:=0;
    visPrivate: Result:=1;
    visStrictProtected: Result:=2;
    visProtected: Result:=3;
    visPublished: Result:=5;
    else Result:=4; // default, public
    end;
  end;

  function IsComposable(Member: TPasElement): boolean;
  var
    C: TClass;
  begin
    Result:=false;
    if Member.Name='' then exit;
    C:=Member.ClassType;
    if C=TPasVariable then
      Result:=[vmClass,vmStatic]*TPasVariable(Member).VarModifiers=[]
    else if (C=TPasConst) or (C=TPasProperty) then
      Result:=true
    else if Member is TPasProcedure then
      Result:=not (Member is TPasOperator)
          and (C<>TPasClassConstructor) and (C<>TPasClassDestructor);
  end;

  procedure AddComposed(Member: TPasElement; const Path: TPasVariableArray;
    MemberVis, SectionVis: TPasMemberVisibility; IsUnnamed: boolean;
    ErrorEl: TPasElement);
  var
    Existing: TPasIdentifier;
    Vis: TPasMemberVisibility;
  begin
    // only members visible to the composing record are composed
    case MemberVis of
    visStrictPrivate,visStrictProtected:
      exit;
    visPrivate,visProtected:
      if Member.GetModule<>El.GetModule then exit;
    end;
    if VisRank(SectionVis)<VisRank(MemberVis) then
      Vis:=SectionVis
    else
      Vis:=MemberVis;

    // check collisions
    Existing:=Scope.FindLocalIdentifier(Member.Name);
    if (Existing=nil) and (Scope.CompositionScope<>nil) then
      begin
      Existing:=Scope.CompositionScope.FindLocalIdentifier(Member.Name);
      if (Existing<>nil)
          and (TPasCompositionIdentifier(Existing).Path[0]=Path[0]) then
        Existing:=nil; // e.g. overloads of the same composition
      end;
    if Existing<>nil then
      begin
      if IsUnnamed then
        RaiseMsg(20260928120010,nDuplicateIdentifier,sDuplicateIdentifier,
          [Member.Name,GetElementSourcePosStr(Existing.Element)],ErrorEl)
      else if (Member is TPasProcedure)
          or ((Member is TPasProperty) and (TPasProperty(Member).Args.Count>0)) then
        LogMsg(20260928120020,mtWarning,nIdentifierXCannotBeOverloadedForTypeY,
          sIdentifierXCannotBeOverloadedForTypeY,[Member.Name,El.Name],ErrorEl)
      else
        LogMsg(20260928120030,mtWarning,nDuplicateIdentifierX,
          sDuplicateIdentifierX,[Member.Name],ErrorEl);
      exit;
      end;

    if Scope.CompositionScope=nil then
      begin
      Scope.CompositionScope:=TPasRecordCompositionScope.Create;
      Scope.CompositionScope.Element:=El;
      Scope.CompositionScope.VisibilityContext:=El;
      end;
    Scope.CompositionScope.AddComposition(Member.Name,Member,Path,Vis);
  end;

  procedure AddMembers(Members: TFPList; const Path: TPasVariableArray;
    SectionVis: TPasMemberVisibility; IsUnnamed: boolean; ErrorEl: TPasElement);
  var
    i, j: Integer;
    Member: TPasElement;
    Overloads: TFPList;
  begin
    for i:=0 to Members.Count-1 do
      begin
      Member:=TPasElement(Members[i]);
      if Member is TPasOverloadedProc then
        begin
        Overloads:=TPasOverloadedProc(Member).Overloads;
        for j:=0 to Overloads.Count-1 do
          begin
          Member:=TPasElement(Overloads[j]);
          if IsComposable(Member) then
            AddComposed(Member,Path,Member.Visibility,SectionVis,IsUnnamed,ErrorEl);
          end;
        end
      else if IsComposable(Member) then
        AddComposed(Member,Path,Member.Visibility,SectionVis,IsUnnamed,ErrorEl);
      end;
  end;

  procedure AddVariantMembers(Rec: TPasRecordType; const Path: TPasVariableArray;
    SectionVis: TPasMemberVisibility; IsUnnamed: boolean; ErrorEl: TPasElement);
  var
    i: Integer;
    SubRec: TPasRecordType;
  begin
    if Rec.Variants=nil then exit;
    for i:=0 to Rec.Variants.Count-1 do
      begin
      SubRec:=TPasVariant(Rec.Variants[i]).Members;
      if SubRec=nil then continue;
      AddMembers(SubRec.Members,Path,SectionVis,IsUnnamed,ErrorEl);
      AddVariantMembers(SubRec,Path,SectionVis,IsUnnamed,ErrorEl);
      end;
  end;

  function IsRecordConstraint(TemplType: TPasGenericTemplateType): boolean;
  var
    i: Integer;
  begin
    for i:=0 to length(TemplType.Constraints)-1 do
      if GetGenericConstraintKeyword(TemplType.Constraints[i])<>tkrecord then
        exit(false);
    Result:=true;
  end;

  procedure AddComposition(Field: TPasVariable; SectionVis: TPasMemberVisibility;
    ErrorEl: TPasElement);
  var
    LoType: TPasType;
    ChildRec: TPasRecordType;
    ChildScope: TPasRecordScope;
    Path, SubPath: TPasVariableArray;
    IsUnnamed: Boolean;
    i, j: Integer;
    Item: TPasCompositionIdentifier;
  begin
    LoType:=ResolveAliasType(Field.VarType);
    if LoType is TPasGenericTemplateType then
      begin
      // composition is deferred until specialization
      if not IsRecordConstraint(TPasGenericTemplateType(LoType)) then
        RaiseMsg(20260928120040,nRecordTypeExpected,sRecordTypeExpected,[],ErrorEl);
      if Scope.ComposedTemplate=nil then
        Scope.ComposedTemplate:=TPasGenericTemplateType(LoType);
      exit;
      end;
    if not (LoType is TPasRecordType)
        or not (LoType.CustomData is TPasRecordScope) then
      RaiseMsg(20260928120050,nRecordTypeExpected,sRecordTypeExpected,[],ErrorEl);
    ChildRec:=TPasRecordType(LoType);
    ChildScope:=TPasRecordScope(ChildRec.CustomData);
    if (ChildScope.ComposedTemplate<>nil) and (Scope.ComposedTemplate=nil) then
      Scope.ComposedTemplate:=ChildScope.ComposedTemplate;
    IsUnnamed:=Field.Name='';
    Path:=nil;
    SetLength(Path,1);
    Path[0]:=Field;
    AddMembers(ChildRec.Members,Path,SectionVis,IsUnnamed,ErrorEl);
    AddVariantMembers(ChildRec,Path,SectionVis,IsUnnamed,ErrorEl);
    if ChildScope.CompositionScope<>nil then
      for i:=0 to ChildScope.CompositionScope.Items.Count-1 do
        begin
        Item:=TPasCompositionIdentifier(ChildScope.CompositionScope.Items[i]);
        SubPath:=nil;
        SetLength(SubPath,length(Item.Path)+1);
        SubPath[0]:=Field;
        for j:=0 to length(Item.Path)-1 do
          SubPath[j+1]:=Item.Path[j];
        AddComposed(Item.Element,SubPath,Item.Visibility,SectionVis,IsUnnamed,ErrorEl);
        end;
  end;

  procedure AddCompositions(Members: TFPList);
  var
    i: Integer;
    Member: TPasElement;
    Ref: TResolvedReference;
  begin
    for i:=0 to Members.Count-1 do
      begin
      Member:=TPasElement(Members[i]);
      if (Member.ClassType=TPasVariable)
          and (vmContains in TPasVariable(Member).VarModifiers) then
        AddComposition(TPasVariable(Member),Member.Visibility,Member)
      else if Member.ClassType=TPasContainsAlias then
        begin
        if not (TPasContainsAlias(Member).Expr.CustomData is TResolvedReference) then
          continue;
        Ref:=TResolvedReference(TPasContainsAlias(Member).Expr.CustomData);
        AddComposition(Ref.Declaration as TPasVariable,Member.Visibility,Member);
        end;
      end;
  end;

  procedure AddVariantCompositions(Rec: TPasRecordType);
  var
    i: Integer;
    SubRec: TPasRecordType;
  begin
    if Rec.Variants=nil then exit;
    for i:=0 to Rec.Variants.Count-1 do
      begin
      SubRec:=TPasVariant(Rec.Variants[i]).Members;
      if SubRec=nil then continue;
      AddCompositions(SubRec.Members);
      AddVariantCompositions(SubRec);
      end;
  end;

begin
  AddCompositions(El.Members);
  AddVariantCompositions(El);
end;

procedure TPasResolver.FinishContainsAlias(El: TPasContainsAlias);
// record composition: "contains alias FieldName"
var
  aName: String;
  Rec: TPasRecordType;
  RecScope: TPasRecordScope;
  Item: TPasIdentifier;
  Field: TPasElement;
  i: Integer;
  Member: TPasElement;
  Ref: TResolvedReference;
begin
  if El.Expr.CustomData is TResolvedReference then exit;
  if not (El.Expr is TPrimitiveExpr) then
    RaiseNotYetImplemented(20260928120100,El.Expr);
  aName:=TPrimitiveExpr(El.Expr).Value;
  Rec:=El.Parent as TPasRecordType;
  while Rec.Parent is TPasVariant do
    Rec:=Rec.Parent.Parent as TPasRecordType;
  RecScope:=Rec.CustomData as TPasRecordScope;
  Item:=RecScope.FindLocalIdentifier(aName);
  if Item=nil then
    RaiseIdentifierNotFound(20260928120110,aName,El.Expr);
  Field:=Item.Element;
  if (Field.ClassType<>TPasVariable)
      or ([vmClass,vmStatic]*TPasVariable(Field).VarModifiers<>[]) then
    RaiseXExpectedButYFound(20260928120120,'field',GetElementTypeName(Field),El.Expr);
  if vmContains in TPasVariable(Field).VarModifiers then
    RaiseMsg(20260928120130,nDuplicateIdentifier,sDuplicateIdentifier,
      [aName,GetElementSourcePosStr(Field)],El.Expr);
  // check duplicate alias
  for i:=0 to TPasRecordType(El.Parent).Members.Count-1 do
    begin
    Member:=TPasElement(TPasRecordType(El.Parent).Members[i]);
    if Member=El then break;
    if (Member.ClassType=TPasContainsAlias)
        and (TPasContainsAlias(Member).Expr.CustomData is TResolvedReference) then
      begin
      Ref:=TResolvedReference(TPasContainsAlias(Member).Expr.CustomData);
      if Ref.Declaration=Field then
        RaiseMsg(20260928120140,nDuplicateIdentifier,sDuplicateIdentifier,
          [aName,GetElementSourcePosStr(Member)],El.Expr);
      end;
    end;
  CreateReference(Field,El.Expr,rraRead);
end;

procedure TPasResolver.FinishClassType(El: TPasClassType);
type
  TMethResolution = record
    InterfaceIndex: integer;
    ProcClassType: TPasProcedureClass;
    InterfaceName: string;
    ImplementName: string;
    ResolutionEl: TPasMethodResolution;
    Count: integer; // needed to check if method resolution is used
  end;
var
  ClassScope: TPasClassScope;
  i, j, k: Integer;
  IntfType: TPasClassType;
  Resolutions: array of TMethResolution;
  Map: TPasClassIntfMap;
  o: TObject;
  Member, Parent: TPasElement;
  IntfProc: TPasProcedure;
  FindData: TFindProcData;
  Abort: boolean;
  MethRes: TPasMethodResolution;
  ResolvedEl: TPasResolverResult;
  ProcName, IntfProcName: String;
  Expr: TPasExpr;
  SectionScope: TPasSectionScope;
begin
  Resolutions:=nil;
  ClassScope:=nil;
  if not El.IsForward then
    begin
    if TopScope.Element<>El then
      RaiseInternalError(20180322142534,GetObjName(El)+'<>'+GetObjName(TopScope.Element));
    ClassScope:=El.CustomData as TPasClassScope;
    if ClassScope=nil then
      RaiseNotYetImplemented(20190803204709,El);

    // A sealed class cannot be subclassed, so an abstract method could never be
    // overridden -> illegal (FPC parser_e_sealed_class_cannot_have_abstract_methods).
    if pcsfSealed in ClassScope.Flags then
      for i:=0 to El.Members.Count-1 do
        if (TPasElement(El.Members[i]) is TPasProcedure)
            and TPasProcedure(El.Members[i]).IsAbstract then
          RaiseMsg(20260623100000,nSealedClassCannotHaveAbstractMethod,
            sSealedClassCannotHaveAbstractMethod,[],TPasElement(El.Members[i]));

    // Check for trailing unbound attributes at end of class body
    if El.Members.Count > 0 then
      begin
      Member := TPasElement(El.Members[El.Members.Count - 1]);
      if Member is TPasAttributes then
        RaiseMsg(20260226100001,nAttributeNotAllowedHere,
          sAttributeNotAllowedHere,[],Member);
      end;

    if El.ObjKind=okClass then
      begin
      if (El.Interfaces.Count>0) then
        begin
        if (ClassScope.Interfaces=nil) then
          RaiseInternalError(20180408162725,'');
        if (ClassScope.Interfaces.Count<>El.Interfaces.Count) then
          RaiseInternalError(20180408162746,'');
        end
      else if ClassScope.Interfaces<>nil then
        RaiseInternalError(20180408162803,'');

      // check explicit method resolutions, e.g. procedure intf.intfproc = implproc
      for i:=0 to El.Members.Count-1 do
        begin
        Member:=TPasElement(El.Members[i]);
        if not (Member is TPasMethodResolution) then continue;
        MethRes:=TPasMethodResolution(Member);

        // get interface
        ComputeElement(MethRes.InterfaceName,ResolvedEl,[rcNoImplicitProc]);
        if not (ResolvedEl.IdentEl is TPasType) then
          RaiseInternalError(20180323135729,GetResolverResultDbg(ResolvedEl));
        j:=El.Interfaces.IndexOf(ResolvedEl.IdentEl);
        if (j<0) and (ResolvedEl.LoTypeEl<>nil) then
          begin
          // The method resolution's interface type may be a different object
          // than the one in the class's interface list (e.g. a generic interface
          // specialization parsed separately — tgeneric78/79). Match by resolved
          // type identity.
          for j:=0 to El.Interfaces.Count-1 do
            if IsSameType(TPasType(El.Interfaces[j]),
                TPasType(ResolvedEl.IdentEl),prraAlias) then
              break;
          if j>=El.Interfaces.Count then
            j:=-1;
          end;
        if j<0 then
          RaiseInternalError(20180323135900,GetResolverResultDbg(ResolvedEl));
        // get class-interface-map, check delegations
        o:=TObject(ClassScope.Interfaces[j]);
        if o is TPasProperty then
          RaiseMsg(20180323140046,nCannotMixMethodResolutionAndDelegationAtX,
            sCannotMixMethodResolutionAndDelegationAtX,
            [GetElementSourcePosStr(TPasProperty(o))],MethRes.InterfaceName);
        if o=nil then
          o:=CreateClassIntfMap(El,j);
        Map:=TPasClassIntfMap(o);
        // get interface proc name
        Expr:=MethRes.InterfaceProc;
        if not (Expr is TPrimitiveExpr) then
          RaiseXExpectedButYFound(20180327162230,'method name',GetElementTypeName(Expr),Expr);
        if TPrimitiveExpr(Expr).Kind<>pekIdent then
          RaiseXExpectedButYFound(20180327162236,'method name',GetElementTypeName(Expr),Expr);
        IntfProcName:=TPrimitiveExpr(Expr).Value;
        // get implementation proc name
        Expr:=MethRes.ImplementationProc;
        if not (Expr is TPrimitiveExpr) then
          RaiseXExpectedButYFound(20180327152115,'method name',GetElementTypeName(Expr),Expr);
        if TPrimitiveExpr(Expr).Kind<>pekIdent then
          RaiseXExpectedButYFound(20180327152157,'method name',GetElementTypeName(Expr),Expr);
        ProcName:=TPrimitiveExpr(Expr).Value;

        for k:=0 to length(Resolutions)-1 do
          with Resolutions[k] do
            if (InterfaceIndex=j) and (ProcClassType=MethRes.ProcClass)
                and (InterfaceName=IntfProcName) then
              RaiseMsg(20180327164626,nDuplicateIdentifier,sDuplicateIdentifier,
                [GetElementTypeName(ProcClassType)+' '+Map.Intf.Name+'.'+InterfaceName,
                 GetElementSourcePosStr(ResolutionEl)],MethRes.InterfaceProc);

        // add resolution
        k:=length(Resolutions);
        SetLength(Resolutions,k+1);
        with Resolutions[k] do
          begin
          InterfaceIndex:=j;
          ProcClassType:=MethRes.ProcClass;
          InterfaceName:=IntfProcName;
          ImplementName:=ProcName;
          ResolutionEl:=MethRes;
          Count:=0;
          end;
        end;

      // method resolution
      for i:=0 to El.Interfaces.Count-1 do
        begin
        o:=TObject(ClassScope.Interfaces[i]);
        //writeln('TPasResolver.FinishClassType class=',GetObjName(El),' i=',i,' Intf=',GetObjName(TObject(El.Interfaces[i])),' Map=',GetObjName(o));
        if o is TPasProperty then
          continue; // interface implemented via a property
        if o=nil then
          o:=CreateClassIntfMap(El,i);
        Map:=TPasClassIntfMap(o);
        while Map<>nil do
          begin
          IntfType:=Map.Intf;
          //writeln('TPasResolver.FinishClassType ',GetObjName(Map),' ',GetObjName(IntfType),' Count=',IntfType.Members.Count);
          // An interface reached through a partial specialization is the bare
          // GENERIC (PartialSpecsAsGeneric), so its methods still have its own
          // type parameters and nothing in this class can match them. Nothing
          // is decided yet either - the specialized copy is checked for real.
          if FPartialSpecsAsGeneric and ParamIsUndecided(IntfType) then
            begin
            Map:=Map.AncestorMap;
            continue;
            end;
          for j:=0 to IntfType.Members.Count-1 do
            begin
            Member:=TPasElement(IntfType.Members[j]);
            if not (Member is TPasProcedure) then continue;
            IntfProc:=TPasProcedure(Member);
            ProcName:=IntfProc.Name;
            // check resolutions
            for k:=0 to length(Resolutions)-1 do
              with Resolutions[k] do
                begin
                if (InterfaceIndex=i) and (ProcClassType=IntfProc.ClassType)
                    and SameText(InterfaceName,IntfProc.Name) then
                  begin
                  ProcName:=ImplementName;
                  inc(Count);
                  end;
                end;

            // search interface method in class
            FindData:=Default(TFindProcData);
            FindData.Proc:=IntfProc;
            FindData.Args:=IntfProc.ProcType.Args;
            FindData.Kind:=fpkProcDeclaration;
            Abort:=false;
            IterateElements(ProcName,@OnFindProcDeclaration,@FindData,Abort);
            // A strict-private method inherited from an ancestor is not
            // accessible here and cannot implement an interface method (tb0266).
            if (FindData.Found<>nil)
                and (FindData.Found.Visibility=visStrictPrivate)
                and (FindData.Found.Parent<>El) then
              FindData.Found:=nil;
            // A generic whose ancestor still depends on a type parameter has
            // that ancestor answered with the bare generic, whose methods have
            // its own type parameters: the specialized class is checked for real.
            if (FindData.Found=nil) and FPartialSpecsAsGeneric
                and (GetTypeParameterCount(El)>0) and (El.AncestorType<>nil)
                and ParamIsUndecided(El.AncestorType) then
              continue;
            if FindData.Found=nil then
              RaiseMsg(20180322143202,nNoMatchingImplForIntfMethodXFound,
                sNoMatchingImplForIntfMethodXFound,
                [GetProcTypeDescription(IntfProc.ProcType,[prptdUseName,prptdAddPaths,prptdResolveSimpleAlias])],El); // ToDo: jump to interface list
            // check calling conventions
            //writeln('TPasResolver.FinishClassType Intf=',GetObjPath(IntfProc),' Found=',GetObjPath(FindData.Found));
            CheckProcSignatureMatch(IntfProc,TPasProcedure(FindData.Found),true);

            Map.Procs[j]:=FindData.Found;
            end;
          Map:=Map.AncestorMap;
          end;
        end;

      // ToDo: hint if method resolution is not used
      end;

    if El.ObjKind in okAllHelpers then
      begin
      // activate helper
      AddActiveHelper(El);

      // cache helpers in interface, so other modules don't have to search
      Parent:=El.Parent;
      while Parent<>nil do
        begin
        if Parent.ClassType=TInterfaceSection then
          begin
          SectionScope:=Parent.CustomData as TPasSectionScope;
          AddHelper(El,SectionScope.Helpers);
          break;
          end;
        Parent:=Parent.Parent;
        end;
      end;

    end;

  if TopScope.Element=El then
    PopScope // pop TPasClassScope
  else
    ; // e.g. class forward

  if TopScope is TPasGenericParamsScope then
    PopGenericParamScope(El);

  if not El.IsForward then
    FinishGenericClassOrRecIntf(ClassScope);
end;

procedure TPasResolver.FinishClassOfType(El: TPasClassOfType);
var
  TypeEl: TPasType;
begin
  TypeEl:=ResolveAliasType(El.DestType);
  if TypeEl is TUnresolvedPendingRef then
    exit;
  if (TypeEl is TPasClassType) and (TPasClassType(TypeEl).ObjKind=okClass) then exit;
  // `class of T` for a type parameter: its specialization decides
  if (TypeEl is TPasGenericTemplateType)
      or (FPartialSpecsAsGeneric and ParamIsUndecided(El.DestType)) then
    exit;
  RaiseMsg(20170216151602,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
    [El.DestType.Name,'class'],El);
end;

procedure TPasResolver.FinishPointerType(El: TPasPointerType);
var
  TypeEl: TPasType;
begin
  TypeEl:=ResolveAliasType(El.DestType);
  if TypeEl is TUnresolvedPendingRef then
    exit;
  if (El.DestType.Parent=El) and not (El.DestType is TPasSpecializeType)
      and not (El.DestType is TPasTypeOfType) then
    RaiseMsg(20180429094237,nNotYetImplemented,sNotYetImplemented,['pointer of anonymous type'], El.DestType);
  CheckUseAsType(El.DestType,20190123095118,El);
  CheckPointerCycle(El);
end;

procedure TPasResolver.FinishArrayType(El: TPasArrayType);
var
  i: Integer;
  Expr: TPasExpr;
  RangeResolved: TPasResolverResult;
  TypeEl: TPasType;
  Parent: TPasArrayType;
  Scope: TPasArrayScope;
begin
  // check cycles
  Parent:=El;
  repeat
    if Parent=El.ElType then
      RaiseMsg(20190807104630,nIllegalExpression,sIllegalExpression,[],El);
    if Parent.Parent is TPasArrayType then
      Parent:=TPasArrayType(Parent.Parent)
    else
      break;
  until false;

  for i:=0 to length(El.Ranges)-1 do
    begin
    Expr:=El.Ranges[i];
    ResolveExpr(Expr,rraRead);
    ComputeElement(Expr,RangeResolved,[rcConstant]);
    if (RangeResolved.IdentEl<>nil) and not (RangeResolved.IdentEl is TPasType) then
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.FinishArrayType ',GetResolverResultDbg(RangeResolved));
      {$ENDIF}
      RaiseXExpectedButYFound(20170216151607,'range',GetElementTypeName(RangeResolved.IdentEl),Expr);
      end;
    // Emit hints (deprecated/platform/...) for a named index type, e.g.
    // `array[TTest]` where TTest is a deprecated subrange type.
    if RangeResolved.IdentEl is TPasType then
      EmitTypeHints(Expr,TPasType(RangeResolved.IdentEl));
    if (RangeResolved.BaseType=btRange) then
      begin
      if (RangeResolved.SubType in btArrayRangeTypes) then
        // range, e.g. 1..2
      else if RangeResolved.SubType=btContext then
        begin
        TypeEl:=RangeResolved.LoTypeEl;
        if TypeEl is TPasRangeType then
          // custom range
        else if TypeEl is TPasEnumType then
          // anonymous enum range
        else
          RaiseXExpectedButYFound(20171009193629,'range',GetElementTypeName(RangeResolved.IdentEl),Expr);
        end
      else
        RaiseXExpectedButYFound(20171009193514,'range',GetElementTypeName(RangeResolved.IdentEl),Expr);
      end
    else if RangeResolved.BaseType in btArrayRangeTypes then
      // full range, e.g. array[char]
    else if (RangeResolved.BaseType=btContext) and (RangeResolved.LoTypeEl is TPasEnumType) then
      // e.g. array[enumtype]
    else if (RangeResolved.BaseType=btContext) and (RangeResolved.LoTypeEl is TPasGenericTemplateType) then
      // e.g. Tarr<T> = array[T] of ...
    else if RangeResolved.IdentEl<>nil then
      RaiseXExpectedButYFound(20170216151609,'range',GetElementTypeName(RangeResolved.IdentEl),Expr)
    else
      RaiseXExpectedButYFound(20190830215123,'range',GetResolverResultDescription(RangeResolved),Expr);
    end;
  if El.ElType=nil then
    begin
    // array of const
    if length(El.Ranges)>0 then
      RaiseNotYetImplemented(20190215102529,El);
    FindTVarRec(El);
    end
  else
    begin
    CheckUseAsType(El.ElType,20190123095401,El);
    // Emit hints (deprecated/platform/...) for the element type, e.g.
    // `array of TTest` where TTest is a deprecated type.
    EmitTypeHints(El,El.ElType);
    FinishSubElementType(El,El.ElType);
    end;

  if El.CustomData is TPasArrayScope then
    begin
    Scope:=TPasArrayScope(El.CustomData);
    Scope.GenericStep:=psgsImplementationParsed;
    end;

  if TopScope.Element=El then
    PopScope;
end;

procedure TPasResolver.FinishAliasType(El: TPasAliasType);
var
  aType: TPasType;
  Decls: TFPList;
begin
  // Attributes are not allowed on a plain (transparent) type alias, which has no
  // RTTI of its own. A DISTINCT type alias (`type X = type Y`, TPasTypeAliasType)
  // does get its own RTTI, so attributes ARE allowed there (tcustomattr18).
  // Note: El is not yet in Parent.Declarations at this point (FinishScope
  // is called before Add), so check if the last declaration is a TPasAttributes.
  if (El.Parent is TPasDeclarations) and MaximizeFPCCompatibility
      and not (El is TPasTypeAliasType) then
    begin
    Decls := TPasDeclarations(El.Parent).Declarations;
    if  (Decls.Count > 0)
        and (TObject(Decls[Decls.Count - 1]) is TPasAttributes) then
      RaiseMsg(20260226100002,nAttributeNotAllowedHere,
        sAttributeNotAllowedHere,[],El);
    end;
  aType:=ResolveAliasType(El);
  if (aType is TPasMembersType) and (aType.CustomData=nil) then
    exit;
  if (aType is TPasGenericType)
      and (GetTypeParameterCount(TPasGenericType(aType))>0) then
    RaiseMsg(20190818135830,nXExpectedButYFound,sXExpectedButYFound,
      ['type',GetTypeDescription(aType)],El);
  EmitTypeHints(El,TPasAliasType(El).DestType);
end;

procedure TPasResolver.FinishTypeOfType(El: TPasTypeOfType);
begin
  Inc(FTypeOfOperandLevel);
  try
    ResolveExpr(El.Expr,rraRead);
    El.DestType:=GetTypeOfOperandType(El.Expr,El.Expr);
  finally
    Dec(FTypeOfOperandLevel);
  end;
end;

procedure TPasResolver.FinishGenericTemplateType(El: TPasGenericTemplateType);
var
  ConEl: TPasElement;

  procedure RaiseCannotBeTogether(const Id: TMaxPrecInt; const X,Y: string);
  begin
    RaiseMsg(Id,nConstraintXAndConstraintYCannotBeTogether,
             sConstraintXAndConstraintYCannotBeTogether,[X,Y],
             GetGenericConstraintErrorEl(ConEl,El));
  end;

  procedure RaiseXIsNotAValidConstraint(const Id: TMaxPrecInt; const X: string);
  begin
    RaiseMsg(Id,nXIsNotAValidConstraint,sXIsNotAValidConstraint,[X],
             GetGenericConstraintErrorEl(ConEl,El));
  end;

var
  i: Integer;
  IsClass, IsRecord, IsConstructor, HasConcreteClass: Boolean;
  LastType: TPasType;
  MemberType: TPasMembersType;
  aClass: TPasClassType;
  ConToken: TToken;
  ResolvedEl: TPasResolverResult;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishGenericTemplateType ',GetObjName(El),' El.Parent=',GetObjName(El.Parent),' Constraints=',length(El.Constraints));
  {$ENDIF}
  // For const generic params, constraints are type annotations (e.g., ': Integer'),
  // not type constraints (e.g., ': class'). Skip standard constraint validation.
  if El.IsConst then
    begin
    if MaximizeFPCCompatibility and (length(El.Constraints) = 0) then
      RaiseMsg(20260304200001,nXExpectedButYFound,sXExpectedButYFound,
        ['type annotation for const parameter','untyped const'],
        GetGenericConstraintErrorEl(El,El));
    exit;
    end;

  // Constraint-combination matrix, matching FPC (compiler/pgenutil.pas):
  //   * "class" keyword: at most once; not with a concrete class type;
  //     may combine with interfaces and "constructor".
  //   * "record": exclusive (no class/constructor/interface/type constraints).
  //   * "constructor": at most once; not with "record".
  //   * a concrete class type: at most one; not with the "class" keyword;
  //     may combine with interfaces and "constructor".
  //   * interface types: any number; combine with class/concrete-class/ctor.
  // The checks are order-independent (flag based), not order-dependent.
  IsClass:=false;
  IsRecord:=false;
  IsConstructor:=false;
  HasConcreteClass:=false;
  LastType:=nil;
  for i:=0 to length(El.Constraints)-1 do
    begin
    ConEl:=El.Constraints[i];
    ConToken:=GetGenericConstraintKeyword(ConEl);
    case ConToken of
    tkclass:
      begin
      if IsClass then
        RaiseMsg(20190720202412,nConstraintXSpecifiedMoreThanOnce,
          sConstraintXSpecifiedMoreThanOnce,['class'],ConEl);
      if IsRecord then
        RaiseCannotBeTogether(20190720202516,'record','class');
      if HasConcreteClass then
        RaiseCannotBeTogether(20190720205708,LastType.Name,'class');
      IsClass:=true;
      end;
    tkrecord:
      begin
      if IsRecord then
        RaiseMsg(20190720203028,nConstraintXSpecifiedMoreThanOnce,
          sConstraintXSpecifiedMoreThanOnce,['record'],ConEl);
      if IsClass then
        RaiseCannotBeTogether(20190720203039,'class','record');
      if IsConstructor then
        RaiseCannotBeTogether(20190720203056,'constructor','record');
      if LastType<>nil then
        RaiseCannotBeTogether(20190720205938,LastType.Name,'record');
      IsRecord:=true;
      end;
    tkconstructor:
      begin
      if IsConstructor then
        RaiseMsg(20190720203123,nConstraintXSpecifiedMoreThanOnce,
          sConstraintXSpecifiedMoreThanOnce,['constructor'],ConEl);
      if IsRecord then
        RaiseCannotBeTogether(20190720203148,'record','constructor');
      // constructor combines freely with class/concrete-class/interfaces
      IsConstructor:=true;
      end;
    else
      if not (ConEl is TPasType) then
        RaiseXIsNotAValidConstraint(20190912215619,GetElementTypeName(ConEl));
      // type identifier: class or interface
      ComputeElement(ConEl,ResolvedEl,[rcType]);
      if ResolvedEl.BaseType<>btContext then
        RaiseXIsNotAValidConstraint(20190914105144,GetElementTypeName(ConEl));
      if IsRecord then
        RaiseCannotBeTogether(20190720210130,'record',ResolvedEl.HiTypeEl.Name);

      if ResolvedEl.LoTypeEl is TPasGenericTemplateType then
        begin
        if ResolvedEl.LoTypeEl=El then
          RaiseMsg(20200820185313,nTypeCycleFound,sTypeCycleFound,[],
                      GetGenericConstraintErrorEl(ConEl,El));
        // ok
        if length(El.Constraints)>1 then
          RaiseXIsNotAValidConstraint(20190831213645,ResolvedEl.HiTypeEl.Name);
        end
      else if ResolvedEl.LoTypeEl is TPasMembersType then
        begin
        MemberType:=TPasMembersType(ResolvedEl.LoTypeEl);
        if MemberType is TPasClassType then
          begin
          aClass:=TPasClassType(MemberType);
          case aClass.ObjKind of
          okClass:
            begin
            // a concrete class type: not together with the "class" keyword,
            // and at most one concrete class constraint.
            if IsClass then
              RaiseCannotBeTogether(20190720210202,'class',MemberType.Name);
            if HasConcreteClass then
              RaiseCannotBeTogether(20190720210351,LastType.Name,MemberType.Name);
            HasConcreteClass:=true;
            end;
          okInterface:
            // any number of interface constraints, freely combined with
            // class keyword / concrete class / constructor.
            ;
          else
            RaiseXIsNotAValidConstraint(20190720210919,MemberType.Name);
          end;
          end
        else
          RaiseXIsNotAValidConstraint(20190720210809,MemberType.Name);
        end
      else
        RaiseXIsNotAValidConstraint(20190720204604,GetResolverResultDescription(ResolvedEl,true));

      LastType:=ResolvedEl.LoTypeEl;
    end; // end of case
    end; // end of for
end;

procedure TPasResolver.FinishSpecializeType(El: TPasSpecializeType);
begin
  // Already specialized - `A<B>.C` builds A<B> before reading C, and the parser
  // finishes the same node again afterwards.
  if El.CustomData<>nil then
    exit;
  FinishSpecializeTypeBody(El);
end;

procedure TPasResolver.FinishSpecializeTypeBody(El: TPasSpecializeType);
var
  Params, GenericTemplateList: TFPList;
  P: TPasElement;
  DestType: TPasType;
  i, ScopeDepth: Integer;
  GenType: TPasGenericType;
begin
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.FinishSpecializeType ');
  {$ENDIF}
  // resolve Params
  ScopeDepth:=StashSubExprScopes;
  Params:=El.Params;
  if Params.Count=0 then
    RaiseMsg(20190724114416,nMissingParameterX,sMissingParameterX,['type'],El);
  for i:=0 to Params.Count-1 do
    begin
    P:=TPasElement(Params[i]);
    if P is TPasExpr then
      ResolveExpr(TPasExpr(P),rraRead)
    else if P is TPasType then
    else
      RaiseMsg(20190728113336,nXExpectedButYFound,sXExpectedButYFound,['type identifier',GetObjName(P)+' parameter '+IntToStr(i+1)],El);
    end;
  RestoreStashedScopes(ScopeDepth);

  // check DestType
  DestType:=El.DestType;
  if DestType=nil then
    RaiseMsg(20190725184734,nIdentifierNotFound,sIdentifierNotFound,['specialize type'],El)
  else if not (DestType is TPasGenericType) then
    RaiseMsg(20190725193552,nXExpectedButYFound,sXExpectedButYFound,['generic type',DestType.Name],El);
  GenType:=TPasGenericType(DestType);
  // Note: there can be TBird, TBird<T> and TBird<T,U>
  GenericTemplateList:=GenType.GenericTemplateTypes;
  if GenericTemplateList=nil then
    RaiseMsg(20190725194222,nWrongNumberOfParametersForGenericX,sWrongNumberOfParametersForGenericX,
      ['type '+DestType.Name],El);
  if GenericTemplateList.Count<>Params.Count then
    RaiseMsg(20190801222656,nWrongNumberOfParametersForGenericX,sWrongNumberOfParametersForGenericX,
      ['type '+DestType.Name],El);

  GetSpecializedEl(El,GenType,Params);
end;

procedure TPasResolver.FinishResourcestring(El: TPasResString);
var
  ResolvedEl: TPasResolverResult;
begin
  ResolveExpr(El.Expr,rraRead);
  ComputeElement(El.Expr,ResolvedEl,[rcConstant]);
  if not (ResolvedEl.BaseType in btAllStringAndChars) then
    RaiseXExpectedButYFound(20171004135753,'string',GetTypeDescription(ResolvedEl),El.Expr);
end;

procedure TPasResolver.FinishProcedure(Proc: TPasProcedure);
var
  i: Integer;
  Body: TProcedureBody;
  SubEl: TPasElement;
  SubProcScope, ProcScope, DeclProcScope: TPasProcedureScope;
  SpecializedItem: TPRSpecializedItem;
  ParentScope: TPasScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishProcedure START');
  {$ENDIF}
  CheckTopScope(FScopeClass_Proc);
  ProcScope:=TPasProcedureScope(TopScope);
  if ProcScope.Element<>Proc then
    RaiseInternalError(20170220163043);

  SpecializedItem:=ProcScope.SpecializedFromItem;
  if SpecializedItem<>nil then
    begin
    if SpecializedItem.Step<prssImplementationBuilding then
      RaiseNotYetImplemented(20190920184908,Proc);
    if SpecializedItem.Step>prssImplementationBuilding then
      RaiseNotYetImplemented(20190920185123,Proc);
    end;

  Body:=Proc.Body;
  if Body<>nil then
    begin
    StoreScannerFlagsInProc(ProcScope);
    if Body.Body is TPasImplAsmStatement then
      Proc.Modifiers:=Proc.Modifiers+[pmAssembler];
    ResolveImplBlock(Body.Body);

    // check if all nested forward procs are resolved
    for i:=0 to Body.Declarations.Count-1 do
      begin
      SubEl:=TPasElement(Body.Declarations[i]);
      if (SubEl is TPasProcedure) and TPasProcedure(SubEl).IsForward then
        begin
        SubProcScope:=TPasProcedure(SubEl).CustomData as TPasProcedureScope;
        if SubProcScope.ImplProc=nil then
          RaiseMsg(20170216151613,nForwardProcNotResolved,sForwardProcNotResolved,
            [GetElementTypeName(SubEl),SubEl.Name],SubEl);
        end;
      end;
    if ProcScope.GroupScope<>nil then
      begin
      ProcScope.GroupScope.Free;
      ProcScope.GroupScope:=nil;
      if ProcScope.NestedMembersScope<>nil then
        begin
        for i:=0 to ScopeCount-1 do
          if Scopes[i]=ProcScope.NestedMembersScope then
            begin
            DeleteScope(i);
            break;
            end;
        ProcScope.NestedMembersScope.Free;
        ProcScope.NestedMembersScope:=nil;
        end;
      end;
    ProcScope.GenericStep:=psgsImplementationParsed;
    if ProcScope.DeclarationProc<>nil then
      begin
      DeclProcScope:=ProcScope.DeclarationProc.CustomData as TPasProcedureScope;
      DeclProcScope.GenericStep:=psgsImplementationParsed;
      end;
    end;
  if ProcScope.GroupScope<>nil then
    RaiseNotYetImplemented(20190122142142,Proc);
  if ProcScope.NestedMembersScope<>nil then
    RaiseNotYetImplemented(20191014233200,Proc);

  // For operators, CorrectName changes the name after initial scope registration.
  // Re-add with the final name so forward declaration matching can find it.
  if (Proc is TPasOperator) and (Proc.Name <> '') then
    begin
    ParentScope:=Scopes[ScopeCount-2];
    if ParentScope is TPasIdentifierScope then
      AddIdentifier(TPasIdentifierScope(ParentScope), Proc.Name, Proc, pikProc);
    end;

  if TopScope.Element<>Proc then
    RaiseInternalError(20190806094032);
  PopScope;

  if ProcScope.GenericStep=psgsImplementationParsed then
    begin
    if ProcScope.DeclarationProc<>nil then
      ProcScope:=TPasProcedureScope(ProcScope.DeclarationProc.CustomData);
    if ProcScope.SpecializedItems<>nil then
      FinishSpecializations(ProcScope);
    end;
end;

procedure TPasResolver.FinishProcedureType(El: TPasProcedureType);
var
  ProcName: String;
  FindData: TFindProcData;
  DeclProc, Proc, ParentProc: TPasProcedure;
  Abort, HasDots, IsClassConDestructor, HasRequired: boolean;
  DeclProcScope, ProcScope: TPasProcedureScope;
  ParentScope: TPasIdentifierScope;
  pm: TProcedureModifier;
  ptm: TProcTypeModifier;
  ObjKind: TPasObjKind;
  ParentBody: TProcedureBody;
  HelperForType: TPasType;
  Args, TemplTypes: TFPList;
  Arg: TPasArgument;
  ProcTypeScope: TPasProcTypeScope;
  C: TClass;
  FuncType: TPasFunctionType;
  i: Integer;
begin
  if TopScope.Element=El then
    begin
    ProcTypeScope:=El.CustomData as TPasProcTypeScope;
    ProcTypeScope.GenericStep:=psgsImplementationParsed;
    ProcTypeScope.BoolSwitches:=CurrentParser.Scanner.CurrentBoolSwitches;
    PopScope;
    end;
  if El.Parent is TPasProcedure then
    Proc:=TPasProcedure(El.Parent)
  else
    Proc:=nil;
  if (Proc<>nil) and (Proc.ProcType=El) then
    begin
    // finished header of a procedure declaration
    CheckTopScope(FScopeClass_Proc);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishProcedureHeader El=',GetTreeDbg(El),' ',GetElementSourcePosStr(El),' IsForward=',Proc.IsForward,' Parent=',GetObjName(El.Parent));
    {$ENDIF}
    ProcName:=Proc.Name;
    ProcScope:=Proc.CustomData as TPasProcedureScope;

    TemplTypes:=GetProcTemplateTypes(Proc);
    if (TemplTypes<>nil) then
      begin
      // Proc is parameterized
      if (Proc is TPasConstructor) or (Proc is TPasDestructor) then
        RaiseMsg(20190911104114,nTypeParamsNotAllowedOnX,sTypeParamsNotAllowedOnX,
          [Proc.ElementTypeName],Proc);
      if Proc.IsVirtual or Proc.IsDynamic or Proc.IsMessage or Proc.IsOverride then
        RaiseMsg(20190911112925,nXMethodsCannotHaveTypeParams,
          sXMethodsCannotHaveTypeParams,['virtual, dynamic or message'],El);
      if Proc.IsOverride then
        RaiseMsg(20191016174218,nXMethodsCannotHaveTypeParams,
          sXMethodsCannotHaveTypeParams,['override'],El);
      if not ((Proc.Visibility in [visDefault,visPrivate,visStrictPrivate,visProtected,visStrictProtected,visPublic])
           or ((Proc.Visibility=visPublished) and AllowGenericPublishedMethod)) then
        RaiseMsg(20191016174327,nXMethodsCannotHaveTypeParams,
          sXMethodsCannotHaveTypeParams,[VisibilityNames[Proc.Visibility]],El);
      end;

    if El is TPasFunctionType then
      begin
      FuncType:=TPasFunctionType(El);
      if FuncType.ResultEl<>nil then
        begin
        CheckUseAsType(FuncType.ResultEl.ResultType,20190123095743,FuncType.ResultEl);
        // Register named result variable (e.g. "c" in "operator +(a,b: T) c: T")
        if (FuncType.ResultEl.Name <> '') and
           not SameText(FuncType.ResultEl.Name, ResolverResultVar) then
          AddIdentifier(ProcScope,FuncType.ResultEl.Name,FuncType.ResultEl,pikSimple);
        end;
      end;

    if (proProcTypeWithoutIsNested in Options) and El.IsNested then
      RaiseInvalidProcTypeModifier(20170402120811,El,ptmIsNested,El);

    ParentBody:=GetParentProcBody(Proc.Parent);
    if (ParentBody<>nil) then
      begin
      // nested sub proc
      if TemplTypes<>nil then
        RaiseMsg(20190912173450,nTypeParamsNotAllowedOnX,sTypeParamsNotAllowedOnX,
          ['nested '+Proc.ElementTypeName],Proc);
      if not (proProcTypeWithoutIsNested in Options) then
        El.IsNested:=true;
      // inherit 'of Object'
      ParentProc:=ParentBody.Parent as TPasProcedure;
      if ParentProc.ProcType.IsOfObject then
        El.IsOfObject:=true;
      end;

    if El.IsReferenceTo then
      begin
      if El.IsNested then
        RaiseInvalidProcTypeModifier(20170419142818,El,ptmIsNested,El);
      if El.IsOfObject then
        RaiseInvalidProcTypeModifier(20170419142844,El,ptmOfObject,El);
      end;

    if Proc.IsExternal then
      begin
      for pm in Proc.Modifiers do
        if not (pm in [pmVirtual, pmDynamic, pmOverride,
                       pmOverload, pmMessage, pmReintroduce,
                       pmExternal, pmWeakExternal, pmDispId,
                       pmfar]) then
          RaiseMsg(20170216151616,nInvalidXModifierY,
            sInvalidXModifierY,[GetElementTypeName(Proc),'external, '+ModifierNames[pm]],Proc);
      for ptm in Proc.ProcType.Modifiers do
        if not (ptm in [ptmOfObject,ptmIsNested,ptmStatic,ptmVarargs,ptmReferenceTo,ptmAsync]) then
          RaiseMsg(20170411171224,nInvalidXModifierY,
            sInvalidXModifierY,[GetElementTypeName(Proc),'external, '+ProcTypeModifiers[ptm]],Proc);
      end;

    FinishProcTypeNativeChecks(El,Proc);

    if El.IsAsync then
      begin
      // async procedure
      C:=Proc.ClassType;
      if (C<>TPasProcedure)
          and (C<>TPasFunction)
          and (C<>TPasClassProcedure)
          and (C<>TPasClassFunction)
          and (C<>TPasAnonymousProcedure)
          and (C<>TPasAnonymousFunction) then
        RaiseMsg(20200524105449,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'async'],Proc);
      end;

    IsClassConDestructor:=(Proc.ClassType=TPasClassConstructor)
                       or (Proc.ClassType=TPasClassDestructor);
    if IsClassConDestructor then
      begin
      // class constructor/destructor
      if Proc.IsVirtual then
        RaiseMsg(20181231150237,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'virtual'],Proc);
      if Proc.IsOverride then
        RaiseMsg(20181231150305,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'override'],Proc);
      if Proc.IsDynamic then
        RaiseMsg(20181231150319,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'dynamic'],Proc);
      if Proc.IsStatic then
        RaiseMsg(20190216214651,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'static'],Proc);
      if El.Args.Count>0 then
        RaiseMsg(20181231150404,nXCannotHaveParameters,sXCannotHaveParameters,[GetElementTypeName(Proc)],Proc);
      end;

    HasDots:=GetFirstDotPos(ProcName)>0;

    if Proc.Parent is TPasClassType then
      begin
      // method declaration
      ObjKind:=TPasClassType(Proc.Parent).ObjKind;
      case ObjKind of
      okInterface,okDispInterface:
        begin
        if Proc.IsVirtual then
          RaiseMsg(20180321234324,nInvalidXModifierY,sInvalidXModifierY,[ObjKindNames[ObjKind]+' '+GetElementTypeName(Proc),'virtual'],Proc);
        if Proc.IsOverride then
          RaiseMsg(20180321234551,nInvalidXModifierY,sInvalidXModifierY,[ObjKindNames[ObjKind]+' '+GetElementTypeName(Proc),'override'],Proc);
        if TemplTypes<>nil then
          RaiseMsg(20190912153024,nXMethodsCannotHaveTypeParams,sXMethodsCannotHaveTypeParams,['interface'],Proc);
        end;
      okClassHelper,okRecordHelper,okTypeHelper:
        begin
        if Proc.IsAbstract then
          RaiseMsg(20190116215744,nInvalidXModifierY,sInvalidXModifierY,[ObjKindNames[ObjKind]+' '+GetElementTypeName(Proc),'abstract'],Proc);
        {if msDelphi in CurrentParser.CurrentModeswitches then
          begin
          // Delphi allows virtual/override in class helpers
          // But using them crashes in Delphi 10.3
          // -> do not support them
          end
        }
        // fcl-passrc's default policy forbids virtual/override in helpers (Delphi
        // 10.3 crashes on them). A native/FPC target may allow a helper's own
        // virtual/override hierarchy in Delphi mode (tchlp10/tchlp42) via the
        // AllowHelperVirtualMethods seam; a Delphi override with no helper
        // ancestor is still invalid (tchlp30). objfpc always forbids (thlp4/5/6).
        if not AllowHelperVirtualMethods then
          begin
          if Proc.IsVirtual then
            RaiseMsg(20190116215823,nInvalidXModifierY,sInvalidXModifierY,[ObjKindNames[ObjKind]+' '+GetElementTypeName(Proc),'virtual'],Proc);
          if Proc.IsOverride then
            RaiseMsg(20190116215825,nInvalidXModifierY,sInvalidXModifierY,[ObjKindNames[ObjKind]+' '+GetElementTypeName(Proc),'override'],Proc);
          end
        else if Proc.IsOverride and (TPasClassType(Proc.Parent).AncestorType=nil) then
          RaiseMsg(20260620100001,nInvalidXModifierY,sInvalidXModifierY,[ObjKindNames[ObjKind]+' '+GetElementTypeName(Proc),'override'],Proc);
        if IsClassConDestructor then
          RaiseMsg(20260225100001,nXIsNotSupported,sXIsNotSupported,
            ['class '+GetElementTypeName(Proc)+' in '+ObjKindNames[ObjKind]],Proc);
        if Proc.IsMessage then
          if not (msDelphi in CurrentParser.CurrentModeswitches) then
            RaiseMsg(20260225100002,nXIsNotSupported,sXIsNotSupported,
              ['message method in '+ObjKindNames[ObjKind]],Proc);
        HelperForType:=ResolveAliasType(TPasClassType(Proc.Parent).HelperForType);
        if (not Proc.IsStatic) and IsClassMethod(Proc) and not IsClassConDestructor then
          begin
          // non static class methods require a class
          if (not (HelperForType.ClassType=TPasClassType))
              or (TPasClassType(HelperForType).ObjKind<>okClass) then
            RaiseMsg(20190201153831,nClassMethodsMustBeStaticInX,sClassMethodsMustBeStaticInX,[ObjKindNames[ObjKind]],Proc);
          end;
        if Proc.ClassType=TPasDestructor then
          RaiseMsg(20190302151019,nXIsNotSupported,sXIsNotSupported,['destructor'],Proc);
        if (Proc.ClassType=TPasConstructor) then
          begin
          if (HelperForType.ClassType=TPasClassType)
              and (TPasClassType(HelperForType).ObjKind<>okClass) then
            RaiseMsg(20190302151514,nXIsNotSupported,sXIsNotSupported,['constructor'],Proc)
          else if not (HelperForType.ClassType=TPasClassType) then
            begin
            // Type/record helper constructor: must have required params
            HasRequired:=false;
            for i:=0 to Proc.ProcType.Args.Count-1 do
              if TPasArgument(Proc.ProcType.Args[i]).ValueExpr=nil then
                begin HasRequired:=true; break; end;
            if MaximizeFPCCompatibility and not HasRequired then
              RaiseMsg(20260225100003,nXIsNotSupported,sXIsNotSupported,
                ['parameterless constructor in '+ObjKindNames[ObjKind]],Proc);
            end;
          end;
        end;
      end;
      if Proc.IsAbstract then
        begin
        if not Proc.IsVirtual and not Proc.IsDynamic then
          // dynamic is virtual-with-DMT; `dynamic;abstract` is valid (cg/tcalcla1)
          RaiseMsg(20170216151623,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'abstract without virtual'],Proc);
        if Proc.IsOverride then
          RaiseMsg(20170216151625,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'abstract, override'],Proc);
        end;
      if (pmFinal in Proc.Modifiers) and not Proc.IsVirtual and not Proc.IsOverride then
        RaiseMsg(20260225100013,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'final without virtual'],Proc);
      if Proc.IsVirtual and Proc.IsOverride then
        RaiseMsg(20170216151627,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'virtual, override'],Proc);
      if Proc.IsReintroduced and Proc.IsOverride then
        RaiseMsg(20171119111845,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'reintroduce, override'],Proc);
      if Proc.IsForward then
        RaiseMsg(20170216151629,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'forward'],Proc);
      if Proc.IsStatic then
        if (Proc.ClassType<>TPasClassProcedure) and (Proc.ClassType<>TPasClassFunction)
            and (ObjKind<>okObject) then
          RaiseMsg(20170216151631,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'static'],Proc);
      end
    else if Proc.Parent is TPasRecordType then
      begin
      if (Proc.ClassType=TPasConstructor)
          and ((El.Args.Count=0)
            or (TPasArgument(El.Args[0]).ValueExpr<>nil)) then
        RaiseMsg(20181226231333,nParameterlessConstructorsNotAllowedInRecords,
          sParameterlessConstructorsNotAllowedInRecords,[],El);
      if Proc.IsReintroduced then
        RaiseMsg(20181218195735,nInvalidXModifierY,sInvalidXModifierY,['record '+GetElementTypeName(Proc),'reintroduce'],Proc);
      if Proc.IsVirtual then
        RaiseMsg(20181218195431,nInvalidXModifierY,sInvalidXModifierY,['record '+GetElementTypeName(Proc),'virtual'],Proc);
      if Proc.IsOverride then
        RaiseMsg(20181218195437,nInvalidXModifierY,sInvalidXModifierY,['record '+GetElementTypeName(Proc),'override'],Proc);
      if Proc.IsAbstract then
        RaiseMsg(20181218195552,nInvalidXModifierY,sInvalidXModifierY,['record '+GetElementTypeName(Proc),'abstract'],Proc);
      if Proc.IsForward then
        RaiseMsg(20181218195514,nInvalidXModifierY,sInvalidXModifierY,['record '+GetElementTypeName(Proc),'forward'],Proc);
      if IsClassMethod(Proc) and not IsClassConDestructor and not (Proc is TPasOperator) then
        begin
        // Note: class constructor/destructor must not be static
        if not Proc.IsStatic then
          RaiseMsg(20190106121503,nClassMethodsMustBeStaticInX,sClassMethodsMustBeStaticInX,['records'],Proc);
        end
      else if Proc.IsStatic then
        RaiseMsg(20190206150922,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(Proc),'static'],Proc);
      end
    else
      begin
      // intf proc, forward proc, proc body, method body, anonymous proc
      if Proc.IsAbstract then
        RaiseInvalidProcModifier(20170216151634,Proc,pmAbstract,Proc);
      if Proc.IsVirtual then
        RaiseInvalidProcModifier(20170216151635,Proc,pmVirtual,Proc);
      if Proc.IsOverride then
        RaiseInvalidProcModifier(20170216151637,Proc,pmOverride,Proc);
      if Proc.IsMessage then
        RaiseInvalidProcModifier(20170216151638,Proc,pmMessage,Proc);
      if Proc.IsStatic and not HasDots then
        RaiseInvalidProcTypeModifier(20170216151640,El,ptmStatic,El);
      // ppcx64 rejects exactly what its own message names: "constructors,
      // destructors and class operators must be method". A unit-level
      // `class procedure` / `class function` is ACCEPTED - the `class` simply
      // has no meaning there (vcl-compat writes one).
      if (not HasDots)
          and (Proc.GetProcTypeEnum in [
               ptClassOperator,
               ptConstructor, ptDestructor,
               ptClassConstructor, ptClassDestructor
               ]) then
        RaiseXExpectedButYFound(20170419232724,'full method name','short name',El);
      end;

    ProcScope.GenericStep:=psgsInterfaceParsed;

    if HasDots then
      begin
      FinishMethodImplHeader(Proc);
      exit;
      end;

    // finish interface/implementation/nested procedure/method declaration

    if (ProcName='')
        and not (Proc.GetProcTypeEnum in [ptAnonymousProcedure,ptAnonymousFunction]) then
      RaiseNotYetImplemented(20160922163407,El);

    if (El is TPasFunctionType) and not (ppsfIsSpecialized in ProcScope.Flags) then
      EmitTypeHints(TPasFunctionType(El).ResultEl,TPasFunctionType(El).ResultEl.ResultType);

    // Validate operator declarations
    if (Proc is TPasOperator) and not (ppsfIsSpecialized in ProcScope.Flags) then
      begin
      // A global (non-class) Implicit operator is not overloadable in FPC — use
      // `operator :=` for that. But a global Explicit operator IS allowed (it
      // backs an explicit typecast, e.g. Integer(foo)); only reject Implicit here.
      if (TPasOperator(Proc).OperatorType = otImplicit) and
         not (Proc is TPasClassOperator) then
        RaiseMsg(20170216151720,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [ObjKindNames[okClass]+' '+OperatorNames[TPasOperator(Proc).OperatorType],'operator'],Proc);
      CheckOperatorOverloadable(TPasOperator(Proc));
      end;

    // For operators, CorrectName changes the name after initial scope registration.
    // Re-add with the corrected name so forward matching and cross-unit lookup work.
    if (Proc is TPasOperator) and (Proc.Name <> '') then
      begin
      ParentScope:=GetParentLocalScope as TPasIdentifierScope;
      AddIdentifier(ParentScope, Proc.Name, Proc, pikProc);
      end;

    if Proc.PublicName<>nil then
      ResolveExpr(Proc.PublicName,rraRead);
    if Proc.LibraryExpr<>nil then
      ResolveExpr(Proc.LibraryExpr,rraRead);
    if Proc.LibrarySymbolName<>nil then
      ResolveExpr(Proc.LibrarySymbolName,rraRead);
    if Proc.DispIDExpr<>nil then
      ResolveExpr(Proc.DispIDExpr,rraRead);
    if Proc.MessageExpr<>nil then
      begin
      // message modifier
      ResolveExpr(Proc.MessageExpr,rraRead);
      Args:=Proc.ProcType.Args;
      if Args.Count<>1 then
        RaiseMsg(20190303223701,nMessageHandlersInvalidParams,sMessageHandlersInvalidParams,[],El);
      Arg:=TPasArgument(Args[0]);
      if not (Arg.Access in [argVar,argOut]) then
        RaiseMsg(20190303223834,nMessageHandlersInvalidParams,sMessageHandlersInvalidParams,[],El);
      if (Proc.ClassType<>TPasProcedure)
          and (Proc.ClassType<>TPasFunction) then
        RaiseMsg(20190303224128,nXExpectedButYFound,sXExpectedButYFound,['procedure name(var Msg);message id;',GetElementTypeName(El)],El);
      end;

    if Proc.Parent is TPasMembersType then
      begin
      FinishMethodDeclHeader(Proc);
      exit;
      end;

    // finish interface/implementation/nested procedure
    if (ProcName<>'') and ProcNeedsBody(Proc) then
      begin
      if ppsfIsSpecialized in ProcScope.Flags then
        begin
        if ProcScope.DeclarationProc<>nil then
          ReplaceProcScopeImplArgsWithDeclArgs(ProcScope);
        end
      else
        begin
        // check if there is a forward declaration
        //writeln('TPasResolver.FinishProcedureType ',GetObjName(TopScope),' ',GetObjName(Scopes[ScopeCount-2]));
        ParentScope:=GetParentLocalScope as TPasIdentifierScope;
        //writeln('TPasResolver.FinishProcedureType FindForward2 ParentScope=',GetObjName(ParentScope),'=',GetObjName(ParentScope.Element),' Proc=',GetObjName(Proc),' at ',GetElementSourcePosStr(Proc));
        DeclProc:=FindProcSameSignature(ProcName,Proc,ParentScope,true);
        //writeln('TPasResolver.FinishProcedureType FindForward3 DeclProc=',GetObjName(DeclProc),' Proc.Parent=',GetObjName(Proc.Parent));
        //if DeclProc<>nil then writeln('TPasResolver.FinishProcedureType DeclProc at ',GetElementSourcePosStr(DeclProc));
        if (DeclProc=nil) and (Proc.Parent.ClassType=TImplementationSection) then
          DeclProc:=FindProcSameSignature(ProcName,Proc,
            (Proc.GetModule.InterfaceSection.CustomData) as TPasIdentifierScope,true);
        // An implementation that omits the parameter list (0 args) matches its
        // forward/interface declaration by name — adopt its arguments (treg1).
        // A GENERIC proc's empty value-argument list is NOT an omission (its
        // parameters are type parameters), so it must not be matched by name to a
        // non-generic forward — that would pair e.g. `generic procedure proc<T>`
        // with a separate `procedure proc` and wrongly report they differ. Such a
        // generic proc is a distinct overload; leave DeclProc nil (GitLab #40819).
        if (DeclProc=nil) and (Proc.ProcType.Args.Count=0)
            and (Proc.ProcType is TPasProcedureType)
            and (GetProcTemplateTypes(Proc)=nil) then
          begin
          DeclProc:=FindSoleUnimplementedForward(ProcName,ParentScope,Proc);
          if (DeclProc=nil) and (Proc.Parent.ClassType=TImplementationSection) then
            DeclProc:=FindSoleUnimplementedForward(ProcName,
              (Proc.GetModule.InterfaceSection.CustomData) as TPasIdentifierScope,Proc);
          end;
        //writeln('TPasResolver.FinishProcedureType FindForward4 ',GetObjName(DeclProc),' at ',GetElementSourcePosStr(DeclProc));
        if (DeclProc<>nil) then
          begin
          if ProcNeedsImplProc(DeclProc) then
            begin
            // found forward declaration
            DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;
            if DeclProcScope.ImplProc<>nil then
              RaiseMsg(20180318222430,nDuplicateIdentifier,sDuplicateIdentifier,
                [DeclProcScope.ImplProc.Name,GetElementSourcePosStr(DeclProcScope.ImplProc)],Proc);
            // connect
            {$IFDEF VerbosePasResolver}
            writeln('TPasResolver.FinishProcedureHeader forward found: Proc2=',GetTreeDbg(DeclProc),' ',GetElementSourcePosStr(DeclProc),' IsForward=',DeclProc.IsForward,' Parent=',GetObjName(DeclProc.Parent));
            {$ENDIF}
            CheckProcSignatureMatch(DeclProc,Proc,false);
            DeclProcScope.ImplProc:=Proc;
            if DeclProc.IsAssembler then
              Proc.Modifiers:=Proc.Modifiers+[pmAssembler];
            ProcScope.DeclarationProc:=DeclProc;
            // remove ImplProc from scope
            ParentScope.RemoveLocalIdentifier(Proc);
            // replace arguments with declaration arguments
            ReplaceProcScopeImplArgsWithDeclArgs(ProcScope);
            exit;
            end
          else
            RaiseMsg(20180318220543,nDuplicateIdentifier,sDuplicateIdentifier,
              [DeclProc.Name,GetElementSourcePosStr(DeclProc)],Proc);
          end;
        end;
      end
    else
      begin
      // An external proc in the implementation section completes an interface
      // forward declaration (the external directive provides the body). Without
      // this, `procedure p; external name '..';` in the implementation is treated
      // as a second forward and clashes with the interface decl ("Duplicate
      // identifier"). Mirror the body-proc forward-completion above.
      if (ProcName<>'') and Proc.IsExternal
          and (Proc.Parent.ClassType=TImplementationSection)
          and (Proc.GetModule.InterfaceSection<>nil) then
        begin
        DeclProc:=FindProcSameSignature(ProcName,Proc,
          (Proc.GetModule.InterfaceSection.CustomData) as TPasIdentifierScope,true);
        if (DeclProc<>nil) and ProcNeedsImplProc(DeclProc) then
          begin
          DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;
          if DeclProcScope.ImplProc<>nil then
            RaiseMsg(20240630120000,nDuplicateIdentifier,sDuplicateIdentifier,
              [DeclProcScope.ImplProc.Name,GetElementSourcePosStr(DeclProcScope.ImplProc)],Proc);
          CheckProcSignatureMatch(DeclProc,Proc,false);
          DeclProcScope.ImplProc:=Proc;
          ProcScope.DeclarationProc:=DeclProc;
          ParentScope:=GetParentLocalScope as TPasIdentifierScope;
          ParentScope.RemoveLocalIdentifier(Proc);
          ReplaceProcScopeImplArgsWithDeclArgs(ProcScope);
          exit;
          end;
        end;
      // forward declaration
      // ToDo: store the scanner flags *before* it has parsed the token after the proc
      StoreScannerFlagsInProc(ProcScope);
      end;

    if ProcName<>'' then
      begin
      // check for invalid overloads
      FindData:=Default(TFindProcData);
      FindData.Proc:=Proc;
      FindData.Args:=Proc.ProcType.Args;
      FindData.Kind:=fpkProc;
      Abort:=false;
      IterateElements(ProcName,@OnFindProc,@FindData,Abort);
      end;
    end
  else if El.Name<>'' then
    begin
    // finished proc type, e.g. type TProcedure = procedure;
    end;
end;

procedure TPasResolver.FinishMethodDeclHeader(Proc: TPasProcedure);

  procedure VisibilityLowered(Proc, OverloadProc: TPasProcedure);
  begin
    LogMsg(20170325004215,mtNote,nVirtualMethodXHasLowerVisibility,
      sVirtualMethodXHasLowerVisibility,[Proc.Name,
        VisibilityNames[Proc.Visibility],OverloadProc.Parent.Name,
        VisibilityNames[OverloadProc.Visibility]],Proc);
    Proc.Visibility:=OverloadProc.Visibility;
  end;

  {$IF defined(fpc) and (FPC_FULLVERSION<30101)}
  procedure Delete(var A: TArrayOfPasProcedure; Index, Count: integer); overload;
  var
    i: Integer;
  begin
    if Index<0 then
      RaiseInternalError(20171227121538);
    if Index+Count>length(A) then
      RaiseInternalError(20171227121156);
    for i:=Index+Count to length(A)-1 do
      A[i-Count]:=A[i];
    SetLength(A,length(A)-Count);
  end;

  procedure Insert(Item: TPasProcedure; var A: TArrayOfPasProcedure; Index: integer); overload;
  var
    i: Integer;
  begin
    if Index<0 then
      RaiseInternalError(20171227121544);
    if Index>length(A) then
      RaiseInternalError(20171227121558);
    SetLength(A,length(A)+1);
    for i:=length(A)-1 downto Index+1 do
      A[i]:=A[i-1];
    A[Index]:=Item;
  end;
  {$ENDIF}

var
  Abort, IsClassConDestructor: boolean;
  ClassOrRecScope: TPasClassOrRecordScope;
  FindData: TFindProcData;
  OverloadProc: TPasProcedure;
  ProcScope: TPasProcedureScope;
  i: Integer;
  ParentScope: TPasScope;
  TemplTypes: TFPList;
  ClassRecType: TPasMembersType;
begin
  if not (ptmStatic in Proc.ProcType.Modifiers) then
    Proc.ProcType.IsOfObject:=true;
  ProcScope:=TopScope as TPasProcedureScope;
  ParentScope:=Scopes[ScopeCount-2];
  // ToDo: store the scanner flags *before* it has parsed the token after the proc
  StoreScannerFlagsInProc(ProcScope);
  ClassRecType:=TPasMembersType(Proc.Parent);
  ClassOrRecScope:=ClassRecType.CustomData as TPasClassOrRecordScope;
  ProcScope.ClassRecScope:=ClassOrRecScope;

  TemplTypes:=GetProcTemplateTypes(Proc);

  FindData:=Default(TFindProcData);
  IsClassConDestructor:=(Proc.ClassType=TPasClassConstructor)
                     or (Proc.ClassType=TPasClassDestructor);
  if IsClassConDestructor then
    begin
    if TemplTypes<>nil then
      RaiseNotYetImplemented(20190911105953,Proc);
    end
  else
    begin
    FindData.Proc:=Proc;
    FindData.Args:=Proc.ProcType.Args;
    FindData.Kind:=fpkMethod;
    Abort:=false;
    ParentScope.IterateElements(Proc.Name,ClassOrRecScope,
                                @OnFindProc,@FindData,Abort);
    end;

  if FindData.Found=nil then
    begin
    // no overload
    if Proc.IsOverride then
      begin
      // A class whose ancestor is a class-constrained generic template parameter
      // has its inherited methods in the still-unresolved T; defer the override
      // check to specialization instead of raising (tgeneric116).
      if (ClassOrRecScope is TPasClassScope)
          and (pcsfDeferredAncestor in TPasClassScope(ClassOrRecScope).Flags) then
        // defer override check to specialization
      else
        RaiseMsg(20170216151702,nNoMethodInAncestorToOverride,
          sNoMethodInAncestorToOverride,[GetProcTypeDescription(Proc.ProcType)],Proc.ProcType);
      end;
    end
  else
    begin
    // overload found
    OverloadProc:=FindData.Found;

    // Note: 'inherited;' needs the OverriddenProc, even without 'override' modifier
    ProcScope.OverriddenProc:=OverloadProc;

    if Proc.IsOverride then
      begin
      if (not OverloadProc.IsVirtual) and (not OverloadProc.IsOverride)
          and (not OverloadProc.IsDynamic) then
        // the OverloadProc fits the signature, but is not virtual/dynamic
        // (a 'dynamic' method is virtual via the DMT and can be overridden)
        RaiseMsg(20170216151708,nNoMethodInAncestorToOverride,
          sNoMethodInAncestorToOverride,[GetProcTypeDescription(Proc.ProcType)],Proc.ProcType);
      if pmFinal in OverloadProc.Modifiers then
        RaiseMsg(20260225100014,nInvalidXModifierY,sInvalidXModifierY,
          [GetElementTypeName(Proc),'override of final method'],Proc);
      // override a virtual method
      CheckProcSignatureMatch(OverloadProc,Proc,true);
      // check visibility
      if Proc.Visibility<>OverloadProc.Visibility then
        case Proc.Visibility of
          visPrivate,visStrictPrivate:
            if not (OverloadProc.Visibility in [visPrivate,visStrictPrivate]) then
              VisibilityLowered(Proc,OverloadProc);
          visProtected,visStrictProtected:
            if not (OverloadProc.Visibility in [visPrivate,visProtected,visStrictPrivate,visStrictProtected]) then
              VisibilityLowered(Proc,OverloadProc);
          visPublic:
            if not (OverloadProc.Visibility in [visPrivate..visPublic,visStrictPrivate,visStrictProtected]) then
              VisibilityLowered(Proc,OverloadProc);
          visPublished: ;
        else
          RaiseNotYetImplemented(20170325003315,Proc,'visibility');
        end;
      // check name case
      if proFixCaseOfOverrides in Options then
        Proc.Name:=OverloadProc.Name;
      // remove abstract
      if OverloadProc.IsAbstract and (ClassOrRecScope is TPasClassScope) then
        for i:=length(TPasClassScope(ClassOrRecScope).AbstractProcs)-1 downto 0 do
          if TPasClassScope(ClassOrRecScope).AbstractProcs[i]=OverloadProc then
            Delete(TPasClassScope(ClassOrRecScope).AbstractProcs,i,1);
      // check inherited "overload"
      if IsProcOverload(OverloadProc) then
        Include(ProcScope.Flags,ppsfIsOverrideOverload);
      end;
    end;
  // add abstract
  if Proc.IsAbstract and (ClassOrRecScope is TPasClassScope) then
    Insert(Proc,TPasClassScope(ClassOrRecScope).AbstractProcs,
           length(TPasClassScope(ClassOrRecScope).AbstractProcs));

  CreateProcSelfArg(Proc);
end;

procedure TPasResolver.FinishMethodImplHeader(ImplProc: TPasProcedure);
var
  ProcName: String;
  ImplProcScope, DeclProcScope: TPasProcedureScope;
  DeclProc: TPasProcedure;
  ClassOrRecScope: TPasClassOrRecordScope;
  SelfArg: TPasArgument;
  LastNamePart: TProcedureNamePart;
  OldSelfIdent: TPasIdentifier;
begin
  if ImplProc.IsExternal then
    RaiseMsg(20170216151715,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(ImplProc),'external'],ImplProc);
  if ImplProc.IsExported then
    RaiseMsg(20170216151717,nInvalidXModifierY,sInvalidXModifierY,[GetElementTypeName(ImplProc),'export'],ImplProc);

  ProcName:=ImplProc.Name;
  ImplProcScope:=ImplProc.CustomData as TPasProcedureScope;

  ClassOrRecScope:=ImplProcScope.ClassRecScope;
  if ClassOrRecScope=nil then
    RaiseInternalError(20161013172346);
  if ImplProcScope.GroupScope=nil then
    RaiseInternalError(20190120135017);

  if ImplProc.NameParts<>nil then
    begin
    LastNamePart:=TProcedureNamePart(ImplProc.NameParts[ImplProc.NameParts.Count-1]);
    ProcName:=LastNamePart.Name;
    end
  else
    begin
    // remove path from ProcName
    ProcName:=LastDottedIdentifier(ProcName);
    end;

  DeclProc:=nil;
  DeclProcScope:=nil;
  if ImplProcScope.DeclarationProc=nil then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishMethodImplHeader searching declaration "',ImplProc.Name,'" ...');
    {$ENDIF}
    // search ImplProc in class
    // Note: operator names include signature suffix e.g. 'implicit(TTest):Integer'
    if not IsValidIdent(ProcName) and not (ImplProc is TPasOperator) then
      RaiseNotYetImplemented(20160922163421,ImplProc.ProcType);

    // search proc in class/record
    if ImplProc.ClassType=TPasClassConstructor then
      DeclProc:=ClassOrRecScope.ClassConstructor
    else if ImplProc.ClassType=TPasClassDestructor then
      DeclProc:=ClassOrRecScope.ClassDestructor
    else
      DeclProc:=FindProcSameSignature(ProcName,ImplProc,ClassOrRecScope,false);
    if DeclProc=nil then
      RaiseIdentifierNotFound(20170216151720,GetProcName(ImplProc),ImplProc.ProcType);
    DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;
    ImplProc.ProcType.IsOfObject:=DeclProc.ProcType.IsOfObject;

    // connect method declaration and body
    if DeclProcScope.ImplProc<>nil then
      RaiseMsg(20180212094546,nDuplicateIdentifier,sDuplicateIdentifier,
        [DeclProcScope.ImplProc.Name,GetElementSourcePosStr(DeclProcScope.ImplProc)],
        ImplProc);
    if DeclProc.IsAbstract then
      RaiseMsg(20170216151722,nAbstractMethodsMustNotHaveImplementation,sAbstractMethodsMustNotHaveImplementation,[],ImplProc);
    if DeclProc.IsExternal then
      RaiseXExpectedButYFound(20170216151725,'method','external method',ImplProc);
    CheckProcSignatureMatch(DeclProc,ImplProc,false);
    if DeclProc.IsAssembler then
      ImplProc.Modifiers:=ImplProc.Modifiers+[pmAssembler];
    ImplProcScope.DeclarationProc:=DeclProc;
    DeclProcScope.ImplProc:=ImplProc;

    // replace arguments in scope with declaration arguments
    ReplaceProcScopeImplArgsWithDeclArgs(ImplProcScope);
    end
  else if ppsfIsSpecialized in ImplProcScope.Flags then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishMethodImplHeader specialized "',ImplProc.Name,'" ...');
    {$ENDIF}
    DeclProc:=ImplProcScope.DeclarationProc;
    DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;
    if DeclProcScope.ImplProc<>ImplProc then
      RaiseNotYetImplemented(20190804182220,ImplProc);
    // replace arguments in scope with declaration arguments
    ReplaceProcScopeImplArgsWithDeclArgs(ImplProcScope);
    end
  else
    RaiseNotYetImplemented(20190804181222,ImplProc);

  SelfArg:=DeclProcScope.SelfArg;
  if SelfArg<>nil then
    begin
    // add 'Self'
    ImplProcScope.SelfArg:=SelfArg;
    if AllowArgumentNamedSelf then
      begin
      // a parameter named Self is hidden by the instance
      OldSelfIdent:=ImplProcScope.FindLocalIdentifier('Self');
      if (OldSelfIdent<>nil) and (OldSelfIdent.Element is TPasArgument) then
        ImplProcScope.RemoveLocalIdentifier(OldSelfIdent.Element);
      end;
    AddIdentifier(ImplProcScope,'Self',SelfArg,pikSimple);
    end;

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishMethodImplHeader END "',ImplProc.Name,'" ...');
  {$ENDIF}
end;

procedure TPasResolver.FinishExceptOnExpr;
var
  El: TPasElement;
  ResolvedType: TPasResolverResult;
  TypeEl: TPasType;
begin
  CheckTopScope(TPasExceptOnScope);
  El:=FTopScope.Element;
  if El is TTryExceptExprOn then
    TypeEl:=TTryExceptExprOn(El).TypeEl
  else
    TypeEl:=TPasImplExceptOn(El).TypeEl;
  ComputeElement(TypeEl,ResolvedType,[rcType]);
  CheckIsClass(TypeEl,ResolvedType);
end;

procedure TPasResolver.FinishExceptOnStatement;
var
  El: TPasElement;
begin
  //writeln('TPasResolver.FinishExceptOnStatement START');
  CheckTopScope(TPasExceptOnScope);
  El:=FTopScope.Element;
  if El is TTryExceptExprOn then
    // on-branch of a try-except-expression, resolved while E is in scope
    ResolveExpr(TTryExceptExprOn(El).Value,rraRead)
  else
    ResolveImplElement(TPasImplExceptOn(El).Body);
  PopScope;
end;

procedure TPasResolver.FinishParserSpecializeType(El: TPasSpecializeType);
begin
  if El=nil then ;
  PopScope;
end;

procedure TPasResolver.FinishWithDo(El: TPasImplWithDo);
begin
  PopWithScope(El);
end;

procedure TPasResolver.FinishForLoopHeader(Loop: TPasImplForLoop);
var
  VarResolved, StartResolved, EndResolved,
    OrigStartResolved, ElResolved: TPasResolverResult;
  EnumeratorFound, HasInValues: Boolean;
  InRange, VarRange: TResEvalValue;
  InRangeInt, VarRangeInt: TResEvalRangeInt;
  bt: TResolverBaseType;
  TypeEl, ElType: TPasType;
  C: TClass;
  IdentEl: TPasElement;
  FlattenDepth, i: Integer;
begin
  CreateScope(Loop,TPasForLoopScope);

  // loop var
  ResolveExpr(Loop.VariableName,rraReadAndAssign);
  ComputeElement(Loop.VariableName,VarResolved,[rcNoImplicitProc,rcSetReferenceFlags]);
  if not ResolvedElCanBeVarParam(VarResolved,Loop.VariableName) then
    RaiseVarExpected(20170216151955,Loop.VariableName,VarResolved.IdentEl);
  IdentEl:=VarResolved.IdentEl;
  C:=IdentEl.ClassType;
  if (C=TPasArgument)
    or (C=TPasResultElement)
    or ((C=TPasVariable) and ((IdentEl.Parent is TProcedureBody) or (IdentEl.Parent is TPasSection))) then
    // loop var is simple local var
  else
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishForLoopHeader ',GetResolverResultDbg(VarResolved),' IdentEl=',GetObjPath(IdentEl));
    {$ENDIF}
    RaiseMsg(20220815164902,nForLoopControlVarMustBeSimpleLocalVar,sForLoopControlVarMustBeSimpleLocalVar,[],Loop.VariableName);
    end;

  // resolve start expression
  ResolveExpr(Loop.StartExpr,rraRead);
  ComputeElement(Loop.StartExpr,StartResolved,[rcSetReferenceFlags]);

  case Loop.LoopType of
  ltNormal,ltDown:
    begin
    // start value
    if CheckAssignResCompatibility(VarResolved,StartResolved,Loop.StartExpr,true)=cIncompatible then
      RaiseIncompatibleTypeRes(20170216151958,nIncompatibleTypesGotExpected,
        [],StartResolved,VarResolved,Loop.StartExpr);
    CheckAssignExprRange(VarResolved,Loop.StartExpr);

    // end value
    ResolveExpr(Loop.EndExpr,rraRead);
    ComputeElement(Loop.EndExpr,EndResolved,[rcSetReferenceFlags]);
    if CheckAssignResCompatibility(VarResolved,EndResolved,Loop.EndExpr,false)=cIncompatible then
      RaiseIncompatibleTypeRes(20170216152001,nIncompatibleTypesGotExpected,
        [],EndResolved,VarResolved,Loop.EndExpr);
    CheckAssignExprRange(VarResolved,Loop.EndExpr);
    end;
  ltIn:
    begin
    // check range
    EnumeratorFound:=CheckForIn(Loop,VarResolved,StartResolved);
    if (not EnumeratorFound)
        and not (StartResolved.IdentEl is TPasType)
        and (rrfReadable in StartResolved.Flags) then
      begin
      EnumeratorFound:=CheckForInClassOrRec(Loop,VarResolved,StartResolved);
      end;

    if not EnumeratorFound then
      begin
      VarRange:=nil;
      InRange:=nil;
      try
        OrigStartResolved:=StartResolved;
        if StartResolved.IdentEl is TPasType then
          begin
          // e.g. for e in TEnum do
          TypeEl:=StartResolved.LoTypeEl;
          // An enum with holes (non-contiguous values) cannot be iterated by a
          // for-in loop (FPC parser_e_for_in_loop_cannot_be_used_for_the_type). tforin20.
          if (TypeEl is TPasEnumType) and EnumHasHoles(TPasEnumType(TypeEl)) then
            RaiseMsg(20260623110000,nCannotFindEnumeratorForType,
              sCannotFindEnumeratorForType,[GetTypeDescription(TypeEl)],Loop.StartExpr);
          if TypeEl is TPasArrayType then
            begin
            if length(TPasArrayType(TypeEl).Ranges)=1 then
              InRange:=Eval(TPasArrayType(TypeEl).Ranges[0],[refConst]);
            end;
          if InRange=nil then
            InRange:=EvalTypeRange(TypeEl,[]);
          {$IFDEF VerbosePasResolver}
          {AllowWriteln}
          if InRange<>nil then
            writeln('TPasResolver.ResolveImplForLoop in type: InRange=',InRange.AsDebugString)
          else
            writeln('TPasResolver.ResolveImplForLoop in type: InRange=nil');
          {AllowWriteln-}
          {$ENDIF}
          end
        else if rrfReadable in StartResolved.Flags then
          begin
          // value  (variable or expression)
          bt:=StartResolved.BaseType;
          if bt in [btSet,btArrayOrSet] then
            begin
            if (StartResolved.IdentEl=nil) and (StartResolved.ExprEl<>nil) then
              InRange:=Eval(StartResolved.ExprEl,[]);
            if InRange=nil then
              InRange:=EvalTypeRange(StartResolved.LoTypeEl,[]);
            end
          else if (bt=btArrayLit) and (StartResolved.ExprEl is TParamsExpr)
              and (TParamsExpr(StartResolved.ExprEl).Kind=pekSet) then
            begin
            // for s in ['a', b, c] do: an array constructor of non-ordinal
            // values (strings) - every element must fit the loop variable.
            for i:=0 to length(TParamsExpr(StartResolved.ExprEl).Params)-1 do
              begin
              ComputeElement(TParamsExpr(StartResolved.ExprEl).Params[i],ElResolved,[]);
              if CheckAssignResCompatibility(VarResolved,ElResolved,
                  TParamsExpr(StartResolved.ExprEl).Params[i],true)=cIncompatible then
                RaiseIncompatibleTypeRes(20261001120000,nIncompatibleTypesGotExpected,
                  [],ElResolved,VarResolved,TParamsExpr(StartResolved.ExprEl).Params[i]);
              end;
            EnumeratorFound:=true;
            end
          else if bt=btContext then
            begin
            TypeEl:=StartResolved.LoTypeEl;
            C:=TypeEl.ClassType;
            if C=TPasArrayType then
              begin
              // for x in <array>. The loop var normally matches the array's
              // element type. When it matches a DEEPER element type instead, the
              // loop flattens the leading array dimensions (multi-dimensional
              // for-in). Peel element types (static or dynamic) until the loop var
              // is assignment-compatible, recording the flatten depth for codegen.
              ElType:=GetArrayElType(TPasArrayType(TypeEl));
              ComputeElement(ElType,ElResolved,[rcType]);
              ElResolved.Flags:=OrigStartResolved.Flags*[rrfReadable,rrfWritable];
              FlattenDepth:=1;
              while (CheckAssignResCompatibility(VarResolved,ElResolved,Loop.StartExpr,false)=cIncompatible)
                  and (ElResolved.LoTypeEl is TPasArrayType) do
                begin
                ElType:=GetArrayElType(TPasArrayType(ElResolved.LoTypeEl));
                ComputeElement(ElType,ElResolved,[rcType]);
                ElResolved.Flags:=OrigStartResolved.Flags*[rrfReadable,rrfWritable];
                Inc(FlattenDepth);
                end;
              StartResolved:=ElResolved;
              if CheckAssignResCompatibility(VarResolved,StartResolved,Loop.StartExpr,true)=cIncompatible then
                RaiseIncompatibleTypeRes(20171112210138,nIncompatibleTypesGotExpected,
                  [],StartResolved,VarResolved,Loop.StartExpr);
              (Loop.CustomData as TPasForLoopScope).ForInFlattenDepth:=FlattenDepth;
              EnumeratorFound:=true;
              end;
            end
          else
            begin
            bt:=GetActualBaseType(bt);
            case bt of
            {$ifdef FPC_HAS_CPSTRING}
            btAnsiString:
              InRange:=TResEvalRangeInt.CreateValue(revskChar,nil,0,$ff);
            btShortString:
              // native ShortString/OpenString: elements are AnsiChar (0..$ff)
              InRange:=TResEvalRangeInt.CreateValue(revskChar,nil,0,$ff);
            {$ENDIF}              
            btUnicodeString,btWideString:
              // both are 2-byte char strings; for-in yields WideChar (0..$ffff).
              // btWideString was previously omitted -> "cannot find enumerator for
              // the type WideString" (unix sysutils TStringHelper on WideString).
              InRange:=TResEvalRangeInt.CreateValue(revskChar,nil,0,$ffff);
            end;
            end;
          end;
        if (not EnumeratorFound) and (InRange<>nil) then
          begin
          // for v in <constant> do
          // -> check if same type
          VarRange:=EvalTypeRange(VarResolved.LoTypeEl,[]);
          if VarRange=nil then
            RaiseXExpectedButYFound(20171109191528,'range',
                         GetResolverResultDescription(VarResolved),Loop.VariableName);
          //writeln('TPasResolver.ResolveImplForLoop ForIn VarRange=',VarRange.AsDebugString);
          //writeln('TPasResolver.ResolveImplForLoop ForIn InRange=',InRange.AsDebugString,' ElType=',GetResolverResultDbg(StartResolved));
          case InRange.Kind of
          revkRangeInt,revkSetOfInt:
            begin
            InRangeInt:=TResEvalRangeInt(InRange);
            case VarRange.Kind of
            revkRangeInt:
              begin
              VarRangeInt:=TResEvalRangeInt(VarRange);
              HasInValues:=(InRange.Kind<>revkSetOfInt) or (length(TResEvalSet(InRange).Ranges)>0);
              case InRangeInt.ElKind of
                revskEnum:
                  if (VarRangeInt.ElKind<>revskEnum)
                      or not IsSameType(InRangeInt.ElType,VarRangeInt.ElType,prraAlias) then
                    RaiseXExpectedButYFound(20171109200752,GetTypeDescription(InRangeInt.ElType),
                      GetResolverResultDescription(VarResolved,true),loop.VariableName);
                revskInt:
                  if VarRangeInt.ElKind<>revskInt then
                    RaiseXExpectedButYFound(20171109200752,'integer',
                      GetResolverResultDescription(VarResolved,true),loop.VariableName);
                revskChar:
                  if VarRangeInt.ElKind<>revskChar then
                    RaiseXExpectedButYFound(20171109200753,'char',
                      GetResolverResultDescription(VarResolved,true),loop.VariableName);
                revskBool:
                  if VarRangeInt.ElKind<>revskBool then
                    RaiseXExpectedButYFound(20171109200754,'boolean',
                      GetResolverResultDescription(VarResolved,true),loop.VariableName);
              else
                if HasInValues then
                  RaiseNotYetImplemented(20171109200954,Loop.StartExpr);
              end;
              if HasInValues then
                begin
                if (VarRangeInt.RangeStart>InRangeInt.RangeStart) then
                  begin
                  {$IFDEF VerbosePasResolver}
                  writeln('TPasResolver.ResolveImplForLoop VarRange=',VarRangeInt.AsDebugString,' ',InRangeInt.AsDebugString);
                  {$ENDIF}
                  fExprEvaluator.EmitRangeCheckConst(20171109201428,
                    InRangeInt.ElementAsString(InRangeInt.RangeStart),
                    VarRangeInt.ElementAsString(VarRangeInt.RangeStart),
                    VarRangeInt.ElementAsString(VarRangeInt.RangeEnd),Loop.VariableName,mtError);
                  end;
                if (VarRangeInt.RangeEnd<InRangeInt.RangeEnd) then
                  begin
                  {$IFDEF VerbosePasResolver}
                  writeln('TPasResolver.ResolveImplForLoop VarRange=',VarRangeInt.AsDebugString,' ',InRangeInt.AsDebugString);
                  {$ENDIF}
                  fExprEvaluator.EmitRangeCheckConst(20171109201429,
                    InRangeInt.ElementAsString(InRangeInt.RangeEnd),
                    VarRangeInt.ElementAsString(VarRangeInt.RangeStart),
                    VarRangeInt.ElementAsString(VarRangeInt.RangeEnd),Loop.VariableName,mtError);
                  end;
                end;
              EnumeratorFound:=true;
              end;
            else
              {$IFDEF VerbosePasResolver}
              writeln('TPasResolver.ResolveImplForLoop ForIn VarRange=',VarRange.AsDebugString);
              {$ENDIF}
            end;
            end;
          else
            {$IFDEF VerbosePasResolver}
            writeln('TPasResolver.ResolveImplForLoop ForIn InRange=',InRange.AsDebugString);
            {$ENDIF}
          end;
          end;
        if not EnumeratorFound then
          begin
          {$IFDEF VerbosePasResolver}
          {AllowWriteln}
          writeln('TPasResolver.ResolveImplForLoop StartResolved=',GetResolverResultDbg(StartResolved));
          if VarRange<>nil then
            writeln('TPasResolver.ResolveImplForLoop VarRange=',VarRange.AsDebugString);
          {AllowWriteln-}
          {$ENDIF}
          RaiseMsg(20171108223818,nCannotFindEnumeratorForType,sCannotFindEnumeratorForType,
            [GetBaseDescription(OrigStartResolved)],Loop.StartExpr);
          end;
      finally
        ReleaseEvalValue(VarRange);
        ReleaseEvalValue(InRange);
      end;
      end;

    end;
  else
    RaiseNotYetImplemented(20171108221334,Loop);
  end;
end;

procedure TPasResolver.FinishDeclaration(El: TPasElement);
var
  C: TClass;
begin
  C:=El.ClassType;
  if (C=TPasVariable) or (C=TPasConst) then
    FinishVariable(TPasVariable(El))
  else if C=TPasProperty then
    FinishProperty(TPasProperty(El))
  else if C=TPasArgument then
    FinishArgument(TPasArgument(El))
  else if C=TPasMethodResolution then
    FinishMethodResolution(TPasMethodResolution(El))
  else if C=TPasAttributes then
    FinishAttributes(TPasAttributes(El))
  else if C=TPasExportSymbol then
    FinishExportSymbol(TPasExportSymbol(El))
  else if C=TPasContainsAlias then
    FinishContainsAlias(TPasContainsAlias(El))
  else
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishDeclaration ',GetObjName(El));
    {$ENDIF}
    RaiseNotYetImplemented(20180127121557,El);
    end;
end;

procedure TPasResolver.FinishVariable(El: TPasVariable);

  function InitializerNamesAProc(RHS: TPasExpr): Boolean;
  // True if RHS is the address of (or a bare reference to) a procedure/function —
  // i.e. `@Proc`, `Proc`, `@Type.Method` or `Type.Method`. Used to tell a
  // proc-address initializer apart from nil or another constant.
  var
    lOp: TPasExpr;
    lProcResolved: TPasResolverResult;
  begin
    Result:=false;
    lOp:=RHS;
    if (lOp is TUnaryExpr) and (TUnaryExpr(lOp).OpCode=eopAddress) then
      lOp:=TUnaryExpr(lOp).Operand;
    if lOp is TPrimitiveExpr then
      begin
      ComputeElement(lOp,lProcResolved,[rcNoImplicitProc]);
      Result:=lProcResolved.IdentEl is TPasProcedure;
      end
    else if (lOp is TBinaryExpr) and (TBinaryExpr(lOp).OpCode=eopSubIdent) then
      begin
      ComputeElement(TBinaryExpr(lOp).right,lProcResolved,[rcNoImplicitProc]);
      Result:=lProcResolved.IdentEl is TPasProcedure;
      end;
  end;

var
  ResolvedAbs: TPasResolverResult;
  C: TClass;
  Value: TResEvalValue;
  AbsBin: TBinaryExpr;
  BaseResolved: TPasResolverResult;
  GenWalk: TPasElement;
begin
  if (El.Visibility=visPublished) then
    begin
    // a constant in a published section has no RTTI, so it is harmless
    if (not (El is TPasConst))
        and ([vmClass,vmStatic,vmCVar]*El.VarModifiers<>[]) then
      RaiseMsg(20170403223837,nSymbolCannotBePublished,sSymbolCannotBePublished,[],El);
    end;
  if El.Expr<>nil then
    ResolveExpr(El.Expr,rraRead);
  if El.VarType<>nil then
    begin
    // An instance field of the enclosing record type is illegal (infinite size).
    // A class var / static field does not contribute to the instance layout, so
    // it may legally reference its owner type (terecs8).
    if (El.Parent is TPasRecordType) and (El.VarType=El.Parent)
        and (El.VarModifiers*[vmClass,vmStatic]=[]) then
      RaiseMsg(20181218173631,nTypeXIsNotYetCompletelyDefined,
        sTypeXIsNotYetCompletelyDefined,[El.VarType.Name],El);
    CheckUseAsType(El.VarType,20190123095916,El);
    if El.Expr<>nil then
      begin
      CheckAssignCompatibility(El,El.Expr,true);
      // A typed constant / initialized variable of a procedure type may only be
      // initialized with nil or the address of a GLOBAL procedure/function: a
      // nested proc needs a runtime frame pointer and an instance method a runtime
      // Self, neither available at load time (tmaclocalprocparam4f).
      // IsConstFoldableProcAddr accepts exactly the load-time-constant proc
      // addresses (global proc, @Type.Method), so anything naming a proc that it
      // rejects is illegal here.
      if (ResolveAliasType(El.VarType) is TPasProcedureType)
          and InitializerNamesAProc(El.Expr)
          and not IsConstFoldableProcAddr(El.Expr) then
        RaiseMsg(20260720120500,nConstantExpressionExpected,sConstantExpressionExpected,[],El.Expr);
      end;
    if El.VarType.Parent=El then
      FinishSubElementType(El,El.VarType);
    end
  else if El.Expr<>nil then
    begin
    // A member const of a GENERIC type is never specialized in this unit if the
    // generic is never instantiated, so Eval below cannot fold it (operands are
    // template params) and the operator is never type-checked. Force a
    // ComputeElement so an illegal operator (e.g. `I div I` with `I: string`,
    // tgenconst15) is rejected at the generic definition — matching FPC. The
    // const's operand types are known from the template constraints
    // (UnwrapConstParam in ComputeBinaryExpr resolves a typed const param).
    GenWalk:=El.Parent;
    while GenWalk<>nil do
      begin
      if (GenWalk is TPasGenericType)
          and (TPasGenericType(GenWalk).GenericTemplateTypes<>nil)
          and (TPasGenericType(GenWalk).GenericTemplateTypes.Count>0) then
        begin
        ComputeElement(El.Expr,ResolvedAbs,[rcConstant]);
        break;
        end;
      GenWalk:=GenWalk.Parent;
      end;
    // no VarType, has Expr, e.g. const a = Expr
    Value:=Eval(El.Expr,[refConstExt]); // e.g. const Tau = 2*PI
    // {$H-}: an untyped string constant is a ShortString (max 255 chars); a
    // longer literal overflows it (tcstring2/tmultilinestring11). Only fires
    // under an explicit {$H-} — the objfpc/{$H+} default keeps AnsiString.
    if (Value<>nil)
        and not (bsLongStrings in CurrentParser.Scanner.CurrentBoolSwitches) then
      begin
      {$IFNDEF PAS2JS}
      if ((Value.Kind=revkString) and (Length(TResEvalString(Value).S)>255))
          or ((Value.Kind=revkUnicodeString) and (Length(TResEvalUTF16(Value).S)>255)) then
        begin
        ReleaseEvalValue(Value);
        RaiseMsg(20260623120000,nRangeCheckError,
          'string exceeds the maximum length of 255 chars',[],El.Expr);
        end;
      {$ENDIF}  
      end;
    ReleaseEvalValue(Value);
    end;
  if El.AbsoluteExpr<>nil then
    begin
    if El.ClassType=TPasConst then
      RaiseMsg(20180201225530,nXModifierMismatchY,sXModifierMismatchY,
        ['absolute','const'],El.AbsoluteExpr);
    if El.VarType=nil then
      RaiseMsg(20171225235125,nVariableIdentifierExpected,sVariableIdentifierExpected,[],El.AbsoluteExpr);
    if vmExternal in El.VarModifiers then
      RaiseMsg(20171226104221,nXModifierMismatchY,sXModifierMismatchY,
        ['absolute','external'],El.AbsoluteExpr);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishVariable El=',GetObjName(El),' Absolute="',GetObjName(El.AbsoluteExpr),'"');
    {$ENDIF}
    ResolveExpr(El.AbsoluteExpr,rraRead);
    ComputeElement(El.AbsoluteExpr,ResolvedAbs,[rcNoImplicitProc]);
    if (not (rrfReadable in ResolvedAbs.Flags))
        or (ResolvedAbs.IdentEl=nil) then
      RaiseVarExpected(20171225234734,El.AbsoluteExpr,ResolvedAbs.IdentEl);
    C:=ResolvedAbs.IdentEl.ClassType;
    if (C=TPasVariable)
        or (C=TPasArgument)
        or ((C=TPasConst) and (TPasConst(ResolvedAbs.IdentEl).VarType<>nil))
        or (C=TPasResultElement) then
    // A record property that reads a plain field IS that field, for fpc and
    // Delphi alike: the LCL's `PicWidth: Integer absolute PicSize.Width`.
    else if (C=TPasProperty) and IsFieldProperty(TPasProperty(ResolvedAbs.IdentEl)) then
    else
      RaiseMsg(20171225235203,nVariableIdentifierExpected,sVariableIdentifierExpected,[],El.AbsoluteExpr);
    if not (rrfReadable in ResolvedAbs.Flags) then
      RaiseVarExpected(20171225235249,El.AbsoluteExpr,ResolvedAbs.IdentEl);
    // check for cycles
    if ResolvedAbs.IdentEl=El then
      RaiseMsg(20171226000703,nVariableIdentifierExpected,sVariableIdentifierExpected,[],El.AbsoluteExpr);
    // an absolute over a bit-packed element needs its byte address, which is illegal
    if IsBitPackedOrdinalAccess(El.AbsoluteExpr) then
      RaiseMsg(20260622120100,nCannotTakeAddrOfBitPackedElement,
        sCannotTakeAddrOfBitPackedElement,[],El.AbsoluteExpr);
    // absolute over an INDEX is a fixed address only when the base is a STATIC
    // array: a string or a dynamic array holds a runtime data pointer, so its
    // element has no address at declaration time (tabsvr3/5). A static array
    // element is fine - rtl-generics writes `LHash1: UInt32 absolute LHash[0]`.
    if (El.AbsoluteExpr is TParamsExpr)
        and (TParamsExpr(El.AbsoluteExpr).Kind=pekArrayParams) then
      begin
      ComputeElement(TParamsExpr(El.AbsoluteExpr).Value,BaseResolved,[rcNoImplicitProc]);
      if (BaseResolved.BaseType in btAllStringAndChars)
          or (BaseResolved.BaseType=btPointer)
          or (BaseResolved.LoTypeEl is TPasPointerType)
          or IsDynArray(BaseResolved.LoTypeEl) then
        RaiseMsg(20260824120000,nVariableIdentifierExpected,
          sVariableIdentifierExpected,[],El.AbsoluteExpr);
      end;
    // absolute over a field reached THROUGH a class instance or a pointer is not
    // a fixed address (the base is a runtime reference), so it is illegal
    // (FPC parser_e_absolute_only_to_var_or_const). tabsvr2/4.
    if (El.AbsoluteExpr is TBinaryExpr)
        and (TBinaryExpr(El.AbsoluteExpr).OpCode=eopSubIdent) then
      begin
      AbsBin:=TBinaryExpr(El.AbsoluteExpr);
      ComputeElement(AbsBin.left,BaseResolved,[rcNoImplicitProc]);
      if ((BaseResolved.BaseType=btContext)
            and (BaseResolved.LoTypeEl is TPasClassType)
            and (TPasClassType(BaseResolved.LoTypeEl).ObjKind=okClass))
          or (BaseResolved.BaseType=btPointer)
          or (BaseResolved.LoTypeEl is TPasPointerType) then
        RaiseMsg(20260622160000,nVariableIdentifierExpected,
          sVariableIdentifierExpected,[],El.AbsoluteExpr);
      end;
    end;
  if El.VarType<>nil then
    EmitTypeHints(El,El.VarType);
end;

procedure TPasResolver.FinishProperty(PropEl: TPasProperty);
var
  PropType: TPasType;
  ClassOrRecScope: TPasClassOrRecordScope;
  ClassScope: TPasClassScope;
  AncestorProp: TPasProperty;
  IndexExpr: TPasExpr;

  procedure GetPropType;
  var
    AncEl: TPasElement;
    GroupScope: TPasGroupScope;
  begin
    if PropType<>nil then exit;
    AncEl:=nil;
    if (ClassScope<>nil) and (ClassScope.AncestorScope<>nil) then
      begin
      CheckTopScope(TPasGroupScope);
      GroupScope:=TPasGroupScope(TopScope);
      AncEl:=GroupScope.FindAncestorElement(PropEl.Name);
      end;
    if AncEl is TPasProperty then
      begin
      // override or redeclaration property
      AncestorProp:=TPasProperty(AncEl);
      TPasPropertyScope(PropEl.CustomData).AncestorProp:=AncestorProp;
      if proFixCaseOfOverrides in Options then
        PropEl.Name:=AncestorProp.Name;
      end
    else
      AncestorProp:=nil;

    if PropEl.VarType<>nil then
      begin
      // new property or redeclaration
      PropType:=PropEl.VarType;
      CheckUseAsType(PropEl.VarType,20190123100011,PropEl);
      end
    else
      begin
      // property override
      if AncestorProp=nil then
        RaiseMsg(20170216151741,nNoPropertyFoundToOverride,sNoPropertyFoundToOverride,[],PropEl);
      // check property versus class property
      if PropEl.ClassType<>AncestorProp.ClassType then
        RaiseXExpectedButYFound(20170216151744,GetElementTypeName(AncestorProp),GetElementTypeName(PropEl),PropEl);
      // get inherited type
      PropType:=GetPasPropertyType(AncestorProp);
      // update DefaultProperty
      if ClassScope=nil then
        RaiseNotYetImplemented(20181231130642,PropEl);
      if ClassScope.DefaultProperty=AncestorProp then
        ClassScope.DefaultProperty:=PropEl;
      end;
  end;

  function CheckClassAccessorStatic(ProcIsStatic: boolean): boolean;
  begin
    if ClassScope=nil then
      // record: class getter/setter must be static
      Result:=ProcIsStatic=true
    else if proClassPropertyNonStatic in Options then
      Result:=true // both allowed
    else
      Result:=ProcIsStatic=true;
  end;

  procedure CheckIndexArg(ArgNo: integer; const IndexResolved: TPasResolverResult;
    ProcArg: TPasArgument; ErrorEl: TPasElement);
  var
    ProcArgResolved: TPasResolverResult;
  begin
    // check access: const, ...
    if not (ProcArg.Access in [argDefault,argConst]) then
      RaiseMsg(20170924202437,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        [IntToStr(ArgNo),AccessDescriptions[ProcArg.Access],
         AccessDescriptions[argConst]],ErrorEl);
    // check argument type
    if ProcArg.ArgType=nil then
      RaiseMsg(20170924202531,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        [IntToStr(ArgNo),'untyped',GetTypeDescription(IndexResolved)],ErrorEl)
    else
      begin
      if CheckParamCompatibility(IndexExpr,ProcArg,ArgNo,true)=cIncompatible then
        begin
        ComputeElement(ProcArg.ArgType,ProcArgResolved,[rcType]);
        RaiseIncompatibleTypeRes(20170924203829,nIncompatibleTypeArgNo,
          [IntToStr(ArgNo)],ProcArgResolved,IndexResolved,ErrorEl);
        end;
      end;
  end;

  procedure CheckArgs(Proc: TPasProcedure; const IndexVal: TResEvalValue;
    const IndexResolved: TPasResolverResult; ErrorEl: TPasElement);
  var
    ArgNo: Integer;
    PropArg, ProcArg: TPasArgument;
    PropArgResolved, ProcArgResolved: TPasResolverResult;
    NeedCheckingAccess: Boolean;
  begin
    ArgNo:=0;
    while ArgNo<PropEl.Args.Count do
      begin
      if ArgNo>=Proc.ProcType.Args.Count then
        RaiseMsg(20170216151805,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[Proc.Name],ErrorEl);
      PropArg:=TPasArgument(PropEl.Args[ArgNo]);
      ProcArg:=TPasArgument(Proc.ProcType.Args[ArgNo]);
      inc(ArgNo);

      // check access: var, const, ...
      NeedCheckingAccess:=false;
      if PropArg.Access<>ProcArg.Access then
        begin

        if (PropArg.Access in [argDefault, argConst])
            and (ProcArg.Access in [argDefault, argConst]) then
          begin
          // passing an arg as default to const or const to default
          if (PropArg.ArgType<>nil)
              and (ProcArg.ArgType<>nil) then
            NeedCheckingAccess:=true;
          end;
        if not NeedCheckingAccess then
          RaiseMsg(20170216151808,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(ArgNo),AccessDescriptions[ProcArg.Access],
             AccessDescriptions[PropArg.Access]],ErrorEl);
        end;

      // check argument type
      if PropArg.ArgType=nil then
        begin
        if ProcArg.ArgType<>nil then
          RaiseMsg(20170216151811,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(ArgNo),GetElementTypeName(ProcArg.ArgType),'untyped'],ErrorEl);
        end
      else if ProcArg.ArgType=nil then
        RaiseMsg(20170216151813,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
          [IntToStr(ArgNo),'untyped',GetElementTypeName(PropArg.ArgType)],ErrorEl)
      else
        begin
        ComputeElement(PropArg,PropArgResolved,[rcNoImplicitProc]);
        ComputeElement(ProcArg,ProcArgResolved,[rcNoImplicitProc]);

        if (PropArgResolved.BaseType<>ProcArgResolved.BaseType) then
          RaiseMsg(20170216151816,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(ArgNo),BaseTypeNames[ProcArgResolved.BaseType],BaseTypeNames[PropArgResolved.BaseType]],ErrorEl);
        if PropArgResolved.LoTypeEl=nil then
          RaiseInternalError(20161010125255);
        if ProcArgResolved.LoTypeEl=nil then
          RaiseInternalError(20161010125304);
        // Two `array of T` written separately are two anonymous types, the same
        // when their elements are (fpdebug's `property MemberCountEx[const AIndex:
        // array of Int64]` read by `GetMemberCountEx(const AIndex: array of Int64)`).
        if not IsSameType(PropArgResolved.HiTypeEl,ProcArgResolved.HiTypeEl,prraSimple)
            and not ((PropArgResolved.LoTypeEl is TPasArrayType)
              and (ProcArgResolved.LoTypeEl is TPasArrayType)
              and IsOpenArray(PropArgResolved.LoTypeEl)
              and IsOpenArray(ProcArgResolved.LoTypeEl)
              and IsSameType(GetArrayElType(TPasArrayType(PropArgResolved.LoTypeEl)),
                             GetArrayElType(TPasArrayType(ProcArgResolved.LoTypeEl)),
                             prraSimple)) then
          RaiseIncompatibleType(20170216151819,nIncompatibleTypeArgNo,
            [IntToStr(ArgNo)],ProcArgResolved.HiTypeEl,PropArgResolved.HiTypeEl,ErrorEl);
        end;

        // NeedCheckingAccess: the property passes the argument as default while
        // the accessor takes it as const, or the other way round, e.g.
        //   function GetItems(const i: integer): byte;
        //   property Items[i: integer]: byte read GetItems;
        // Real FPC allows that for ANY argument type, not only the simple ones -
        // rtl-generics declares `property Items[Index: TKey]` over
        // `GetItem(const AKey: TKey)`.
      end;

    if IndexVal<>nil then
      begin
      if ArgNo>=Proc.ProcType.Args.Count then
        RaiseMsg(20170924202334,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[Proc.Name],ErrorEl);
      ProcArg:=TPasArgument(Proc.ProcType.Args[ArgNo]);
      CheckIndexArg(ArgNo,IndexResolved,ProcArg,ErrorEl);
      end;
  end;

  procedure CheckImplements;
  var
    i, j: Integer;
    Expr: TPasExpr;
    ResolvedEl: TPasResolverResult;
    aClass, PropClassType: TPasClassType;
    IntfType, OrigIntfType, PropTypeRes: TPasType;
    o: TObject;
  begin
    if not (PropEl.Parent is TPasClassType) then
      RaiseInternalError(20180323172125,GetElementDbgPath(PropEl));
    aClass:=TPasClassType(PropEl.Parent);
    if PropEl.Args.Count>0 then
      RaiseMsg(20180323170952,nImplementsDoesNotSupportArrayProperty,
        sImplementsDoesNotSupportArrayProperty,[],PropEl.Implements[0]);
    if IndexExpr<>nil then
      RaiseMsg(20180323171354,nImplementsDoesNotSupportIndex,
        sImplementsDoesNotSupportIndex,[],PropEl.Implements[0]);
    if GetPasPropertyGetter(PropEl)=nil then
      RaiseMsg(20180323221322,nImplPropMustHaveReadSpec,
        sImplPropMustHaveReadSpec,[],PropEl.Implements[0]);
    for i:=0 to length(PropEl.Implements)-1 do
      begin
      // resolve expression
      Expr:=PropEl.Implements[i];
      ResolveExpr(Expr,rraRead);
      // check expr is an interface type
      ComputeElement(Expr,ResolvedEl,[rcType,rcNoImplicitProc]);
      if not (ResolvedEl.IdentEl is TPasType) then
        if ResolvedEl.IdentEl=nil then
          RaiseXExpectedButYFound(20180323171911,'interface',
            GetElementTypeName(ResolvedEl.LoTypeEl),Expr)
        else
          RaiseXExpectedButYFound(20180323224846,'interface',
            GetElementTypeName(ResolvedEl.IdentEl),Expr);
      OrigIntfType:=TPasType(ResolvedEl.IdentEl);
      IntfType:=ResolveAliasType(OrigIntfType);
      if (not (IntfType is TPasClassType))
          or (TPasClassType(IntfType).ObjKind<>okInterface) then
        RaiseXExpectedButYFound(20180323172904,'interface',
          GetElementTypeName(OrigIntfType),Expr);
      // check it is one of the current implemented interfaces (not of ancestors)
      j:=IndexOfImplementedInterface(aClass,IntfType);
      if j<0 then
        RaiseMsg(20180323172420,nImplementsUsedOnUnimplIntf,sImplementsUsedOnUnimplIntf,
          [OrigIntfType.Name],Expr);
      // check property type fits
      PropTypeRes:=ResolveAliasType(PropType);
      if not (PropTypeRes is TPasClassType) then
        RaiseMsg(20180323222334,nDoesNotImplementInterface,sDoesNotImplementInterface,
          [GetElementTypeName(PropType),GetElementTypeName(OrigIntfType)],Expr);
      PropClassType:=TPasClassType(PropTypeRes);
      case PropClassType.ObjKind of
      okClass:
        // e.g. property Obj: ClassType read Getter implements IntfType
        // check ClassType or ancestors implements IntfType
        if GetClassImplementsIntf(PropClassType,TPasClassType(IntfType))=nil then
          RaiseMsg(20180323223324,nDoesNotImplementInterface,sDoesNotImplementInterface,
            [GetElementTypeName(PropType),GetElementTypeName(OrigIntfType)],Expr);
      okInterface:
        // e.g. property IntfVar: IntfType read Getter implements IntfType2
        // check that IntfType is IntfType2
        if CheckClassIsClass(PropType,IntfType)=cIncompatible then
          RaiseIncompatibleType(20180323173746,nIncompatibleTypesGotExpected,
            [],OrigIntfType,PropType,Expr);
      else
        RaiseMsg(20180323222821,nDoesNotImplementInterface,sDoesNotImplementInterface,
          [GetElementTypeName(PropType),GetElementTypeName(OrigIntfType)],Expr);
      end;
      // map
      o:=TObject(ClassScope.Interfaces[j]);
      if o is TPasProperty then
        RaiseMsg(20180323174240,nDuplicateImplementsForIntf,sDuplicateImplementsForIntf,
          [OrigIntfType.Name,GetElementSourcePosStr(TPasProperty(o))],Expr)
      else if o is TPasClassIntfMap then
        begin
        // properties are checked before method resolutions
        RaiseInternalError(20180323175919,GetElementDbgPath(PropEl));
        end
      else if o<>nil then
        RaiseInternalError(20180323174342,GetObjName(o))
      else
        ClassScope.Interfaces[j]:=PropEl;
      end;
  end;

  procedure CheckStoredAccessor(Expr: TPasExpr; const IndexVal: TResEvalValue;
    const IndexResolved: TPasResolverResult);
  var
    ResolvedEl: TPasResolverResult;
    Value: TResEvalValue;
    Proc: TPasProcedure;
    ResultType, TypeEl: TPasType;
    aVar: TPasVariable;
    IdentEl: TPasElement;
    ExpArgCnt: Integer;
    ProcArg: TPasArgument;
  begin
    ResolveExpr(Expr,rraRead);
    ComputeElement(Expr,ResolvedEl,[rcNoImplicitProc]);
    IdentEl:=ResolvedEl.IdentEl;
    if IdentEl is TPasProcedure then
      begin
      // function
      Proc:=TPasProcedure(IdentEl);
      // check if member
      if not (Expr is TPrimitiveExpr) then
        RaiseXExpectedButYFound(20170923202002,'member function','foreign '+GetElementTypeName(Proc),Expr);
      if Proc.ClassType<>TPasFunction then
        RaiseXExpectedButYFound(20170216151925,'function',GetElementTypeName(Proc),Expr);
      // check function result type
      ResultType:=TPasFunction(Proc).FuncType.ResultEl.ResultType;
      if not IsBaseType(ResultType,btBoolean,true) then
        RaiseXExpectedButYFound(20170923200836,'function: boolean',
          'function:'+GetTypeDescription(ResultType),PropEl.StoredAccessor);
      if Proc.IsAsync then
        RaiseInvalidProcTypeModifier(20200524104719,Proc.ProcType,ptmAsync,Expr);
      // check arg count
      ExpArgCnt:=0;
      if IndexVal<>nil then
        inc(ExpArgCnt);
      if Proc.ProcType.Args.Count<>ExpArgCnt then
        RaiseMsg(20170923200840,nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
          [Proc.Name],Expr);
      if IndexVal<>nil then
        begin
        // check arg type
        ProcArg:=TPasArgument(Proc.ProcType.Args[0]);
        CheckIndexArg(1,IndexResolved,ProcArg,Expr);
        end;
      exit;
      end;
    if (IdentEl<>nil)
      and ((IdentEl.ClassType=TPasVariable)
        or ((IdentEl.ClassType=TPasConst) and not TPasConst(IdentEl).IsConst)) then
      begin
      // field
      aVar:=TPasVariable(IdentEl);
      // check if member
      if not (Expr is TPrimitiveExpr) then
        RaiseXExpectedButYFound(20170923202003,'member variable','foreign '+GetElementTypeName(aVar),Expr);
      // check type boolean
      TypeEl:=aVar.VarType;
      TypeEl:=ResolveAliasType(TypeEl);
      if not IsBaseType(TypeEl,btBoolean,true) then
        RaiseIncompatibleType(20170409214300,nIncompatibleTypesGotExpected,
          [],TypeEl,BaseTypes[btBoolean],Expr);
      // check class var
      if (vmClass in PropEl.VarModifiers)<>(vmClass in aVar.VarModifiers) then
        if vmClass in PropEl.VarModifiers then
          RaiseXExpectedButYFound(20170409214351,'class var','var',Expr)
        else
          RaiseXExpectedButYFound(20170409214359,'var','class var',Expr);
      exit;
      end;
    if (ResolvedEl.BaseType=btBoolean) and (ResolvedEl.ExprEl<>nil) then
      begin
      // try evaluating const boolean
      Value:=Eval(Expr,[refConst]);
      if Value<>nil then
        try
          if Value.Kind<>revkBool then
            RaiseXExpectedButYFound(20170923200256,'boolean',GetResolverResultDescription(ResolvedEl),Expr);
          exit;
        finally
          ReleaseEvalValue(Value);
        end;
      end;
    RaiseXExpectedButYFound(20170923194234,'identifier',GetResolverResultDescription(ResolvedEl),Expr);
  end;

  function SelectAccessorOverload(AccExpr: TPasExpr; AccEl: TPasElement;
    NeededArgCount: integer; IsSetter: boolean): TPasElement;
  // A property read/write accessor may name an overloaded method. ResolveAccessor
  // returns the first-found overload, which can be the wrong one. Pick the overload
  // whose signature matches the property (arg count + value/result type) and redirect
  // the accessor reference so later checks and codegen use the right method.

    function ParamsMatch(P: TPasProcedure): boolean;
    // An indexed property's own parameters are the accessor's LEADING arguments.
    // Two overloads may agree on argument count and value type and differ only
    // there — fcl-xml xpath.pp reads both `Functions[Index: Integer]` and
    // `FunctionsByName[const AName: String]` through an overloaded GetFunction.
    var
      i: Integer;
      PropArg, AccArg: TPasArgument;
    begin
      Result:=false;
      if PropEl.Args.Count>P.ProcType.Args.Count then exit;
      for i:=0 to PropEl.Args.Count-1 do
        begin
        PropArg:=TPasArgument(PropEl.Args[i]);
        AccArg:=TPasArgument(P.ProcType.Args[i]);
        if (PropArg.ArgType=nil) or (AccArg.ArgType=nil) then continue;
        if not IsSameType(PropArg.ArgType,AccArg.ArgType,prraAlias) then exit;
        end;
      // With `Index N` the accessor takes the index right after the property's
      // own arguments, and two overloads may differ only there: fcl-web's
      // httpdefs.pp declares SetHTTPVariable for Integer and for
      // THTTPVariableType, and only the Integer one can take `Index Ord(...)`.
      if (IndexExpr<>nil) and (PropEl.Args.Count<P.ProcType.Args.Count) then
        begin
        AccArg:=TPasArgument(P.ProcType.Args[PropEl.Args.Count]);
        if (AccArg.ArgType<>nil)
            and (CheckParamCompatibility(IndexExpr,AccArg,PropEl.Args.Count+1,false)=cIncompatible) then
          exit;
        end;
      Result:=true;
    end;

    function AccMatches(P: TPasProcedure; out TypeMatches: boolean): boolean;
    var
      LArg: TPasArgument;
      LRes: TPasType;
    begin
      TypeMatches:=false;
      Result:=false;
      if P.ProcType.Args.Count<>NeededArgCount then exit;
      if IsSetter then
        begin
        // a setter is a procedure, not a function
        if P is TPasFunction then exit;
        if NeededArgCount<1 then exit;
        LArg:=TPasArgument(P.ProcType.Args[NeededArgCount-1]);
        Result:=true;
        TypeMatches:=(LArg.ArgType<>nil) and PropTypesMatch(LArg.ArgType,PropType)
          and ParamsMatch(P);
        end
      else
        begin
        // a getter is a function
        if not (P is TPasFunction) then exit;
        LRes:=TPasFunction(P).FuncType.ResultEl.ResultType;
        Result:=true;
        TypeMatches:=PropTypesMatch(LRes,PropType) and ParamsMatch(P);
        end;
    end;

  var
    Owner: TPasElement;
    Members: TFPList;
    i: Integer;
    M, Best, FallBack: TPasProcedure;
    LTypeMatches: Boolean;
    Ref: TResolvedReference;
  begin
    Result:=AccEl;
    if not (AccEl is TPasProcedure) then exit;
    // already the right overload (arg count and type) -> keep it
    if AccMatches(TPasProcedure(AccEl),LTypeMatches) and LTypeMatches then exit;
    Best:=nil; FallBack:=nil;
    Owner:=AccEl.Parent;
    while Owner is TPasMembersType do
      begin
      Members:=TPasMembersType(Owner).Members;
      for i:=0 to Members.Count-1 do
        if TObject(Members[i]) is TPasProcedure then
          begin
          M:=TPasProcedure(Members[i]);
          if not SameText(M.Name,AccEl.Name) then continue;
          if AccMatches(M,LTypeMatches) then
            if LTypeMatches then
              begin
              Best:=M;
              break;
              end
            else if FallBack=nil then
              FallBack:=M;
          end;
      if Best<>nil then break;
      if Owner is TPasClassType then
        Owner:=ResolveAliasType(TPasClassType(Owner).AncestorType)
      else
        Owner:=nil;
      end;
    if Best=nil then Best:=FallBack;
    if (Best<>nil) and (Best<>AccEl) then
      begin
      if (AccExpr<>nil) and (AccExpr.CustomData is TResolvedReference) then
        begin
        Ref:=TResolvedReference(AccExpr.CustomData);
        Ref.Declaration:=Best;
        end;
      Result:=Best;
      end;
  end;

var
  ResultType, aType, AccType: TPasType;
  MembersType: TPasMembersType;
  AccEl: TPasElement;
  Proc: TPasProcedure;
  Arg: TPasArgument;
  PropArgCount, NeedArgCnt: Integer;
  PropTypeResolved, DefaultResolved, IndexResolved,
    AncIndexResolved: TPasResolverResult;
  m: TVariableModifier;
  IndexVal: TResEvalValue;
  AncIndexExpr, ErrorEl: TPasExpr;
  CurClass: TPasClassType;
begin
  CheckTopScope(TPasPropertyScope);
  PopScope;

  if PropEl.Visibility=visPublished then
    for m in PropEl.VarModifiers do
      if not (m in [vmExternal]) then
        RaiseMsg(20170403224112,nInvalidXModifierY,sInvalidXModifierY,
          ['published property','"'+VariableModifierNames[m]+'"'],PropEl);

  PropType:=nil;
  // A unit-level (global) property has a section owner, not a class/record; the
  // members-type / class-or-record scope stay nil and control falls through the
  // ClassScope=nil new-property path below (GetPropType uses PropEl.VarType).
  if PropEl.Parent is TPasMembersType then
    begin
    MembersType:=TPasMembersType(PropEl.Parent);
    ClassOrRecScope:=NoNil(MembersType.CustomData) as TPasClassOrRecordScope;
    end
  else
    begin
    MembersType:=nil;
    ClassOrRecScope:=nil;
    end;
  ClassScope:=nil;
  CurClass:=nil;
  if ClassOrRecScope is TPasClassScope then
    begin
    ClassScope:=TPasClassScope(ClassOrRecScope);
    CurClass:=TPasClassType(MembersType);
    end;
  AncestorProp:=nil;
  GetPropType;
  // A published property's type must be publishable (have RTTI). File types
  // (text/file) have none -> reject (FPC type_e_symbol_cannot_be_published).
  if (PropEl.Visibility=visPublished) and (PropType<>nil) then
    begin
    ComputeElement(PropType,PropTypeResolved,[rcType]);
    {$IFNDEF PAS2JS}
    if PropTypeResolved.BaseType in [btFile,btText] then
      RaiseMsg(20260622100001,nSymbolCannotBePublished,sSymbolCannotBePublished,[],PropEl);
    {$ENDIF}
    end;
  IndexVal:=nil;
  try
    if PropEl.IndexExpr<>nil then
      begin
      // index specifier
      // -> check if simple value
      IndexExpr:=PropEl.IndexExpr;
      ResolveExpr(IndexExpr,rraRead);
      end
    else
      IndexExpr:=GetPasPropertyIndex(PropEl);
    if IndexExpr<>nil then
      begin
      ComputeElement(IndexExpr,IndexResolved,[rcConstant]);
      IndexVal:=Eval(IndexExpr,[refConst]);
      case IndexVal.Kind of
      revkBool,
      revkInt, revkUInt,
      revkFloat,
      revkCurrency,
      {$ifdef FPC_HAS_CPSTRING}
      revkString,
      {$endif}
      revkUnicodeString,
      revkEnum: ; // ok
      else
        RaiseXExpectedButYFound(20170924202837,'ordinal',GetTypeDescription(IndexResolved),PropEl.IndexExpr);
      end;
      if (PropEl.IndexExpr<>nil) and (PropEl.VarType=nil) then
        begin
        // check if index is compatible to ancestor index specifier
        AncIndexExpr:=GetPasPropertyIndex(AncestorProp);
        if AncIndexExpr=nil then
          begin
          // ancestor had no index specifier
          if PropEl.ReadAccessor=nil then
            begin
            AccEl:=GetPasPropertyGetter(AncestorProp);
            if AccEl is TPasProcedure then
              RaiseMsg(20171002144103,nAddingIndexSpecifierRequiresNewX,
                sAddingIndexSpecifierRequiresNewX,['read'],IndexExpr);
            end;
          if PropEl.WriteAccessor=nil then
            begin
            AccEl:=GetPasPropertySetter(AncestorProp);
            if AccEl is TPasProcedure then
              RaiseMsg(20171002144419,nAddingIndexSpecifierRequiresNewX,
                sAddingIndexSpecifierRequiresNewX,['write'],IndexExpr);
            end;
          if PropEl.StoredAccessor=nil then
            begin
            AccEl:=GetPasPropertyStoredExpr(AncestorProp);
            if AccEl<>nil then
              begin
              ComputeElement(AccEl,AncIndexResolved,[rcNoImplicitProc]);
              if AncIndexResolved.IdentEl is TPasProcedure then
                RaiseMsg(20171002144644,nAddingIndexSpecifierRequiresNewX,
                  sAddingIndexSpecifierRequiresNewX,['stored'],IndexExpr);
              end;
            end;
          end
        else
          // ancestor had already an index specifier -> check same type
          CheckEqualElCompatibility(PropEl.IndexExpr,AncIndexExpr,PropEl.IndexExpr,true);
        end;
      end;

    if PropEl.ReadAccessor<>nil then
      begin
      // check compatibility
      ErrorEl:=PropEl.ReadAccessor;
      AccEl:=ResolveAccessor(PropEl.ReadAccessor);
      // if an overloaded method was named, pick the getter overload matching the property
      AccEl:=SelectAccessorOverload(PropEl.ReadAccessor,AccEl,
        PropEl.Args.Count+ord(IndexVal<>nil),false);
      // record/type helper: accessor must belong to the helper, not the extended type
      if (AccEl <> nil) and (ClassOrRecScope <> nil)
          and (ClassOrRecScope.Element is TPasClassType)
          and (TPasClassType(ClassOrRecScope.Element).ObjKind in [okRecordHelper, okTypeHelper])
          and (MaximizeFPCCompatibility or (AccEl.Parent <> ResolveAliasType(TPasClassType(ClassOrRecScope.Element).HelperForType)))
          and (AccEl.Parent <> ClassOrRecScope.Element) then
        RaiseMsg(20260225100010,nIdentifierNotFound,sIdentifierNotFound,
          [AccEl.Name],PropEl.ReadAccessor);
      if (AccEl.ClassType=TPasVariable) or (AccEl.ClassType=TPasConst) then
        begin
        if (PropEl.Args.Count>0) then
          RaiseXExpectedButYFound(20170216151823,'function',GetElementTypeName(AccEl),ErrorEl);
        AccType:=TPasVariable(AccEl).VarType;
        if PropEl.ReadAccessor is TParamsExpr then
          AccType:=AccessorElementType(PropEl.ReadAccessor);
        if not PropTypesMatch(AccType,PropType)
            and not UndecidedPair(AccType,PropType)
            and not ProcTypesHaveSameSignature(PropType,AccType) then
          RaiseIncompatibleType(20170216151826,nIncompatibleTypesGotExpected,
            [],PropType,AccType,ErrorEl);
        if (vmClass in PropEl.VarModifiers)<>(vmClass in TPasVariable(AccEl).VarModifiers) then
          if vmClass in PropEl.VarModifiers then
            RaiseXExpectedButYFound(20170216151828,'class var','var',ErrorEl)
          else
            RaiseXExpectedButYFound(20170216151831,'var','class var',ErrorEl);
        end
      else if AccEl is TPasProcedure then
        begin
        // check function
        Proc:=TPasProcedure(AccEl);
        if (vmClass in PropEl.VarModifiers) then
          begin
          if Proc.ClassType<>TPasClassFunction then
            RaiseXExpectedButYFound(20170216151834,'class function',GetElementTypeName(Proc),ErrorEl);
          if not CheckClassAccessorStatic(Proc.IsStatic) then
            if Proc.IsStatic then
              RaiseMsg(20170216151837,nClassPropertyAccessorMustNotBeStatic,sClassPropertyAccessorMustNotBeStatic,[],ErrorEl)
            else
              RaiseMsg(20170216151839,nClassPropertyAccessorMustBeStatic,sClassPropertyAccessorMustBeStatic,[],ErrorEl);
          end
        else
          begin
          // an instance property may also read through a class function, as
          // fpc and Delphi allow (lazarus' TDebuggerIntf.SupportedCommands)
          if (Proc.ClassType<>TPasFunction) and (Proc.ClassType<>TPasClassFunction) then
            RaiseXExpectedButYFound(20170216151842,'function',GetElementTypeName(Proc),ErrorEl);
          end;
        // check function result type
        ResultType:=TPasFunction(Proc).FuncType.ResultEl.ResultType;
        if not PropTypesMatch(ResultType,PropType) then
          RaiseXExpectedButYFound(20170216151844,'function result '+GetTypeDescription(PropType,true),
            GetTypeDescription(ResultType,true),ErrorEl);
        if Proc.IsAsync then
          RaiseMsg(20200526101546,nInvalidXModifierY,sInvalidXModifierY,['property getter',
            ProcTypeModifiers[ptmAsync]],ErrorEl);
        // check args
        CheckArgs(Proc,IndexVal,IndexResolved,PropEl.ReadAccessor);
        NeedArgCnt:=PropEl.Args.Count;
        if IndexVal<>nil then
          inc(NeedArgCnt);
        if Proc.ProcType.Args.Count<>NeedArgCnt then
          RaiseMsg(20170216151847,nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
            [Proc.Name],ErrorEl);
        end
      else
        RaiseXExpectedButYFound(20170216151850,'variable',GetElementTypeName(AccEl),ErrorEl);
      end;

    if PropEl.WriteAccessor<>nil then
      begin
      // check compatibility
      ErrorEl:=PropEl.WriteAccessor;
      AccEl:=ResolveAccessor(PropEl.WriteAccessor);
      // if an overloaded method was named, pick the setter overload matching the property
      AccEl:=SelectAccessorOverload(PropEl.WriteAccessor,AccEl,
        PropEl.Args.Count+ord(IndexVal<>nil)+1,true);
      // record/type helper: accessor must belong to the helper, not the extended type
      if (AccEl <> nil) and (ClassOrRecScope <> nil)
          and (ClassOrRecScope.Element is TPasClassType)
          and (TPasClassType(ClassOrRecScope.Element).ObjKind in [okRecordHelper, okTypeHelper])
          and (MaximizeFPCCompatibility or (AccEl.Parent <> ResolveAliasType(TPasClassType(ClassOrRecScope.Element).HelperForType)))
          and (AccEl.Parent <> ClassOrRecScope.Element) then
        RaiseMsg(20260225100011,nIdentifierNotFound,sIdentifierNotFound,
          [AccEl.Name],PropEl.WriteAccessor);
      if (AccEl.ClassType=TPasVariable)
          or ((AccEl.ClassType=TPasConst) and (not TPasConst(AccEl).IsConst)) then
        begin
        if (PropEl.Args.Count>0) then
          RaiseXExpectedButYFound(20170216151852,'procedure',GetElementTypeName(AccEl),ErrorEl);
        AccType:=TPasVariable(AccEl).VarType;
        if PropEl.WriteAccessor is TParamsExpr then
          AccType:=AccessorElementType(PropEl.WriteAccessor);
        if not PropTypesMatch(AccType,PropType)
            and not UndecidedPair(AccType,PropType)
            and not ProcTypesHaveSameSignature(PropType,AccType) then
          RaiseIncompatibleType(20170216151855,nIncompatibleTypesGotExpected,
            [],PropType,AccType,ErrorEl);
        if (vmClass in PropEl.VarModifiers)<>(vmClass in TPasVariable(AccEl).VarModifiers) then
          if vmClass in PropEl.VarModifiers then
            RaiseXExpectedButYFound(20170216151858,'class var','var',ErrorEl)
          else
            RaiseXExpectedButYFound(20170216151900,'var','class var',ErrorEl);
        end
      else if AccEl is TPasProcedure then
        begin
        // check procedure
        Proc:=TPasProcedure(AccEl);
        if (vmClass in PropEl.VarModifiers) then
          begin
          if Proc.ClassType<>TPasClassProcedure then
            RaiseXExpectedButYFound(20170216151903,'class procedure',GetElementTypeName(Proc),ErrorEl);
          if not CheckClassAccessorStatic(Proc.IsStatic) then
            if Proc.IsStatic then
              RaiseMsg(20170216151905,nClassPropertyAccessorMustNotBeStatic,sClassPropertyAccessorMustNotBeStatic,[],ErrorEl)
            else
              RaiseMsg(20170216151906,nClassPropertyAccessorMustBeStatic,sClassPropertyAccessorMustBeStatic,[],ErrorEl);
          end
        else
          begin
          if Proc.ClassType<>TPasProcedure then
            RaiseXExpectedButYFound(20170216151910,'procedure',GetElementTypeName(Proc),ErrorEl);
          end;
        if Proc.IsAsync then
          RaiseMsg(20200526101635,nInvalidXModifierY,sInvalidXModifierY,['property setter',
            ProcTypeModifiers[ptmAsync]],ErrorEl);
        // check args
        CheckArgs(Proc,IndexVal,IndexResolved,PropEl.WriteAccessor);
        // check write arg
        PropArgCount:=PropEl.Args.Count;
        if IndexVal<>nil then
          inc(PropArgCount);
        if Proc.ProcType.Args.Count<>PropArgCount+1 then
          RaiseMsg(20170216151913,nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
            [Proc.Name],ErrorEl);
        Arg:=TPasArgument(Proc.ProcType.Args[PropArgCount]);
        // {$VARPROPSETTER ON} additionally allows a 'var' value parameter, so
        // the setter can write back to the assigned expression.
        if not ((Arg.Access in [argDefault,argConst])
            or ((Arg.Access=argVar) and (bsVarPropSetter in CurrentParser.Scanner.CurrentBoolSwitches))) then
          RaiseMsg(20170216151917,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(PropArgCount+1),AccessDescriptions[Arg.Access],
             AccessDescriptions[argConst]],ErrorEl);
        if not PropTypesMatch(Arg.ArgType,PropType) then
          RaiseIncompatibleType(20170216151919,nIncompatibleTypeArgNo,
            [IntToStr(PropArgCount+1)],Arg.ArgType,PropType,ErrorEl);
        end
      else
        RaiseXExpectedButYFound(20170216151921,'variable',GetElementTypeName(AccEl),ErrorEl);
      end
    else if (PropEl.ReadAccessor=nil) and (PropEl.VarType<>nil) then
      RaiseMsg(20180519173551,nPropertyMustHaveReadOrWrite,sPropertyMustHaveReadOrWrite,[],PropEl);

    if length(PropEl.Implements)>0 then
      CheckImplements;

    // Class properties do not support the 'stored' specifier (no instance to
    // stream) (FPC parser_e_illegal_expression / class property stored).
    if (vmClass in PropEl.VarModifiers) and (PropEl.StoredAccessor<>nil) then
      RaiseMsg(20260328000002,nInvalidXModifierY,sInvalidXModifierY,
        ['class property','"stored"'],PropEl.StoredAccessor);

    if PropEl.StoredAccessor<>nil then
      begin
      // check compatibility
      CheckStoredAccessor(PropEl.StoredAccessor,IndexVal,IndexResolved);
      end;

    if PropEl.DefaultExpr<>nil then
      begin
      // check compatibility with type
      ResolveExpr(PropEl.DefaultExpr,rraRead);
      ComputeElement(PropEl.DefaultExpr,DefaultResolved,[rcConstant]);
      ComputeElement(PropType,PropTypeResolved,[rcType]);
      PropTypeResolved.IdentEl:=PropEl;
      PropTypeResolved.Flags:=[rrfReadable];
      CheckEqualResCompatibility(PropTypeResolved,DefaultResolved,PropEl.DefaultExpr,true);
      end;
    if PropEl.IsDefault then
      begin
      if (CurClass<>nil) and (CurClass.HelperForType<>nil) then
        begin
        aType:=ResolveAliasType(CurClass.HelperForType);
        if not (aType is TPasMembersType) then
          RaiseMsg(20190117125004,nDefaultPropertyNotAllowedInHelperForX,
            sDefaultPropertyNotAllowedInHelperForX,
            [GetTypeDescription(CurClass.HelperForType)],PropEl);
        end;
      // set default array property
      if (ClassOrRecScope.DefaultProperty<>nil)
          and (ClassOrRecScope.DefaultProperty.Parent=PropEl.Parent) then
        RaiseMsg(20170216151938,nOnlyOneDefaultPropertyIsAllowed,sOnlyOneDefaultPropertyIsAllowed,[],PropEl);
      ClassOrRecScope.DefaultProperty:=PropEl;
      end;
    EmitTypeHints(PropEl,PropEl.VarType);
  finally
    ReleaseEvalValue(IndexVal);
  end;
end;

procedure TPasResolver.FinishArgument(El: TPasArgument);

  procedure CheckHasGenTemplRef(Arg: TPasArgument);

    procedure Check(Parent: TPasElement; Cur: TPasType; TemplTypes: TFPList);
    var
      C: TClass;
      Arr: TPasArrayType;
    begin
      if Cur=nil then exit;
      C:=Cur.ClassType;
      if C=TPasGenericTemplateType then
        begin
        if TemplTypes.IndexOf(Cur)>=0 then
          RaiseMsg(20191007213121,nParamOfThisTypeCannotHaveDefVal,sParamOfThisTypeCannotHaveDefVal,[],El);
        end
      else if Cur.Parent<>Parent then
        exit
      else if C=TPasArrayType then
        begin
        Arr:=TPasArrayType(Cur);
        Check(Arr,Arr.ElType,TemplTypes);
        end;
    end;

  var
    Proc: TPasProcedure;
    TemplTypes: TFPList;
  begin
    if Arg.ArgType=nil then exit;
    if not (Arg.Parent is TPasProcedureType) then exit;
    if not (Arg.Parent.Parent is TPasProcedure) then exit;
    Proc:=TPasProcedure(Arg.Parent.Parent);
    TemplTypes:=GetProcTemplateTypes(Proc);
    if TemplTypes=nil then exit;
    Check(Arg,Arg.ArgType,TemplTypes);
  end;

var
  IsDelphi: Boolean;
begin
  if not (El.Access in [argDefault,argConst,argVar,argOut,argConstRef]) then
    RaiseMsg(20191018235644,nNotYetImplemented,sNotYetImplemented,[AccessDescriptions[El.Access]],El);
  if El.ArgType<>nil then
    CheckUseAsType(El.ArgType,20190123100049,El);
  if El.ValueExpr<>nil then
    begin
    ResolveExpr(El.ValueExpr,rraRead);
    if El.ArgType<>nil then
      begin
      CheckAssignCompatibility(El,El.ValueExpr,true);
      IsDelphi:=msDelphi in CurrentParser.CurrentModeswitches;
      if IsDelphi then
        CheckHasGenTemplRef(El);
      end;
    end;
  EmitTypeHints(El,El.ArgType);
end;

procedure TPasResolver.FinishAncestors(aClass: TPasClassType);
// called when the ancestor and interface list of a class has been parsed,
// before parsing the class elements
var
  DirectAncestor: TPasType; // e.g. TPasAliasType or TPasClassType
  AncestorClassEl: TPasClassType;

  function IsDefaultAncestor(c: TPasClassType; const DefAncestorName: string): boolean;
  begin
    Result:=SameText(c.Name,DefAncestorName)
        and (c.Parent is TPasSection);
  end;

  procedure FindDefaultAncestor(const DefAncestorName, Expected: string);
  var
    CurEl: TPasElement;
  begin
    AncestorClassEl:=nil;
    if SameText(aClass.Name,DefAncestorName) then
      begin
      if IsDefaultAncestor(aClass,DefAncestorName) then exit;
      RaiseXExpectedButYFound(20190106132328,'top level '+DefAncestorName,'nested '+aClass.Name,aClass);
      end;
    CurEl:=FindElementWithoutParams(DefAncestorName,aClass,false,true);
    if not (CurEl is TPasType) then
      RaiseXExpectedButYFound(20180321150128,Expected,GetElementTypeName(CurEl),aClass);
    DirectAncestor:=TPasType(CurEl);
    CurEl:=ResolveAliasType(DirectAncestor);
    if not (CurEl is TPasClassType) then
      RaiseXExpectedButYFound(20170216151941,Expected,GetElementTypeName(DirectAncestor),aClass);
    AncestorClassEl:=TPasClassType(CurEl);
  end;

var
  ClassScope, AncestorClassScope: TPasClassScope;
  AncestorType, El: TPasType;
  i: Integer;
  aModifier, DefAncestorName: String;
  IsSealed, IsDelphi, IsClassAbstract, IsDeferredAncestor: Boolean;
  CanonicalSelf: TPasClassOfType;
  Decl: TPasElement;
  j, TypeParamCnt: integer;
  IntfType, IntfTypeRes, HelperForType, AncestorHelperFor: TPasType;
  ResIntfList, Members: TFPList;
  GroupScope: TPasGroupScope;
  C: TClass;
  LookupClassEl: TPasClassType;
begin
  IsDelphi:=msDelphi in CurrentParser.CurrentModeswitches;
  IsDeferredAncestor:=false;

  if aClass.IsForward then
    begin
    PopGenericParamScope(aClass);

    // check for duplicate forwards
    C:=aClass.Parent.ClassType;
    if C.InheritsFrom(TPasDeclarations) then
      Members:=TPasDeclarations(aClass.Parent).Declarations
    else if (C=TPasClassType) or (C=TPasRecordType) then
      Members:=TPasMembersType(aClass.Parent).Members
    else
      RaiseNotYetImplemented(20180430141934,aClass,GetObjName(aClass.Parent));
    TypeParamCnt:=GetTypeParameterCount(aClass);
    for i:=0 to Members.Count-1 do
      begin
      Decl:=TPasElement(Members[i]);
      if (CompareText(Decl.Name,aClass.Name)<>0)
          or (Decl=aClass) then continue;
      if (Decl is TPasGenericType)
          and (GetTypeParameterCount(TPasGenericType(Decl))<>TypeParamCnt) then
        continue;
      RaiseMsg(20180212144132,nDuplicateIdentifier,sDuplicateIdentifier,
        [Decl.Name,GetElementSourcePosStr(Decl)],aClass);
      end;

    if TypeParamCnt>0 then
      begin
      // A generic forward needs TPasClassScope to store the specialized types.
      // Will later be transferred to the actual class.
      CreateScope(aClass,ScopeClass_Class);
      end;
    exit;
    end;

  // not forward, actual declaration ...

  case aClass.ObjKind of
  okClass:
    begin
    AncestorType:=ResolveAliasType(aClass.AncestorType);
    if (AncestorType is TPasClassType)
        and (TPasClassType(AncestorType).ObjKind=okInterface)
        and not isDelphi then
      begin
      // e.g. type c = class(intf)
      // ObjFPC allows to omit TObject as default ancestor, Delphi does not
      aClass.Interfaces.Insert(0,aClass.AncestorType);
      aClass.AncestorType:=nil;
      end;
    end;
  okInterface:
    begin
    if aClass.IsExternal then
      RaiseMsg(20180321115831,nIllegalQualifier,sIllegalQualifier,['external'],aClass);
    if not (aClass.InterfaceType in [citCom,citCorba]) then
      RaiseMsg(20180321143613,nIllegalQualifier,sIllegalQualifier,
        [CurrentParser.Scanner.CurrentValueSwitch[vsInterfaces]],aClass);
    end;
  okClassHelper,okRecordHelper,okTypeHelper:
    begin
    if aClass.IsExternal then
      RaiseMsg(20190116192722,nIllegalQualifier,sIllegalQualifier,['external'],aClass);
    // A helper may extend a NAMED specialized generic (TFooInteger = TFoo<Integer>;
    // helper for TFooInteger — thlp26), but NOT an inline/anonymous specialization
    // written directly in the helper-for clause (helper for TFoo<Integer> — thlp24).
    if MaximizeFPCCompatibility and (aClass.HelperForType is TPasSpecializeType)
        and (aClass.HelperForType.Name='') then
      RaiseMsg(20260225100004,nXIsNotSupported,sXIsNotSupported,
        ['helper for inline specialized generic type'],aClass);
    HelperForType:=ResolveAliasType(aClass.HelperForType);
    if HelperForType=nil then
      RaiseNotYetImplemented(20191016125557,aClass);
    if (aClass=HelperForType) or (aClass.HasParent(HelperForType)) then
      RaiseMsg(20190118190935,nTypeXIsNotYetCompletelyDefined,
        sTypeXIsNotYetCompletelyDefined,[HelperForType.Name],aClass);
    case aClass.ObjKind of
    okClassHelper:
      begin
      // A class helper may extend a generic type parameter that is constrained
      // to be a class (nested: TFoo<T: class> = class type THelper = class helper
      // for T; ... end; end; — FPC thlp30).
      if (HelperForType is TPasGenericTemplateType)
          and HasClassConstraint(TPasGenericTemplateType(HelperForType)) then
        // ok
      else
        begin
        if not (HelperForType is TPasClassType) then
          RaiseXExpectedButYFound(20190116194751,'class type',GetTypeDescription(aClass.HelperForType),aClass);
        if TPasClassType(HelperForType).ObjKind<>okClass then
          RaiseXExpectedButYFound(20190116194855,'class type',GetTypeDescription(aClass.HelperForType),aClass);
        if TPasClassType(HelperForType).IsForward then
          RaiseMsg(20190116194931,nTypeXIsNotYetCompletelyDefined,
            sTypeXIsNotYetCompletelyDefined,[HelperForType.Name],aClass);
        end;
      end;
    okRecordHelper:
      begin
      if MaximizeFPCCompatibility then
        if not (msDelphi in CurrentParser.CurrentModeswitches)
            and not (msAdvancedRecords in CurrentParser.CurrentModeswitches) then
          RaiseMsg(20260225100005,nXIsNotSupported,sXIsNotSupported,
            ['record helper without modeswitch advancedrecords'],aClass);
      // A record helper may extend a generic type parameter that is constrained
      // to be a record (nested THelper = record helper for T, where T: record —
      // FPC thlp46).
      if (HelperForType is TPasGenericTemplateType)
          and HasRecordConstraint(TPasGenericTemplateType(HelperForType)) then
        // ok
      else if isDelphi then
        begin
        if (HelperForType.ClassType=TPasRecordType)
            or (HelperForType.ClassType=TPasArrayType)
            or (HelperForType.ClassType=TPasSetType)
            or (HelperForType.ClassType=TPasEnumType)
            or (HelperForType.ClassType=TPasRangeType)
            or (HelperForType.ClassType=TPasPointerType)
            then
          // ok
        else if ((HelperForType.ClassType=TPasUnresolvedSymbolRef)
            and (HelperForType.CustomData is TResElDataBaseType)) then
        else
          RaiseMsg(20190116200304,nTypeXCannotBeExtendedByARecordHelper,
            sTypeXCannotBeExtendedByARecordHelper,[GetTypeDescription(HelperForType)],aClass);
        end
      else
        begin
        // mode objfpc
        if (HelperForType.ClassType=TPasRecordType)
            or (HelperForType.ClassType=TPasArrayType)
            or (HelperForType.ClassType=TPasSetType)
            or (HelperForType.ClassType=TPasEnumType)
            or (HelperForType.ClassType=TPasRangeType)
            or (HelperForType.ClassType=TPasPointerType)
            then
          // ok
        else if ((HelperForType.ClassType=TPasUnresolvedSymbolRef)
            and (HelperForType.CustomData is TResElDataBaseType)) then
          // ok - built-in type like Integer, Byte, etc.
        else
          RaiseMsg(20190116200519,nTypeXCannotBeExtendedByARecordHelper,
            sTypeXCannotBeExtendedByARecordHelper,[GetTypeDescription(HelperForType)],aClass);
        end;
      end;
    okTypeHelper:
      begin
      if (HelperForType.ClassType=TPasRecordType)
          or (HelperForType.ClassType=TPasArrayType)
          or (HelperForType.ClassType=TPasSetType)
          or (HelperForType.ClassType=TPasEnumType)
          or (HelperForType.ClassType=TPasRangeType)
          or (HelperForType.ClassType=TPasPointerType)
          then
        // ok
      else if ((HelperForType.ClassType=TPasUnresolvedSymbolRef)
          and (HelperForType.CustomData is TResElDataBaseType)) then
      else if (HelperForType.ClassType=TPasClassType)
          and (TPasClassType(HelperForType).ObjKind in [okClass,okInterface]) then
        begin
        if TPasClassType(HelperForType).IsForward then
          RaiseMsg(20190116200940,nTypeXIsNotYetCompletelyDefined,
            sTypeXIsNotYetCompletelyDefined,[HelperForType.Name],aClass);
        end
      else
        RaiseMsg(20190116200304,nTypeXCannotBeExtendedByATypeHelper,
          sTypeXCannotBeExtendedByATypeHelper,[GetTypeDescription(HelperForType)],aClass);
      end;
    end;
    end;
  okObject:
    begin
    // TP objects: value types with methods — no special validation needed
    end;
  else
    RaiseNotYetImplemented(20161010174638,aClass,'Kind='+ObjKindNames[aClass.ObjKind]);
  end;

  IsSealed:=false;
  IsClassAbstract:=false;
  for i:=0 to aClass.Modifiers.Count-1 do
    begin
    aModifier:=lowercase(aClass.Modifiers[i]);
    case aModifier of
    'sealed': IsSealed:=true;
    'abstract': IsClassAbstract:=true;
    else
      RaiseMsg(20170320190619,nIllegalQualifier,sIllegalQualifier,[aClass.Modifiers[i]],aClass);
    end;
    end;
  if IsSealed and IsClassAbstract then
    RaiseMsg(20260225100012,nInvalidXModifierY,sInvalidXModifierY,
      [GetElementTypeName(aClass),'abstract, sealed'],aClass);

  AncestorClassEl:=nil;
  LookupClassEl:=nil;
  DirectAncestor:=aClass.AncestorType;
  AncestorType:=ResolveAliasType(DirectAncestor);

  if AncestorType=nil then
    begin
    if DirectAncestor<>nil then
      RaiseInternalError(20180321151851,GetObjName(DirectAncestor));
    // use default ancestor
    DefAncestorName:='';
    case aClass.ObjKind of
    okClass:
      begin
      DefAncestorName:='TObject';
      if aClass.IsExternal or IsDefaultAncestor(aClass,DefAncestorName) then
        begin
          // ok, no ancestor
          AncestorClassEl:=nil;
        end
      else
        begin
        // search default ancestor TObject
        FindDefaultAncestor(DefAncestorName,'class type');
        if TPasClassType(AncestorClassEl).ObjKind<>okClass then
          RaiseXExpectedButYFound(20180321145626,'class type',GetElementTypeName(AncestorClassEl),aClass);
        end;
      end;
    okInterface:
      begin
      if aClass.InterfaceType=citCom then
        begin
        if isDelphi then
          DefAncestorName:='IInterface'
        else
          DefAncestorName:='IUnknown';
        if IsDefaultAncestor(aClass,DefAncestorName) then
          AncestorClassEl:=nil
        else
          begin
          // search default ancestor interface
          FindDefaultAncestor(DefAncestorName,'interface type');
          if TPasClassType(AncestorClassEl).ObjKind<>okInterface then
            RaiseXExpectedButYFound(20180321145725,'interface type',
              GetElementTypeName(AncestorClassEl),aClass);
          end;
        end;
      end;
    okObject: ; // TP objects have no default ancestor
    okClassHelper,okRecordHelper,okTypeHelper: ; // no root ancestor
    end;
    end
  else if AncestorType.ClassType<>TPasClassType then
    begin
    // A class may inherit from a class-constrained generic template type parameter
    // (TTest<T: class> = class(T)); its ancestor is unresolved until specialization,
    // so defer ancestor resolution and mark the scope deferred (tgeneric116). The
    // constraint may be the "class" keyword or a concrete class type
    // (T: TBase = class(T)); both guarantee T is a class after specialization (#36843).
    if (AncestorType.ClassType=TPasGenericTemplateType)
        and (HasClassConstraint(TPasGenericTemplateType(AncestorType))
             or HasClassTypeConstraint(TPasGenericTemplateType(AncestorType))) then
      begin
      AncestorClassEl:=nil;
      IsDeferredAncestor:=true;
      // Members are looked up in what T is known to be: its class-type
      // constraint, else the root class (Self.Free in lazutils' generics).
      LookupClassEl:=GetClassTypeConstraint(TPasGenericTemplateType(AncestorType));
      if LookupClassEl=nil then
        begin
        Decl:=ResolveAliasType(TPasType(FindElementWithoutParams('TObject',aClass,false,true)));
        if Decl is TPasClassType then
          LookupClassEl:=TPasClassType(Decl);
        end;
      end
    else
      RaiseXExpectedButYFound(20170216151944,'class type',GetTypeDescription(AncestorType),aClass);
    end
  else if aClass=AncestorType then
    RaiseMsg(20170525125854,nAncestorCycleDetected,sAncestorCycleDetected,[],aClass)
  else
    begin
    AncestorClassEl:=TPasClassType(AncestorType);
    // record helper and type helper are the same value-type-helper concept, so a
    // type helper may inherit from a record helper and vice versa (tthlp25).
    if (AncestorClassEl.ObjKind<>aClass.ObjKind)
        and not ((aClass.ObjKind in [okRecordHelper,okTypeHelper])
             and (AncestorClassEl.ObjKind in [okRecordHelper,okTypeHelper])) then
      RaiseXExpectedButYFound(20180321152107,GetElementTypeName(aClass)+' type',
        GetElementTypeName(AncestorClassEl)+' type',aClass);

    if aClass.ObjKind in okAllHelpers then
      begin
      // record helper inheritance only in ObjFPC
      if (aClass.ObjKind = okRecordHelper)
          and (msDelphi in CurrentParser.CurrentModeswitches) then
        RaiseMsg(20260225100006,nXIsNotSupported,sXIsNotSupported,
          ['record helper inheritance in Delphi mode'],aClass);
      if aClass.ObjKind in [okRecordHelper, okTypeHelper] then
        begin
        // For type/record helpers, 'type X' creates a distinct type
        // Do NOT resolve type aliases - compare raw HelperForType identity
        if aClass.HelperForType <> AncestorClassEl.HelperForType then
          begin
          HelperForType:=ResolveAliasType(aClass.HelperForType);
          AncestorHelperFor:=ResolveAliasType(AncestorClassEl.HelperForType);
          if IsSameType(HelperForType,AncestorHelperFor,prraNone) then
            begin
            // Same resolved type but different identity
            // Child extending base type when ancestor extends type-alias is invalid
            // (child must be same or more specific than ancestor)
            if (AncestorClassEl.HelperForType is TPasAliasType)
                and not (aClass.HelperForType is TPasAliasType) then
              RaiseMsg(20260225100007,nDerivedXMustExtendASubClassY,sDerivedXMustExtendASubClassY,
                [GetElementTypeName(aClass),AncestorClassEl.HelperForType.Name],aClass);
            end
          else if (HelperForType is TPasClassType)
              and (AncestorHelperFor is TPasClassType)
              and (CheckClassIsClass(HelperForType,AncestorHelperFor)<>cIncompatible) then
            // type helper for descendant class of ancestor's HelperForType -> ok
          else
            RaiseMsg(20190116203931,nDerivedXMustExtendASubClassY,sDerivedXMustExtendASubClassY,
              [GetElementTypeName(aClass),AncestorClassEl.HelperForType.Name],aClass);
          end;
        end
      else
        begin
        // Class helpers: existing logic with full alias resolution
        HelperForType:=ResolveAliasType(aClass.HelperForType);
        AncestorHelperFor:=ResolveAliasType(AncestorClassEl.HelperForType);
        if IsSameType(HelperForType,AncestorHelperFor,prraNone) then
          // ok
        else if (HelperForType is TPasClassType)
            and (AncestorHelperFor is TPasClassType)
            and (CheckClassIsClass(HelperForType,AncestorHelperFor)<>cIncompatible) then
          // ok
        else
          RaiseMsg(20190116203931,nDerivedXMustExtendASubClassY,sDerivedXMustExtendASubClassY,
            [GetElementTypeName(aClass),AncestorClassEl.HelperForType.Name],aClass);
        end;
      end;
    EmitTypeHints(aClass,AncestorClassEl);
    end;

  AncestorClassScope:=nil;
  if AncestorClassEl=nil then
    begin
    // root class e.g. TObject, IUnknown, helper
    end
  else
    begin
    // inherited class
    if AncestorClassEl.IsForward then
      RaiseMsg(20170216151947,nCantUseForwardDeclarationAsAncestor,
        sCantUseForwardDeclarationAsAncestor,[AncestorClassEl.Name],aClass);
    if aClass.IsExternal and not AncestorClassEl.IsExternal then
      RaiseMsg(20170321144035,nAncestorIsNotExternal,sAncestorIsNotExternal,
        [AncestorClassEl.Name],aClass);
    // A specialization cascade can reach a class before the specialization of
    // its own ancestor has been finished, leaving that ancestor without a scope
    // (rtl-generics' TList<T> through TCustomListWithPointers<T>). Finish it
    // now - every use below needs its members, and the cycle check above has
    // already ruled out a real ancestor cycle.
    if (AncestorClassEl.CustomData=nil) and not AncestorClassEl.IsForward
        and (Pos('<',AncestorClassEl.Name)>0) then
      FinishAncestors(AncestorClassEl);
    AncestorClassScope:=AncestorClassEl.CustomData as TPasClassScope;
    // If the ancestor itself has a deferred (class-constrained type-parameter)
    // ancestor, this class's inherited members are equally unresolved until
    // specialization, so its override checks must be deferred too — propagate the
    // flag transitively through intermediate partial specializations (tgeneric116).
    if (AncestorClassScope<>nil)
        and (pcsfDeferredAncestor in AncestorClassScope.Flags) then
      IsDeferredAncestor:=true;
    // Descending from a GENERIC that still has its type parameters: the
    // ancestor reference was a partial specialization answered with the generic
    // (PartialSpecsAsGeneric), so what is inherited - and every signature in it
    // - is only settled when this class is specialized. That is what the
    // deferred-ancestor flag already means, and the override checks it guards
    // are re-run on the real specialization.
    if (AncestorClassEl is TPasGenericType)
        and (GetTypeParameterCount(TPasGenericType(AncestorClassEl))>0) then
      IsDeferredAncestor:=true;
    if (AncestorClassScope<>nil) and (pcsfSealed in AncestorClassScope.Flags) then
      RaiseMsg(20170320191735,nCannotCreateADescendantOfTheSealedXY,
        sCannotCreateADescendantOfTheSealedXY,
        [GetElementTypeName(AncestorClassEl),AncestorClassEl.Name],aClass);
    // check for cycle
    El:=AncestorClassEl;
    repeat
      if El=aClass then
        RaiseMsg(20170216151949,nAncestorCycleDetected,sAncestorCycleDetected,[],aClass);
      if (El.ClassType=TPasAliasType)
      or (El.ClassType=TPasTypeAliasType)
      or (El.ClassType=TPasSpecializeType)
      then
        El:=TPasAliasType(El).DestType
      else if El.ClassType=TPasClassType then
        El:=TPasClassType(El).AncestorType
      else if El.ClassType=TPasGenericTemplateType then
        break // ancestor chain reaches a template type, no cycle possible
      else
        RaiseNotYetImplemented(20190825195203,aClass,GetObjName(El));
    until El=nil;

    if not aClass.RTTIVisibility.Explicit then
      begin
      // inherit extended RTTI visibilities
      aClass.RTTIVisibility.Fields:=AncestorClassEl.RTTIVisibility.Fields+aClass.RTTIVisibility.Fields;
      aClass.RTTIVisibility.Methods:=AncestorClassEl.RTTIVisibility.Methods+aClass.RTTIVisibility.Methods;
      aClass.RTTIVisibility.Properties:=AncestorClassEl.RTTIVisibility.Properties+aClass.RTTIVisibility.Properties;
      end;
    end;

  if TopScope is TPasGenericParamsScope then
    PopGenericParamScope(aClass);

  // start scope for members
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.FinishAncestors ',GetObjName(aClass.CustomData));
  {$ENDIF}
  if aClass.CustomData=nil then
    ClassScope:=TPasClassScope(CreateScope(aClass,ScopeClass_Class))
  else
    begin
    // has already the scope, e.g. scope moved from a generic forward
    ClassScope:=aClass.CustomData as TPasClassScope;
    if pcsfAncestorResolved in ClassScope.Flags then
      RaiseNotYetImplemented(20190803203715,aClass);
    end;
  Include(ClassScope.Flags,pcsfAncestorResolved);
  if IsSealed then
    Include(ClassScope.Flags,pcsfSealed);
  if IsDeferredAncestor then
    Include(ClassScope.Flags,pcsfDeferredAncestor);
  AddGenericTemplateIdentifiers(aClass.GenericTemplateTypes,ClassScope);
  ClassScope.DirectAncestor:=DirectAncestor;
  if AncestorClassEl<>nil then
    if AncestorClassScope<>nil then
      begin
      ClassScope.AncestorScope:=AncestorClassScope;
      ClassScope.DefaultProperty:=AncestorClassScope.DefaultProperty;
      if pcsfPublished in AncestorClassScope.Flags then
        Include(ClassScope.Flags,pcsfPublished);
      ClassScope.AbstractProcs:=copy(AncestorClassScope.AbstractProcs);
      end
    else
      // The ancestor has no scope YET: a specialization cascade can reach a
      // class before the specialization of its own ancestor has been finished
      // (rtl-generics' TList<T> through TCustomListWithPointers<T>). Its
      // inherited members are unknown until then, which is exactly what the
      // deferred-ancestor flag means - and dereferencing the nil scope here was
      // an outright Access violation.
      Include(ClassScope.Flags,pcsfDeferredAncestor);
  // class(T) in a generic: search members in T's constraint class. The class
  // stays deferred, so override checks still wait for the specialization.
  if (AncestorClassEl=nil) and (LookupClassEl<>nil)
      and (LookupClassEl.CustomData is TPasClassScope) then
    ClassScope.AncestorScope:=TPasClassScope(LookupClassEl.CustomData);
  if bsTypeInfo in CurrentParser.Scanner.CurrentBoolSwitches then
    Include(ClassScope.Flags,pcsfPublished);

  if aClass.ObjKind in ([okClass]+okAllHelpers) then
    begin
    // create canonical class-of for the "Self" in non static class functions
    CanonicalSelf:=TPasClassOfType(CreateOwnedElement(TPasClassOfType,'Self',aClass));
    ClassScope.CanonicalClassOf:=CanonicalSelf;
    CanonicalSelf.DestType:=aClass;
    CanonicalSelf.Visibility:=visStrictPrivate;
    CanonicalSelf.SourceFilename:=aClass.SourceFilename;
    CanonicalSelf.SourceLinenumber:=aClass.SourceLinenumber;
    end;

  // push scope (must be done after setting aClass.AncestorScope)
  GroupScope:=PushGroupScope(aClass);
  GroupScope.VisibilityContext:=aClass;

  // check interfaces
  if aClass.Interfaces.Count>0 then
    begin
    if not (aClass.ObjKind in [okClass]) then
      RaiseXExpectedButYFound(20180322001341,'one ancestor',
        IntToStr(1+aClass.Interfaces.Count),aClass);
    if aClass.IsExternal then
      RaiseMsg(20180324183641,nIllegalQualifier,sIllegalQualifier,['external'],aClass);
    ResIntfList:=TFPList.Create;
    try
      for i:=0 to aClass.Interfaces.Count-1 do
        begin
        IntfType:=TPasType(aClass.Interfaces[i]);
        IntfTypeRes:=ResolveAliasType(IntfType);
        if IntfTypeRes=nil then
          RaiseMsg(20180322140044,nCantUseForwardDeclarationAsAncestor,
            sCantUseForwardDeclarationAsAncestor,[IntfType.Name],aClass);
        if not (IntfTypeRes is TPasClassType) then
          RaiseXExpectedButYFound(20180322001051,'interface type',
            GetElementTypeName(IntfTypeRes)+' type',aClass);
        if TPasClassType(IntfTypeRes).ObjKind<>okInterface then
          RaiseXExpectedButYFound(20180322001143,'interface type',
            GetElementTypeName(IntfTypeRes)+' type',aClass);
        j:=ResIntfList.IndexOf(IntfTypeRes);
        if j>=0 then
          RaiseMsg(20180322001505,nDuplicateIdentifier,sDuplicateIdentifier,
            [IntfType.Name,IntToStr(j+1)],aClass); // todo: jump to interface list
        ResIntfList.Add(IntfTypeRes);
        end;
    finally
      ResIntfList.Free;
    end;
    // create interfaces maps
    ClassScope.Interfaces:=TFPList.Create;
    ClassScope.Interfaces.Count:=aClass.Interfaces.Count;
    end;
end;

procedure TPasResolver.FinishMethodResolution(El: TPasMethodResolution);
var
  ResolvedEl: TPasResolverResult;
  aClass, IntfType: TPasClassType;
  i: Integer;
  IntfProc: TPasProcedure;
  Expr: TPasExpr;
  ProcName: String;
  IntfScope: TPasClassScope;
  Identifier: TPasIdentifier;
begin
  // procedure InterfaceName.InterfaceProc = ...
  // check InterfaceName
  ResolveExpr(El.InterfaceName,rraRead);
  ComputeElement(El.InterfaceName,ResolvedEl,[rcType,rcNoImplicitProc]);
  if not (ResolvedEl.IdentEl is TPasType) then
    RaiseXExpectedButYFound(20180323132601,'interface type',
      GetResolverResultDescription(ResolvedEl),El.InterfaceName);
  aClass:=El.Parent as TPasClassType;
  i:=IndexOfImplementedInterface(aClass,TPasType(ResolvedEl.IdentEl));
  if i<0 then
    RaiseXExpectedButYFound(20180323133055,'interface type',
      GetResolverResultDescription(ResolvedEl),El.InterfaceName);
  IntfType:=TPasClassType(ResolveAliasType(TPasClassType(aClass.Interfaces[i])));
  // check InterfaceProc
  Expr:=El.InterfaceProc;
  if not (Expr is TPrimitiveExpr) then
    RaiseXExpectedButYFound(20180327152808,'method name',GetElementTypeName(Expr),Expr);
  if TPrimitiveExpr(Expr).Kind<>pekIdent then
    RaiseXExpectedButYFound(20180327152841,'method name',GetElementTypeName(Expr),Expr);
  ProcName:=TPrimitiveExpr(Expr).Value;
  IntfScope:=IntfType.CustomData as TPasClassScope;
  IntfProc:=nil;
  while IntfScope<>nil do
    begin
    Identifier:=IntfScope.FindLocalIdentifier(ProcName);
    while Identifier<>nil do
      begin
      if not (Identifier.Element is TPasProcedure) then
        RaiseXExpectedButYFound(20180327153110,'interface method',GetElementTypeName(Identifier.Element),Expr);
      IntfProc:=TPasProcedure(Identifier.Element);
      if IntfProc.ClassType=El.ProcClass then
        break;
      Identifier:=Identifier.NextSameIdentifier;
      end;
    IntfScope:=IntfScope.AncestorScope;
    end;
  if IntfProc=nil then
    RaiseIdentifierNotFound(20180327153044,ProcName,Expr);
  CreateReference(IntfProc,Expr,rraRead);
  if IntfProc.ClassType<>El.ProcClass then
    RaiseXExpectedButYFound(20180323144107,GetElementTypeName(El.ProcClass),GetElementTypeName(IntfProc),El.InterfaceProc);
  // Note: do not create map here. CheckImplements in FinishProperty must be called before.

  // El.ImplementationProc is resolved in FinishClassType
end;

procedure TPasResolver.FinishAttributes(El: TPasAttributes);
var
  IsArg: boolean;
  i, j: Integer;
  NameExpr, Expr: TPasExpr;
  Bin: TBinaryExpr;
  LeftResolved, ParamResolved: TPasResolverResult;
  aModule: TPasModule;
  LTypeEl: TPasType;
  AttrName: String;
  Data: TPRFindData;
  CurEl, DeclEl: TPasElement;
  ClassEl: TPasClassType;
  aConstructor: TPasConstructor;
  Args: TFPList;
  AttrRef, ParamRef: TResolvedReference;
  DotScope: TPasDotBaseScope;
  Params: TPasExprArray;
begin
  IsArg:=El.Parent is TPasArgument;
  for i:=0 to length(El.Calls)-1 do
    begin
    NameExpr:=El.Calls[i];
    {$IFDEF VerbosePasResolver}
    //writeln('TPasResolver.FinishAttributes El.Calls[',i,']=',GetObjName(NameExpr));
    {$ENDIF}
    if IsArg and (NameExpr.Kind=pekIdent)
        and (SameText(TPrimitiveExpr(NameExpr).Value,'ref')) then
    begin
      if TPasArgument(El.Parent).Access=argConstRef then
        continue; // const [ref] arg
    end;

    if NameExpr is TParamsExpr then
      NameExpr:=TParamsExpr(NameExpr).Value;
    DotScope:=nil;
    if NameExpr is TBinaryExpr then
      begin
      Bin:=TBinaryExpr(NameExpr);
      ResolveExpr(Bin.Left,rraRead);
      ComputeElement(Bin.Left,LeftResolved,[rcType,rcSetReferenceFlags]);
      if LeftResolved.BaseType=btModule then
        begin
        // e.g. unitname.identifier
        // => search in interface and if this is our module in the implementation
        aModule:=NoNil(LeftResolved.IdentEl) as TPasModule;
        DotScope:=PushModuleDotScope(aModule);
        end
      else if (LeftResolved.BaseType=btContext)
          and (LeftResolved.IdentEl is TPasType)
          and (LeftResolved.LoTypeEl is TPasMembersType) then
        begin
        // classtype.identifier or recordtype.identifier
        LTypeEl:=LeftResolved.LoTypeEl;
        if LTypeEl.ClassType=TPasClassType then
          begin
          DotScope:=PushClassDotScope(TPasClassType(LTypeEl));
          DotScope.OnlyTypeMembers:=true;
          end
        else if LTypeEl.ClassType=TPasRecordType then
          begin
          DotScope:=PushRecordDotScope(TPasRecordType(LTypeEl),LeftResolved.HiTypeEl);
          DotScope.OnlyTypeMembers:=true;
          end
        else
          RaiseNotYetImplemented(20190221124930,Bin);
        end
      else
        RaiseMsg(20190221102049,nXExpectedButYFound,sXExpectedButYFound,
          ['module or type',GetResolverResultDescription(LeftResolved,true)],NameExpr);
      NameExpr:=Bin.Right;
      end;
    // find attribute class
    if not IsNameExpr(NameExpr) then
      RaiseMsg(20190221125204,nXExpectedButYFound,sXExpectedButYFound,
        ['identifier',GetElementTypeName(Bin)],NameExpr);
    AttrName:=TPrimitiveExpr(NameExpr).Value;
    CurEl:=nil;
    if not SameText(RightStr(AttrName,length('Attribute')),'Attribute') then
      begin
      // first search AttrName+'Attribute'
      CurEl:=FindFirstEl(AttrName+'Attribute',Data,NameExpr);
      end;
    // then search the name
    if CurEl=nil then
      CurEl:=FindFirstEl(AttrName,Data,NameExpr);
    if DotScope<>nil then
      PopScope;
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishAttributes Found Attr "'+AttrName+'"=',GetObjName(CurEl),' TopScope=',GetObjName(TopScope));
    {$ENDIF}

    // check if found element is a TCustomAttribute
    if CurEl=nil then
      begin
      if MaximizeFPCCompatibility then
        RaiseMsg(20190221144613,nUnknownCustomAttributeX,sUnknownCustomAttributeX, [AttrName],NameExpr);
      LogMsg(20190221144613,mtWarning,nUnknownCustomAttributeX,sUnknownCustomAttributeX, [AttrName],NameExpr);
      continue;
      end;
    // An attribute class may be reached through a type ALIAS: vcl-compat's
    // system.json.serializers re-exports
    // `JsonNameAttribute = System.JSON.Types.JsonNameAttribute`, and fpc
    // accepts `[JsonName(...)]` through it.
    if (CurEl is TPasType) and not (CurEl.ClassType=TPasClassType) then
      CurEl:=ResolveAliasType(TPasType(CurEl));
    if not IsCustomAttribute(CurEl) then
      RaiseMsg(20190221130400,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        [GetElementTypeName(CurEl),'TCustomAttribute'],NameExpr);
    ClassEl:=TPasClassType(CurEl);
    AttrRef:=CreateReference(ClassEl,NameExpr,rraRead);
    if ClassEl.IsAbstract then
      // Delphi silently skips attributes using abstract classes/methods
      LogMsg(20190223194424,mtWarning,nAttributeIgnoredBecauseAbstractX,
        sAttributeIgnoredBecauseAbstractX,['class'],NameExpr);

    // search constructor "Create" using the params
    DotScope:=PushClassDotScope(ClassEl);
    DotScope.OnlyTypeMembers:=true;
    Expr:=El.Calls[i];
    if Expr is TParamsExpr then
      begin
      // attribute with params
      if Expr.Kind<>pekFuncParams then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.FinishAttributes ',ExprKindNames[Expr.Kind]);
        {$ENDIF}
        RaiseMsg(20190223195605,nXExpectedButYFound,sXExpectedButYFound,
          ['(','['],Expr);
        end;
      // first resolve params
      ResolveParamsExprParams(TParamsExpr(Expr));
      // then resolve call 'Create'
      FFindingAttributeCreate:=true;
      try
        ResolveFuncParamsExprName(Expr,nil,TParamsExpr(Expr),rraRead,'Create');
      finally
        FFindingAttributeCreate:=false;
      end;
      // then check that each parameter is a constant expression
      Params:=TParamsExpr(Expr).Params;
      for j:=0 to length(Params)-1 do
        ComputeElement(Params[j],ParamResolved,[rcConstant]);
      // check if call is constructor
      ParamRef:=Expr.CustomData as TResolvedReference;
      DeclEl:=ParamRef.Declaration;
      if DeclEl.ClassType<>TPasConstructor then
        RaiseXExpectedButYFound(20190221150212,'constructor Create',GetElementTypeName(DeclEl),NameExpr);
      aConstructor:=TPasConstructor(DeclEl);
      end
    else
      begin
      // attribute without params
      // -> resolve call 'Create'
      FFindingAttributeCreate:=true;
      try
        DeclEl:=FindElementWithoutParams('Create',Data,NameExpr,false,true);
      finally
        FFindingAttributeCreate:=false;
      end;
      if DeclEl=nil then
        RaiseIdentifierNotFound(20190221144516,'Create',NameExpr);
      // check call is constructor
      if DeclEl.ClassType<>TPasConstructor then
        RaiseXExpectedButYFound(20190221145003,'constructor Create',
          GetElementTypeName(DeclEl),NameExpr);
      aConstructor:=TPasConstructor(DeclEl);
      // An attribute class with no accessible (public) constructor cannot be
      // applied: a private inherited Create (e.g. TCustomAttribute.Create) is
      // not visible to the attribute user (tcustomattr9).
      if (aConstructor.Visibility in [visPrivate,visStrictPrivate])
          and (aConstructor.Parent<>ClassEl) then
        RaiseIdentifierNotFound(20260301120000,'Create',NameExpr);
      // check constructor without needed args
      Args:=aConstructor.ProcType.Args;
      if (Args.Count>0) and (TPasArgument(Args[0]).ValueExpr=nil) then
        RaiseMsg(20190221145407,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[aConstructor.Name],Expr);
      // check if ancestor constructor is hidden by a derived Create with required args
      if MaximizeFPCCompatibility and (aConstructor.Parent <> ClassEl) then
        for j := 0 to ClassEl.Members.Count - 1 do
          begin
          CurEl := TPasElement(ClassEl.Members[j]);
          if (CurEl is TPasConstructor)
              and SameText(TPasConstructor(CurEl).Name, 'Create')
              and not TPasConstructor(CurEl).IsOverload then
            begin
            Args := TPasConstructor(CurEl).ProcType.Args;
            if (Args.Count > 0) and (TPasArgument(Args[0]).ValueExpr = nil) then
              RaiseMsg(20260226100003,nWrongNumberOfParametersForCallTo,
                sWrongNumberOfParametersForCallTo,
                [TPasConstructor(CurEl).Name],Expr);
            break;
            end;
          end;
      end;
    if aConstructor.IsAbstract then
      LogMsg(20190223193645,mtWarning,nAttributeIgnoredBecauseAbstractX,
        sAttributeIgnoredBecauseAbstractX,['mrthod'],NameExpr);
    // store reference to constructor in NameExpr
    if AttrRef.Context<>nil then
      RaiseNotYetImplemented(20190221164717,NameExpr,GetObjName(AttrRef.Context));
    AttrRef.Context:=TResolvedRefCtxAttrProc.Create;
    TResolvedRefCtxAttrProc(AttrRef.Context).Proc:=aConstructor;
    PopScope;
    end;
end;

procedure TPasResolver.FinishExportSymbol(El: TPasExportSymbol);

  procedure CheckConstExpr(Expr: TPasExpr; Kinds: TREVKinds; const Expected: string);
  var
    Value: TResEvalValue;
    ResolvedEl: TPasResolverResult;
  begin
    if Expr=nil then exit;
    ResolveExpr(Expr,rraRead);
    Value:=Eval(Expr,[refConst]);
    if (Value<>nil) and (Value.Kind in Kinds) then
      begin
      ReleaseEvalValue(Value);
      exit;
      end;
    ReleaseEvalValue(Value);
    ComputeElement(Expr,ResolvedEl,[rcConstant]);
    RaiseXExpectedButYFound(20210101194628,Expected,GetTypeDescription(ResolvedEl),Expr);
  end;

var
  Expr: TPasExpr;
  DeclEl, DuplicateEl: TPasElement;
  FindData: TPRFindData;
  Ref: TResolvedReference;
  ResolvedEl: TPasResolverResult;
  Scope: TPasIdentifierScope;
  ScopeIdent: TPasIdentifier;
begin
  Expr:=El.NameExpr;
  if Expr<>nil then
    begin
    ResolveExpr(Expr,rraRead);
    ComputeElement(Expr,ResolvedEl,[rcConstant]);
    DeclEl:=ResolvedEl.IdentEl;
    if DeclEl=nil then
      RaiseMsg(20210103012907,nXExpectedButYFound,sXExpectedButYFound,['symbol',GetTypeDescription(ResolvedEl)],Expr);
    if DeclEl.Parent=nil then
      RaiseMsg(20220206142147,nSymbolCannotBeExportedFromALibrary,
        sSymbolCannotBeExportedFromALibrary,[],El);
    if DeclEl.Parent is TPasSection then
      // global
    else if (DeclEl.Parent is TPasMembersType) and (DeclEl is TPasProcedure)
        and (TPasProcedure(DeclEl).IsStatic) then
      // static proc
    else
      RaiseMsg(20210103012908,nXExpectedButYFound,sXExpectedButYFound,['global symbol',GetElementTypeName(DeclEl)],Expr);
    end
  else
    begin
    FindFirstEl(El.Name,FindData,El);
    DeclEl:=FindData.Found;
    if DeclEl=nil then
      RaiseMsg(20210103002747,nIdentifierNotFound,sIdentifierNotFound,[El.Name],El);
    if not (DeclEl.Parent is TPasSection) then
      RaiseMsg(20210103003244,nXExpectedButYFound,sXExpectedButYFound,['global symbol',GetObjPath(DeclEl)],El);
    Ref:=CreateReference(DeclEl,El,rraRead,@FindData);
    CheckFoundElement(FindData,Ref);
    end;

  if DeclEl.Parent.CustomData is TPasIdentifierScope then
    begin
    Scope:=DeclEl.Parent.CustomData as TPasIdentifierScope;
    ScopeIdent:=Scope.FindLocalIdentifier(DeclEl.Name);
    if (ScopeIdent=nil) then
      RaiseNotYetImplemented(20210106103001,El,GetObjPath(DeclEl));
    if ScopeIdent.NextSameIdentifier<>nil then
      if DeclEl is TPasProcedure then
        RaiseMsg(20210106103320,nCantDetermineWhichOverloadedFunctionToCall,
          sCantDetermineWhichOverloadedFunctionToCall,[],El)
      else
        begin
        if ScopeIdent.Element=DeclEl then
          DuplicateEl:=ScopeIdent.NextSameIdentifier.Element
        else
          DuplicateEl:=ScopeIdent.Element;
        RaiseMsg(20220206141619,nDuplicateIdentifier,
          sDuplicateIdentifier,[DuplicateEl.Name,GetElementSourcePosStr(DuplicateEl)],El);
        end;
    end;

  // check index and name
  CheckConstExpr(El.ExportIndex,[revkInt,revkUInt],'integer');
  CheckConstExpr(El.ExportName,revkAllStrings,'string');
end;

procedure TPasResolver.FinishProcParamAccess(ProcType: TPasProcedureType;
  Params: TParamsExpr);
var
  ParamAccess: TResolvedRefAccess;
  i: Integer;
  ArrParams: TPasExprArray;
  Args: TFPList;
begin
  ArrParams:=Params.Params;
  Args:=ProcType.Args;
  for i:=0 to length(ArrParams)-1 do
    begin
    ParamAccess:=rraRead;
    if i<Args.Count then
      case TPasArgument(Args[i]).Access of
      argVar: ParamAccess:=rraVarParam;
      argOut: ParamAccess:=rraOutParam;
      end;
    AccessExpr(ArrParams[i],ParamAccess);
    end;
  CheckCallProcCompatibility(ProcType,Params,false,true);
end;

procedure TPasResolver.FinishPropertyParamAccess(Params: TParamsExpr;
  Prop: TPasProperty);
var
  i: Integer;
  ParamAccess: TResolvedRefAccess;
begin
  for i:=0 to length(Params.Params)-1 do
    begin
    ParamAccess:=rraRead;
    if i<Prop.Args.Count then
      case TPasArgument(Prop.Args[i]).Access of
      argVar: ParamAccess:=rraVarParam;
      argOut: ParamAccess:=rraOutParam;
      end;
    FinishCallArgAccess(Params.Params[i],ParamAccess);
    end;
end;

procedure TPasResolver.FinishCallArgAccess(Expr: TPasExpr;
  Access: TResolvedRefAccess);
var
  ResolvedEl: TPasResolverResult;
  Flags: TPasResolverComputeFlags;
begin
  AccessExpr(Expr,Access);
  Flags:=[rcSetReferenceFlags];
  if Access<>rraRead then
    Include(Flags,rcNoImplicitProc);
  ComputeElement(Expr,ResolvedEl,Flags);
end;

procedure TPasResolver.FinishInitialFinalization(El: TPasImplBlock);
begin
  if El=nil then ;
  CheckTopScope(ScopeClass_InitialFinalization);
  PopScope;
end;
