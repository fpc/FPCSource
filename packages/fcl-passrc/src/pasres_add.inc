{%MainUnit pasresolver.pp}
{ **********************************************************************
    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - adding all kinds of symbols

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }


procedure TPasResolver.AddModule(El: TPasModule);
var
  C: TClass;
  ModScope: TPasModuleScope;
begin
  if Hub=nil then
    RaiseNotYetImplemented(20200815182122,El);
  if TopScope<>DefaultScope then
    RaiseInvalidScopeForElement(20160922163504,El);
  ModScope:=TPasModuleScope(PushScope(El,FScopeClass_Module));
  ModScope.VisibilityContext:=El;
  ModScope.FirstName:=FirstDottedIdentifier(El.Name);
  C:=El.ClassType;
  if (C=TPasProgram) or (C=TPasLibrary) or (C=TPasPackage) then
    FDefaultNameSpace:=ChompDottedIdentifier(El.Name)
  else
    FDefaultNameSpace:='';
  ModScope.BoolSwitches:=CurrentParser.Scanner.CurrentBoolSwitches;
end;

procedure TPasResolver.AddSection(El: TPasSection);
// TInterfaceSection, TImplementationSection, TProgramSection, TLibrarySection
// Note: implementation scope is within the interface scope
var
  Scope: TPasSectionScope;
  aModule: TPasModule;
begin
  if TopScope is TPasSectionScope then
    FinishSection(TPasSectionScope(TopScope).Element as TPasSection);
  if TopScope is TPasModuleScope then
    TPasModuleScope(TopScope).BoolSwitches:=CurrentParser.Scanner.CurrentBoolSwitches;
  {$IFDEF VerbosePasResolver}
  if FPendingForwardProcs.IndexOf(El)=0 then
    RaiseNotYetImplemented(20190804114718,El);
  {$ENDIF}
  FPendingForwardProcs.Add(El); // check forward declarations at the end
  Scope:=TPasSectionScope(PushScope(El,ScopeClass_Section));
  Scope.BoolSwitches:=CurrentParser.Scanner.CurrentBoolSwitches;
  Scope.ModeSwitches:=CurrentParser.Scanner.CurrentModeSwitches;
  if El.ClassType=TImplementationSection then
    begin
    aModule:=El.GetModule;
    if (aModule<>nil) and (aModule.InterfaceSection<>nil)
        and (aModule.InterfaceSection.CustomData is TPasSectionScope) then
      TPasSectionScope(aModule.InterfaceSection.CustomData).ImplUsesScopes:=Scope.UsesScopes;
    end;
end;

procedure TPasResolver.AddInitialFinalizationSection(El: TPasImplBlock);
begin
  PushScope(El,ScopeClass_InitialFinalization);
end;

procedure TPasResolver.AddType(El: TPasType);
begin
  if (El.Name='') then exit; // sub type
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddType El=',GetObjName(El),' El.Parent=',GetObjName(El.Parent));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160922163506,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
end;

procedure TPasResolver.AddArrayType(El: TPasArrayType; TypeParams: TFPList);
var
  Scope: TPasArrayScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddArrayType ',GetObjName(El),' Parent=',GetObjName(El.Parent));
  {$ENDIF}
  if TypeParams<>nil then
    begin
    El.SetGenericTemplates(TypeParams);
    TypeParams:=El.GenericTemplateTypes;
    CheckGenericTemplateTypes(El);
    end;
  PopGenericParamScope(El);

  if El.Name<>'' then
    begin
    if not (TopScope is TPasIdentifierScope) then
      RaiseInvalidScopeForElement(20190812215622,El);

    AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);

    if TypeParams<>nil then
      begin
      Scope:=TPasArrayScope(PushScope(El,ScopeClass_Array));
      AddGenericTemplateIdentifiers(TypeParams,Scope);
      end;
    end
  else if TypeParams<>nil then
    RaiseNotYetImplemented(20190812215851,El); // anonymous generic array type
end;

procedure TPasResolver.AddRecordType(El: TPasRecordType; TypeParams: TFPList);
var
  Scope: TPasRecordScope;
  C: TClass;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddRecordType ',GetObjName(El),' Parent=',GetObjName(El.Parent));
  {$ENDIF}
  C:=El.Parent.ClassType;
  if (El.Name='') then
    begin
    // anonymous record
    if (C=TPasVariable)
        or (C=TPasConst)
        or (C=TPasVariant)
        or (C=TPasArrayType) then // array element type, e.g. "array[..] of record..end"
      // ok
    else
      RaiseMsg(20220321224331,nCannotNestAnonymousX,sCannotNestAnonymousX,['record'],El);
    if TypeParams<>nil then
      RaiseNotYetImplemented(20220322220743,El);
    DeanonymizeType(El);
    end;

  if TypeParams<>nil then
    begin
    El.SetGenericTemplates(TypeParams);
    TypeParams:=El.GenericTemplateTypes;
    CheckGenericTemplateTypes(El);
    end;
  PopGenericParamScope(El);

  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160922163508,El);

  if El.Name<>'' then begin
    AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
    {$IFDEF VerbosePasResolver}
    if FPendingForwardProcs.IndexOf(El)=0 then
      RaiseNotYetImplemented(20190804114737,El);
    {$ENDIF}
    FPendingForwardProcs.Add(El); // check forward declarations at the end
  end;

  if C<>TPasVariant then
    begin
    Scope:=TPasRecordScope(PushScope(El,ScopeClass_Record));
    Scope.VisibilityContext:=El;
    if TypeParams<>nil then
      begin
      // generic array
      if El.Name='' then
        RaiseNotYetImplemented(20190812220821,El);
      AddGenericTemplateIdentifiers(TypeParams,Scope);
      end;
    end;
end;

procedure TPasResolver.AddRecordVariant(El: TPasVariant);
begin
  if El=nil then ;
end;

procedure TPasResolver.AddClassType(El: TPasClassType; TypeParams: TFPList);
// Note: IsForward is not yet set!
var
  Duplicate: TPasIdentifier;
  ForwardDecl: TPasClassType;
  CurScope, LocalScope: TPasIdentifierScope;
  GenTemplCnt, i, j: Integer;
  ClassScope: TPasClassScope;
  ForwGenTempl, ActGenTempl: TPasGenericTemplateType;
  ForwConstraints, ActConstraints: TPasElementArray;
  DuplEl, ForwConstraint, ActConstraint: TPasElement;
  ForwToken, ActToken: TToken;
  ForwConstraintResolved, ActConstraintResolved: TPasResolverResult;
begin
  // Beware: El.ObjKind is not yet set!
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.AddClassType ',GetObjName(El),' Parent=',GetObjName(El.Parent),' ',GetElementSourcePosStr(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160922163510,El);
  if TypeParams=nil then
    begin
    GenTemplCnt:=0;
    if TopScope is TPasGenericParamsScope then
      // Synthetic non-generic class ($FuncRef$M<..> interface) added while an
      // enclosing generic's params scope is on top; register beneath it.
      CurScope:=TPasIdentifierScope(Scopes[ScopeCount-2])
    else
      CurScope:=TPasIdentifierScope(TopScope);
    end
  else
    begin
    if not (TopScope is TPasGenericParamsScope) then
      RaiseInvalidScopeForElement(20190831205038,El,GetObjName(TopScope));
    CurScope:=TPasIdentifierScope(Scopes[ScopeCount-2]);
    GenTemplCnt:=TypeParams.Count;
    El.SetGenericTemplates(TypeParams);
    TypeParams:=El.GenericTemplateTypes;
    CheckGenericTemplateTypes(El);
    end;

  if CurScope is TPasGroupScope then
    LocalScope:=TPasGroupScope(CurScope).Scopes[0]
  else
    LocalScope:=CurScope;
  Duplicate:=LocalScope.FindLocalIdentifier(El.Name);
  while Duplicate<>nil do
    begin
    DuplEl:=Duplicate.Element;
    if (DuplEl is TPasGenericType)
        and (GetTypeParameterCount(TPasGenericType(DuplEl))=GenTemplCnt) then
      break;
    Duplicate:=Duplicate.NextSameIdentifier;
    end;

  //if Duplicate<>nil then
    //writeln('  Duplicate=',GetObjName(Duplicate.Element),' ',ord(Duplicate.Kind));

  if (Duplicate<>nil)
      and (Duplicate.Element is TPasClassType)
      and TPasClassType(Duplicate.Element).IsForward
      and (Duplicate.Element.Parent=El.Parent)
  then
    begin
    // forward declaration found
    ForwardDecl:=TPasClassType(Duplicate.Element);
    {$IFDEF VerbosePasResolver}
    writeln('  Resolving Forward=',GetObjName(ForwardDecl),' ',GetElementSourcePosStr(ForwardDecl));
    {$ENDIF}
    if GenTemplCnt>0 then
      begin
      // check generic constraints match exactly
      for i:=0 to GenTemplCnt-1 do
        begin
        ForwGenTempl:=TPasGenericTemplateType(ForwardDecl.GenericTemplateTypes[i]);
        ActGenTempl:=TPasGenericTemplateType(TypeParams[i]);
        if not SameText(ForwGenTempl.Name,ActGenTempl.Name) then
          RaiseMsg(20190814114811,nDeclOfXDiffersFromPrevAtY,sDeclOfXDiffersFromPrevAtY,
            [GetTypeDescription(ActGenTempl),GetElementSourcePosStr(ForwGenTempl)],ActGenTempl);
        ForwConstraints:=ForwGenTempl.Constraints;
        ActConstraints:=ActGenTempl.Constraints;
        if length(ForwConstraints)<>length(ActConstraints) then
          RaiseMsg(20190814121031,nDeclOfXDiffersFromPrevAtY,sDeclOfXDiffersFromPrevAtY,
            [GetTypeDescription(ActGenTempl),GetElementSourcePosStr(ForwGenTempl)],ActGenTempl);
        for j:=0 to length(ForwConstraints)-1 do
          begin
          ForwConstraint:=ForwConstraints[j];
          ActConstraint:=ActConstraints[j];
          ForwToken:=GetGenericConstraintKeyword(ForwConstraint);
          ActToken:=GetGenericConstraintKeyword(ActConstraint);
          if ForwToken<>ActToken then
            RaiseMsg(20190814121139,nDeclOfXDiffersFromPrevAtY,sDeclOfXDiffersFromPrevAtY,
              [GetTypeDescription(ActGenTempl),GetElementSourcePosStr(ForwConstraint)],
              GetGenericConstraintErrorEl(ActConstraint,ActGenTempl));
          if ForwToken=tkEOF then
            begin
            ComputeElement(ForwConstraint,ForwConstraintResolved,[rcType]);
            ComputeElement(ActConstraint,ActConstraintResolved,[rcType]);
            if CheckElTypeCompatibility(ForwConstraintResolved.LoTypeEl,
                ActConstraintResolved.LoTypeEl,prraNone)<>cExact then
              RaiseMsg(20190814121509,nDeclOfXDiffersFromPrevAtY,sDeclOfXDiffersFromPrevAtY,
                [GetTypeDescription(ActGenTempl),
                GetElementSourcePosStr(GetGenericConstraintErrorEl(ForwConstraint,ForwGenTempl))],
                GetGenericConstraintErrorEl(ActConstraint,ActGenTempl));
            end;
          end;
        end;
      end;

    if ForwardDecl.CustomData<>nil then
      begin
      // move the classscope to the real declaration
      ClassScope:=ForwardDecl.CustomData as TPasClassScope;
      if El.CustomData<>nil then
        RaiseInternalError(20190803202959,'real class has already customdata');
      ForwardDecl.CustomData:=nil;
      El.CustomData:=ClassScope;
      ClassScope.Element:=El;
      end;
    // create a ref from the forward to the real declaration
    CreateReference(El,ForwardDecl,rraRead);
    // change the cache item
    Duplicate.Element:=El;
    end
  else
    AddIdentifier(CurScope,El.Name,El,pikSimple);

  if TypeParams<>nil then
    begin
    // Parsing the ancestor+interface list requires the type params.
    // AddGenericTemplateIdentifiers not needed, already in TPasGenericParamsScope
    end;

  {$IFDEF VerbosePasResolver}
  if FPendingForwardProcs.IndexOf(El)>=0 then
    RaiseNotYetImplemented(20190804114746,El);
  {$ENDIF}
  FPendingForwardProcs.Add(El); // check forward declarations at the end
end;

procedure TPasResolver.AddVariable(El: TPasVariable);
begin
  if (El.Name='') then exit; // anonymous var
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddVariable ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160929205730,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
end;

procedure TPasResolver.AddResourceString(El: TPasResString);
var
  C: TClass;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddResourceString ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20171004092114,El);
  C:=El.Parent.ClassType;
  if not C.InheritsFrom(TPasSection) then
    RaiseNotYetImplemented(20171004092518,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
end;

procedure TPasResolver.AddExportSymbol(El: TPasExportSymbol);
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddExportSymbol ',GetObjName(El));
  {$ENDIF}
  // Note: export symbol is not added to scope
  if El=nil then ;
end;

procedure TPasResolver.AddEnumType(El: TPasEnumType);
var
  CanonicalSet: TPasSetType;
  EnumScope: TPasEnumTypeScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddEnumType ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160929205732,El);
  if El.Name<>'' then
    AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple)
  else
    begin
    // anonymous enumtype
    end;
  EnumScope:=TPasEnumTypeScope(PushScope(El,ScopeClass_EnumType));
  // add canonical set
  if El.Parent is TPasSetType then
    begin
    // set of anonymous enumtype, e.g. "set of ()"
    CanonicalSet:=TPasSetType(El.Parent);
    end
  else
    begin
    CanonicalSet:=TPasSetType(CreateOwnedElement(TPasSetType,'',El));
    CanonicalSet.EnumType:=El;
    end;
  EnumScope.CanonicalSet:=CanonicalSet;
end;

procedure TPasResolver.AddEnumValue(El: TPasEnumValue);
var
  i: Integer;
  Scope: TPasScope;
  Old: TPasIdentifier;
  ClassOrRec: TPasMembersType;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddEnumValue ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasEnumTypeScope) then
    RaiseInvalidScopeForElement(20160929205736,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);

  // propagate enum to parent scopes
  //  TEnum = (red, green); -> dot not propagate
  //  TFlags = set of (red,blue); -> propagate
  if (bsScopedEnums in CurrentParser.Scanner.CurrentBoolSwitches)
      and not (El.Parent.Parent is TPasSetType) then
    exit;
  for i:=ScopeCount-2 downto 0 do
    begin
    Scope:=Scopes[i];
    if Scope is TPasGroupScope then
      Scope:=TPasGroupScope(Scope).Scopes[0];
    if Scope is TPasClassOrRecordScope then
      begin
      // class or record: add if not duplicate
      Old:=TPasIdentifierScope(Scope).FindIdentifier(El.Name);
      if Old=nil then
        TPasIdentifierScope(Scope).AddIdentifier(El.Name,El,pikSimple);
      ClassOrRec:=Scope.Element as TPasMembersType;
      // A generic's enum values reach its section too when the policy allows
      // it, as in FPC; the base resolver keeps them inside the generic.
      if (GetTypeParameterCount(ClassOrRec)>0)
          and not AllowGenericEnumValuesInSection then
        break;
      if TPasClassOrRecordScope(Scope).SpecializedFromItem<>nil then
        break; // nor in a specialization: it belongs to no section
      end
    else if (Scope is TPasProcedureScope) or (Scope is TPasSectionScope) then
      begin
      // procedure or section: check for duplicate and add
      Old:=TPasIdentifierScope(Scope).FindLocalIdentifier(El.Name);
      if Old<>nil then
        RaiseMsg(20170216152224,nDuplicateIdentifier,sDuplicateIdentifier,
                 [El.Name,GetElementSourcePosStr(Old.Element)],El);
      TPasIdentifierScope(Scope).AddIdentifier(El.Name,El,pikSimple);
      break;
      end
    else
      break;
    end;
end;

procedure TPasResolver.AddProperty(El: TPasProperty);
begin
  if (El.Name='') then
    RaiseNotYetImplemented(20160922163518,El);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddProperty ',GetObjName(El));
  {$ENDIF}
  // A property may also be declared at unit scope (a global property backed by
  // read/write functions, as the RTL uses); its local scope is then a section
  // scope, not a class/record scope. TPasSectionScope is a TPasIdentifierScope,
  // so the AddIdentifier/PushScope below accept it unchanged.
  if not ((GetLocalScope is TPasClassOrRecordScope)
      or (GetLocalScope is TPasSectionScope)) then
    RaiseInvalidScopeForElement(20160922163520,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
  PushScope(El,TPasPropertyScope);
end;

procedure TPasResolver.AddProcedureType(El: TPasProcedureType;
  TypeParams: TFPList);
var
  Scope: TPasProcTypeScope;
  C: TClass;
begin
  if El.Name<>'' then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.AddProcedureType ',GetObjName(El),' Parent=',GetObjName(El.Parent));
    {$ENDIF}
    if El.Parent is TPasProcedure then
      RaiseNotYetImplemented(20190911102852,El,GetObjPath(El.Parent));
    if TypeParams<>nil then
      begin
      El.SetGenericTemplates(TypeParams);
      TypeParams:=El.GenericTemplateTypes;
      CheckGenericTemplateTypes(El);
      end;
    PopGenericParamScope(El);

    if not (TopScope is TPasIdentifierScope) then
      RaiseInvalidScopeForElement(20190813193703,El);

    AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);

    if TypeParams<>nil then
      begin
      Scope:=TPasProcTypeScope(PushScope(El,ScopeClass_ProcType));
      AddGenericTemplateIdentifiers(TypeParams,Scope);
      end
    else if StoreProcTypeScopeBoolSwitches then
      begin
      // Store bool switches for non-generic named proc types (needed for a funcref's
      // {$M+} RTTI). Must be done here (not in FinishProcedureType) because
      // ParseProcedureModifiers reads ahead and may process {$M-} before
      // FinishProcedureType is called (tfuncref28/texrtti19).
      // Only a native/FPC target needs this: base and pas2js create no scope for a
      // named non-generic proc type (pristine upstream), so their PCU round-trip and
      // scope invariants are unchanged.
      Scope:=TPasProcTypeScope(ScopeClass_ProcType.Create);
      Scope.BoolSwitches:=CurrentParser.Scanner.CurrentBoolSwitches;
      AddResolveData(El,Scope,lkModule);
      end;
    end
  else
    begin
    // no name
    if TypeParams<>nil then
      RaiseNotYetImplemented(20190813193745,El);
    if El.Parent=nil then
      RaiseNotYetImplemented(20220320122040,El);
    if El.Parent is TPasProcedure then
      // proctype of procedure has no name -> normal
    else
      begin
      // anonymous procedure type, e.g. "var p: procedure;"
      C:=El.Parent.ClassType;
      if C.InheritsFrom(TPasVariable) then
        // ok
      else if C.InheritsFrom(TPasArgument) then
        // ok: inline procedural parameter (Mac/ISO/TP nestedprocvars),
        // e.g. "procedure p(procedure pp(i: longint))"
      else if C.InheritsFrom(TPasArrayType) then
        // ok: anonymous element type, e.g. "array[0..1] of Procedure"
      else
        RaiseMsg(20220320165827,nCannotNestAnonymousX,sCannotNestAnonymousX,[GetElementTypeName(El)],El);
      DeanonymizeType(El);
      end;
    end;
end;

procedure TPasResolver.AddProcedure(El: TPasProcedure; TypeParams: TFPList);

  procedure AddClassConDestructor(ClassOrRecordScope: TPasClassOrRecordScope;
    var Field: TPasProcedure);
  begin
    if Field<>nil then
      RaiseMsg(20181231144353,nMultipleXinTypeYNameZCAandB,
        sMultipleXinTypeYNameZCAandB,[GetElementTypeName(El),
          GetElementTypeName(ClassOrRecordScope.Element),
          ClassOrRecordScope.Element.Name,Field.Name,El.Name],El);
    Field:=El;
  end;

  function FindBestMembersType(const ClassOrRecName: string;
    TypeParamCnt: integer; Scope: TPasIdentifierScope;
    var Best: TPasMembersType; ErrorPos: TPasElement): integer;
  // returns number of candidates
  var
    Identifier: TPasIdentifier;
    CurEl: TPasElement;
  begin
    Result:=0;
    Identifier:=Scope.FindLocalIdentifier(ClassOrRecName);
    while Identifier<>nil do
      begin
      CurEl:=Identifier.Element;
      // A method may be implemented under an ALIAS of its class: fcl-db's
      // bufdataset declares `TDataPacketReader = TDataPacketHandler` and writes
      // `class function TDataPacketReader.RowStateToByte`.
      if (CurEl is TPasType) and not (CurEl is TPasMembersType) then
        if ResolveAliasType(TPasType(CurEl)) is TPasMembersType then
          CurEl:=ResolveAliasType(TPasType(CurEl));
      if not (CurEl is TPasMembersType) then
        RaiseXExpectedButYFound(20170216152557,
          'class',CurEl.Name+':'+GetElementTypeName(CurEl),ErrorPos);
      inc(Result);
      if Best=nil then
        Best:=TPasMembersType(CurEl);
      if GetTypeParameterCount(TPasGenericType(CurEl))=TypeParamCnt then
        begin
        // fits
        Best:=TPasMembersType(CurEl);
        exit;
        end;
      Identifier:=Identifier.NextSameIdentifier;
      end;
  end;

  function FindMembersType(Scope: TPasIdentifierScope;
    const ClassOrRecName: string; TypeParamCnt: integer; IsDelphi: boolean;
    ErrorPos: TPasElement): TPasMembersType;
  var
    Found: integer;
  begin
    Result:=nil;
    if Scope<>nil then
      Found:=FindBestMembersType(ClassOrRecName,TypeParamCnt,Scope,Result,ErrorPos)
    else if TopScope is TPasIdentifierScope then
      begin
      Found:=FindBestMembersType(ClassOrRecName,TypeParamCnt,
          TPasIdentifierScope(TopScope),Result,ErrorPos);
      if (Result=nil) or (TypeParamCnt<>GetTypeParameterCount(Result)) then
        begin
        if (TopScope is TPasSectionScope)
            and (ScopeCount>1) and (Scopes[ScopeCount-2] is TPasSectionScope) then
          // search in unit interface too
          Found:=Found+FindBestMembersType(ClassOrRecName,TypeParamCnt,
                TPasIdentifierScope(Scopes[ScopeCount-2]),Result,ErrorPos);
        end;
      end;
    if Result=nil then
      RaiseMsg(20190818112356,nClassXNotFoundInThisModule,sClassXNotFoundInThisModule,
               [ClassOrRecName+GetGenericParamCommas(TypeParamCnt)],ErrorPos);
    if TypeParamCnt=GetTypeParameterCount(Result) then
      exit; // fits perfectly
    if (not IsDelphi) and (TypeParamCnt=0) and (Found=1) then
      exit; // in objfpc type params can be omitted if there is only one type
    // found one or more, but type param count do not fit
    RaiseMsg(20190818112856,nXExpectedButYFound,sXExpectedButYFound,
      [Result.Name+GetGenericParamCommas(GetTypeParameterCount(Result)),
       ClassOrRecName+GetGenericParamCommas(TypeParamCnt)],ErrorPos);
  end;

  procedure CheckTemplateNames;
  var
    i, j: Integer;
    NamePart: TProcedureNamePart;
    TemplTypes: TFPList;
    TemplType: TPasGenericTemplateType;
  begin
    for i:=0 to TypeParams.Count-1 do
      begin
      NamePart:=TProcedureNamePart(TypeParams[i]);
      TemplTypes:=NamePart.Templates;
      if TemplTypes=nil then continue;
      for j:=0 to TemplTypes.Count-1 do
        begin
        TemplType:=TPasGenericTemplateType(TemplTypes[j]);
        if SameText(TemplType.Name,El.Name) then
          RaiseMsg(20190912174817,nDuplicateIdentifier,sDuplicateIdentifier,
            [],TemplType);
        end;
      end;
  end;

var
  ProcName, aClassName: String;
  p: SizeInt;
  ClassOrRecType: TPasMembersType;
  ProcScope: TPasProcedureScope;
  HasDot, IsClassConDestructor, IsDelphi: Boolean;
  ClassOrRecScope: TPasClassOrRecordScope;
  C: TClass;
  CurScope: TPasScope;
  LocalScope: TPasScope;
  Level, TypeParamCount, i: Integer;
  NamePart: TProcedureNamePart;
  TemplType, FoundTemplType: TPasGenericTemplateType;
  NestedMembersScope: TPasGroupScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddProcedure ',GetObjName(El));
  {$ENDIF}

  if TypeParams<>nil then
    begin
    // move type param elements to El
    El.SetNameParts(TypeParams);
    TypeParams:=El.NameParts;
    if TopScope is TPasGenericParamsScope then
      PopScope;
    CheckTemplateNames;
    end;

  CurScope:=TopScope;
  if CurScope.ClassType=TPasGroupScope then
    LocalScope:=TPasGroupScope(CurScope).Scopes[0]
  else
    LocalScope:=CurScope;

  ProcName:=El.Name;
  if El.Name<>'' then
    begin
    // named proc
    if not (LocalScope is TPasIdentifierScope) then
      RaiseInvalidScopeForElement(20160922163522,El);
    end
  else
    begin
    // anonymous proc
    if TypeParams<>nil then
      RaiseNotYetImplemented(20190818101856,El);
    C:=LocalScope.ClassType;
    if (C=ScopeClass_InitialFinalization)
        or C.InheritsFrom(TPasProcedureScope)
        or (C=TPasWithScope)
        or (C=ScopeClass_WithExpr)
        or (C=TPasExceptOnScope)
        or (C=TPasForLoopScope) then
      // ok
    else
      RaiseInvalidScopeForElement(20181210173134,El);
    end;

  // Note: El.ProcType is nil !  It is parsed later.

  HasDot:=GetFirstDotPos(ProcName)>1;
  if (TypeParams<>nil) then
    if HasDot<>(TypeParams.Count>1) then
      RaiseNotYetImplemented(20190818093923,El);
  IsClassConDestructor:=(El.ClassType=TPasClassConstructor)
      or (El.ClassType=TPasClassDestructor);

  ClassOrRecType:=nil;
  if El.CustomData is TPasProcedureScope then
    begin
    // adding a specialized implementation proc
    ProcScope:=TPasProcedureScope(El.CustomData);
    if ProcScope.DeclarationProc<>nil then
      TypeParams:=ProcScope.DeclarationProc.NameParts;
    ClassOrRecScope:=ProcScope.ClassRecScope;
    if ClassOrRecScope<>nil then
      begin
      ClassOrRecType:=TPasMembersType(ClassOrRecScope.Element);
      if GetTypeParameterCount(ClassOrRecType)>0 then
        RaiseNotYetImplemented(20190804175518,El);
      if ProcScope.GroupScope<>nil then
        RaiseNotYetImplemented(20190804175451,El);
      if (not HasDot) and IsClassConDestructor then
        begin
        if El.ClassType=TPasClassConstructor then
          AddClassConDestructor(ClassOrRecScope,TPasProcedure(ClassOrRecScope.ClassConstructor))
        else
          AddClassConDestructor(ClassOrRecScope,TPasProcedure(ClassOrRecScope.ClassDestructor));
        end;
      end;

    PushScope(ProcScope);
    end
  else
    begin
    IsDelphi:=msDelphi in CurrentParser.CurrentModeswitches;

    if (not HasDot) and IsClassConDestructor then
      begin
      if ProcName='' then
        RaiseNotYetImplemented(20181231145302,El);
      if not (LocalScope is TPasClassOrRecordScope) then
        RaiseInvalidScopeForElement(20181231143831,El);
      ClassOrRecScope:=TPasClassOrRecordScope(LocalScope);
      if El.ClassType=TPasClassConstructor then
        AddClassConDestructor(ClassOrRecScope,TPasProcedure(ClassOrRecScope.ClassConstructor))
      else
        AddClassConDestructor(ClassOrRecScope,TPasProcedure(ClassOrRecScope.ClassDestructor));
      if TypeParams<>nil then
        RaiseMsg(20190818094753,nTypeParamsNotAllowedOnX,sTypeParamsNotAllowedOnX,
          [El.ElementTypeName],El);
      end;

    if (not HasDot) and (ProcName<>'')
        and not IsClassConDestructor // the name of a class con/destructor is irrelevant and cannot be referenced
    then
      begin
      // add proc name to scope
      AddIdentifier(TPasIdentifierScope(CurScope),ProcName,El,pikProc);
      end;

    ProcScope:=TPasProcedureScope(CreateScope(El,FScopeClass_Proc));
    ProcScope.ModeSwitches:=CurrentParser.CurrentModeswitches;
    if HasDot then
      begin
      // method implementation -> search class
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.AddProcedure searching class of "',ProcName,'" ...');
      {$ENDIF}
      ClassOrRecType:=nil;
      Level:=0;
      repeat
        inc(Level);
        p:=GetFirstDotPos(ProcName);
        if p<1 then
          begin
          if ClassOrRecType=nil then
            RaiseInternalError(20161013170829);
          break;
          end;
        aClassName:=FirstDottedIdentifier(ProcName);
        Delete(ProcName,1,p);
        TypeParamCount:=0;
        if TypeParams<>nil then
          begin
          // e.g. aclassname<T>.
          if Level>TypeParams.Count then
            RaiseNotYetImplemented(20190818122217,El);
          NamePart:=TProcedureNamePart(TypeParams[Level-1]);
          if NamePart.Name<>aClassName then
            RaiseNotYetImplemented(20190818102541,El,IntToStr(Level)+': '+NamePart.Name+'<>'+aClassName);
          if NamePart.Templates<>nil then
            begin
            TypeParamCount:=NamePart.Templates.Count;
            for i:=0 to TypeParamCount-1 do
              begin
              TemplType:=TPasGenericTemplateType(NamePart.Templates[i]);
              if length(TemplType.Constraints)>0 then
                RaiseMsg(20190818102850,nIllegalQualifierAfter,sIllegalQualifierAfter,
                  [':',TemplType.name],TemplType);
              end;
            end;
          end
        else
          NamePart:=nil;
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.AddProcedure searching class "',aClassName,GetGenericParamCommas(TypeParamCount),'" ProcName="',ProcName,'" ...');
        {$ENDIF}
        if not IsValidIdent(aClassName) then
          RaiseNotYetImplemented(20161013170844,El);

        if ClassOrRecType<>nil then
          begin
          ClassOrRecScope:=TPasClassOrRecordScope(ClassOrRecType.CustomData);
          ClassOrRecType:=FindMembersType(ClassOrRecScope,aClassName,
                                          TypeParamCount,IsDelphi,El);
          end
        else
          ClassOrRecType:=FindMembersType(nil,aClassName,
                                          TypeParamCount,IsDelphi,El);

        if ClassOrRecType is TPasClassType then
          begin
          if not (TPasClassType(ClassOrRecType).ObjKind in
              ([okClass,okObject]+okAllHelpers)) then
            begin
            aClassName:=LeftStr(El.Name,length(El.Name)-length(ProcName)-1);
            RaiseXExpectedButYFound(20180321161722,'class',
              aClassname+GetGenericParamCommas(GetTypeParameterCount(ClassOrRecType))+':'+GetElementTypeName(ClassOrRecType),El);
            end
          end;
        if ClassOrRecType.GetModule<>El.GetModule then
          RaiseNotYetImplemented(20190818120051,El);
        if NamePart<>nil then
          begin
          // check that all type param names match
          for i:=0 to TypeParamCount-1 do
            begin
            TemplType:=TPasGenericTemplateType(NamePart.Templates[i]);
            FoundTemplType:=TPasGenericTemplateType(ClassOrRecType.GenericTemplateTypes[i]);
            if not SameText(TemplType.Name,FoundTemplType.Name) then
              RaiseMsg(20190822014652,nXExpectedButYFound,
                sXExpectedButYFound,[FoundTemplType.Name,TemplType.Name],TemplType);
            end;
          end;

      until false;

      if not IsValidIdent(ProcName) then
        RaiseNotYetImplemented(20161013170956,El);

      ProcScope.VisibilityContext:=ClassOrRecType;
      ProcScope.ClassRecScope:=NoNil(ClassOrRecType.CustomData) as TPasClassOrRecordScope;

      if TypeParams<>nil then
        begin
        if Level<>TypeParams.Count then
          RaiseNotYetImplemented(20190818122315,El);
        NamePart:=TProcedureNamePart(TypeParams[Level-1]);
        if NamePart.Name<>ProcName then
          RaiseNotYetImplemented(20190818122551,El,IntToStr(Level)+': '+NamePart.Name+'<>'+ProcName);
        end;

      end
    else
      begin
      // HasDot=false
      end;
    PushScope(ProcScope);
    end;// end source proc, not specialized

  if HasDot then
    begin
    // create GroupScope
    if TopScope<>ProcScope then
      RaiseNotYetImplemented(20191014235935,El,GetObjName(TopScope));
    ProcScope.GroupScope:=CreateGroupScope(ClassOrRecType);
    if ClassOrRecType.Parent is TPasMembersType then
      begin
      // nested class
      ClassOrRecType:=TPasMembersType(ClassOrRecType.Parent);
      NestedMembersScope:=CreateGroupScope(ClassOrRecType);
      ProcScope.NestedMembersScope:=NestedMembersScope;
      NestedMembersScope.OnlyTypeMembers:=true;
      // Delphi searches the parent class scopes *after* the section scopes
      // and before the module scope - sigh
      // -> Move scope between module scope and section scope
      i:=0;
      while (i<ScopeCount) and not (FScopes[i] is TPasModuleScope) do
        inc(i);
      InsertScope(NestedMembersScope,i+1);

      while ClassOrRecType.Parent is TPasMembersType do
        begin
        ClassOrRecType:=TPasMembersType(ClassOrRecType.Parent);
        GroupScope_AddTypeAndAncestors(NestedMembersScope,ClassOrRecType);
        end;
      end;
    end;

  // add generic params to scope
  if TypeParams<>nil then
    begin
    NamePart:=TProcedureNamePart(TypeParams[TypeParams.Count-1]);
    if NamePart<>nil then
      AddGenericTemplateIdentifiers(NamePart.Templates,ProcScope);
    end;
end;

procedure TPasResolver.AddArgument(El: TPasArgument);
var
  ProcType: TPasProcedureType;
  i: Integer;
  Arg: TPasArgument;
  CurScope: TPasScope;
begin
  if (El.Name='') then
    RaiseInternalError(20160922163526,GetObjName(El));
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddArgument ',GetObjName(El));
  {$ENDIF}
  CurScope:=TopScope;
  if (CurScope=nil) then
    RaiseInvalidScopeForElement(20160922163529,El);
  if El.Parent.ClassType=TPasProperty then
    begin
    if CurScope.ClassType<>TPasPropertyScope then
      RaiseInvalidScopeForElement(20161014124530,El);
    AddIdentifier(TPasIdentifierScope(CurScope),El.Name,El,pikSimple);
    end
  else if El.Parent is TPasProcedureType then
    begin
    ProcType:=TPasProcedureType(El.Parent);
    if ProcType.Parent is TPasProcedure then
      begin
      if CurScope.ClassType<>FScopeClass_Proc then
        RaiseInvalidScopeForElement(20160922163529,El,GetObjName(TopScope));
      AddIdentifier(TPasIdentifierScope(CurScope),El.Name,El,pikSimple);
      end
    else
      begin
      for i:=0 to ProcType.Args.Count-1 do
        begin
        Arg:=TPasArgument(ProcType.Args[i]);
        if (Arg<>El) and (CompareText(TPasArgument(ProcType.Args[i]).Name,El.Name)=0) then
          RaiseMsg(20170216152225,nDuplicateIdentifier,sDuplicateIdentifier,[Arg.Name,GetElementSourcePosStr(Arg)],El);
        end;
      end;
    end
  else
    RaiseNotYetImplemented(20161014124937,El);
end;

procedure TPasResolver.AddFunctionResult(El: TPasResultElement);
var
  CurScope: TPasScope;
begin
  CurScope:=TopScope;
  if CurScope.ClassType<>FScopeClass_Proc then exit;
  if El.Parent is TPasProcedureType then
    begin
    if not (El.Parent.Parent is TPasProcedure) then
      exit;
    end
  else if not (El.Parent is TPasProcedure) then
    exit;
  AddIdentifier(TPasProcedureScope(CurScope),ResolverResultVar,El,pikSimple);
end;

procedure TPasResolver.AddGenericTemplateType(El: TPasGenericTemplateType);
var
  ParamScope: TPasGenericParamsScope;
  OldIdentifier: TPasIdentifier;
begin
  if TopScope is TPasGenericParamsScope then
    begin
    ParamScope:=TPasGenericParamsScope(TopScope);
    if ParamScope.Element.Parent<>El.Parent then
      RaiseNotYetImplemented(20190831203132,El,GetObjName(ParamScope.Element));
    end
  else
    begin
    if El.CustomData<>nil then
      RaiseNotYetImplemented(20190831202627,El,GetObjName(El.CustomData));
    ParamScope:=TPasGenericParamsScope.Create;
    AddResolveData(El,ParamScope,lkModule);
    PushScope(ParamScope);
    end;
  OldIdentifier:=ParamScope.FindIdentifier(El.Name);
  if OldIdentifier<>nil then
    RaiseMsg(20190831202920,nDuplicateIdentifier,sDuplicateIdentifier,
      [OldIdentifier.Identifier,GetElementSourcePosStr(OldIdentifier.Element)],El);
  ParamScope.AddIdentifier(El.Name,El,pikSimple);
end;

procedure TPasResolver.AddExceptOn(El: TPasImplExceptOn);
begin
  PushScope(El,TPasExceptOnScope);
end;

procedure TPasResolver.AddTryExceptExprOn(El: TTryExceptExprOn);
begin
  PushScope(El,TPasExceptOnScope);
end;

procedure TPasResolver.AddWithDo(El: TPasImplWithDo);
begin
  if TPasWithScope.FreeOnPop then
    RaiseInternalError(20181210162344);
  PushScope(El,TPasWithScope);
end;

procedure TPasResolver.AddProcedureBody(El: TProcedureBody);
begin
  if El=nil then ;
  CheckTopScope(FScopeClass_Proc);
end;
