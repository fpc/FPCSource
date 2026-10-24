{%MainUnit pasresolver.pp}
{ **********************************************************************
 
    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - Expressions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }


procedure TPasResolver.ResolveStatementConditionExpr(El: TPasExpr);
var
  ResolvedCond: TPasResolverResult;
begin
  ResolveExpr(El,rraRead);
  ComputeElement(El,ResolvedCond,[rcSetReferenceFlags]);
  CheckConditionExpr(El,ResolvedCond);
end;

procedure TPasResolver.ResolveNameExpr(El: TPasExpr; const aName: string;
  Access: TResolvedRefAccess);
var
  FindData: TPRFindData;
  DeclEl: TPasElement;
  Proc, ImplProc: TPasProcedure;
  Ref: TResolvedReference;
  BuiltInProc: TResElDataBuiltInProc;
  p: SizeInt;
  DottedName: String;
  Bin: TBinaryExpr;
  ProcScope: TPasProcedureScope;
  ParentParams: TPRParentParams;
  TypeCnt: Integer;
  InlParams, TemplTypes: TFPList;
  ParentEl: TPasElement;

  function IsDelphiProcConstRef(aExpr: TPasExpr): boolean;
  // In Delphi mode a bare procedure name in a typed constant's initializer is a
  // procvar reference, not a call: `const F: TExprFunc = FuncA;` and the array
  // form fcl-db's dbf_idxfile uses. ObjFPC still requires the @ there.
  var
    ParentDecl: TPasElement;
    ConstType: TPasType;
  begin
    Result:=false;
    if not (msDelphi in CurrentParser.CurrentModeswitches) then exit;
    ParentDecl:=aExpr.Parent;
    while ParentDecl is TPasExpr do
      ParentDecl:=ParentDecl.Parent;
    if not (ParentDecl is TPasConst) then exit;
    ConstType:=TPasConst(ParentDecl).VarType;
    while ConstType<>nil do
      begin
      ConstType:=ResolveAliasType(ConstType);
      if ConstType is TPasArrayType then
        ConstType:=TPasArrayType(ConstType).ElType
      else
        break;
      end;
    Result:=ConstType is TPasProcedureType;
  end;

begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveNameExpr El=',GetObjName(El),' Name="',aName,'" ',Access);
  {$ENDIF}
  GetParamsOfNameExpr(El,ParentParams);
  if ParentParams.InlineSpec<>nil then
    InlParams:=ParentParams.InlineSpec.Params
  else
    InlParams:=nil;
  //writeln('TPasResolver.ResolveNameExpr Inline=',GetObjName(ParentParams.InlineSpec),' Params=',GetObjName(ParentParams.Params),' ',GetObjPath(El));
  if ParentParams.Params<>nil then
    begin
    case ParentParams.Params.Kind of
    pekFuncParams:
      ResolveFuncParamsExprName(El,InlParams,ParentParams.Params,Access);
    pekArrayParams:
      ResolveArrayParamsExprName(El,ParentParams.Params,Access);
    else
      RaiseNotYetImplemented(20190912190428,El,GetObjPath(ParentParams.Params));
    end;
    exit;
    end;

  if ParentParams.InlineSpec<>nil then
    begin
    TypeCnt:=InlParams.Count;
    DeclEl:=FindGenericEl(aName,TypeCnt,FindData,El);
    if DeclEl<>nil then
      begin
      // GenType<params> -> create specialize type/proc
      DeclEl:=GetSpecializedEl(ParentParams.InlineSpec,DeclEl,InlParams);
      end
    else
      RaiseXExpectedButYFound(20190916160829,'generic type',GetElementTypeName(DeclEl),El);
    end
  else
    // NoGenerics=true: a bare name (no <...> specialization) in an expression
    // prefers a non-generic identifier sharing the name (e.g. a variable Tst
    // over a generic type Tst<const N>), so "Tst < N" is a comparison of the
    // variable, not an operator on the generic class. A generic with no
    // non-generic alternative is still returned (and errors appropriately).
    begin
    DeclEl:=FindElementWithoutParams(aName,FindData,El,false,true);
    // the generic's bare name inside a specialization of it
    DeclEl:=MapGenericSelfRef(DeclEl,El);
    FindData.Found:=DeclEl;
    end;

  if DeclEl.ClassType=TPasUsesUnit then
    begin
    // the first name of a unit matches -> find unit with longest match
    FindLongestUnitName(DeclEl,El);
    FindData.Found:=DeclEl;
    end;

  if (DeclEl is TPasVariable) and El.HasParent(DeclEl) then
    RaiseIdentifierNotFound(20230712105546,aName,El);

  Ref:=CreateReference(DeclEl,El,Access,@FindData);
  CheckFoundElement(FindData,Ref);

  if DeclEl is TPasProcedure then
    begin
    // identifier is a proc and args brackets are missing
    Proc:=TPasProcedure(DeclEl);
    if ParentParams.InlineSpec=nil then
      begin
      TemplTypes:=GetProcTemplateTypes(Proc);
      if (TemplTypes<>nil) then
        begin
        // A generic function's own name used as its RESULT variable (FuncName:=...,
        // or FuncName on the rhs / as an argument) is NOT an implicit specialization:
        // redirect it to the result element before attempting inference, exactly as
        // the non-generic cases below do (GitLab #41370). Only a BARE name is the
        // result variable — FuncName(args) is a real recursive call that must still
        // infer, so exclude the callee (Value) of a parameter list; @FuncName is a
        // reference, excluded by ExprIsAddrTarget.
        if (Proc.ProcType is TPasFunctionType)
            and (El.ClassType=TPrimitiveExpr)
            and not ExprIsAddrTarget(El)
            and not ((El.Parent is TParamsExpr) and (TParamsExpr(El.Parent).Value=El)) then
          begin
          ParentEl:=El;
          while (ParentEl<>nil) and not (ParentEl is TPasProcedure) do
            ParentEl:=ParentEl.Parent;
          // Compare BASE names (strip any "<...>" specialization suffix): inside the
          // SPECIALIZED body the enclosing function is e.g. "Add<System.Integer>"
          // while the resolved identifier is still the template "Add".
          if (ParentEl<>nil) and (ParentEl is TPasFunction)
              and SameText(
                Copy(TPasFunction(ParentEl).Name,1,Pos('<',TPasFunction(ParentEl).Name+'<')-1),
                Copy(Proc.Name,1,Pos('<',Proc.Name+'<')-1)) then
            begin
            Ref.Declaration:=TPasFunctionType(TPasFunction(ParentEl).ProcType).ResultEl;
            exit;
            end;
          end;
        // implicit function specialization without bracket
        {$IFDEF VerbosePasResolver}
        DeclEl:=El;
        while DeclEl.Parent is TPasExpr do
          DeclEl:=DeclEl.Parent;
        {AllowWriteln}
        writeln('TPasResolver.ResolveNameExpr ',WritePasElTree(TPasExpr(DeclEl),'  '));
        {AllowWriteln-}
        {$ENDIF}
        RaiseMsg(20191007222004,nCouldNotInferTypeArgXForMethodY,
          sCouldNotInferTypeArgXForMethodY,[TPasGenericTemplateType(TemplTypes[0]).Name,Proc.Name],El);
        end;
      end;

    if El.Parent.ClassType=TPasProperty then
      // a property accessor does not need args -> ok
      // Note: the detailed tests are in FinishProperty
    else
      begin
      // examples: funca or @proca or a.funca or @a.funca ...
      if (Access=rraAssign) and (Proc.ProcType is TPasFunctionType)
          and (El.ClassType=TPrimitiveExpr)
          and (El.Parent.ClassType=TPasImplAssign)
          and (TPasImplAssign(El.Parent).Left=El) then
        begin
        // e.g. funcname:=
        ProcScope:=Proc.CustomData as TPasProcedureScope;
        ImplProc:=ProcScope.ImplProc;
        if ImplProc=nil then
          ImplProc:=Proc;
        if El.HasParent(ImplProc) then
          begin
          // "FuncA:=" within FuncA  -> redirect to ResultEl
          Ref.Declaration:=TPasFunctionType(Proc.ProcType).ResultEl;
          exit;
          end;
        // Overloaded case: FindFirstEl may have picked a same-named overload whose
        // body does NOT enclose El, so El.HasParent fails; the general Case 4 below
        // walks the parent chain to the actual enclosing function and redirects.
        end;
      // Case 2: Field access — "FuncName.field := value"
      // When function name is the left side of a sub-ident expression inside its
      // own body, redirect to ResultEl so fields can be accessed on the result.
      // Uses parent-chain walk for overload safety (same as Case 3).
      if (Proc.ProcType is TPasFunctionType)
          and (El.ClassType=TPrimitiveExpr)
          and (El.Parent is TBinaryExpr)
          and (TBinaryExpr(El.Parent).OpCode=eopSubIdent)
          and (TBinaryExpr(El.Parent).Left=El) then
        begin
        ParentEl:=El.Parent;
        while (ParentEl<>nil) and not (ParentEl is TPasProcedure) do
          ParentEl:=ParentEl.Parent;
        if (ParentEl<>nil) and (ParentEl is TPasFunction)
            and (CompareText(TPasFunction(ParentEl).Name, Proc.Name)=0) then
          begin
          Ref.Declaration:=TPasFunctionType(TPasFunction(ParentEl).ProcType).ResultEl;
          exit;
          end;
        end;
      // Case 3: Parameter in call expression — "Builtin(x, FuncName)" or "Proc(FuncName)"
      // When a function name that requires parameters appears as a bare identifier
      // (without args) as a parameter in a call expression inside its own body,
      // redirect to ResultEl so it resolves as the result variable.
      // Guard: only for functions that need params — a bare name is then unambiguously
      // the result variable. For parameterless functions, it could be a recursive call,
      // so we leave those alone.
      // Note: with overloads, Proc may point to a different overload than the enclosing
      // function, so we walk the parent chain to find the actual enclosing function,
      // and use that function's ResultEl (not Proc's, which may be a different overload).
      if (Proc.ProcType is TPasFunctionType)
          and ProcNeedsParams(Proc.ProcType)
          and (El.ClassType=TPrimitiveExpr)
          and (El.Parent is TParamsExpr)
          and (El<>TParamsExpr(El.Parent).Value) then
        begin
        // Walk parent chain to find enclosing function with matching name
        // Use ParentEl to avoid clobbering DeclEl
        ParentEl:=EnclosingFunctionNamed(El,Proc.Name);
        if (ParentEl<>nil)
            and IsSelfRefCandidate(El)
            and IsSelfRefResultName(El,Proc) then
          begin
          Ref.Declaration:=TPasFunctionType(TPasFunction(ParentEl).ProcType).ResultEl;
          exit;
          end;
        end;
      // Case 4: General catch-all — function name used as result variable
      // in any expression context (e.g., RHS of assignment: cstr:=cstr+istr).
      // When a function name with required params appears without args inside
      // its own body and hasn't matched Cases 1-3, redirect to ResultEl.
      if (Proc.ProcType is TPasFunctionType)
          and ProcNeedsParams(Proc.ProcType)
          and (El.ClassType=TPrimitiveExpr)
          and not ExprIsAddrTarget(El) then
        begin
        ParentEl:=EnclosingFunctionNamed(El,Proc.Name);
        // The name alone is not enough: a `with` can put a DIFFERENT type's
        // same-named method in a nearer scope, and that one wins - ppcx64 calls
        // it (vcl-compat's TRegEx.Match does `with FRegEx do Found:=Match`,
        // where TPerlRegEx.Match: Boolean is the one meant). Identity is what
        // makes it a self-reference.
        if (ParentEl<>nil)
            and IsSelfRefCandidate(El)
            and IsSelfRefResultName(El,Proc) then
          begin
          Ref.Declaration:=TPasFunctionType(TPasFunction(ParentEl).ProcType).ResultEl;
          exit;
          end;
        end;
      { Case 5: Overloaded self-reference. FindFirstEl may have selected a
        same-named overload whose body does NOT enclose El (e.g. a 0-arg
        overload picked while we are inside the multi-arg overload's body).
        Inside a function's own body a bare use of that name is the result
        variable of the enclosing function, not a call to the other overload,
        so redirect to the enclosing function's result. This covers both
         "FuncName:=" and a bare read (e.g. "if FuncName then"), and works even
        when the found overload is parameterless (Cases 3/4 skip those).
        Checked for "El is NOT inside the found Proc's body": in the non-overloaded
        self-reference case El.HasParent(ImplProc) is true and the paths above
        already handled it, so this block only fires for the wrong-overload pick.
        The found overload may be a PROCEDURE (fcl-stl's gset has both
        `procedure Insert(value)` and `function Insert(value,nod,position)`), so
        only the ENCLOSING routine has to be a function. A procedure overload is
        accepted only when it NEEDS parameters: a parameterless one is a plain
        recursive call and must stay one. }
      // ...and never when the name was reached through a `with`: that puts a
      // DIFFERENT type's same-named method in a nearer scope, and ppcx64 calls
      // it (vcl-compat's TRegEx.Match does `with FRegEx do Found:=Match`, where
      // TPerlRegEx.Match: Boolean is the one meant). This block is about a
      // wrong OVERLOAD of the enclosing routine, not another type's member.
      if (El.ClassType=TPrimitiveExpr)
          and (Ref.WithExprScope=nil)
          and ((Proc.ProcType is TPasFunctionType)
               or ProcNeedsParams(Proc.ProcType))
          and not ExprIsAddrTarget(El)
          and not ((El.Parent is TParamsExpr) and (TParamsExpr(El.Parent).Value=El)) then
        begin
        ProcScope:=Proc.CustomData as TPasProcedureScope;
        ImplProc:=ProcScope.ImplProc;
        if ImplProc=nil then
          ImplProc:=Proc;
        if not El.HasParent(ImplProc) then
          begin
          ParentEl:=El.Parent;
          while (ParentEl<>nil) do
            begin
            if (ParentEl is TPasFunction)
                and IsSelfRefCandidate(El)
            and SameSelfRefProcName(TPasFunction(ParentEl).Name, Proc.Name) then
              begin
              Ref.Declaration:=TPasFunctionType(TPasFunction(ParentEl).ProcType).ResultEl;
              exit;
              end;
            ParentEl:=ParentEl.Parent;
            end;
          end;
        end;
      // When a proc requiring params is used as a function argument
      // (rraParamToUnknownProc), don't raise here — it may be a valid
      // proc reference for a proc-typed parameter. CheckParamCompatibility
      // will catch incompatible types during overload resolution.
      if ProcNeedsParams(Proc.ProcType) and not ExprIsAddrTarget(El)
          and (Access<>rraParamToUnknownProc)
          and not IsDelphiProcConstRef(El) then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveNameExpr ',GetObjPath(El),' Args.Count=',Proc.ProcType.Args.Count);
        {$ENDIF}
        RaiseMsg(20170216152138,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[Proc.Name],El);
        end;
      end;
    end
  else if DeclEl.ClassType=TPasUnresolvedSymbolRef then
    begin
    if DeclEl.CustomData is TResElDataBuiltInProc then
      begin
      BuiltInProc:=TResElDataBuiltInProc(DeclEl.CustomData);
      BuiltInProc.GetCallCompatibility(BuiltInProc,El,true);
      end;
    end
  else if (DeclEl.ClassType=TPasUsesUnit) or (DeclEl is TPasModule) then
    begin
    // unit reference
    // dotted unit name needs a ref for each expression identifier
    // Note: El is the first TPrimitiveExpr of the dotted unit name reference
    DottedName:=DeclEl.Name;
    repeat
      p:=Pos('.',DottedName);
      if p<1 then break;
      Delete(DottedName,1,p);
      El:=GetNextDottedExpr(El);
      if El=nil then
        RaiseInternalError(20170503002012);
      CreateReference(DeclEl,El,Access);
      if (El.Parent is TBinaryExpr) and (TBinaryExpr(El.Parent).Right=El) then
        begin
        Bin:=TBinaryExpr(El.Parent);
        while Bin.OpCode=eopSubIdent do
          begin
          CreateReference(DeclEl,Bin,Access);
          if not (Bin.Parent is TBinaryExpr) then break;
          if (TBinaryExpr(Bin.Parent).Right<>Bin) then break;
          Bin:=TBinaryExpr(Bin.Parent);
          end;
        end;
    until false;
    end;
end;

procedure TPasResolver.ResolveInherited(El: TInheritedExpr;
  Access: TResolvedRefAccess);
var
  SelfScope: TPasProcedureScope;
  AncestorScope: TPasClassScope;
  ClassRecScope: TPasClassOrRecordScope;
  DeclProc, AncestorProc: TPasProcedure;
  aClass: TPasClassType;
  HelperForType: TPasType;
  InhScope: TPasInheritedScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveInherited El.Parent=',GetTreeDbg(El.Parent));
  {$ENDIF}
  if (El.Parent.ClassType=TBinaryExpr)
  and (TBinaryExpr(El.Parent).OpCode=eopNone) then
    begin
    // e.g. 'inherited Proc;'
    ResolveInheritedName(TBinaryExpr(El.Parent),Access);
    exit;
    end;

  // 'inherited;' without expression
  SelfScope:=GetCurrentSelfScope(El);
  if SelfScope=nil then
    RaiseMsg(20170216152141,nInheritedOnlyWorksInMethods,sInheritedOnlyWorksInMethods,[],El);
  DeclProc:=SelfScope.DeclarationProc;
  if DeclProc=nil then
    RaiseNotYetImplemented(20190121172251,El);
  ClassRecScope:=SelfScope.ClassRecScope;

  if not (ClassRecScope is TPasClassScope) then
    begin
    // inherited in record method
    RaiseMsg(20181218194022,nTheUseOfXisNotAllowedInARecord,sTheUseOfXisNotAllowedInARecord,
      ['inherited'],El);
    end;
  AncestorProc:=nil;

  // inherited in class/interface/helper method
  aClass:=ClassRecScope.Element as TPasClassType;
  // inherited in record helper only allowed in ObjFPC
  if (aClass.ObjKind = okRecordHelper)
      and (msDelphi in CurrentParser.CurrentModeswitches) then
    RaiseMsg(20260225100009,nTheUseOfXisNotAllowedInARecord,
      sTheUseOfXisNotAllowedInARecord,['inherited'],El);
  HelperForType:=ResolveAliasType(aClass.HelperForType);
  //writeln('TPasResolver.ResolveInherited aClass=',GetObjName(aClass),' HelperForType=',GetObjName(HelperForType));
  if HelperForType is TPasMembersType then
    begin
    // inherited; inside helper -> skip helper ancestors and search in HelperForType
    if msDelphi in CurrentParser.CurrentModeswitches then
      begin
      // Delphi skips ancestors and HelperForType
      if not (HelperForType is TPasClassType) then
        // 'inherited;' without ancestor class is silently ignored
        exit;
      AncestorScope:=TPasClassScope(HelperForType.CustomData).AncestorScope;
      if AncestorScope=nil then
        // 'inherited;' without ancestor class is silently ignored
        exit;
      InhScope:=PushInheritedScope(TPasMembersType(AncestorScope.Element),true,nil);
      end
    else
      begin
      // ObjFPC searches first in HelperForType and its ancestors, then in
      // own ancestors
      AncestorScope:=TPasClassScope(aClass.CustomData).AncestorScope;
      InhScope:=PushInheritedScope(TPasMembersType(HelperForType),false,
        AncestorScope);
      end;
    end
  else
    begin
    // inherited; inside class/interface method
    // -> search in ancestor and its helper(s)
    AncestorScope:=TPasClassScope(ClassRecScope).AncestorScope;
    if AncestorScope=nil then
      // 'inherited;' without ancestor class is silently ignored
      exit;
    InhScope:=PushInheritedScope(TPasMembersType(AncestorScope.Element),true,nil);
    end;
  AncestorProc:=FindProcSameSignature(DeclProc.Name,DeclProc,InhScope,false);
  PopScope;
  if AncestorProc=nil then
    // 'inherited;' without ancestor DeclProc is silently ignored
    exit;

  if not (AncestorProc.Parent is TPasMembersType) then
    RaiseNotYetImplemented(20190121181234,El); // inconsistency

  CreateReference(AncestorProc,El,Access);
  if AncestorProc.IsAbstract then
    RaiseMsg(20170216152144,nAbstractMethodsCannotBeCalledDirectly,
      sAbstractMethodsCannotBeCalledDirectly,[],El);
end;

procedure TPasResolver.ResolveInheritedName(El: TBinaryExpr;
  Access: TResolvedRefAccess);
// El.OpCode=eopNone
// El.left is TInheritedExpr
// El.right is the identifier and/or paramexpr
var
  SelfScope: TPasProcedureScope;
  ClassRecScope: TPasClassOrRecordScope;
  AncestorClass, aClass: TPasClassType;
  HelperForType: TPasType;
  OnlyTypeMembers: Boolean;
  Proc: TPasProcedure;
  AncestorScope: TPasClassScope;
  InhScope: TPasInheritedScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveInheritedCall El=',GetTreeDbg(El),' Access=',Access);
  {$ENDIF}

  SelfScope:=GetCurrentSelfScope(El);
  if SelfScope=nil then
    RaiseMsg(20170216152148,nInheritedOnlyWorksInMethods,sInheritedOnlyWorksInMethods,[],El);
  ClassRecScope:=SelfScope.ClassRecScope;

  if not (ClassRecScope is TPasClassScope) then
    // inherited in a method of a record
    RaiseMsg(20181218194436,nTheUseOfXisNotAllowedInARecord,sTheUseOfXisNotAllowedInARecord,
      ['inherited'],El);

  Proc:=TPasProcedure(SelfScope.Element);
  OnlyTypeMembers:=IsClassMethod(Proc);

  // inherited in a method of a class/interface/helper
  aClass:=TPasClassType(ClassRecScope.Element);
  AncestorScope:=TPasClassScope(ClassRecScope).AncestorScope;
  if aClass.ObjKind in okAllHelpers then
    begin
    // inherited in record helper only allowed in ObjFPC
    if (aClass.ObjKind = okRecordHelper)
        and (msDelphi in CurrentParser.CurrentModeswitches) then
      RaiseMsg(20260225100008,nTheUseOfXisNotAllowedInARecord,
        sTheUseOfXisNotAllowedInARecord,['inherited'],El);
    HelperForType:=ResolveAliasType(aClass.HelperForType);
    if HelperForType is TPasMembersType then
      begin
      // record helper(ancestor) for aRecord
      // or class helper(ancestor) for aClass
      // -> search in helperfortype, then in ancestors
      InhScope:=PushInheritedScope(TPasMembersType(HelperForType),false,
        AncestorScope);
      InhScope.OnlyTypeMembers:=OnlyTypeMembers;
      ResolveExpr(El.Right,Access);
      PopScope;
      exit;
      end
    else
      begin
      // type helper(ancestortype) for simpletype -> search in ancestortype
      end;
    end
  else
    begin
    // class or interface -> search in ancestor and its helpers
    end;
  // search in ancestor and its helpers
  if AncestorScope<>nil then
    AncestorClass:=TPasClassType(AncestorScope.Element)
  else
    begin
    // class(T) inside a generic: search in T's class-type constraint
    AncestorClass:=nil;
    if TPasClassScope(ClassRecScope).DirectAncestor is TPasGenericTemplateType then
      AncestorClass:=GetClassTypeConstraint(
        TPasGenericTemplateType(TPasClassScope(ClassRecScope).DirectAncestor));
    if AncestorClass=nil then
      RaiseMsg(20170216152151,nInheritedNeedsAncestor,sInheritedNeedsAncestor,[],El.Left);
    end;
  // search call in ancestor
  InhScope:=PushInheritedScope(AncestorClass,true,nil);
  InhScope.OnlyTypeMembers:=OnlyTypeMembers;
  ResolveExpr(El.Right,Access);
  PopScope;
end;

procedure TPasResolver.ResolveBinaryExpr(El: TBinaryExpr;
  Access: TResolvedRefAccess);
var
  Left, Next: TPasExpr;
  Bin: TBinaryExpr;
  CmpResolved: TPasResolverResult;
begin
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.ResolveBinaryExpr left=',GetObjName(El.Left),' right=',GetObjName(El.Right),' opcode=',OpcodeStrings[El.OpCode]);
  {$ENDIF}
  case El.OpCode of
  eopNone:
    case El.Kind of
    pekRange:
      begin
      ResolveExpr(El.Left,rraRead);
      if El.Right=nil then exit;
      ResolveExpr(El.Right,rraRead);
      end;
    else
      if El.Left.ClassType=TInheritedExpr then
        begin
        ResolveExpr(El.Left,Access);
        end
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveBinaryExpr El.Kind=',ExprKindNames[El.Kind],' El.Left=',GetObjName(El.Left),' El.Right=',GetObjName(El.Right),' parent=',GetObjName(El.Parent));
        {$ENDIF}
        RaiseNotYetImplemented(20160922163456,El);
        end;
    end;
  eopAdd:
    begin
    // handle multi add
    Left:=El.Left;
    while (Left.ClassType=TBinaryExpr) do
      begin
      Bin:=TBinaryExpr(Left);
      if Bin.OpCode<>eopAdd then break;
      Next:=TBinaryExpr(Left).Left;
      if Next.Parent<>Left then
        RaiseNotYetImplemented(20210321201257,Left);
      Left:=Next;
      end;
    ResolveExpr(Left,rraRead);
    repeat
      Bin:=TBinaryExpr(Left.Parent);
      if Bin.Right<>nil then
        ResolveExpr(Bin.Right,rraRead);
      Left:=Bin;
    until Left=El;
    end;
  eopSubtract,
  eopMultiply,
  eopDivide,
  eopDiv,
  eopMod,
  eopPower,
  eopShr,
  eopShl,
  eopNot,
  eopAnd,
  eopOr,
  eopXor,
  eopEqual,
  eopNotEqual,
  eopLessThan,
  eopGreaterThan,
  eopLessthanEqual,
  eopGreaterThanEqual,
  eopIn,
  eopNotIn,
  eopIs,
  eopIsNot,
  eopAs,
  eopSymmetricaldifference:
    begin
    ResolveExpr(El.Left,rraRead);
    if El.Right=nil then exit;
    ResolveExpr(El.Right,rraRead);
    // `ProcVar = @Obj.OverloadedMethod`: the procvar's type picks the overload
    if (El.OpCode in [eopEqual,eopNotEqual]) and (El.Right is TUnaryExpr) then
      begin
      ComputeElement(El.Left,CmpResolved,[rcNoImplicitProcType]);
      RefineProcAddrForTarget(CmpResolved,El.Right);
      end
    else if (El.OpCode in [eopEqual,eopNotEqual]) and (El.Left is TUnaryExpr) then
      begin
      ComputeElement(El.Right,CmpResolved,[rcNoImplicitProcType]);
      RefineProcAddrForTarget(CmpResolved,El.Left);
      end;
    end;
  eopSubIdent:
    begin
    ResolveExpr(El.Left,rraRead);
    if El.Right=nil then exit;
    ResolveSubIdent(El,Access);
    end;
  else
    RaiseNotYetImplemented(20160922163459,El,OpcodeStrings[El.OpCode]);
  end;
end;

procedure TPasResolver.ResolveSubIdent(El: TBinaryExpr;
  Access: TResolvedRefAccess);

  procedure ResolveRight; inline;
  begin
    ResolveExpr(El.Right,Access);
    PopScope;
  end;

  function SearchInTypeHelpers(HiType: TPasType;
    const LeftRes: TPasResolverResult): boolean;
  var
    DotScope: TPasDotBaseScope;
  begin
    if HiType=nil then exit(false);
    DotScope:=PushHelperDotScope(HiType);
    if DotScope=nil then exit(false);
    // Only a TYPE NAME restricts the scope to type members. A readable VALUE
    // whose IdentEl merely happens to be a type - a built-in call such as
    // Ord(x), whose declaration is a TPasUnresolvedSymbolRef - must still reach
    // instance members: fcl-net writes `ord(Addr.AddressType).ToString`.
    if (LeftRes.IdentEl is TPasType)
        and not (rrfReadable in LeftRes.Flags) then
      // e.g. TFlag.HelperProc
      DotScope.OnlyTypeMembers:=true;
    ResolveRight;
    Result:=true;
  end;

  function LitIntBaseType(V: TMaxPrecInt): TResolverBaseType;
  // FPC binds a type helper on an integer constant by the constant's inherent
  // type: the smallest type that holds the value, PREFERRING SIGNED at each byte
  // size (2->ShortInt, 200->Byte, 40000->Word).
  begin
    if V>=0 then
      begin
      if V<=High(ShortInt) then Result:=btShortInt
      else if V<=High(Byte) then Result:=btByte
      else if V<=High(SmallInt) then Result:=btSmallInt
      else if V<=High(Word) then Result:=btWord
      else if V<=High(LongInt) then Result:=btLongint
      else if V<=TMaxPrecInt(High(LongWord)) then Result:=btLongWord
      {$IFNDEF PAS2JS}
      else Result:=btInt64;
      {$ENDIF}
      end
    else
      begin
      if V>=Low(ShortInt) then Result:=btShortInt
      else if V>=Low(SmallInt) then Result:=btSmallInt
      else if V>=Low(LongInt) then Result:=btLongint
      {$IFNDEF PAS2JS}
      else Result:=btInt64;
      {$ENDIF}
      end;
  end;

  function NumericLiteral(E: TPasExpr): TPrimitiveExpr;
  // The numeric literal underlying E, unwrapping a leading sign. The parser's
  // -N.member fold makes El.left a TUnaryExpr(eopSubtract, number); Eval(E) then
  // yields the signed value. nil if E is not a (possibly signed) numeric literal.
  begin
    Result:=nil;
    if (E is TUnaryExpr) and (TUnaryExpr(E).OpCode in [eopSubtract,eopAdd]) then
      E:=TUnaryExpr(E).Operand;
    if (E is TPrimitiveExpr) and (TPrimitiveExpr(E).Kind=pekNumber) then
      Result:=TPrimitiveExpr(E);
  end;

  function IsHidingNamespace(const aName: string): boolean;
  // True if aName denotes a namespace: a (proper) prefix of a used dotted unit's
  // name in an in-scope section, or a prefix of the module's own dotted
  // namespace. FPC's dotted-namespace precedence: such a namespace hides a
  // shorter used unit with the same leading name, so aName.X must resolve via a
  // unit named aName (none here) rather than as a member of that shorter unit.
  var
    si, ui: Integer;
    Sc: TPasScope;
    Sect: TPasSection;
    U: TPasUsesUnit;
  begin
    Result:=false;
    if aName='' then exit;
    if (FDefaultNameSpace<>'') and IsDottedIdentifierPrefix(aName,FDefaultNameSpace) then
      exit(true);
    for si:=ScopeCount-1 downto 0 do
      begin
      Sc:=Scopes[si];
      if not (Sc is TPasSectionScope) then continue;
      Sect:=TPasSection(TPasSectionScope(Sc).Element);
      if Sect=nil then continue;
      for ui:=0 to Sect.UsesList.Count-1 do
        begin
        U:=Sect.UsesClause[ui];
        if (U<>nil) and (CompareText(U.Name,aName)<>0)
            and IsDottedIdentifierPrefix(aName,U.Name) then
          exit(true);
        end;
      end;
  end;

  function DeferMissingMember: boolean;
  // True, after popping the dot scope, when the member is not in it.
  var
    FindData: TPRFindData;
    Abort: boolean;
  begin
    Result:=false;
    if not (El.Right is TPrimitiveExpr)
        or (TPrimitiveExpr(El.Right).Kind<>pekIdent) then
      exit;
    FindData:=Default(TPRFindData);
    FindData.ErrorPosEl:=El.Right;
    Abort:=false;
    TopScope.IterateElements(TPrimitiveExpr(El.Right).Value,TopScope,@OnFindFirst,@FindData,Abort);
    if FindData.Found<>nil then
      exit;
    PopScope;
    Result:=true;
  end;

  function DeferUndecidedMember(HiType: TPasType): boolean;
  // True, after popping the dot scope, when the member is not found and the
  // left side's declared type still depends on a type parameter: only the
  // specialization can say what that type has.
  begin
    Result:=false;
    if not FPartialSpecsAsGeneric or (HiType=nil)
        or not ParamIsUndecided(HiType) then
      exit;
    Result:=DeferMissingMember;
  end;

var
  aModule: TPasModule;
  ClassEl: TPasClassType;
  ClassScope: TPasDotClassScope;
  LeftResolved: TPasResolverResult;
  Left: TPasExpr;
  RecordEl: TPasRecordType;
  RecordScope: TPasDotClassOrRecordScope;
  LLoTypeEl, LHiTypeEl, DestEl: TPasType;
  DotScope: TPasDotBaseScope;
  SetType: TPasSetType;
  LitPrim: TPrimitiveExpr;
  LitVal: TResEvalValue;
  LitBt: TResolverBaseType;
  REx: TPasExpr;
  RangeResolved: TPasResolverResult;
begin
  if El.CustomData is TResolvedReference then
    exit; // for example, when a.b has a dotted unit name

  // a.b.c where b was left for the specialization: so is c
  if (El.Left is TBinaryExpr) and (TBinaryExpr(El.Left).OpCode=eopSubIdent)
      and (TBinaryExpr(El.Left).Right is TPrimitiveExpr)
      and (TPrimitiveExpr(TBinaryExpr(El.Left).Right).Kind=pekIdent)
      and not (TBinaryExpr(El.Left).Right.CustomData is TResolvedReference) then
    exit;

  Left:=El.Left;
  //writeln('TPasResolver.ResolveSubIdent Left=',GetObjName(Left));
  ComputeElement(Left,LeftResolved,[rcSetReferenceFlags]);

  if LeftResolved.BaseType=btModule then
    begin
    // e.g. unitname.identifier
    // => search in interface and if this is our module in the implementation
    aModule:=NoNil(LeftResolved.IdentEl) as TPasModule;
    // Dotted-namespace precedence: if <unit>.<right> forms a namespace (a prefix
    // of a used dotted unit, or the module's own namespace) with no unit of that
    // name, the unit is hidden and the qualified name cannot resolve via it.
    if (El.Right is TPrimitiveExpr)
        and IsHidingNamespace(aModule.Name+'.'+TPrimitiveExpr(El.Right).Value) then
      RaiseIdentifierNotFound(20260624120000,
        aModule.Name+'.'+TPrimitiveExpr(El.Right).Value,El.Right);
    PushModuleDotScope(aModule);
    ResolveRight;
    exit;
    end
  else if LeftResolved.LoTypeEl=nil then
    begin
    // A `nil` literal (BaseType=btNil, LoTypeEl=nil) can still have a
    // `type helper for Pointer` — search it before falling through to the
    // illegal-qualifier error (tthlp4: Nil.Test binds the Pointer helper).
    if (LeftResolved.BaseType=btNil) and (FBaseTypes[btPointer]<>nil) then
      if SearchInTypeHelpers(FBaseTypes[btPointer],LeftResolved) then exit;
    // else illegal qualifier, see below
    end
  else
    begin
    LHiTypeEl:=LeftResolved.HiTypeEl;
    LLoTypeEl:=LeftResolved.LoTypeEl;
    if (LLoTypeEl.ClassType=TPasPointerType)
        and ElHasModeSwitch(El,msAutoDeref)
        and (rrfReadable in LeftResolved.Flags)
        then
      begin
      // a.b  ->  a^.b
      LHiTypeEl:=TPasPointerType(LLoTypeEl).DestType;
      LLoTypeEl:=ResolveAliasType(LHiTypeEl);
      Include(LeftResolved.Flags,rrfWritable);
      end;

    //writeln('TPasResolver.ResolveSubIdent ',GetObjPath(El),' ',GetObjPath(LLoTypeEl));
    if LLoTypeEl.ClassType=TPasClassType then
      begin
      ClassEl:=TPasClassType(LLoTypeEl);
      if ClassEl.HelperForType<>nil then
        RaiseHelpersCannotBeUsedAsType(20190123093438,El);
      ClassScope:=PushClassDotScope(ClassEl);
      if LeftResolved.IdentEl is TPasType then
        begin
        // e.g. TFPMemoryImage.FindHandlerFromExtension()
        ClassScope.OnlyTypeMembers:=true;
        // @ClassOrObjectType.Method gets the code pointer of an instance method
        // via the type name (not a VMT dispatch). FPC allows this for TP objects
        // AND (non-external) classes -- classes/stringl.inc compares
        // @TStringList.ExchangeItems (declared address) with @Self.ExchangeItems
        // (dispatched) to detect an override. EXTERNAL classes (pas2js) keep the
        // OnlyTypeMembers restriction: an external-class instance must not reach
        // static class members this way (pas2js TestExternalClass_ClassMethod).
        if (El.Parent is TUnaryExpr)
            and (TUnaryExpr(El.Parent).OpCode = eopAddress)
            and ((ClassEl.ObjKind = okObject)
                 or ((ClassEl.ObjKind = okClass) and not ClassEl.IsExternal)) then
          ClassScope.OnlyTypeMembers:=false;
        end
      else
        // e.g. Image.Width
        ClassScope.OnlyTypeMembers:=false;
      if DeferUndecidedMember(LHiTypeEl) then
        exit;
      ResolveRight;
      exit;
      end
    else if LLoTypeEl.ClassType=TPasClassOfType then
      begin
      // e.g. ImageClass.FindHandlerFromExtension()
      DestEl:=ResolveAliasType(TPasClassOfType(LLoTypeEl).DestType);
      if DestEl is TPasGenericTemplateType then
        begin
        // `class of T`: the class a generic ancestor was given for T, else
        // what T is known to be: its class constraint or the root class
        ClassEl:=BoundAncestorArgClass(TPasGenericTemplateType(DestEl));
        if ClassEl=nil then
          ClassEl:=GetClassTypeConstraint(TPasGenericTemplateType(DestEl));
        if ClassEl=nil then
          begin
          DestEl:=ResolveAliasType(TPasType(FindElementWithoutParams('TObject',El,false,true)));
          if not (DestEl is TPasClassType) then
            exit; // left to the specialization
          ClassEl:=TPasClassType(DestEl);
          end;
        end
      else
        ClassEl:=DestEl as TPasClassType;
      ClassScope:=PushClassDotScope(ClassEl);
      ClassScope.OnlyTypeMembers:=true;
      ClassScope.IsClassOf:=true;
      ResolveRight;
      exit;
      end
    else if LLoTypeEl.ClassType=TPasRecordType then
      begin
      RecordEl:=TPasRecordType(LLoTypeEl);
      RecordScope:=PushRecordDotScope(RecordEl);
      RecordScope.ConstParent:=not (rrfWritable in LeftResolved.Flags);
      // A TYPE NAME is not readable; a VALUE is. Without the readable test an
      // operator's record result counted as a type reference and its own methods
      // came back "inaccessible here" - rtl-objpas' system.math.vectors writes
      // `(PCurr-PLast).Length`.
      if (LeftResolved.IdentEl is TPasType)
          and not (rrfReadable in LeftResolved.Flags) then
        // e.g. TPoint.PointInCircle
        RecordScope.OnlyTypeMembers:=true
      else
        begin
        // e.g. aPoint.X
        AccessExpr(El.Left,Access);
        RecordScope.OnlyTypeMembers:=false;
        end;
      if DeferUndecidedMember(LHiTypeEl) then
        exit;
      // record composition of a template type: members are known after specialization
      if IsRecordComposingTemplate(RecordEl) and DeferMissingMember then
        exit;
      ResolveRight;
      exit;
      end
    else if LLoTypeEl.ClassType=TPasEnumType then
      begin
      if (LeftResolved.IdentEl is TPasType)
          and (ResolveAliasType(TPasType(LeftResolved.IdentEl)).ClassType=TPasEnumType) then
        begin
        // e.g. TShiftState.ssAlt
        DotScope:=PushEnumDotScope(LHiTypeEl,TPasEnumType(LLoTypeEl));
        DotScope.OnlyTypeMembers:=true;
        ResolveRight;
        exit;
        end;
      end
    else if LLoTypeEl.ClassType=TPasRangeType then
      begin
      // e.g. TRange.EnumValue where TRange is a subrange of an enum: resolve the
      // member against the subrange's base enum type (e.g. TRange1 = first..second).
      if LeftResolved.IdentEl is TPasType then
        begin
        REx:=TPasRangeType(LLoTypeEl).RangeExpr;
        if (REx is TBinaryExpr) and (REx.Kind=pekRange) then
          REx:=TBinaryExpr(REx).left;
        if REx<>nil then
          begin
          ComputeElement(REx,RangeResolved,[]);
          if (RangeResolved.LoTypeEl<>nil)
              and (RangeResolved.LoTypeEl.ClassType=TPasEnumType) then
            begin
            DotScope:=PushEnumDotScope(RangeResolved.HiTypeEl,
              TPasEnumType(RangeResolved.LoTypeEl));
            DotScope.OnlyTypeMembers:=true;
            ResolveRight;
            exit;
            end;
          end;
        end;
      end
    else if LLoTypeEl.ClassType=TPasGenericTemplateType then
      begin
      DotScope:=PushTemplateDotScope(TPasGenericTemplateType(LLoTypeEl),El);
      if DotScope<>nil then
        begin
        if LeftResolved.IdentEl is TPasType then
          // e.g. T.Member
          DotScope.OnlyTypeMembers:=true
        else
          // e.g. VarOfTypeT.Member
          DotScope.OnlyTypeMembers:=false;
        // A parameter of ANOTHER generic, reached through a partial
        // specialization answered with that generic (`FList[i]` of a
        // TFPGObjectList<TItem<_ITEM>>), stands for a type only the
        // specialization knows: its constraint does not limit the members.
        if FPartialSpecsAsGeneric
            and not TemplateIsInScope(TPasGenericTemplateType(LLoTypeEl),El)
            and DeferMissingMember then
          exit;
        ResolveRight;
        exit;
        end;
      end;
    // Dynamic array type alias .Create pseudo-constructor
    // e.g. TIntArray.Create(1,2,3) where TIntArray = array of integer
    if (LLoTypeEl <> nil) and (LLoTypeEl.ClassType = TPasArrayType)
        and (Length(TPasArrayType(LLoTypeEl).Ranges) = 0)
        and (LeftResolved.IdentEl is TPasType)
        and (El.Right is TPrimitiveExpr)
        and (CompareText(TPrimitiveExpr(El.Right).Value, 'Create') = 0) then
      begin
      // Resolve .Create as a reference to the array type itself
      CreateReference(LLoTypeEl, El.Right, rraRead);
      exit;
      end;
    // default: search for type helpers
    if (LeftResolved.BaseType in btAllIntrinsicTypes)
        or (LeftResolved.BaseType=btContext)
        or (LeftResolved.BaseType=btPointer)
        {$IFNDEF PAS2JS} or (LeftResolved.BaseType=btVariant){$ENDIF}
        or (LeftResolved.BaseType=btCustom) then
      begin
      LHiTypeEl:=LeftResolved.HiTypeEl;
      // An address-of result (@x) resolves to btPointer but keeps the POINTEE as
      // its HiTypeEl. A raw pointer value binds the generic Pointer type helper,
      // not a helper on the pointee's type (tthlp4: (@ml).Test binds Pointer, not
      // the MyLongInt helper). Genuine typed-pointer variables are btContext and
      // are unaffected; a plain `Pointer` var already has this HiTypeEl.
      if (LeftResolved.BaseType=btPointer) and (FBaseTypes[btPointer]<>nil) then
        LHiTypeEl:=FBaseTypes[btPointer];
      // {$H-}: `String` denotes a ShortString, so both a bare string literal
      // ('lit'.Method — tthlp5) and the type name (String.ClassMethod —
      // tthlp26a) must PREFER the ShortString type helper over the AnsiString
      // one. Done as a FIRST-TRY (not by replacing LHiTypeEl): if no ShortString
      // helper exists but an AnsiString one does, fall through to the default
      // LHiTypeEl search below (upstream TestTypeHelper_String). Scoped to the
      // helper lookup only — no codegen blast radius; inert for pas2js (no
      // btShortString base type).
      if (LeftResolved.BaseType=btString)
          {$IFDEF FPC_HAS_CPSTRING}and (FBaseTypes[btShortString]<>nil){$ENDIF}
          and (CurrentParser<>nil)
          and not (bsLongStrings in CurrentParser.Scanner.CurrentBoolSwitches)
          and ( ((Left is TPrimitiveExpr)
                  and (TPrimitiveExpr(Left).Kind in [pekString,pekStringMultiLine]))
                or (LeftResolved.IdentEl is TPasType) ) then
        begin
        {$IFDEF FPC_HAS_CPSTRING}
        LitBt:=btShortString; 
        {$ELSE}
        litBt:=btUnicodeString;
        {$ENDIF}
        // A string literal containing a wide codepoint (>255) cannot be a
        // ShortString; FPC types it UnicodeString (tthlp4: 'Uni'#1234.Test binds
        // the UnicodeString helper, not ShortString). The evaluated constant kind
        // is revkUnicodeString exactly when the literal has wide chars.
        if (Left is TPrimitiveExpr)
            and (TPrimitiveExpr(Left).Kind in [pekString,pekStringMultiLine])
            and (FBaseTypes[btUnicodeString]<>nil) then
          begin
          LitVal:=Eval(Left,[]);
          if LitVal<>nil then
            try
              if LitVal.Kind=revkUnicodeString then
                LitBt:=btUnicodeString;
            finally
              ReleaseEvalValue(LitVal);
            end;
          end;
        if (FBaseTypes[LitBt]<>nil) and (FBaseTypes[LitBt]<>LHiTypeEl) then
          if SearchInTypeHelpers(FBaseTypes[LitBt],LeftResolved) then exit;
        end;
      // An integer *literal* binds the helper by its inherent smallest,
      // signed-preferring type (tthlp4: 2->ShortInt, 200->Byte, 40000->Word),
      // not by its btLongint value type. Try that type first; if no helper
      // exists for it, fall back to the default-width search below. Scoped to
      // the lookup — the literal keeps btLongint everywhere else (no codegen
      // blast radius; the helper Self is typed from the resolved helper).
      LitPrim:=NumericLiteral(Left);
      if (LitPrim<>nil)
          and (LeftResolved.BaseType in btAllInteger)
          and not NumberIsFloat(LitPrim.Value) then
        begin
        LitVal:=Eval(Left,[]);
        if LitVal<>nil then
          try
            LitBt:=btNone;
            case LitVal.Kind of
            revkInt: LitBt:=LitIntBaseType(TResEvalInt(LitVal).Int);
            {$IFDEF HasInt64}
            revkUInt: LitBt:=btQWord;
            {$ENDIF}
            end;
            if (LitBt<>btNone) and (FBaseTypes[LitBt]<>nil)
                and (FBaseTypes[LitBt]<>LHiTypeEl) then
              if SearchInTypeHelpers(FBaseTypes[LitBt],LeftResolved) then exit;
          finally
            ReleaseEvalValue(LitVal);
          end;
        end;
      // A float *literal* binds the helper by FPC's rule: Single when the value
      // is exactly representable as Single, otherwise Extended (tthlp4: 1.25 is
      // Single, 0.1/1.25e10 are Extended — never auto-Double). The default width
      // (LHiTypeEl) already covers the else case, so only the Single-exact
      // literal needs redirecting. Scoped to the lookup.
      LitPrim:=NumericLiteral(Left);
      if (LitPrim<>nil)
          and (LeftResolved.BaseType in btAllFloats)
          and NumberIsFloat(LitPrim.Value)
          and (FBaseTypes[btSingle]<>nil) then
        begin
        LitVal:=Eval(Left,[]);
        if (LitVal<>nil) and (LitVal.Kind=revkFloat) then
          try
            if TMaxPrecFloat(Single(TResEvalFloat(LitVal).FloatValue))
                = TResEvalFloat(LitVal).FloatValue then
              if SearchInTypeHelpers(FBaseTypes[btSingle],LeftResolved) then exit;
          finally
            ReleaseEvalValue(LitVal);
          end;
        end;
      if SearchInTypeHelpers(LHiTypeEl,LeftResolved) then exit;
      end
    else if LeftResolved.BaseType=btSet then
      begin
      SetType:=GetSetType(LeftResolved);
      if SearchInTypeHelpers(SetType,LeftResolved) then exit;
      end;
    end;

  // A member access on a still-unbound generic template type can only be checked
  // once T is known, so defer it to specialization instead of refusing here -
  // the same treatment `with` and indexing on a template already get.
  // rtl/objpas/fgl.pp calls `T(Dest^)._Release` in TFPGInterfacedObjectList.
  if IsGenericTemplType(LeftResolved) then
    exit;
  // The same where the left side merely DEPENDS on a type parameter rather than
  // being one: `LItem: TQueueDictionary.PValue` is `^TValue` taken from the bare
  // generic once a partial specialization answers with it
  // (PartialSpecsAsGeneric). What `.Pair` means there is settled when the
  // generic is specialized.
  if FPartialSpecsAsGeneric and (LeftResolved.LoTypeEl<>nil)
      and ParamIsUndecided(LeftResolved.LoTypeEl) then
    exit;

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveSubIdent left=',GetObjName(Left),' right=',GetObjName(El.Right),' leftresolved=',GetResolverResultDbg(LeftResolved));
  {$ENDIF}
  RaiseMsg(20170216152157,nIllegalQualifierAfter,sIllegalQualifierAfter,
    ['.',GetResolverResultDescription(LeftResolved)],El);
end;

procedure TPasResolver.ResolveParamsExpr(Params: TParamsExpr;
  Access: TResolvedRefAccess);
begin
  if (Params.Kind=pekSet) and not (Access in [rraRead,rraParamToUnknownProc]) then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ResolveParamsExpr SET literal Access=',Access);
    {$ENDIF}
    RaiseMsg(20170303211052,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Params);
    end;

  // first resolve params
  ResolveParamsExprParams(Params);

  // then resolve the call, typecast, array, set
  if (Params.Kind=pekFuncParams) then
    ResolveFuncParamsExpr(Params,Access)
  else if (Params.Kind=pekArrayParams) then
    ResolveArrayParamsExpr(Params,Access)
  else if (Params.Kind=pekSet) then
    ResolveSetParamsExpr(Params)
  else
    RaiseNotYetImplemented(20160922163501,Params);
end;

procedure TPasResolver.ResolveParamsExprParams(Params: TParamsExpr);
var
  ScopeDepth, i: integer;
  ParamAccess: TResolvedRefAccess;
  Pars: TPasExprArray;
  TmpName: string;
  MaybeNew: boolean;
  ParResolved: TPasResolverResult;
begin
  ScopeDepth:=StashSubExprScopes;
  if Params.Kind in [pekFuncParams,pekArrayParams] then
    ParamAccess:=rraParamToUnknownProc
  else
    ParamAccess:=rraRead;
  Pars:=Params.Params;
  MaybeNew:=false;
  if (Params.Kind=pekFuncParams) and (Length(Pars)>=2)
      and (Params.Value is TPrimitiveExpr) then
    begin
    TmpName:=LowerCase(TPrimitiveExpr(Params.Value).Value);
    MaybeNew:=(TmpName='new') or (TmpName='dispose');
    end;
  if MaybeNew then
    begin
    // For the BUILT-IN New(P,Ctor)/Dispose(P,Dtor), the 2nd parameter names a
    // constructor and must be resolved in the POINTED-TO type's scope, not
    // here. The name alone does not say which one this is: a user routine
    // called New takes ordinary arguments, and leaving them unresolved made
    // overload ranking fail on the second one (fcl-sdo's
    // TDataAccessInterface.New). The first parameter being a POINTER is what
    // tells the two apart.
    ResolveExpr(Pars[0],ParamAccess);
    ComputeElement(Pars[0],ParResolved,[rcNoImplicitProc]);
    MaybeNew:=(ParResolved.BaseType=btContext)
      and (ParResolved.LoTypeEl is TPasPointerType);
    if not MaybeNew then
      for i:=1 to Length(Pars)-1 do
        ResolveExpr(Pars[i],ParamAccess);
    end
  else
    for i:=0 to Length(Pars)-1 do
      ResolveExpr(Pars[i],ParamAccess);
  RestoreStashedScopes(ScopeDepth);
end;

procedure TPasResolver.ResolveFuncParamsExpr(Params: TParamsExpr;
  Access: TResolvedRefAccess);
var
  Value: TPasExpr;
  SubParams: TParamsExpr;
  ResolvedEl: TPasResolverResult;
begin
  Value:=Params.Value;
  if Value is TBinaryExpr then
    begin
    // Note: a.b() is the same as (a.b)()
    // Note: a.b().c  is stored as
    //             TBinaryExpr eopSubIdent
    //           /                         \
    //         left = TParamsExpr           right = TPrimitiveExpr 'c'
    //         Value = TBinaryExpr
    //         /                 \
    // left = TPrimitiveExpr 'a'  right = TPrimitiveExpr 'b'
    if (Value is TBinaryExpr) and (TBinaryExpr(Value).OpCode=eopSubIdent) then
      Value:=TBinaryExpr(Value).Right;
    if IsNameExpr(Value) then
      begin
      ResolveBinaryExpr(TBinaryExpr(Params.Value),Access);
      if not (Value.CustomData is TResolvedReference) then
        begin
        // A CALL on a member of a still-unbound template type (T.Method(x)) is
        // checked at specialization, like the member access itself. fcl-stl's
        // garrayutils calls TCompare.c(a,b).
        if IsDeferredTemplMember(Value) then
          exit;
        RaiseNotYetImplemented(20190115140557,Params);
        end;
      // already resolved
      exit;
      end
    else if Value.ClassType=TInlineSpecializeExpr then
      begin
      ResolveBinaryExpr(TBinaryExpr(Params.Value),Access);
      // already resolved
      exit;
      end;
    // ToDo: (a+b)()
    //ResolveBinaryExpr(TBinaryExpr(Params.Value),rraRead);
    RaiseNotYetImplemented(20190115140809,Params);
    end
  else if IsNameExpr(Value) then
    ResolveFuncParamsExprName(Value,nil,Params,Access)
  else if Value.ClassType=TInlineSpecializeExpr then
    begin
    // e.g. Name<>()
    ResolveInlineSpecializeExpr(TInlineSpecializeExpr(Value),Access);
    end
  else if Value.ClassType=TParamsExpr then
    begin
    SubParams:=TParamsExpr(Value);
    if (SubParams.Kind in [pekArrayParams,pekFuncParams]) then
      begin
      // e.g. Name()() or Name[]()
      ResolveParamsExpr(SubParams,rraRead);
      ComputeElement(SubParams,ResolvedEl,[rcNoImplicitProc,rcSetReferenceFlags]);
      if IsProcedureType(ResolvedEl,true) then
        begin
        // `List[i](..)` on a default array property: List[i] already refers
        // to the property
        if not (Value.CustomData is TResolvedReference) then
          CreateReference(TPasProcedureType(ResolvedEl.LoTypeEl),Value,Access);
        FinishProcParamAccess(TPasProcedureType(ResolvedEl.LoTypeEl),Params);
        exit;
        end;
      if (GetTypeOfCastRoot(Params)<>nil) and (SubParams.CustomData=nil)
          and (rrfReadable in ResolvedEl.Flags) and (ResolvedEl.HiTypeEl<>nil) then
        begin
        // "type of a[0](b)" -> typecast b to the type of a[0]
        FinishTypeOfCast(Params,ResolvedEl.HiTypeEl,Access);
        exit;
        end;
      end;
    RaiseMsg(20170216152202,nIllegalQualifierAfter,sIllegalQualifierAfter,
      ['(',SubParams.ElementTypeName],Params);
    end
  else if Value.ClassType=TUnaryExpr then
    begin
    if TUnaryExpr(Value).OpCode=eopTypeOf then
      begin
      // "(type of a)(b)" -> typecast b to the type of a
      ResolveExpr(Value,rraRead);
      if not (Value.CustomData is TResolvedReference) then
        RaiseMsg(20260922100010,nIllegalQualifierAfter,sIllegalQualifierAfter,
          ['(',Value.ElementTypeName],Params);
      CheckTypeCast(ResolveAliasType(TResolvedReference(Value.CustomData).Declaration as TPasType),Params,true);
      FinishTypeCastParamAccess(TResolvedReference(Value.CustomData).Declaration as TPasType,Params,Access);
      exit;
      end
    else if (TUnaryExpr(Value).OpCode=eopDeref) and (GetTypeOfCastRoot(Params)<>nil) then
      begin
      // "type of p^(b)" -> typecast b to the type of p^
      ResolveExpr(Value,rraRead);
      ComputeElement(Value,ResolvedEl,[rcNoImplicitProc,rcSetReferenceFlags]);
      if not IsProcedureType(ResolvedEl,true)
          and (rrfReadable in ResolvedEl.Flags) and (ResolvedEl.HiTypeEl<>nil) then
        begin
        FinishTypeOfCast(Params,ResolvedEl.HiTypeEl,Access);
        exit;
        end;
      end;
    RaiseMsg(20260922100011,nIllegalQualifierAfter,sIllegalQualifierAfter,
      ['(',Value.ElementTypeName],Params);
    end
  else if Value.ClassType=TProcedureExpr then
    begin
    // inline call of anonymous proc: procedure begin ... end(args)
    // TProcedureExpr is already resolved by FinishScope(stProcedure)
    // Resolve parameters against the anonymous proc's type
    FinishProcParamAccess(TProcedureExpr(Value).Proc.ProcType,Params);
    end
  else
    RaiseNotYetImplemented(20161014085118,Params.Value);
end;

procedure TPasResolver.ResolveTypeOfExpr(El: TUnaryExpr;
  Access: TResolvedRefAccess);
// "type of Operand"
// Either a type, e.g. "SizeOf(type of a)",
// or a typecast, e.g. "type of a(b)", which is a value.
var
  TypeEl: TPasType;
begin
  Inc(FTypeOfOperandLevel);
  try
    ResolveExpr(El.Operand,Access);
    if IsTypeOfCastExpr(El) then
      exit; // a value
    TypeEl:=GetTypeOfOperandType(El.Operand,El);
  finally
    Dec(FTypeOfOperandLevel);
  end;
  CreateReference(TypeEl,El,rraRead);
end;

procedure TPasResolver.FinishTypeOfCast(Params: TParamsExpr; TypeEl: TPasType;
  Access: TResolvedRefAccess);
// "type of Value(Param)" -> typecast Param to TypeEl, the type of Value
var
  Ref: TResolvedReference;
begin
  CheckTypeCast(ResolveAliasType(TypeEl),Params,true);
  Ref:=CreateReference(TypeEl,Params.Value,rraRead);
  Include(Ref.Flags,rrfTypeOfCast);
  FinishTypeCastParamAccess(TypeEl,Params,Access);
end;

procedure TPasResolver.FinishTypeCastParamAccess(TypeEl: TPasType;
  Params: TParamsExpr; Access: TResolvedRefAccess);
var
  i: Integer;
begin
  TypeEl:=ResolveAliasType(TypeEl);
  if TypeEl is TPasProcedureType then
    AccessExpr(Params.Params[0],Access)
  else if Access<>rraParamToUnknownProc then
    for i:=0 to length(Params.Params)-1 do
      FinishCallArgAccess(Params.Params[i],Access);
end;

procedure TPasResolver.ResolveFuncParamsExprName(NameExpr: TPasExpr;
  TemplParams: TFPList; Params: TParamsExpr; Access: TResolvedRefAccess;
  CallName: string);

  procedure RaiseMultiFit;
  var
    FindCallData: TFindCallElData;
    Msg: String;
    i: Integer;
    El: TPasElement;
    Abort: boolean;
  begin
    FindCallData:=Default(TFindCallElData);
    FindCallData.Params:=Params;
    FindCallData.List:=TFPList.Create;
    try
      Abort:=false;
      IterateElements(CallName,@OnFindCallElements,@FindCallData,Abort);
      Msg:='';
      for i:=0 to FindCallData.List.Count-1 do
        begin
        El:=TPasElement(FindCallData.List[i]);
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveFuncParamsExpr Overload Candidate: ',GetElementSourcePosStr(El),' ',GetTreeDbg(El));
        {$ENDIF}
        // emit a hint for each candidate
        if El is TPasProcedure then
          LogMsg(20170417180320,mtHint,nFoundCallCandidateX,sFoundCallCandidateX,
            [GetProcTypeDescription(TPasProcedure(El).ProcType,
              [prptdUseName,prptdAddPaths,prptdResolveSimpleAlias])],El);
        Msg:=Msg+', '+GetElementSourcePosStr(El);
        end;
    finally
      FindCallData.List.Free;
    end;
    RaiseMsg(20170216152200,nCantDetermineWhichOverloadedFunctionToCall,
      sCantDetermineWhichOverloadedFunctionToCall+Msg,[CallName],NameExpr);
  end;

  procedure FinishUntypedParams(ParamAccess: TResolvedRefAccess);
  var
    i: Integer;
  begin
    if ParamAccess=rraParamToUnknownProc then exit;
    for i:=0 to length(Params.Params)-1 do
      FinishCallArgAccess(Params.Params[i],ParamAccess);
  end;

  procedure CheckTemplParams(GenTemplates, TemplParams: TFPList);
  var
    i: Integer;
    Param, PosEl: TPasElement;
    ResolvedEl: TPasResolverResult;
  begin
    for i:=0 to TemplParams.Count-1 do
      begin
      if TPasGenericTemplateType(GenTemplates[i]).IsConst then
        continue; // const param constraints are checked in GetSpecializedEl
      Param:=TPasElement(TemplParams[i]);
      ComputeElement(Param,ResolvedEl,[rcType]);
      if Param is TPasExpr then
        PosEl:=Param
      else
        PosEl:=Params;
      if CheckTemplateFitsParamRes(TPasGenericTemplateType(GenTemplates[i]),
          ResolvedEl,prtcoAssignToTempl,PosEl)=cIncompatible then
        // should have raise error
        RaiseNotYetImplemented(20190919095604,PosEl,GetResolverResultDbg(ResolvedEl));
      end;
  end;

  procedure CheckIncompatibleProc(const CallName: string;
    FoundProcType: TPasProcedureType; TemplParamsCnt: integer);
  var
    FoundTemplCnt: Integer;
    aName: String;
  begin
    CheckCallProcCompatibility(FoundProcType,Params,true);
    if FoundProcType.GenericTemplateTypes<>nil then
      FoundTemplCnt:=FoundProcType.GenericTemplateTypes.Count
    else
      FoundTemplCnt:=0;
    if TemplParamsCnt<>FoundTemplCnt then
      begin
      if FoundProcType.Parent is TPasProcedure then
        aName:=FoundProcType.Parent.Name
      else
        aName:=FoundProcType.Name;
      if aName='' then
        aName:=GetObjPath(FoundProcType);
      RaiseMsg(20201101205447,nXExpectedButYFound,sXExpectedButYFound,
               [aName,CallName+GetGenericParamCommas(TemplParamsCnt)],Params);
      end;
  end;

var
  FindCallData: TFindCallElData;
  Abort: boolean;
  FoundEl: TPasElement;
  Ref: TResolvedReference;
  FindData: TPRFindData;
  BuiltInProc: TResElDataBuiltInProc;
  ResolvedEl: TPasResolverResult;
  TypeEl: TPasType;
  C: TClass;
  TemplParamsCnt: Integer;
  GenTemplates, InferenceParams: TFPList;
  InvokeProcType: TPasProcedureType;
  IsTypeOfCast: Boolean;
  ViaAncestorArg: boolean;
begin
  // e.g. Name() -> find compatible
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.ResolveFuncParamsExprName NameExpr=',GetObjName(NameExpr),' TemplParams=',TemplParams<>nil,' CallName="',CallName,'"');
  {$ENDIF}
  if CallName<>'' then
  else if NameExpr.ClassType=TPrimitiveExpr then
    CallName:=TPrimitiveExpr(NameExpr).Value
  else
    RaiseNotYetImplemented(20190115143539,NameExpr);
  FindCallData:=Default(TFindCallElData);
  FindCallData.Params:=Params;
  if TemplParams<>nil then
    begin
    TemplParamsCnt:=TemplParams.Count;
    FindCallData.TemplCnt:=TemplParamsCnt;
    FindCallData.TemplParams:=TemplParams;
    end
  else
    TemplParamsCnt:=0;
  Abort:=false;
  IterateElements(CallName,@OnFindCallElements,@FindCallData,Abort);
  FoundEl:=FindCallData.Found;
  ViaAncestorArg:=false;
  if (FoundEl=nil) and FPartialSpecsAsGeneric then
    begin
    // a method of a partial specialization's type argument, see
    // FindMemberViaAncestorArgs
    FoundEl:=FindMemberViaAncestorArgs(CallName);
    if FoundEl is TPasProcedure then
      begin
      ViaAncestorArg:=true;
      FindCallData.Found:=FoundEl;
      FindCallData.StartScope:=TopScope;
      FindCallData.ElScope:=TopScope;
      FindCallData.Count:=1;
      FindCallData.Distance:=CheckCallProcCompatibility(
        TPasProcedure(FoundEl).ProcType,Params,false);
      end
    else
      FoundEl:=nil;
    end;
  if FoundEl=nil then
    RaiseIdentifierNotFound(20170216152544,CallName,NameExpr);
  IsTypeOfCast:=false;
  if (FindCallData.Distance=cIncompatible)
      and (FindCallData.Count<=1)
      and (GetTypeOfCastRoot(Params)<>nil) then
    begin
    // "type of Value(Param)" -> typecast Param to the type of Value
    TypeEl:=GetTypeOfCastValueType(FoundEl);
    if TypeEl<>nil then
      begin
      IsTypeOfCast:=true;
      FoundEl:=TypeEl;
      FindCallData.Distance:=cExact;
      FindCallData.Count:=1;
      end;
    end;
  if FindCallData.Distance=cIncompatible then
    begin
    // FoundEl one element, but it was incompatible => raise error
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ResolveFuncParamsExpr found one element, but it was incompatible => check again to raise error. Found=',GetObjName(FindCallData.Found));
    WriteScopes;
    {$ENDIF}
    if FoundEl is TPasProcedure then
      CheckIncompatibleProc(CallName,TPasProcedure(FoundEl).ProcType,TemplParamsCnt)
    else if FoundEl is TPasProcedureType then
      CheckTypeCast(TPasProcedureType(FoundEl),Params,true)
    else if FoundEl.ClassType=TPasUnresolvedSymbolRef then
      begin
      if FoundEl.CustomData is TResElDataBuiltInProc then
        begin
        BuiltInProc:=TResElDataBuiltInProc(FoundEl.CustomData);
        BuiltInProc.GetCallCompatibility(BuiltInProc,Params,true);
        RaiseNotYetImplemented(20200525124749,FoundEl,'missing exception, Found=['+BuiltInProc.Signature+']');
        end
      else if FoundEl.CustomData is TResElDataBaseType then
        CheckTypeCast(TPasUnresolvedSymbolRef(FoundEl),Params,true)
      else
        RaiseNotYetImplemented(20161006132825,FoundEl);
      end
    else if FoundEl is TPasType then
      // Note: check TPasType after TPasUnresolvedSymbolRef
      CheckTypeCast(TPasType(FoundEl),Params,true)
    else if FoundEl is TPasVariable then
      begin
      TypeEl:=ResolveAliasType(TPasVariable(FoundEl).VarType);
      if TypeEl is TPasProcedureType then
        CheckIncompatibleProc(CallName,TPasProcedureType(TypeEl),TemplParamsCnt)
      else
        begin
        InvokeProcType:=GetFuncRefInvokeProcType(TypeEl,length(Params.Params));
        if InvokeProcType<>nil then
          CheckIncompatibleProc(CallName,InvokeProcType,TemplParamsCnt)
        else
          RaiseMsg(20170405003522,nIllegalQualifierAfter,sIllegalQualifierAfter,
                   ['(',TypeEl.ElementTypeName],Params);
        end;
      end
    else if FoundEl is TPasArgument then
      begin
      TypeEl:=ResolveAliasType(TPasArgument(FoundEl).ArgType);
      if TypeEl is TPasProcedureType then
        CheckIncompatibleProc(CallName,TPasProcedureType(TypeEl),TemplParamsCnt)
      else
        begin
        InvokeProcType:=GetFuncRefInvokeProcType(TypeEl,length(Params.Params));
        if InvokeProcType<>nil then
          CheckIncompatibleProc(CallName,InvokeProcType,TemplParamsCnt)
        else
          RaiseMsg(20180228145412,nIllegalQualifierAfter,sIllegalQualifierAfter,
                   ['(',TypeEl.ElementTypeName],Params);
        end;
      end
    else
      RaiseNotYetImplemented(20161003134755,FoundEl);
    // missing raise exception
    RaiseNotYetImplemented(20180621002400,Params,'missing exception, Found='+GetObjName(FoundEl));
    end;

  if FindCallData.Count>1 then
    begin
    // multiple overloads fit
    if (FoundEl is TPasProcedure)
        and (IndexOfGenericParam(Params.Params)>=0) then
      // generic params -> ignore ambiguity
    else
      // => search again and list the candidates
      RaiseMultiFit;
    end;

  // the generic's bare name inside a specialization of it, e.g. a typecast
  if TemplParamsCnt=0 then
    FoundEl:=MapGenericSelfRef(FoundEl,NameExpr);

  // check template params
  if FoundEl is TPasProcedure then
    GenTemplates:=GetProcTemplateTypes(TPasProcedure(FoundEl))
  else if FoundEl is TPasGenericType then
    GenTemplates:=TPasGenericType(FoundEl).GenericTemplateTypes
  else
    GenTemplates:=nil;

  if TemplParamsCnt>0 then
    begin
    // check template types
    if GenTemplates=nil then
      RaiseMsg(20190919100922,nTypeParamsNotAllowedOnX,sTypeParamsNotAllowedOnX,
        [FoundEl.Name],NameExpr);
    if TemplParamsCnt<>GenTemplates.Count then
      RaiseMsg(20190919101051,nWrongNumberOfParametersForGenericX,sWrongNumberOfParametersForGenericX,
        [GetElementTypeName(FoundEl)+' '+FoundEl.Name],NameExpr);
    CheckTemplParams(GenTemplates,TemplParams);
    FoundEl:=GetSpecializedEl(NameExpr,FoundEl,TemplParams);
    if FoundEl is TPasProcedure then
      begin
      // check if params fit the explicit specialized function, e.g. Run<Word>()
      CheckCallProcCompatibility(TPasProcedure(FoundEl).ProcType,Params,true);
      end;
    end
  else if (GenTemplates<>nil) and (GenTemplates.Count>0) then
    begin
    if (FoundEl is TPasProcedure)
        and (msImplicitFunctionSpec in CurrentParser.CurrentModeswitches) then
      begin
      // GenericProc()  -> create template types by inference
      InferenceParams:=CreateInferenceTypesForCall(Params,TPasProcedure(FoundEl));
      try
        CheckTemplParams(GenTemplates,InferenceParams);
        FoundEl:=GetSpecializedEl(NameExpr,FoundEl,InferenceParams);
        // check if params fit the implicit specialized function, e.g. Run()
        CheckCallProcCompatibility(TPasProcedure(FoundEl).ProcType,Params,true);
      finally
        FreeAndNil(InferenceParams);
      end;
      end
    else if (FoundEl is TPasGenericType)
        and IsInsideGenericTypeAt(TPasGenericType(FoundEl),NameExpr) then
      // a typecast to the generic itself inside its own body (objfpc); the
      // specialization maps it to itself
    else
      // GenericType()  -> missing type params
      RaiseMsg(20190919120728,nWrongNumberOfParametersForGenericX,sWrongNumberOfParametersForGenericX,
        [GetElementTypeName(FoundEl)+' '+FoundEl.Name],NameExpr);
    end;

  if FoundEl is TPasType then
    begin
      // typecast
      TypeEl:=ResolveAliasType(TPasType(FoundEl));
      C:=TypeEl.ClassType;
      if C=TPasUnresolvedSymbolRef then
        begin
        // typecast to built-in type
        if TypeEl.CustomData is TResElDataBaseType then
          CheckTypeCast(TypeEl,Params,true); // emit warnings
        end
      else
        begin
        // typecast to user type
        CheckTypeCast(TypeEl,Params,true); // emit warnings, and errors for specializations
        end;
    end;

  // FoundEl compatible element -> create reference
  Ref:=CreateReference(FoundEl,NameExpr,rraRead);
  if IsTypeOfCast then
    Include(Ref.Flags,rrfTypeOfCast);
  if FindCallData.StartScope.ClassType=ScopeClass_WithExpr then
    Ref.WithExprScope:=TPasWithExprScope(FindCallData.StartScope);
  SetCompositionPath(Ref,FindCallData.ElScope);
  FindData:=Default(TPRFindData);
  FindData.ErrorPosEl:=NameExpr;
  FindData.StartScope:=FindCallData.StartScope;
  FindData.ElScope:=FindCallData.ElScope;
  FindData.Found:=FoundEl;
  FindData.ViaAncestorArg:=ViaAncestorArg;
  CheckFoundElement(FindData,Ref);

  // set param expression Access flags
  if FoundEl is TPasProcedure then
    begin
    // now it is known which overloaded proc to call
    if not (Access in [rraRead,rraParamToUnknownProc]) then
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.ResolveFuncParamsExprName Params=',GetObjName(Params),' NameExpr=',GetObjName(NameExpr),' Access=',Access);
      {$ENDIF}
      RaiseMsg(20170306104440,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Params);
      end;
    FinishProcParamAccess(TPasProcedure(FoundEl).ProcType,Params);
    end
  else if FoundEl is TPasType then
    begin
    TypeEl:=ResolveAliasType(TPasType(FoundEl));
    C:=TypeEl.ClassType;
    if (C=TPasClassType)
        or (C=TPasClassOfType)
        or (C=TPasRecordType)
        or (C=TPasEnumType)
        or (C=TPasSetType)
        or (C=TPasPointerType)
        or (C=TPasArrayType)
        or (C=TPasRangeType)
        or (C=TPasStringType)
        or (C=TPasGenericTemplateType) then
      begin
      // type cast (incl. codepage string types)
      FinishUntypedParams(Access);
      end
    else if (C=TPasProcedureType)
        or (C=TPasFunctionType) then
      begin
      // type cast to proc type
      AccessExpr(Params.Params[0],Access);
      end
    else if C=TPasUnresolvedSymbolRef then
      begin
      if TypeEl.CustomData is TResElDataBuiltInProc then
        begin
        // call built-in proc
        BuiltInProc:=TResElDataBuiltInProc(TypeEl.CustomData);
        if Assigned(BuiltInProc.FinishParamsExpression) then
          BuiltInProc.FinishParamsExpression(BuiltInProc,Params)
        else
          FinishUntypedParams(rraRead);
        end
      else if TypeEl.CustomData is TResElDataBaseType then
        begin
        // type cast to base type
        FinishUntypedParams(Access);
        end
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveFuncParamsExpr FoundEl=',GetObjName(FoundEl),' CustomData=',GetObjName(FoundEl.CustomData));
        {$ENDIF}
        RaiseNotYetImplemented(20170325145720,Params);
        end;
      end
    else
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.ResolveFuncParamsExpr FoundEl=',GetObjName(FoundEl),' CustomData=',GetObjName(FoundEl.CustomData));
      {$ENDIF}
      RaiseMsg(20170306121908,nIllegalQualifierAfter,sIllegalQualifierAfter,
        ['(',TypeEl.ElementTypeName],Params);
      end;
    end
  else
    begin
    // FoundEl is not a type, maybe a var
    ComputeElement(FoundEl,ResolvedEl,[rcNoImplicitProc,rcSetReferenceFlags]);
    TypeEl:=ResolvedEl.LoTypeEl;
    if TypeEl is TPasProcedureType then
      begin
      if not (Access in [rraRead,rraParamToUnknownProc]) then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveFuncParamsExprName Params=',GetObjName(Params),' NameExpr=',GetObjName(NameExpr),' Access=',Access);
        {$ENDIF}
        RaiseMsg(20190215195439,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Params);
        end;
      FinishProcParamAccess(TPasProcedureType(TypeEl),Params);
      exit;
      end;
    InvokeProcType:=GetFuncRefInvokeProcType(TypeEl,length(Params.Params));
    if InvokeProcType<>nil then
      begin
      if not (Access in [rraRead,rraParamToUnknownProc]) then
        RaiseMsg(20260319173600,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Params);
      FinishProcParamAccess(InvokeProcType,Params);
      exit;
      end;
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ResolveFuncParamsExpr FoundEl=',GetObjName(FoundEl),' CustomData=',GetObjName(FoundEl.CustomData),' Resolvedel=',GetResolverResultDbg(ResolvedEl));
    {$ENDIF}
    RaiseMsg(20170306104301,nIllegalQualifierAfter,sIllegalQualifierAfter,
      ['(',TypeEl.ElementTypeName],Params);
    end;
end;

procedure TPasResolver.ResolveArrayParamsExpr(Params: TParamsExpr;
  Access: TResolvedRefAccess);
var
  ResolvedEl: TPasResolverResult;
  Value: TPasExpr;
  SubParams: TParamsExpr;
begin
  Value:=Params.Value;
  if Value=nil then
    RaiseInternalError(20180423093120,GetObjName(Params));

  if IsNameExpr(Value) then
    begin
    // e.g. Name[]
    ResolveArrayParamsExprName(Value,Params,Access);
    exit;
    end
  else if Value.ClassType=TParamsExpr then
    begin
    SubParams:=TParamsExpr(Value);
    // e.g. Name()[] or Name[][] or [][]
    ResolveExpr(SubParams,rraRead);
    ComputeElement(SubParams,ResolvedEl,[rcNoImplicitProc,rcSetReferenceFlags]);
    if Value.CustomData=nil then
      CreateReference(ResolvedEl.LoTypeEl,Value,Access);
    ResolvedEl.IdentEl:=nil;
    end
  else if Value.InheritsFrom(TUnaryExpr) then
    begin
    ResolveExpr(TUnaryExpr(Value).Operand,Access);
    ComputeElement(Value,ResolvedEl,[rcSetReferenceFlags]);
    end
  else if Value is TBinaryExpr then
    begin
    // Note: a.b[] is the same as (a.b)[]
    // Note: a.b[].c  is stored as
    //             TBinaryExpr eopSubIdent
    //           /                         \
    //         left = TParamsExpr           right = TPrimitiveExpr 'c'
    //         Value = TBinaryExpr
    //         /                 \
    // left = TPrimitiveExpr 'a'  right = TPrimitiveExpr 'b'
    while (Value is TBinaryExpr) and (TBinaryExpr(Value).OpCode=eopSubIdent) do
      Value:=TBinaryExpr(Value).Right;
    if IsNameExpr(Value) then
      begin
      ResolveBinaryExpr(TBinaryExpr(Params.Value),Access);
      if not (Value.CustomData is TResolvedReference) then
        begin
        // Indexing a member of a still-unbound template type is checked at
        // specialization too. fcl-stl's ghashmap writes (FData[Fh]).Mutable[Fp].
        if IsDeferredTemplMember(Value) then
          exit;
        RaiseNotYetImplemented(20190115144534,Params);
        end;
      // already resolved via ResolveNameExpr, which calls ResolveArrayParamsExprName
      exit;
      end
    else
      begin
      // For example (a+b)[]  or (a as b)[]
      Value:=Params.Value;
      ResolveBinaryExpr(TBinaryExpr(Value),rraRead);
      ComputeElement(Value,ResolvedEl,[rcSetReferenceFlags]);
      end;
    end
  else if (Value is TPrimitiveExpr)
      and (TPrimitiveExpr(Value).Kind in [pekString,pekStringMultiLine]) then
    begin
    // e.g. 'Hello World'[5] — index a string-literal constant to get a character.
    ResolveExpr(Value,rraRead);
    ComputeElement(Value,ResolvedEl,[rcSetReferenceFlags]);
    end
  else
    RaiseNotYetImplemented(20160927212610,Value);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveArrayParamsExpr Value=',GetObjName(Value),' ',GetResolverResultDbg(ResolvedEl));
  {$ENDIF}
  ResolveArrayParamsArgs(Params,ResolvedEl,Access);
end;

procedure TPasResolver.ResolveArrayParamsExprName(NameExpr: TPasExpr;
  Params: TParamsExpr; Access: TResolvedRefAccess);
// e.g. a.NameExpr[]
var
  ArrayName: String;
  FindData: TPRFindData;
  Ref: TResolvedReference;
  DeclEl: TPasElement;
  Proc, ImplProc: TPasProcedure;
  ProcScope: TPasProcedureScope;
  ResolvedEl: TPasResolverResult;
  EnclFunc: TPasFunction;

  function FindEnclosingFuncNamed(El: TPasElement; const aFuncName: String;
    out Found: TPasFunction): boolean;
  // The function being compiled around El, when it has aFuncName. Used to
  // recover from FindFirstEl picking the wrong same-named overload.
  var
    P: TPasElement;
  begin
    Found:=nil;
    P:=El;
    while P<>nil do
      begin
      if (P is TPasFunction) and SameText(TPasFunction(P).Name,aFuncName) then
        begin
        Found:=TPasFunction(P);
        exit(true);
        end;
      P:=P.Parent;
      end;
    Result:=false;
  end;

begin
  if (NameExpr.ClassType=TPrimitiveExpr)
      and (TPrimitiveExpr(NameExpr).Kind=pekIdent) then
    // e.g. Name[]
    ArrayName:=TPrimitiveExpr(NameExpr).Value
  else if NameExpr.ClassType=TInlineSpecializeExpr then
    RaiseMsg(20190912190518,nIllegalQualifierAfter,sIllegalQualifierAfter,
      ['[','inline specialize'],Params)
  else
    RaiseNotYetImplemented(20190131154557,NameExpr);

  // NoProcsWithArgs=false: an indexed function-name that is a self-reference to the
  // enclosing function (funcname[i] -> Result[i]) must be resolved to ResultEl below,
  // not rejected as "call with wrong number of parameters". A genuine non-self proc
  // needing args is re-checked after the redirect (see RaiseMsg below).
  DeclEl:=FindElementWithoutParams(ArrayName,FindData,NameExpr,false,true);
  Ref:=CreateReference(DeclEl,NameExpr,Access,@FindData);
  CheckFoundElement(FindData,Ref);
  if DeclEl is TPasProcedure then
    begin
    Proc:=TPasProcedure(DeclEl);
    ProcScope:=Proc.CustomData as TPasProcedureScope;
    ImplProc:=ProcScope.ImplProc;
    if ImplProc=nil then
      ImplProc:=Proc;
    if (Proc.ProcType is TPasFunctionType) and Params.HasParent(ImplProc) then
      // "FuncA[i]" (read, write or by-reference) within FuncA's own body -> the
      // function name denotes its Result variable, so index Result. FPC allows this
      // for any access (tspace: `space[0]:=` and `FillChar(Space[1],..)`).
      Ref.Declaration:=TPasFunctionType(Proc.ProcType).ResultEl
    else if (Proc.ProcType is TPasFunctionType)
        and FindEnclosingFuncNamed(Params,Proc.Name,EnclFunc) then
      // Overloaded case: FindFirstEl may have picked a same-named overload whose
      // body does NOT enclose Params, so HasParent fails. Walk out to the function
      // actually being compiled and index ITS Result — rtl/linux/termio.pp writes
      // `ttyname[0]:=...` inside TTYname(Handle) while TTYname(var Text) also exists.
      Ref.Declaration:=TPasFunctionType(EnclFunc.ProcType).ResultEl
    else if ProcNeedsParams(Proc.ProcType) then
      // A non-self-reference procedure that needs arguments cannot be indexed.
      RaiseMsg(20260715120000,nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,
        [GetProcTypeDescription(Proc.ProcType)],NameExpr);
    end;

  ComputeElement(NameExpr,ResolvedEl,[rcSetReferenceFlags]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveArrayParamsExprName NameExp=',GetObjName(NameExpr),' ',GetResolverResultDbg(ResolvedEl));
  {$ENDIF}
  ResolveArrayParamsArgs(Params,ResolvedEl,Access);
end;

procedure TPasResolver.ResolveArrayParamsArgs(Params: TParamsExpr;
  const ResolvedValue: TPasResolverResult; Access: TResolvedRefAccess);

  procedure ReadAccessParamValue;
  var
    Left: TPasExpr;
    Ref: TResolvedReference;
  begin
    if Access=rraAssign then
      begin
      // ArrayStringPointer[]:=
      // -> writing the element needs reading the value
      Left:=Params.Value;
      if (Left is TBinaryExpr) and (TBinaryExpr(Left).OpCode=eopSubIdent) then
        Left:=TBinaryExpr(Left).Right;
      if Left.CustomData is TResolvedReference then
        begin
        Ref:=TResolvedReference(Left.CustomData);
        if Ref.Access=rraAssign then
          Ref.Access:=rraReadAndAssign;
        end;
      end;
  end;

  function CheckStringOrPointerIndex(IsStringIndex: boolean): boolean;
  var
    ArgExp: TPasExpr;
    ResolvedArg: TPasResolverResult;
  begin
    ReadAccessParamValue;
    if not IsStringIndex then
      begin
      // pointer
      if not (ElHasBoolSwitch(Params,bsPointerMath)
          or IsPointerMathType(ResolvedValue.LoTypeEl)) then
        exit(false);
      end;
    Result:=true;
    if not (rrfReadable in ResolvedValue.Flags) then
      RaiseXExpectedButYFound(20170216152548,'index',GetElementTypeName(ResolvedValue.LoTypeEl),Params);
    // check single argument
    if length(Params.Params)<1 then
      RaiseMsg(20170216152204,nMissingParameterX,
        sMissingParameterX,[BoolToStr(IsStringIndex,'character index','index')],Params)
    else if length(Params.Params)>1 then
      RaiseMsg(20170216152551,nIllegalQualifier,sIllegalQualifier,[','],Params.Params[1]);
    // check argument is integer
    ArgExp:=Params.Params[0];
    ComputeElement(ArgExp,ResolvedArg,[rcSetReferenceFlags]);
    if not (ResolvedArg.BaseType in btAllInteger) then
      RaiseMsg(20170216152209,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        [BaseTypeNames[ResolvedArg.BaseType],'integer'],ArgExp);
    if not (rrfReadable in ResolvedArg.Flags) then
      RaiseMsg(20170216152211,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        ['type','value'],ArgExp);
    AccessExpr(ArgExp,rraRead);
  end;

var
  PropEl: TPasProperty;
  i: Integer;
  TypeEl: TPasType;
  C: TClass;
  DerefValue: TPasResolverResult;
begin
  // Delphi reads `P[i]` on a pointer to an ARRAY as `P^[i]`. A bare name goes
  // through ResolveArrayParamsExprName, which lands here directly, so the
  // dereference belongs here rather than at one call site.
  DerefValue:=ResolvedValue;
  if DerefPointerToArray(DerefValue,Params) then
    begin
    ResolveArrayParamsArgs(Params,DerefValue,Access);
    exit;
    end;
  if IsGenericTemplType(ResolvedValue) then
    begin
    // value[i] where value has a generic template type: the element/index
    // semantics are unknown until specialization. Resolve the index args and
    // defer the rest (the specialized copy resolves it normally).
    for i:=0 to length(Params.Params)-1 do
      AccessExpr(Params.Params[i],rraRead);
    exit;
    end;
  if ResolvedValue.BaseType in btAllStrings then
    begin
    // string -> check that ResolvedValue is not merely a type, but has a value
    if CheckStringOrPointerIndex(true) then
      exit;
    end
  else if (ResolvedValue.IdentEl is TPasProperty)
      and (GetPasPropertyArgs(TPasProperty(ResolvedValue.IdentEl)).Count>0) then
    begin
    PropEl:=TPasProperty(ResolvedValue.IdentEl);
    CheckCallPropertyCompatibility(PropEl,Params,true);
    FinishPropertyParamAccess(Params,PropEl);
    exit;
    end
  else if ResolvedValue.BaseType=btPointer then
    begin
    if CheckStringOrPointerIndex(false) then
      exit;
    end
  else if ResolvedValue.BaseType=btContext then
    begin
    TypeEl:=ResolvedValue.LoTypeEl;
    C:=TypeEl.ClassType;
    if (C=TPasClassType)
        or (C=TPasRecordType)
        or (C=TPasClassOfType) then
      begin
      if ResolveBracketOperatorClassOrRec(Params,ResolvedValue,Access) then
        exit;
      end
    else if C=TPasArrayType then
      begin
      if ResolvedValue.IdentEl is TPasType then
        RaiseMsg(20170216152215,nIllegalQualifierAfter,sIllegalQualifierAfter,
          ['[',ResolvedValue.IdentEl.ElementTypeName],Params);
      ReadAccessParamValue;
      CheckCallArrayCompatibility(TPasArrayType(TypeEl),Params,true,true);
      for i:=0 to length(Params.Params)-1 do
        AccessExpr(Params.Params[i],rraRead);
      exit;
      end
    else if C=TPasPointerType then
      begin
      if CheckStringOrPointerIndex(false) then exit;
      end;
    end
  {$IFNDEF PAS2JS}
  else if ResolvedValue.BaseType=btVariant then
    begin
    // Variant[i] / Variant[i,j,..] indexes a variant array; the element is itself
    // a Variant fetched/stored via VarArrayGet/VarArrayPut at runtime. Accept any
    // number of index args (multi-dimensional variant arrays).
    ReadAccessParamValue;
    for i:=0 to length(Params.Params)-1 do
      AccessExpr(Params.Params[i],rraRead);
    exit;
    end
  {$ENDIF}
    ;
  RaiseMsg(20170216152217,nIllegalQualifierAfter,sIllegalQualifierAfter,
    ['[',GetResolverResultDescription(ResolvedValue,true)],Params);
end;

function TPasResolver.ResolveBracketOperatorClassOrRec(Params: TParamsExpr;
  const ResolvedValue: TPasResolverResult; Access: TResolvedRefAccess): boolean;
var
  PropEl: TPasProperty;
  Value: TPasExpr;
  Group: TPasGroupScope;
  i: Integer;
  Scope: TPasIdentifierScope;
  HiType, LoType: TPasType;
  IsClassOf: Boolean;
begin
  HiType:=ResolvedValue.HiTypeEl;
  LoType:=ResolvedValue.LoTypeEl;
  IsClassOf:=LoType.ClassType=TPasClassOfType;
  if IsClassOf then
    begin
    HiType:=TPasClassOfType(LoType).DestType;
    LoType:=ResolveAliasType(LoType);
    end;

  Group:=CreateGroupScope(HiType);
  PropEl:=nil;
  for i:=0 to Group.Count-1 do
    begin
    Scope:=Group.Scopes[i];
    if Scope is TPasClassOrRecordScope then
      begin
      PropEl:=TPasClassOrRecordScope(Scope).DefaultProperty;
      if PropEl<>nil then break;
      end;
    end;
  Group.Free;
  if PropEl=nil then exit(false);

  // class/record/interface has default property
  if (IsClassOf or (ResolvedValue.IdentEl is TPasType)) and (not PropEl.IsClass) then
    RaiseMsg(20170216152213,nIllegalQualifierAfter,sIllegalQualifierAfter,
      ['[',GetResolverResultDescription(ResolvedValue,true)],Params);
  Value:=Params.Value;
  if Value.CustomData is TResolvedReference then
    SetResolvedRefAccess(Value,TResolvedReference(Value.CustomData),rraRead);
  CreateReference(PropEl,Params,Access);
  CheckCallPropertyCompatibility(PropEl,Params,true);
  FinishPropertyParamAccess(Params,PropEl);
  Result:=true;
end;

procedure TPasResolver.ResolveSetParamsExpr(Params: TParamsExpr);
// e.g. resolving '[1,2..3]'
var
  i: Integer;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveSetParamsExpr ',GetTreeDbg(Params));
  {$ENDIF}
  if Params.Value<>nil then
    RaiseNotYetImplemented(20160930135910,Params);
  for i:=0 to length(Params.Params)-1 do
    begin
    Param:=Params.Params[i];
    ComputeElement(Param,ParamResolved,[rcNoImplicitProcType,rcSetReferenceFlags]);
    end;
end;

procedure TPasResolver.ResolveArrayValues(El: TArrayValues);
var
  i: Integer;
begin
  for i:=0 to length(El.Values)-1 do
    ResolveExpr(El.Values[i],rraRead);
end;

procedure TPasResolver.ResolveRecordValues(El: TRecordValues);

  function GetMember(RecType: TPasMembersType; const aName: string): TPasElement;
  // A TP object inherits its ancestor's fields, and ppcx64 lets a typed
  // constant name them, so the search walks the ancestor chain too.
  var
    i: Integer;
    Rec: TPasRecordType;
  begin
    while RecType<>nil do
      begin
      for i:=0 to RecType.Members.Count-1 do
        begin
        Result:=TPasElement(RecType.Members[i]);
        if SameText(Result.Name,aName) then
          exit;
        end;
      if RecType.ClassType=TPasRecordType then
        begin
        Rec:=TPasRecordType(RecType);
        if Rec.VariantEl is TPasVariable then
          begin
          Result:=TPasVariable(Rec.VariantEl);
          if SameText(Result.Name,aName) then
            exit;
          end;
        if Rec.Variants<>nil then
          for i:=0 to Rec.Variants.Count-1 do
            begin
            Result:=GetMember(TPasVariant(Rec.Variants[i]).Members,aName);
            if Result<>nil then
              exit;
            end;
        break;
        end;
      RecType:=TPasMembersType(ResolveAliasType(TPasClassType(RecType).AncestorType));
      if (RecType<>nil) and (RecType.ClassType<>TPasClassType) then
        break;
      end;
    Result:=nil;
  end;

var
  i, j: Integer;
  Member: TPasElement;
  RecType: TPasMembersType;
  Field: PRecordValuesItem;
  s: String;
  ResolvedEl: TPasResolverResult;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveRecordValues ',El.Fields[0].Name,' ',GetObjName(El.Parent),' ',GetObjName(El.Parent.Parent));
  {$ENDIF}
  ComputeElement(El,ResolvedEl,[]);
  RecType:=nil;
  if ResolvedEl.BaseType=btContext then
    RecType:=GetRecordValuesMembersType(ResolvedEl.LoTypeEl);
  if RecType=nil then
    RaiseIncompatibleTypeDesc(20180429104135,nIncompatibleTypesGotExpected,
      [],'record value',GetTypeDescription(ResolvedEl),El);
  // A TP object with a VMT cannot be a typed constant: the initializer says
  // nothing about the VMT pointer the layout starts with. ppcx64 rejects it as
  // "Constants of objects containing a VMT are not allowed".
  if ObjectHasVMT(RecType) then
    RaiseMsg(20260826170000,nIllegalExpression,sIllegalExpression,[],El);
  //writeln('TPasResolver.ResolveRecordValues ',GetObjName(El.Parent),' ',GetObjName(RecType));
  for i:=0 to length(El.Fields)-1 do
    begin
    Field:=@El.Fields[i];
    // check member exists
    Member:=GetMember(RecType,Field^.Name);
    if Member=nil then
      RaiseIdentifierNotFound(20180429104703,Field^.Name,Field^.NameExp);
    if Member.ClassType<>TPasVariable then
      RaiseMsg(20180429121933,nIdentifierXIsNotAnInstanceField,sIdentifierXIsNotAnInstanceField,
        [],Field^.ValueExp);
    if TPasVariable(Member).VarModifiers*[vmClass,vmStatic]<>[] then
      RaiseMsg(20190105221450,nIdentifierXIsNotAnInstanceField,sIdentifierXIsNotAnInstanceField,
        ['record assignment'],Field^.ValueExp);
    CreateReference(Member,Field^.NameExp,rraAssign);
    // check duplicates
    for j:=0 to i-1 do
      if SameText(Field^.Name,El.Fields[j].Name) then
        RaiseMsg(20180429104942,nDuplicateIdentifier,sDuplicateIdentifier,
          [Field^.Name,GetElementSourcePosStr(El.Fields[j].NameExp)],Field^.NameExp);
    // resolve expression
    ResolveExpr(El.Fields[i].ValueExp,rraRead);
    // check compatible
    CheckAssignCompatibility(Member,Field^.ValueExp);
    end;
  // hint for missing fields
  s:='';
  for i:=0 to RecType.Members.Count-1 do
    begin
    Member:=TPasElement(RecType.Members[i]);
    if Member.ClassType<>TPasVariable then continue;
    if TPasVariable(Member).VarModifiers*[vmClass,vmStatic]<>[] then
      continue;
    j:=length(El.Fields)-1;
    while (j>=0) and not SameText(Member.Name,El.Fields[j].Name) do
      dec(j);
    //writeln('TPasResolver.ResolveRecordValues ',GetObjName(Member),' ',j);
    if j<0 then
      begin
      if s<>'' then s:=s+', ';
      if length(s)>30 then
        begin
        s:=s+'...';
        break;
        end;
      s:=s+Member.Name;
      end;
    end;
  // ToDo: hint for missing variants
  if s<>'' then
    LogMsg(20180429121127,mtHint,nMissingFieldsX,sMissingFieldsX,[s],El);
end;

procedure TPasResolver.ResolveInlineSpecializeExpr(El: TInlineSpecializeExpr;
  Access: TResolvedRefAccess);
begin
  // params are TPasTypes and already resolved
  if El.Params.Count=0 then
    RaiseMsg(20190916155014,nMissingParameterX,sMissingParameterX,['type'],El);

  // resolve name
  // Note: ResolveNameExpr considers the params
  ResolveExpr(El.NameExpr,Access);
end;

function TPasResolver.ResolveAccessor(Expr: TPasExpr): TPasElement;

  function SubResolvePrimitive(Prim: TPrimitiveExpr; AllowNotFound: boolean = false): TPasElement;
  var
    FindData: TPRFindData;
    Ref: TResolvedReference;
    Scope: TPasScope;
    Abort: boolean;
  begin
    if Prim.Kind<>pekIdent then
      RaiseXExpectedButYFound(20170216151746,'class',Prim.Value,Prim);
    // search in class and ancestors, not in unit interface
    Scope:=TopScope;
    FindData:=Default(TPRFindData);
    FindData.ErrorPosEl:=Expr;
    Abort:=false;
    Scope.IterateElements(Prim.Value,Scope,@OnFindFirst,@FindData,Abort);
    Result:=FindData.Found;
    if (Result=nil) and AllowNotFound then
      exit;
    if Result=nil then
      RaiseIdentifierNotFound(20170216151749,Prim.Value,Prim);
    Ref:=CreateReference(Result,Prim,rraRead);
    CheckFoundElementVisibility(FindData,Ref);
  end;

var
  Prim: TPrimitiveExpr;
  DeclEl: TPasElement;
  AccMembersType: TPasType;
  LeftVar: TPasVariable;
  i: Integer;
  IdxValue: TResEvalValue;
begin
  LeftVar:=nil;
  if Expr.ClassType=TBinaryExpr then
    begin
    DeclEl:=nil;
    if (TBinaryExpr(Expr).Left is TPrimitiveExpr)
        or ((TBinaryExpr(Expr).Left.ClassType=TBinaryExpr)
          and (TBinaryExpr(TBinaryExpr(Expr).Left).OpCode=eopSubIdent)) then
      begin
      if TBinaryExpr(Expr).Left is TPrimitiveExpr then
        begin
        Prim:=TPrimitiveExpr(TBinaryExpr(Expr).Left);
        DeclEl:=SubResolvePrimitive(Prim);
        end
      else
        // a path of fields, e.g. `read FData.Mid.Inner`
        DeclEl:=ResolveAccessor(TBinaryExpr(Expr).Left);
      // The left side may be a FIELD of a record/class type, not only a type
      // name: rtl-generics writes `property Key: TKey read Data.Key`, where Data
      // is a field whose type is a record.
      if (DeclEl is TPasVariable) and (TPasVariable(DeclEl).VarType<>nil) then
        begin
        LeftVar:=TPasVariable(DeclEl);
        AccMembersType:=ResolveAliasType(TPasVariable(DeclEl).VarType);
        if AccMembersType is TPasMembersType then
          DeclEl:=AccMembersType;
        end;
      if not (DeclEl is TPasMembersType) then
        RaiseXExpectedButYFound(20170216151752,'class',GetElementTypeName(DeclEl),
          TBinaryExpr(Expr).Left);
      end
    else
      RaiseMsg(20170216151754,nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TBinaryExpr(Expr).OpCode]],Expr);
    if TBinaryExpr(Expr).OpCode<>eopSubIdent then
      RaiseMsg(20170216151757,nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TBinaryExpr(Expr).OpCode]],Expr);
    if DeclEl.ClassType=TPasClassType  then
      PushClassDotScope(TPasClassType(DeclEl))
    else if DeclEl.ClassType=TPasRecordType  then
      PushRecordDotScope(TPasRecordType(DeclEl))
    else
      RaiseMsg(20190123145559,nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TBinaryExpr(Expr).OpCode]],Expr);
    Expr:=TBinaryExpr(Expr).Right;
    if (LeftVar<>nil) and (Expr is TPrimitiveExpr) and FPartialSpecsAsGeneric
        and ParamIsUndecided(LeftVar.VarType) then
      begin
      // The left field's type still depends on a type parameter, so its members
      // are only known in a specialization, where the accessor is checked again.
      Result:=SubResolvePrimitive(TPrimitiveExpr(Expr),true);
      if Result=nil then
        Result:=LeftVar;
      end
    else
      Result:=ResolveAccessor(Expr);
    PopScope;
    end
  else if Expr.ClassType=TPrimitiveExpr then
    begin
    Prim:=TPrimitiveExpr(Expr);
    Result:=SubResolvePrimitive(Prim);
    end
  else if (Expr.ClassType=TParamsExpr) and (TParamsExpr(Expr).Kind=pekArrayParams) then
    begin
    // one element of a static array field, at a constant index, as fpc and
    // Delphi allow: the LCL's `property OKButton: TPanelBitBtn read FButtons[pbOK]`
    Result:=ResolveAccessor(TParamsExpr(Expr).Value);
    if not (Result is TPasVariable) or (Result is TPasProperty) then
      RaiseXExpectedButYFound(20260927120000,'field',GetElementTypeName(Result),Expr);
    for i:=0 to length(TParamsExpr(Expr).Params)-1 do
      begin
      ResolveExpr(TParamsExpr(Expr).Params[i],rraRead);
      // the index must be a constant; Eval raises otherwise
      IdxValue:=Eval(TParamsExpr(Expr).Params[i],[refConst]);
      ReleaseEvalValue(IdxValue);
      end;
    if AccessorElementType(Expr)=nil then
      RaiseXExpectedButYFound(20260927120001,'static array',
        GetElementTypeName(TPasVariable(Result).VarType),Expr);
    end
  else
    RaiseNotYetImplemented(20160922163436,Expr);
end;

function TPasResolver.AccessorElementType(Expr: TPasExpr): TPasType;
// The type a property accessor stands for: the field's own type, or for
// `Field[i, ...]` the element type of that static array field; nil if the
// indexes do not fit a static array.
var
  Decl: TPasElement;
  ArrType: TPasType;
  Left, i: Integer;
begin
  Result:=nil;
  if not ((Expr.ClassType=TParamsExpr) and (TParamsExpr(Expr).Kind=pekArrayParams)) then
    begin
    Decl:=GetAccessorDeclaration(Expr);
    if Decl is TPasVariable then
      Result:=TPasVariable(Decl).VarType;
    exit;
    end;
  Decl:=GetAccessorDeclaration(TParamsExpr(Expr).Value);
  if not (Decl is TPasVariable) then exit;
  ArrType:=ResolveAliasType(TPasVariable(Decl).VarType);
  Left:=length(TParamsExpr(Expr).Params);
  while Left>0 do
    begin
    if not (ArrType is TPasArrayType) or IsDynArray(ArrType)
        or (length(TPasArrayType(ArrType).Ranges)=0) then
      exit(nil);
    for i:=1 to length(TPasArrayType(ArrType).Ranges) do
      begin
      if Left=0 then
        exit(nil); // fewer indexes than dimensions: not one element
      dec(Left);
      end;
    Result:=TPasArrayType(ArrType).ElType;
    ArrType:=ResolveAliasType(Result);
    end;
end;

procedure TPasResolver.SetResolvedRefAccess(Expr: TPasExpr;
  Ref: TResolvedReference; Access: TResolvedRefAccess);
begin
  if (Ref.Access=Access) then exit;
  if Access in [rraNone,rraParamToUnknownProc] then
    exit;
  if Expr=nil then ;

  case Ref.Access of
    rraNone,rraParamToUnknownProc:
      Ref.Access:=Access;
    rraRead:
      if Access in [rraAssign,rraReadAndAssign,rraVarParam,rraOutParam] then
        Ref.Access:=rraReadAndAssign
      else
        exit;
    rraAssign,rraOutParam:
      if Access in [rraRead,rraReadAndAssign,rraVarParam] then
        Ref.Access:=rraReadAndAssign
      else
        exit;
    rraReadAndAssign: exit;
    rraVarParam: exit;
  else
    RaiseInternalError(20170403163727);
  end;
end;

procedure TPasResolver.AccessExpr(Expr: TPasExpr;
  Access: TResolvedRefAccess);
// called after a call target was found, called for each element
// to change the rraParamToUnknownProc value to Access
var
  Ref: TResolvedReference;
  Bin: TBinaryExpr;
  Params: TParamsExpr;
  ValueResolved: TPasResolverResult;
  C: TClass;
begin
  if (Expr.CustomData is TResolvedReference) then
    begin
    Ref:=TResolvedReference(Expr.CustomData);
    SetResolvedRefAccess(Expr,Ref,Access);
    end;

  C:=Expr.ClassType;
  if C=TBinaryExpr then
    begin
    Bin:=TBinaryExpr(Expr);
    if Bin.OpCode in [eopSubIdent,eopNone] then
      AccessExpr(Bin.Right,Access);
    end
  else if C=TParamsExpr then
    begin
    Params:=TParamsExpr(Expr);
    case Params.Kind of
    pekFuncParams:
      if IsTypeCast(Params) then
        FinishCallArgAccess(Params.Params[0],Access)
      else
        AccessExpr(Params.Value,Access);
    pekArrayParams:
      begin
      ComputeElement(Params.Value,ValueResolved,[]);
      if IsDynArray(ValueResolved.LoTypeEl,false)
          or (ValueResolved.BaseType=btPointer) then
        // when accessing an element of a dynamic array the array is read
        AccessExpr(Params.Value,rraRead)
      else
        AccessExpr(Params.Value,Access);
      // Note: an element of an open or static array or a string is connected to the variable
      end;
    pekSet:
      if Access<>rraRead then
        RaiseMsg(20170306112306,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Expr);
    else
      RaiseNotYetImplemented(20170403173831,Params);
    end;
    end
  else if (C=TPrimitiveExpr) and (TPrimitiveExpr(Expr).Kind=pekIdent) then
    // ok
  else if (Access in [rraRead,rraParamToUnknownProc])
      and ((C=TPrimitiveExpr)
        or (C=TNilExpr)
        or (C=TBoolConstExpr)
        or (C=TInheritedExpr)
        or (C=TProcedureExpr)
        or (C=TIfExpr)
        or (C=TCaseExpr)
        or (C=TTryExceptExpr))
        or (C=TInlineSpecializeExpr) then
    // ok
  else if C=TUnaryExpr then
    begin
    if (TUnaryExpr(Expr).OpCode=eopDeref) and (TUnaryExpr(Expr).Operand is TNilExpr) then
      // `nil^` as a var argument: there is no variable to mark
    else
      AccessExpr(TUnaryExpr(Expr).Operand,Access);
    end
  else
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.AccessExpr Expr=',GetObjName(Expr),' Access=',Access,' Declaration="',Expr.GetDeclaration(false),'"');
    {$ENDIF}
    RaiseNotYetImplemented(20170306102158,Expr);
    end;
end;

function TPasResolver.MarkArrayExpr(Expr: TParamsExpr; ArrayType: TPasArrayType
  ): boolean;
var
  Ref: TResolvedReference;
begin
  if Expr.CustomData=nil then
    begin
    // mark set expression as array
    CreateReference(ArrayType,Expr,rraRead);
    Result:=true;
    end
  else if Expr.CustomData is TResolvedReference then
    begin
    // already set
    Result:=false;
    // check consistency
    Ref:=TResolvedReference(Expr.CustomData);
    if not (Ref.Declaration is TPasArrayType) then
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.MarkArrayExpr Expr=',GetObjName(Expr),' Ref.Declaration=',GetObjName(Ref.Declaration),' ',Ref.Declaration.ParentPath);
      {$ENDIF}
      RaiseNotYetImplemented(20180618102230,Expr,GetObjName(Ref.Declaration));
      end;
    end
  else
    // already set with something else
    RaiseNotYetImplemented(20180618102408,Expr,GetObjName(Expr.CustomData));
end;

procedure TPasResolver.MarkArrayExprRecursive(Expr: TPasExpr;
  ArrType: TPasArrayType);

  procedure Traverse(CurExpr: TPasExpr; ArrayType: TPasArrayType; RgIndex: integer);
  var
    Params: TPasExprArray;
    i: Integer;
    ResolvedElType: TPasResolverResult;
    ParamsExpr: TParamsExpr;
    BuiltInProc: TResElDataBuiltInProc;
    Ref: TResolvedReference;
  begin
    if IsArrayOperatorAdd(CurExpr) then
      begin
      Traverse(TBinaryExpr(CurExpr).Left,ArrayType,RgIndex);
      Traverse(TBinaryExpr(CurExpr).Right,ArrayType,RgIndex);
      end
    else if CurExpr.ClassType=TParamsExpr then
      begin
      ParamsExpr:=TParamsExpr(CurExpr);
      Params:=ParamsExpr.Params;
      if CurExpr.Kind=pekSet then
        begin
        MarkArrayExpr(ParamsExpr,ArrayType);

        // traverse into nested expressions, e.g. [ A, B ]
        if length(Params)=0 then exit;
        inc(RgIndex);
        if RgIndex>length(ArrayType.Ranges) then
          begin
          if ArrayType.ElType=nil then
            exit; // elements are not arrays
          ComputeElement(ArrayType.ElType,ResolvedElType,[rcType]);
          if (ResolvedElType.BaseType=btContext)
              and (ResolvedElType.LoTypeEl is TPasArrayType) then
            begin
            ArrayType:=TPasArrayType(ResolvedElType.LoTypeEl);
            RgIndex:=0;
            end
          else
            exit; // elements are not arrays
          end;
        for i:=0 to length(Params)-1 do
          Traverse(Params[i],ArrayType,RgIndex);
        end
      else if CurExpr.Kind=pekFuncParams then
        begin
        if TParamsExpr(CurExpr).Value.CustomData is TResolvedReference then
          begin
          Ref:=TResolvedReference(TParamsExpr(CurExpr).Value.CustomData);
          if (Ref.Declaration is TPasUnresolvedSymbolRef)
              and (Ref.Declaration.CustomData is TResElDataBuiltInProc) then
            begin
            BuiltInProc:=TResElDataBuiltInProc(Ref.Declaration.CustomData);
            if BuiltInProc.BuiltIn=bfConcatArray then
              begin
              // concat(array1,array2,...)
              for i:=0 to length(Params)-1 do
                Traverse(Params[i],ArrayType,RgIndex);
              end
            else if BuiltInProc.BuiltIn=bfCopyArray then
              // copy(array,...)
              Traverse(Params[0],ArrayType,RgIndex);
            end;
          end;
        end;
      end;
  end;

begin
  Traverse(Expr,ArrType,0);
end;

procedure TPasResolver.DeanonymizeType(El: TPasType);

  procedure InsertInFront(NewParent, Child: TPasElement; List: TFPList);
  var
    i: Integer;
    Prev: TPasElement;
  begin
    // e.g. m,n:array of longint; -> insert n$a in front of m
    i:=List.Count-1;
    while (i>=0) and (List[i]<>Pointer(Child)) do
      dec(i);
    // skip preview variables with shared type
    if Child is TPasVariable then
      begin
      while (i>0) do
        begin
        Prev:=TPasElement(List[i-1]);
        if (Prev.ClassType=Child.ClassType)
            and (TPasVariable(Prev).VarType=TPasVariable(Child).VarType) then
          dec(i) // e.g. m,n: array of longint
        else
          break;
        end;
      end;
    // skip attributes
    while (i>0) and (TPasElement(List[i-1]).ClassType=TPasAttributes) do
      dec(i);
    if i<0 then
      List.Add(El)
    else
      List.Insert(i,El);
    {$IFDEF VerbosePasResolver}
    if El.Parent<>NewParent then writeln('TPasResolver.DeanonymizeType.InsertInFront OldParent=',GetObjName(El.Parent),' -> ',GetObjPath(NewParent));
    {$ENDIF}
    El.Parent:=NewParent;
  end;

var
  Decl: TPasDeclarations;
  Child, p: TPasElement;
  MembersType: TPasMembersType;
  CurName: String;
begin
  if (AnonymousElTypePostfix='') then
    exit;
  if (El.Name<>'') then
    RaiseNotYetImplemented(20220320121923,El);
  if El.ClassType=TPasSpecializeType then exit;

  CurName:='';
  Child:=El;
  p:=El.Parent;
  repeat
    if p=nil then
      RaiseNotYetImplemented(20220320165553,El);
    if (p is TPasDeclarations) or (p is TPasMembersType) then
      begin
      if CurName='' then
        RaiseMsg(20220320122946,nCannotNestAnonymousX,sCannotNestAnonymousX,[GetElementTypeName(El)],El);
      El.Name:=CurName+AnonymousElTypePostfix;
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.DeanonymizeType named anonymous type "',GetObjPath(El),'"');
      {$ENDIF}
      if p is TPasDeclarations then
        begin
        Decl:=TPasDeclarations(p);
        InsertInFront(Decl,Child,Decl.Declarations);
        Decl.Types.Add(El);
        end
      else if p is TPasMembersType then
        begin
        MembersType:=TPasMembersType(p);
        InsertInFront(MembersType,Child,MembersType.Members);
        end;
      break;
      end
    else if p.Name<>'' then
      begin
      if CurName<>'' then
        RaiseMsg(20220320132732,nCannotNestAnonymousX,sCannotNestAnonymousX,[GetElementTypeName(El)],El)
      else
        CurName:=p.Name;
      end;
    Child:=p;
    p:=p.Parent;
  until false;
end;

procedure TPasResolver.CheckPointerCycle(El: TPasPointerType);
var
  C: TClass;
  CurEl, Dest: TPasType;
begin
  CurEl:=El;
  while CurEl<>nil do
    begin
    C:=CurEl.ClassType;
    if C=TPasPointerType then
      Dest:=TPasPointerType(CurEl).DestType
    else if C.InheritsFrom(TPasAliasType) then
      Dest:=TPasAliasType(CurEl).DestType
    else
      exit;
    if Dest=El then
      RaiseMsg(20180422165758,nTypeCycleFound,sTypeCycleFound,[],El);
    CurEl:=Dest;
    end;
end;

procedure TPasResolver.CheckGenericTemplateTypes(El: TPasGenericType);
var
  GenTemplates: TFPList;
  i: Integer;
  TemplType: TPasGenericTemplateType;
begin
  GenTemplates:=El.GenericTemplateTypes;
  if (GenTemplates=nil) or (GenTemplates.Count=0) then
    RaiseNotYetImplemented(20190726184902,El,'empty generic template list');

  // template names must differ from generic type name
  for i:=0 to GenTemplates.Count-1 do
    begin
    TemplType:=TPasGenericTemplateType(GenTemplates[i]);
    if SameText(TemplType.Name,El.Name) then
      RaiseMsg(20190801101444,nDuplicateIdentifier,sDuplicateIdentifier,[
        TemplType.Name,GetElementSourcePosStr(El)],TemplType);
    end;
end;

procedure TPasResolver.ComputeUnaryNot(El: TUnaryExpr;
  var ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);
begin
  RaiseMsg(20180208121532,nIllegalQualifierInFrontOf,sIllegalQualifierInFrontOf,
    [OpcodeStrings[El.OpCode],GetResolverResultDescription(ResolvedEl)],El);
  if Flags=[] then ;
end;
