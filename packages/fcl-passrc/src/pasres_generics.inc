{%MainUnit pasresolver.pp}
{ **********************************************************************
 
    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - Generics handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }


procedure TPasResolver.AddGenericTemplateIdentifiers(
  GenericTemplateTypes: TFPList; Scope: TPasIdentifierScope);
var
  TemplType: TPasGenericTemplateType;
  i: Integer;
begin
  if GenericTemplateTypes=nil then exit;
  for i:=0 to GenericTemplateTypes.Count-1 do
    begin
    TemplType:=TPasGenericTemplateType(GenericTemplateTypes[i]);
    Scope.AddIdentifier(TemplType.Name,TemplType,pikSimple);
    end;
end;

procedure TPasResolver.AddSpecializedTemplateIdentifiers(
  GenericTemplateTypes: TFPList; SpecializedItem: TPRSpecializedItem;
  Scope: TPasIdentifierScope; CheckConstraints: boolean);
var
  i: Integer;
  TemplType: TPasGenericTemplateType;
  ParamTypes: TPasTypeArray;
  ParamType: TPasType;
  ErrorPos: TPasElement;
  SynthConst: TPasConst;
begin
  ParamTypes:=SpecializedItem.Params;
  ErrorPos:=SpecializedItem.FirstSpecialize;
  for i:=0 to length(ParamTypes)-1 do
    begin
    TemplType:=TPasGenericTemplateType(GenericTemplateTypes[i]);

    if TemplType.IsConst and (i<length(SpecializedItem.ConstExprs))
        and (SpecializedItem.ConstExprs[i]<>nil) then
      begin
      // Const param: create synthetic TPasConst
      SynthConst:=TPasConst.Create(TemplType.Name,SpecializedItem.SpecializedEl);
      SynthConst.IsConst:=true;
      SynthConst.VarType:=nil;
      SynthConst.Expr:=CreateConstExprForSpecParam(
        SpecializedItem.ConstExprs[i],SynthConst);
      // Store for lifetime management
      if SpecializedItem.SyntheticConsts=nil then
        SpecializedItem.SyntheticConsts:=TObjectList.Create(true);
      SpecializedItem.SyntheticConsts.Add(SynthConst);
      AddIdentifier(Scope,TemplType.Name,SynthConst,pikSimple);
      end
    else
      begin
      // Type param (existing code)
      ParamType:=ParamTypes[i];
      if CheckConstraints then
        begin
        if ParamType is TPasGenericTemplateType then
          CheckTemplateFitsTemplate(TPasGenericTemplateType(ParamType),
                                               TemplType,ErrorPos)
        else
          CheckTemplateFitsParam(ParamType,TemplType,SpecializedItem,
                                 prtcoAssignToTempl,ErrorPos);
        end;
      AddIdentifier(Scope,TemplType.Name,ParamTypes[i],pikSimple);
      end;
    end;
end;

function TPasResolver.CreateInferenceTypesForCall(Params: TParamsExpr;
  TargetProc: TPasProcedure): TFPList;
type
  TInferredType = record
    InferType: TPasType;
    IsVarOut: boolean;
  end;
  TInferredTypes = array of TInferredType;

  procedure RaiseInferTypeMismatch(const Id: TMaxPrecInt; ArgType: TPasType;
    ErrorPos: TPasElement);
  begin
    RaiseMsg(Id,nInferredTypeXFromDiffArgsMismatchFromMethodY,
      sInferredTypeXFromDiffArgsMismatchFromMethodY,
      [ArgType.Name,TargetProc.Name],ErrorPos);
  end;

  procedure Infer(ArgParent: TPasElement; ArgType, ParamLoType, ParamHiType: TPasType;
    NeedVar, IsSubType, IsDelphi: boolean;
    InferenceParams: TInferredTypes; TemplTypes: TFPList;
    ErrorPos: TPasElement); forward;

  procedure InferFromSpecialize(FormalSpec: TPasSpecializeType;
    ParamLoType, ParamHiType: TPasType;
    NeedVar, IsDelphi: boolean;
    InferenceParams: TInferredTypes; TemplTypes: TFPList;
    ErrorPos: TPasElement);
  // Infer template types from a formal param that is "specialize TGeneric<T,...>"
  // by matching against the actual param type
  var
    GenType: TPasGenericType;
    GenTemplList: TFPList;
    j: Integer;
    ActualScope: TPasGenericScope;
    SpecItem: TPRSpecializedItem;
    FormalParam, ActualParam, ActualParamHi: TPasType;
    GenDefType: TPasType;
    GenDefArr: TPasArrayType;
    GenDefProc: TPasProcedureType;
    GenTemplMap: array of TPasType; // maps generic template index -> our template type
    GenTemplTypes: TFPList;
    k: Integer;
    ActualProcType: TPasProcedureType;
  begin
    if ParamLoType = nil then exit;
    if ParamHiType = nil then ;
    // Resolve type aliases to reach the actual specialization type, e.g.
    // TSomeRecord = specialize TAnyRecord<Integer> (timpfuncspez20).
    ParamLoType := ResolveAliasType(ParamLoType, true);
    if ParamLoType = nil then exit;

    // Get the generic base type from the formal specialization
    if not (FormalSpec.DestType is TPasGenericType) then exit;
    GenType := TPasGenericType(FormalSpec.DestType);
    GenTemplList := GenType.GenericTemplateTypes;
    if (GenTemplList = nil) or (GenTemplList.Count = 0) then exit;
    if FormalSpec.Params.Count <> GenTemplList.Count then exit;

    // Strategy 1: Pairwise matching when actual is a specialization of the same generic
    // e.g., formal = specialize TAnyClass<T>, actual = TSomeClass = specialize TAnyClass<Integer>
    if (ParamLoType.CustomData <> nil) and (ParamLoType.CustomData is TPasGenericScope) then
      begin
      ActualScope := TPasGenericScope(ParamLoType.CustomData);
      SpecItem := ActualScope.SpecializedFromItem;
      if (SpecItem <> nil) and (SpecItem.GenericEl = GenType) then
        begin
        // Same generic base — match pairwise
        if Length(SpecItem.Params) = FormalSpec.Params.Count then
          begin
          for j := 0 to FormalSpec.Params.Count - 1 do
            begin
            FormalParam := GetSpecializeParamAsType(TPasElement(FormalSpec.Params[j]));
            if FormalParam=nil then continue;
            ActualParam := SpecItem.Params[j];
            if ActualParam <> nil then
              Infer(FormalSpec, FormalParam, ActualParam,
                    ResolveAliasType(ActualParam),
                    NeedVar, true, IsDelphi, InferenceParams, TemplTypes, ErrorPos);
            end;
          exit;
          end;
        end;
      end;

    // Strategy 2: Resolve the generic definition and infer through its structure
    // e.g., formal = specialize TMyArray<T> where TMyArray<T> = array of T
    //   and actual = [1,2,3] resolved to array of Integer
    // Or formal = specialize TProc<T> where TProc<T> = procedure(name: string; value: T)
    //   and actual = @DoCallback with type procedure(string, LongInt)

    // Build a mapping from generic's template types to our formal specialization params
    GenTemplMap:=[];
    SetLength(GenTemplMap, GenTemplList.Count);
    for j := 0 to GenTemplList.Count - 1 do
      GenTemplMap[j] := GetSpecializeParamAsType(TPasElement(FormalSpec.Params[j]));

    // Resolve what the generic type actually IS (class, record, array, etc.)
    // For generic type aliases like "TMyArray<T> = array of T", the underlying type
    // is accessible via the generic type's own structure
    GenDefType := nil;
    if GenType is TPasArrayType then
      GenDefType := GenType
    else if GenType is TPasClassType then
      begin
      // For classes: check if actual is a subclass/same class of a specialization
      // This is already handled by Strategy 1 above
      // If Strategy 1 didn't match, we can't do structural inference for classes
      exit;
      end
    else if GenType is TPasRecordType then
      exit // can't structurally infer from records
    else if GenType is TPasProcedureType then
      GenDefType := GenType
    else
      exit; // unknown generic type kind

    if GenDefType = nil then exit;

    // For array types: match element types
    if GenDefType is TPasArrayType then
      begin
      GenDefArr := TPasArrayType(GenDefType);
      // The actual param should also be an array type
      if ParamLoType is TPasArrayType then
        begin
        // Match: GenDefArr.ElType (may reference generic templates) with actual's ElType
        // Replace generic template references with our formal params
        if GenDefArr.ElType <> nil then
          begin
          // Find which generic template the element type references
          GenTemplTypes := GenType.GenericTemplateTypes;
          for k := 0 to GenTemplTypes.Count - 1 do
            if GenDefArr.ElType = TPasType(GenTemplTypes[k]) then
              begin
              // Element type IS a generic template — infer from actual array's element type
              ActualParam := TPasArrayType(ParamLoType).ElType;
              if ActualParam <> nil then
                Infer(FormalSpec, GenTemplMap[k], ActualParam,
                      ResolveAliasType(ActualParam),
                      NeedVar, true, IsDelphi, InferenceParams, TemplTypes, ErrorPos);
              exit;
              end;
          // Element type might be another specialize type — recurse
          if GenDefArr.ElType is TPasSpecializeType then
            begin
            ActualParam := TPasArrayType(ParamLoType).ElType;
            if ActualParam <> nil then
              begin
              // Build a new specialize type reference with our mapped params
              // For now, direct recursive call through Infer
              Infer(GenDefArr, GenDefArr.ElType, ActualParam,
                    ResolveAliasType(ActualParam),
                    NeedVar, true, IsDelphi, InferenceParams, TemplTypes, ErrorPos);
              end;
            end;
          end;
        end;
      end
    // For procedure types: match parameter types
    else if GenDefType is TPasProcedureType then
      begin
      GenDefProc := TPasProcedureType(GenDefType);
      // The actual param should also be a procedure type
      if ParamLoType is TPasProcedureType then
        begin
        ActualProcType := TPasProcedureType(ParamLoType);
        // Match each procedure parameter type
        GenTemplTypes := GenType.GenericTemplateTypes;
        for j := 0 to GenDefProc.Args.Count - 1 do
          begin
          if j >= ActualProcType.Args.Count then break;
          FormalParam := TPasArgument(GenDefProc.Args[j]).ArgType;
          if FormalParam = nil then continue;
          ActualParam := TPasArgument(ActualProcType.Args[j]).ArgType;
          if ActualParam = nil then continue;
          // Check if FormalParam is a generic template type
          for k := 0 to GenTemplTypes.Count - 1 do
            if FormalParam = TPasType(GenTemplTypes[k]) then
              begin
              ActualParamHi := ResolveAliasType(ActualParam);
              Infer(FormalSpec, GenTemplMap[k], ActualParam, ActualParamHi,
                    NeedVar, true, IsDelphi, InferenceParams, TemplTypes, ErrorPos);
              break;
              end;
          end;
        end;
      end;
  end;

  procedure InferFromSpecializeWithArrayLit(FormalSpec: TPasSpecializeType;
    ElemLoType, ElemHiType: TPasType;
    NeedVar, IsDelphi: boolean;
    InferenceParams: TInferredTypes; TemplTypes: TFPList;
    ErrorPos: TPasElement);
  // Array constructor [expr,...] passed to "specialize TMyArray<T>"
  // ElemLoType/ElemHiType are the element types of the array literal
  var
    GenType: TPasGenericType;
    GenTemplList: TFPList;
    GenDefArr: TPasArrayType;
    k: Integer;
    FormalParam: TPasType;
  begin
    if ElemLoType = nil then exit;
    if not (FormalSpec.DestType is TPasGenericType) then exit;
    GenType := TPasGenericType(FormalSpec.DestType);
    GenTemplList := GenType.GenericTemplateTypes;
    if (GenTemplList = nil) or (GenTemplList.Count = 0) then exit;
    if not (GenType is TPasArrayType) then exit;
    GenDefArr := TPasArrayType(GenType);
    if GenDefArr.ElType = nil then exit;
    // Check if element type is a generic template → direct mapping
    for k := 0 to GenTemplList.Count - 1 do
      if GenDefArr.ElType = TPasType(GenTemplList[k]) then
        begin
        // Map generic template index k → our formal param
        FormalParam := TPasType(FormalSpec.Params[k]);
        if FormalParam is TPasSpecializeType then
          begin
          // Formal param is itself a specialization (e.g., specialize TArray<T>)
          // For nested array literals like [[1,2,3],...], recurse through the inner spec
          InferFromSpecializeWithArrayLit(TPasSpecializeType(FormalParam),
                ElemLoType, ElemHiType,
                NeedVar, IsDelphi, InferenceParams, TemplTypes, ErrorPos);
          end
        else
          Infer(FormalSpec, FormalParam, ElemLoType, ElemHiType,
                NeedVar, true, IsDelphi, InferenceParams, TemplTypes, ErrorPos);
        exit;
        end;
  end;

  procedure Infer(ArgParent: TPasElement; ArgType, ParamLoType, ParamHiType: TPasType;
    NeedVar, IsSubType, IsDelphi: boolean;
    InferenceParams: TInferredTypes; TemplTypes: TFPList;
    ErrorPos: TPasElement);
  var
    C: TClass;
    i: Integer;
    OldInferType, ParamElType: TPasType;
    ResolveAlias: TPRResolveAlias;
    Arr: TPasArrayType;
    Param1Resolved, Param2Resolved: TPasResolverResult;
    NewBaseType, BaseType1, BaseType2: TResolverBaseType;
    MinIntVal1, MaxIntVal1, MinIntVal2, MaxIntVal2: TMaxPrecInt;
  begin
    if (ArgType=nil) or (ParamLoType=nil) then exit;
    C:=ArgType.ClassType;
    if C=TPasGenericTemplateType then
      begin
      i:=TemplTypes.IndexOf(ArgType);
      if i>=0 then
        begin
        // a generic type param corresponds to ParamType
        OldInferType:=InferenceParams[i].InferType;
        if OldInferType=nil then
          begin
          // template type inferred first time
          InferenceParams[i].InferType:=ParamHiType;
          InferenceParams[i].IsVarOut:=NeedVar;
          exit;
          end;

        // already inferred -> check compatibility
        ResolveAlias:=prraAlias;
        if IsDelphi and (NeedVar or InferenceParams[i].IsVarOut) then
          // Delphi allows passing alias, but not type alias to a var arg
          ResolveAlias:=prraSimple;
        if IsSameType(OldInferType,ParamHiType,ResolveAlias) then
          exit; // same types -> ok

        if IsSubType then
          begin
          if CheckElTypeCompatibility(OldInferType,InferenceParams[i].InferType,
              ResolveAlias)<=cGenericExact then
            exit;
          // e.g. "array of TA" and "array of TB"
          RaiseInferTypeMismatch(20191006215539,ArgType,ErrorPos);
          end;

        // top level type does not fit exactly
        if NeedVar then
          begin
          // second is var/out
          if InferenceParams[i].IsVarOut then
            // two var/out arguments mismatch
            RaiseInferTypeMismatch(20191006220355,ArgType,ErrorPos);
          if CheckAssignCompatibility(ParamHiType,OldInferType,
              false,ErrorPos)=cIncompatible then
            // second is var/out, and do not match
            RaiseInferTypeMismatch(20191006220402,ArgType,ErrorPos);
          // first can be widened to fit
          InferenceParams[i].InferType:=ParamHiType;
          InferenceParams[i].IsVarOut:=NeedVar;
          exit;
          end
        else if InferenceParams[i].IsVarOut then
          begin
          // first was var/out
          if CheckAssignCompatibility(OldInferType,ParamHiType,
              false,ErrorPos)=cIncompatible then
            // first was var/out, and do not match
            RaiseInferTypeMismatch(20191006220750,ArgType,ErrorPos);
          // second can be widened to fit
          exit;
          end;

        // None is var/out -> find a type compatible to both
        // widen type to some common base types to avoid high number of specialization
        ComputeElement(ParamHiType,Param1Resolved,[],ErrorPos);
        ComputeElement(InferenceParams[i].InferType,Param2Resolved,[],ErrorPos);
        NewBaseType:=btNone;
        BaseType1:=Param1Resolved.BaseType;
        BaseType2:=Param2Resolved.BaseType;
        // If the new param type's range fully contains the old inferred type's
        // range, keep the old (narrower) inference. This prevents widening a
        // proc-type-derived ShortInt to Integer from an integer literal
        // (timpfuncspez13: T pinned to ShortInt by the callback, not the `100`).
        // Native/FPC target only: base and pas2js widen two integer inferences to a
        // common base type (pristine upstream; pas2js TestGenProc_Infer_Widen groups
        // word/byte/smallint/longint into one longint specialization).
        if PreferNarrowerInferredInteger
            and (BaseType1 in btAllInteger) and (BaseType2 in btAllInteger) then
          begin
          if GetIntegerRange(BaseType1,MinIntVal1,MaxIntVal1)
              and GetIntegerRange(BaseType2,MinIntVal2,MaxIntVal2)
              and (MinIntVal1<=MinIntVal2) and (MaxIntVal1>=MaxIntVal2) then
            exit;
          end;
        if BaseType1 in btAllBooleans then
          begin
          if BaseType2 in btAllBooleans then
            if BaseTypes[btBoolean]<>nil then
              NewBaseType:=btBoolean
            else
              NewBaseType:=GetCombinedBoolean(BaseType1,BaseType2,ErrorPos);
          end
        else if BaseType1 in btAllInteger then
          begin
          NewBaseType:=TResolverBaseType(Max(ord(BaseType1),ord(BaseType2)));
          if (BaseTypes[btLongint]<>nil)
              and (NewBaseType in [btByte,btShortInt,btWord,btSmallInt,btIntSingle,btUIntSingle,btLongint])
              and (BaseType1<>btLongWord) and (BaseType2<>btLongWord) then
            NewBaseType:=btLongint
          {$ifdef HasInt64}
          else if (BaseTypes[btInt64]<>nil)
              and (NewBaseType<=btInt64)
              and (BaseType1<>btQWord) and (BaseType2<>btQWord) then
            NewBaseType:=btInt64
          {$endif}
          else if (BaseTypes[btIntDouble]<>nil)
              and (NewBaseType<=btIntDouble) then
            NewBaseType:=btIntDouble
          {$ifdef HasInt64}
          else if (BaseTypes[btQWord]<>nil)
              and not (NewBaseType in btAllSignedInteger) then
            NewBaseType:=btQWord
          {$endif}
          else
            NewBaseType:=GetCombinedInt(Param1Resolved,Param2Resolved,ErrorPos);
          end
        else if Param1Resolved.BaseType in btAllStringAndChars then
          begin
          if Param2Resolved.BaseType in btAllStringAndChars then
            if BaseTypes[btUnicodeString]<>nil then
              NewBaseType:=btUnicodeString
            else
              NewBaseType:=GetCombinedString(Param1Resolved,Param2Resolved,ErrorPos);
          end
        else if Param1Resolved.BaseType in btAllFloats then
          begin
          if BaseTypes[btDouble]<>nil then
            NewBaseType:=btDouble;
          end;
        if NewBaseType<>btNone then
          begin
          InferenceParams[i].InferType:=BaseTypes[NewBaseType];
          InferenceParams[i].IsVarOut:=NeedVar;
          exit;
          end;

        // ToDo
        RaiseInferTypeMismatch(20191006220406,ArgType,ErrorPos);
        end;
      end
    else if ArgParent<>ArgType.Parent then
      // ArgType is a reference
    else if C=TPasArrayType then
      begin
      // e.g. Proc(a: array...)
      Arr:=TPasArrayType(ArgType);
      if ParamLoType.ClassType<>TPasArrayType then
        exit;
      ParamElType:=TPasArrayType(ParamLoType).ElType;
      Infer(Arr,Arr.ElType,ParamElType,ResolveAliasType(ParamElType),
            NeedVar,true,IsDelphi,InferenceParams,TemplTypes,ErrorPos);
      end
    else if C=TPasSpecializeType then
      begin
      // e.g. specialize TFoo<T> — try to match against actual param type
      InferFromSpecialize(TPasSpecializeType(ArgType), ParamLoType, ParamHiType,
        NeedVar, IsDelphi, InferenceParams, TemplTypes, ErrorPos);
      end
    else
      begin
      {$IFDEF VerbosePasResolver}
      //writeln('Infer ArgType=',GetObjName(ArgType),' ParamLoType=',GetObjName(ParamLoType));
      {$ENDIF}
      end;
  end;

  procedure InferParam(i: integer; NeedVar: boolean; ParamsExprs: TPasExprArray;
    ProcArgs: TFPList;
    InferenceParams: TInferredTypes; TemplTypes: TFPList; IsDelphi: boolean);
  var
    Arg: TPasArgument;
    ArgType: TPasType;
    ArgResolved, ExprResolved: TPasResolverResult;
    Expr: TPasExpr;
    ElemLo, ElemHi: TPasType;
    InfArr: TPasArrayType;

    function AllStringLiterals(E: TPasExpr): boolean;
    // True if E is an array/set constructor whose elements are all string
    // literals (so the natural element type is ShortString, not AnsiString).
    var
      k: Integer;
      P: TPasExpr;
    begin
      Result:=false;
      if not (E is TParamsExpr) then exit;
      if length(TParamsExpr(E).Params)=0 then exit;
      for k:=0 to length(TParamsExpr(E).Params)-1 do
        begin
        P:=TParamsExpr(E).Params[k];
        if not ((P is TPrimitiveExpr)
            and (TPrimitiveExpr(P).Kind in [pekString])) then
          exit;
        end;
      Result:=true;
    end;

  begin
    //writeln('InferParam i=',i,' NeedVar=',NeedVar,' IsDelphi=',IsDelphi,' ProcArgs.Count=',ProcArgs.Count);
    Arg:=TPasArgument(ProcArgs[i]);
    ArgType:=Arg.ArgType;
    if ArgType=nil then
      exit; // untyped arg
    if (ArgType.Parent<>Arg) and (ArgType.ClassType<>TPasGenericTemplateType)
        and (ArgType.ClassType<>TPasSpecializeType) then
      exit; // a reference -> no need to search for a template reference
    if NeedVar<>(Arg.Access in [argVar, argOut]) then
      exit;

    if i<length(ParamsExprs) then
      Expr:=ParamsExprs[i]
    else if InferTemplTypesFromDefaults then
      begin
      Expr:=Arg.ValueExpr;
      if Expr=nil then exit;
      end
    else
      // The arg was not passed at the call site. A native/FPC target does NOT use
      // a default VALUE to infer a template type (timpfuncspez16/17: `DoThis(1)`
      // for `DoThis<T>(a:integer; c:T=0)` leaves T uninferred -> error).
      exit;
    ComputeArgumentAndExpr(Arg,ArgResolved,Expr,ExprResolved,false);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CreateInferenceTypesForCall Arg=',GetTreeDbg(Arg,2),' ArgResolved=',GetResolverResultDbg(ArgResolved));
    {$ENDIF}

    if ExprResolved.BaseType in btAllWithSubType then
      begin
      if (ExprResolved.BaseType in [btArrayLit, btArrayOrSet]) and (ExprResolved.LoTypeEl <> nil) then
        begin
        ElemLo:=ExprResolved.LoTypeEl;
        ElemHi:=ResolveAliasType(ExprResolved.LoTypeEl);
        // An array constructor of string literals (e.g. ['aaa','bbb']) infers its
        // element type as ShortString — a string constant's natural type — not
        // AnsiString (FPC, timpfuncspez12). Inert for targets without ShortString.
        if (ExprResolved.SubType in btAllStrings) and AllStringLiterals(Expr)
            {$IFDEF FPC_HAS_CPSTRING}and (BaseTypes[btShortString]<>nil) {$ENDIF} then
          begin
          {$IFDEF FPC_HAS_CPSTRING}
          ElemLo:=BaseTypes[btShortString];
          {$ELSE}
          ElemLo:=BaseTypes[btUnicodeString];
          {$ENDIF}
          ElemHi:=ElemLo;
          end;
        if ArgType.ClassType = TPasArrayType then
          begin
          // array constructor [expr,...] passed to "array of T" — infer T from element type
          Infer(Arg, TPasArrayType(ArgType).ElType,
                ElemLo, ElemHi,
                NeedVar, true, IsDelphi, InferenceParams, TemplTypes, Expr);
          end
        else if ArgType.ClassType = TPasSpecializeType then
          begin
          // array constructor [expr,...] passed to "specialize TMyArray<T>"
          // Resolve generic base — if it's an array type, infer from element type
          InferFromSpecializeWithArrayLit(TPasSpecializeType(ArgType),
                ElemLo, ElemHi,
                NeedVar, IsDelphi, InferenceParams, TemplTypes, Expr);
          end
        else if (ArgType.ClassType = TPasGenericTemplateType) and (ElemHi<>nil) then
          begin
          // array constructor [expr,...] passed to a bare template "value: T":
          // infer T as a dynamic array of the element type ("array of ElemHi").
          // The synthesized array type is resolver-owned; its ElType is a plain
          // reference (ElemHi.Parent<>InfArr), so FreeChildren won't free it.
          InfArr:=TPasArrayType(CreateOwnedElement(TPasArrayType,'',Arg));
          InfArr.ElType:=ElemHi;
          Infer(Arg, ArgType, InfArr, InfArr,
                NeedVar, false, IsDelphi, InferenceParams, TemplTypes, Expr);
          end;
        end;
      end
    else if (ExprResolved.SubType<>btNone) then
      RaiseNotYetImplemented(20191006203622,Expr)
    else if (ExprResolved.BaseType in btAllStrings)
        and (ArgType.ClassType = TPasArrayType)
        and (TPasArrayType(ArgType).ElType <> nil) then
      begin
      // String passed to "array of T" — infer T = Char (timpfuncspez4)
      if BaseTypes[btChar] <> nil then
        Infer(Arg, TPasArrayType(ArgType).ElType,
              BaseTypes[btChar], BaseTypes[btChar],
              NeedVar, true, IsDelphi, InferenceParams, TemplTypes, Expr);
      end
    else if (ExprResolved.BaseType = btNil) and (ExprResolved.LoTypeEl = nil) then
      begin
      // nil — infer as Pointer type
      if BaseTypes[btPointer] <> nil then
        Infer(Arg,ArgType,BaseTypes[btPointer],BaseTypes[btPointer],
              NeedVar,false,IsDelphi,
              InferenceParams,TemplTypes,Expr);
      end
    else
      Infer(Arg,ArgType,ExprResolved.LoTypeEl,ExprResolved.HiTypeEl,
            NeedVar,false,IsDelphi,
            InferenceParams,TemplTypes,Expr);
  end;

var
  TemplTypes, ProcArgs: TFPList;
  InferenceTypes: TInferredTypes;
  ParamsExprs: TPasExprArray;
  IsDelphi: Boolean;
  i: Integer;
begin
  Result:=nil;
  TemplTypes:=GetProcTemplateTypes(TargetProc);
  if (TemplTypes=nil) or (TemplTypes.Count=0) then
    RaiseNotYetImplemented(20191006174321,Params);
  ProcArgs:=TargetProc.ProcType.Args;
  ParamsExprs:=Params.Params;
  if ProcArgs.Count<length(ParamsExprs) then
    RaiseNotYetImplemented(20191006183021,Params);

  IsDelphi:=msDelphi in CurrentParser.CurrentModeswitches;

  SetLength(InferenceTypes{%H-},TemplTypes.Count);
  for i:=0 to TemplTypes.Count-1 do
    InferenceTypes[i]:=Default(TInferredType);

  // first infer from var/out args exact types
  for i:=0 to ProcArgs.Count-1 do
    InferParam(i,true,ParamsExprs,ProcArgs,InferenceTypes,TemplTypes,IsDelphi);

  // then infer from the other args
  for i:=0 to ProcArgs.Count-1 do
    InferParam(i,false,ParamsExprs,ProcArgs,InferenceTypes,TemplTypes,IsDelphi);

  // check that all types are inferred
  for i:=0 to TemplTypes.Count-1 do
    if InferenceTypes[i].InferType=nil then
      RaiseMsg(20191006175104,nCouldNotInferTypeArgXForMethodY,
        sCouldNotInferTypeArgXForMethodY,
        [TPasGenericTemplateType(TemplTypes[i]).Name,TargetProc.Name],Params);

  Result:=TFPList.Create;
  for i:=0 to length(InferenceTypes)-1 do
    begin
    Result.Add(InferenceTypes[i].InferType);
    InferenceTypes[i].InferType:=nil;
    end;
end;

function TPasResolver.CheckGenericConstraintFitsParam(ParamType: TPasType;
  SpecializedItem: TPRSpecializedItem; TemplType: TPasGenericTemplateType;
  ConEl: TPasElement; Operation: TPRTemplateCompOp; ErrorPos: TPasElement
  ): integer;

  function RaiseXExpButYFound(id: TMaxPrecInt; const X: string; Y: TPasType): integer;
  begin
    if ErrorPos<>nil then
      RaiseXExpectedButTypeYFound(id,X,Y,ErrorPos);
    Result:=cIncompatible;
  end;

  procedure RaiseNotValidConstraint(Id: TMaxPrecInt; ConEl: TPasElement);
  begin
    RaiseMsg(Id,nXIsNotAValidConstraint,sXIsNotAValidConstraint,
      [GetElementSourcePosStr(ConEl)],ErrorPos);
  end;

  function ElementReferencesTemplateTypes(El: TPasElement;
    GenericTemplateTypes: TFPList): boolean;
  var
    C: TClass;
    Prim: TPrimitiveExpr;
    Decl: TPasElement;
    Bin: TBinaryExpr;
    Spec: TPasSpecializeType;
    Arr: TPasArrayType;
    i: Integer;
    InlineSpec: TInlineSpecializeExpr;
    CaseEx: TCaseExpr;
    CaseBranch: TCaseExprBranch;
    j: Integer;
    TryEx: TTryExceptExpr;
    TryOn: TTryExceptExprOn;
  begin
    Result:=false;
    if El=nil then exit;
    C:=El.ClassType;
    if C=TPrimitiveExpr then
      begin
      Prim:=TPrimitiveExpr(El);
      if Prim.Kind=pekIdent then
        begin
        if Prim.CustomData is TResolvedReference then
          begin
          Decl:=TResolvedReference(Prim.CustomData).Declaration;
          exit(ElementReferencesTemplateTypes(Decl,GenericTemplateTypes));
          end;
        end
      else
        exit;
      end
    else if C=TBinaryExpr then
      begin
      Bin:=TBinaryExpr(El);
      Result:=ElementReferencesTemplateTypes(Bin.Left,GenericTemplateTypes)
        or ElementReferencesTemplateTypes(Bin.Right,GenericTemplateTypes);
      end
    else if C=TIfExpr then
      Result:=ElementReferencesTemplateTypes(TIfExpr(El).ConditionExpr,GenericTemplateTypes)
        or ElementReferencesTemplateTypes(TIfExpr(El).ThenExpr,GenericTemplateTypes)
        or ElementReferencesTemplateTypes(TIfExpr(El).ElseExpr,GenericTemplateTypes)
    else if C=TCaseExpr then
      begin
      CaseEx:=TCaseExpr(El);
      Result:=ElementReferencesTemplateTypes(CaseEx.CaseExpr,GenericTemplateTypes)
        or ElementReferencesTemplateTypes(CaseEx.ElseExpr,GenericTemplateTypes);
      for i:=0 to CaseEx.Branches.Count-1 do
        begin
        if Result then break;
        CaseBranch:=TCaseExprBranch(CaseEx.Branches[i]);
        Result:=ElementReferencesTemplateTypes(CaseBranch.Value,GenericTemplateTypes);
        for j:=0 to CaseBranch.Labels.Count-1 do
          begin
          Result:=ElementReferencesTemplateTypes(TPasElement(CaseBranch.Labels[j]),GenericTemplateTypes);
          if Result then break;
          end;
        end;
      end
    else if C=TUnaryExpr then
      Result:=ElementReferencesTemplateTypes(TUnaryExpr(El).Operand,GenericTemplateTypes)
    else if C=TTryExceptExpr then
      begin
      TryEx:=TTryExceptExpr(El);
      Result:=ElementReferencesTemplateTypes(TryEx.TryExpr,GenericTemplateTypes)
        or ElementReferencesTemplateTypes(TryEx.ElseExpr,GenericTemplateTypes);
      for i:=0 to TryEx.OnBranches.Count-1 do
        begin
        TryOn:=TTryExceptExprOn(TryEx.OnBranches[i]);
        Result:=ElementReferencesTemplateTypes(TryOn.TypeEl,GenericTemplateTypes)
          or ElementReferencesTemplateTypes(TryOn.Value,GenericTemplateTypes);
        if Result then break;
        end;
      end
    else if C=TInlineSpecializeExpr then
      begin
      InlineSpec:=TInlineSpecializeExpr(El);
      if ElementReferencesTemplateTypes(InlineSpec.NameExpr,GenericTemplateTypes) then
        exit(true);
      for i:=0 to InlineSpec.Params.Count-1 do
        begin
        Decl:=TPasElement(InlineSpec.Params[i]);
        if Decl.Parent<>InlineSpec then continue;
        if ElementReferencesTemplateTypes(Decl,GenericTemplateTypes) then
          exit(true);
        end;
      end
    else if C=TPasGenericTemplateType then
      Result:=GenericTemplateTypes.IndexOf(El)>=0
    else if C.InheritsFrom(TPasType) then
      begin
      if TPasType(El).Name<>'' then exit;
      if C=TPasSpecializeType then
        begin
        Spec:=TPasSpecializeType(El);
        if ElementReferencesTemplateTypes(Spec.DestType,GenericTemplateTypes) then
          exit(true);
        for i:=0 to Spec.Params.Count-1 do
          if ElementReferencesTemplateTypes(TPasElement(Spec.Params[i]),GenericTemplateTypes) then
            exit(true);
        end
      else if C=TPasArrayType then
        begin
        Arr:=TPasArrayType(El);
        for i:=0 to length(Arr.Ranges)-1 do
          if ElementReferencesTemplateTypes(Arr.Ranges[i],GenericTemplateTypes) then exit(true);
        Result:=ElementReferencesTemplateTypes(Arr.ElType,GenericTemplateTypes);
        end
      else if C=TPasPointerType then
        Result:=ElementReferencesTemplateTypes(TPasPointerType(El).DestType,GenericTemplateTypes)
      else if C=TPasSetType then
        Result:=ElementReferencesTemplateTypes(TPasSetType(El).EnumType,GenericTemplateTypes)
      else if C=TPasTypeOfType then
        Result:=ElementReferencesTemplateTypes(TPasTypeOfType(El).DestType,GenericTemplateTypes)
          or ElementReferencesTemplateTypes(TPasTypeOfType(El).Expr,GenericTemplateTypes)
      else if C=TPasEnumType then
      else
        RaiseNotYetImplemented(20190905110152,El);
      end
    else
      RaiseNotYetImplemented(20190905105648,El);
  end;

var
  ConToken: TToken;
  aClass, ConstraintClass: TPasClassType;
  GenTempl: TPasGenericTemplateType;
  i: Integer;
  ResolvedEl: TPasResolverResult;
  ConType: TPasType;
  GenericTemplateTypes: TFPList;
  GenericEl: TPasElement;
begin
  ConToken:=GetGenericConstraintKeyword(ConEl);
  case ConToken of
  tkrecord:
    begin
    if ParamType is TPasRecordType then exit(cExact);
    exit(RaiseXExpButYFound(20190725200015,'record type',ParamType));
    end;
  tkclass,tkconstructor:
    begin
    if not (ParamType is TPasClassType) then
      exit(RaiseXExpButYFound(20190726133231,'class type',ParamType));
    aClass:=TPasClassType(ParamType);
    if aClass.ObjKind<>okClass then
      exit(RaiseXExpButYFound(20190726133232,'class type',ParamType));
    if aClass.IsExternal then
      exit(RaiseXExpButYFound(20190726133233,'non external class type',ParamType));
    if ConToken=tkconstructor then
      begin
      if FindDefaultConstructor(aClass)=nil then
        exit(RaiseXExpButYFound(20190831000225,'class type with constructor create()',ParamType));
      end;
    exit;
    end;
  end;

  if not (ConEl is TPasType) then
    RaiseNotYetImplemented(20190912214727,ConEl,GetObjPath(ErrorPos));

  // constraint can be a class type, interface type or a template type
  // Param must be a class
  if SpecializedItem<>nil then
    begin
    GenericEl:=SpecializedItem.GenericEl;
    if GenericEl is TPasGenericType then
      GenericTemplateTypes:=TPasGenericType(GenericEl).GenericTemplateTypes
    else if GenericEl is TPasProcedure then
      GenericTemplateTypes:=GetProcTemplateTypes(TPasProcedure(GenericEl))
    else
      RaiseNotYetImplemented(20190920114755,ConEl);
    if ElementReferencesTemplateTypes(ConEl,GenericTemplateTypes) then
      begin
      // constraint contains templates -> specialize constraint
      if ConEl is TPasType then
        begin
        // type reference
        ConType:=TPasType(ConEl);
        i:=length(SpecializedItem.SpecializedConstraints);
        Setlength(SpecializedItem.SpecializedConstraints,i+1);
        SpecializedItem.SpecializedConstraints[i]:=nil;
        SpecializeElType(TemplType,SpecializedItem.SpecializedEl,ConType,
                         TPasType(SpecializedItem.SpecializedConstraints[i]));
        ConEl:=SpecializedItem.SpecializedConstraints[i];
        end
      else
        // non type reference
        RaiseNotValidConstraint(20190915181137,ConEl);
      end;
    end;
  ComputeElement(ConEl,ResolvedEl,[rcType]);
  if ResolvedEl.BaseType<>btContext then
    RaiseNotValidConstraint(20190914105836,ConEl);

  if ResolvedEl.HiTypeEl.Name='' then
    RaiseNotValidConstraint(20190726134037,GetGenericConstraintErrorEl(ConEl,TemplType));
  if ResolvedEl.LoTypeEl is TPasGenericTemplateType then
    begin
    GenTempl:=TPasGenericTemplateType(ResolvedEl.LoTypeEl);
    if GenTempl=ConEl.Parent then
      RaiseNotYetImplemented(20190831213359,GenTempl);
    Result:=CheckTemplateFitsParam(ParamType,GenTempl,nil,Operation,ErrorPos);
    end
  else if ResolvedEl.LoTypeEl is TPasClassType then
    begin
    // constraint is classtype or interfacetype
    ConstraintClass:=TPasClassType(ResolvedEl.LoTypeEl);
    if not (ParamType is TPasClassType) then
      begin
      if ErrorPos<>nil then
        RaiseIncompatibleType(20190726135859,nIncompatibleTypesGotExpected,[''],
                              ParamType,ConstraintClass,ErrorPos);
      exit(cIncompatible);
      end;
    if not (TPasClassType(ParamType).ObjKind in [okClass,okInterface]) then
      begin
      if ErrorPos<>nil then
        RaiseMsg(20190904175144,nXExpectedButYFound,sXExpectedButYFound,
                 ['class',GetTypeDescription(ParamType)],ErrorPos);
      exit(cIncompatible);
      end;
    case ConstraintClass.ObjKind of
    okClass:
      case Operation of
      prtcoAssignToTempl:
        // TemplateClass:=ParamClassType
        if CheckClassIsClass(ParamType,ConstraintClass)=cIncompatible then
          begin
          // ParamType is not ConstraintClass
          if ErrorPos<>nil then
            RaiseIncompatibleType(20190726135309,nIncompatibleTypesGotExpected,[''],
                                    ParamType,ConstraintClass,ErrorPos);
          exit(cIncompatible);
          end;
      prtcoAssignFromTempl:
        // ParamClassType:=TemplateClass -- legal when the constraint class IS
        // the target or descends from it. The test was inverted: it raised
        // exactly when the assignment was VALID, so `Items[i]:=v` with
        // `T: TObject` into a TObject property reported
        // 'got "System.TObject" expected "System.TObject"'
        // (fcl-md's markdown.utils TGFPObjectList).
        if CheckClassIsClass(ConstraintClass,ParamType)=cIncompatible then
          begin
          // Undecided target: `TDictionary<T,TEmptyRecord>.TKeyEnumerator` is a
          // member of the bare generic once a partial specialization answers
          // with it (PartialSpecsAsGeneric), so which class it is depends on a
          // type argument nobody has supplied yet.
          if UndecidedPair(ConstraintClass,ParamType) then
            exit(cCompatible);
          // ConstraintClass is not ParamType
          if ErrorPos<>nil then
            RaiseIncompatibleType(20190915202812,nIncompatibleTypesGotExpected,[''],
                                  ConstraintClass,ParamType,ErrorPos);
          exit(cIncompatible);
          end;
      prtcoEqual:
        // TemplateClass=ParamClassType
        if (CheckClassIsClass(ParamType,ConstraintClass)=cIncompatible)
            and (CheckClassIsClass(ConstraintClass,ParamType)<>cIncompatible) then
          begin
          // ParamType is not related to ConstraintClass
          if ErrorPos<>nil then
            RaiseIncompatibleType(20190915203651,nIncompatibleTypesGotExpected,[''],
                                    ParamType,ConstraintClass,ErrorPos);
          exit(cIncompatible);
          end;
      else
        RaiseNotYetImplemented(20190915203439,ConEl);
      end;
    okInterface:
      begin
      // A forward *interface* param provably satisfies the universal root
      // interface (IInterface/IUnknown, which has no ancestor), since every
      // interface descends from it. A forward *class* does NOT: a class must
      // explicitly implement an interface, which an unresolved forward cannot
      // promise — so it falls through and is rejected.
      // (tgenconstraint37 accepts forward interface vs root; 38 rejects a
      // forward class vs IInterface; 40 rejects forward vs a specific interface.)
      if (ParamType is TPasClassType) and TPasClassType(ParamType).IsForward
          and (TPasClassType(ParamType).ObjKind = okInterface)
          and not (ParamType.CustomData is TResolvedReference)
          and (ConstraintClass.CustomData is TPasClassScope)
          and (TPasClassScope(ConstraintClass.CustomData).DirectAncestor=nil) then
        exit(cExact);
      case Operation of
      prtcoAssignToTempl:
        // TemplateClassWithIntf:=ParamClassType
        if GetClassImplementsIntf(TPasClassType(ParamType),ConstraintClass)=nil then
          begin
          // ParamType does not implement ConstraintClass
          if ErrorPos<>nil then
            RaiseIncompatibleType(20190726135458,nIncompatibleTypesGotExpected,[''],
                                  ParamType,ConstraintClass,ErrorPos);
          exit(cIncompatible);
          end;
      prtcoAssignFromTempl:
        // ParamClassType:=TemplateClassWithIntf
        begin
        // check when specialize
        end;
      prtcoEqual:
        // TemplateClassWithIntf=ParamClassType
        begin
        // check when specialize
        end;
      else
        RaiseNotYetImplemented(20190915203218,ConEl);
      end;
      end;
    else
      if ErrorPos<>nil then
        RaiseIncompatibleType(20190726135310,nIncompatibleTypesGotExpected,[''],
                              ParamType,ConstraintClass,ErrorPos);
      exit(cIncompatible);
    end;
    end
  else
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckSpecializedParamFitsConstraintExpr ',GetObjPath(ResolvedEl.LoTypeEl));
    {$ENDIF}
    RaiseMsg(20190726134223,nXIsNotAValidConstraint,sXIsNotAValidConstraint,
             [GetElementSourcePosStr(GetGenericConstraintErrorEl(ConEl,ConEl.Parent))],
              ErrorPos);
    end;
  Result:=cExact;
end;

function TPasResolver.CheckTemplateFitsParam(ParamType: TPasType;
  GenTempl: TPasGenericTemplateType; SpecializedItem: TPRSpecializedItem;
  Operation: TPRTemplateCompOp; ErrorPos: TPasElement): integer;
var
  i: Integer;
begin
  // check if the ParamType fits the constraints
  for i:=0 to length(GenTempl.Constraints)-1 do
    begin
    Result:=CheckGenericConstraintFitsParam(ParamType,SpecializedItem,
                       GenTempl,GenTempl.Constraints[i],Operation,ErrorPos);
    if Result=cIncompatible then exit;
    end;
  Result:=cExact;
end;

function TPasResolver.CheckTemplateFitsParamRes(
  GenTempl: TPasGenericTemplateType; const ResolvedEl: TPasResolverResult;
  Operation: TPRTemplateCompOp; ErrorPos: TPasElement): integer;
var
  i: Integer;
  ConEl: TPasElement;
  ConToken: TToken;
  LoTypeEl: TPasType;
begin
  if length(GenTempl.Constraints)=0 then
    exit(cGenericExact);
  if GenTempl.IsConst then
    exit(cGenericExact); // const param value type checked in GetSpecializedEl
  if ResolvedEl.BaseType=btContext then
    begin
    LoTypeEl:=ResolvedEl.LoTypeEl;
    if LoTypeEl is TPasGenericTemplateType then
      begin
      if LoTypeEl=GenTempl then
        exit(cGenericExact);
      if (Operation=prtcoAssignToTempl) and (ErrorPos<>nil) then
        CheckTemplateFitsTemplate(TPasGenericTemplateType(LoTypeEl),GenTempl,ErrorPos);
      Result:=cGenericExact;
      end
    else
      Result:=CheckTemplateFitsParam(LoTypeEl,GenTempl,nil,Operation,ErrorPos);
    end
  else if ResolvedEl.BaseType=btNil then
    begin
    for i:=0 to length(GenTempl.Constraints)-1 do
      begin
      ConEl:=GenTempl.Constraints[i];
      ConToken:=GetGenericConstraintKeyword(ConEl);
      if ConToken=tkrecord then
        begin
        if ErrorPos<>nil then
          RaiseXExpectedButYFound(20190915211000,'record type','nil',ErrorPos);
        exit(cIncompatible);
        end;
      end;
    Result:=cGenericExact;
    end
  else
    begin
    if ErrorPos<>nil then
      RaiseNotYetImplemented(20190915205441,ErrorPos);
    Result:=cIncompatible;
    end;
end;

procedure TPasResolver.CheckTemplateFitsTemplate(ParamTemplType,
  GenTempl: TPasGenericTemplateType; ErrorPos: TPasElement);

  procedure RaiseNotValidConstraint(const Id: TMaxPrecInt; ConEl: TPasElement);
  begin
    RaiseMsg(Id,nXIsNotAValidConstraint,sXIsNotAValidConstraint,
      [GetElementTypeName(ConEl)],GetGenericConstraintErrorEl(ConEl,GenTempl));
  end;

var
  ParamConstraints: TPasElementArray;
  j, k: Integer;
  ConToken: TToken;
  ConstraintClass, ParamClassType: TPasClassType;
  ConEl, ParamConstraintEl: TPasElement;
  ParamLoType, ParamHiType: TPasType;
  ResolvedEl: TPasResolverResult;
begin
  // A const generic param's "constraint" is its value TYPE (e.g. integer), not a
  // class/interface constraint. When a const param is forwarded to another const
  // param (generic TList<T;const U> ... specialize THelperA<U>), the value-type
  // compatibility is verified when the outer generic is specialized with a
  // concrete value, so the class-constraint fitting below does not apply.
  if GenTempl.IsConst or ParamTemplType.IsConst then
    exit;
  // Two type parameters belonging to DIFFERENT generics, neither of them bound
  // yet. That pairing only arises once a partial specialization answers with
  // the bare generic (PartialSpecsAsGeneric) - `inherited Pop` inside
  // TObjectStack<T> then returns TStack's own T. Whether the argument really
  // satisfies the constraint is settled, and checked, when the enclosing
  // generic is specialized with a real type.
  if FPartialSpecsAsGeneric and (GenTempl.Parent<>ParamTemplType.Parent) then
    exit;
  ParamConstraints:=ParamTemplType.Constraints;
  for j:=0 to length(GenTempl.Constraints)-1 do
    begin
    ConEl:=GenTempl.Constraints[j];
    ConToken:=GetGenericConstraintKeyword(ConEl);
    if ConToken<>tkEOF then
      begin
      // constraint is keyword
      // -> check if keyword is in ParamConstraints
      k:=length(ParamConstraints)-1;
      while (k>=0) and (GetGenericConstraintKeyword(ParamConstraints[k])<>ConToken) do
        dec(k);
      if k<0 then
        RaiseMsg(20190816230021,nTypeParamXIsMissingConstraintY,
          sTypeParamXIsMissingConstraintY,
          [ParamTemplType.Name,TokenInfos[ConToken]],ErrorPos);
      end
    else if ConEl is TPasType then
      begin
      // constraint is a type
      ComputeElement(ConEl,ResolvedEl,[rcType]);
      if ResolvedEl.BaseType<>btContext then
        RaiseNotValidConstraint(20190816231846,ConEl);
      if not (ResolvedEl.LoTypeEl is TPasClassType) then
        RaiseNotValidConstraint(20190816231849,ConEl);
      ConstraintClass:=TPasClassType(ResolvedEl.LoTypeEl);
      // constraint is class/interface type
      // -> check if one of ParamConstraints fits the constraint type
      // i.e. ParamConstraints must be more strict than target constraints
      k:=length(ParamConstraints)-1;
      while k>=0 do
        begin
        ParamConstraintEl:=ParamConstraints[k];
        if ParamConstraintEl is TPasType then
          begin
          ParamHiType:=TPasType(ParamConstraintEl);
          ParamLoType:=ResolveAliasType(ParamHiType);
          if not (ParamLoType is TPasClassType) then
            RaiseMsg(20190816232459,nXExpectedButYFound,sXExpectedButYFound,
              ['type',GetTypeDescription(ParamHiType)],
              GetGenericConstraintErrorEl(ParamConstraintEl,ParamTemplType));
          ParamClassType:=TPasClassType(ParamLoType);
          if (ConstraintClass.ObjKind=okInterface)
              and (ParamClassType.ObjKind=okClass) then
            begin
            if GetClassImplementsIntf(ParamClassType,ConstraintClass)<>nil then
              break;
            end
          else
            begin
            if CheckClassIsClass(ParamClassType,ConstraintClass)<cIncompatible then
              break;
            end;
          end;
        dec(k);
        end;
      if k<0 then
        begin
        if ConstraintClass.ObjKind=okInterface then
          RaiseMsg(20190816233102,nTypeParamXMustSupportIntfY,
            sTypeParamXMustSupportIntfY,
            [ParamTemplType.Name,GetTypeDescription(ConstraintClass)],ErrorPos)
        else
          RaiseMsg(20190816230021,nTypeParamXIsNotCompatibleWithY,
            sTypeParamXIsNotCompatibleWithY,
            [ParamTemplType.Name,GetTypeDescription(ConstraintClass)],ErrorPos);
        end;
      end
    else
      RaiseNotYetImplemented(20190912215702,GetGenericConstraintErrorEl(ConEl,GenTempl));
    end;
end;

function TPasResolver.CreateSpecializedItem(El: TPasElement;
  GenericEl: TPasElement; const ParamsResolved: TPasTypeArray;
  const AConstExprs: array of TPasExpr): TPRSpecializedItem;
var
  NewEl: TPasElement;
  GenScope: TPasGenericScope;
  SpecializedItems: TObjectList;

  procedure InsertBehind(List: TFPList);
  var
    Last: TPasElement;
    i, LastIndex: Integer;
    GenScope: TPasGenericScope;
    ProcScope: TPasProcedureScope;
  begin
    // insert in front of currently parsed elements
    // beware: specializing an element can create other specialized elements
    // add behind last finished specialized element of this GenericEl
    // for example: A = class(B<C<D>>)
    // =>
    //  D
    //  C<D>
    //  B<C<D>>
    //  A
    Last:=GenericEl;
    if SpecializedItems<>nil then
      begin
      i:=SpecializedItems.Count-2;
      if i>=0 then
        Last:=TPRSpecializedItem(SpecializedItems[i]).SpecializedEl;
      end;
    LastIndex:=List.IndexOf(Last);
    { LastIndex<0 means the reference element is not (yet) in the target list.
      This happens for an indirectly recursive generic type whose own generic
      declaration is not yet in the list when the first specialization is
      created — e.g.
      Wrapper<t1,t2> = record 
        f: ^Wrapper<t2,t1> 
      end. 
    }
    i:=List.Count-1;
    while i>LastIndex do
      begin
      Last:=TPasElement(List[i]);
      if Last is TPasGenericType then
        begin
        if (Last.CustomData is TPasGenericScope) then
          begin
          GenScope:=Last.CustomData as TPasGenericScope;
          if GenScope.GenericStep>=psgsInterfaceParsed then
            break; // finished generic type
          end;
        // type is still parsed => insert in front
        dec(i);
        end
      else if Last is TPasProcedure then
        begin
        // A procedure loaded from a precompiled unit has no scope: it is not
        // being parsed, so it counts as finished.
        if not (Last.CustomData is TPasProcedureScope) then
          break;
        ProcScope:=TPasProcedureScope(Last.CustomData);
        if ProcScope.GenericStep>=psgsInterfaceParsed then
          break; // finished generic proc
        // proc is still parsed => insert in front
        dec(i);
        end
      else
        break;
      end;
    List.Insert(i+1,NewEl);
  end;

var
  NewName: String;
  NewClass: TPTreeElement;
  SrcModule: TPasModule;
  SrcModuleScope: TPasModuleScope;
  SrcResolver: TPasResolver;
  NewParent: TPasElement;
  TypeItem: TPRSpecializedTypeItem;
  ProcItem: TPRSpecializedProcItem;
  i: Integer;
begin
  Result:=nil;

  SrcModule:=GenericEl.GetModule;
  SrcModuleScope:=SrcModule.CustomData as TPasModuleScope;
  SrcResolver:=SrcModuleScope.Owner as TPasResolver;
  if SrcResolver<>Self then
    RaiseInternalError(20190728121705);

  GenScope:=TPasGenericScope(GenericEl.CustomData);
  SpecializedItems:=GenScope.SpecializedItems;

  TypeItem:=nil;
  ProcItem:=nil;
  if GenericEl is TPasGenericType then
    begin
    TypeItem:=TPRSpecializedTypeItem.Create;
    Result:=TypeItem;
    end
  else if GenericEl is TPasProcedure then
    begin
    ProcItem:=TPRSpecializedProcItem.Create;
    Result:=ProcItem;
    end
  else
    RaiseNotYetImplemented(20190920140756,GenericEl);
  Result.GenericEl:=GenericEl;
  Result.FirstSpecialize:=El;
  Result.Params:=ParamsResolved;
  SetLength(Result.ConstExprs,length(AConstExprs));
  for i:=0 to length(AConstExprs)-1 do
    Result.ConstExprs[i]:=AConstExprs[i];
  Result.Index:=SpecializedItems.Count;
  SpecializedItems.Add(Result);
  NewName:=CreateSpecializedTypeName(Result);
  NewClass:=TPTreeElement(GenericEl.ClassType);
  NewParent:=GenericEl.Parent;
  NewEl:=TPasElement(CreateOwnedElement(NewClass,NewName,NewParent));
  if TypeItem<>nil then
    TypeItem.SpecializedType:=TPasGenericType(NewEl) // this calls AddRef
  else
    ProcItem.SpecializedProc:=TPasProcedure(NewEl); // this calls AddRef

  if NewParent is TPasDeclarations then
    begin
    InsertBehind(TPasDeclarations(NewParent).Declarations);
    end
  else if NewParent is TPasMembersType then
    begin
    InsertBehind(TPasMembersType(NewParent).Members);
    end;

  RegisterSpecializedItem(NewEl,Result);

  if GenScope.GenericStep>=psgsInterfaceParsed then
    SpecializeGenericIntf(Result);

  if GenScope.GenericStep>=psgsImplementationParsed then
    begin
    // Specializing a method body copies the generic's statements and
    // type-checks them, which is most of what a specialization costs: bringing
    // rtl-generics into scope creates ~9000 specializations and over a million
    // element copies, for a consumer that may call none of them. With
    // DeferSpecializedImpls the bodies are owed until something needs the CODE;
    // the interface built above is what callers are checked against.
    if FDeferSpecImpls and (FLoadedUnitDepth>0) then
      begin
      // Owed until something needs the code. Nothing drains this: a body built
      // because a batch closed would be built for a unit nobody calls into.
      Result.ImplOwed:=true;
      end
    else if (FBuildingSpecializations<>nil) and (FBuildingSpecializations.Count>0) then
      begin
      // Also owed while another specialization's INTERFACE is being built:
      // building a body then type-checks it against a half-built hierarchy -
      // rtl-generics' TList<X> is reached while its own ancestor TCustomList<X>
      // is in flight, and `TEnumerator.Create(Self)` cannot then see that
      // TList<X> IS a TCustomList<X>.
      if FPendingSpecImpls=nil then
        FPendingSpecImpls:=TFPList.Create;
      if FPendingSpecImpls.IndexOf(Result)<0 then
        FPendingSpecImpls.Add(Result);
      end
    else if HasUnboundForwardClassParam(Result) then
      begin
      // `specialize TFPGObjectList<TFoo>` right after `TFoo = class;`: the bodies
      // use TFoo's members, which only exist once the type section is finished.
      if FForwardSpecImpls=nil then
        FForwardSpecImpls:=TFPList.Create;
      if FForwardSpecImpls.IndexOf(Result)<0 then
        FForwardSpecImpls.Add(Result);
      end
    else
      SpecializeGenericImpl(Result);
    end;
end;

function TPasResolver.HasUnboundForwardClassParam(Item: TPRSpecializedItem
  ): boolean;
var
  i: Integer;
  ParamType: TPasType;
begin
  Result:=false;
  for i:=0 to length(Item.Params)-1 do
    begin
    ParamType:=ResolveAliasType(Item.Params[i]);
    if not (ParamType is TPasClassType) then continue;
    if TPasClassType(ParamType).IsForward then
      begin
      if not (ParamType.CustomData is TResolvedReference) then
        exit(true);
      // bound already, but the full class may still be in its ancestor list:
      // `TFoldData = class(specialize TGenNode<TFoldData>)` (SynEdit)
      ParamType:=ResolveAliasType(TPasType(TResolvedReference(ParamType.CustomData).Declaration));
      if not (ParamType is TPasClassType) then continue;
      end;
    if (TPasClassType(ParamType).ObjKind in [okClass,okObject])
        and not ((ParamType.CustomData is TPasClassScope)
                 and (pcsfAncestorResolved in TPasClassScope(ParamType.CustomData).Flags)) then
      exit(true);
    end;
end;

procedure TPasResolver.BuildForwardWaitingSpecImpls;
var
  i, j: Integer;
  Item: TPRSpecializedItem;
  SrcResolver: TPasResolver;
begin
  if FForwardSpecImpls=nil then exit;
  // Not from inside another specialization's body: that has its own scopes.
  if FSpecImplDepth>0 then exit;
  i:=0;
  while (FForwardSpecImpls<>nil) and (i<FForwardSpecImpls.Count) do
    begin
    Item:=TPRSpecializedItem(FForwardSpecImpls[i]);
    if HasUnboundForwardClassParam(Item) then
      inc(i)
    else
      begin
      FForwardSpecImpls.Delete(i);
      // The bodies see the full class, not its forward declaration: that has no
      // ancestor, and SynEdit's `T(ANode)` from the base node class to T failed.
      for j:=0 to length(Item.Params)-1 do
        if (Item.Params[j] is TPasClassType)
            and TPasClassType(Item.Params[j]).IsForward
            and (Item.Params[j].CustomData is TResolvedReference)
            and (TResolvedReference(Item.Params[j].CustomData).Declaration is TPasType) then
          Item.Params[j]:=TPasType(TResolvedReference(Item.Params[j].CustomData).Declaration);
      // Built by the resolver of the generic's module, which knows its helpers.
      SrcResolver:=GenericModuleResolver(Item);
      SrcResolver.SpecializeGenericImpl(Item);
      i:=0;
      end;
    end;
  if (FForwardSpecImpls<>nil) and (FForwardSpecImpls.Count=0) then
    FreeAndNil(FForwardSpecImpls);
end;

function TPasResolver.GenericModuleResolver(Item: TPRSpecializedItem): TPasResolver;
var
  Module: TPasModule;
begin
  Result:=nil;
  Module:=Item.GenericEl.GetModule;
  if (Module<>nil) and (Module.CustomData is TPasModuleScope)
      and (TPasModuleScope(Module.CustomData).Owner is TPasResolver) then
    Result:=TPasResolver(TPasModuleScope(Module.CustomData).Owner);
  if Result=nil then
    Result:=Self;
end;

function TPasResolver.CreateSpecializedTypeName(Item: TPRSpecializedItem): string;

  function Get_ProcName(aProc: TPasProcedure): string; forward;
  function GetTypeName(aType: TPasType): string; forward;

  function GetParentName(El: TPasElement): string;
  begin
    if El.Parent is TPasType then
      Result:=GetTypeName(TPasType(El.Parent))
    else if El is TPasUnresolvedSymbolRef then
      Result:='System'
    else if El.Parent is TPasProcedure then
      Result:=Get_ProcName(TPasProcedure(El.Parent))
    else
      Result:=El.GetModule.Name;
  end;

  function Get_ProcName(aProc: TPasProcedure): string;
  begin
    Result:=GetParentName(aProc);
    if aProc.Name<>'' then
      Result:=Result+'.'+aProc.Name;
  end;

  function GetConstValueStr(Expr: TPasExpr): string;
  var
    Val: TResEvalValue;
  begin
    Val:=Eval(Expr,[refConst]);
    try
      if Val=nil then
        Result:='?'
      else
        case Val.Kind of
          revkBool:
            if TResEvalBool(Val).B then Result:='True' else Result:='False';
          revkInt:
            Result:=IntToStr(TResEvalInt(Val).Int);
          revkUInt:
            Result:=IntToStr(TResEvalUInt(Val).UInt);
          revkFloat:
            Result:=FloatToStr(TResEvalFloat(Val).FloatValue);
          {$ifdef FPC_HAS_CPSTRING}
          revkString:
            Result:=''''+TResEvalString(Val).S+'''';
          {$endif}
          revkUnicodeString:
            Result:=''''+String(TResEvalUTF16(Val).S)+'''';
          revkEnum:
            Result:=TResEvalEnum(Val).GetEnumName;
        else
          Result:=Val.AsString;
        end;
    finally
      ReleaseEvalValue(Val);
    end;
  end;

  function GetSpecParams(Item: TPRSpecializedItem): string;
  var
    i: Integer;
  begin
    Result:='<';
    for i:=0 to length(Item.Params)-1 do
      begin
      if i>0 then Result:=Result+',';
      if (i<length(Item.ConstExprs)) and (Item.ConstExprs[i]<>nil) then
        Result:=Result+GetConstValueStr(Item.ConstExprs[i])
      else
        Result:=Result+GetTypeName(Item.Params[i]);
      end;
    Result:=Result+'>';
  end;

  function GetTypeName(aType: TPasType): string;
  var
    Arr: TPasArrayType;
    ElType: TPasType;
    ChildItem: TPRSpecializedItem;
  begin
    if aType.Name='' then
      begin
      if aType is TPasArrayType then
        begin
        // e.g. TBird<array of word>
        Result:='array of ';
        Arr:=TPasArrayType(aType);
        if length(Arr.Ranges)>0 then
          RaiseNotYetImplemented(20200905173026,Item.FirstSpecialize);
        ElType:=ResolveAliasType(Arr.ElType,false);
        if ElType is TPasArrayType then
          RaiseNotYetImplemented(20200905173159,Arr,'multidimensional anonymous array as generic param');
        Result:=Result+GetTypeName(ElType);
        end
      else
        RaiseNotYetImplemented(20200905173241,aType);
      end
    else
      begin
      Result:=GetParentName(aType)+'.'+aType.Name;
      if (aType.CustomData is TPasGenericScope) and (Pos('<',aType.Name)<1) then
        begin
        ChildItem:=TPasGenericScope(aType.CustomData).SpecializedFromItem;
        if ChildItem<>nil then
          Result:=Result+GetSpecParams(ChildItem);
        end;
      end;
  end;

begin
  if Pos('<',Item.GenericEl.Name)>0 then
    RaiseNotYetImplemented(20201203140102,Item.SpecializedEl,Item.GenericEl.Name);

  Result:=Item.GenericEl.Name+GetSpecParams(Item);

  if Pos('><',Result)>0 then
    RaiseNotYetImplemented(20201203140223,Item.SpecializedEl,Result);
end;

function TPasResolver.CreateConstExprForSpecParam(OrigExpr: TPasExpr;
  AParent: TPasElement): TPasExpr;
// Evaluate OrigExpr and create a new self-contained expression node
// that the evaluator can process without scope lookups.
var
  Val: TResEvalValue;
  SetExpr: TParamsExpr;
  Elem, LeftExpr, RightExpr: TPrimitiveExpr;
  RangeExpr: TBinaryExpr;
  RStart, REnd: TMaxPrecInt;
  i: Integer;
begin
  Result:=nil;
  Val:=Eval(OrigExpr,[refConst]);
  try
    if Val=nil then
      RaiseNotYetImplemented(20260304100001,OrigExpr,'cannot evaluate const generic param');
    case Val.Kind of
      revkBool:
        Result:=TBoolConstExpr.Create(AParent,pekBoolConst,TResEvalBool(Val).B);
      revkInt:
        Result:=TPrimitiveExpr.Create(AParent,pekNumber,IntToStr(TResEvalInt(Val).Int));
      revkUInt:
        Result:=TPrimitiveExpr.Create(AParent,pekNumber,IntToStr(TResEvalUInt(Val).UInt));
      revkFloat:
        Result:=TPrimitiveExpr.Create(AParent,pekNumber,FloatToStr(TResEvalFloat(Val).FloatValue));
      {$ifdef FPC_HAS_CPSTRING}
      revkString:
        Result:=TPrimitiveExpr.Create(AParent,pekString,''''+TResEvalString(Val).S+'''');
      {$endif}
      revkUnicodeString:
        Result:=TPrimitiveExpr.Create(AParent,pekString,''''+String(TResEvalUTF16(Val).S)+'''');
      revkEnum:
        Result:=TPrimitiveExpr.Create(AParent,pekNumber,IntToStr(TResEvalEnum(Val).Index));
      revkNil:
        Result:=TNilExpr.Create(AParent);
      revkSetOfInt:
        begin
        SetExpr:=TParamsExpr.Create(AParent,pekSet);
        try
          for i:=0 to length(TResEvalSet(Val).Ranges)-1 do
            begin
            RStart:=TResEvalSet(Val).Ranges[i].RangeStart;
            REnd:=TResEvalSet(Val).Ranges[i].RangeEnd;
            if RStart=REnd then
              begin
              // Single element
              case TResEvalSet(Val).ElKind of
                revskChar:
                  Elem:=TPrimitiveExpr.Create(SetExpr,pekString,
                    ''''+Chr(RStart)+'''');
              else
                Elem:=TPrimitiveExpr.Create(SetExpr,pekNumber,
                  IntToStr(RStart));
              end;
              SetExpr.AddParam(Elem);
              end
            else
              begin
              // Range: create left..right
              case TResEvalSet(Val).ElKind of
                revskChar:
                  begin
                  LeftExpr:=TPrimitiveExpr.Create(SetExpr,pekString,
                    ''''+Chr(RStart)+'''');
                  RightExpr:=TPrimitiveExpr.Create(SetExpr,pekString,
                    ''''+Chr(REnd)+'''');
                  end;
              else
                begin
                LeftExpr:=TPrimitiveExpr.Create(SetExpr,pekNumber,
                  IntToStr(RStart));
                RightExpr:=TPrimitiveExpr.Create(SetExpr,pekNumber,
                  IntToStr(REnd));
                end;
              end;
              RangeExpr:=TBinaryExpr.CreateRange(SetExpr,LeftExpr,RightExpr);
              SetExpr.AddParam(RangeExpr);
              end;
            end;
        except
          SetExpr.Free;
          raise;
        end;
        Result:=SetExpr;
        end;
    else
      RaiseNotYetImplemented(20260304100002,OrigExpr,'const generic param kind '+Val.TypeAsString);
    end;
  finally
    ReleaseEvalValue(Val);
  end;
end;

procedure TPasResolver.InitSpecializeScopes(El: TPasElement; out
  State: TScopeStashState);

  function PushParentScopes(CurEl: TPasElement): integer;
  var
    Keep: Integer;
    Scope: TPasScope;
    IntfSection: TInterfaceSection;
  begin
    {$IFDEF VerboseInitSpecializeScopes}
    writeln('  PushParentScopes START ',GetObjName(CurEl));
    {$ENDIF}
    if CurEl=nil then
      RaiseInternalError(20190728125025);
    if CurEl is TPasModule then
      begin
      if not (CurEl.CustomData is TPasModuleScope) then
        RaiseNotYetImplemented(20190728142609,El,GetObjName(CurEl)+' '+GetObjName(CurEl.CustomData));
      Keep:=0;
      if FScopeCount<=Keep then
        RaiseInternalError(20190728124857);
      if not (FScopes[Keep] is TPasDefaultScope) then
        RaiseInternalError(20190728124858);
      end
    else
      begin
      if CurEl.Parent=nil then
        RaiseInternalError(20190728130238,GetObjName(CurEl));
      if CurEl.CustomData=nil then
        exit(PushParentScopes(CurEl.Parent));
      if not (CurEl.CustomData is TPasIdentifierScope) then
        RaiseNotYetImplemented(20190728131934,El,GetObjName(CurEl)+' '+GetObjName(CurEl.CustomData));
      Keep:=PushParentScopes(CurEl.Parent);
      end;
    inc(Keep);
    Scope:=TPasScope(CurEl.CustomData);
    {$IFDEF VerboseInitSpecializeScopes}
    writeln('  PushParentScopes ',GetObjName(CurEl),' Scope=',GetObjName(Scope),' Keep=',Keep);
    {$ENDIF}
    if Scope.FreeOnPop then
      RaiseInternalError(20190728131153,GetObjName(CurEl));
    if (Keep<FScopeCount) and (FScopes[Keep]=Scope) then
      // Scope is already on the scopestack
    else
      begin
      if Keep<FScopeCount then
        begin
        // cannot use current scope stack -> stash
        {$IFDEF VerboseInitSpecializeScopes}
        writeln('  PushParentScopes StashScopes Keep=',Keep);
        {$ENDIF}
        StashScopes(Keep);
        if Keep<>FScopeCount then
          RaiseNotYetImplemented(20190813005130,El);
        State.ScopeCount:=ScopeCount;
        end;
      if (CurEl.ClassType=TImplementationSection) then
        begin
        // unit implementation -> push interface scope
        IntfSection:=CurEl.GetModule.InterfaceSection;
        if IntfSection=nil then
          RaiseNotYetImplemented(20190825112907,CurEl);
        if not (IntfSection.CustomData is TPasSectionScope) then
          RaiseNotYetImplemented(20190825112907,CurEl);
        PushScope(TPasSectionScope(IntfSection.CustomData));
        inc(Keep);
        end;
      PushScope(Scope);
      end;
    exit(Keep);
  end;

var
  Keep: Integer;
begin
  {$IFDEF VerboseInitSpecializeScopes}
  writeln('TPasResolver.InitSpecializeScopes START ',GetObjName(El));
  {$ENDIF}
  State.ScopeCount:=ScopeCount;
  State.StashCount:=FStashScopeCount;
  Keep:=PushParentScopes(El.Parent)+1;
  if Keep<FScopeCount then
    begin
    // cannot use current scope stack -> stash
    {$IFDEF VerboseInitSpecializeScopes}
    writeln('TPasResolver.InitSpecializeScopes StashScopes Keep=',Keep);
    {$ENDIF}
    StashScopes(Keep);
    if Keep<>FScopeCount then
      RaiseNotYetImplemented(20190813005859,El);
    end;
  {$IFDEF VerboseInitSpecializeScopes}
  WriteScopesShort('TPasResolver.InitSpecializeScopes END');
  {$ENDIF}
end;

procedure TPasResolver.RestoreSpecializeScopes(const State: TScopeStashState);
begin
  while ScopeCount>State.ScopeCount do
    PopScope;
  RestoreStashedScopes(State.StashCount);
end;

procedure TPasResolver.SpecializeGenericIntf(SpecializedItem: TPRSpecializedItem
  );
var
  SpecEl, GenericEl: TPasElement;
  C: TClass;
  NewRecordType, GenRecordType: TPasRecordType;
  NewClassType, GenClassType: TPasClassType;
  NewArrayType, GenArrayType: TPasArrayType;
  GenProcType, NewProcType: TPasProcedureType;
  GenProc, NewProc: TPasProcedure;
  OldScopeState: TScopeStashState;
  PendingItem: TPRSpecializedItem;
begin
  if SpecializedItem.Step<>prssNone then
    exit;
  SpecializedItem.Step:=prssInterfaceBuilding;
  SpecEl:=SpecializedItem.SpecializedEl;
  GenericEl:=SpecializedItem.GenericEl;
  if FBuildingSpecializations=nil then
    FBuildingSpecializations:=TFPList.Create;
  FBuildingSpecializations.Add(SpecEl);
  FBuildingSpecializations.Add(GenericEl);
  try

  // change scope
  InitSpecializeScopes(GenericEl,OldScopeState);
  {$IFDEF VerbosePasResolver}
  WriteScopesShort('TPasResolver.SpecializeGenericIntf Init SpecEl='+SpecEl.FullName+' GenericEl='+GenericEl.FullName);
  {$ENDIF}

  SpecializePasElementProperties(GenericEl,SpecEl);

  C:=SpecEl.ClassType;
  if C=TPasRecordType then
    begin
    NewRecordType:=TPasRecordType(SpecEl);
    GenRecordType:=TPasRecordType(GenericEl);
    SpecializeRecordType(GenRecordType,NewRecordType,TPRSpecializedTypeItem(SpecializedItem));
    end
  else if C=TPasClassType then
    begin
    NewClassType:=TPasClassType(SpecEl);
    GenClassType:=TPasClassType(GenericEl);
    SpecializeClassType(GenClassType,NewClassType,TPRSpecializedTypeItem(SpecializedItem));
    end
  else if C=TPasArrayType then
    begin
    GenArrayType:=TPasArrayType(GenericEl);
    NewArrayType:=TPasArrayType(SpecEl);
    SpecializeArrayType(GenArrayType,NewArrayType,TPRSpecializedTypeItem(SpecializedItem));
    end
  else if (C=TPasProcedureType)
      or (C=TPasFunctionType) then
    begin
    GenProcType:=TPasProcedureType(GenericEl);
    NewProcType:=TPasProcedureType(SpecEl);
    SpecializeProcedureType(GenProcType,NewProcType,TPRSpecializedTypeItem(SpecializedItem));
    end
  else if C.InheritsFrom(TPasProcedure) then
    begin
    GenProc:=TPasProcedure(GenericEl);
    NewProc:=TPasProcedure(SpecEl);
    SpecializeProcedure(GenProc,NewProc,SpecializedItem);
    end
  else
    RaiseNotYetImplemented(20190728134933,GenericEl);

  // A top-level generic type is specialized here, NOT via SpecializeElement, so
  // propagate the template's pack values onto the specialized type now that its
  // scope exists (SpecializeRecordType/etc. above created it).
  SpecializePackValues(GenericEl,SpecEl);

  {$IFDEF VerbosePasResolver}
  WriteScopesShort('TPasResolver.SpecializeGenericIntf Finish: '+SpecEl.FullName);
  {$ENDIF}

  RestoreSpecializeScopes(OldScopeState);
  {$IFDEF VerbosePasResolver}
  WriteScopesShort('TPasResolver.SpecializeGenericIntf RestoreStashedScopes: '+SpecEl.FullName);
  {$ENDIF}
  finally
    FBuildingSpecializations.Delete(FBuildingSpecializations.Count-1);
    FBuildingSpecializations.Delete(FBuildingSpecializations.Count-1);
    // The cascade is complete: the bodies owed by specializations created
    // inside it can be built now, against a finished hierarchy.
    if FBuildingSpecializations.Count=0 then
      BuildOwedSpecializedImpls;
  end;
end;

procedure TPasResolver.BeginSpecializationBatch;
// Two entries, because the interface build pops them in pairs.
begin
  if FBuildingSpecializations=nil then
    FBuildingSpecializations:=TFPList.Create;
  FBuildingSpecializations.Add(nil);
  FBuildingSpecializations.Add(nil);
end;


procedure TPasResolver.EndSpecializationBatch;
begin
  if (FBuildingSpecializations=nil) or (FBuildingSpecializations.Count<2) then
    exit;
  FBuildingSpecializations.Delete(FBuildingSpecializations.Count-1);
  FBuildingSpecializations.Delete(FBuildingSpecializations.Count-1);
  if FBuildingSpecializations.Count=0 then
    BuildOwedSpecializedImpls;
end;


function SpecializedItemIsPartial(Item: TPRSpecializedItem): boolean;
// Whether any of a specialization's arguments is still a type PARAMETER, or is
// declared inside something that is.

  function IsOpen(El: TPasElement; Depth: integer): boolean;
  var
    P: TPasElement;
    Sub: TPRSpecializedItem;
    i: Integer;
  begin
    Result:=false;
    if (El=nil) or (Depth>8) then exit;
    P:=El;
    while P<>nil do
      begin
      if P is TPasGenericTemplateType then exit(true);
      if P.CustomData is TPasGenericScope then
        begin
        Sub:=TPasGenericScope(P.CustomData).SpecializedFromItem;
        if Sub<>nil then
          for i:=0 to length(Sub.Params)-1 do
            if IsOpen(Sub.Params[i],Depth+1) then exit(true);
        end;
      P:=P.Parent;
      end;
  end;

var
  i: Integer;
begin
  Result:=false;
  if Item=nil then exit;
  for i:=0 to length(Item.Params)-1 do
    if IsOpen(Item.Params[i],0) then exit(true);
end;


procedure TPasResolver.BeginLoadedUnit;
begin
  inc(FLoadedUnitDepth);
end;

procedure TPasResolver.EndLoadedUnit;
begin
  if FLoadedUnitDepth>0 then
    dec(FLoadedUnitDepth);
end;

procedure TPasResolver.EnsureSpecializedImpl(El: TPasElement);
// Build the BODIES of the specialization El belongs to, if they are still owed.
// Called when something needs the CODE - the code generator's demand walk -
// rather than when the specialization was created. Building one can create
// more specializations, and those are owed in their turn.
var
  Item: TPRSpecializedItem;
  Scope: TPasGenericScope;
  SrcResolver: TPasResolver;
begin
  if (El=nil) or not (El.CustomData is TPasGenericScope) then exit;
  Scope:=TPasGenericScope(El.CustomData);
  Item:=Scope.SpecializedFromItem;
  if Item=nil then exit;
  // Only what is actually OWED. A specialization that was never queued is one
  // that would not have been built eagerly either - a generic INTERFACE has no
  // method bodies, and its scope never reaches psgsImplementationParsed, so
  // asking to build it raises "not yet implemented".
  if not Item.ImplOwed then exit;
  Item.ImplOwed:=false;
  // Built by the resolver of the generic's module, which knows its helpers.
  SrcResolver:=GenericModuleResolver(Item);
  if FPendingSpecImpls<>nil then
    FPendingSpecImpls.Remove(Item);
  if (SrcResolver<>Self) and (SrcResolver.FPendingSpecImpls<>nil) then
    SrcResolver.FPendingSpecImpls.Remove(Item);
  SrcResolver.SpecializeGenericImpl(Item);
end;

procedure TPasResolver.BuildOwedSpecializedImpls;
// Every body still owed. Called when a specialization cascade finishes, and as
// the backstop for a path that needs code without having gone through a demand
// walk. Does nothing while the bodies are deliberately owed.
var
  Item: TPRSpecializedItem;
begin
  while (FPendingSpecImpls<>nil) and (FPendingSpecImpls.Count>0) do
    begin
    Item:=TPRSpecializedItem(FPendingSpecImpls[0]);
    FPendingSpecImpls.Delete(0);
    Item.ImplOwed:=false;
    if HasUnboundForwardClassParam(Item) then
      begin
      // Waits for the class it is specialized with, as in CreateSpecializedItem.
      if FForwardSpecImpls=nil then
        FForwardSpecImpls:=TFPList.Create;
      if FForwardSpecImpls.IndexOf(Item)<0 then
        FForwardSpecImpls.Add(Item);
      end
    else
      SpecializeGenericImpl(Item);
    end;
end;

procedure TPasResolver.SpecializeGenericImpl(SpecializedItem: TPRSpecializedItem
  );
var
  GenericEl: TPasElement;
  GenScope: TPasGenericScope;
  SpecializedTypeItem: TPRSpecializedTypeItem;
  SpecializedProcItem: TPRSpecializedProcItem;
  GenImplProc, GenIntfProc, SpecDeclProc: TPasProcedure;
  GenDeclProcScope: TPasProcedureScope;
  OldScopeState: TScopeStashState;
begin
  // check specialized type step
  if SpecializedItem.Step>prssInterfaceFinished then
    exit;
  // A body may not be built while another specialization's interface is still
  // being built, nor while a precompiled unit is being read - the generic's own
  // bodies are not all there yet. Owe it until the outermost build is done.
  if (FBuildingSpecializations<>nil) and (FBuildingSpecializations.Count>0) then
    begin
    if FPendingSpecImpls=nil then
      FPendingSpecImpls:=TFPList.Create;
    if FPendingSpecImpls.IndexOf(SpecializedItem)<0 then
      FPendingSpecImpls.Add(SpecializedItem);
    exit;
    end;
  // Bodies cannot be built before the interface: an owed implementation can be
  // drained while its own interface was never started.
  if SpecializedItem.Step=prssNone then
    begin
    SpecializeGenericIntf(SpecializedItem);
    if SpecializedItem.Step>prssInterfaceFinished then
      exit;
    end;
  GenericEl:=SpecializedItem.GenericEl;
  if SpecializedItem.Step<prssInterfaceFinished then
    if GenericEl is TPasType then
      RaiseMsg(20190804120128,nTypeXIsNotYetCompletelyDefined,sTypeXIsNotYetCompletelyDefined,
        [GetTypeDescription(TPasType(GenericEl))],SpecializedItem.FirstSpecialize)
    else
      RaiseMsg(20190920190727,nTypeXIsNotYetCompletelyDefined,sTypeXIsNotYetCompletelyDefined,
        [GenericEl.Name],SpecializedItem.FirstSpecialize);
  SpecializedItem.Step:=prssImplementationBuilding;

  inc(FSpecImplDepth);
  try
  // check generic type is resolved completely
  GenScope:=TPasGenericScope(GenericEl.CustomData);
  if GenScope.GenericStep<psgsImplementationParsed then
    RaiseNotYetImplemented(20190804174019,GenericEl,GetObjName(SpecializedItem.SpecializedEl));

  if GenericEl is TPasMembersType then
    begin
    // specialize all method bodies
    SpecializedTypeItem:=TPRSpecializedTypeItem(SpecializedItem);
    if SpecializedTypeItem.ImplProcs=nil then
      SpecializedTypeItem.ImplProcs:=TFPList.Create;
    SpecializeMembersImpl(TPasMembersType(GenericEl),
      TPasMembersType(SpecializedTypeItem.SpecializedType),
      SpecializedTypeItem);
    end
  else if GenericEl is TPasProcedure then
    begin
    // specialize proc implementation
    GenIntfProc:=TPasProcedure(GenericEl);
    if GenIntfProc.IsAbstract or GenIntfProc.IsExternal then
      //
    else
      begin
      SpecializedProcItem:=TPRSpecializedProcItem(SpecializedItem);
      GenDeclProcScope:=TPasProcedureScope(GenScope);
      GenImplProc:=GenDeclProcScope.ImplProc;
      // No implementation to copy: the template came from a precompiled unit
      // that could not hold this body. The specialized routine then has a
      // declaration and no code, which the linker reports if it is ever called -
      // far better than refusing to specialize at all, which stops the consumer
      // from compiling even what IS complete. Same rule as for a method of a
      // specialized class.
      if (GenImplProc=nil) or (GenImplProc.Body=nil) then
        exit;
      SpecDeclProc:=SpecializedProcItem.SpecializedProc;

      InitSpecializeScopes(GenImplProc,OldScopeState);
      SpecializeGenImplProc(GenIntfProc,SpecDeclProc,SpecializedProcItem);
      RestoreSpecializeScopes(OldScopeState);
      end;
    end;

  SpecializedItem.Step:=prssImplementationFinished;
  finally
    dec(FSpecImplDepth);
  end;
end;

procedure TPasResolver.SpecializeMembers(GenMembersType,
  SpecMembersType: TPasMembersType);
var
  i: Integer;
  GenEl, NewEl: TPasElement;
  NewClass: TPTreeElement;
begin
  // Phase 1: Create all member elements first, so that forward references
  // (e.g. PListItem = ^TListItem where TListItem is declared after) can be
  // resolved during Phase 2 by searching the members list.
  if SpecMembersType.Members.Count>0 then
    RaiseNotYetImplemented(20260331120001,SpecMembersType,'Members not empty');
  for i:=0 to GenMembersType.Members.Count-1 do
    begin
    GenEl:=TPasElement(GenMembersType.Members[i]);
    if GenEl.Parent<>GenMembersType then
      RaiseNotYetImplemented(20190728145634,GenEl,GetObjName(GenEl.Parent));
    NewClass:=TPTreeElement(GenEl.ClassType);
    NewEl:=TPasElement(CreateOwnedElement(NewClass,GenEl.Name,SpecMembersType));
    SpecMembersType.Members.Add(NewEl);
    end;
  // Phase 2: Specialize each element (all siblings already exist in Members)
  for i:=0 to GenMembersType.Members.Count-1 do
    begin
    GenEl:=TPasElement(GenMembersType.Members[i]);
    NewEl:=TPasElement(SpecMembersType.Members[i]);
    SpecializeElement(GenEl,NewEl);
    end;
end;

procedure TPasResolver.SpecializeMembersImpl(GenericType,
  SpecType: TPasMembersType; SpecializedItem: TPRSpecializedTypeItem);
var
  GenClassOrRec, SpecClassOrRec: TPasMembersType;
  i: Integer;
  GenMember, SpecMember, ImplParent: TPasElement;
  GenIntfProc, GenImplProc, SpecIntfProc: TPasProcedure;
  GenIntfProcScope: TPasProcedureScope;
  OldScopeState: TScopeStashState;
begin
  GenClassOrRec:=TPasMembersType(GenericType);
  SpecClassOrRec:=TPasMembersType(SpecType);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.SpecializeMembersImpl RestoreStashedScopes ',GetObjPath(SpecClassOrRec),' ',ScopeCount,' FStashScopeCount=',FStashScopeCount);
  {$ENDIF}

  // specialize member bodies
  ImplParent:=nil;
  OldScopeState:=default(TScopeStashState);
  for i:=0 to GenClassOrRec.Members.Count-1 do
    begin
    GenMember:=TPasElement(GenClassOrRec.Members[i]);
    SpecMember:=TPasElement(SpecClassOrRec.Members[i]);
    if SpecMember.ClassType<>GenMember.ClassType then
      RaiseNotYetImplemented(20190816002658,GenMember,GetObjName(SpecMember));
    if SpecMember.Name<>GenMember.Name then
      RaiseNotYetImplemented(20190804124220,GenMember,GetObjName(SpecMember));
    if GenMember is TPasProcedure then
      begin
      GenIntfProc:=TPasProcedure(GenMember);
      SpecIntfProc:=TPasProcedure(SpecMember);
      if GenIntfProc.IsAbstract or GenIntfProc.IsExternal then continue;
      GenIntfProcScope:=TPasProcedureScope(GenIntfProc.CustomData);
      GenImplProc:=GenIntfProcScope.ImplProc;
      // No implementation to copy: the template came from a precompiled unit
      // that could not hold this body. The specialized method then has a
      // declaration and no code, which the linker reports if it is ever called -
      // far better than refusing to specialize the type at all, which stops the
      // consumer from compiling even the members that ARE complete.
      if GenImplProc=nil then continue;
      if ImplParent=nil then
        begin
        // switch scope (e.g. unit implementation section)
        ImplParent:=GenImplProc.Parent;
        InitSpecializeScopes(GenImplProc,OldScopeState);
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.SpecializeGenImplProc Specialize implprocs: SpecType=',GetObjName(SpecType),' ImplParent=',GetObjName(ImplParent),' ScopeCount=',ScopeCount,' FStashScopeCount=',FStashScopeCount,' TopScope=',GetObjName(TopScope));
        {$ENDIF}
        end
      else if ImplParent<>GenImplProc.Parent then
        RaiseNotYetImplemented(20190804130322,GenImplProc,GetObjName(ImplParent));

      SpecializeGenImplProc(GenIntfProc,SpecIntfProc,SpecializedItem);
      end
    else if GenMember is TPasMembersType then
      begin
      // nested record/class type
      SpecializeMembersImpl(TPasMembersType(GenMember),TPasMembersType(SpecMember),
                            SpecializedItem);
      end;
    end;
  if ImplParent<>nil then
    begin
    // restore scope
    RestoreSpecializeScopes(OldScopeState);
    end;
end;

procedure TPasResolver.SpecializeGenImplProc(GenDeclProc,
  SpecDeclProc: TPasProcedure; SpecializedItem: TPRSpecializedItem);

  procedure InsertBehind(ParentElList: TFPList;
    SpecializedItems: TObjectList; GenImplProc, SpecImplProc: TPasProcedure);
  // insert SpecImplProc behind last specialized impl proc
  // Note: impl procs are not always specialized in order
  var
    Last: TPasProcedure;
    i: Integer;
  begin
    Last:=nil;

    if SpecializedItems<>nil then
      begin
      i:=SpecializedItems.Count-1;
      while i>=0 do
        begin
        Last:=TPRSpecializedProcItem(SpecializedItems[i]).ImplProc;
        if Last=SpecImplProc then
          Last:=nil
        else if Last<>nil then
          break;
        dec(i);
        end;
      end;
    if Last=nil then
      Last:=GenImplProc;
    i:=ParentElList.IndexOf(Last);
    if i<0 then
      begin
      {$IF defined(VerbosePasResolver) or defined(VerbosePas2JS)}
      {AllowWriteln}
      writeln('InsertBehind GenImplProc=',GetObjPath(GenImplProc),' Last=',GetObjPath(Last));
      for i:=0 to ParentElList.Count-1 do
        begin
        writeln('  ',GetObjName(TObject(ParentElList[i])));
        if TObject(ParentElList[i]) is TPasProcedure then
          writeln('    IsForward=',TPasProcedure(ParentElList[i]).IsForward);
        end;
      {AllowWriteln-}
      {$ENDIF}
      RaiseNotYetImplemented(20191017122900,GenDeclProc);
      end;
    ParentElList.Insert(i+1,SpecImplProc);
  end;

var
  GenDeclProcScope, GenImplProcScope, SpecDeclProcScope,
    SpecImplProcScope: TPasProcedureScope;
  GenImplProc, SpecImplProc: TPasProcedure;
  NewClass: TPTreeElement;
  SpecClassOrRec, GenClassOrRec: TPasMembersType;
  SpecClassOrRecScope: TPasClassOrRecordScope;
  NewImplProcName, OldClassname: String;
  p, LastDotP: Integer;
  SpecializedProcItem: TPRSpecializedProcItem;
  SpecializedTypeItem: TPRSpecializedTypeItem;
  Templates: TFPList;
  NewParent: TPasElement;
  SavedInSpecialize: Boolean;
begin
  SavedInSpecialize:=FInSpecialize;
  FInSpecialize:=true;
  try
  SpecializedProcItem:=nil;
  SpecializedTypeItem:=nil;
  if SpecializedItem is TPRSpecializedProcItem then
    // impl proc of a specialized forward proc
    SpecializedProcItem:=TPRSpecializedProcItem(SpecializedItem)
  else if SpecializedItem is TPRSpecializedTypeItem then
    // method of a specialized class/record
    SpecializedTypeItem:=TPRSpecializedTypeItem(SpecializedItem)
  else
    RaiseNotYetImplemented(20190922145050,SpecDeclProc);

  GenDeclProcScope:=TPasProcedureScope(GenDeclProc.CustomData);
  GenImplProc:=GenDeclProcScope.ImplProc;
  if GenImplProc=nil then
    RaiseNotYetImplemented(20190804122134,GenDeclProc);
  if GenImplProc.Body=nil then
    RaiseNotYetImplemented(20190921220216,GenImplProc);
  GenImplProcScope:=TPasProcedureScope(GenImplProc.CustomData);
  SpecDeclProcScope:=TPasProcedureScope(SpecDeclProc.CustomData);
  if SpecDeclProc.Parent is TPasMembersType then
    begin
    SpecClassOrRec:=SpecDeclProc.Parent as TPasMembersType;
    SpecClassOrRecScope:=SpecClassOrRec.CustomData as TPasClassOrRecordScope;
    end
  else
    begin
    SpecClassOrRec:=nil;
    SpecClassOrRecScope:=nil;
    end;

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.SpecializeGenImplProc Specialize GenImplProc: ',GetObjName(GenImplProc));
  {$ENDIF}

  // create impl proc name
  if SpecializedTypeItem<>nil then
    begin
    // method of a specialized class/record
    if SpecClassOrRecScope=nil then
      RaiseNotYetImplemented(20190921221839,SpecDeclProc);
    NewImplProcName:=GenImplProc.Name;
    LastDotP:=GetLastDotPos(NewImplProcName);
    if LastDotP<1 then
      RaiseNotYetImplemented(20190921221730,GenImplProc);
    // has classname -> replace generic classname with specialized classname
    p:=LastDotP;
    while (p>1) and (NewImplProcName[p-1]<>'.') do dec(p);
    OldClassname:=copy(NewImplProcName,p,LastDotP-p);
    GenClassOrRec:=GenDeclProc.Parent as TPasMembersType;
    if not SameText(OldClassname,GenClassOrRec.Name) then
      RaiseNotYetImplemented(20190814141833,GenImplProc);
    NewImplProcName:=LeftStr(NewImplProcName,p-1)+SpecClassOrRec.Name+copy(NewImplProcName,LastDotP,length(NewImplProcName));
    end
  else
    begin
    // use classname of GenImplProc and name of SpecDeclProc
    OldClassname:=GenImplProc.Name;
    p:=GetLastDotPos(OldClassname);
    if p>0 then
      NewImplProcName:=LeftStr(OldClassname,p)+SpecDeclProc.Name
    else
      NewImplProcName:=SpecDeclProc.Name;
    end;

  // create impl proc
  NewClass:=TPTreeElement(GenImplProc.ClassType);
  NewParent:=GenImplProc.Parent;
  SpecImplProc:=TPasProcedure(CreateOwnedElement(NewClass,NewImplProcName,NewParent));
  SpecDeclProcScope.ImplProc:=SpecImplProc;
  if SpecializedProcItem<>nil then
    SpecializedProcItem.ImplProc:=SpecImplProc
  else
    SpecializedTypeItem.ImplProcs.Add(SpecImplProc);

  if (SpecializedProcItem<>nil) and (NewParent is TPasDeclarations) then
    InsertBehind(TPasDeclarations(NewParent).Declarations,
                 GenDeclProcScope.SpecializedItems,GenImplProc,SpecImplProc);

  // create impl proc scope
  SpecImplProcScope:=TPasProcedureScope(CreateScope(SpecImplProc,FScopeClass_Proc));
  SpecImplProcScope.Flags:=[ppsfIsSpecialized];
  SpecImplProcScope.DeclarationProc:=SpecDeclProc;
  SpecImplProcScope.ModeSwitches:=GenImplProcScope.Modeswitches;
  SpecImplProcScope.BoolSwitches:=GenImplProcScope.BoolSwitches;
  SpecImplProcScope.VisibilityContext:=SpecClassOrRec;
  SpecImplProcScope.ClassRecScope:=SpecClassOrRecScope;

  if SpecializedProcItem<>nil then
    begin
    Templates:=GetProcTemplateTypes(GenDeclProc);
    AddSpecializedTemplateIdentifiers(Templates,SpecializedItem,SpecImplProcScope,
      false);
    end;

  // specialize props
  SpecializePasElementProperties(GenImplProc,SpecImplProc);
  AddProcedure(SpecImplProc,nil);
  // This path specializes the implementation proc via SpecializeProcedure
  // directly (not SpecializeElement->SpecializeOperator), so an operator's
  // OperatorType/TokenBased must be copied here too, otherwise the specialized
  // operator impl body keeps OperatorType=otUnknown.
  if GenImplProc is TPasOperator then
    begin
    TPasOperator(SpecImplProc).OperatorType:=TPasOperator(GenImplProc).OperatorType;
    TPasOperator(SpecImplProc).TokenBased:=TPasOperator(GenImplProc).TokenBased;
    end;
  SpecializeProcedure(GenImplProc,SpecImplProc,SpecializedItem);
  finally
    FInSpecialize:=SavedInSpecialize;
  end;
end;


procedure TPasResolver.SpecializeElement(GenEl, SpecEl: TPasElement);
var
  C: TClass;
begin
  // first copy source filename and linenumber needed by error messages
  SpecializePasElementProperties(GenEl,SpecEl);

  (*
     propagate the {$R+}/{$Q+} state flags from the generic template. 
    These live on TPasElement.States (always present), so they can be copied up-front. 
    The {$MINENUMSIZE}/{$PACKSET}/{$PACKRECORDS} values live on the specialized
    type's scope/resolve-data, which is only created by the type-specific
    handling below, so they are propagated at the end (see bottom of method). 
  *)
  if IsRangeChecked(GenEl) then
    MarkRangeChecked(SpecEl);
  if IsOverflowChecked(GenEl) then
    MarkOverflowChecked(SpecEl);

  C:=GenEl.ClassType;
  // expressions
  if C=TPrimitiveExpr then
    SpecializePrimitiveExpr(TPrimitiveExpr(GenEl),TPrimitiveExpr(SpecEl))
  else if C=TUnaryExpr then
    SpecializeUnaryExpr(TUnaryExpr(GenEl),TUnaryExpr(SpecEl))
  else if C=TBinaryExpr then
    SpecializeBinaryExpr(TBinaryExpr(GenEl),TBinaryExpr(SpecEl))
  else if C=TBoolConstExpr then
    SpecializeBoolConstExpr(TBoolConstExpr(GenEl),TBoolConstExpr(SpecEl))
  else if C=TNilExpr then
    SpecializeExpr(TNilExpr(GenEl),TNilExpr(SpecEl))
  else if C=TInheritedExpr then
    SpecializeExpr(TInheritedExpr(GenEl),TInheritedExpr(SpecEl))
  else if C=TParamsExpr then
    SpecializeParamsExpr(TParamsExpr(GenEl),TParamsExpr(SpecEl))
  else if C=TRecordValues then
    SpecializeRecordValues(TRecordValues(GenEl),TRecordValues(SpecEl))
  else if C=TArrayValues then
    SpecializeArrayValues(TArrayValues(GenEl),TArrayValues(SpecEl))
  else if C=TInlineSpecializeExpr then
    SpecializeInlineSpecializeExpr(TInlineSpecializeExpr(GenEl),TInlineSpecializeExpr(SpecEl))
  else if C=TProcedureExpr then
    SpecializeProcedureExpr(TProcedureExpr(GenEl),TProcedureExpr(SpecEl))
  else if C=TIfExpr then
    SpecializeIfExpr(TIfExpr(GenEl),TIfExpr(SpecEl))
  else if C=TCaseExpr then
    SpecializeCaseExpr(TCaseExpr(GenEl),TCaseExpr(SpecEl))
  else if C=TCaseExprBranch then
    SpecializeCaseExprBranch(TCaseExprBranch(GenEl),TCaseExprBranch(SpecEl))
  else if C=TTryExceptExpr then
    SpecializeTryExceptExpr(TTryExceptExpr(GenEl),TTryExceptExpr(SpecEl))
  else if C=TTryExceptExprOn then
    begin
    AddTryExceptExprOn(TTryExceptExprOn(SpecEl));
    SpecializeTryExceptExprOn(TTryExceptExprOn(GenEl),TTryExceptExprOn(SpecEl));
    end
  // TPasType
  else if (C=TPasAliasType)
      or (C=TPasTypeAliasType)
      or (C=TPasClassOfType) then
    begin
    AddType(TPasAliasType(SpecEl));
    SpecializeAliasType(TPasAliasType(GenEl),TPasAliasType(SpecEl));
    end
  else if C=TPasTypeOfType then
    begin
    AddType(TPasTypeOfType(SpecEl));
    SpecializeTypeOfType(TPasTypeOfType(GenEl),TPasTypeOfType(SpecEl));
    end
  else if C=TPasPointerType then
    begin
    AddType(TPasPointerType(SpecEl));
    SpecializePointerType(TPasPointerType(GenEl),TPasPointerType(SpecEl));
    end
  else if C=TPasRangeType then
    begin
    AddType(TPasRangeType(SpecEl));
    SpecializeRangeType(TPasRangeType(GenEl),TPasRangeType(SpecEl));
    end
  else if C=TPasArrayType then
    begin
    if GetTypeParameterCount(TPasArrayType(GenEl))>0 then
      RaiseNotYetImplemented(20190815201219,GenEl);
    AddArrayType(TPasArrayType(SpecEl),nil);
    SpecializeArrayType(TPasArrayType(GenEl),TPasArrayType(SpecEl),nil);
    end
  else if C=TPasEnumValue then
    begin
    AddEnumValue(TPasEnumValue(SpecEl));
    SpecializeEnumValue(TPasEnumValue(GenEl),TPasEnumValue(SpecEl));
    end
  else if C=TPasEnumType then
    begin
    AddEnumType(TPasEnumType(SpecEl));
    SpecializeEnumType(TPasEnumType(GenEl),TPasEnumType(SpecEl));
    end
  else if C=TPasSetType then
    SpecializeSetType(TPasSetType(GenEl),TPasSetType(SpecEl))
  else if C=TPasVariant then
    SpecializeVariant(TPasVariant(GenEl),TPasVariant(SpecEl))
  else if C=TPasRecordType then
    begin
    if GetTypeParameterCount(TPasRecordType(GenEl))>0 then
      RaiseNotYetImplemented(20190815201201,GenEl);
    AddRecordType(TPasRecordType(SpecEl),nil);
    SpecializeRecordType(TPasRecordType(GenEl),TPasRecordType(SpecEl),nil);
    end
  else if C=TPasClassType then
    begin
    if GetTypeParameterCount(TPasClassType(GenEl))>0 then
      RaiseNotYetImplemented(20190816214947,GenEl);
    AddClassType(TPasClassType(SpecEl),nil);
    SpecializeClassType(TPasClassType(GenEl),TPasClassType(SpecEl),nil);
    end
  else if C=TPasStringType then
    begin
    AddType(TPasStringType(SpecEl));
    SpecializeStringType(TPasStringType(GenEl),TPasStringType(SpecEl));
    end
  else if C=TPasSpecializeType then
    begin
    AddType(TPasSpecializeType(SpecEl));
    SpecializeSpecializeType(TPasSpecializeType(GenEl),TPasSpecializeType(SpecEl));
    end
  else if C=TPasGenericTemplateType then
    SpecializeGenericTemplateType(TPasGenericTemplateType(GenEl),TPasGenericTemplateType(SpecEl))
  // empty statement
  else if C=TPasImplCommand then
  // TPasImplBlock
  else if C=TPasImplBeginBlock then
    SpecializeImplBlock(TPasImplBeginBlock(GenEl),TPasImplBeginBlock(SpecEl))
  else if C=TPasImplAsmStatement then
    SpecializeImplAsmStatement(TPasImplAsmStatement(GenEl),TPasImplAsmStatement(SpecEl))
  else if C=TPasImplRepeatUntil then
    SpecializeImplRepeatUntil(TPasImplRepeatUntil(GenEl),TPasImplRepeatUntil(SpecEl))
  else if C=TPasImplIfElse then
    SpecializeImplIfElse(TPasImplIfElse(GenEl),TPasImplIfElse(SpecEl))
  else if C=TPasImplWhileDo then
    SpecializeImplWhileDo(TPasImplWhileDo(GenEl),TPasImplWhileDo(SpecEl))
  else if C=TPasImplWithDo then
    SpecializeImplWithDo(TPasImplWithDo(GenEl),TPasImplWithDo(SpecEl))
  else if C=TPasImplCaseOf then
    SpecializeImplCaseOf(TPasImplCaseOf(GenEl),TPasImplCaseOf(SpecEl))
  else if C=TPasImplCaseStatement then
    SpecializeImplCaseStatement(TPasImplCaseStatement(GenEl),TPasImplCaseStatement(SpecEl))
  else if C=TPasImplCaseElse then
    SpecializeImplBlock(TPasImplCaseElse(GenEl),TPasImplCaseElse(SpecEl))
  else if C=TPasImplAssign then
    SpecializeImplAssign(TPasImplAssign(GenEl),TPasImplAssign(SpecEl))
  else if C=TPasImplSimple then
    SpecializeImplSimple(TPasImplSimple(GenEl),TPasImplSimple(SpecEl))
  else if C=TPasImplForLoop then
    SpecializeImplForLoop(TPasImplForLoop(GenEl),TPasImplForLoop(SpecEl))
  else if C=TPasImplTry then
    SpecializeImplTry(TPasImplTry(GenEl),TPasImplTry(SpecEl))
  else if (C=TPasImplTryFinally)
      or (C=TPasImplTryExcept)
      or (C=TPasImplTryExceptElse) then
    SpecializeImplBlock(TPasImplTryHandler(GenEl),TPasImplTryHandler(SpecEl))
  else if C=TPasImplExceptOn then
    begin
    AddExceptOn(TPasImplExceptOn(SpecEl));
    SpecializeImplExceptOn(TPasImplExceptOn(GenEl),TPasImplExceptOn(SpecEl));
    end
  else if C=TPasImplRaise then
    SpecializeImplRaise(TPasImplRaise(GenEl),TPasImplRaise(SpecEl))
  // declaration
  else if C=TPasResString then
    begin
    AddResourceString(TPasResString(SpecEl));
    SpecializeResString(TPasResString(GenEl),TPasResString(SpecEl));
    end
  else if C=TPasVariable then
    begin
    AddVariable(TPasVariable(SpecEl));
    SpecializeVariable(TPasVariable(GenEl),TPasVariable(SpecEl),true);
    end
  else if C=TPasConst then
    begin
    AddVariable(TPasConst(SpecEl));
    SpecializeConst(TPasConst(GenEl),TPasConst(SpecEl));
    end
  else if C=TPasContainsAlias then
    SpecializeContainsAlias(TPasContainsAlias(GenEl),TPasContainsAlias(SpecEl))
  else if C=TPasProperty then
    begin
    AddProperty(TPasProperty(SpecEl));
    SpecializeProperty(TPasProperty(GenEl),TPasProperty(SpecEl));
    end
  else if C=TPasAttributes then
    SpecializeAttributes(TPasAttributes(GenEl),TPasAttributes(SpecEl))
  else if C=TPasMethodResolution then
    SpecializeMethodResolution(TPasMethodResolution(GenEl),TPasMethodResolution(SpecEl))
  // procedure
  else if C=TPasArgument then
    begin
    AddArgument(TPasArgument(SpecEl));
    SpecializeArgument(TPasArgument(GenEl),TPasArgument(SpecEl));
    end
  else if C=TProcedureBody then
    begin
    AddProcedureBody(TProcedureBody(SpecEl));
    SpecializeProcedureBody(TProcedureBody(GenEl),TProcedureBody(SpecEl));
    end
  else if C.InheritsFrom(TPasOperator) then
    // TPasOperator and its descendant TPasClassOperator: must run
    // SpecializeOperator so the specialized copy keeps its OperatorType
    // (otImplicit/otExplicit/...). Matching only C=TPasOperator let a
    // `class operator` fall through to the plain-procedure branch below,
    // leaving OperatorType=otUnknown so implicit/explicit conversions on a
    // specialized generic record were not found.
    begin
    AddProcedure(TPasOperator(SpecEl),nil);
    SpecializeOperator(TPasOperator(GenEl),TPasOperator(SpecEl));
    end
  else if C.InheritsFrom(TPasProcedure) then
    begin
    AddProcedure(TPasProcedure(SpecEl),nil);
    SpecializeProcedure(TPasProcedure(GenEl),TPasProcedure(SpecEl),nil);
    end
  else if C.InheritsFrom(TPasProcedureType) then
    begin
    AddProcedureType(TPasProcedureType(SpecEl),nil);
    SpecializeProcedureType(TPasProcedureType(GenEl),TPasProcedureType(SpecEl),nil);
    end
  else if C=TPasExportSymbol then
    RaiseMsg(20210101234958,nSymbolCannotBeExportedFromALibrary,sSymbolCannotBeExportedFromALibrary,[],GenEl)
  else
    RaiseNotYetImplemented(20190728151215,GenEl);

  (*  propagate the {$PACKRECORDS} packing value now
     that the specialized type's scope/resolve-data (its storage site) exists.
     (Nested-element path; top-level generic types are handled in
     SpecializeGenericIntf, which does not route through here.) 
  *)
  SpecializePackValues(GenEl,SpecEl);
end;

procedure TPasResolver.SpecializePasElementProperties(GenEl, SpecEl: TPasElement
  );
begin
  SpecEl.SourceFilename:=GenEl.SourceFilename;
  SpecEl.SourceLinenumber:=GenEl.SourceLinenumber;;
  SpecEl.SourceEndLinenumber:=GenEl.SourceEndLinenumber;
  SpecEl.Visibility:=GenEl.Visibility;
  SpecEl.Hints:=GenEl.Hints;
  SpecEl.HintMessage:=GenEl.HintMessage;
  SpecEl.DocComment:=GenEl.DocComment;
end;

procedure TPasResolver.SpecializeVariable(GenEl, SpecEl: TPasVariable;
  Finish: boolean);
begin
  SpecializeElType(GenEl,SpecEl,GenEl.VarType,SpecEl.VarType);
  SpecEl.VarModifiers:=GenEl.VarModifiers;
  if GenEl.LibraryName<>nil then
    SpecializeElExpr(GenEl,SpecEl,GenEl.LibraryName,SpecEl.LibraryName);
  if GenEl.ExportName<>nil then
    SpecializeElExpr(GenEl,SpecEl,GenEl.ExportName,SpecEl.ExportName);
  SpecEl.Modifiers:=GenEl.Modifiers;
  if GenEl.AbsoluteExpr<>nil then
    SpecializeElExpr(GenEl,SpecEl,GenEl.AbsoluteExpr,SpecEl.AbsoluteExpr);
  if GenEl.Expr<>nil then
    SpecializeElExpr(GenEl,SpecEl,GenEl.Expr,SpecEl.Expr);
  if Finish then
    FinishVariable(SpecEl);
end;

procedure TPasResolver.SpecializeContainsAlias(GenEl, SpecEl: TPasContainsAlias);
begin
  SpecializeElExpr(GenEl,SpecEl,GenEl.Expr,SpecEl.Expr);
  FinishContainsAlias(SpecEl);
end;

procedure TPasResolver.SpecializeConst(GenEl, SpecEl: TPasConst);
begin
  SpecEl.IsConst:=GenEl.IsConst;
  SpecializeVariable(GenEl,SpecEl,true);
end;

procedure TPasResolver.SpecializeProperty(GenEl, SpecEl: TPasProperty);
begin
  SpecializeVariable(GenEl,SpecEl,false);
  SpecializeElExpr(GenEl,SpecEl,GenEl.IndexExpr,SpecEl.IndexExpr);
  SpecializeElExpr(GenEl,SpecEl,GenEl.ReadAccessor,SpecEl.ReadAccessor);
  SpecializeElExpr(GenEl,SpecEl,GenEl.WriteAccessor,SpecEl.WriteAccessor);
  SpecializeElExpr(GenEl,SpecEl,GenEl.DispIDExpr,SpecEl.DispIDExpr);
  SpecializeExprArray(GenEl,SpecEl,GenEl.Implements,SpecEl.Implements);
  SpecializeElExpr(GenEl,SpecEl,GenEl.StoredAccessor,SpecEl.StoredAccessor);
  SpecializeElExpr(GenEl,SpecEl,GenEl.DefaultExpr,SpecEl.DefaultExpr);
  SpecEl.DispIDReadOnly:=GenEl.DispIDReadOnly;
  SpecEl.IsDefault:=GenEl.IsDefault;
  SpecEl.IsNodefault:=GenEl.IsNodefault;
  SpecializeElList(GenEl,SpecEl,GenEl.Args,SpecEl.Args,false);
  FinishProperty(SpecEl);
end;

function TPasResolver.SpecializationRootOf(El: TPasElement): TPasElement;
// The GENERIC an element specializes from, or the element itself. Two types that
// specialize the same generic (a partial and a full one, say) share a root.
begin
  Result:=El;
  if (El<>nil) and (El.CustomData is TPasGenericScope)
      and (TPasGenericScope(El.CustomData).SpecializedFromItem<>nil)
      and (TPasGenericScope(El.CustomData).SpecializedFromItem.GenericEl<>nil) then
    Result:=TPasGenericScope(El.CustomData).SpecializedFromItem.GenericEl;
end;

function TPasResolver.FindSpecializedMemberByOwnerRoot(StartEl: TPasElement;
  OwnerRoot: TPasElement; const aName: string): TPasType;
// Find the member aName on the type StartEl belongs to - or on one of its
// ancestors, its enclosing types, or the ancestors of their nested class members
// - whose specialization root is OwnerRoot. Only the member's own name survives a
// qualified reference into a generic, so this is how the right owner is found
// again: rtl-generics writes `TQueueDictionary.TItemsArray`, and TItemsArray
// lives in an ancestor of that nested class.

  function MemberOf(AType: TPasElement): TPasType;
  var
    Members: TFPList;
    i: Integer;
    M: TPasElement;
  begin
    Result:=nil;
    if not (AType is TPasMembersType) then exit;
    Members:=TPasMembersType(AType).Members;
    for i:=0 to Members.Count-1 do
      begin
      M:=TPasElement(Members[i]);
      if (M is TPasType) and (CompareText(M.Name,aName)=0) then
        exit(TPasType(M));
      end;
  end;

  function ScanChain(AType: TPasElement): TPasType;
  var
    Depth: Integer;
  begin
    Result:=nil;
    Depth:=0;
    while (AType is TPasMembersType) and (Depth<32) do
      begin
      if SpecializationRootOf(AType)=OwnerRoot then
        begin
        Result:=MemberOf(AType);
        if Result<>nil then exit;
        end;
      if not (AType is TPasClassType) then exit;
      AType:=GetPasClassAncestor(TPasClassType(AType),true);
      inc(Depth);
      end;
  end;

  // The nested members of AType and its ancestors, followed through an alias:
  // lazutils' `TLazListClassesInternalMem = specialize TGenListClassesInternalMem<..>`
  // is declared in an ancestor of the class whose method names its PMemRecord.
  function ScanNestedMembers(AType: TPasElement): TPasType;
  var
    Depth, j: Integer;
    Nested: TPasElement;
  begin
    Result:=nil;
    Depth:=0;
    while (AType is TPasMembersType) and (Depth<32) do
      begin
      for j:=0 to TPasMembersType(AType).Members.Count-1 do
        begin
        Nested:=TPasElement(TPasMembersType(AType).Members[j]);
        if not (Nested is TPasType) or (Nested is TPasProcedure) then continue;
        Nested:=ResolveAliasTypeEl(Nested);
        if Nested is TPasMembersType then
          begin
          Result:=ScanChain(Nested);
          if Result<>nil then exit;
          end;
        end;
      if not (AType is TPasClassType) then exit;
      AType:=GetPasClassAncestor(TPasClassType(AType),true);
      inc(Depth);
      end;
  end;

var
  OwnerEl: TPasElement;
  Depth: Integer;
begin
  Result:=nil;
  if (StartEl=nil) or (OwnerRoot=nil) then exit;
  OwnerEl:=StartEl;
  Depth:=0;
  while (OwnerEl<>nil) and (Depth<32) do
    begin
    // the members type this element belongs to
    while (OwnerEl<>nil) and not (OwnerEl is TPasMembersType) do
      begin
      // an implementation proc hangs off the implementation section, not its class
      if (OwnerEl is TPasProcedure)
          and (OwnerEl.CustomData is TPasProcedureScope)
          and (TPasProcedureScope(OwnerEl.CustomData).ClassRecScope<>nil) then
        begin
        OwnerEl:=TPasProcedureScope(OwnerEl.CustomData).ClassRecScope.Element;
        break;
        end;
      OwnerEl:=OwnerEl.Parent;
      end;
    if not (OwnerEl is TPasMembersType) then exit;
    Result:=ScanChain(OwnerEl);
    if Result<>nil then exit;
    // a nested member type may be the one that inherits it
    Result:=ScanNestedMembers(OwnerEl);
    if Result<>nil then exit;
    OwnerEl:=OwnerEl.Parent;
    inc(Depth);
    end;
end;

function TPasResolver.OwnersAreRelated(A, B: TPasElement): boolean;
// True when two members types can hold the SAME member: they specialize the same
// generic, or one inherits from the other (an inherited member is found on the
// descendant, while the generic reference names the class that declares it).
var
  RootA, RootB: TPasElement;
begin
  RootA:=SpecializationRootOf(A);
  RootB:=SpecializationRootOf(B);
  Result:=RootA=RootB;
  if Result or (RootA=nil) or (RootB=nil) then exit;
  if (RootA is TPasClassType) and (RootB is TPasClassType) then
    Result:=(CheckClassIsClass(TPasType(RootA),TPasType(RootB))<>cIncompatible)
         or (CheckClassIsClass(TPasType(RootB),TPasType(RootA))<>cIncompatible);
end;

function TPasResolver.FindMemberViaBoundTemplate(SpecEl: TPasElement;
  GenTypeRef: TPasType): TPasType;
// GenTypeRef is a member type of a class that is the constraint of a template
// parameter (`_CONF_.TSizeT` is stored as the constraint's TSizeT). In the
// specializations enclosing SpecEl, find that template parameter and return the
// same-named member of the class bound to it, searched up its ancestors. Bound
// to another template parameter (a partial specialization), the constraint's
// member itself is the answer.
var
  El, Member: TPasElement;
  Item: TPRSpecializedItem;
  Templates: TFPList;
  Constraint, ConstraintOwner: TPasClassType;
  Bound: TPasType;
  Cls: TPasClassType;
  AName: string;
  i, j: Integer;
begin
  Result:=nil;
  ConstraintOwner:=TPasClassType(GenTypeRef.Parent);
  AName:=GenTypeRef.Name;
  El:=SpecEl;
  while El<>nil do
    begin
    Item:=SpecializedItemInProgress(El);
    if Item<>nil then
      begin
      Templates:=nil;
      if Item.GenericEl is TPasGenericType then
        Templates:=TPasGenericType(Item.GenericEl).GenericTemplateTypes;
      if Templates<>nil then
        for i:=0 to Templates.Count-1 do
          begin
          if i>=length(Item.Params) then break;
          if not (TObject(Templates[i]) is TPasGenericTemplateType) then continue;
          Constraint:=GetClassTypeConstraint(TPasGenericTemplateType(Templates[i]));
          if (Constraint=nil)
              or (CheckClassIsClass(Constraint,ConstraintOwner)=cIncompatible) then
            continue;
          Bound:=ResolveAliasType(Item.Params[i]);
          if Bound is TPasGenericTemplateType then
            exit(GenTypeRef);
          if not (Bound is TPasClassType) then continue;
          Cls:=TPasClassType(Bound);
          while Cls<>nil do
            begin
            for j:=0 to Cls.Members.Count-1 do
              begin
              Member:=TPasElement(Cls.Members[j]);
              if (Member is TPasType) and (CompareText(Member.Name,AName)=0) then
                exit(TPasType(Member));
              end;
            if (Cls.CustomData is TPasClassScope)
                and (TPasClassScope(Cls.CustomData).AncestorScope<>nil)
                and (TPasClassScope(Cls.CustomData).AncestorScope.Element is TPasClassType) then
              Cls:=TPasClassType(TPasClassScope(Cls.CustomData).AncestorScope.Element)
            else
              Cls:=nil;
            end;
          end;
      end;
    El:=El.Parent;
    end;
end;

function TPasResolver.SpecializedItemInProgress(El: TPasElement): TPRSpecializedItem;
// The specialization item of El. While a specialized class's ancestor is being
// specialized, the class has no scope yet: its item is found through the
// generic's header scope on the scope stack.
var
  i, j: Integer;
  Scope: TPasScope;
  Gen: TPasElement;
  Items: TObjectList;
begin
  Result:=nil;
  if El=nil then exit;
  if El.CustomData is TPasGenericScope then
    exit(TPasGenericScope(El.CustomData).SpecializedFromItem);
  if not (El is TPasGenericType) or (El.CustomData<>nil) then exit;
  for i:=ScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    if not (Scope is TPasGenericParamsScope) or (Scope.Element=nil) then continue;
    Gen:=Scope.Element.Parent;
    if not (Gen is TPasGenericType) or not (Gen.CustomData is TPasGenericScope) then
      continue;
    Items:=TPasGenericScope(Gen.CustomData).SpecializedItems;
    if Items=nil then continue;
    for j:=0 to Items.Count-1 do
      if (Items[j] is TPRSpecializedTypeItem)
          and (TPRSpecializedTypeItem(Items[j]).HeaderScope=Scope)
          and (TPRSpecializedItem(Items[j]).SpecializedEl=El) then
        exit(TPRSpecializedItem(Items[j]));
    end;
end;

function TPasResolver.FindGenericViaBoundTemplate(SpecEl: TPasElement;
  GenDestType: TPasType): TPasType;
// GenDestType is a generic nested in a class (`X.TCAWrap.TCapacityAccessor`)
// that a constraint reaches through a member alias (lazutils' TLazListConfig has
// TCapacityFieldT = TLazListAspectCapacityFieldMem). In the specializations
// enclosing SpecEl, take the bound class's member of that name and follow the
// same nested names to the generic it holds. nil when no such route exists.
var
  Top, El, Member: TPasElement;
  Path: TStringList;
  Item: TPRSpecializedItem;
  Templates: TFPList;
  Constraint, Cls: TPasClassType;
  Bound, Cur: TPasType;
  AliasName: string;
  i, j, k: Integer;

  // A type member named AName of AType or, for a class, of its ancestors.
  function FindTypeMember(AType: TPasType; const AName: string): TPasType;
  var
    C: TPasMembersType;
    n: Integer;
    M: TPasElement;
  begin
    Result:=nil;
    AType:=ResolveAliasType(AType);
    while AType is TPasMembersType do
      begin
      C:=TPasMembersType(AType);
      for n:=0 to C.Members.Count-1 do
        begin
        M:=TPasElement(C.Members[n]);
        if (M is TPasType) and (CompareText(M.Name,AName)=0) then
          exit(TPasType(M));
        end;
      if (C is TPasClassType) and (C.CustomData is TPasClassScope)
          and (TPasClassScope(C.CustomData).AncestorScope<>nil)
          and (TPasClassScope(C.CustomData).AncestorScope.Element is TPasType) then
        AType:=TPasType(TPasClassScope(C.CustomData).AncestorScope.Element)
      else
        AType:=nil;
      end;
  end;

  // A generic member of AType or, for a class, of its ancestors, with GenDestType's
  // name and number of type parameters.
  function FindGenericMember(AType: TPasType): TPasType;
  var
    C: TPasMembersType;
    n: Integer;
    M: TPasElement;
  begin
    Result:=nil;
    while AType is TPasMembersType do
      begin
      C:=TPasMembersType(AType);
      for n:=0 to C.Members.Count-1 do
        begin
        M:=TPasElement(C.Members[n]);
        if (M is TPasGenericType)
            and (CompareText(M.Name,GenDestType.Name)=0)
            and (GetTypeParameterCount(TPasGenericType(M))
                 =GetTypeParameterCount(TPasGenericType(GenDestType))) then
          exit(TPasType(M));
        end;
      if (C is TPasClassType) and (C.CustomData is TPasClassScope)
          and (TPasClassScope(C.CustomData).AncestorScope<>nil)
          and (TPasClassScope(C.CustomData).AncestorScope.Element is TPasType) then
        AType:=TPasType(TPasClassScope(C.CustomData).AncestorScope.Element)
      else
        AType:=nil;
      end;
  end;

  // The name of a member alias of AClass (or its ancestors) that stands for ATarget.
  function FindAliasTo(AClass: TPasClassType; ATarget: TPasElement): string;
  var
    C: TPasClassType;
    n: Integer;
    M: TPasElement;
  begin
    Result:='';
    C:=AClass;
    while C<>nil do
      begin
      for n:=0 to C.Members.Count-1 do
        begin
        M:=TPasElement(C.Members[n]);
        if (M is TPasAliasType) and (ResolveAliasType(TPasType(M))=ATarget) then
          exit(M.Name);
        end;
      if (C.CustomData is TPasClassScope)
          and (TPasClassScope(C.CustomData).AncestorScope<>nil)
          and (TPasClassScope(C.CustomData).AncestorScope.Element is TPasClassType) then
        C:=TPasClassType(TPasClassScope(C.CustomData).AncestorScope.Element)
      else
        C:=nil;
      end;
  end;

begin
  Result:=nil;
  if not (GenDestType.Parent is TPasMembersType) then exit;
  // the nested names from the outermost class down to the generic
  Path:=TStringList.Create;
  try
    Top:=GenDestType;
    while Top.Parent is TPasMembersType do
      begin
      Path.Insert(0,Top.Name);
      Top:=Top.Parent;
      end;
    El:=SpecEl;
    while El<>nil do
      begin
      Item:=SpecializedItemInProgress(El);
      if Item<>nil then
        begin
        Templates:=nil;
        if Item.GenericEl is TPasGenericType then
          Templates:=TPasGenericType(Item.GenericEl).GenericTemplateTypes;
        if Templates<>nil then
          for i:=0 to Templates.Count-1 do
            begin
            if i>=length(Item.Params) then break;
            if not (TObject(Templates[i]) is TPasGenericTemplateType) then continue;
            Constraint:=GetClassTypeConstraint(TPasGenericTemplateType(Templates[i]));
            if Constraint=nil then continue;
            // The generic is declared in the constraint class itself or in one
            // of its ancestors (`_TConfT.specialize _TC<..>` in lazutils): the
            // bound class has it, or inherits it.
            if (Top is TPasClassType)
                and (CheckClassIsClass(Constraint,TPasClassType(Top))<>cIncompatible) then
              begin
              Bound:=ResolveAliasType(Item.Params[i]);
              if Bound is TPasGenericTemplateType then
                exit(GenDestType);
              if Bound is TPasClassType then
                begin
                Cur:=Bound;
                k:=0;
                while (Cur<>nil) and (k<Path.Count-1) do
                  begin
                  Cur:=FindTypeMember(Cur,Path[k]);
                  if Cur<>nil then
                    Cur:=ResolveAliasType(Cur);
                  inc(k);
                  end;
                if Cur<>nil then
                  begin
                  Cur:=FindGenericMember(Cur);
                  if Cur<>nil then
                    exit(Cur);
                  end;
                end;
              end;
            AliasName:=FindAliasTo(Constraint,Top);
            if AliasName='' then continue;
            Bound:=ResolveAliasType(Item.Params[i]);
            if Bound is TPasGenericTemplateType then
              exit(GenDestType); // partial specialization: the constraint's
            if not (Bound is TPasClassType) then continue;
            Cls:=TPasClassType(Bound);
            Cur:=FindTypeMember(Cls,AliasName);
            k:=0;
            while (Cur<>nil) and (k<Path.Count) do
              begin
              if k=Path.Count-1 then
                begin
                // the generic itself: same name and number of type parameters
                Cur:=ResolveAliasType(Cur);
                if Cur is TPasMembersType then
                  for j:=0 to TPasMembersType(Cur).Members.Count-1 do
                    begin
                    Member:=TPasElement(TPasMembersType(Cur).Members[j]);
                    if (Member is TPasGenericType)
                        and (CompareText(Member.Name,Path[k])=0)
                        and (GetTypeParameterCount(TPasGenericType(Member))
                             =GetTypeParameterCount(TPasGenericType(GenDestType))) then
                      exit(TPasType(Member));
                    end;
                Cur:=nil;
                end
              else
                Cur:=FindTypeMember(Cur,Path[k]);
              inc(k);
              end;
            end;
        end;
      El:=El.Parent;
      end;
  finally
    Path.Free;
  end;
end;

function TPasResolver.SpecializeTypeRef(GenEl, SpecEl: TPasElement;
  GenTypeRef: TPasType): TPasType;
var
  Ref: TPasElement;
  i: integer;
  Ident: TPasIdentifier;
  Data: TPRFindData;
  Abort: boolean;
  Members: TFPList;
  NewClass: TPTreeElement;
  OwnerType: TPasType;
  GenOwner: TPasElement;
  DiscardedRef: TPasElement;
  OwnerName: String;
begin
  if GenTypeRef.Name='' then
    begin
    // Anonymous type referenced from a generic (e.g. an open-array parameter
    // `array of T`): there is no name to re-resolve, so structurally clone the
    // type (owned by the specialized element) and specialize it recursively
    // (which substitutes the template parameter in, e.g., its element type).
    if SpecEl<>nil then
      begin
      NewClass:=TPTreeElement(GenTypeRef.ClassType);
      Result:=TPasType(CreateOwnedElement(NewClass,'',SpecEl));
      SpecializeElement(GenTypeRef,Result);
      exit;
      end;
    RaiseNotYetImplemented(20190813213555,GenEl,GetObjPath(GenTypeRef));
    end;
  // A reference to a type declared in ANOTHER MODULE cannot be changed by this
  // specialization, so it must NOT be re-resolved by name: a template parameter
  // may have the very same name. rtl-generics declares `D: Rtti.TValue` inside a
  // generic whose own parameter is also called TValue, and the name search then
  // handed back the template's binding. A member of a GENERIC declared there
  // does change: lazutils' `FCapacity: TCapacityT` takes TCapacityT from a
  // generic ancestor in lazlistclassesbase.
  if (GenTypeRef.ClassType<>TPasGenericTemplateType)
      and not ((GenTypeRef.Parent is TPasMembersType) and ParamIsUndecided(GenTypeRef))
      and not ((GenTypeRef is TPasGenericType)
               and (GetTypeParameterCount(TPasGenericType(GenTypeRef))>0))
      and not ((GenTypeRef.CustomData is TPasGenericScope)
               and (TPasGenericScope(GenTypeRef.CustomData).SpecializedFromItem<>nil))
      and (GenEl<>nil) and (GenTypeRef.GetModule<>nil)
      and (GenTypeRef.GetModule<>GenEl.GetModule) then
    exit(GenTypeRef);
  // A SIBLING member of the generic being specialized must be taken from the
  // SPECIALIZED copy, never from a name search: an enclosing scope may declare
  // the same name. rtl-generics nests TOpenAddressing<...> - which has its own
  // TItem - inside TDeamortizedDArrayCuckooMap, which has another TItem, and the
  // specialized TItemsArray came out with the wrong element type.
  if (SpecEl<>nil) and (SpecEl.Parent is TPasMembersType)
      and (GenTypeRef.Parent is TPasMembersType)
      and (SpecEl.Parent.CustomData is TPasGenericScope)
      and (TPasGenericScope(SpecEl.Parent.CustomData).SpecializedFromItem<>nil)
      and (TPasGenericScope(SpecEl.Parent.CustomData).SpecializedFromItem.GenericEl
           =GenTypeRef.Parent) then
    begin
    Members:=TPasMembersType(SpecEl.Parent).Members;
    for i:=0 to Members.Count-1 do
      begin
      Ref:=TPasElement(Members[i]);
      if (Ref is TPasType) and (CompareText(Ref.Name,GenTypeRef.Name)=0) then
        exit(TPasType(Ref));
      end;
    end;
  // A member type reached through a template parameter (`_CONF_.TSizeT`, stored
  // as the CONSTRAINT's member) belongs to the class bound to that parameter:
  // lazutils' __TGenLazListClassesInternalBase<_CONF_: TLazListConfig>. A search
  // by name found the alias being specialized itself, which then pointed to itself.
  if (GenTypeRef.Parent is TPasClassType) and (SpecEl<>nil) then
    begin
    Ref:=FindMemberViaBoundTemplate(SpecEl,GenTypeRef);
    if Ref<>nil then
      exit(TPasType(Ref));
    end;
  // Use non-raising scope search so we can fall back to member search
  // for forward references during specialization
  Abort:=false;
  Data:=Default(TPRFindData);
  Data.ErrorPosEl:=GenEl;
  IterateElements(GenTypeRef.Name,@OnFindFirst_PreferNoParams,@Data,Abort);
  Ref:=Data.Found;
  // The reference was QUALIFIED (`TQueueDictionary.TItemsArray`) but only the
  // member's own name survives here, so the bare search can land on a same-named
  // member of an unrelated class that happens to be in scope. Prefer the member
  // of the type being specialized (or of an ancestor/owner of it) that
  // specializes the SAME generic as the reference's owner; if none is found, the
  // bare hit still stands.
  DiscardedRef:=nil;
  if (Ref<>nil) and (GenTypeRef.ClassType<>TPasGenericTemplateType)
      and (GenTypeRef.Parent is TPasMembersType)
      and not OwnersAreRelated(Ref.Parent,GenTypeRef.Parent) then
    begin
    DiscardedRef:=Ref;
    Ref:=nil;
    end;
  if (Ref=nil) and (GenTypeRef.ClassType<>TPasGenericTemplateType)
      and (GenTypeRef.Parent is TPasMembersType) and (SpecEl<>nil) then
    Ref:=FindSpecializedMemberByOwnerRoot(SpecEl,
      SpecializationRootOf(GenTypeRef.Parent),GenTypeRef.Name);
  if Ref=nil then
    Ref:=DiscardedRef;
  if (Ref=nil) and (SpecEl<>nil) and (SpecEl.Parent is TPasMembersType) then
    begin
    // Not found in scope. During specialization, forward-declared types
    // (e.g. PListItem = ^TListItem) may reference sibling types not yet
    // registered in the scope. Search the specialized parent's members.
    Members:=TPasMembersType(SpecEl.Parent).Members;
    for i:=0 to Members.Count-1 do
      begin
      Ref:=TPasElement(Members[i]);
      if (Ref is TPasType) and (CompareText(Ref.Name,GenTypeRef.Name)=0) then
        break;
      Ref:=nil;
      end;
    end;
  if (Ref=nil) and (GenTypeRef.Parent is TPasMembersType) then
    begin
    // A QUALIFIED reference to a member type of ANOTHER nested type, e.g.
    // fcl-stl's gmap declares, inside the generic TMap,
    //   TMSet = specialize TSet<TPair>;
    //   TIterator = specialize TMapIterator<..., TMSet.Node>;
    // Only "Node" is stored here and it lives in the scope of TMSet's own
    // (partial) specialization, which has no name that is in scope. Its
    // declaring member does: find the member that produced it, then take the
    // same name from the specialization being built.
    OwnerName:=GenTypeRef.Parent.Name;
    GenOwner:=GenEl;
    while GenOwner<>nil do
      begin
      if GenOwner is TPasMembersType then
        break;
      // An implementation proc hangs off the implementation section, not off
      // its class, so the parent chain alone never reaches the members type.
      if (GenOwner is TPasProcedure)
          and (GenOwner.CustomData is TPasProcedureScope)
          and (TPasProcedureScope(GenOwner.CustomData).ClassRecScope<>nil) then
        begin
        GenOwner:=TPasProcedureScope(GenOwner.CustomData).ClassRecScope.Element;
        break;
        end;
      GenOwner:=GenOwner.Parent;
      end;
    if GenOwner is TPasMembersType then
      begin
      Members:=TPasMembersType(GenOwner).Members;
      for i:=0 to Members.Count-1 do
        if (TObject(Members[i]) is TPasType)
            and (ResolveAliasTypeEl(TPasElement(Members[i]))=GenTypeRef.Parent) then
          begin
          OwnerName:=TPasElement(Members[i]).Name;
          break;
          end;
      end;
    if OwnerName<>'' then
      begin
      Abort:=false;
      Data:=Default(TPRFindData);
      Data.ErrorPosEl:=GenEl;
      IterateElements(OwnerName,@OnFindFirst_PreferNoParams,@Data,Abort);
      OwnerType:=ResolveAliasTypeEl(Data.Found);
      if OwnerType is TPasMembersType then
        begin
        Members:=TPasMembersType(OwnerType).Members;
        for i:=0 to Members.Count-1 do
          begin
          Ref:=TPasElement(Members[i]);
          if (Ref is TPasType) and (CompareText(Ref.Name,GenTypeRef.Name)=0) then
            break;
          Ref:=nil;
          end;
        end;
      end;
    end;
  if Ref=nil then
    RaiseIdentifierNotFound(20260331120000,GenTypeRef.Name,GenEl);
  if not (Ref is TPasType) then
    begin
    // FindElement returned a non-type (e.g. a variable shadowing the template
    // parameter name). Search scope chain directly for a type with this name.
    if GenTypeRef is TPasGenericTemplateType then
      begin
      for i:=ScopeCount-1 downto 0 do
        if Scopes[i] is TPasIdentifierScope then
          begin
          Ident:=TPasIdentifierScope(Scopes[i]).FindIdentifier(GenTypeRef.Name);
          if (Ident<>nil) and (Ident.Element is TPasType) then
            begin
            Ref:=Ident.Element;
            break;
            end;
          end;
      end;
    if not (Ref is TPasType) then
      RaiseNotYetImplemented(20260330010000,GenEl,GetObjName(Ref));
    end;
  // Still the member of an unspecialized generic: the reference was qualified
  // through a specialized member of the type being built, and only the member's
  // name survived (a .ppl body of lazutils declares
  // `MPtr: TLazListClassesInternalMem.PMemRecord`). Take it from that member.
  if (Ref=GenTypeRef) and (SpecEl<>nil)
      and (GenTypeRef.Parent is TPasGenericType)
      and (GetTypeParameterCount(TPasGenericType(GenTypeRef.Parent))>0) then
    begin
    OwnerType:=FindSpecializedMemberByOwnerRoot(SpecEl,GenTypeRef.Parent,GenTypeRef.Name);
    if OwnerType<>nil then
      Ref:=OwnerType;
    end;
  // A type that refers to itself makes every later alias walk loop forever.
  if (Ref=SpecEl) and (SpecEl<>nil) then
    RaiseNotYetImplemented(20260927033500,GenEl,
      'specialized type '+GetObjName(SpecEl)+' refers to itself');
  Result:=TPasType(Ref);
end;

procedure TPasResolver.SpecializeElType(GenEl, SpecEl: TPasElement;
  GenElType: TPasType; var SpecElType: TPasType);
var
  NewClass: TPTreeElement;
begin
  if GenElType=nil then exit;
  if SpecElType<>nil then
    RaiseNotYetImplemented(20190812021617,GenEl);
  if (GenElType.Parent<>GenEl)
      or (GenElType.ClassType=TPasGenericTemplateType) then
    begin
    // reference
    GenElType:=SpecializeTypeRef(GenEl,SpecEl,GenElType);
    SpecElType:=GenElType;
    exit;
    end;
  // e.g. anonymous type
  if SpecElType<>nil then
    RaiseNotYetImplemented(20190808222744,SpecEl,GetObjName(SpecElType));
  NewClass:=TPTreeElement(GenElType.ClassType);
  SpecElType:=TPasType(CreateOwnedElement(NewClass,GenElType.Name,SpecEl));
  SpecializeElement(GenElType,SpecElType);
end;

procedure TPasResolver.SpecializeElExpr(GenEl, SpecEl: TPasElement;
  GenElExpr: TPasExpr; var SpecElExpr: TPasExpr);
var
  NewClass: TPTreeElement;
begin
  if GenElExpr=nil then exit;
  if SpecElExpr<>nil then
    RaiseNotYetImplemented(20190803220248,SpecEl,GetObjName(SpecElExpr));
  if GenElExpr.Parent<>GenEl then
    RaiseNotYetImplemented(20190809160834,GenEl);
  // normal expression
  NewClass:=TPTreeElement(GenElExpr.ClassType);
  SpecElExpr:=TPasExpr(CreateOwnedElement(NewClass,GenElExpr.Name,SpecEl));
  SpecializeElement(GenElExpr,SpecElExpr);
end;

procedure TPasResolver.SpecializeElImplEl(GenEl, SpecEl: TPasElement;
  GenImplEl: TPasImplElement; var SpecImplEl: TPasImplElement);
var
  NewClass: TPTreeElement;
begin
  if GenImplEl=nil then exit;
  if GenImplEl.Parent<>GenEl then
    RaiseNotYetImplemented(20190808222638,GenEl,GetObjName(GenImplEl.Parent));
  NewClass:=TPTreeElement(GenImplEl.ClassType);
  SpecImplEl:=TPasImplElement(CreateOwnedElement(NewClass,GenImplEl.Name,SpecEl));
  SpecializeElement(GenImplEl,SpecImplEl);
end;

procedure TPasResolver.SpecializeElImplAlias(GenEl, SpecEl: TPasImplBlock;
  GenImplAlias: TPasImplElement; var SpecImplAlias: TPasImplElement);
var
  i: Integer;
begin
  if GenImplAlias=nil then exit;
  i:=GenEl.Elements.IndexOf(GenImplAlias);
  if i<0 then
    RaiseNotYetImplemented(20190808225239,GenEl);
  SpecImplAlias:=TObject(SpecEl.Elements[i]) as TPasImplElement;
  if SpecImplAlias.ClassType<>GenImplAlias.ClassType then
    RaiseNotYetImplemented(20190808231616,GenImplAlias,GetObjName(SpecImplAlias));
end;

procedure TPasResolver.SpecializeElList(GenEl, SpecEl: TPasElement;
  GenList, SpecList: TFPList; AllowReferences: boolean);
var
  i, si: Integer;
  GenListItem, SpecListItem, Ref: TPasElement;
  NewClass: TPTreeElement;
  Ident: TPasIdentifier;
  SynthConst: TPasConst;
  ValueExpr: TPasExpr;
begin
  for i:=0 to GenList.Count-1 do
    begin
    GenListItem:=TPasElement(GenList[i]);
    if GenListItem.Parent<>GenEl then
      begin
      if not AllowReferences then
        RaiseNotYetImplemented(20190808212421,GenEl,IntToStr(i));
      // A forwarded CONST template param used as a specialize argument
      // (generic TList<T;const U> ... specialize THelperA<U>): when the outer
      // generic is specialized with a concrete value, U is bound to a synthetic
      // const in scope. Substitute that const's value expression so the inner
      // specialization becomes "THelperA<value>" (a normal const-generic arg),
      // instead of trying to resolve U as a type (SpecializeTypeRef would find
      // the const and fail).
      if (GenListItem is TPasGenericTemplateType)
          and TPasGenericTemplateType(GenListItem).IsConst then
        begin
        SynthConst:=nil;
        for si:=ScopeCount-1 downto 0 do
          if Scopes[si] is TPasIdentifierScope then
            begin
            Ident:=TPasIdentifierScope(Scopes[si]).FindIdentifier(GenListItem.Name);
            if (Ident<>nil) and (Ident.Element<>GenListItem)
                and (Ident.Element is TPasConst)
                and (TPasConst(Ident.Element).Expr<>nil) then
              begin
              SynthConst:=TPasConst(Ident.Element);
              break;
              end;
            end;
        if SynthConst<>nil then
          begin
          ValueExpr:=CreateConstExprForSpecParam(SynthConst.Expr,SpecEl);
          SpecList.Add(ValueExpr);
          continue;
          end;
        end;
      if not (GenListItem is TPasType) then
        RaiseNotYetImplemented(20190812025715,GenEl,IntToStr(i)+' GenListItem='+GetObjName(GenListItem));
      // reference
      Ref:=SpecializeTypeRef(GenEl,SpecEl,TpasType(GenListItem));
      SpecList.Add(Ref);
      continue;
      end;
    if GenListItem.ClassType=TPasGenericTemplateType then
      RaiseNotYetImplemented(20190812233309,GenEl);
    NewClass:=TPTreeElement(GenListItem.ClassType);
    SpecListItem:=CreateOwnedElement(NewClass,GenListItem.Name,SpecEl);
    SpecList.Add(SpecListItem);
    SpecializeElement(GenListItem,SpecListItem);
    end;
end;

procedure TPasResolver.SpecializeElArray(GenEl, SpecEl: TPasElement;
  GenList: TPasElementArray; var SpecList: TPasElementArray;
  AllowReferences: boolean);
var
  l, i: Integer;
  GenListItem, Ref, SpecListItem: TPasElement;
  NewClass: TPTreeElement;
begin
  if length(SpecList)>0 then
    RaiseNotYetImplemented(20190914102814,GenEl,GetObjName(SpecEl));
  l:=length(GenList);
  SetLength(SpecList,l);
  for i:=0 to l-1 do
    SpecList[i]:=nil;
  for i:=0 to l-1 do
    begin
    GenListItem:=GenList[i];
    if GenListItem.Parent<>GenEl then
      begin
      if not AllowReferences then
        RaiseNotYetImplemented(20190914102952,GenEl,IntToStr(i));
      if not (GenListItem is TPasType) then
        RaiseNotYetImplemented(20190914102957,GenEl,IntToStr(i)+' GenListItem='+GetObjName(GenListItem));
      // reference
      Ref:=SpecializeTypeRef(GenEl,SpecEl,TPasType(GenListItem));
      SpecList[i]:=Ref;
      continue;
      end;
    if GenListItem.ClassType=TPasGenericTemplateType then
      RaiseNotYetImplemented(20190914103040,GenEl);
    NewClass:=TPTreeElement(GenListItem.ClassType);
    SpecListItem:=CreateOwnedElement(NewClass,GenListItem.Name,SpecEl);
    SpecList[i]:=SpecListItem;
    SpecializeElement(GenListItem,SpecListItem);
    end;
end;

procedure TPasResolver.SpecializeProcedure(GenEl, SpecEl: TPasProcedure;
  SpecializedItem: TPRSpecializedItem);
var
  GenProcType: TPasProcedureType;
  NewClass: TPTreeElement;
  SpecProcScope, GenProcScope: TPasProcedureScope;
  i, j: Integer;
  GenPart, SpecPart: TProcedureNamePart;
  GenTempl, SpecTempl: TPasGenericTemplateType;
  Templates: TFPList;
  GenBody: TProcedureBody;
begin
  GenProcScope:=GenEl.CustomData as TPasProcedureScope;
  SpecProcScope:=SpecEl.CustomData as TPasProcedureScope;
  if SpecProcScope<>nil then
    begin
    if TopScope<>SpecProcScope then
      RaiseNotYetImplemented(20190920194151,SpecEl);
    end
  else if SpecializedItem<>nil then
    begin
    // specialized generic/parameterized procedure
    SpecProcScope:=TPasProcedureScope(PushScope(SpecEl,ScopeClass_Procedure));
    SpecProcScope.SpecializedFromItem:=SpecializedItem;
    if GenProcScope.DeclarationProc<>nil then
      RaiseNotYetImplemented(20190920203700,SpecEl);
    if GenProcScope.OverriddenProc<>nil then
      RaiseNotYetImplemented(20190920203536,SpecEl);
    SpecProcScope.ClassRecScope:=GenProcScope.ClassRecScope;
    // SpecProcScope.Flags
    Templates:=GetProcTemplateTypes(GenEl);
    if (Templates=nil) or (Templates.Count=0) then
      RaiseNotYetImplemented(20190920183140,SpecEl);
    AddSpecializedTemplateIdentifiers(Templates,SpecializedItem,SpecProcScope,true);
    end
  else
    RaiseNotYetImplemented(20190922153918,SpecEl);
  Include(SpecProcScope.Flags,ppsfIsSpecialized);

  // The specialized routine IS the generic's code, so it is judged by the mode
  // of the unit that DECLARED it - not by whatever the consumer is written in.
  // A scope created through the ordinary AddProcedure path takes the CURRENT
  // parser's switches, which for a specialization is the consumer's file (and,
  // for a generic loaded from a precompiled unit, a file that has nothing to do
  // with it). rtl-generics is {$mode delphi} and passes a bare method name
  // where a method pointer is wanted; an objfpc consumer specializing it then
  // rejected its own generic's body.
  if GenProcScope<>nil then
    begin
    SpecProcScope.ModeSwitches:=GenProcScope.ModeSwitches;
    SpecProcScope.BoolSwitches:=GenProcScope.BoolSwitches;
    end;

  if GenEl.PublicName<>nil then
    SpecializeElExpr(GenEl,SpecEl,GenEl.PublicName,SpecEl.PublicName);
  if GenEl.LibrarySymbolName<>nil then
    SpecializeElExpr(GenEl,SpecEl,GenEl.LibrarySymbolName,SpecEl.LibrarySymbolName);
  if GenEl.LibraryExpr<>nil then
    SpecializeElExpr(GenEl,SpecEl,GenEl.LibraryExpr,SpecEl.LibraryExpr);
  if GenEl.DispIDExpr<>nil then
    SpecializeElExpr(GenEl,SpecEl,GenEl.DispIDExpr,SpecEl.DispIDExpr);
  if GenEl.MessageExpr<>nil then
    SpecializeElExpr(GenEl,SpecEl,GenEl.MessageExpr,SpecEl.MessageExpr);
  SpecEl.MessageName:=GenEl.MessageName;
  SpecEl.MessageType:=GenEl.MessageType;
  SpecEl.AliasName:=GenEl.AliasName;
  SpecEl.Modifiers:=GenEl.Modifiers;
  if GenEl.NameParts<>nil then
    begin
    if SpecEl.NameParts<>nil then
      RaiseNotYetImplemented(20190818125620,SpecEl);
    SpecEl.NameParts:=TFPList.Create;
    for i:=0 to GenEl.NameParts.Count-1 do
      begin
      GenPart:=TProcedureNamePart(GenEl.NameParts[i]);
      SpecPart:=TProcedureNamePart.Create;
      SpecEl.NameParts.Add(SpecPart);
      SpecPart.Name:=GenPart.Name;
      if GenPart.Templates<>nil then
        begin
        if (SpecializedItem<>nil) and (i=GenEl.NameParts.Count-1) then
          begin
          // the templates have been specialized to SpecializedItem.Params
          continue;
          end;
        SpecPart.Templates:=TFPList.Create;
        for j:=0 to GenPart.Templates.Count-1 do
          begin
          GenTempl:=TPasGenericTemplateType(GenPart.Templates[j]);
          if GenTempl.Parent<>GenEl then
            RaiseNotYetImplemented(20190818130001,GenEl,IntToStr(i)+','+IntToStr(j)+':'+GenTempl.Name+' GenParent='+GetObjName(GenTempl.Parent)+' GenEl='+GetObjName(GenEl));
          NewClass:=TPTreeElement(GenTempl.ClassType);
          SpecTempl:=TPasGenericTemplateType(CreateOwnedElement(NewClass,GenTempl.Name,SpecEl));
          SpecPart.Templates.Add(SpecTempl);
          SpecializeElement(GenTempl,SpecTempl);
          end;
        end;
      end;
    end;
  if GenEl.ProcType<>nil then
    begin
    GenProcType:=GenEl.ProcType;
    if GenProcType.Parent<>GenEl then
      begin
      {$IFDEF defined(VerbosePCUFiler) or defined(VerbosePJUFiler)}
      writeln('TPasResolver.SpecializeProcedure GenEl=',GetObjPath(GenEl),' GenProcType.Parent=',GetObjPath(GenProcType.Parent));
      {$ENDIF}
      RaiseNotYetImplemented(20190803212426,GenEl,GetObjPath(GenProcType.Parent));
      end;
    NewClass:=TPTreeElement(GenProcType.ClassType);
    SpecEl.ProcType:=TPasProcedureType(CreateOwnedElement(NewClass,GenProcType.Name,SpecEl));
    SpecializeElement(GenProcType,SpecEl.ProcType);
    end;
  SpecProcScope.GenericStep:=psgsInterfaceParsed;

  if GenEl.Body<>nil then
    begin
    // implementation or anonymous proc
    if SpecializedItem<>nil then
      SpecializedItem.Step:=prssImplementationBuilding;
    GenBody:=GenEl.Body;
    if GenBody.Parent<>GenEl then
      RaiseNotYetImplemented(20190804183308,GenEl,GetObjName(GenBody.Parent));
    if SpecEl.Body<>nil then
      RaiseNotYetImplemented(20190920211853,SpecEl);
    NewClass:=TPTreeElement(GenBody.ClassType);
    SpecEl.Body:=TProcedureBody(CreateOwnedElement(NewClass,GenBody.Name,SpecEl));
    SpecializeElement(GenBody,SpecEl.Body);
    FinishProcedure(SpecEl);
    end
  else if SpecializedItem=nil then
    // declaration proc, parent is specialized
    FinishProcedure(SpecEl)
  else
    begin
    // specialized generic procedure, body is not yet parsed
    SpecializedItem.Step:=prssInterfaceFinished;
    if TopScope<>SpecProcScope then
      RaiseNotYetImplemented(20190920190400,SpecEl);
    PopScope;
    end;
end;

procedure TPasResolver.SpecializeOperator(GenEl, SpecEl: TPasOperator);
begin
  SpecEl.OperatorType:=GenEl.OperatorType;
  SpecEl.TokenBased:=GenEl.TokenBased;
  SpecializeProcedure(GenEl,SpecEl,nil);
end;

procedure TPasResolver.SpecializeProcedureType(GenEl,
  SpecEl: TPasProcedureType; SpecializedItem: TPRSpecializedItem);
var
  GenResultEl, NewResultEl: TPasResultElement;
  NewClass: TPTreeElement;
  i: Integer;
  SpecScope: TPasGenericScope;
begin
  if GenEl.GenericTemplateTypes<>nil then
    begin
    SpecScope:=TPasGenericScope(PushScope(SpecEl,ScopeClass_ProcType));
    if SpecializedItem<>nil then
      begin
      // specialized procedure type
      SpecScope.SpecializedFromItem:=SpecializedItem;
      AddSpecializedTemplateIdentifiers(GenEl.GenericTemplateTypes,
        SpecializedItem,SpecScope,true);
      end
    else
      begin
      // generic procedure type inside a generic type
      RaiseNotYetImplemented(20190813194550,GenEl);
      end;
    end;
  // Args
  SpecializeElList(GenEl,SpecEl,GenEl.Args,SpecEl.Args,false);
  for i:=0 to SpecEl.Args.Count-1 do
    FinishArgument(TPasArgument(SpecEl.Args[i]));
  // varargs
  SpecializeElType(GenEl,SpecEl,GenEl.VarArgsType,SpecEl.VarArgsType);

  // calling convention and proc type modifiers
  SpecEl.CallingConvention:=GenEl.CallingConvention;
  SpecEl.Modifiers:=GenEl.Modifiers;

  // function result
  if SpecEl is TPasFunctionType then
    begin
    GenResultEl:=TPasFunctionType(GenEl).ResultEl;
    if GenResultEl<>nil then
      begin
      if GenResultEl.Parent<>GenEl then
        RaiseNotYetImplemented(20190803212935,GenEl,GetObjName(GenResultEl.Parent));
      NewClass:=TPTreeElement(GenResultEl.ClassType);
      NewResultEl:=TPasResultElement(CreateOwnedElement(NewClass,GenResultEl.Name,SpecEl));
      TPasFunctionType(SpecEl).ResultEl:=NewResultEl;
      AddFunctionResult(NewResultEl);
      SpecializeElType(GenResultEl,NewResultEl,GenResultEl.ResultType,NewResultEl.ResultType);
      end;
    end;

  FinishProcedureType(SpecEl);
  if SpecializedItem<>nil then
    SpecializedItem.Step:=prssImplementationFinished;
end;

procedure TPasResolver.SpecializeProcedureBody(GenEl, SpecEl: TProcedureBody);
var
  GenBody, NewBody: TPasImplBlock;
  NewClass: TPTreeElement;
begin
  SpecializeDeclarations(GenEl,SpecEl);
  FinishTypeSection(SpecEl);

  if GenEl.Body<>nil then
    begin
    GenBody:=GenEl.Body;
    if GenBody.Parent<>GenEl then
      RaiseNotYetImplemented(20190804184934,GenBody);
    NewClass:=TPTreeElement(GenBody.ClassType);
    NewBody:=TPasImplBlock(CreateOwnedElement(NewClass,GenBody.Name,SpecEl));
    SpecEl.Body:=NewBody;
    SpecializeElement(GenBody,NewBody);
    end;
end;

procedure TPasResolver.SpecializeDeclarations(GenEl, SpecEl: TPasDeclarations);
var
  i: Integer;
  GenDecl, NewDecl: TPasElement;
  NewClass: TPTreeElement;
begin
  for i:=0 to GenEl.Declarations.Count-1 do
    begin
    GenDecl:=TPasElement(GenEl.Declarations[i]);
    if GenDecl.Parent<>GenEl then
      RaiseNotYetImplemented(20190806091336,GenEl,GetObjName(GenDecl));
    NewClass:=TPTreeElement(GenDecl.ClassType);
    NewDecl:=CreateOwnedElement(NewClass,GenDecl.Name,SpecEl);
    SpecEl.Declarations.Add(NewDecl);
    if NewClass=TPasAttributes then
      SpecEl.Attributes.Add(NewDecl)
    else if (NewClass=TPasClassType)
        or (NewClass=TPasRecordType) then
      SpecEl.Classes.Add(NewDecl)
    else if NewClass=TPasConst then
      SpecEl.Consts.Add(NewDecl)
    else if NewClass=TPasExportSymbol then
      SpecEl.ExportSymbols.Add(NewDecl)
    else if NewClass.InheritsFrom(TPasProcedure) then
      SpecEl.Functions.Add(NewDecl)
    else if NewClass=TPasProperty then
      SpecEl.Properties.Add(NewDecl)
    else if NewClass=TPasResString then
      SpecEl.ResStrings.Add(NewDecl)
    else if NewClass.InheritsFrom(TPasType) then
      SpecEl.Types.Add(NewDecl)
    else if NewClass=TPasVariable then
      SpecEl.Variables.Add(NewDecl)
    else
      RaiseNotYetImplemented(20190804184718,GenDecl);
    SpecializeElement(GenDecl,NewDecl);
    end;
end;

procedure TPasResolver.SpecializeSpecializeType(GenEl,
  SpecEl: TPasSpecializeType);
var
  GenDestType: TPasType;
  Ref: TPasElement;
begin
  // search DestType<ParamCount>
  GenDestType:=GenEl.DestType;
  if GenDestType=nil then
    RaiseNotYetImplemented(20190812022211,GenEl);
  if GenDestType.Parent=GenEl then
    RaiseNotYetImplemented(20190812022251,GenEl);
  // A generic reached through a template parameter's member
  // (`_CONF_.TCapacityFieldT.TCAWrap.specialize TCapacityAccessor<..>`) is not
  // in scope by its bare name; it is taken from the class bound to the parameter.
  Ref:=FindGenericViaBoundTemplate(SpecEl,GenDestType);
  if Ref=nil then
    Ref:=FindElementFor(GenDestType.Name,GenEl.Parent,GenEl.Params.Count);
  if not (Ref is TPasGenericType) then
    RaiseNotYetImplemented(20190812022359,GenEl,GetObjName(Ref));
  SpecEl.DestType:=TPasGenericType(Ref);

  SpecializeElExpr(GenEl,SpecEl,GenEl.Expr,SpecEl.Expr);
  SpecializeElList(GenEl,SpecEl,GenEl.Params,SpecEl.Params,true);

  // The specialization must EXIST before the part after the dot is resolved, or
  // PushParserSpecializeType has only the generic to offer and `A<B>.C` binds
  // C in A. FinishSpecializeType only needs the params, which are specialized
  // above.
  FinishSpecializeType(SpecEl);

  if GenEl.SubType<>nil then
    begin
    PushParserSpecializeType(SpecEl);
    SpecializeElType(GenEl,SpecEl,GenEl.SubType,SpecEl.SubType);
    PopScope;
    end;
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.SpecializeSpecializeType ',GetObjName(SpecEl.DestType),' ',GetObjName(SpecEl.CustomData));
  {$ENDIF}
end;

procedure TPasResolver.SpecializeGenericTemplateType(GenEl,
  SpecEl: TPasGenericTemplateType);
var
  GenConstraints, SpecConstraints: TPasElementArray;
  i: Integer;
  ConEl: TPasElement;
begin
  GenConstraints:=GenEl.Constraints;
  if length(SpecEl.Constraints)>0 then
    RaiseNotYetImplemented(20190914070209,GenEl);
  SetLength(SpecEl.Constraints,length(GenConstraints));
  SpecConstraints:=SpecEl.Constraints;
  for i:=0 to length(SpecConstraints)-1 do
    SpecConstraints[i]:=nil;
  for i:=0 to length(GenConstraints)-1 do
    begin
    ConEl:=GenConstraints[i];
    if ConEl is TPasExpr then
      SpecializeElExpr(GenEl,SpecEl,TPasExpr(ConEl),TPasExpr(SpecConstraints[i]))
    else if ConEl is TPasType then
      SpecializeElType(GenEl,SpecEl,TPasType(ConEl),TPasType(SpecConstraints[i]))
    else
      RaiseNotYetImplemented(20190914070522,GenEl,IntToStr(i)+' '+GetObjName(ConEl));
    end;
end;

procedure TPasResolver.SpecializeArgument(GenEl, SpecEl: TPasArgument);
begin
  SpecEl.Access:=GenEl.Access;
  SpecializeElType(GenEl,SpecEl,GenEl.ArgType,SpecEl.ArgType);
  if GenEl.ValueExpr<>nil then
    SpecializeElExpr(GenEl,SpecEl,GenEl.ValueExpr,SpecEl.ValueExpr);

  // FinishArgument is called when all arguments are ready
end;

procedure TPasResolver.SpecializeImplBlock(GenEl, SpecEl: TPasImplBlock);
var
  i: Integer;
  GenImpl, NewImpl: TPasImplElement;
  NewClass: TPTreeElement;
begin
  for i:=0 to GenEl.Elements.Count-1 do
    begin
    GenImpl:=TPasImplElement(GenEl.Elements[i]);
    if GenImpl.Parent<>GenEl then
      RaiseNotYetImplemented(20190806092151,GenEl,GetElementSourcePosStr(GenImpl));
    NewClass:=TPTreeElement(GenImpl.ClassType);
    NewImpl:=TPasImplElement(CreateOwnedElement(NewClass,GenImpl.Name,SpecEl));
    SpecEl.Elements.Add(NewImpl);
    SpecializeElement(GenImpl,NewImpl);
    end;
end;

procedure TPasResolver.SpecializeImplAsmStatement(GenEl,
  SpecEl: TPasImplAsmStatement);
begin
  SpecializeImplBlock(GenEl,SpecEl);
  SpecEl.Tokens.Assign(GenEl.Tokens);
end;

procedure TPasResolver.SpecializeImplRepeatUntil(GenEl,
  SpecEl: TPasImplRepeatUntil);
begin
  SpecializeImplBlock(GenEl,SpecEl);
  SpecializeElExpr(GenEl,SpecEl,GenEl.ConditionExpr,SpecEl.ConditionExpr);
end;

procedure TPasResolver.SpecializeImplIfElse(GenEl, SpecEl: TPasImplIfElse);
begin
  // do not call SpecializeImplBlock(GenEl,SpecEl);
  SpecializeElExpr(GenEl,SpecEl,GenEl.ConditionExpr,SpecEl.ConditionExpr);
  SpecializeElImplEl(GenEl,SpecEl,GenEl.IfBranch,SpecEl.IfBranch);
  SpecializeElImplEl(GenEl,SpecEl,GenEl.ElseBranch,SpecEl.ElseBranch);
end;

procedure TPasResolver.SpecializeImplWhileDo(GenEl, SpecEl: TPasImplWhileDo);
begin
  // do not call SpecializeImplBlock(GenEl,SpecEl);
  SpecializeElExpr(GenEl,SpecEl,GenEl.ConditionExpr,SpecEl.ConditionExpr);
  SpecializeElImplEl(GenEl,SpecEl,GenEl.Body,SpecEl.Body);
end;

procedure TPasResolver.SpecializeImplWithDo(GenEl, SpecEl: TPasImplWithDo);
var
  i: Integer;
  GenExpr, SpecExpr: TPasExpr;
  NewClass: TPTreeElement;
begin
  if SpecEl.CustomData<>nil then
    RaiseNotYetImplemented(20200530201007,GenEl,GetObjName(SpecEl.CustomData));
  PushScope(SpecEl,TPasWithScope);

  for i:=0 to GenEl.Expressions.Count-1 do
    begin
    GenExpr:=TPasExpr(GenEl.Expressions[i]);
    if GenExpr.Parent<>GenEl then
      RaiseNotYetImplemented(20190808224343,GenEl,IntToStr(i));
    NewClass:=TPTreeElement(GenExpr.ClassType);
    SpecExpr:=TPasExpr(CreateOwnedElement(NewClass,GenExpr.Name,SpecEl));
    SpecEl.Expressions.Add(SpecExpr);
    SpecializeElement(GenExpr,SpecExpr);
    BeginScope(stWithExpr,SpecExpr);
    end;
  SpecializeElImplEl(GenEl,SpecEl,GenEl.Body,SpecEl.Body);

  FinishWithDo(SpecEl);
end;

procedure TPasResolver.SpecializeImplCaseOf(GenEl, SpecEl: TPasImplCaseOf);
begin
  SpecializeElExpr(GenEl,SpecEl,GenEl.CaseExpr,SpecEl.CaseExpr);
  SpecializeImplBlock(GenEl,SpecEl); // Elements
  if GenEl.ElseBranch<>nil then
    SpecializeElImplAlias(GenEl,SpecEl,GenEl.ElseBranch,TPasImplElement(SpecEl.ElseBranch));
end;

procedure TPasResolver.SpecializeImplCaseStatement(GenEl,
  SpecEl: TPasImplCaseStatement);
begin
  SpecializeElList(GenEl,SpecEl,GenEl.Expressions,SpecEl.Expressions,false);
  SpecializeElImplEl(GenEl,SpecEl,GenEl.Body,SpecEl.Body);
end;

procedure TPasResolver.SpecializeImplAssign(GenEl, SpecEl: TPasImplAssign);
begin
  if GenEl.Elements.Count>0 then
    RaiseNotYetImplemented(20190808142935,GenEl);
  SpecEl.Kind:=GenEl.Kind;
  SpecializeElExpr(GenEl,SpecEl,GenEl.Left,SpecEl.Left);
  SpecializeElExpr(GenEl,SpecEl,GenEl.Right,SpecEl.Right);
end;

procedure TPasResolver.SpecializeImplSimple(GenEl, SpecEl: TPasImplSimple);
begin
  if GenEl.Elements.Count>0 then
    RaiseNotYetImplemented(20190808142935,GenEl);
  SpecializeElExpr(GenEl,SpecEl,GenEl.Expr,SpecEl.Expr);
end;

procedure TPasResolver.SpecializeImplForLoop(GenEl, SpecEl: TPasImplForLoop);
var
  i: Integer;
  GenImpl, NewImpl: TPasImplElement;
  NewClass: TPTreeElement;
begin
  if GenEl.Variable<>nil then
    RaiseNotYetImplemented(20190808142627,GenEl);
  SpecializeElExpr(GenEl,SpecEl,GenEl.VariableName,SpecEl.VariableName);
  SpecEl.LoopType:=GenEl.LoopType;
  SpecializeElExpr(GenEl,SpecEl,GenEl.StartExpr,SpecEl.StartExpr);
  SpecializeElExpr(GenEl,SpecEl,GenEl.EndExpr,SpecEl.EndExpr);
  FinishForLoopHeader(SpecEl);

  // SpecEl.Body is set via AddElement
  for i:=0 to GenEl.Elements.Count-1 do
    begin
    GenImpl:=TPasImplElement(GenEl.Elements[i]);
    if GenImpl.Parent<>GenEl then
      RaiseNotYetImplemented(20190806092151,GenEl,GetElementSourcePosStr(GenImpl));
    NewClass:=TPTreeElement(GenImpl.ClassType);
    NewImpl:=TPasImplElement(CreateOwnedElement(NewClass,GenImpl.Name,SpecEl));
    SpecEl.AddElement(NewImpl);
    SpecializeElement(GenImpl,NewImpl);
    end;
end;

procedure TPasResolver.SpecializeImplTry(GenEl, SpecEl: TPasImplTry);
begin
  SpecializeImplBlock(GenEl,SpecEl); // clone elements
  if GenEl.FinallyExcept<>nil then
    SpecializeElImplEl(GenEl,SpecEl,GenEl.FinallyExcept,
      TPasImplElement(SpecEl.FinallyExcept));
  if GenEl.ElseBranch<>nil then
    SpecializeElImplEl(GenEl,SpecEl,GenEl.ElseBranch,
      TPasImplElement(SpecEl.ElseBranch));
end;

procedure TPasResolver.SpecializeImplExceptOn(GenEl, SpecEl: TPasImplExceptOn);
var
  GenVar: TPasVariable;
  NewClass: TPTreeElement;
begin
  GenVar:=GenEl.VarEl;
  if GenVar<>nil then
    begin
    if GenVar.Parent<>GenEl then
      RaiseNotYetImplemented(20190808232327,GenEl);
    NewClass:=TPTreeElement(GenVar.ClassType);
    SpecEl.VarEl:=TPasVariable(CreateOwnedElement(NewClass,GenVar.Name,SpecEl));
    SpecializeElement(GenVar,SpecEl.VarEl);
    if GenVar.VarType<>GenEl.TypeEl then
      RaiseNotYetImplemented(20190808232601,GenEl);
    SpecEl.TypeEl:=SpecEl.VarEl.VarType;
    end
  else
    SpecializeElType(GenEl,SpecEl,GenEl.TypeEl,SpecEl.TypeEl);

  FinishExceptOnExpr;
  SpecializeElImplEl(GenEl,SpecEl,GenEl.Body,SpecEl.Body);
  FinishExceptOnStatement;
end;

procedure TPasResolver.SpecializeImplRaise(GenEl, SpecEl: TPasImplRaise);
begin
  SpecializeElExpr(GenEl,SpecEl,GenEl.ExceptObject,SpecEl.ExceptObject);
  SpecializeElExpr(GenEl,SpecEl,GenEl.ExceptAddr,SpecEl.ExceptAddr);
end;

procedure TPasResolver.SpecializeExpr(GenEl, SpecEl: TPasExpr);
begin
  SpecEl.Kind:=GenEl.Kind;
  SpecEl.OpCode:=GenEl.OpCode;
  SpecializeElExpr(GenEl,SpecEl,GenEl.Format1,SpecEl.Format1);
  SpecializeElExpr(GenEl,SpecEl,GenEl.Format2,SpecEl.Format2);
end;

procedure TPasResolver.SpecializeExprArray(GenEl, SpecEl: TPasElement;
  GenArray: TPasExprArray; var SpecArray: TPasExprArray);
var
  i: Integer;
begin
  if length(SpecArray)>0 then
    RaiseNotYetImplemented(20190808205855,GenEl);
  SetLength(SpecArray,length(GenArray));
  for i:=0 to length(SpecArray)-1 do
    SpecArray[i]:=nil;
  for i:=0 to length(GenArray)-1 do
    SpecializeElExpr(GenEl,SpecEl,GenArray[i],SpecArray[i]);
end;

procedure TPasResolver.SpecializePrimitiveExpr(GenEl, SpecEl: TPrimitiveExpr);
begin
  SpecializeExpr(GenEl,SpecEl);
  SpecEl.Value:=GenEl.Value;
end;

procedure TPasResolver.SpecializeUnaryExpr(GenEl, SpecEl: TUnaryExpr);
begin
  SpecializeExpr(GenEl,SpecEl);
  SpecializeElExpr(GenEl,SpecEl,GenEl.Operand,SpecEl.Operand);
end;

procedure TPasResolver.SpecializeBinaryExpr(GenEl, SpecEl: TBinaryExpr);
begin
  SpecializeExpr(GenEl,SpecEl);
  SpecializeElExpr(GenEl,SpecEl,GenEl.Left,SpecEl.Left);
  SpecializeElExpr(GenEl,SpecEl,GenEl.Right,SpecEl.Right);
end;

procedure TPasResolver.SpecializeIfExpr(GenEl, SpecEl: TIfExpr);
begin
  SpecializeExpr(GenEl,SpecEl);
  SpecializeElExpr(GenEl,SpecEl,GenEl.ConditionExpr,SpecEl.ConditionExpr);
  SpecializeElExpr(GenEl,SpecEl,GenEl.ThenExpr,SpecEl.ThenExpr);
  SpecializeElExpr(GenEl,SpecEl,GenEl.ElseExpr,SpecEl.ElseExpr);
end;

procedure TPasResolver.SpecializeCaseExpr(GenEl, SpecEl: TCaseExpr);
begin
  SpecializeExpr(GenEl,SpecEl);
  SpecializeElExpr(GenEl,SpecEl,GenEl.CaseExpr,SpecEl.CaseExpr);
  SpecializeElList(GenEl,SpecEl,GenEl.Branches,SpecEl.Branches,false);
  SpecializeElExpr(GenEl,SpecEl,GenEl.ElseExpr,SpecEl.ElseExpr);
end;

procedure TPasResolver.SpecializeCaseExprBranch(GenEl, SpecEl: TCaseExprBranch);
begin
  SpecializeElList(GenEl,SpecEl,GenEl.Labels,SpecEl.Labels,false);
  SpecializeElExpr(GenEl,SpecEl,GenEl.Value,SpecEl.Value);
end;

procedure TPasResolver.SpecializeTryExceptExpr(GenEl, SpecEl: TTryExceptExpr);
begin
  SpecializeExpr(GenEl,SpecEl);
  SpecializeElExpr(GenEl,SpecEl,GenEl.TryExpr,SpecEl.TryExpr);
  SpecializeElList(GenEl,SpecEl,GenEl.OnBranches,SpecEl.OnBranches,false);
  SpecializeElExpr(GenEl,SpecEl,GenEl.ElseExpr,SpecEl.ElseExpr);
end;

procedure TPasResolver.SpecializeTryExceptExprOn(GenEl,
  SpecEl: TTryExceptExprOn);
var
  GenVar: TPasVariable;
  NewClass: TPTreeElement;
begin
  GenVar:=GenEl.VarEl;
  if GenVar<>nil then
    begin
    if GenVar.Parent<>GenEl then
      RaiseNotYetImplemented(20260913130100,GenEl);
    NewClass:=TPTreeElement(GenVar.ClassType);
    SpecEl.VarEl:=TPasVariable(CreateOwnedElement(NewClass,GenVar.Name,SpecEl));
    SpecializeElement(GenVar,SpecEl.VarEl);
    if GenVar.VarType<>GenEl.TypeEl then
      RaiseNotYetImplemented(20260913130101,GenEl);
    SpecEl.TypeEl:=SpecEl.VarEl.VarType;
    end
  else
    SpecializeElType(GenEl,SpecEl,GenEl.TypeEl,SpecEl.TypeEl);

  FinishExceptOnExpr;
  SpecializeElExpr(GenEl,SpecEl,GenEl.Value,SpecEl.Value);
  FinishExceptOnStatement;
end;

procedure TPasResolver.SpecializeBoolConstExpr(GenEl, SpecEl: TBoolConstExpr);
begin
  SpecializeExpr(GenEl,SpecEl);
  SpecEl.Value:=GenEl.Value;
end;

procedure TPasResolver.SpecializeParamsExpr(GenEl, SpecEl: TParamsExpr);
begin
  SpecializeExpr(GenEl,SpecEl);
  SpecializeElExpr(GenEl,SpecEl,GenEl.Value,SpecEl.Value);
  SpecializeExprArray(GenEl,SpecEl,GenEl.Params,SpecEl.Params);
end;

procedure TPasResolver.SpecializeRecordValues(GenEl, SpecEl: TRecordValues);
var
  GenField: TRecordValuesItem;
  i: Integer;
  SpecFieldP: PRecordValuesItem;
begin
  SpecializeExpr(GenEl,SpecEl);

  // fields
  SetLength(SpecEl.Fields,length(GenEl.Fields));
  for i:=0 to length(SpecEl.Fields)-1 do
    with SpecEl.Fields[i] do
      begin
      NameExp:=nil;
      ValueExp:=nil;
      end;
  for i:=0 to length(GenEl.Fields)-1 do
    begin
    GenField:=GenEl.Fields[i];
    if GenField.NameExp.Parent<>GenEl then
      RaiseNotYetImplemented(20190808205128,GenEl);
    if GenField.ValueExp.Parent<>GenEl then
      RaiseNotYetImplemented(20190808205138,GenEl);
    SpecFieldP:=@SpecEl.Fields[i];
    // the field NAME is data, not an expression, so it has to be copied too -
    // without it every field of a record constant inside a generic loses its
    // name at specialization (rtl-generics generics.defaults)
    SpecFieldP^.Name:=GenField.Name;
    SpecializeElExpr(GenEl,SpecEl,GenField.NameExp,TPasExpr(SpecFieldP^.NameExp));
    SpecializeElExpr(GenEl,SpecEl,GenField.ValueExp,SpecFieldP^.ValueExp);
    end;
end;

procedure TPasResolver.SpecializeArrayValues(GenEl, SpecEl: TArrayValues);
begin
  SpecializeExpr(GenEl,SpecEl);
  SpecializeExprArray(GenEl,SpecEl,GenEl.Values,SpecEl.Values);
end;

procedure TPasResolver.SpecializeInlineSpecializeExpr(GenEl,
  SpecEl: TInlineSpecializeExpr);
begin
  SpecializeExpr(GenEl,SpecEl);
  SpecializeElExpr(GenEl,SpecEl,GenEl.NameExpr,SpecEl.NameExpr);
  SpecializeElList(GenEl,SpecEl,GenEl.Params,SpecEl.Params,
    true);
end;

procedure TPasResolver.SpecializeProcedureExpr(GenEl, SpecEl: TProcedureExpr);
var
  GenProc: TPasAnonymousProcedure;
  NewClass: TPTreeElement;
begin
  SpecializeExpr(GenEl,SpecEl);
  GenProc:=GenEl.Proc;
  if GenProc=nil then
    RaiseNotYetImplemented(20190808221018,GenEl);
  if not (GenProc is TPasAnonymousProcedure) then
    RaiseNotYetImplemented(20210331224052,GenEl);
  if GenProc.Parent<>GenEl then
    RaiseNotYetImplemented(20210331223856,GenEl);
  NewClass:=TPTreeElement(GenProc.ClassType);
  SpecEl.Proc:=TPasAnonymousProcedure(CreateOwnedElement(NewClass,GenProc.Name,SpecEl));
  SpecializeElement(GenProc,SpecEl.Proc);
end;

procedure TPasResolver.SpecializeResString(GenEl, SpecEl: TPasResString);
begin
  SpecializeElExpr(GenEl,SpecEl,GenEl.Expr,SpecEl.Expr);
  FinishResourcestring(SpecEl);
end;

procedure TPasResolver.SpecializeTypeOfType(GenEl, SpecEl: TPasTypeOfType);
begin
  // the DestType is computed by FinishTypeDef in the specialized context
  SpecializeElExpr(GenEl,SpecEl,GenEl.Expr,SpecEl.Expr);
  FinishTypeDef(SpecEl);
end;

procedure TPasResolver.SpecializeAliasType(GenEl, SpecEl: TPasAliasType);
begin
  SpecializeElType(GenEl,SpecEl,GenEl.DestType,SpecEl.DestType);
  SpecializeElExpr(GenEl,SpecEl,GenEl.Expr,SpecEl.Expr);
  // not needed by specialize: FinishTypeAlias();
  FinishTypeDef(SpecEl);
end;

procedure TPasResolver.SpecializePointerType(GenEl, SpecEl: TPasPointerType);
begin
  SpecializeElType(GenEl,SpecEl,GenEl.DestType,SpecEl.DestType);
  FinishPointerType(SpecEl);
end;

procedure TPasResolver.SpecializeRangeType(GenEl, SpecEl: TPasRangeType);
begin
  SpecializeElExpr(GenEl,SpecEl,GenEl.RangeExpr,TPasExpr(SpecEl.RangeExpr));
  FinishRangeType(SpecEl);
end;

procedure TPasResolver.SpecializeArrayType(GenEl, SpecEl: TPasArrayType;
  SpecializedItem: TPRSpecializedTypeItem);
var
  SpecScope: TPasGenericScope;
begin
  SpecEl.IndexRange:=GenEl.IndexRange;
  SpecEl.PackMode:=GenEl.PackMode;
  if GenEl.GenericTemplateTypes<>nil then
    begin
    SpecScope:=TPasGenericScope(PushScope(SpecEl,ScopeClass_Array));
    if SpecializedItem<>nil then
      begin
      // specialized generic array
      SpecScope.SpecializedFromItem:=SpecializedItem;
      AddSpecializedTemplateIdentifiers(GenEl.GenericTemplateTypes,
                                        SpecializedItem,SpecScope,true);
      end
    else
      begin
      // generic arraytype inside a generic type
      RaiseNotYetImplemented(20190812225218,GenEl);
      end;
    end;
  SpecializeExprArray(GenEl,SpecEl,GenEl.Ranges,SpecEl.Ranges);
  SpecializeElType(GenEl,SpecEl,GenEl.ElType,SpecEl.ElType);
  FinishArrayType(SpecEl);
  if SpecializedItem<>nil then
    SpecializedItem.Step:=prssImplementationFinished;
end;

procedure TPasResolver.SpecializeRecordType(GenEl, SpecEl: TPasRecordType;
  SpecializedItem: TPRSpecializedTypeItem);
var
  SpecScope: TPasGenericScope;
begin
  SpecEl.PackMode:=GenEl.PackMode;
  SpecEl.RTTIVisibility:=GenEl.RTTIVisibility;
  if SpecializedItem<>nil then
    begin
    // specialized generic record
    if SpecEl.CustomData<>nil then
      RaiseNotYetImplemented(20190921204740,SpecEl);
    SpecScope:=TPasGenericScope(PushScope(SpecEl,ScopeClass_Record));
    SpecScope.VisibilityContext:=SpecEl;
    SpecScope.SpecializedFromItem:=SpecializedItem;
    AddSpecializedTemplateIdentifiers(GenEl.GenericTemplateTypes,
                                      SpecializedItem,SpecScope,true);
    if not (msDelphi in CurrentParser.CurrentModeswitches) then
      begin
      // ObjFPC: add canonical type alias
      SpecScope.AddIdentifier(GenEl.Name,SpecEl,pikSimple);
      end;
    end
  else if GenEl.GenericTemplateTypes.Count>0 then
    begin
    // generic recordtype inside a generic type
    if SpecEl.CustomData=nil then
      RaiseNotYetImplemented(20190815201634,SpecEl);
    SpecScope:=TPasGenericScope(SpecEl.CustomData);
    RaiseNotYetImplemented(20190815194327,GenEl);
    end;
  // specialize sub elements
  SpecializeMembers(GenEl,SpecEl);
  // The variant part (case tag field + variant branches) is stored separately
  // from Members and must be specialized too. Done while the record scope is
  // still active (before FinishRecordType pops it).
  SpecializeRecordVariantPart(GenEl,SpecEl);
  FinishRecordType(SpecEl);
  if SpecializedItem<>nil then
    SpecializedItem.Step:=prssInterfaceFinished;
end;

procedure TPasResolver.SpecializeClassType(GenEl, SpecEl: TPasClassType;
  SpecializedItem: TPRSpecializedTypeItem);
var
  HeaderScope: TPasGenericParamsScope;
  TemplType: TPasGenericTemplateType;
  GenericTemplateTypes: TFPList;
  SpecClassScope: TPasClassScope;
begin
  GenericTemplateTypes:=GenEl.GenericTemplateTypes;
  SpecEl.ObjKind:=GenEl.ObjKind;
  SpecEl.PackMode:=GenEl.PackMode;
  SpecEl.RTTIVisibility:=GenEl.RTTIVisibility;
  if GenEl.IsForward then
    RaiseNotYetImplemented(20190730182858,GenEl);
  SpecEl.IsExternal:=GenEl.IsExternal;
  SpecEl.IsShortDefinition:=GenEl.IsShortDefinition;
  if GenEl.GUIDExpr<>nil then
    SpecializeElExpr(GenEl,SpecEl,GenEl.GUIDExpr,SpecEl.GUIDExpr);
  SpecEl.Modifiers.Assign(GenEl.Modifiers);
  SpecEl.ExternalNameSpace:=GenEl.ExternalNameSpace;
  SpecEl.ExternalName:=GenEl.ExternalName;
  SpecEl.InterfaceType:=GenEl.InterfaceType;

  // ancestor+interfaces
  if SpecializedItem<>nil then
    begin
    // ancestor can be a specialized type. For example: = class(TAncestor<T>)
    // -> create a scope with the specialized parameters
    HeaderScope:=TPasGenericParamsScope.Create;
    SpecializedItem.HeaderScope:=HeaderScope;
    TemplType:=TPasGenericTemplateType(GenericTemplateTypes[0]);
    HeaderScope.Element:=TemplType;
    PushScope(HeaderScope);
    AddSpecializedTemplateIdentifiers(GenericTemplateTypes,
                                      SpecializedItem,HeaderScope,true);
    end
  else
    HeaderScope:=nil;
  SpecializeElType(GenEl,SpecEl,
                   GenEl.AncestorType,SpecEl.AncestorType);
  // A helper for a generic type parameter (e.g. a nested `THelper = class helper
  // for T` inside TFoo<T>) needs its HelperForType resolved with the substituted
  // template params (T -> type arg), done here while the HeaderScope is active,
  // exactly like the ancestor above (thlp30).
  if GenEl.HelperForType<>nil then
    SpecializeElType(GenEl,SpecEl,
                     GenEl.HelperForType,SpecEl.HelperForType);
  SpecializeElList(GenEl,SpecEl,
                   GenEl.Interfaces,SpecEl.Interfaces,true);
  if HeaderScope<>nil then
    begin
    if TopScope<>HeaderScope then
      RaiseNotYetImplemented(20190813003056,GenEl);
    PopScope;
    SpecializedItem.HeaderScope:=nil;
    HeaderScope.Free;
    end;

  FinishAncestors(SpecEl);

  if GenEl.Interfaces.Count<>SpecEl.Interfaces.Count then
    RaiseNotYetImplemented(20200601125556,GenEl,IntToStr(GenEl.Interfaces.Count)+'<>'+IntToStr(SpecEl.Interfaces.Count));

  // Note: class scope was created by FinishAncestors
  SpecClassScope:=NoNil(SpecEl.CustomData) as TPasClassScope;

  if SpecClassScope.SpecializedFromItem<>nil then
    RaiseNotYetImplemented(20190816215413,SpecEl);
  if SpecializedItem<>nil then
    begin
    SpecClassScope.SpecializedFromItem:=SpecializedItem;
    AddSpecializedTemplateIdentifiers(GenericTemplateTypes,
                                      SpecializedItem,SpecClassScope,false);
    if not (msDelphi in CurrentParser.CurrentModeswitches) then
      begin
      // ObjFPC: add canonical type alias
      SpecClassScope.AddIdentifier(GenEl.Name,SpecEl,pikSimple);
      end;
    end;
  // specialize sub elements
  SpecializeMembers(GenEl,SpecEl);
  if SpecializedItem<>nil then
    SpecializedItem.Step:=prssInterfaceFinished;
  FinishClassType(SpecEl);
end;

procedure TPasResolver.SpecializeEnumValue(GenEl, SpecEl: TPasEnumValue);
begin
  SpecializeElExpr(GenEl,SpecEl,GenEl.Value,SpecEl.Value);
end;

procedure TPasResolver.SpecializeEnumType(GenEl, SpecEl: TPasEnumType);
begin
  SpecEl.MinSize:=GenEl.MinSize;
  SpecializeElList(GenEl,SpecEl,GenEl.Values,SpecEl.Values,false);
  FinishEnumType(SpecEl);
end;

procedure TPasResolver.SpecializeSetType(GenEl, SpecEl: TPasSetType);
begin
  SpecEl.IsPacked:=GenEl.IsPacked;
  SpecEl.PackSet:=GenEl.PackSet;
  SpecializeElType(GenEl,SpecEl,GenEl.EnumType,SpecEl.EnumType);
  FinishSetType(SpecEl);
end;

procedure TPasResolver.SpecializeVariant(GenEl, SpecEl: TPasVariant);
var
  i: Integer;
begin
  // Case labels (e.g. "one, two") are constant expressions parented to the
  // enclosing record (not the variant) and reference reused, unspecialized
  // constants. Share the references; the two-pass FreeChildren clears non-owned
  // entries (Parent<>variant) so they are not double-freed.
  for i:=0 to GenEl.Values.Count-1 do
    SpecEl.Values.Add(GenEl.Values[i]);
  // The branch's overlay fields live in a nested TPasRecordType; clone and
  // specialize them so field types referencing template params are resolved.
  if GenEl.Members<>nil then
    begin
    SpecEl.Members:=TPasRecordType(CreateOwnedElement(TPTreeElement(GenEl.Members.ClassType),
                                                      GenEl.Members.Name,SpecEl));
    SpecializeMembers(GenEl.Members,SpecEl.Members);
    end;
end;

procedure TPasResolver.SpecializeRecordVariantPart(GenEl, SpecEl: TPasRecordType);
// Specialize a record's variant part (the "case tag:Type of ..." section),
// which lives in VariantEl (tag field/selector type) and Variants (the
// branches), not in Members. fcl-passrc never implemented this (SpecializeVariant
// was a stub); needed e.g. for "specialize TRecArr<Integer>" where TRecArr is
// "array[..] of record case enum:(..) of .. end" (tgeneric27).
var
  i: Integer;
  GenVariant, NewVariant: TPasElement;
begin
  // Tag field / selector type. The selector type (e.g. an anonymous enum) is
  // reused from the generic, not re-cloned (FPC reuses the generic enum
  // definition — see the test comment in tgeneric27). The selector type's
  // Parent stays the generic element, so it is treated as a reference (not
  // owned/freed by the specialized record) — the same convention
  // SpecializeTypeRef uses, hence no refcount bump is needed.
  if GenEl.VariantEl is TPasVariable then
    begin
    SpecEl.VariantEl:=CreateOwnedElement(TPTreeElement(GenEl.VariantEl.ClassType),
                                         GenEl.VariantEl.Name,SpecEl);
    TPasVariable(SpecEl.VariantEl).VarType:=TPasVariable(GenEl.VariantEl).VarType;
    // Register the tag field in the record scope, as normal parsing does via
    // CreateElement->AddVariable, so "rec.<tag>" resolves. TopScope is the
    // specialized record scope here (pushed in SpecializeRecordType).
    AddVariable(TPasVariable(SpecEl.VariantEl));
    // An enum type declared inside a generic does not propagate its values to
    // the enclosing scope at definition time (AddEnumValue stops at a generic).
    // Publish them now (once) so e.g. "A[0].enum := one" resolves.
    PublishGenericEnumValues(TPasVariable(SpecEl.VariantEl).VarType);
    end
  else if GenEl.VariantEl is TPasType then
    begin
    // anonymous "case (..) of" — selector is just a type; reuse it
    SpecEl.VariantEl:=GenEl.VariantEl;
    PublishGenericEnumValues(TPasType(SpecEl.VariantEl));
    end;

  // Variant branches
  if GenEl.Variants<>nil then
    begin
    SpecEl.Variants:=TFPList.Create;
    for i:=0 to GenEl.Variants.Count-1 do
      begin
      GenVariant:=TPasElement(GenEl.Variants[i]);
      NewVariant:=CreateOwnedElement(TPTreeElement(GenVariant.ClassType),
                                     GenVariant.Name,SpecEl);
      SpecEl.Variants.Add(NewVariant);
      SpecializeElement(GenVariant,NewVariant);
      end;
    end;
end;

procedure TPasResolver.PublishGenericEnumValues(EnumType: TPasType);
// Publish the values of an (unscoped) enum type to the nearest section/proc
// scope, once. Used when specializing a generic that declares an enum: such
// enums do not propagate their values at definition time (AddEnumValue breaks
// at a generic), so on specialization the values must become visible at the
// use site. Idempotent: skips values already present (multiple specializations
// of the same generic share the one reused enum).
var
  ET: TPasType;
  EnumT: TPasEnumType;
  i, j: Integer;
  Scope: TPasScope;
  EV: TPasEnumValue;
begin
  if EnumType=nil then exit;
  ET:=ResolveAliasType(EnumType);
  if not (ET is TPasEnumType) then exit;
  if bsScopedEnums in CurrentParser.Scanner.CurrentBoolSwitches then exit;
  EnumT:=TPasEnumType(ET);
  for i:=0 to EnumT.Values.Count-1 do
    begin
    EV:=TPasEnumValue(EnumT.Values[i]);
    for j:=ScopeCount-1 downto 0 do
      begin
      Scope:=Scopes[j];
      if Scope is TPasGroupScope then
        Scope:=TPasGroupScope(Scope).Scopes[0];
      if (Scope is TPasSectionScope) or (Scope is TPasProcedureScope) then
        begin
        if TPasIdentifierScope(Scope).FindLocalIdentifier(EV.Name)=nil then
          TPasIdentifierScope(Scope).AddIdentifier(EV.Name,EV,pikSimple);
        break;
        end;
      end;
    end;
end;

procedure TPasResolver.SpecializeStringType(GenEl, SpecEl: TPasStringType);
begin
  SpecEl.LengthExpr:=GenEl.LengthExpr;
  FinishTypeDef(SpecEl);
end;

procedure TPasResolver.SpecializeAttributes(GenEl, SpecEl: TPasAttributes);
begin
  SpecializeExprArray(GenEl,SpecEl,GenEl.Calls,SpecEl.Calls);
  FinishAttributes(SpecEl);
end;

procedure TPasResolver.SpecializeMethodResolution(GenEl,
  SpecEl: TPasMethodResolution);
begin
  SpecEl.ProcClass:=GenEl.ProcClass;
  SpecializeElExpr(GenEl,SpecEl,GenEl.InterfaceName,SpecEl.InterfaceName);
  SpecializeElExpr(GenEl,SpecEl,GenEl.InterfaceProc,SpecEl.InterfaceProc);
  SpecializeElExpr(GenEl,SpecEl,GenEl.ImplementationProc,SpecEl.ImplementationProc);
  FinishMethodResolution(SpecEl);
end;
