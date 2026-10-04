{%MainUnit pasresolver.pp}
{ ********************************************************************** 
    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - Call compatibility

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }


class function TPasResolver.GetWarnIdentifierNumbers(Identifier: string; out
  MsgNumbers: TIntegerDynArray): boolean;

  procedure SetNumber(Number: integer);
  begin
    {$IF FPC_FULLVERSION>=30101}
    MsgNumbers:=[Number];
    {$ELSE}
    Setlength(MsgNumbers,1);
    MsgNumbers[0]:=Number;
    {$ENDIF}
  end;

  procedure SetNumbers(Numbers: array of integer);
  var
    i: Integer;
  begin
    Setlength(MsgNumbers,length(Numbers));
    for i:=0 to high(Numbers) do
      MsgNumbers[i]:=Numbers[i];
  end;

begin
  if Identifier='' then exit(false);
  if Identifier[1] in ['0'..'9'] then exit(false);

  Result:=true;
  case UpperCase(Identifier) of
  // FPC:
  'CONSTRUCTING_ABSTRACT': SetNumber(nConstructingClassXWithAbstractMethodY); //  Constructing an instance of a class with abstract methods.
  //'IMPLICIT_VARIANTS': ; //  Implicit use of the variants unit.
  // useanalyzer: 'NO_RETVAL': ; // Function result is not set.
  'SYMBOL_DEPRECATED': SetNumber(nSymbolXIsDeprecated); //   Deprecated symbol.
  'SYMBOL_EXPERIMENTAL': SetNumber(nSymbolXIsExperimental); //   Experimental symbol
  'SYMBOL_LIBRARY': SetNumber(nSymbolXBelongsToALibrary); //   Not used.
  'SYMBOL_PLATFORM': SetNumber(nSymbolXIsNotPortable); //   Platform-dependent symbol.
  'SYMBOL_UNIMPLEMENTED': SetNumber(nSymbolXIsNotImplemented); //   Unimplemented symbol.
  //'UNIT_DEPRECATED': ; //   Deprecated unit.
  //'UNIT_EXPERIMENTAL': ; //   Experimental unit.
  //'UNIT_LIBRARY': ; //
  //'UNIT_PLATFORM': ; //   Platform dependent unit.
  //'UNIT_UNIMPLEMENTED': ; //   Unimplemented unit.
  //'ZERO_NIL_COMPAT': ; //   Converting 0 to NIL
  //'IMPLICIT_STRING_CAST': ; // Implicit string type conversion
  //'IMPLICIT_STRING_CAST_LOSS': ; // Implicit string typecast with potential data loss from ”$1” to ”$2”
  //'EXPLICIT_STRING_CAST': ; //   Explicit string type conversion
  //'EXPLICIT_STRING_CAST_LOSS': ; //   Explicit string typecast with potential data loss from ”$1” to ”$2”
  //'CVT_NARROWING_STRING_LOST': ; //   Unicode constant cast with potential data loss

  // Delphi:
  'HIDDEN_VIRTUAL': SetNumber(nMethodHidesMethodOfBaseType); // method hides virtual method of ancestor
  'GARBAGE': SetNumber(nTextAfterFinalIgnored); // text after final end.
  'BOUNDS_ERROR': SetNumbers([nRangeCheckError,
      nHighRangeLimitLTLowRangeLimit,
      nRangeCheckEvaluatingConstantsVMinMax,
      nRangeCheckInSetConstructor]);
  'MESSAGE_DIRECTIVE': SetNumber(nUserDefined); // $message directive
  else
    Result:=false;
  end;
end;

procedure TPasResolver.GetIncompatibleTypeDesc(const GotType,
  ExpType: TPasResolverResult; out GotDesc, ExpDesc: String);
var
  NeedProcSignature: Boolean;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.GetIncompatibleTypeDesc Got={',GetResolverResultDbg(GotType),'} Expected={',GetResolverResultDbg(ExpType),'}');
  {$ENDIF}
  if (GotType.BaseType<>ExpType.BaseType)
      and (GotType.BaseType<>btContext) and (ExpType.BaseType<>btContext) then
    begin
    GotDesc:=GetBaseDescription(GotType);
    if ExpType.BaseType=btNil then
      ExpDesc:=BaseTypeNames[btPointer]
    else
      ExpDesc:=GetBaseDescription(ExpType);
    if GotDesc<>ExpDesc then
      exit;
    GotDesc:=GetBaseDescription(GotType,true);
    ExpDesc:=GetBaseDescription(ExpType,true);
    end
  else if (GotType.LoTypeEl<>nil) and (ExpType.LoTypeEl<>nil) then
    begin
    NeedProcSignature:=(GotType.LoTypeEl is TPasProcedureType)
                   and (ExpType.LoTypeEl is TPasProcedureType);
    if NeedProcSignature then
      begin
      // procedural types
      GetIncompatibleProcParamsDesc(TPasProcedureType(GotType.LoTypeEl),
        TPasProcedureType(ExpType.LoTypeEl),GotDesc,ExpDesc);
      if GotDesc<>ExpDesc then
        exit;
      end;
    GotDesc:=GetTypeDescription(GotType);
    ExpDesc:=GetTypeDescription(ExpType);
    if GotDesc<>ExpDesc then
      exit;
    if GotType.HiTypeEl<>ExpType.HiTypeEl then
      begin
      GotDesc:=GetTypeDescription(GotType.HiTypeEl);
      ExpDesc:=GetTypeDescription(ExpType.HiTypeEl);
      if GotDesc<>ExpDesc then
        exit;
      end;
    GotDesc:=GetTypeDescription(GotType,true);
    ExpDesc:=GetTypeDescription(ExpType,true);
    end
  else
    begin
    GotDesc:=GetResolverResultDescription(GotType,true);
    ExpDesc:=GetResolverResultDescription(ExpType,true);
    if GotDesc<>ExpDesc then
      exit;
    GotDesc:=GetResolverResultDescription(GotType,false);
    ExpDesc:=GetResolverResultDescription(ExpType,false);
    end;
end;

procedure TPasResolver.GetIncompatibleTypeDesc(const GotType,
  ExpType: TPasType; out GotDesc, ExpDesc: String);
var
  GotLoType, ExpLoType: TPasType;
begin
  GotLoType:=ResolveAliasType(GotType);
  ExpLoType:=ResolveAliasType(ExpType);
  if (GotLoType<>nil) and (ExpLoType<>nil) then
    begin
    if (GotLoType.ClassType=ExpLoType.ClassType)
        and (GotLoType is TPasProcedureType) then
      begin
      // procedural types
      GetIncompatibleProcParamsDesc(TPasProcedureType(GotLoType),
        TPasProcedureType(ExpLoType),GotDesc,ExpDesc);
      if GotDesc<>ExpDesc then
        exit;
      end;
    end;
  GotDesc:=GetTypeDescription(GotType);
  ExpDesc:=GetTypeDescription(ExpType);
  if GotDesc<>ExpDesc then exit;
  GotDesc:=GetTypeDescription(GotType,true);
  ExpDesc:=GetTypeDescription(ExpType,true);
end;

procedure TPasResolver.GetIncompatibleProcParamsDesc(GotType,
  ExpType: TPasProcedureType; out GotDesc, ExpDesc: string);

  procedure AppendClass(ProcType: TPasProcedureType; var Desc: string);
  var
    C: TClass;
  begin
    C:=ProcType.ClassType;
    if C=TPasProcedureType then
      Desc:=Desc+'procedure'
    else if C=TPasFunctionType then
      Desc:=Desc+'function'
    else
      RaiseNotYetImplemented(20200216114419,ProcType,ProcType.ClassName);
  end;

var
  i: Integer;
  GotArg, ExpArg: TPasArgument;
  GotArgs, ExpArgs: TFPList;
  GotArgDesc, ExpArgDesc: String;
  GotArgType, ExpArgType: TPasType;
begin
  GotDesc:='';
  ExpDesc:='';
  // reference to
  if (ptmReferenceTo in GotType.Modifiers) and not (ptmReferenceTo in ExpType.Modifiers) then
    GotDesc:='reference to '
  else if not (ptmReferenceTo in GotType.Modifiers) and (ptmReferenceTo in ExpType.Modifiers) then
    ExpDesc:='reference to ';

  // type
  AppendClass(GotType,GotDesc);
  AppendClass(ExpType,ExpDesc);

  // Args
  GotDesc:=GotDesc+'(';
  ExpDesc:=ExpDesc+'(';
  GotArgs:=GotType.Args;
  ExpArgs:=ExpType.Args;
  for i:=0 to GotArgs.Count-1 do
    begin
    if i>0 then
      GotDesc:=GotDesc+';';
    GotArg:=TPasArgument(GotArgs[i]);
    GotArgType:=ResolveAliasType(GotArg.ArgType);
    if i<ExpArgs.Count then
      begin
      if i>0 then
        ExpDesc:=ExpDesc+';';
      ExpArg:=TPasArgument(ExpArgs[i]);
      ExpArgType:=ResolveAliasType(ExpArg.ArgType);
      if GotArgType=ExpArgType then
        begin
        GotDesc:=GotDesc+GetTypeDescription(GotArgType);
        ExpDesc:=ExpDesc+GetTypeDescription(ExpArgType);
        end
      else
        begin
        GetIncompatibleTypeDesc(GotArgType,ExpArgType,GotArgDesc,ExpArgDesc);
        GotDesc:=GotDesc+GotArgDesc;
        ExpDesc:=ExpDesc+ExpArgDesc;
        end;
      end
    else
      begin
      // GotType has more args than ExpType
      GotDesc:=GotDesc+GetTypeDescription(GotArgType);
      end;
    end;
  for i:=GotArgs.Count to ExpArgs.Count-1 do
    begin
    // ExpType has more args then GotType
    if i>0 then
      ExpDesc:=ExpDesc+';';
    ExpArg:=TPasArgument(ExpArgs[i]);
    ExpArgType:=ResolveAliasType(ExpArg.ArgType);
    ExpDesc:=ExpDesc+GetTypeDescription(ExpArgType);
    end;
  GotDesc:=GotDesc+')';
  ExpDesc:=ExpDesc+')';

  // function result
  if GotType is TPasFunctionType then
    GotDesc:=GotDesc+': '+GetTypeDescription(ResolveAliasType(TPasFunctionType(GotType).ResultEl.ResultType));
  if ExpType is TPasFunctionType then
    ExpDesc:=ExpDesc+': '+GetTypeDescription(ResolveAliasType(TPasFunctionType(ExpType).ResultEl.ResultType));

  // modifiers
  if (ptmOfObject in GotType.Modifiers) and not (ptmOfObject in ExpType.Modifiers) then
    GotDesc:=GotDesc+' of Object'
  else if not (ptmOfObject in GotType.Modifiers) and (ptmOfObject in ExpType.Modifiers) then
    ExpDesc:=ExpDesc+' of Object';
  if (ptmIsNested in GotType.Modifiers) and not (ptmIsNested in ExpType.Modifiers) then
    GotDesc:=GotDesc+' is nested'
  else if not (ptmIsNested in GotType.Modifiers) and (ptmIsNested in ExpType.Modifiers) then
    ExpDesc:=ExpDesc+' is nested';
  if (ptmStatic in GotType.Modifiers) and not (ptmStatic in ExpType.Modifiers) then
    GotDesc:=GotDesc+'; static'
  else if not (ptmStatic in GotType.Modifiers) and (ptmStatic in ExpType.Modifiers) then
    ExpDesc:=ExpDesc+'; static';
  if (ptmAsync in GotType.Modifiers) and not (ptmAsync in ExpType.Modifiers) then
    GotDesc:=GotDesc+'; async'
  else if not (ptmAsync in GotType.Modifiers) and (ptmAsync in ExpType.Modifiers) then
    ExpDesc:=ExpDesc+'; async';
  if (ptmVarargs in GotType.Modifiers) and not (ptmVarargs in ExpType.Modifiers) then
    GotDesc:=GotDesc+'; varargs'
  else if not (ptmVarargs in GotType.Modifiers) and (ptmVarargs in ExpType.Modifiers) then
    ExpDesc:=ExpDesc+'; varargs'
  else
    begin
    if GotType.VarArgsType<>nil then
      GotDesc:=GotDesc+'; varargs of '+GetTypeDescription(ResolveAliasType(GotType.VarArgsType));
    if ExpType.VarArgsType<>nil then
      ExpDesc:=ExpDesc+'; varargs of '+GetTypeDescription(ResolveAliasType(ExpType.VarArgsType));
    end;

  // calling convention
  if GotType.CallingConvention<>ExpType.CallingConvention then
    begin
    GotDesc:=GotDesc+';'+cCallingConventions[GotType.CallingConvention];
    ExpDesc:=ExpDesc+';'+cCallingConventions[ExpType.CallingConvention];
    end;

  if GotDesc=ExpDesc then
    begin
    if GotType.Parent is TPasAnonymousProcedure then
      GotDesc:='anonymous '+GotDesc;
    if ExpType.Parent is TPasAnonymousProcedure then
      ExpDesc:='anonymous '+ExpDesc;
    end;
end;

function TPasResolver.CheckCallProcCompatibility(ProcType: TPasProcedureType;
  Params: TParamsExpr; RaiseOnError: boolean; SetReferenceFlags: boolean
  ): integer;
var
  ProcArgs: TFPList;
  i, ParamCnt, ParamCompatibility: Integer;
  Param, Value: TPasExpr;
  ParamResolved, ArgResolved: TPasResolverResult;
  Flags: TPasResolverComputeFlags;
begin
  Result:=cExact;
  ProcArgs:=ProcType.Args;

  Value:=Params.Value;
  if Value is TBinaryExpr then
    Value:=TBinaryExpr(Value).Right; // Note: parser guarantees that this is the rightmost

  // check args
  ParamCnt:=length(Params.Params);
  ArgResolved.BaseType:=btNone;
  i:=0;
  while i<ParamCnt do
    begin
    Param:=Params.Params[i];
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckCallProcCompatibility ',i,'/',ParamCnt);
    {$ENDIF}
    if i<ProcArgs.Count then
      begin
      ParamCompatibility:=CheckParamCompatibility(Param,
        TPasArgument(ProcArgs[i]),i,RaiseOnError,SetReferenceFlags);
      if ParamCompatibility=cIncompatible then
        exit(cIncompatible);
      end
    else
      begin
      if ptmVarargs in ProcType.Modifiers then
        begin
        if ProcType.VarArgsType<>nil then
          begin
          if ArgResolved.BaseType=btNone then
            ComputeElement(ProcType.VarArgsType,ArgResolved,[rcType]);
          ComputeArgumentExpr(ArgResolved,argConst,
                                 Param,ParamResolved,SetReferenceFlags);
          ParamCompatibility:=CheckParamResCompatibility(Param,ParamResolved,
                                   ArgResolved,i,RaiseOnError,SetReferenceFlags);
          if ParamCompatibility=cIncompatible then
            exit(cIncompatible);
          end
        else
          begin
          if SetReferenceFlags then
            Flags:=[rcNoImplicitProcType,rcSetReferenceFlags]
          else
            Flags:=[rcNoImplicitProcType];
          ComputeElement(Param,ParamResolved,Flags,Param);
          if not (rrfReadable in ParamResolved.Flags) then
            begin
            if RaiseOnError then
              RaiseVarExpected(20180712001415,Param,ParamResolved.IdentEl);
            exit(cIncompatible);
            end;
          // an argument taken by `varargs` ranks like an untyped parameter:
          // gtk2's gtk_signal_new(..., n_args; args: array of const) wins
          // over its `varargs` twin, as in ppcx64
          ParamCompatibility:=cUntypedParam;
          end;
        end
      else
        begin
        // too many arguments
        if RaiseOnError then
          RaiseMsg(20170216152408,nWrongNumberOfParametersForCallTo,
            sWrongNumberOfParametersForCallTo,[GetProcTypeDescription(ProcType)],Param);
        exit(cIncompatible);
        end;
      end;
    // Accumulate per-argument distances across ALL bands - capping the running
    // total makes a later argument undecidable (variants.pp DumpVariant, where
    // only the 3rd argument separates the candidates).
    // A cost inside a conversion BAND is first normalised to that band's own
    // value, so two candidates that are both lossy on the same argument stay
    // indistinguishable there and the call remains ambiguous (tover3), while an
    // argument on which one candidate is exact and the other is not still
    // separates them.
    // an int-to-float cost may lie just below its band value (ord(LBT)-ord(RBT)<0)
    if ParamCompatibility>=cIntToFloatConversion-1000 then
      ParamCompatibility:=cIntToFloatConversion
    else if ParamCompatibility>=cLossyConversion then
      ParamCompatibility:=cLossyConversion;
    if ParamCompatibility>=cIncompatible-Result then
      Result:=cIncompatible-1
    else
      inc(Result,ParamCompatibility);
    inc(i);
    end;
  if (i<ProcArgs.Count) then
    if (TPasArgument(ProcArgs[i]).ValueExpr=nil) then
      begin
      // not enough arguments
      if RaiseOnError then
        // ToDo: position cursor on identifier
        RaiseMsg(20170216152410,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[GetProcTypeDescription(ProcType)],Params.Value);
      exit(cIncompatible);
      end
    else
      begin
      // the rest are default params
      end;
end;

function TPasResolver.CheckExplicitSpecBareTemplateArgs(Proc: TPasProcedure;
  TemplParams: TFPList; Params: TParamsExpr): boolean;
// For an explicit-spec call "specialize Proc<T1,...>(args)", each proc parameter
// whose declared type is a BARE template parameter Tk has, under the explicit
// specialization, the concrete type TemplParams[k]. CheckCallProcCompatibility
// scores the unspecialized signature, so such a param is a catch-all
// (cGenericExact) and e.g. an array/set literal wrongly matches a scalar
// "const T", tying with "const array of T". Verify each bare-template param
// actually accepts its argument under the explicit substitution, WITHOUT
// creating a persistent specialization (GetSpecializedEl would emit dead
// specialization code, e.g. surplus functions in pas2js). Returns False if any
// bare-template param rejects its argument.
var
  Templates, ProcArgs: TFPList;
  i, k: Integer;
  ArgType: TPasType;
  ConcreteResolved, ArgResolved: TPasResolverResult;
  ArgExpr: TPasExpr;
begin
  Result:=true;
  if TemplParams=nil then exit;
  Templates:=GetProcTemplateTypes(Proc);
  if (Templates=nil) or (Templates.Count=0) then exit;
  ProcArgs:=Proc.ProcType.Args;
  for i:=0 to length(Params.Params)-1 do
    begin
    if i>=ProcArgs.Count then break;
    if TPasArgument(ProcArgs[i]).ArgType=nil then continue; // untyped
    ArgType:=ResolveAliasType(TPasArgument(ProcArgs[i]).ArgType);
    if not (ArgType is TPasGenericTemplateType) then continue;
    k:=Templates.IndexOf(ArgType);
    if (k<0) or (k>=TemplParams.Count) then continue;
    // concrete type = the explicit specialization argument for Tk
    ComputeElement(TPasElement(TemplParams[k]),ConcreteResolved,[rcType]);
    if (ConcreteResolved.BaseType=btContext)
        and (ConcreteResolved.LoTypeEl is TPasGenericTemplateType) then
      continue; // still generic (nested), cannot decide here
    ArgExpr:=Params.Params[i];
    ComputeElement(ArgExpr,ArgResolved,[]);
    if CheckParamResCompatibility(ArgExpr,ArgResolved,ConcreteResolved,i,false,false)=cIncompatible then
      exit(false);
    end;
end;

function TPasResolver.CheckCallPropertyCompatibility(PropEl: TPasProperty;
  Params: TParamsExpr; RaiseOnError: boolean): integer;
var
  PropArg: TPasArgument;
  ArgNo, ParamComp: Integer;
  Param: TPasExpr;
  PropArgs: TFPList;
begin
  Result:=cExact;
  PropArgs:=GetPasPropertyArgs(PropEl);
  // FPC calls the accessor overload that fits the brackets, whatever the
  // property's declared index count: lazutils' `ItemPointerFast[Cnt, Cap, MPtr]`.
  if (PropArgs.Count<>length(Params.Params))
      and ((GetPropertyAccessorForParams(PropEl,Params,false)<>GetPasPropertyGetter(PropEl))
        or (GetPropertyAccessorForParams(PropEl,Params,true)<>GetPasPropertySetter(PropEl))) then
    exit(cCompatible);
  if PropArgs.Count<length(Params.Params) then
    begin
    if not RaiseOnError then exit(cIncompatible);
    RaiseMsg(20170216152412,nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
      [PropEl.Name],Params)
    end
  else if PropArgs.Count>length(Params.Params) then
    begin
    if not RaiseOnError then exit(cIncompatible);
    RaiseMsg(20170216152413,nMissingParameterX,sMissingParameterX,
      [TPasArgument(PropArgs[length(Params.Params)]).Name],Params);
    end;
  for ArgNo:=0 to PropArgs.Count-1 do
    begin
    PropArg:=TPasArgument(PropArgs[ArgNo]);
    Param:=Params.Params[ArgNo];
    ParamComp:=CheckParamCompatibility(Param,PropArg,ArgNo,false);
    if ParamComp=cIncompatible then
      begin
      // an overloaded accessor may still fit these brackets
      if (GetPropertyAccessorForParams(PropEl,Params,false)<>GetPasPropertyGetter(PropEl))
          or (GetPropertyAccessorForParams(PropEl,Params,true)<>GetPasPropertySetter(PropEl)) then
        exit(cCompatible);
      if RaiseOnError then
        CheckParamCompatibility(Param,PropArg,ArgNo,true);
      exit(cIncompatible);
      end;
    inc(Result,ParamComp);
    end;
end;

function TPasResolver.CheckCallArrayCompatibility(ArrayEl: TPasArrayType;
  Params: TParamsExpr; RaiseOnError: boolean; EmitHints: boolean): integer;
var
  ArgNo: Integer;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;

  procedure GetNextParam;
  begin
    if ArgNo>=length(Params.Params) then
      RaiseMsg(20170216152415,nWrongNumberOfParametersForArray,sWrongNumberOfParametersForArray,
        [],Params);
    Param:=Params.Params[ArgNo];
    ComputeElement(Param,ParamResolved,[]);
    inc(ArgNo);
  end;

  function IsStringElType(aType: TPasType): boolean;
  var
    ElResolved: TPasResolverResult;
  begin
    Result:=false;
    if aType=nil then exit;
    ComputeElement(aType,ElResolved,[rcType]);
    Result:=ElResolved.BaseType in btAllStrings;
  end;

var
  DimNo: integer;
  RangeResolved, OrigRangeResolved, OrigParamResolved: TPasResolverResult;
  bt: TResolverBaseType;
  NextType, TypeEl: TPasType;
  RangeExpr: TPasExpr;
  TypeFits: Boolean;
  ParamValue: TResEvalValue;
begin
  ArgNo:=0;
  repeat
    if length(ArrayEl.Ranges)=0 then
      begin
      // dynamic/open array -> needs exactly one integer
      GetNextParam;
      // An array slice arr[a..b] on a dynamic/open array passes a pekRange: it
      // denotes a sub-array (the whole open array), so accept it here rather
      // than demanding a single integer index.
      if (Param is TBinaryExpr) and (TBinaryExpr(Param).Kind=pekRange) then
        begin
        if ArgNo=length(Params.Params) then exit(cExact);
        continue;
        end;
      if (not (rrfReadable in ParamResolved.Flags))
          or not (ParamResolved.BaseType in btAllInteger) then
        exit(CheckRaiseTypeArgNo(20170216152417,ArgNo,Param,ParamResolved,'integer',RaiseOnError));
      if EmitHints then
        begin
        ParamValue:=Eval(Param,[refAutoConstExt]);
        if ParamValue<>nil then
          try // has const value -> check range
            if ParamValue.Kind=revkExternal then
              // ignore
            else if (ParamValue.Kind<>revkInt)
                or (TResEvalInt(ParamValue).Int<DynArrayMinIndex)
                or (TResEvalInt(ParamValue).Int>DynArrayMaxIndex) then
              fExprEvaluator.EmitRangeCheckConst(20170520202212,ParamValue.AsString,
                                  DynArrayMinIndex,DynArrayMaxIndex,Param);
          finally
            ReleaseEvalValue(ParamValue);
          end;
        end;
      end
    else
      begin
      // static array
      for DimNo:=0 to length(ArrayEl.Ranges)-1 do
        begin
        // Partial index: fewer subscripts than dimensions is valid — it yields
        // a sub-array (row) of the remaining dimensions, e.g. a[0] on
        // array[0..2,0..2]. Accept it here; ComputeArrayParams computes the
        // sub-array result type.
        if ArgNo>=length(Params.Params) then
          exit(cExact);
        GetNextParam;
        RangeExpr:=ArrayEl.Ranges[DimNo];
        // A bound that still depends on a type parameter cannot be computed
        // here; the index is checked when the array is specialized.
        if ExprNeedsSpecialization(RangeExpr) then
          continue;
        ComputeElement(RangeExpr,RangeResolved,[]);
        bt:=RangeResolved.BaseType;
        if not (rrfReadable in ParamResolved.Flags) then
          begin
          if not RaiseOnError then exit(cIncompatible);
          RaiseIncompatibleTypeRes(20170216152421,nIncompatibleTypeArgNo,
            [IntToStr(ArgNo)],ParamResolved,RangeResolved,Param);
          end;
        TypeFits:=false;
        OrigRangeResolved:=RangeResolved;
        OrigParamResolved:=ParamResolved;

        if bt=btRange then
          begin
          ConvertRangeToElement(RangeResolved);
          bt:=RangeResolved.BaseType;
          end;
        if ParamResolved.BaseType=btRange then
          begin
          ConvertRangeToElement(ParamResolved);
          end;

        if (bt in btAllBooleans) then
          begin
          if (ParamResolved.BaseType in btAllBooleans) then
            TypeFits:=true;
          end
        else if (bt in btAllInteger) then
          begin
          if (ParamResolved.BaseType in btAllInteger) then
            TypeFits:=true;
          end
        else if (bt in btAllChars) then
          begin
          if (ParamResolved.BaseType in btAllChars) then
            TypeFits:=true;
          end
        else if (bt=btContext) then
          begin
          TypeEl:=RangeResolved.LoTypeEl;
          if ParamResolved.BaseType=btContext then
            begin
            if (TypeEl.ClassType=TPasEnumType)
                and IsSameType(TypeEl,ParamResolved.LoTypeEl,prraNone) then
              TypeFits:=true;
            end;
          end;
        if not TypeFits then
          begin
          // incompatible
          if not RaiseOnError then exit(cIncompatible);
          RaiseIncompatibleTypeRes(20170216152422,nIncompatibleTypeArgNo,
            [IntToStr(ArgNo)],OrigParamResolved,OrigRangeResolved,Param);
          end;
        // An array slice arr[a..b] passes a pekRange as the index — it denotes a
        // sub-array, not a single element, so the single-value range check
        // (IsInRange evaluates the range value → RaiseNotYetImplemented) must be
        // skipped. ComputeArrayParams already yields the open-array result type.
        if EmitHints
            and not ((Param is TBinaryExpr) and (TBinaryExpr(Param).Kind=pekRange)) then
          fExprEvaluator.IsInRange(Param,RangeExpr,true);
        end;
      end;
    if ArgNo=length(Params.Params) then exit(cExact);

    // there are more parameters -> continue in sub array
    NextType:=ResolveAliasType(ArrayEl.ElType);
    if (NextType=nil) or (NextType.ClassType<>TPasArrayType) then
      begin
      // An array of string takes one extra index: a[i,j] means a[i][j], the
      // j-th character of the string element. Both FPC and Delphi allow this.
      if (ArgNo=length(Params.Params)-1) and IsStringElType(NextType) then
        begin
        GetNextParam;
        if (not (rrfReadable in ParamResolved.Flags))
            or not (ParamResolved.BaseType in btAllInteger) then
          exit(CheckRaiseTypeArgNo(20260803141500,ArgNo,Param,ParamResolved,'integer',RaiseOnError));
        exit(cExact);
        end;
      RaiseMsg(20170216152424,nWrongNumberOfParametersForArray,sWrongNumberOfParametersForArray,
        [],Params);
      end;
    ArrayEl:=TPasArrayType(NextType);
  until false;
  Result:=cIncompatible;
end;

function TPasResolver.CheckProcOverloadCompatibility(Proc1, Proc2: TPasProcedure): boolean;
// returns if number and type of arguments fit
// does not check calling convention

  function SameSignatureArg(Arg1, Arg2: TPasArgument): boolean;
  // Two spellings of the SAME type are one signature, not two: an implementation
  // may write an argument differently from its forward (iconvenc_dyn declares
  // `var error: AnsiString` and implements `var error: string`), and ppcx64
  // equally rejects two overloads differing only that way as a duplicate.
  // AnsiString vs RawByteString also ranks cAliasExact, but those two are
  // DISTINCT types (different static codepage) that ppcx64 does let overload —
  // our own system unit's GetDir relies on it — so they are excluded here.
  function DistinctAliasOf(El: TPasType): TPasType;
  // The `type X = type Y` link a type reference goes through, if any. Such a type
  // is DISTINCT from what it aliases, so it overloads against it.
  begin
    Result:=nil;
    while El<>nil do
      begin
      if El is TPasTypeAliasType then exit(El);
      if El.ClassType=TPasAliasType then
        El:=TPasAliasType(El).DestType
      else
        exit;
      end;
  end;

  var
    Comp: Integer;
    Res1, Res2: TPasResolverResult;
  begin
    Comp:=CheckProcArgCompatibility(Arg1,Arg2);
    if Comp=cExact then exit(true);
    // cGenericExact is deliberately NOT accepted: `T` and `array of T` rank that
    // way against each other and ARE two distinct overloads (tgenfunc23).
    if Comp<>cAliasExact then exit(false);
    // Two arguments reaching different `type X = type Y` declarations are two
    // distinct types: FPC's UTF8String is `type AnsiString`, and webidl overloads
    // GetResolvedType on String and on UTF8String.
    if DistinctAliasOf(Arg1.ArgType)<>DistinctAliasOf(Arg2.ArgType) then
      exit(false);
    ComputeElement(Arg1.ArgType,Res1,[rcType]);
    ComputeElement(Arg2.ArgType,Res2,[rcType]);
    // Only the STRING/CHAR aliases collapse. Extended also ranks cAliasExact
    // because this target has no 80-bit type and aliases it to Double, but on
    // real x86_64 FPC the two ARE distinct and do overload (tover4).
    if not (Res1.BaseType in btAllStringAndChars)
        or not (Res2.BaseType in btAllStringAndChars) then
      exit(false);
    Result:=true;
    {$ifdef FPC_HAS_CPSTRING}
    // AnsiString vs RawByteString differ in static codepage: distinct types that
    // ppcx64 does let overload — our own system unit's GetDir relies on it.
    if (Res1.BaseType in [btAnsiString,btRawByteString])
        and (Res2.BaseType in [btAnsiString,btRawByteString])
        and (Res1.BaseType<>Res2.BaseType) then
      Result:=false;
    {$endif}
  end;

var
  ProcArgs1, ProcArgs2, TemplTypes1, TemplTypes2: TFPList;
  i: Integer;
  ResultResolved1, ResultResolved2: TPasResolverResult;
begin
  Result:=false;

  if (Proc1.NameParts<>nil) or (Proc2.NameParts<>nil) then
    begin
    TemplTypes1:=GetProcTemplateTypes(Proc1);
    TemplTypes2:=GetProcTemplateTypes(Proc2);
    if TemplTypes1=nil then
      begin
      if TemplTypes2<>nil then
        exit;
      end
    else if TemplTypes2=nil then
      exit
    else if TemplTypes1.Count<>TemplTypes2.Count then
      exit;
    end;

  ProcArgs1:=Proc1.ProcType.Args;
  ProcArgs2:=Proc2.ProcType.Args;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckProcOverloadCompatibility START Count=',ProcArgs1.Count,' ',ProcArgs2.Count);
  {$ENDIF}
  // check args
  if ProcArgs1.Count<>ProcArgs2.Count then
    exit;
  for i:=0 to ProcArgs1.Count-1 do
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckProcOverloadCompatibility ',i,'/',ProcArgs1.Count);
    {$ENDIF}
    if not SameSignatureArg(TPasArgument(ProcArgs1[i]),TPasArgument(ProcArgs2[i])) then
      exit;
    end;
  // Conversion operators (Implicit/Explicit) overload on their result type in
  // addition to their arguments: two of them with matching arguments but
  // different result types are distinct overloads, not a duplicate.
  if (Proc1 is TPasOperator) and (Proc2 is TPasOperator)
      and (TPasOperator(Proc1).OperatorType=TPasOperator(Proc2).OperatorType)
      and (TPasOperator(Proc1).OperatorType in [otImplicit,otExplicit])
      and (Proc1.ProcType is TPasFunctionType)
      and (Proc2.ProcType is TPasFunctionType) then
    begin
    ComputeElement(TPasFunctionType(Proc1.ProcType).ResultEl.ResultType,ResultResolved1,[rcType]);
    ComputeElement(TPasFunctionType(Proc2.ProcType).ResultEl.ResultType,ResultResolved2,[rcType]);
    if ResultResolved1.LoTypeEl<>ResultResolved2.LoTypeEl then
      exit;
    end;
  Result:=true;
end;

function TPasResolver.CheckProcTypeCompatibility(Proc1,
  Proc2: TPasProcedureType; IsAssign: boolean; ErrorEl: TPasElement;
  RaiseOnIncompatible: boolean): boolean;
// if RaiseOnIncompatible=true, then Expected=Proc1 Actual=Proc2

  function ModifierError(Modifier: TProcTypeModifier): boolean;
  begin
    Result:=false;
    if not RaiseOnIncompatible then exit;
    RaiseMsg(20170402112049,nXModifierMismatchY,sXModifierMismatchY,
      [GetElementTypeName(Proc1),ProcTypeModifiers[Modifier]],ErrorEl);
  end;

var
  ProcArgs1, ProcArgs2: TFPList;
  i: Integer;
  ExpectedArg, ActualArg: TPasArgument;
  ResultType1, ResultType2: TPasType;
begin
  Result:=false;
  if Proc1.ClassType<>Proc2.ClassType then
    begin
    if RaiseOnIncompatible then
      RaiseXExpectedButYFound(20170402112353,GetElementTypeName(Proc1),GetElementTypeName(Proc2),ErrorEl);
    exit;
    end;
  if Proc1.IsReferenceTo then
    begin
    if IsAssign then
      // aRefTo:=aproc -> any IsNested/OfObject is allowed
    else
      ; // aRefTo = AnyProc -> ok
    end
  else if Proc2.IsReferenceTo then
    begin
    if IsAssign then
      // NonRefTo := aRefTo  -> not possible
      exit(ModifierError(ptmReferenceTo))
    else
      ; // AnyProc = aRefTo -> ok
    end
  else if Proc2.Parent is TPasAnonymousProcedure then
    begin
    // Anonymous procs are compatible with any target proc type
    // (plain, of object, is nested, reference to).
    // Actual codegen adjusts based on captures.
    end
  else
    begin
    // neither Proc1 nor Proc2 is a reference-to  -> check isNested and OfObject
    if Proc1.IsNested<>Proc2.IsNested then
      exit(ModifierError(ptmIsNested));
    if Proc1.IsOfObject<>Proc2.IsOfObject then
      begin
      if (proProcTypeWithoutIsNested in Options) then
        exit(ModifierError(ptmOfObject))
      else if Proc1.IsNested then
        // "is nested" can handle both, proc and method.
      else
        exit(ModifierError(ptmOfObject))
      end;
    end;
  if Proc1.CallingConvention<>Proc2.CallingConvention then
    begin
    if (proSafecallAllowsDefault in Options)
        and (Proc1.CallingConvention=ccSafeCall)
        and (Proc2.CallingConvention=ccDefault) then
      // ok
    else
      begin
      if RaiseOnIncompatible then
        RaiseMsg(20170402112253,nCallingConventionMismatch,sCallingConventionMismatch,
          [],ErrorEl);
      exit;
      end;
    end;
  ProcArgs1:=Proc1.Args;
  ProcArgs2:=Proc2.Args;
  if ProcArgs1.Count<>ProcArgs2.Count then
    begin
    if RaiseOnIncompatible then
      RaiseMsg(20170902142829,nIncompatibleTypesGotParametersExpected,
        sIncompatibleTypesGotParametersExpected,
        [IntToStr(ProcArgs1.Count),IntToStr(ProcArgs2.Count)],ErrorEl);
    exit;
    end;
  for i:=0 to ProcArgs1.Count-1 do
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckProcTypeCompatibility ',i,'/',ProcArgs1.Count);
    {$ENDIF}
    ExpectedArg:=TPasArgument(ProcArgs1[i]);
    ActualArg:=TPasArgument(ProcArgs2[i]);
    if not ProcArgsMatchForSignature(ExpectedArg,ActualArg) then
      begin
      if RaiseOnIncompatible then
        begin
        if ExpectedArg.Access<>ActualArg.Access then
          RaiseMsg(20170404151541,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(i+1),'access modifier '+AccessDescriptions[ActualArg.Access],
             AccessDescriptions[ExpectedArg.Access]],
            ErrorEl);
        RaiseIncompatibleType(20170404151538,nIncompatibleTypeArgNo,
          [IntToStr(i+1)],ExpectedArg.ArgType,ActualArg.ArgType,ErrorEl);
        end;
      exit;
      end;
    end;
  if Proc1 is TPasFunctionType then
    begin
    ResultType1:=TPasFunctionType(Proc1).ResultEl.ResultType;
    ResultType2:=TPasFunctionType(Proc2).ResultEl.ResultType;
    // cAliasExact is "the same actual type under another name" - $H+ String vs
    // AnsiString, or two AnsiString-family types differing only in codepage. An
    // override may restate its result that way, and rtl-objpas' rtti.pp and
    // fcl-base's streamex both do (`function ToString: String; override;` over
    // TObject.ToString).
    // A WideString result stands in for a UnicodeString one and back, for the
    // same reason as the parameters above (fpwidestring.pp's UpperUnicodeString
    // fills the manager's UpperWideStringProc slot).
    if (CheckElTypeCompatibility(ResultType1,ResultType2,prraSimple)>cAliasExact)
        and not TypesMatchAsWideVsUnicodeString(ResultType1,ResultType2)
        and not ResultsMatchAsAnsiStrings(ResultType1,ResultType2)
        and not IntegerTypeFitsSameStorage(ResultType2,ResultType1) then
      begin
      if RaiseOnIncompatible then
        begin
        RaiseIncompatibleType(20170402112648,nResultTypeMismatchExpectedButFound,
          [],ResultType1,ResultType2,ErrorEl);
        end;
      exit;
      end;

    if Proc1.IsAsync<>Proc2.IsAsync then
      RaiseMsg(20200524112519,nXModifierMismatchY,sXModifierMismatchY,['procedure type','async'],ErrorEl);
    end;
  Result:=true;
end;

function SameArgAccess(A1, A2: TArgumentAccess): boolean;
// Whether two parameter access modifiers make the same parameter list.

begin
  Result:=A1=A2;
  if Result then exit;
  Result:=(A1 in [argConst,argConstRef]) and (A2 in [argConst,argConstRef]);
end;


function TPasResolver.CheckProcArgCompatibility(Arg1, Arg2: TPasArgument
  ): integer;
begin
  // check access: var, const, ...
  // const and constref are ONE parameter list for ppcx64: it refuses to overload
  // them ("Overloaded functions have the same parameter list") and it matches an
  // interface declaration to an implementation that swaps them - fcl-net's
  // fpsockets declares `generic function SendTo<T>(constref ReceiverAddr; const
  // AData: T)` and implements it with the two the other way round.
  if not SameArgAccess(Arg1.Access,Arg2.Access) then exit(cIncompatible);

  Result:=CheckElTypeCompatibility(Arg1.ArgType,Arg2.ArgType,prraSimple);
end;

function TPasResolver.TypesMatchAsWideVsUnicodeString(T1, T2: TPasType
  ): boolean;
// True when the two types are the WideString/UnicodeString pair on a target that
// stores them identically. See ProcArgsMatchForSignature for why this is kept
// out of the general compatibility ranking.
var
  Res1, Res2: TPasResolverResult;
begin
  Result:=false;
  if (T1=nil) or (T2=nil) then exit;
  ComputeElement(T1,Res1,[rcType]);
  ComputeElement(T2,Res2,[rcType]);
  Result:=WideStringMatchesUnicodeString(Res1.BaseType,Res2.BaseType);
end;

function TPasResolver.ResultsMatchAsAnsiStrings(T1, T2: TPasType): boolean;
// True when two result types are AnsiString-family types fpc treats as equal:
// String ($H+) and RawByteString, as in lazutils' `function ReadIdent: String;
// override;` over classes' RawByteString one. Kept out of the element ranking,
// which also decides overload identity (SetString(String) and
// SetString(RawByteString) are two overloads).
var
  Res1, Res2: TPasResolverResult;
  B1, B2: TResolverBaseType;
begin
  Result:=false;
  if (T1=nil) or (T2=nil) then exit;
  ComputeElement(T1,Res1,[rcType]);
  ComputeElement(T2,Res2,[rcType]);
  B1:=GetActualBaseType(Res1.BaseType);
  B2:=GetActualBaseType(Res2.BaseType);
  Result:=(B1 in [btAnsiString,btRawByteString]) and (B2 in [btAnsiString,btRawByteString]);
end;

function TPasResolver.ProcArgsMatchForSignature(Arg1, Arg2: TPasArgument
  ): boolean;
// Whether two parameters are the same for PROCEDURE-TYPE compatibility. Beyond
// an exact match this accepts a WideString against a UnicodeString, which off
// Windows share one storage type: rtl/objpas/fpwidestring.pp installs its
// Unicode2AnsiMove(...,var UnicodeString,...) into the widestring manager's
// Wide2AnsiMoveProc(...,var WideString,...) field.
// Deliberately NOT folded into CheckProcArgCompatibility: that one also decides
// overload IDENTITY, and ppcx64 lets a WideString and a UnicodeString routine
// overload each other (tcpstr22), so there they must stay different.
begin
  Result:=CheckProcArgCompatibility(Arg1,Arg2)<=cGenericExact;
  if Result then exit;
  if Arg1.Access<>Arg2.Access then exit;
  Result:=TypesMatchAsWideVsUnicodeString(Arg1.ArgType,Arg2.ArgType)
    or TypesAreOneBaseType(Arg1.ArgType,Arg2.ArgType);
end;

function TPasResolver.TypesAreOneBaseType(T1, T2: TPasType): boolean;
// True when the two types are two names of one base type: String under $H+ and
// AnsiString, as in the LCL's `const Msg: string` method assigned to fcl-image's
// TFPImgProgressEvent with `const Msg: AnsiString`.
var
  Res1, Res2: TPasResolverResult;
begin
  Result:=false;
  if (T1=nil) or (T2=nil) then exit;
  ComputeElement(T1,Res1,[rcType]);
  ComputeElement(T2,Res2,[rcType]);
  Result:=(Res1.BaseType<>Res2.BaseType)
    and (GetActualBaseType(Res1.BaseType)=GetActualBaseType(Res2.BaseType));
end;

function TPasResolver.CheckElTypeCompatibility(Arg1, Arg2: TPasType;
  ResolveAlias: TPRResolveAlias): integer;
var
  Arg1Resolved, Arg2Resolved: TPasResolverResult;
  C: TClass;
  Arr1, Arr2: TPasArrayType;
  TemplType1, TemplType2: TPasGenericTemplateType;
  Templates1, Templates2, ProcArgs1, ProcArgs2: TFPList;
  i: Integer;
  Proc1, Proc2: TPasProcedureType;
  Ptr1, Ptr2: TPasPointerType;
begin
  if Arg1=Arg2 then exit(cExact);
  // One type under a simple alias (SynEdit's `TSynStatusChanges =
  // SynEditTypes.TSynStatusChanges`, a set type, overriding a method declared
  // with the original name): computing the two can give different base types.
  if (ResolveAlias in [prraSimple,prraAlias]) and (Arg1<>nil) and (Arg2<>nil)
      and not (Arg1 is TUnresolvedPendingRef) and not (Arg2 is TUnresolvedPendingRef)
      and (ResolveSimpleAliasType(Arg1)=ResolveSimpleAliasType(Arg2)) then
    exit(cExact);
  // A forward pointer's target is only bound at the END of its type section, so
  // two methods declared LATER IN THAT SAME section compare argument types whose
  // target is still a placeholder - and each `^T` reference makes its OWN
  // placeholder, so they are not the same element. Two placeholders for the same
  // NAME stand for the same type; computing one raised "not yet implemented".
  // fv's dialogs.inc declares `PCommandSItem = ^TCommandSItem` and then two
  // classes whose `constructor Init(...; ACommandStrings: PCommandSItem)`
  // overload each other.
  if (Arg1 is TUnresolvedPendingRef) or (Arg2 is TUnresolvedPendingRef) then
    begin
    if (Arg1<>nil) and (Arg2<>nil) and SameText(Arg1.Name,Arg2.Name) then
      exit(cExact);
    exit(cIncompatible);
    end;
  ComputeElement(Arg1,Arg1Resolved,[rcType]);
  ComputeElement(Arg2,Arg2Resolved,[rcType]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckElTypeCompatibility Arg1=',GetResolverResultDbg(Arg1Resolved),' Arg2=',GetResolverResultDbg(Arg2Resolved));
  {$ENDIF}

  if IsGenericTemplType(Arg1Resolved) then
    begin
    Result:=cGenericExact;
    if Arg1Resolved.LoTypeEl=Arg2Resolved.LoTypeEl then
      exit(cExact)
    else if IsGenericTemplType(Arg2Resolved) then
      begin
      TemplType1:=TPasGenericTemplateType(Arg1Resolved.LoTypeEl);
      TemplType2:=TPasGenericTemplateType(Arg2Resolved.LoTypeEl);
      if (TemplType1.Parent is TPasProcedure)
          and (TemplType2.Parent is TPasProcedure) then
        begin
        Templates1:=GetProcTemplateTypes(TPasProcedure(TemplType1.Parent));
        Templates2:=GetProcTemplateTypes(TPasProcedure(TemplType2.Parent));
        i:=Templates1.IndexOf(TemplType1);
        if (i>=0) and (i=Templates2.IndexOf(TemplType2)) then
          exit(cExact);
        end;
      end;
    exit;
    end
  else if IsGenericTemplType(Arg2Resolved) then
    exit(cGenericExact);

  {$ifdef FPC_HAS_CPSTRING}
  // Two refcounted AnsiString-family element types (AnsiString, RawByteString and
  // codepage variants such as UTF8String) differ only in their static codepage
  // treat them as the same element type for array/element compatibility, so e.g. an
  if (Arg1Resolved.BaseType in [btAnsiString,btRawByteString])
      and (Arg2Resolved.BaseType in [btAnsiString,btRawByteString])
      and (Arg1Resolved.BaseType<>Arg2Resolved.BaseType) then
    exit(cAliasExact);
  {$endif}

  // String, Char and Extended are ALIASES for a concrete base type ($H+ makes
  // String = AnsiString), so two element types that differ only in which of the
  // two names was written are the same type. fcl-passrc relies on this: it passes
  // a TRTLStringDynArray (= array of AnsiString) to ParseSource's
  // `const FPCCommandLine: array of String`.
  if (Arg1Resolved.BaseType<>Arg2Resolved.BaseType)
      and (GetActualBaseType(Arg1Resolved.BaseType)
           =GetActualBaseType(Arg2Resolved.BaseType)) then
    exit(cAliasExact);

  if (Arg1Resolved.BaseType<>Arg2Resolved.BaseType)
      or (Arg1Resolved.LoTypeEl=nil)
      or (Arg2Resolved.LoTypeEl=nil) then
    exit(cIncompatible);

  // Variant and OleVariant are DISTINCT types although both map to btVariant.
  // For an overload signature / arg match they must not collapse to the same
  // type, else e.g. VarClear(var Variant) and VarClear(var OleVariant) are seen
  // as a duplicate identifier. Distinguish the two named btVariant built-ins by
  // name (a same-named Variant<->Variant still matches).
  if (Arg1Resolved.BaseType=btVariant)
      and (Arg1Resolved.HiTypeEl<>nil) and (Arg2Resolved.HiTypeEl<>nil)
      and (CompareText(Arg1Resolved.HiTypeEl.Name,Arg2Resolved.HiTypeEl.Name)<>0) then
    exit(cIncompatible);

  if ResolveAlias=prraSimple then
    begin
    if IsSameType(Arg1Resolved.HiTypeEl,Arg2Resolved.HiTypeEl,prraSimple) then
      exit(cExact);
    // Two distinct TPasSpecializeType instances for the same generic
    // self-specialization (e.g. specialize TList<_T> in declaration vs body)
    // have different HiTypeEl but identical LoTypeEl.
    if (Arg1Resolved.HiTypeEl is TPasSpecializeType)
        and (Arg2Resolved.HiTypeEl is TPasSpecializeType)
        and IsSameType(Arg1Resolved.LoTypeEl,Arg2Resolved.LoTypeEl,prraNone) then
      exit(cExact);
    end
  else
    begin
    if IsSameType(Arg1Resolved.LoTypeEl,Arg2Resolved.LoTypeEl,prraNone) then
      exit(cExact);
    end;

  // Two file types with matching element type are the same signature. Each `File`
  // keyword builds a fresh anonymous TPasFileType (ElType=nil), so an untyped-file
  // parameter in an interface would otherwise never match its implementation body
  if (Arg1Resolved.BaseType=btFile) and (Arg2Resolved.BaseType=btFile)
      and (Arg1Resolved.LoTypeEl is TPasFileType)
      and (Arg2Resolved.LoTypeEl is TPasFileType) then
    begin
    if (TPasFileType(Arg1Resolved.LoTypeEl).ElType=nil)
        and (TPasFileType(Arg2Resolved.LoTypeEl).ElType=nil) then
      exit(cExact)   // both untyped `File`
    else if (TPasFileType(Arg1Resolved.LoTypeEl).ElType<>nil)
        and (TPasFileType(Arg2Resolved.LoTypeEl).ElType<>nil) then
      exit(CheckElTypeCompatibility(TPasFileType(Arg1Resolved.LoTypeEl).ElType,
                                    TPasFileType(Arg2Resolved.LoTypeEl).ElType,ResolveAlias));
    end;

  if Arg1Resolved.BaseType=btContext then
    begin
    C:=Arg1Resolved.LoTypeEl.ClassType;
    if C<>Arg2Resolved.LoTypeEl.ClassType then
      exit(cIncompatible);
    if C=TPasArrayType then
      begin
      Arr1:=TPasArrayType(Arg1Resolved.LoTypeEl);
      Arr2:=TPasArrayType(Arg2Resolved.LoTypeEl);
      if length(Arr1.Ranges)<>length(Arr2.Ranges) then
        exit(cIncompatible);
      // An open-array parameter (anonymous `array of X` in the arg list) and a
      // dynamic-array parameter (a named `array of X` type) are DISTINCT overload
      // signatures in FPC, though both have 0 ranges and the same element type —
      // so `Test(array of LongInt)` and `Test(TLongIntArray)` may coexist (tarrconstr6).
      if IsOpenArray(Arr1)<>IsOpenArray(Arr2) then
        exit(cIncompatible);
      // Two STATIC arrays are one signature only when their index bounds agree;
      // differing bounds make them distinct types, so distinct overloads.
      if (length(Arr1.Ranges)>0) and not SameArrayRanges(Arr1,Arr2) then
        exit(cIncompatible);
      Result:=CheckElTypeCompatibility(GetArrayElType(Arr1),GetArrayElType(Arr2),ResolveAlias);
      exit;
      end
    else if C=TPasPointerType then
      begin
      // Two pointer types are one signature when they point at the same type —
      // exactly as the array case above compares element types. PWideChar and
      // PUnicodeChar are two separate declarations of ^WideChar (UnicodeChar is
      // an alias of WideChar), and FPC accepts either for the other in a
      // procedure signature: rtl/objpas/fpwidestring.pp assigns its
      // Unicode2AnsiMove(PUnicodeChar,...) to a Wide2AnsiMoveProc(PWideChar,...)
      // field of the widestring manager.
      Ptr1:=TPasPointerType(Arg1Resolved.LoTypeEl);
      Ptr2:=TPasPointerType(Arg2Resolved.LoTypeEl);
      if (Ptr1.DestType=nil) or (Ptr2.DestType=nil) then
        begin
        if Ptr1.DestType=Ptr2.DestType then
          exit(cExact);  // two forward pointers, both still unresolved
        exit(cIncompatible);
        end;
      // The very same target element is the same type, whatever it is. It may
      // still be a TUnresolvedPendingRef: a forward pointer is only bound at the
      // END of its type section, so two methods declared LATER IN THAT SAME
      // section already compare their argument types - fv's dialogs.inc declares
      // `PCommandSItem = ^TCommandSItem` and then two classes whose
      // `constructor Init(...; ACommandStrings: PCommandSItem)` overload each
      // other. Computing the placeholder raised "not yet implemented".
      if Ptr1.DestType=Ptr2.DestType then
        exit(cExact);
      Result:=CheckElTypeCompatibility(Ptr1.DestType,Ptr2.DestType,ResolveAlias);
      exit;
      end
    else if (C.InheritsFrom(TPasProcedureType))
        and not (msDelphi in CurrentParser.CurrentModeswitches) then
      begin
      // FPC checks proc types arguments by signature, Delphi checks by type
      Proc1:=TPasProcedureType(Arg1Resolved.LoTypeEl);
      Proc2:=TPasProcedureType(Arg2Resolved.LoTypeEl);
      if Proc1.CallingConvention<>Proc2.CallingConvention then
        exit(cIncompatible);
      if Proc1.Modifiers<>Proc2.Modifiers then
        exit(cIncompatible);
      if Proc1.VarArgsType<>Proc2.VarArgsType then
        begin
        Result:=CheckElTypeCompatibility(Proc1.VarArgsType,Proc2.VarArgsType,ResolveAlias);
        if Result=cIncompatible then exit;
        end;
      ProcArgs1:=Proc1.Args;
      ProcArgs2:=Proc2.Args;
      if ProcArgs1.Count<>ProcArgs2.Count then
        exit(cIncompatible);
      for i:=0 to ProcArgs1.Count-1 do
        begin
        if not ProcArgsMatchForSignature(TPasArgument(ProcArgs1[i]),TPasArgument(ProcArgs2[i])) then
          exit(cIncompatible);
        end;
      exit(cExact);
      end;
    end;

  Result:=cIncompatible;
end;

function TPasResolver.OwnTemplateTypeOf(El: TPasElement): TPasType;
// The first template type of the innermost generic that declares El, or nil.
begin
  Result:=nil;
  while El<>nil do
    begin
    if (El is TPasGenericType) and (TPasGenericType(El).GenericTemplateTypes<>nil)
        and (TPasGenericType(El).GenericTemplateTypes.Count>0) then
      exit(TPasType(TPasGenericType(El).GenericTemplateTypes[0]));
    El:=El.Parent;
    end;
end;

function TPasResolver.MemberOfUndecidedType(Expr: TPasElement): boolean;
// Whether Expr is `X.Member` (or its Member part) where the declared type of X
// still depends on a type parameter.
var
  Bin: TBinaryExpr;
  LeftResolved: TPasResolverResult;
begin
  Result:=false;
  if (Expr is TPrimitiveExpr) and (Expr.Parent is TBinaryExpr)
      and (TBinaryExpr(Expr.Parent).Right=Expr) then
    Expr:=Expr.Parent;
  if not (Expr is TBinaryExpr) then exit;
  Bin:=TBinaryExpr(Expr);
  if Bin.OpCode<>eopSubIdent then exit;
  ComputeElement(Bin.Left,LeftResolved,[]);
  Result:=((LeftResolved.HiTypeEl<>nil) and ParamIsUndecided(LeftResolved.HiTypeEl))
      or ((LeftResolved.LoTypeEl<>nil) and ParamIsUndecided(LeftResolved.LoTypeEl));
end;

function TPasResolver.CheckCanBeLHS(const ResolvedEl: TPasResolverResult;
  ErrorOnFalse: boolean; ErrorEl: TPasElement): boolean;
var
  El: TPasElement;
begin
  Result:=false;
  // Assigning to a member of a still-unbound template type can only be checked
  // once T is known. fcl-stl's ghashmap writes ((FData[Fh]).mutable[Fp])^.Value.
  if IsGenericTemplType(ResolvedEl) then
    exit(true);
  // The same for a member of a value whose declared type still depends on a
  // type parameter: lazutils' `fItemSize.ItemSize := ..` names a const of the
  // constraint's TSizeT, but a field of the TSizeT the specialization gets.
  if FPartialSpecsAsGeneric
      and (MemberOfUndecidedType(ResolvedEl.ExprEl) or MemberOfUndecidedType(ErrorEl)) then
    exit(true);
  El:=ResolvedEl.IdentEl;
  if El=nil then
    begin
    if (ResolvedEl.ExprEl is TUnaryExpr)
        and (TUnaryExpr(ResolvedEl.ExprEl).OpCode=eopDeref) then
      begin
      // e.g. p^:=
      end
    else if (ResolvedEl.ExprEl is TParamsExpr)
        and (TParamsExpr(ResolvedEl.ExprEl).Kind=pekArrayParams) then
      begin
      // e.g. p[i]:= (pointer indexing)
      end
    else if (ResolvedEl.ExprEl is TParamsExpr)
        and (TParamsExpr(ResolvedEl.ExprEl).Kind=pekFuncParams) then
      begin
      // e.g. TypeCast(x)[i] := ... (type cast as LValue)
      end
    else if IsDelphiProcVarAddr(ResolvedEl.ExprEl) then
      begin
      // Delphi mode: `@ProcVar := Value` assigns the procedural VARIABLE.
      end
    else if (ResolvedEl.ExprEl is TBinaryExpr)
        and (TBinaryExpr(ResolvedEl.ExprEl).Left is TInheritedExpr)
        and (rrfWritable in ResolvedEl.Flags) then
      begin
      // e.g. T(inherited Insert(Index)^):= (fgl's TFPGList.Insert)
      end
    else
      begin
      if ErrorOnFalse then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.CheckCanBeLHS no identifier: ',GetResolverResultDbg(ResolvedEl));
        {$ENDIF}
        if (ResolvedEl.LoTypeEl<>nil) and (ResolvedEl.ExprEl<>nil) then
          RaiseXExpectedButYFound(20170216152727,'identifier',GetElementTypeName(ResolvedEl.LoTypeEl),ResolvedEl.ExprEl)
        else
          RaiseVarExpected(20170216152426,ErrorEl,ResolvedEl.IdentEl);
        end;
      exit;
      end;
    end;
  if [rrfWritable,rrfAssignable]*ResolvedEl.Flags<>[] then
    exit(not IsVariableConst(El,ErrorEl,ErrorOnFalse));
  // not writable
  if not ErrorOnFalse then exit;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckCanBeLHS not writable: ',GetResolverResultDbg(ResolvedEl));
  {$ENDIF}
  if ResolvedEl.IdentEl is TPasProperty then
    RaiseMsg(20170216152427,nPropertyNotWritable,sPropertyNotWritable,[],ErrorEl)
  else if ResolvedEl.IdentEl is TPasConst then
    RaiseMsg(20180430012042,nCantAssignValuesToConstVariable,sCantAssignValuesToConstVariable,[],ErrorEl)
  else
    RaiseMsg(20170216152429,nVariableIdentifierExpected,sVariableIdentifierExpected,[],ErrorEl);
end;

function TPasResolver.CheckAssignCompatibility(const LHS, RHS: TPasElement;
  RaiseOnIncompatible: boolean; ErrorEl: TPasElement): integer;
var
  LeftResolved, RightResolved: TPasResolverResult;
  Flags: TPasResolverComputeFlags;
  IsProcType: Boolean;
begin
  if ErrorEl=nil then
    ErrorEl:=RHS;
  ComputeElement(LHS,LeftResolved,[rcNoImplicitProc]);
  Flags:=[];
  IsProcType:=IsProcedureType(LeftResolved,true);
  if IsProcType then
    if msDelphi in CurrentParser.CurrentModeswitches then
      Include(Flags,rcNoImplicitProc)
    else
      Include(Flags,rcNoImplicitProcType);
  ComputeElement(RHS,RightResolved,Flags);
  Result:=CheckAssignResCompatibility(LeftResolved,RightResolved,ErrorEl,RaiseOnIncompatible);
  if RHS is TPasExpr then
    CheckAssignExprRange(LeftResolved,TPasExpr(RHS));
end;

function TPasResolver.IsConstFoldableProcAddr(RHS: TPasExpr): Boolean;
// True if RHS is a pure link-time code address usable as a const initializer:
//  - a bare top-level proc name / @TopLevelProc, or
//  - @Type.Method  (address of a method reached through a TYPE, not an instance) —
//    only the code address is taken, so no Self/frame is needed.
// Rejected: nested procs (need a frame), and X.Method where X is a variable/instance
// (needs a runtime Self) — the latter is what keeps tprocvar18/19 correctly %FAIL.
var
  Op: TPasExpr;
  HasAddr: Boolean;
  R, L: TPasResolverResult;
  P: TPasProcedure;
begin
  Result:=false;
  Op:=RHS;
  HasAddr:=false;
  if (RHS is TUnaryExpr) and (TUnaryExpr(RHS).OpCode=eopAddress) then
    begin
    Op:=TUnaryExpr(RHS).Operand;
    HasAddr:=true;
    end;
  if Op is TPrimitiveExpr then
    begin
    ComputeElement(Op,R,[rcNoImplicitProc]);
    if not (R.IdentEl is TPasProcedure) then exit;
    P:=TPasProcedure(R.IdentEl);
    if P.ProcType=nil then exit;
    Result:=(not P.ProcType.IsNested) and (not P.ProcType.IsOfObject)
      and (not (P.Parent is TPasClassType)) and (not (P.Parent is TPasRecordType));
    exit;
    end;
  if (Op is TBinaryExpr) and (TBinaryExpr(Op).OpCode=eopSubIdent) then
    begin
    ComputeElement(TBinaryExpr(Op).right,R,[rcNoImplicitProc]);
    if not (R.IdentEl is TPasProcedure) then exit;
    P:=TPasProcedure(R.IdentEl);
    if (P.ProcType=nil) or P.ProcType.IsNested then exit;
    ComputeElement(TBinaryExpr(Op).left,L,[]);
    if not (L.IdentEl is TPasType) then exit; // THE RAZOR: reject aObj.Method /
      // aMetaclass.Method (variable base needs a runtime Self) -> tprocvar18/19 %FAIL.
    if HasAddr then
      // @Type.Method : only the code address is taken -> always foldable.
      Result:=true
    else
      // Type.Method without @ builds a method pointer {code,data}; its Data is a
      // link-time constant only when Self is the class VMT, i.e. a class method.
      Result:=(P is TPasClassProcedure) or (P is TPasClassFunction);
    end;
end;

procedure TPasResolver.CheckAssignExprRange(
  const LeftResolved: TPasResolverResult; RHS: TPasExpr);
// if RHS is a constant check if it fits into range LeftResolved
var
  LRangeValue, RValue: TResEvalValue;
  Int, MinVal, MaxVal: TMaxPrecInt;
  RangeExpr: TBinaryExpr;
  C: TClass;
  EnumType: TPasEnumType;
  bt: TResolverBaseType;
  LTypeEl: TPasType;
begin
  LTypeEl:=LeftResolved.LoTypeEl;
  if (LTypeEl<>nil)
      and ((LTypeEl.ClassType=TPasArrayType)
        or (LTypeEl.ClassType=TPasRecordType)
        // A Turbo Pascal OBJECT is checked by field too - it can be a typed
        // constant just like a record (fv's `MinWinSize: TPoint = (X:16;Y:6)`),
        // and asking for the whole value said "Constant expression expected".
        or ((LTypeEl.ClassType=TPasClassType)
            and (TPasClassType(LTypeEl).ObjKind=okObject))) then
    exit; // arrays, records and TP objects are checked by element, not by value
  if LTypeEl is TPasClassOfType then
    exit; // class-of are checked only by type, not by value
  if IsConstFoldableProcAddr(RHS)
      and (IsProcedureType(LeftResolved,true)
        or (LeftResolved.BaseType=btPointer)) then
    exit; // @TopLevelProc / @Type.Method is a valid link-time constant (no
          // frame/instance) for a procvar OR a (code)pointer target; nested procs
          // and instance method pointers are rejected by IsConstFoldableProcAddr.
  RValue:=Eval(RHS,[refAutoConstExt]);
  if RValue=nil then
    exit; // not a const expression
  {$IFDEF VerbosePasResEval}
  writeln('TPasResolver.CheckAssignExprRange Left=',GetResolverResultDbg(LeftResolved),' RValue=',RValue.AsDebugString);
  {$ENDIF}
  LRangeValue:=nil;
  try
    if RValue.Kind=revkExternal then
      // skip
    else if LeftResolved.BaseType=btCustom then
      CheckAssignExprRangeToCustom(LeftResolved,RValue,RHS)
    else if LeftResolved.BaseType=btSet then
      begin
      // assign to a set
      C:=LTypeEl.ClassType;
      if C=TPasRangeType then
        begin
        RangeExpr:=TPasRangeType(LTypeEl).RangeExpr;
        LRangeValue:=Eval(RangeExpr,[refConst],false);
        end
      else if C=TPasEnumType then
        begin
        EnumType:=TPasEnumType(LTypeEl);
        LRangeValue:=TResEvalRangeInt.CreateValue(revskEnum,EnumType,
          GetEnumMinOrdinal(EnumType),GetEnumMaxOrdinal(EnumType));
        end
      else if C=TPasUnresolvedSymbolRef then
        begin
        // set of basetype
        if LTypeEl.CustomData is TResElDataBaseType then
          begin
          bt:=GetActualBaseType(TResElDataBaseType(LTypeEl.CustomData).BaseType);
          if (bt in btAllIntegerNoQWord) and GetIntegerRange(bt,MinVal,MaxVal) then
            LRangeValue:=TResEvalRangeInt.CreateValue(revskInt,nil,MinVal,MaxVal)
          else if bt=btBoolean then
            LRangeValue:=TResEvalRangeInt.CreateValue(revskBool,nil,0,1)
          {$ifdef FPC_HAS_CPSTRING}
          else if bt=btAnsiChar then
            LRangeValue:=TResEvalRangeInt.CreateValue(revskChar,nil,0,$ff)
          {$endif}
          else if bt=btWideChar then
            LRangeValue:=TResEvalRangeInt.CreateValue(revskChar,nil,0,$ffff)
          else
            RaiseNotYetImplemented(20170714205110,RHS);
          end
        else
          RaiseNotYetImplemented(20170714204803,RHS);
        end
      else
        RaiseNotYetImplemented(20170714193100,RHS);
      fExprEvaluator.IsSetCompatible(RValue,RHS,LRangeValue,true);
      end
    else if LTypeEl is TPasRangeType then
      begin
      RangeExpr:=TPasRangeType(LTypeEl).RangeExpr;
      LRangeValue:=Eval(RangeExpr,[refConst]);
      if LeftResolved.BaseType=btSet then
        fExprEvaluator.IsSetCompatible(RValue,RHS,LRangeValue,true)
      else
        fExprEvaluator.IsInRange(RValue,RHS,LRangeValue,RangeExpr,true);
      end
    else if (LeftResolved.BaseType in btAllIntegerNoQWord)
        and GetIntegerRange(LeftResolved.BaseType,MinVal,MaxVal) then
      case RValue.Kind of
      revkInt:
        if (MinVal>TResEvalInt(RValue).Int)
            or (MaxVal<TResEvalInt(RValue).Int) then
          fExprEvaluator.EmitRangeCheckConst(20170530093126,
            IntToStr(TResEvalInt(RValue).Int),MinVal,MaxVal,RHS);
      revkUInt:
        if (TResEvalUInt(RValue).UInt>High(TMaxPrecInt))
            or (MinVal>TMaxPrecInt(TResEvalUInt(RValue).UInt))
            or (MaxVal<TMaxPrecInt(TResEvalUInt(RValue).UInt)) then
          fExprEvaluator.EmitRangeCheckConst(20170530093616,
            IntToStr(TResEvalUInt(RValue).UInt),IntToStr(MinVal),IntToStr(MaxVal),RHS);
      revkFloat:
        if TResEvalFloat(RValue).IsInt(Int) then
          begin
          if (MinVal>Int) or (MaxVal<Int) then
            fExprEvaluator.EmitRangeCheckConst(20170802133307,
              IntToStr(Int),MinVal,MaxVal,RHS,mtError);
          end
        else
          begin
          {$IFDEF VerbosePasResEval}
          writeln('TPasResolver.CheckAssignExprRange ',Frac(TResEvalFloat(RValue).FloatValue),' ',TResEvalFloat(RValue).FloatValue<TMaxPrecFloat(low(TMaxPrecInt)),' ',TResEvalFloat(RValue).FloatValue>TMaxPrecFloat(high(TMaxPrecInt)),' ',TResEvalFloat(RValue).FloatValue,' ',high(TMaxPrecInt));
          {$ENDIF}
          RaiseRangeCheck(20170802133750,RHS);
          end;
      revkCurrency:
        if TResEvalCurrency(RValue).IsInt(Int) then
          begin
          if (MinVal>Int) or (MaxVal<Int) then
            fExprEvaluator.EmitRangeCheckConst(20180421171325,
              IntToStr(Int),MinVal,MaxVal,RHS,mtError);
          end
        else
          begin
          {$IFDEF VerbosePasResEval}
          writeln('TPasResolver.CheckAssignExprRange ',Frac(TResEvalCurrency(RValue).Value),' ',TResEvalCurrency(RValue).Value,' ',high(TMaxPrecInt));
          {$ENDIF}
          RaiseRangeCheck(20180421171438,RHS);
          end;
      else
        {$IFDEF VerbosePasResEval}
        writeln('TPasResolver.CheckAssignExprRange ',RValue.AsDebugString);
        {$ENDIF}
        RaiseNotYetImplemented(20170530092731,RHS);
      end
    {$ifdef HasInt64}
    else if LeftResolved.BaseType=btQWord then
      case RValue.Kind of
      revkInt:
        if (TResEvalInt(RValue).Int<0) then
          fExprEvaluator.EmitRangeCheckConst(20170530094316,
            IntToStr(TResEvalUInt(RValue).UInt),'0',IntToStr(High(QWord)),RHS);
      revkUInt: ;
      else
        RaiseNotYetImplemented(20170530094311,RHS);
      end
    {$endif}
    else if RValue.Kind in [revkNil,revkBool] then
      // simple type check is enough
    {$IFNDEF PAS2JS} else if LeftResolved.BaseType in btAllFloats+[btComp] then {$ENDIF}
      // simple type check is enough
      // ToDo: warn if precision loss
    else if LeftResolved.BaseType in btAllChars then
      begin
      case RValue.Kind of
      {$ifdef FPC_HAS_CPSTRING}
      revkString,
      {$endif}
      revkUnicodeString:
        Int:=fExprEvaluator.StringToOrd(RValue,RHS);
      else
        RaiseNotYetImplemented(20170714171218,RHS);
      end;
      case GetActualBaseType(LeftResolved.BaseType) of
      {$ifdef FPC_HAS_CPSTRING}
      btAnsiChar: MaxVal:=$ff;
      {$endif}
      btWideChar: MaxVal:=$ffff;
      end;
      if (Int>MaxVal) then
        fExprEvaluator.EmitRangeCheckConst(20170714171911,
          '#'+IntToStr(Int),'#0','#'+IntToStr(MaxVal),RHS);
      end
    else if LeftResolved.BaseType in btAllStrings then
      // simple type check is enough
      // ToDo: warn if unicode to non-utf8
    {$IFDEF FPC}
    else if LeftResolved.BaseType=btVariant then
      // variant accepts any value
    {$ENDIF}
    else if LeftResolved.BaseType=btContext then
      // simple type check is enough
    else if LeftResolved.BaseType=btPointer then
      // pointer accepts an integer-address constant or nil (tatomic1: p := Pointer($66))
    else if LeftResolved.BaseType=btRange then
      begin
      if (LeftResolved.ExprEl is TBinaryExpr)
          and (TBinaryExpr(LeftResolved.ExprEl).Kind=pekRange) then
        begin
        LRangeValue:=Eval(LeftResolved.ExprEl,[refConst]);
        try
          case LRangeValue.Kind of
          revkRangeInt:
            case TResEvalRangeInt(LRangeValue).ElKind of
            revskEnum:
              if (RValue.Kind<>revkEnum) then
                RaiseNotYetImplemented(20171009171251,RHS)
              else if (TResEvalEnum(RValue).Index<TResEvalRangeInt(LRangeValue).RangeStart)
                  or (TResEvalEnum(RValue).Index>TResEvalRangeInt(LRangeValue).RangeEnd) then
                fExprEvaluator.EmitRangeCheckConst(20171009171442,
                  TResEvalEnum(RValue).AsString,
                  TResEvalRangeInt(LRangeValue).ElementAsString(TResEvalRangeInt(LRangeValue).RangeStart),
                  TResEvalRangeInt(LRangeValue).ElementAsString(TResEvalRangeInt(LRangeValue).RangeEnd),
                  RHS);
            else
              RaiseNotYetImplemented(20171009165348,LeftResolved.ExprEl);
            end;
          else
            RaiseNotYetImplemented(20171009165326,LeftResolved.ExprEl);
          end;
        finally
          ReleaseEvalValue(LRangeValue);
        end;
        end
      else
        RaiseNotYetImplemented(20171009171005,RHS);
      end
    else
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.CheckAssignExprRange LeftResolved=',GetResolverResultDbg(LeftResolved));
      {$ENDIF}
      RaiseNotYetImplemented(20170530095243,RHS);
      end;
  finally
    ReleaseEvalValue(RValue);
    ReleaseEvalValue(LRangeValue);
  end;
end;

procedure TPasResolver.CheckAssignExprRangeToCustom(
  const LeftResolved: TPasResolverResult; RValue: TResEvalValue; RHS: TPasExpr);
begin
  if LeftResolved.BaseType<>btCustom then exit;
  if RValue=nil then exit;
  if RHS=nil then ;
end;

function TPasResolver.CheckAssignResCompatibility(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
var
  LTypeEl, RTypeEl: TPasType;
  Handled: Boolean;
  C: TClass;
  LBT, RBT: TResolverBaseType;
  LRange, RValue, Value: TResEvalValue;
  RightSubResolved: TPasResolverResult;
  wc: WideChar;
  ExprModeEl: TPasElement;
begin
  // check if the RHS can be converted to LHS
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckAssignResCompatibility START LHS='+GetResolverResultDbg(LHS)+' RHS='+GetResolverResultDbg(RHS));
  {$ENDIF}
  Result:=-1;

  Handled:=false;
  // let descendant check first
  Result:=CheckAssignCompatibilityCustom(LHS,RHS,ErrorEl,RaiseOnIncompatible,Handled);
  if Handled and (Result>=cExact) and (Result<cIncompatible) then
    exit;

  if not Handled then
    begin
    LBT:=GetActualBaseType(LHS.BaseType);
    RBT:=GetActualBaseType(RHS.BaseType);
    if (LHS.LoTypeEl=nil) and (LBT=btUntyped) then
      // An UNTYPED parameter takes any value, including one whose type is still a
      // generic template - rtl-generics hands Move an element of `array of T`.
      exit(cTypeConversion);
    // A parameter of ANOTHER generic, reached through a partial specialization
    // answered with that generic (`Result := FList[i]` of a
    // TFPGObjectList<TItem<_ITEM>>), stands for a type only the specialization
    // knows: its constraint does not decide the assignment. Only as the VALUE: a
    // parameter of the routine being called is typed by that routine's own
    // parameter (`specialize Test<T>(aArg[i])`), and deferring there made every
    // overload fit alike (tgenfunc23 then chose the `array of T` one).
    if FPartialSpecsAsGeneric and IsGenericTemplType(RHS)
        and not IsGenericTemplType(LHS)
        and not TemplateIsInScope(TPasGenericTemplateType(RHS.LoTypeEl),ErrorEl) then
      exit(cPartialSpecDefer);
    if IsGenericTemplType(LHS) then
      begin
      // Template := RHS
      if not RaiseOnIncompatible then
        ErrorEl:=nil;
      Result:=CheckTemplateFitsParamRes(TPasGenericTemplateType(LHS.LoTypeEl),
         RHS,prtcoAssignToTempl,ErrorEl);
      exit;
      end
    else if IsGenericTemplType(RHS) then
      begin
      // LHS := Template
      // The template used as a TYPE name is a class reference (`TClass := T`,
      // fcl-db's TTypedConnectionPool<T>.FindConnection passes T itself). The
      // concrete class is only known at specialization, so defer.
      if (RHS.IdentEl is TPasType) and (LHS.BaseType=btContext)
          and (LHS.LoTypeEl is TPasClassOfType) then
        exit(cGenericExact);
      if not RaiseOnIncompatible then
        ErrorEl:=nil;
      Result:=CheckTemplateFitsParamRes(TPasGenericTemplateType(RHS.LoTypeEl),
         LHS,prtcoAssignFromTempl,ErrorEl);
      exit;
      end;
    {$IFNDEF PAS2JS}
    if (LBT=btVariant) xor (RBT=btVariant) then
      begin
      // Variant is assignment-compatible with any simple/string value in either
      // direction; the actual conversion (varString/varOleStr/varInteger/...) is
      // performed at runtime. (variant:=string/int/..., or string/int/...:=variant).
      // Only one side is Variant here; Variant:=Variant keeps the normal path.
      Result:=cCompatible;
      exit;
      end
    else 
    {$ENDIF}
    if LHS.LoTypeEl=nil then
      begin
      if LBT=btUntyped then
        begin
        // untyped parameter
        Result:=cTypeConversion;
        end
      else
        RaiseNotYetImplemented(20160922163631,LHS.IdentEl);
      end
    else if LBT=RBT then
      begin
      if LBT=btContext then
        exit(CheckAssignCompatibilityUserType(LHS,RHS,ErrorEl,RaiseOnIncompatible))
      else
        begin
        // same base type, maybe not same type (e.g. longint and integer)
        if IsSameType(LHS.HiTypeEl,RHS.HiTypeEl,prraSimple)
            and HasExactType(RHS) then
          Result:=cExact
        else
          Result:=cAliasExact;
        end;
      end
    else if (LBT in btAllBooleans)
        and (RBT in btAllBooleans) then
      Result:=cCompatible
    else if (LBT in btAllChars) then
      begin
      if (RBT in btAllChars) then
        case LBT of
        {$ifdef FPC_HAS_CPSTRING}
        btAnsiChar:
          Result:=cLossyConversion;
        {$endif}
        btWideChar:
          {$ifdef FPC_HAS_CPSTRING}
          if RBT=btAnsiChar then
            Result:=cCompatible
          else
          {$endif}
            Result:=cLossyConversion;
        else
          RaiseNotYetImplemented(20170728132440,ErrorEl,BaseTypeNames[LBT]);
        end
      else if (RBT=btRange) and (RHS.SubType in btAllChars) then
        begin
        if LBT=btWideChar then
          exit(cCompatible);
        {$ifdef FPC_HAS_CPSTRING}
        // LHS is ansichar
        if GetActualBaseType(RHS.SubType)=btAnsiChar then
          exit(cExact);
        RValue:=Eval(RHS,[refAutoConstExt]);
        if RValue<>nil then
          try
            // ansichar:=constvalue
            case RValue.Kind of
            revkString:
              if not ExprEvaluator.GetWideChar(TResEvalString(RValue).S,wc) then
                exit(cIncompatible);
            revkUnicodeString:
              begin
              if length(TResEvalUTF16(RValue).S)<>1 then
                exit(cIncompatible);
              wc:=TResEvalUTF16(RValue).S[1];
              end;
            revkExternal:
              exit(cCompatible);
            else
              RaiseNotYetImplemented(20171108194650,ErrorEl);
            end;
            if ord(wc)>255 then
              exit(cIncompatible);
            exit(cCompatible);
          finally
            ReleaseEvalValue(RValue);
          end;
        // LHS is ansichar, RHS is not a const
        if (RHS.ExprEl is TBinaryExpr) and (TBinaryExpr(RHS.ExprEl).Kind=pekRange) then
          begin
          RValue:=Eval(RHS.ExprEl,[refConst]);
          try
            if RValue.Kind<>revkRangeInt then
              RaiseNotYetImplemented(20171108195035,ErrorEl);
            if TResEvalRangeInt(RValue).RangeStart>255 then
              exit(cIncompatible);
            if TResEvalRangeInt(RValue).RangeEnd>255 then
              exit(cLossyConversion);
            exit(cCompatible);
          finally
            ReleaseEvalValue(RValue);
          end;
          end;
        {$endif}
        RaiseNotYetImplemented(20171108195216,ErrorEl);
        end;
      end
    else if (LBT in btAllStrings) then
      begin
      if (RBT=btRange) and (RHS.SubType in btAllChars) then
        begin
        // A CHAR SUBRANGE assigns to a string exactly as its element char does -
        // the mirror of the char-LHS rule above. fcl-fpterm declares
        // TC1Char = #$80..#$9F and returns one from a `string` function.
        RightSubResolved:=RHS;
        RightSubResolved.BaseType:=RHS.SubType;
        RightSubResolved.SubType:=btNone;
        Result:=CheckAssignResCompatibility(LHS,RightSubResolved,ErrorEl,false);
        end
      else if (RBT in btAllStringAndChars) then
        case LBT of
        {$ifdef FPC_HAS_CPSTRING}
        btAnsiString:
          if RBT in [btAnsiChar,btShortString,btRawByteString] then
            Result:=cCompatible
          else
            Result:=cLossyConversion;
        btShortString:
          if RBT=btAnsiChar then
            Result:=cCompatible
          else
            Result:=cLossyConversion;
        btRawByteString:
          if RBT in [btAnsiChar,btAnsiString,btShortString] then
            Result:=cCompatible
          else
            Result:=cLossyConversion;
        {$endif}
        btWideString,btUnicodeString:
          Result:=cCompatible;
        else
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.CheckAssignResCompatibility ',{$ifdef pas2js}str(LBT){$else}LBT{$ENDIF});
          {$ENDIF}
          RaiseNotYetImplemented(20170417195208,ErrorEl,BaseTypeNames[LBT]);
        end
      else if RBT=btContext then
        begin
        RTypeEl:=RHS.LoTypeEl;
        if RTypeEl.ClassType=TPasClassType then
          begin
          if (TPasClassType(RTypeEl).ObjKind=okInterface)
              and IsTGUIDString(LHS) then
            // aGUIDString:=IntfTypeOrVar
            exit(cInterfaceToString);  // no check for rrfReadable
          end
        else if RTypeEl.ClassType=TPasRecordType then
          begin
          if IsTGUID(TPasRecordType(RTypeEl)) then
            // aString:=GUID
            Result:=cTGUIDToString;
          end
        else if RTypeEl.ClassType=TPasPointerType then
          begin
          // String:=PChar — pointer to a char type. Rank by matching char width
          // vs string width so an overloaded RawByteString/UnicodeString pair
          // picks PAnsiChar->RawByteString and PWideChar->UnicodeString (tstring11).
          ComputeElement(TPasPointerType(RTypeEl).DestType, RightSubResolved, []);
          if RightSubResolved.BaseType in btAllChars then
            begin
            if (GetActualBaseType(RightSubResolved.BaseType)=btWideChar)
                = (LBT in [btUnicodeString,btWideString]) then
              begin
              Result:=cTypeConversion;
              {$ifdef FPC_HAS_CPSTRING}
              // A ShortString truncates at 255 characters, so it is a WORSE
              // target for a pointer than a dynamic string of the same char
              // width. gdbint writes `pos('%', question)` with a PAnsiChar, and
              // Pos(AnsiChar,AnsiString) and Pos(AnsiChar,ShortString) were tied.
              if GetActualBaseType(LBT)=btShortString then
                inc(Result);
              {$endif}
              end
            else
              Result:=cLossyConversion;
            end;
          end
        else if RTypeEl.ClassType=TPasArrayType then
          begin
          // String:=array-of-char — a STATIC array (fixed ranges) or an OPEN
          // array param (`array of Char`, 0 ranges, Parent is TPasArgument) with a
          // char element type. A true dynamic array is NOT implicitly string-
          // compatible in FPC. Rank by matching char width vs string width so an
          // overloaded RawByteString/UnicodeString pair picks the right one
          // (tstring11): matching width = cTypeConversion, mismatch = cLossyConversion.
          if (((length(TPasArrayType(RTypeEl).Ranges) > 0)
                and not IsDynArray(TPasArrayType(RTypeEl)))
              or IsOpenArray(TPasArrayType(RTypeEl))) then
            begin
            ComputeElement(TPasArrayType(RTypeEl).ElType, RightSubResolved, []);
            if RightSubResolved.BaseType in btAllChars then
              begin
              if (GetActualBaseType(RightSubResolved.BaseType)=btWideChar)
                  = (LBT in [btUnicodeString,btWideString]) then
                Result:=cTypeConversion
              else
                Result:=cLossyConversion;
              end;
            end;
          end;
        end;
      end
    else if (LBT in btAllInteger)
        and (RBT in btAllInteger) then
      begin
      Result:=cIntToIntConversion+ord(LBT)-ord(RBT);
      // A CONSTANT whose value fits the target loses nothing, so it must not be
      // priced as a lossy conversion. Otherwise two candidates that both take
      // the constant - `VarIsType(v, varByte)` against the TVarType and the
      // `array of TVarType` overloads - are both flattened to cLossyConversion
      // by CheckCallProcCompatibility and the call is ambiguous, where ppcx64
      // simply picks the scalar (fcl-report, 18 units). Same shape as the
      // Ord() typing fix: a value's RANGE, not its declared width, decides.
      if IsConstIntFitting(RHS,LBT) then
        exit;
      case LBT of
      btByte,
      btShortInt: inc(Result,cLossyConversion);
      btWord,
      btSmallInt:
        if not (RBT in [btByte,btShortInt]) then
          inc(Result,cLossyConversion);
      btUIntSingle:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt]) then
          inc(Result,cLossyConversion);
      btIntSingle:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt,btUIntSingle]) then
          inc(Result,cLossyConversion);
      btLongWord,
      btLongint:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt,btUIntSingle,btIntSingle]) then
          inc(Result,cLossyConversion);
      btUIntDouble:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt,btLongWord,btLongint]) then
          inc(Result,cLossyConversion);
      btIntDouble:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt,btLongWord,btLongint,btUIntDouble]) then
          inc(Result,cLossyConversion);
      {$ifdef HasInt64}
      btQWord,
      btInt64,btComp:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt,btUIntSingle,btIntSingle,
            btLongWord,btLongint,btUIntDouble,btIntDouble]) then
          inc(Result,cLossyConversion);
      {$endif}
      else
        RaiseNotYetImplemented(20170417205301,ErrorEl,BaseTypeNames[LBT]);
      end;
      end
    else if (LBT in btAllFloats)
        and (RBT in btAllFloats) then
      begin
      Result:=cFloatToFloatConversion+ord(LBT)-ord(RBT);
      case LBT of
      btSingle:
        if RBT>btSingle then
          inc(Result,cLossyConversion);
      btDouble:
        if RBT>btDouble then
          inc(Result,cLossyConversion);
      btExtended,btCExtended:
        if RBT>btCExtended then
          inc(Result,cLossyConversion);
      btCurrency:
        inc(Result,cLossyConversion);
      else
        RaiseNotYetImplemented(20170417205910,ErrorEl,BaseTypeNames[LBT]);
      end;
      end
    else if (LBT in btAllFloats)
        and (RBT in btAllInteger) then
      begin
      Result:=cIntToFloatConversion+ord(LBT)-ord(RBT);
      case LBT of
      btSingle:
        if RBT>btUIntSingle then
          inc(Result,cLossyConversion);
      btDouble:
        if RBT>btUIntDouble then
          inc(Result,cLossyConversion);
      btExtended,btCExtended:
        if RBT>btCExtended then
          inc(Result,cLossyConversion);
      btCurrency:
        if not (RBT in [btByte,btShortInt,btWord,btSmallInt,
            btIntSingle,btUIntSingle,
            btLongWord,btLongint]) then
          inc(Result,cLossyConversion);
      else
        RaiseNotYetImplemented(20170417205911,ErrorEl,BaseTypeNames[LBT]);
      end;
      end
    else if LBT=btNil then
      begin
      if RaiseOnIncompatible then
        RaiseMsg(20170216152431,nCantAssignValuesToAnAddress,sCantAssignValuesToAnAddress,
          [],ErrorEl);
      exit(cIncompatible);
      end
    else if LBT=btRange then
      begin
      if (LHS.ExprEl is TBinaryExpr) and (TBinaryExpr(LHS.ExprEl).Kind=pekRange) then
        begin
        LRange:=Eval(LHS.ExprEl,[refConst]);
        RValue:=nil;
        try
          {$IFDEF VerbosePasResolver}
          //writeln('TPasResolver.CheckAssignResCompatibility LeftRange=',LRange.AsDebugString);
          {$ENDIF}
          case LRange.Kind of
          revkRangeInt:
            case TResEvalRangeInt(LRange).ElKind of
            revskEnum:
              if RHS.BaseType=btContext then
                begin
                if IsSameType(TResEvalRangeInt(LRange).ElType,RHS.LoTypeEl,prraAlias) then
                  begin
                  // same enum type
                  {$IFDEF VerbosePasResolver}
                  writeln('TPasResolver.CheckAssignResCompatibility LeftRange=',LRange.AsDebugString,' Left.ElType=',GetObjName(TResEvalRangeInt(LRange).ElType),' RHS.TypeEl=',GetObjName(RHS.LoTypeEl));
                  {$ENDIF}
                  // ToDo: check if LRange is smaller than Range of RHS (cLossyConversion)
                  exit(cExact);
                  end;
                end;
            revskInt:
              if RHS.BaseType in btAllInteger then
                begin
                RValue:=Eval(RHS,[refAutoConstExt]);
                if RValue<>nil then
                  begin
                  // ToDo: check range
                  end;
                exit(cCompatible);
                end;
            revskChar:
              if RHS.BaseType in btAllStringAndChars then
                begin
                RValue:=Eval(RHS,[refAutoConstExt]);
                if RValue<>nil then
                  begin
                  case RValue.Kind of
                  {$ifdef FPC_HAS_CPSTRING}
                  revkString:
                    if not fExprEvaluator.GetWideChar(TResEvalString(RValue).S,wc) then
                      exit(cIncompatible);
                  {$endif}
                  revkUnicodeString:
                    begin
                    if length(TResEvalUTF16(RValue).S)<>1 then
                      exit(cIncompatible);
                    wc:=TResEvalUTF16(RValue).S[1];
                    end;
                  revkExternal:
                    exit(cCompatible);
                  else
                    RaiseNotYetImplemented(20171108192232,ErrorEl);
                  end;
                  if (ord(wc)<TResEvalRangeInt(LRange).RangeStart)
                      or (ord(wc)>TResEvalRangeInt(LRange).RangeEnd) then
                    exit(cIncompatible);
                  end;
                exit(cCompatible);
                end;
            revskBool:
              if RHS.BaseType=btBoolean then
                begin
                RValue:=Eval(RHS,[refAutoConstExt]);
                if RValue<>nil then
                  begin
                  // ToDo: check range
                  end;
                exit(cCompatible);
                end;
            end;
          end;
        finally
          ReleaseEvalValue(LRange);
          ReleaseEvalValue(RValue);
        end;
        end;
      end
    else if LBT=btSet then
      begin
      if RBT=btArrayOrSet then
        begin
        if RHS.SubType=btNone then
          // a:=[]
          Result:=cExact
        else if IsSameType(LHS.HiTypeEl,RHS.HiTypeEl,prraSimple)
            and HasExactType(RHS) then
          Result:=cExact
        else if LHS.SubType=RHS.SubType then
          Result:=cAliasExact
        else if (LHS.SubType in btAllBooleans) and (RHS.SubType in btAllBooleans) then
          Result:=cCompatible
        else if (LHS.SubType in btAllInteger) and (RHS.SubType in btAllInteger) then
          begin
          // ToDo: range check
          Result:=cCompatible;
          end
        else if (LHS.SubType in btAllChars) and (RHS.SubType in btAllChars) then
          begin
          // ToDo: range check
          Result:=cCompatible;
          end;
        end;
      end
    else if LBT in [btArrayLit,btArrayOrSet,btModule,btProc] then
      begin
      if RaiseOnIncompatible then
        RaiseMsg(20170216152432,nIllegalExpression,sIllegalExpression,[],ErrorEl);
      exit(cIncompatible);
      end
    else if (LHS.IdentEl=nil) and (LHS.ExprEl=nil) then
      begin
      if RaiseOnIncompatible then
        RaiseMsg(20170216152434,nIllegalExpression,sIllegalExpression,[],ErrorEl);
      exit(cIncompatible);
      end
    else if RBT=btNil then
      begin
      if LBT=btPointer then
        Result:=cExact
      else if LBT=btContext then
        begin
        LTypeEl:=LHS.LoTypeEl;
        C:=LTypeEl.ClassType;
        // an old-style object is a value: nil is no object
        if ((C=TPasClassType) and (TPasClassType(LTypeEl).ObjKind<>okObject))
            or (C=TPasClassOfType)
            or (C=TPasPointerType)
            or C.InheritsFrom(TPasProcedureType)
            or IsDynArray(LTypeEl) then
          Result:=cExact;
        end;
      end
    else if RBT=btProc then
      begin
      if RHS.ExprEl<>nil then
        ExprModeEl:=RHS.ExprEl
      else
        ExprModeEl:=ErrorEl;
      // The MODE that decides this belongs to the CODE THE EXPRESSION IS IN,
      // not to whatever file the parser happens to be in: a generic body is
      // re-resolved when a CONSUMER specializes it, and that consumer may be in
      // another mode entirely - or, for a generic loaded from a precompiled
      // unit, there is no parser for it at all. rtl-generics is {$mode delphi}
      // and passes a bare method name (`Create(AEq, GetHashCodeMethod, AExt)`),
      // which an objfpc consumer specializing it then rejected.
      // The referenced ROUTINE's own mode is the wrong question - dbf_idxfile
      // builds `array of TExprFunc` from bare names of routines declared
      // elsewhere.
      if ((msDelphi in GetElModeSwitches(ExprModeEl))
           or (msTPProcVar in GetElModeSwitches(ExprModeEl)))
          and (LHS.LoTypeEl is TPasProcedureType)
          and (RHS.IdentEl is TPasProcedure) then
        begin
        // ProcVar:=Proc (bare proc name, no @). Allowed in Delphi and the TP-style
        // procvar modes (tp/macpas/gpc — msTPProcVar). NOTE: pas2js strips
        // msTPProcVar from its Delphi mode, so keep the explicit msDelphi arm too.
        if CheckProcTypeCompatibility(TPasProcedureType(LHS.LoTypeEl),
            TPasProcedure(RHS.IdentEl).ProcType,true,ErrorEl,RaiseOnIncompatible) then
          exit(cExact);
        end
      else if (LHS.LoTypeEl is TPasProcedureType)
          and (RHS.ExprEl is TProcedureExpr) then
        begin
        // for example  ProcVar:=anonymous-procedure...
        if CheckProcTypeCompatibility(TPasProcedureType(LHS.LoTypeEl),
            TProcedureExpr(RHS.ExprEl).Proc.ProcType,true,ErrorEl,RaiseOnIncompatible) then
          begin
          if MaximizeFPCCompatibility or TPasProcedureType(LHS.LoTypeEl).IsReferenceTo then
            exit(cExact)
          else
            exit(cCompatible);
          end;
        end
      end
    else if LBT=btPointer then
      begin
      // A static ARRAY decays to a pointer to its element type here too - the
      // same rule as in the btContext arm above, for a pointer that resolves
      // with btPointer rather than as a declared type.
      if (RBT=btContext) and (RHS.LoTypeEl is TPasArrayType)
          and (length(TPasArrayType(RHS.LoTypeEl).Ranges)>0)
          and (TPasArrayType(RHS.LoTypeEl).ElType<>nil)
          and (LHS.LoTypeEl is TPasPointerType)
          and (TPasPointerType(LHS.LoTypeEl).DestType<>nil)
          and IsSameType(ResolveAliasType(TPasPointerType(LHS.LoTypeEl).DestType),
                         ResolveAliasType(TPasArrayType(RHS.LoTypeEl).ElType),
                         prraAlias) then
        exit(cTypeConversion);
      if RBT=btPointer then
        begin
        LTypeEl:=LHS.LoTypeEl;
        RTypeEl:=RHS.LoTypeEl;
        if IsBaseType(LTypeEl,btPointer) then
          Result:=cExact // btPointer can take any pointer
        else if IsBaseType(RTypeEl,btPointer) then
          Result:=cTypeConversion // any pointer can take a btPointer
        else if IsSameType(LTypeEl,RTypeEl,prraAlias) then
          Result:=cExact // pointer of same type
        else if (LTypeEl.ClassType=TPasPointerType)
            and (RTypeEl.ClassType=TPasPointerType) then
          Result:=CheckAssignCompatibility(TPasPointerType(LTypeEl).DestType,
            TPasPointerType(RTypeEl).DestType,RaiseOnIncompatible);
        end
      else if IsBaseType(LHS.LoTypeEl,btPointer) then
        begin
        // UntypedPointer:=...
        if RBT=btContext then
          begin
          RTypeEl:=RHS.LoTypeEl;
          C:=RTypeEl.ClassType;
          if C=TPasClassType then
            // UntypedPointer:=ClassTypeOrInstance
            exit(cTypeConversion)
          else if C=TPasClassOfType then
            // UntypedPointer:=ClassOfVar
            Result:=cTypeConversion
          else if C=TPasArrayType then
            begin
            if IsDynArray(RTypeEl) then
              // UntypedPointer:=DynArray
              Result:=cTypeConversion;
            end
          else if (C=TPasProcedureType) or (C=TPasFunctionType) then
            // UntypedPointer:=procvar
            Result:=cLossyConversion
          else if C=TPasPointerType then
            // UntypedPointer:=TypedPointer. Allowed, but ranked below an exact
            // match so an overload declared with that very pointer type wins the
            // tie, as in FPC: strutils declares both
            // BinToHex(PAnsiChar,PAnsiChar,..) and BinToHex(Pointer,PAnsiChar,..).
            Result:=cCompatible;
          end;
        end;
      end
    else if (LBT=btContext) then
      begin
      LTypeEl:=LHS.LoTypeEl;
      if (LTypeEl.ClassType=TPasArrayType) then
        begin
        // DynArray:=UntypedPointer. A dynamic array IS one machine pointer, and
        // the mirror (UntypedPointer:=DynArray) is allowed a few lines above, so
        // this completes the pair - fcl-db's TDataSet.SetBookmarkStr calls
        // GotoBookMark(Pointer(Value)) where the parameter is a TBytes.
        // Only a pointer VALUE, never a dereference: `P^` names the pointed-to
        // memory (untyped), and letting that match a dynamic array made
        // TStream.Write(Buffer^,Count) ambiguous between the untyped
        // `const Buffer` overload and the TBytes one (streams.inc).
        if IsDynArray(LTypeEl) and (RHS.BaseType=btPointer)
            and (rrfReadable in RHS.Flags)
            and not ((RHS.ExprEl is TUnaryExpr)
                     and (TUnaryExpr(RHS.ExprEl).OpCode=eopDeref)) then
          Result:=cTypeConversion
        else
          Result:=CheckAssignCompatibilityArrayType(LHS,RHS,ErrorEl,RaiseOnIncompatible);
        end
      else if LTypeEl.ClassType=TPasEnumType then
        begin
        if (RHS.BaseType=btRange) and (RHS.SubType=btContext) then
          begin
          RTypeEl:=RHS.LoTypeEl;
          if RTypeEl.ClassType=TPasRangeType then
            begin
            ComputeElement(TPasRangeType(RTypeEl).RangeExpr.Left,RightSubResolved,[rcConstant]);
            if (RightSubResolved.BaseType=btContext)
                and IsSameType(LTypeEl,RightSubResolved.LoTypeEl,prraAlias) then
              begin
              // enumtype := enumrange
              Result:=cExact;
              end;
            end;
          end;
        end
      else if LTypeEl.ClassType=TPasRecordType then
        begin
        if (RBT in btAllStrings) and IsTGUID(TPasRecordType(LTypeEl))
            and (rrfReadable in RHS.Flags) then
          begin
          // GUIDVar := string, e.g. IObjectInstance: TGuid = '{D91C9AF4-3C93-420F-A303-BF5BA82BFD23}'
          // ASK for the constant, never demand it: this runs while OVERLOAD
          // CANDIDATES are being ranked, where nothing may raise. A TGUID
          // overload made `WriteValue(string(P))` fail outright instead of
          // simply not matching (vcl-compat's TJsonWriter).
          Value:=Eval(RHS,[]);
          try
            if Value=nil then
              if RaiseOnIncompatible then
                RaiseXExpectedButYFound(20180414105916,'string literal','string', ErrorEl)
              else
                exit(cIncompatible);
          finally
            ReleaseEvalValue(Value);
          end;
          Result:=cStringToTGUID;
          end;
        end
      else if (LTypeEl.ClassType=TPasClassType)
          and (TPasClassType(LTypeEl).ObjKind in [okInterface,okDispInterface])
          and (RBT=btPointer) and IsBaseType(RHS.LoTypeEl,btPointer) then
        // Interface:=UntypedPointer - the raw reference, which real FPC accepts:
        // rtl-generics' TComparer<T>.Default returns _LookupVtableInfo's Pointer
        Result:=cTypeConversion
      else if LTypeEl.ClassType=TPasPointerType then
        begin
        // TypedPointer:=
        // A STATIC ARRAY decays to a pointer to its first element when its
        // element type is what the pointer points to - fcl-db's ibconnection
        // passes a `TStatusVector = array[0..19] of ISC_STATUS` to a
        // PISC_STATUS parameter, exactly as ppcx64 does.
        if (RHS.BaseType=btContext) and (RHS.LoTypeEl is TPasArrayType)
            and (length(TPasArrayType(RHS.LoTypeEl).Ranges)>0)
            and (TPasArrayType(RHS.LoTypeEl).ElType<>nil)
            and (TPasPointerType(LTypeEl).DestType<>nil)
            and IsSameType(ResolveAliasType(TPasPointerType(LTypeEl).DestType),
                           ResolveAliasType(TPasArrayType(RHS.LoTypeEl).ElType),
                           prraAlias) then
          exit(cTypeConversion);
        if RHS.BaseType=btPointer then
          begin
          RTypeEl:=RHS.LoTypeEl;
          if IsBaseType(RTypeEl,btPointer) then
            // TypedPointer:=UntypedPointer
            Result:=cTypeConversion
          else if RTypeEl=nil then
            // TypedPointer:=@untypedparam (LoTypeEl is nil for @untyped)
            Result:=cTypeConversion
          else if not (bsTypedAddress in CurrentParser.Scanner.CurrentBoolSwitches) then
            // {$T-} (default): @Var yields an untyped pointer, assignable to any
            // typed pointer regardless of the pointed-to type (e.g. PByte:=@CharVar).
            Result:=cTypeConversion
          else
            begin
            // {$T+}: typed @ — pointed-to types must match
            Result:=CheckAssignCompatibilityPointerType(
              TPasPointerType(LTypeEl).DestType,RTypeEl,ErrorEl,false);
            end;
          end
        else if RBT in btAllStringAndChars then
          begin
          // String/Char -> PChar: check if pointer to char type
          ComputeElement(TPasPointerType(LHS.LoTypeEl).DestType, RightSubResolved, []);
          if RightSubResolved.BaseType in btAllChars then
            Result:=cTypeConversion;
          end
        else if RBT=btNil then
          // TypedPointer:=nil
          Result:=cExact;
        end;
      end;
    end;

  if (Result>=0) and (Result<cIncompatible) then
    begin
    // type fits -> check readable
    if not (rrfReadable in RHS.Flags) then
      begin
      if RaiseOnIncompatible then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.CheckAssignResCompatibility RHS not readable. LHS='+GetResolverResultDbg(LHS)+' RHS='+GetResolverResultDbg(RHS));
        {$ENDIF}
        RaiseVarExpected(20170318235637,ErrorEl,RHS.IdentEl);
        end;
      exit(cIncompatible);
      end;
    exit;
    end;

  // incompatible
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckAssignResCompatibility incompatible LHS='+GetResolverResultDbg(LHS)+' RHS='+GetResolverResultDbg(RHS));
  {$ENDIF}
  // Either side still PARTIALLY specialized - a type argument is a template, so
  // what it stands for is only decided at specialization, where the assignment is
  // checked again (rtl-generics assigns one nested PNode to another).
  if IsPartiallySpecializedType(LHS.LoTypeEl)
      and IsPartiallySpecializedType(RHS.LoTypeEl) then
    exit(cPartialSpecDefer);
  if not RaiseOnIncompatible then
    exit(cIncompatible);

  // create error messages
  RaiseIncompatibleTypeRes(20170216152437,nIncompatibleTypesGotExpected,
    [],RHS,LHS,ErrorEl);
end;

function TPasResolver.CheckEqualElCompatibility(Left, Right: TPasElement;
  ErrorEl: TPasElement; RaiseOnIncompatible: boolean; SetReferenceFlags: boolean
  ): integer;
// check if the RightResolved is type compatible to LeftResolved
var
  LFlags, RFlags: TPasResolverComputeFlags;
  LeftResolved, RightResolved: TPasResolverResult;
  LeftErrorEl, RightErrorEl: TPasElement;
begin
  Result:=cIncompatible;
  // Delphi resolves both sides, so it forbids "if procvar=procvar then"
  // FPC is more clever. It supports "if procvar=@proc then", "function=value"
  if msDelphi in CurrentParser.CurrentModeswitches then
    LFlags:=[]
  else
    LFlags:=[rcNoImplicitProcType];
  if SetReferenceFlags then
    Include(LFlags,rcSetReferenceFlags);
  ComputeElement(Left,LeftResolved,LFlags);

  if (msDelphi in CurrentParser.CurrentModeswitches) then
    RFlags:=LFlags
  else
    begin
    if LeftResolved.BaseType=btNil then
      RFlags:=[rcNoImplicitProcType]
    else if IsProcedureType(LeftResolved,true) then
      RFlags:=[rcNoImplicitProcType]
    else
      RFlags:=[];
    end;
  if SetReferenceFlags then
    Include(RFlags,rcSetReferenceFlags);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckEqualElCompatibility LFlags=',dbgs(LFlags),' Left=',GetResolverResultDbg(LeftResolved),' Delphi=',msDelphi in CurrentParser.CurrentModeswitches,' RFlags=',dbgs(RFlags));
  {$ENDIF}
  ComputeElement(Right,RightResolved,RFlags);
  if ErrorEl=nil then
    begin
    LeftErrorEl:=Left;
    RightErrorEl:=Right;
    end
  else
    begin
    LeftErrorEl:=ErrorEl;
    RightErrorEl:=ErrorEl;
    end;
  Result:=CheckEqualResCompatibility(LeftResolved,RightResolved,LeftErrorEl,
    RaiseOnIncompatible,RightErrorEl);
end;

function TPasResolver.CheckEqualResCompatibility(const LHS,
  RHS: TPasResolverResult; LErrorEl: TPasElement; RaiseOnIncompatible: boolean;
  RErrorEl: TPasElement): integer;

var
  LTypeEl, RTypeEl: TPasType;
  ResolvedEl: TPasResolverResult;
begin
  Result:=cIncompatible;
  if RErrorEl=nil then RErrorEl:=LErrorEl;
  // check if the RHS is type compatible to LHS
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckEqualResCompatibility LHS=',GetResolverResultDbg(LHS),' RHS=',GetResolverResultDbg(RHS));
  {$ENDIF}
  // An operand that is still an unbound generic template - a member read off it
  // stays unresolved on purpose - can only be judged at specialization
  // (rtl-generics compares FMainIndex with TCuckooCfg.D).
  if IsGenericTemplType(LHS) or IsGenericTemplType(RHS) then
    exit(cGenericExact);
  if not (rrfReadable in LHS.Flags) then
    begin
    if (LHS.BaseType=btContext) then
      begin
      LTypeEl:=LHS.LoTypeEl;
      if (LTypeEl.ClassType=TPasClassType)
          and (ResolveAliasTypeEl(LHS.IdentEl)=LTypeEl) then
        begin
        // LHS is class type, e.g. TObject or IInterface
        if RHS.BaseType=btNil then
          exit(cExact)
        else if RHS.BaseType in btAllStrings then
          begin
          if (rrfReadable in RHS.Flags)
              and (TPasClassType(LTypeEl).ObjKind=okInterface)
              and IsTGUIDString(RHS) then
            // e.g.  IUnknown=aGUIDString
            exit(cInterfaceToString);
          end
        else if (RHS.BaseType=btContext) then
          begin
          RTypeEl:=RHS.LoTypeEl;
          if (RTypeEl.ClassType=TPasClassOfType)
              and (rrfReadable in RHS.Flags)
              and (TPasClassType(LTypeEl).ObjKind=okClass) then
            // for example  if TImage=ImageClass then
            exit(cExact)
          else if (RTypeEl.ClassType=TPasRecordType)
              and (rrfReadable in RHS.Flags)
              and (TPasClassType(LTypeEl).ObjKind=okInterface)
              and IsTGUID(TPasRecordType(RTypeEl)) then
            // e.g.  if IUnknown=TGuidVar then
            exit(cInterfaceToTGUID);
          end;
        end;
      end;
    RaiseMsg(20170216152438,nNotReadable,sNotReadable,[],LErrorEl);
    end;
  if not (rrfReadable in RHS.Flags) then
    begin
    if (RHS.BaseType=btContext) then
      begin
      RTypeEl:=RHS.LoTypeEl;
      if (RTypeEl.ClassType=TPasClassType)
          and (ResolveAliasTypeEl(RHS.IdentEl)=RTypeEl) then
        begin
        // RHS is class type, e.g. TObject or IInterface
        if LHS.BaseType=btNil then
          exit(cExact)
        else if LHS.BaseType in btAllStrings then
          begin
          if (rrfReadable in LHS.Flags)
              and (TPasClassType(RTypeEl).ObjKind=okInterface)
              and IsTGUIDString(LHS) then
            // e.g.  aGUIDString=IUnknown
            exit(cInterfaceToString);
          end
        else if (LHS.BaseType=btContext) then
          begin
          LTypeEl:=LHS.LoTypeEl;
          if (LTypeEl.ClassType=TPasClassOfType)
              and (rrfReadable in LHS.Flags)
              and (TPasClassType(RTypeEl).ObjKind=okClass) then
            // for example  if ImageClass=TImage then
            exit(cExact)
          else if (LTypeEl.ClassType=TPasRecordType)
              and (rrfReadable in LHS.Flags)
              and (TPasClassType(RTypeEl).ObjKind=okInterface)
              and IsTGUID(TPasRecordType(LTypeEl)) then
            // e.g.  if TGuidVar=IUnknown then
            exit(cInterfaceToTGUID);
          end;
        end;
      end;
    RaiseMsg(20170216152440,nNotReadable,sNotReadable,[],RErrorEl);
    end;
    
  {$IFNDEF PAS2JS}
  if (GetActualBaseType(LHS.BaseType)=btVariant)
      or (GetActualBaseType(RHS.BaseType)=btVariant) then
    // A Variant compares against any simple/string operand; the comparison is
    // resolved at runtime (the non-variant side is coerced to a variant).
    exit(cCompatible);
  {$ENDIF}
  
  if IsGenericTemplType(LHS) then
    begin
    // TemplateVar = x
    Result:=CheckTemplateFitsParamRes(TPasGenericTemplateType(LHS.LoTypeEl),RHS,prtcoEqual,nil);
    if Result<>cIncompatible then exit;
    end
  else if IsGenericTemplType(RHS) then
    begin
    // x = TemplateVar
    Result:=CheckTemplateFitsParamRes(TPasGenericTemplateType(RHS.LoTypeEl),LHS,prtcoEqual,nil);
    if Result<>cIncompatible then exit;
    end;

  if (LHS.BaseType=btCustom) or (RHS.BaseType=btCustom) then
    begin
    Result:=CheckEqualCompatibilityCustomType(LHS,RHS,LErrorEl,RaiseOnIncompatible);
    if (Result=cIncompatible) and RaiseOnIncompatible then
      RaiseIncompatibleTypeRes(20170330010727,nIncompatibleTypesGotExpected,
        [],RHS,LHS,LErrorEl);
    exit;
    end
  else if LHS.BaseType=RHS.BaseType then
    begin
    if LHS.BaseType=btContext then
      exit(CheckEqualCompatibilityUserType(LHS,RHS,LErrorEl,RaiseOnIncompatible))
    else
      exit(cExact); // same base type, maybe not same type name (e.g. longint and integer)
    end
  else if LHS.BaseType in btAllInteger then
    begin
    if RHS.BaseType in btAllInteger+btAllFloats then
      exit(cCompatible)
    else if (RHS.BaseType=btRange) and (RHS.SubType in btAllInteger) then
      exit(cCompatible);
    end
  else if LHS.BaseType in btAllFloats then
    begin
    if RHS.BaseType in btAllInteger+btAllFloats then
      exit(cCompatible);
    end
  else if LHS.BaseType in btAllBooleans then
    begin
    if RHS.BaseType in btAllBooleans then
      exit(cCompatible)
    else if (RHS.BaseType=btRange) and (RHS.SubType in btAllBooleans) then
      exit(cCompatible);
    end
  else if LHS.BaseType in btAllStringAndChars then
    begin
    if RHS.BaseType in btAllStringAndChars then
      exit(cCompatible)
    else if (RHS.BaseType=btRange) and (RHS.SubType in btAllStringAndChars) then
      exit(cCompatible)
    else if RHS.BaseType=btContext then
      begin
      RTypeEl:=RHS.LoTypeEl;
      if (RTypeEl.ClassType=TPasClassType) then
        begin
        if (TPasClassType(RTypeEl).ObjKind=okInterface)
            and IsTGUIDString(LHS) then
          // e.g. aGUIDString=IntfVar
          exit(cInterfaceToString);
        end
      else if (RTypeEl.ClassType=TPasRecordType)
          and IsTGUID(TPasRecordType(RTypeEl)) then
        // e.g. aString=GuidVar
        exit(cTGUIDToString)
      else if (RTypeEl.ClassType=TPasArrayType)
          and (TPasArrayType(RTypeEl).ElType <> nil) then
        begin
        ComputeElement(TPasArrayType(RTypeEl).ElType, ResolvedEl, []);
        if ResolvedEl.BaseType in btAllChars then
          exit(cCompatible); // e.g. aString = array-of-char
        end
      else if (RTypeEl.ClassType=TPasPointerType) then
        begin
        // e.g. aString = PChar / aShortString = PChar (PChar -> string)
        ComputeElement(TPasPointerType(RTypeEl).DestType, ResolvedEl, []);
        if ResolvedEl.BaseType in btAllChars then
          exit(cCompatible);
        end;
      end;
    end
  else if LHS.BaseType=btNil then
    begin
      if RHS.BaseType in [btPointer,btNil] then
        exit(cExact)
      else if RHS.BaseType=btContext then
        begin
        LTypeEl:=RHS.LoTypeEl;
        if (LTypeEl.ClassType=TPasClassType)
            or (LTypeEl.ClassType=TPasClassOfType)
            or (LTypeEl.ClassType=TPasPointerType)
            or (LTypeEl is TPasProcedureType)
            or IsDynArray(LTypeEl) then
          exit(cExact);
        end;
      if RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20170216152442,nIncompatibleTypesGotExpected,
          [],RHS,LHS,RErrorEl)
      else
        exit(cIncompatible);
    end
  else if RHS.BaseType=btNil then
    begin
      if LHS.BaseType=btPointer then
        exit(cExact)
      else if LHS.BaseType=btContext then
        begin
        LTypeEl:=LHS.LoTypeEl;
        if (LTypeEl.ClassType=TPasClassType)
            or (LTypeEl.ClassType=TPasClassOfType)
            or (LTypeEl.ClassType=TPasPointerType)
            or (LTypeEl is TPasProcedureType)
            or IsDynArray(LTypeEl) then
          exit(cExact);
        end;
      if RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20170216152444,nIncompatibleTypesGotExpected,
          [],RHS,LHS,LErrorEl)
      else
        exit(cIncompatible);
    end
  else if LHS.BaseType=btPointer then
    begin
    if RHS.BaseType=btContext then
      begin
      RTypeEl:=RHS.LoTypeEl;
      if RTypeEl.ClassType=TPasPointerType then
        // @Something=TypedPointer
        exit(cExact)
      else if RTypeEl.ClassType=TPasClassType then
        // @Something=ClassOrInterface
        exit(cCompatible)
      else if RTypeEl.ClassType=TPasClassOfType then
        // @Something=ClassOf
        exit(cCompatible)
      else if RTypeEl is TPasProcedureType then
        // Pointer = @proc / procvar — a pointer compares with a procedural value
        exit(cCompatible);
      end;
    end
  else if LHS.BaseType in [btSet,btArrayOrSet] then
    begin
    if RHS.BaseType in [btSet,btArrayOrSet] then
      begin
      if LHS.LoTypeEl=nil then
        exit(cExact); // empty set
      if RHS.LoTypeEl=nil then
        exit(cExact); // empty set
      if IsSameType(LHS.LoTypeEl,RHS.LoTypeEl,prraAlias) then
        exit(cExact);
      // a set of an enum subrange is compatible with a set of its base enum
      // (e.g. "set of (A_b..A_e)" vs a set literal "[A_c,A_d]" of the base enum)
      LTypeEl:=LHS.LoTypeEl;
      if (LTypeEl is TPasRangeType) and (TPasRangeType(LTypeEl).RangeExpr<>nil) then
        begin
        ComputeElement(TPasRangeType(LTypeEl).RangeExpr.Left,ResolvedEl,[]);
        if ResolvedEl.LoTypeEl<>nil then LTypeEl:=ResolvedEl.LoTypeEl;
        end;
      RTypeEl:=RHS.LoTypeEl;
      if (RTypeEl is TPasRangeType) and (TPasRangeType(RTypeEl).RangeExpr<>nil) then
        begin
        ComputeElement(TPasRangeType(RTypeEl).RangeExpr.Left,ResolvedEl,[]);
        if ResolvedEl.LoTypeEl<>nil then RTypeEl:=ResolvedEl.LoTypeEl;
        end;
      if (LTypeEl<>nil) and (LTypeEl=RTypeEl) then
        exit(cExact);
      if (LHS.SubType=RHS.SubType) and (LHS.SubType in (btAllBooleans+btAllInteger+btAllChars)) then
        exit(cExact);
      if ((LHS.SubType in btAllBooleans) and (RHS.SubType in btAllBooleans))
          or ((LHS.SubType in btAllInteger) and (RHS.SubType in btAllInteger)) then
        exit(cCompatible);
      if RaiseOnIncompatible then
        RaiseMsg(20170216152446,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          ['set of '+BaseTypeNames[LHS.SubType],'set of '+BaseTypeNames[RHS.SubType]],LErrorEl)
      else
        exit(cIncompatible);
      end;
    end
  else if LHS.BaseType=btRange then
    begin
    if LHS.SubType in btAllInteger then
      begin
      // e.g. 2..4
      if RHS.BaseType in btAllInteger then
        exit(cCompatible)
      else if (RHS.BaseType=btRange) and (RHS.SubType in btAllInteger) then
        exit(cCompatible);
      end
    else if LHS.SubType in btAllBooleans then
      begin
      if RHS.BaseType in btAllBooleans then
        exit(cCompatible)
      else if (RHS.BaseType=btRange) and (RHS.SubType in btAllBooleans) then
        exit(cCompatible);
      end
    else if LHS.SubType in btAllChars then
      begin
      if RHS.BaseType in btAllStringAndChars then
        exit(cCompatible)
      else if (RHS.BaseType=btRange) and (RHS.SubType in btAllChars) then
        exit(cCompatible);
      end
    else if LHS.SubType=btContext then
      begin
      LTypeEl:=LHS.LoTypeEl;
      if LTypeEl.ClassType=TPasRangeType then
        begin
        ComputeElement(TPasRangeType(LTypeEl).RangeExpr.Left,ResolvedEl,[rcConstant]);
        if ResolvedEl.BaseType=btContext then
          begin
          LTypeEl:=ResolvedEl.LoTypeEl;
          if LTypeEl.ClassType=TPasEnumType then
            begin
            if RHS.BaseType=btContext then
              begin
              RTypeEl:=RHS.LoTypeEl;
              if (LTypeEl=RTypeEl) then
                exit(cCompatible);
              end;
            end;
          end;
        end;
      end;
    end
  else if LHS.BaseType=btContext then
    begin
    LTypeEl:=LHS.LoTypeEl;
    if LTypeEl.ClassType=TPasEnumType then
      begin
      if RHS.BaseType=btRange then
        begin
        RTypeEl:=RHS.LoTypeEl;
        if RTypeEl.ClassType=TPasRangeType then
          begin
          ComputeElement(TPasRangeType(RTypeEl).RangeExpr.Left,ResolvedEl,[rcConstant]);
          if ResolvedEl.BaseType=btContext then
            begin
            RTypeEl:=ResolvedEl.LoTypeEl;
            if LTypeEl=RTypeEl then
              exit(cCompatible);
            end;
          end;
        end;
      end
    else if LTypeEl.ClassType=TPasClassType then
      begin
      if RHS.BaseType=btPointer then
        exit(cCompatible)
      else if TPasClassType(LTypeEl).ObjKind=okInterface then
        begin
        if RHS.BaseType in btAllStrings then
          begin
          if IsTGUIDString(RHS) then
            // e.g. IntfVar=aGUIDString
            exit(cInterfaceToString);
          end
        else if RHS.BaseType=btContext then
          begin
          RTypeEl:=RHS.LoTypeEl;
          if (RTypeEl.ClassType=TPasRecordType)
              and IsTGUID(TPasRecordType(RTypeEl)) then
            // e.g. IntfVar=GuidVar
            exit(cInterfaceToTGUID);
          end;
        end;
      end
    else if LTypeEl.ClassType=TPasClassOfType then
      begin
      if RHS.BaseType=btPointer then
        exit(cCompatible);
      end
    else if LTypeEl.ClassType=TPasRecordType then
      begin
      if IsTGUID(TPasRecordType(LTypeEl)) then
        begin
        // LHS is TGUID
        if (RHS.BaseType in btAllStrings) then
          // GuidVar=aString
          exit(cTGUIDToString)
        else if RHS.BaseType=btContext then
          begin
          RTypeEl:=RHS.LoTypeEl;
          if (RTypeEl.ClassType=TPasClassType)
              and (TPasClassType(RTypeEl).ObjKind=okInterface) then
            // GUIDVar=IntfVar
            exit(cInterfaceToTGUID);
          end;
        end;
      end
    else if LTypeEl.ClassType=TPasPointerType then
      begin
      if RHS.BaseType=btPointer then
        // TypedPointer=@Something
        exit(cExact)
      else if RHS.BaseType in btAllStringAndChars then
        begin
        // PChar=String: check if pointer to char type
        ComputeElement(TPasPointerType(LTypeEl).DestType, ResolvedEl, []);
        if ResolvedEl.BaseType in btAllChars then
          exit(cCompatible);
        end;
      end
    else if (LTypeEl.ClassType=TPasArrayType)
        and (TPasArrayType(LTypeEl).ElType <> nil)
        and (RHS.BaseType in btAllStringAndChars) then
      begin
      // array-of-char = String (e.g. charArray = 'FPC')
      ComputeElement(TPasArrayType(LTypeEl).ElType, ResolvedEl, []);
      if ResolvedEl.BaseType in btAllChars then
        exit(cCompatible);
      end;
    end;
  // Check reverse: String=PChar
  if (Result<0) and (LHS.BaseType in btAllStringAndChars)
      and (RHS.BaseType=btContext) then
    begin
    RTypeEl:=RHS.LoTypeEl;
    if (RTypeEl<>nil) and (RTypeEl.ClassType=TPasPointerType) then
      begin
      ComputeElement(TPasPointerType(RTypeEl).DestType, ResolvedEl, []);
      if ResolvedEl.BaseType in btAllChars then
        exit(cCompatible);
      end;
    end;
  if RaiseOnIncompatible then
    RaiseIncompatibleTypeRes(20170216152449,nIncompatibleTypesGotExpected,
      [],RHS,LHS,RErrorEl)
  else
    exit(cIncompatible);
end;
