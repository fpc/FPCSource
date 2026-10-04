{%MainUnit pasresolver.pp}
{ **********************************************************************
    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - Builtins handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }

 
function TPasResolver.CheckAssignCompatibilityCustom(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean;
  var Handled: boolean): integer;
// called when LHS or RHS BaseType is btCustom
// if RaiseOnIncompatible=true you can raise an useful error.
begin
  Result:=cIncompatible;
  if LHS.BaseType=btNone then ;
  if RHS.BaseType=btNone then ;
  if ErrorEl=nil then ;
  if RaiseOnIncompatible then ;
  if Handled then ;
end;

function TPasResolver.CheckEqualCompatibilityCustomType(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
begin
  Result:=cIncompatible;
  if LHS.BaseType=RHS.BaseType then;
  if ErrorEl=nil then;
  if RaiseOnIncompatible then ;
end;

function TPasResolver.BI_Length_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'length'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  Ranges: TPasExprArray;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: string or dynamic array or type/const of static array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if ParamResolved.BaseType in btAllStringAndChars then
    begin
    if rrfReadable in ParamResolved.Flags then
      Result:=cExact;
    end
  else if ParamResolved.BaseType=btContext then
    begin
    if (ParamResolved.LoTypeEl.ClassType=TPasArrayType) then
      begin
      Ranges:=TPasArrayType(ParamResolved.LoTypeEl).Ranges;
      if length(Ranges)=0 then
        begin
        if rrfReadable in ParamResolved.Flags then
          Result:=cExact;
        end
      else
        // static array
        Result:=cExact;
      end
    else if (ParamResolved.LoTypeEl is TPasPointerType)
        and (TPasPointerType(ParamResolved.LoTypeEl).DestType<>nil)
        and (rrfReadable in ParamResolved.Flags) then
      begin
      // Length(PChar)/Length(PWideChar): the strlen of the null-terminated data.
      // FPC accepts this (verified vs real ppcx64); the unix sysutils path uses it
      // (fina.inc: Length(PathPChar)).
      ComputeElement(TPasPointerType(ParamResolved.LoTypeEl).DestType,ParamResolved,[rcType]);
      if ParamResolved.BaseType in btAllChars then
        Result:=cExact;
      end;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170329160335,1,Param,ParamResolved,
      'string or dynamic array',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Length_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  if Params=nil then ;
  SetResolverIdentifier(ResolvedEl,BaseTypeLength,Proc.Proc,
    FBaseTypes[BaseTypeLength],FBaseTypes[BaseTypeLength],[rrfReadable]);
end;

procedure TPasResolver.BI_Length_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  Param, Expr: TPasExpr;
  ParamResolved: TPasResolverResult;
  Ranges: TPasExprArray;
  IdentEl: TPasElement;
begin
  Evaluated:=nil;
  // first param: string or dynamic array or type/const of static array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  if ParamResolved.BaseType in btAllStringAndChars then
    Evaluated:=EvalLengthOfString(ParamResolved,Param,Flags)
  else if ParamResolved.BaseType=btContext then
    begin
    if (ParamResolved.LoTypeEl.ClassType=TPasArrayType) then
      begin
      Ranges:=TPasArrayType(ParamResolved.LoTypeEl).Ranges;
      if length(Ranges)=0 then
        begin
        // open or dynamic array
        IdentEl:=ParamResolved.IdentEl;
        if (IdentEl is TPasVariable)
            and (TPasVariable(IdentEl).Expr is TPasExpr) then
          begin
          Expr:=TPasVariable(IdentEl).Expr;
          if Expr is TArrayValues then
            Evaluated:=TResEvalInt.CreateValue(length(TArrayValues(Expr).Values))
          else if (Expr is TParamsExpr) and (TParamsExpr(Expr).Kind=pekSet) then
            Evaluated:=TResEvalInt.CreateValue(length(TParamsExpr(Expr).Params));
          end;
        end
      else
        begin
        // static array
        Evaluated:=TResEvalInt.CreateValue(GetRangeLength(Ranges[0]));
        end;
      end;
    end;
  if Proc=nil then ;
end;

function TPasResolver.BI_SetLength_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'setlength'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, DimResolved: TPasResolverResult;
  ArgNo: Integer;
  DynArr: TPasArrayType;
  ElType: TPasType;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,2,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: string or array variable
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  // The function's own name inside its body IS its Result: rtl-extra's
  // objects.pp calls `SetLength(ReadRawByteString, L)` in ReadRawByteString.
  RedirectSelfNameToResult(Param,ParamResolved,false);
  Result:=cIncompatible;
  DynArr:=nil;
  if ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if ParamResolved.BaseType in btAllStrings then
      Result:=cExact
    else if ParamResolved.BaseType=btContext then
      begin
      if IsDynArray(ParamResolved.LoTypeEl) then
        begin
        Result:=cExact;
        DynArr:=NoNil(ParamResolved.LoTypeEl) as TPasArrayType;
        end;
      end;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152250,1,Param,ParamResolved,
      'string or dynamic array variable',RaiseOnError));

  // second param: new length
  ArgNo:=2;
  repeat
    Param:=Params.Params[ArgNo-1];
    ComputeElement(Param,DimResolved,[]);
    Result:=cIncompatible;
    if (rrfReadable in DimResolved.Flags)
        and (DimResolved.BaseType in btAllInteger) then
      Result:=cExact;
    if Result=cIncompatible then
      exit(CheckRaiseTypeArgNo(20170329160338,ArgNo,Param,DimResolved,
        'integer',RaiseOnError));
    if (DynArr=nil) or (ArgNo=length(Params.Params)) then break;
    ElType:=ResolveAliasType(DynArr.ElType);
    if not IsDynArray(ElType) then break;
    DynArr:=NoNil(ElType) as TPasArrayType;
    inc(ArgNo);
  until false;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,ArgNo,RaiseOnError);
end;

procedure TPasResolver.BI_SetLength_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  if P=nil then ;
  FinishCallArgAccess(P[0],rraVarParam);
  FinishCallArgAccess(P[1],rraRead);
end;

function TPasResolver.BI_InExclude_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'include'
var
  Params: TParamsExpr;
  Param0, Param1: TPasExpr;
  Param0Resolved, Param1Resolved, ElResolved: TPasResolverResult;
  EnumType: TPasEnumType;
  C: TClass;
  LoTypeEl: TPasType;
  RgType: TPasRangeType;
  OrdSetOK: Boolean;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,2,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first Param0: set variable
  // todo set of int, set of AnsiChar, set of bool
  Param0:=Params.Params[0];
  ComputeElement(Param0,Param0Resolved,[rcNoImplicitProc]);
  // The function's own name inside its body IS its Result, so it is assignable
  // here: rtl-console's keyboard.pp does `Include(EnhShiftState,essAlt)` inside
  // `function EnhShiftState: TEnhancedShiftState`.
  RedirectSelfNameToResult(Param0,Param0Resolved,false);
  Param1:=Params.Params[1];
  ComputeElement(Param1,Param1Resolved,[]);

  EnumType:=nil;
  RgType:=nil;
  OrdSetOK:=false;
  // The set has to be an assignable LOCATION, which is not the same as a named
  // variable: `Include(P^, c)` names no identifier at all, and regexpr's
  // rcParseCharRange takes the class set as a `PCharSet` and writes through it.
  if ([rrfReadable,rrfWritable]*Param0Resolved.Flags=[rrfReadable,rrfWritable])
      and ((Param0Resolved.IdentEl<>nil)
           or CheckCanBeLHS(Param0Resolved,false,Param0)) then
    begin
    C:=nil;
    if Param0Resolved.IdentEl<>nil then
      C:=Param0Resolved.IdentEl.ClassType;
    if (C=nil)
        or C.InheritsFrom(TPasVariable)
        or (C=TPasArgument)
        or (C=TPasResultElement) then
      begin
      if Param0Resolved.BaseType=btSet then
        begin
        LoTypeEl:=Param0Resolved.LoTypeEl;
        if LoTypeEl.ClassType=TPasEnumType then
          begin
          EnumType:=TPasEnumType(LoTypeEl);
          if (not (rrfReadable in Param0Resolved.Flags))
              or (Param0Resolved.LoTypeEl<>EnumType) then
            begin
            if RaiseOnError then
              RaiseIncompatibleType(20210201225926,nIncompatibleTypeArgNo,
                ['2'],Param0Resolved.LoTypeEl,EnumType,Param0);
            exit(cIncompatible);
            end;
          end
        else if LoTypeEl.ClassType=TPasRangeType then
          begin
          RgType:=TPasRangeType(LoTypeEl);
          ComputeElement(RgType.RangeExpr.Left,Param0Resolved,[]);
          Result:=CheckAssignResCompatibility(Param0Resolved,Param1Resolved,Param1,RaiseOnError);
          end
        else if AllowInExcludeNonEnumSet then
          begin
          // set of a base ordinal (char/int/boolean), e.g. Include(charSet,c) on
          // sysutils' TSysCharSet. Accept if the value assigns to the element type.
          ComputeElement(LoTypeEl,ElResolved,[]);
          if GetActualBaseType(ElResolved.BaseType) in (btAllChars+btAllInteger+btAllBooleans) then
            begin
            Result:=CheckAssignResCompatibility(ElResolved,Param1Resolved,Param1,RaiseOnError);
            if Result<>cIncompatible then
              OrdSetOK:=true;
            end;
          end;
        end;
      end;
    end;
  if (EnumType=nil) and (RgType=nil) and (not OrdSetOK) then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnGetCallCompatibility_InExclude ',GetResolverResultDbg(Param0Resolved));
    {$ENDIF}
    exit(CheckRaiseTypeArgNo(20170216152301,1,Param0,Param0Resolved,
      'variable of set of enumtype',RaiseOnError));
    end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_InExclude_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  if P=nil then ;
  FinishCallArgAccess(P[0],rraVarParam);
  FinishCallArgAccess(P[1],rraRead);
end;

function TPasResolver.BI_Break_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
begin
  if GetLoop(Expr)=nil then
    RaiseMsg(20170216152306,nMustBeInsideALoop,sMustBeInsideALoop,['Break'],Expr);
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)=0) then
    exit(cExact);
  Params:=TParamsExpr(Expr);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Break Params=',length(Params.Params));
  {$ENDIF}
  Result:=CheckBuiltInMaxParamCount(Proc,Params,0,RaiseOnError);
end;

function TPasResolver.BI_Continue_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
begin
  if GetLoop(Expr)=nil then
    RaiseMsg(20170216152309,nMustBeInsideALoop,sMustBeInsideALoop,['Continue'],Expr);
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)=0) then
    exit(cExact);
  Params:=TParamsExpr(Expr);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Continue Params=',length(Params.Params));
  {$ENDIF}
  Result:=CheckBuiltInMaxParamCount(Proc,Params,0,RaiseOnError);
end;

function TPasResolver.BI_Exit_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, ResultResolved: TPasResolverResult;
  i: Integer;
  ProcScope: TPasProcedureScope;
  ResultEl: TPasResultElement;
  Flags: TPasResolverComputeFlags;
  CtxProc: TPasProcedure;
begin
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)=0) then
    exit(cExact);
  Params:=TParamsExpr(Expr);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Exit Params=',length(Params.Params));
  {$ENDIF}

  // first param: result
  Param:=Params.Params[0];
  Result:=cIncompatible;
  i:=ScopeCount-1;
  while (i>0) and (not (Scopes[i] is TPasProcedureScope)) do dec(i);
  if i>0 then
    begin
    // inside procedure: first param is function result
    ProcScope:=TPasProcedureScope(Scopes[i]);
    if ProcScope.DeclarationProc<>nil then
      CtxProc:=ProcScope.DeclarationProc
    else
      CtxProc:=TPasProcedure(ProcScope.Element);
    if not (CtxProc.ProcType is TPasFunctionType) then
      begin
      if RaiseOnError then
        RaiseMsg(20170216152312,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,['procedure exit'],Params.Params[0]);
      exit(cIncompatible);
      end;
    ResultEl:=TPasFunctionType(CtxProc.ProcType).ResultEl;
    ComputeResultElement(ResultEl,ResultResolved,[],Expr);
    end
  else
    begin
    // default: main program, param is an integer
    SetResolverTypeExpr(ResultResolved,btLongint,FBaseTypes[btLongint],FBaseTypes[btLongint],
                        [rrfReadable,rrfWritable]);
    end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Exit ResultResolved=',GetResolverResultDbg(ResultResolved));
  {$ENDIF}

  Flags:=[];
  if IsProcedureType(ResultResolved,true) then
    Include(Flags,rcNoImplicitProc);
  ComputeElement(Param,ParamResolved,Flags);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Exit ParamResolved=',GetResolverResultDbg(ParamResolved));
  {$ENDIF}

  // A class name is a value for a `class of` result, as in an assignment: the
  // LCL's `Exit(TBitmap)` in a function returning TRasterImageClass.
  if (rrfReadable in ParamResolved.Flags)
      or ((ParamResolved.IdentEl is TPasType)
        and (ResolveAliasType(TPasType(ParamResolved.IdentEl)) is TPasClassType)) then
    Result:=CheckAssignResCompatibility(ResultResolved,ParamResolved,Param,false);
  if Result=cIncompatible then
    begin
    if RaiseOnError then
      RaiseIncompatibleTypeRes(20170216152314,nIncompatibleTypeArgNo,
        [IntToStr(1)],ParamResolved,ResultResolved,Param);
    exit;
    end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

function TPasResolver.BI_IncDec_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, IncrResolved: TPasResolverResult;
  TypeEl: TPasType;
  bt: TResolverBaseType;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: var Integer
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  // The function's own name inside its body IS its Result, so it is assignable
  // here: rtl-console's keyboard.pp does `inc(shiftstate,kbCtrl)` inside
  // `function ShiftState: byte`. Same redirection SetLength already needs.
  RedirectSelfNameToResult(Param,ParamResolved,false);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.BI_IncDec_OnGetCallCompatibility ParamResolved=',GetResolverResultDbg(ParamResolved));
  {$ENDIF}
  Result:=cIncompatible;
  // Expr must be a variable
  if not ResolvedElCanBeVarParam(ParamResolved,Param) then
    begin
    // A STRING ELEMENT is deliberately assignable but not writable: `S[i]:=c`
    // is allowed, aliasing it as a var parameter is not. Inc/Dec neither alias
    // nor keep the address - they read, add and assign back - so FPC accepts
    // Inc(S[i]) and so must this. fcl-xml's WStrLower is Inc(word(S[i]),32),
    // and the cast passes the same flags through.
    if not (([rrfReadable,rrfAssignable]*ParamResolved.Flags=[rrfReadable,rrfAssignable])
        and (ParamResolved.ExprEl is TParamsExpr)
        and (TParamsExpr(ParamResolved.ExprEl).Kind=pekArrayParams)) then
      begin
      if RaiseOnError then
        RaiseVarExpected(20170216152319,Expr,ParamResolved.IdentEl);
      exit;
      end;
    end;
  bt:=ParamResolved.BaseType;
  if bt=btRange then
    bt:=ParamResolved.SubType;
  if bt in btAllInteger then
    Result:=cExact
  else if (bt in (btAllChars+btAllBooleans))
      and AllowIncDecOnOrdinal(bt,ParamResolved.LoTypeEl) then
    // Inc/Dec on a char or boolean, like the real FPC compiler (Inc(charVar) in
    // sysutils' float-rounding). Native targets only.
    Result:=cExact
  else if bt=btPointer then
    begin
    if ElHasBoolSwitch(Expr,bsPointerMath)
        or AllowIncDecOnPointer(ParamResolved.LoTypeEl) then
      Result:=cExact;
    end
  else if bt=btContext then
    begin
    TypeEl:=ParamResolved.LoTypeEl;
    if (TypeEl.ClassType=TPasPointerType)
        and (ElHasBoolSwitch(Expr,bsPointerMath)
          or AllowIncDecOnPointer(TypeEl)) then
      Result:=cExact
    else if TypeEl.ClassType=TPasRangeType then
      Result:=cExact
    else if (TypeEl.ClassType=TPasEnumType)
        and AllowIncDecOnOrdinal(bt,TypeEl) then
      // Inc/Dec on an enum value (native targets only).
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152320,1,Param,ParamResolved,'integer',RaiseOnError));

  if length(Params.Params)=1 then
    exit;

  // second param: increment/decrement
  Param:=Params.Params[1];
  ComputeElement(Param,IncrResolved,[]);
  Result:=cIncompatible;
  if rrfReadable in IncrResolved.Flags then
    begin
    if IncrResolved.BaseType in btAllInteger then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152322,2,Param,IncrResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_IncDec_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  FinishCallArgAccess(P[0],rraVarParam);
  if Length(P)>1 then
    FinishCallArgAccess(P[1],rraRead);
end;

function TPasResolver.BI_Assigned_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'Assigned'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  C: TClass;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: pointer, class, class instance, proc type or array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProcType]);
  Result:=cIncompatible;
  if ParamResolved.BaseType in [btNil,btPointer] then
    Result:=cExact
  else if (ParamResolved.BaseType=btContext) then
    begin
    C:=ParamResolved.LoTypeEl.ClassType;
    if (C=TPasClassType)
        or (C=TPasClassOfType)
        or (C=TPasPointerType)
        or C.InheritsFrom(TPasProcedureType)
        or ((C=TPasArrayType) and (length(TPasArrayType(ParamResolved.LoTypeEl).Ranges)=0)) then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152329,1,Param,ParamResolved,'class or array',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Assigned_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,btBoolean,Proc.Proc,
                     FBaseTypes[btBoolean],FBaseTypes[btBoolean],[rrfReadable]);
  if Params=nil then ;
end;

procedure TPasResolver.BI_Assigned_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExpr;
  ResolvedEl: TPasResolverResult;
begin
  if Proc=nil then ;
  P:=Params.Params[0];
  AccessExpr(P,rraRead);
  ComputeElement(P,ResolvedEl,[rcNoImplicitProcType,rcSetReferenceFlags]);
end;

function TPasResolver.BI_Chr_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: integer
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if rrfReadable in ParamResolved.Flags then
    begin
    if ParamResolved.BaseType in btAllInteger then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170325185321,1,Param,ParamResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Chr_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,BaseTypeChar,Proc.Proc,
    FBaseTypes[BaseTypeChar],FBaseTypes[BaseTypeChar],[rrfReadable]);
  if Params=nil then ;
end;

procedure TPasResolver.BI_Chr_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  Param: TPasExpr;
  Value: TResEvalValue;
begin
  Evaluated:=nil;
  Param:=Params.Params[0];
  Value:=Eval(Param,Flags);
  {$IFDEF VerbosePasResEval}
  {AllowWriteln}
  if Value=nil then
    writeln('TPasResolver.BI_Chr_OnEval Value=NIL')
  else
    writeln('TPasResolver.BI_Chr_OnEval Value=',Value.AsDebugString);
  {AllowWriteln-}
  {$ENDIF}
  if Value=nil then exit;
  try
    Evaluated:=fExprEvaluator.ChrValue(Value,Params);
  finally
    ReleaseEvalValue(Value);
  end;
  if Proc=nil then ;
end;

function TPasResolver.BI_Ord_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, ResolvedEl: TPasResolverResult;
  TypeEl: TPasType;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: bool, integer, enum or AnsiChar
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if rrfReadable in ParamResolved.Flags then
    begin
    if ParamResolved.BaseType in btArrayRangeTypes then
      Result:=cExact
    else if (ParamResolved.BaseType=btContext) and (ParamResolved.LoTypeEl is TPasEnumType) then
      Result:=cExact
    // Inside an unspecialized generic body the argument type is not known yet;
    // rtl-objpas' TRttiEnumerationType.GetName<T> does Ord(aValue) on one.
    else if IsGenericTemplType(ParamResolved) then
      Result:=cGenericExact
    else if ParamResolved.BaseType=btRange then
      begin
      if ParamResolved.SubType in btArrayRangeTypes then
        Result:=cExact
      else if ParamResolved.SubType=btContext then
        begin
        TypeEl:=ParamResolved.LoTypeEl;
        if TypeEl.ClassType=TPasRangeType then
          begin
          ComputeElement(TPasRangeType(TypeEl).RangeExpr.Left,ResolvedEl,[rcConstant]);
          if ResolvedEl.LoTypeEl.ClassType=TPasEnumType then
            exit(cExact);
          end;
        end;
      end;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152334,1,Param,ParamResolved,'enum or char',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Ord_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
// ord() answers the OPERAND's ordinal range, not always a longint. ppcx64 picks
// the Word overload over a Cardinal one for ord(AnsiChar)/ord(WideChar)/
// ord(Boolean)/ord(Byte) - both fit, the narrower wins - and reports
// ord(longint) and ord(enum) as genuinely ambiguous between the two. Typing
// every ord() as longint made BOTH candidates lossy, and
// CheckCallProcCompatibility flattens each conversion band so two candidates
// lossy on the same argument stay indistinguishable (tover3) - so the call came
// out ambiguous instead (fcl-md's GetProps(Ord(aChar))).
var
  ParamResolved: TPasResolverResult;
  bt: TResolverBaseType;
begin
  bt:=btLongint;
  if (Params<>nil) and (length(Params.Params)>0) then
    begin
    ComputeElement(Params.Params[0],ParamResolved,[]);
    case GetActualBaseType(ParamResolved.BaseType) of
    btAnsiChar,btBoolean,btByteBool,btByte: bt:=btByte;
    btWideChar,btWord,btWordBool: bt:=btWord;
    end;
    // Only the UNSIGNED narrow operands are narrowed. A SIGNED one (shortint,
    // smallint) keeps longint: narrowing it made `writeln(ord(SmallIntVar))`
    // print 65533 for -3, and it buys nothing here - the ambiguity only ever
    // involved targets that a non-negative range fits.
    // A target that does not offer the narrow base type keeps longint.
    if FBaseTypes[bt]=nil then
      bt:=btLongint;
    end;
  SetResolverIdentifier(ResolvedEl,bt,Proc.Proc,
    FBaseTypes[bt],FBaseTypes[bt],[rrfReadable]);
end;

procedure TPasResolver.BI_Ord_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  Param: TPasExpr;
  Value: TResEvalValue;
begin
  Evaluated:=nil;
  Param:=Params.Params[0];
  Value:=Eval(Param,Flags);
  {$IFDEF VerbosePasResEval}
  {AllowWriteln}
  if Value=nil then
    writeln('TPasResolver.BI_Ord_OnEval Value=NIL')
  else
    writeln('TPasResolver.BI_Ord_OnEval Value=',Value.AsDebugString);
  {AllowWriteln-}
  {$ENDIF}
  if Value=nil then exit;
  try
    Evaluated:=fExprEvaluator.OrdValue(Value,Params);
  finally
    if Evaluated<>Value then
      ReleaseEvalValue(Value);
  end;
  if Proc=nil then ;
end;

function TPasResolver.BI_LowHigh_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'Low' or 'High'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  C: TClass;
  bt: TResolverBaseType;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: enumtype, range, built-in ordinal type (char, longint, ...)
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  bt:=ParamResolved.BaseType;
  if bt in btAllRanges then
    // e.g. high(char)
    Result:=cExact
  else if bt=btSet then
    Result:=cExact
  else if bt in btAllStrings then
    Result:=cExact
  else if (bt=btContext) then
    begin
    C:=ParamResolved.LoTypeEl.ClassType;
    if (C=TPasArrayType)
        or (C=TPasSetType)
        or (C=TPasEnumType) then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.BI_LowHigh_OnGetCallCompatibility ParamResolved=',GetResolverResultDbg(ParamResolved));
    {$ENDIF}
    exit(CheckRaiseTypeArgNo(20170216152338,1,Param,ParamResolved,'ordinal type, array or set',RaiseOnError));
    end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_LowHigh_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
var
  ArrayEl: TPasArrayType;
  Param: TPasExpr;
  TypeEl: TPasType;
begin
  Param:=Params.Params[0];
  ComputeElement(Param,ResolvedEl,[]);
  // low/high of a SUBRANGE type answer a value of what that subrange is OF, as
  // the array branch below already reduces a range to its element type. Left as
  // the range, ord(low(TDigit)) read nothing and writeln refused it outright.
  if ResolvedEl.BaseType=btRange then
    ConvertRangeToElement(ResolvedEl);
  if ResolvedEl.BaseType=btContext then
    begin
    TypeEl:=ResolvedEl.LoTypeEl;
    if TypeEl.ClassType=TPasArrayType then
      begin
      // array: result type is type of first dimension
      ArrayEl:=TPasArrayType(TypeEl);
      if length(ArrayEl.Ranges)=0 then
        SetResolverIdentifier(ResolvedEl,BaseTypeLength,Proc.Proc,
          FBaseTypes[BaseTypeLength],FBaseTypes[BaseTypeLength],[rrfReadable])
      else
        begin
        ComputeElement(ArrayEl.Ranges[0],ResolvedEl,[rcConstant]);
        if ResolvedEl.BaseType=btRange then
          ConvertRangeToElement(ResolvedEl);
        end;
      end
    else if TypeEl.ClassType=TPasSetType then
      begin
      ResolvedEl.LoTypeEl:=TPasSetType(TypeEl).EnumType;
      ResolvedEl.HiTypeEl:=ResolvedEl.LoTypeEl;
      end;
    end
  else if ResolvedEl.BaseType=btSet then
    begin
    ResolvedEl.BaseType:=ResolvedEl.SubType;
    ResolvedEl.SubType:=btNone;
    // `set of rsPage..rsColumn`: the element type is a SUBRANGE, so low/high
    // answer the type that subrange is OF, exactly as the array branch above
    // reduces a range to its element type. Left as the range itself,
    // ord(low(T)) was refused as a "range type" (fcl-report's
    // ReportSectionsToString).
    if (ResolvedEl.BaseType=btContext)
        and (ResolvedEl.LoTypeEl<>nil)
        and (ResolvedEl.LoTypeEl.ClassType=TPasRangeType)
        and (TPasRangeType(ResolvedEl.LoTypeEl).RangeExpr<>nil) then
      begin
      ComputeElement(TPasRangeType(ResolvedEl.LoTypeEl).RangeExpr.Left,
        ResolvedEl,[rcConstant]);
      if ResolvedEl.BaseType=btRange then
        ConvertRangeToElement(ResolvedEl);
      end;
    end
  else if ResolvedEl.BaseType in btAllStrings then
    begin
    // high(aString)
    SetResolverIdentifier(ResolvedEl,BaseTypeLength,Proc.Proc,
      FBaseTypes[BaseTypeLength],FBaseTypes[BaseTypeLength],[rrfReadable]);
    end
  else
    ;// ordinal: result type is argument type
  ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable]+[rrfReadable];
end;

procedure TPasResolver.BI_LowHigh_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);

  function IsDynArrayConstExpr(IdentEl: TPasElement): boolean;
  begin
    Result:=false;
    if not (IdentEl is TPasVariable) then exit;
    if not (TPasVariable(IdentEl).Expr is TPasExpr) then exit;

    if (IdentEl.ClassType=TPasConst) and TPasConst(IdentEl).IsConst then
      exit(true);
    if fExprEvaluator.IsConst(Params) then
      exit(true); // a const refers an initial value
  end;

var
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;

  // low/high of a set type: the first/last value of its element type
  procedure EvalSetType(SetType: TPasSetType);
  var
    ElType: TPasType;
    ElEnum: TPasEnumType;
  begin
    ElType:=SetType.EnumType;
    if ElType.ClassType=TPasEnumType then
      begin
      // ElType is ALREADY the element type - reading .EnumType off it again
      // (as a TPasSetType) fetched a field that is not there, so high(set of
      // TEnum) silently answered 0 instead of the last enum value.
      ElEnum:=TPasEnumType(ElType);
      if Proc.BuiltIn=bfLow then
        Evaluated:=TResEvalEnum.CreateValue(Integer(GetEnumMinOrdinal(ElEnum)),TPasEnumValue(ElEnum.Values[0]))
      else
        Evaluated:=TResEvalEnum.CreateValue(Integer(GetEnumMaxOrdinal(ElEnum)),
          TPasEnumValue(ElEnum.Values[ElEnum.Values.Count-1]));
      end
    else if (ElType.ClassType=TPasRangeType)
        and (TPasRangeType(ElType).RangeExpr<>nil) then
      // `set of rsPage..rsColumn`: the bounds are the SUBRANGE's, so low is
      // rsPage and not the enum's first value.
      Evaluated:=EvalRangeLimit(TPasRangeType(ElType).RangeExpr,Flags,
        Proc.BuiltIn=bfLow,Param)
    else
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.BI_LowHigh_OnEval ',GetResolverResultDbg(ParamResolved),' ElType=',ElType.ClassName);
      {$ENDIF}
      RaiseNotYetImplemented(20170601203026,Params);
      end;
  end;

var
  TypeEl: TPasType;
  ArrayEl: TPasArrayType;
  Value: TResEvalValue;
  EnumType: TPasEnumType;
  aSet: TResEvalSet;
  bt: TResolverBaseType;
  Int, MinInt, MaxInt: TMaxPrecInt;
  Expr: TPasExpr;
begin
  Evaluated:=nil;
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  TypeEl:=ParamResolved.LoTypeEl;
  if ParamResolved.BaseType=btContext then
    begin
    if TypeEl.ClassType=TPasArrayType then
      begin
      // array: low/high of first dimension
      ArrayEl:=TPasArrayType(TypeEl);
      if length(ArrayEl.Ranges)=0 then
        begin
        // dyn or open array
        if Proc.BuiltIn=bfLow then
          Evaluated:=TResEvalInt.CreateValue(0)
        else if IsDynArrayConstExpr(ParamResolved.IdentEl) then
          begin
          Expr:=TPasVariable(ParamResolved.IdentEl).Expr;
          if Expr is TArrayValues then
            Evaluated:=TResEvalInt.CreateValue(TMaxPrecInt(length(TArrayValues(Expr).Values))-1)
          else if (Expr is TParamsExpr) and (TParamsExpr(Expr).Kind=pekSet) then
            Evaluated:=TResEvalInt.CreateValue(TMaxPrecInt(length(TParamsExpr(Expr).Params))-1);
          if Evaluated=nil then
            RaiseXExpectedButYFound(20170601191003,'array constant','expression',Params);
          end
        else
          exit;
        end
      else
        begin
        // static array
        Evaluated:=EvalRangeLimit(ArrayEl.Ranges[0],Flags,Proc.BuiltIn=bfLow,Param);
        end;
      end
    else if TypeEl.ClassType=TPasSetType then
      // set: first/last enum
      EvalSetType(TPasSetType(TypeEl))
    else if TypeEl.ClassType=TPasEnumType then
      begin
      EnumType:=TPasEnumType(TypeEl);
      if Proc.BuiltIn=bfLow then
        Evaluated:=TResEvalEnum.CreateValue(Integer(GetEnumMinOrdinal(EnumType)),TPasEnumValue(EnumType.Values[0]))
      else
        Evaluated:=TResEvalEnum.CreateValue(Integer(GetEnumMaxOrdinal(EnumType)),TPasEnumValue(EnumType.Values[EnumType.Values.Count-1]));
      end;
    end
  else if ParamResolved.BaseType=btSet then
    begin
    if (ParamResolved.IdentEl is TPasVariable)
        and (ParamResolved.IdentEl.ClassType<>TPasConst) then
      begin
      // low/high of a set VARIABLE (fpc only): the bounds of its set type
      TypeEl:=ResolveAliasType(TPasVariable(ParamResolved.IdentEl).VarType);
      if TypeEl is TPasSetType then
        begin
        EvalSetType(TPasSetType(TypeEl));
        exit;
        end;
      end;
    Value:=Eval(Param,Flags);
    if Value=nil then exit;
    case Value.Kind of
    revkSetOfInt:
      begin
      aSet:=TResEvalSet(Value);
      if length(aSet.Ranges)=0 then
        RaiseXExpectedButYFound(20170601201637,'ordinal value',Value.AsString,Param);
      if Proc.BuiltIn=bfLow then
        Int:=aSet.RangeStart
      else
        Int:=aSet.RangeEnd;
      case aSet.ElKind of
        revskEnum:
          begin
          EnumType:=aSet.IdentEl as TPasEnumType;
          Evaluated:=TResEvalEnum.CreateValue(Int,GetEnumValueForOrdinal(EnumType,Int));
          end;
        revskInt:
          Evaluated:=TResEvalInt.CreateValue(Int);
        revskChar:
          {$ifdef FPC_HAS_CPSTRING}
          if Int<256 then
            Evaluated:=TResEvalString.CreateValue(chr(Int))
          else
          {$endif}
            Evaluated:=TResEvalUTF16.CreateValue(widechar(Int));
        revskBool:
          if Int=0 then
            Evaluated:=TResEvalBool.CreateValue(false)
          else
            Evaluated:=TResEvalBool.CreateValue(true)
      end;
      end;
    revkRangeInt:
      // a set TYPE evaluates to the range of its element type
      begin
      if Proc.BuiltIn=bfLow then
        Int:=TResEvalRangeInt(Value).RangeStart
      else
        Int:=TResEvalRangeInt(Value).RangeEnd;
      case TResEvalRangeInt(Value).ElKind of
        revskEnum:
          Evaluated:=TResEvalEnum.CreateValue(Int,GetEnumValueForOrdinal(
            TResEvalRangeInt(Value).ElType as TPasEnumType,Int));
        revskChar:
          {$ifdef FPC_HAS_CPSTRING}
          if Int<256 then
            Evaluated:=TResEvalString.CreateValue(chr(Int))
          else
          {$endif}
            Evaluated:=TResEvalUTF16.CreateValue(widechar(Int));
        revskBool:
          Evaluated:=TResEvalBool.CreateValue(Int<>0);
      else
        Evaluated:=TResEvalInt.CreateValue(Int);
      end;
      end;
    else
      RaiseXExpectedButYFound(20170601201237,'ordinal value',Value.AsString,Param);
    end;
    end
  else if (TypeEl is TPasUnresolvedSymbolRef)
      and (TypeEl.CustomData is TResElDataBaseType) then
    begin
    // low,high(base type)
    bt:=TResElDataBaseType(TypeEl.CustomData).BaseType;
    bt:=GetActualBaseType(bt);
    if bt in btAllBooleans then
      Evaluated:=TResEvalBool.CreateValue(Proc.BuiltIn=bfHigh)
    {$ifdef HasInt64}
    else if bt=btQWord then
      begin
      if Proc.BuiltIn=bfLow then
        Evaluated:=TResEvalInt.CreateValue(0)
      else
        Evaluated:=TResEvalUInt.CreateValue(High(QWord));
      end
    {$endif}
    else if (bt in btAllIntegerNoQWord) and GetIntegerRange(bt,MinInt,MaxInt) then
      begin
      if Proc.BuiltIn=bfLow then
        Evaluated:=TResEvalInt.CreateValue(MinInt)
      else
        Evaluated:=TResEvalInt.CreateValue(MaxInt);
      end
    {$ifdef FPC_HAS_CPSTRING}
    else if bt=btAnsiChar then
      begin
      if Proc.BuiltIn=bfLow then
        Evaluated:=TResEvalString.CreateValue(#0)
      else
        Evaluated:=TResEvalString.CreateValue(#255);
      end
    {$endif}
    else if bt=btWideChar then
      begin
      if Proc.BuiltIn=bfLow then
        Evaluated:=TResEvalUTF16.CreateValue(#0)
      else
        Evaluated:=TResEvalUTF16.CreateValue(#$ffff);
      end
    else if bt in btAllStrings then
      begin
      if Proc.BuiltIn=bfLow then
        Evaluated:=TResEvalInt.CreateValue(1)
      else
        Evaluated:=EvalLengthOfString(ParamResolved,Param,Flags);
      end
    else
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.BI_LowHigh_OnEval ',GetResolverResultDbg(ParamResolved));
      {$ENDIF}
      RaiseNotYetImplemented(20170602070738,Params);
      end;
    end
  else if ParamResolved.LoTypeEl is TPasRangeType then
    begin
    // e.g. type t = 2..10;
    Evaluated:=EvalRangeLimit(TPasRangeType(TypeEl).RangeExpr,FLags,Proc.BuiltIn=bfLow,Param);
    end
  else if ParamResolved.BaseType in btAllStrings then
    begin
    if Proc.BuiltIn=bfLow then
      Evaluated:=TResEvalInt.CreateValue(1)
    else
      Evaluated:=EvalLengthOfString(ParamResolved,Param,Flags);
    end
  else
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.BI_LowHigh_OnEval ',GetResolverResultDbg(ParamResolved));
    {$ENDIF}
    RaiseNotYetImplemented(20170601202353,Params);
    end;
  {$IFDEF VerbosePasResEval}
  {AllowWriteln}
  if Evaluated=nil then
    writeln('TPasResolver.BI_LowHigh_OnEval END ResolvedEl=',GetResolverResultDbg(ParamResolved),' Evaluated NO SET')
  else
    writeln('TPasResolver.BI_LowHigh_OnEval END ResolvedEl=',GetResolverResultDbg(ParamResolved),' Evaluated=',Evaluated.AsDebugString);
  {AllowWriteln-}
  {$ENDIF}
end;

function TPasResolver.BI_PredSucc_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'Pred' or 'Succ'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: enum, range, set, char or integer
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if CheckIsOrdinal(ParamResolved,Param,false) then
    Result:=cExact
  else if IsGenericTemplType(ParamResolved) or IsDeferredTemplMember(Param) then
    // `Pred(T.Const)` on an unbound template: what it stands for is only known at
    // specialization (rtl-generics writes `array[0..Pred(TCuckooCfg.D)]`)
    Result:=cGenericExact;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152343,1,Param,ParamResolved,'ordinal',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_PredSucc_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  ComputeElement(Params.Params[0],ResolvedEl,[]);
  ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable];
  if Proc=nil then ;
end;

procedure TPasResolver.BI_PredSucc_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  Param: TPasExpr;
begin
  //writeln('TPasResolver.BI_PredSucc_OnEval START');
  Evaluated:=nil;
  Param:=Params.Params[0];
  Evaluated:=Eval(Param,Flags);
  //writeln('TPasResolver.BI_PredSucc_OnEval Param=',Evaluated<>nil);
  if Evaluated=nil then exit;
  //writeln('TPasResolver.BI_PredSucc_OnEval Param=',Evaluated.AsString);
  if Evaluated.Element<>nil then
    Evaluated:=Evaluated.Clone;
  if Proc.BuiltIn=bfPred then
    fExprEvaluator.PredValue(Evaluated,Params)
  else
    fExprEvaluator.SuccValue(Evaluated,Params);
end;

function TPasResolver.BI_Str_CheckParam(IsFunc: boolean; Param: TPasExpr;
  const ParamResolved: TPasResolverResult; ArgNo: integer; RaiseOnError: boolean
  ): integer;

  function CheckFormat(FormatExpr: TPasExpr; Index: integer;
    const ParamResolved: TPasResolverResult): boolean;
  var
    ResolvedEl: TPasResolverResult;
    Ok: Boolean;
  begin
    if FormatExpr=nil then exit(true);
    Result:=false;
    Ok:=false;
    if ParamResolved.BaseType in btAllFloats then
      // floats supports value:Width:Precision
      Ok:=true
    else
      // all other only support value:Width
      Ok:=Index<2;
    if not Ok then
      begin
      if RaiseOnError then
        RaiseMsg(20170319222319,nIllegalExpression,sIllegalExpression,[],FormatExpr);
      exit;
      end;
    ComputeElement(FormatExpr,ResolvedEl,[]);
    if not (ResolvedEl.BaseType in btAllInteger) then
      begin
      if RaiseOnError then
        RaiseXExpectedButYFound(20170319221515,
          'integer',GetResolverResultDescription(ResolvedEl,true),FormatExpr);
      exit;
      end;
    if not (rrfReadable in ResolvedEl.Flags) then
      begin
      if RaiseOnError then
        RaiseMsg(20170319221755,nNotReadable,sNotReadable,[],FormatExpr);
      exit;
      end;
    Result:=true;
  end;

var
  bt: TResolverBaseType;
  C: TClass;
begin
  Result:=cIncompatible;
  bt:=ParamResolved.BaseType;
  if bt=btRange then
    bt:=ParamResolved.SubType;
  if bt in (btAllInteger+btAllBooleans+btAllFloats) then
    Result:=cExact
  else if IsFunc and (bt in btAllStringAndChars) then
    Result:=cExact
  else if bt=btContext then
    begin
    C:=ParamResolved.LoTypeEl.ClassType;
    if (C=TPasEnumType) or (C=TPasRangeType) then
      Result:=cExact
    else if C=TPasGenericTemplateType then
      // Inside a generic, a value whose type is a type parameter: the concrete
      // type (and thus whether Str accepts it) is only known after
      // specialization, so accept it here and let the specialized body be
      // re-checked. Otherwise Str(fieldOfTypeT,...) failed to compile even
      // though the specialization uses e.g. integer.
      Result:=cExact
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170319220517,ArgNo,Param,ParamResolved,'boolean, integer, enum value',RaiseOnError));
  if not CheckFormat(Param.Format1,1,ParamResolved) then
    exit(cIncompatible);
  if not CheckFormat(Param.Format2,2,ParamResolved) then
    exit(cIncompatible);
end;

function TPasResolver.BI_StrProc_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built-in procedure 'Str'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,2,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);
  if ParentNeedsExprResult(Params) then
    begin
    if RaiseOnError then
      RaiseMsg(20170326084331,nIncompatibleTypesGotExpected,
        sIncompatibleTypesGotExpected,['procedure str','function str'],Params);
    exit(cIncompatible);
    end;

  // first param: boolean, integer, enum, class instance
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=BI_Str_CheckParam(false,Param,ParamResolved,1,RaiseOnError);
  if Result=cIncompatible then
    exit;

  // second parameter: string variable
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if ParamResolved.BaseType in btAllStrings then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170319220806,1,Param,ParamResolved,'string variable',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_StrProc_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  if P=nil then ;
  FinishCallArgAccess(P[0],rraRead);
  FinishCallArgAccess(P[1],rraVarParam);
end;

function TPasResolver.BI_StrFunc_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  i: Integer;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);
  if not ParentNeedsExprResult(Params) then
    begin
    // not in an expression -> the 'procedure str' is needed, not the 'function str'
    if RaiseOnError then
      RaiseMsg(20170326084622,nIncompatibleTypesGotExpected,
        sIncompatibleTypesGotExpected,['function str','procedure str'],Params);
    exit(cIncompatible);
    end;

  // param: string, boolean, integer, enum, class instance
  for i:=0 to length(Params.Params)-1 do
    begin
    Param:=Params.Params[i];
    ComputeElement(Param,ParamResolved,[]);
    Result:=BI_Str_CheckParam(true,Param,ParamResolved,i+1,RaiseOnError);
    if Result=cIncompatible then
    exit;
    end;

  Result:=cExact;
end;

procedure TPasResolver.BI_StrFunc_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,btString,Proc.Proc,
    FBaseTypes[btString],FBaseTypes[btString],[rrfReadable]);
  if Params=nil then ;
  if Proc=nil then ;
end;

procedure TPasResolver.BI_StrFunc_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
begin
  Evaluated:=fExprEvaluator.EvalStrFunc(Params,Flags);
  if Proc=nil then ;
end;

function TPasResolver.BI_WriteStrProc_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built-in procedure 'Str'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  i: Integer;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,2,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first parameter: string variable
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if ParamResolved.BaseType in btAllStrings then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20180527190304,1,Param,ParamResolved,'string variable',RaiseOnError));

  // other parameters: boolean, integer, enum, class instance
  for i:=1 to length(Params.Params)-1 do
    begin
    Param:=Params.Params[i];
    ComputeElement(Param,ParamResolved,[]);
    Result:=BI_Str_CheckParam(false,Param,ParamResolved,i,RaiseOnError);
    if Result=cIncompatible then
      exit;
    end;
end;

procedure TPasResolver.BI_WriteStrProc_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
  i: Integer;
begin
  if Proc=nil then ;
  P:=Params.Params;
  if P=nil then ;
  FinishCallArgAccess(P[0],rraOutParam);
  for i:=0 to length(Params.Params)-1 do
    FinishCallArgAccess(P[i],rraRead);
end;

function TPasResolver.BI_Val_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built-in procedure 'Val(const s: string; out v: valtype; out code: integer)'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  bt: TResolverBaseType;
  C: TClass;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,3,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first parameter: string
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if ParamResolved.BaseType in btAllStrings then
    Result:=cExact;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20181214141250,1,Param,ParamResolved,'string',RaiseOnError));

  // second parameter: var value
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    bt:=ParamResolved.BaseType;
    if bt=btRange then
      bt:=ParamResolved.SubType;
    if bt in (btAllInteger+btAllBooleans+btAllFloats) then
      Result:=cExact
    else if bt=btContext then
      begin
      C:=ParamResolved.LoTypeEl.ClassType;
      if (C=TPasEnumType) or (C=TPasRangeType) then
        Result:=cExact
      else if C=TPasGenericTemplateType then
        // Inside a generic, a value whose type is a type parameter: the concrete
        // type (and thus whether Val accepts it) is only known after
        // specialization, so accept it here and re-check the specialized body.
        Result:=cExact;
      end;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20181214141704,2,Param,ParamResolved,
         'boolean/integer/float/enum variable',RaiseOnError));

  // third parameter: out Code: integer
  Param:=Params.Params[2];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if ParamResolved.BaseType in btAllInteger then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20181214141511,3,Param,ParamResolved,'integer variable',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,3,RaiseOnError);
end;

procedure TPasResolver.BI_Val_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  if P=nil then ;
  FinishCallArgAccess(P[0],rraRead);
  FinishCallArgAccess(P[1],rraOutParam);
  FinishCallArgAccess(P[2],rraOutParam);
end;

function TPasResolver.BI_LoHi_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    Exit(cIncompatible);
  Params:=TParamsExpr(Expr);
  // first Param: any integer type
  Param:=Params.params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if (rrfReadable in ParamResolved.Flags)
      and (ParamResolved.BaseType in btAllInteger)
  then
    Result:=cExact;
  if Result=cIncompatible then
    Exit(CheckRaiseTypeArgNo(20190128232600,1,Param,ParamResolved,'integer type',RaiseOnError));
  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_LoHi_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
var
  ResolvedParam: TPasResolverResult;
  BaseType: TResolverBaseType;
  Mask: LongWord;
begin
  ComputeElement(Params.Params[0],ResolvedParam,[]);
  GetShiftAndMaskForLoHiFunc(ResolvedParam.BaseType, Proc.BuiltIn=bfLo, Mask);
  case Mask of
    $F, $FF: BaseType := btByte;
    $FFFF: BaseType := btWord;
    else { $FFFFFFFF } BaseType := btLongWord;
  end;
  SetResolverIdentifier(ResolvedEl,BaseType,Proc.Proc,
    FBaseTypes[BaseType],FBaseTypes[BaseType],[rrfReadable]);
end;

procedure TPasResolver.BI_LoHi_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  Param: TPasExpr;
  ResolvedParam: TPasResolverResult;
  Value: TResEvalValue;
  Shift: Integer;
  Mask: LongWord;
begin
  Evaluated := nil;
  Param := Params.Params[0];
  Value := Eval(Param,Flags);
  {$IFDEF VerbosePasResEval}
  {AllowWriteln}
  if value=nil then
    writeln('TPasResolver.BI_LoHi_OnEval Value=NIL')
  else
    writeln('TPasResolver.BI_LoHi_OnEval Value=',value.AsDebugString);
  {AllowWriteln-}
  {$ENDIF}
  if Value=nil then exit;
  try
    ComputeElement(Param,ResolvedParam,[]);
    Shift := GetShiftAndMaskForLoHiFunc(ResolvedParam.BaseType, Proc.BuiltIn=bfLo, Mask);
    Evaluated := fExprEvaluator.ShiftAndMaskValue(Value,Shift,Mask,Params);
  finally
    ReleaseEvalValue(Value);
  end;
end;

function TPasResolver.BI_ConcatArray_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, ElTypeResolved, FirstElTypeResolved: TPasResolverResult;
  i: Integer;
  ArrType: TPasArrayType;
  ElType: TPasType;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  FirstElTypeResolved:=Default(TPasResolverResult);
  for i:=0 to length(Params.Params)-1 do
    begin
    // all params: array
    Param:=Params.Params[i];
    ComputeElement(Param,ParamResolved,[]);
    ElTypeResolved:=default(TPasResolverResult);
    if rrfReadable in ParamResolved.Flags then
      begin
      if ParamResolved.BaseType=btContext then
        begin
        if IsDynArray(ParamResolved.LoTypeEl) then
          begin
          ArrType:=TPasArrayType(ParamResolved.LoTypeEl);
          ElType:=GetArrayElType(ArrType);
          ComputeElement(ElType,ElTypeResolved,[rcType]);
          end;
        end
      else if ParamResolved.BaseType in [btArrayLit,btArrayOrSet] then
        SetResolverValueExpr(ElTypeResolved,ParamResolved.SubType,
          ParamResolved.LoTypeEl,ParamResolved.HiTypeEl,Param,ParamResolved.Flags);
      end;
    if ElTypeResolved.BaseType=btNone then
      exit(CheckRaiseTypeArgNo(20170329181206,i+1,Param,ParamResolved,'dynamic array',RaiseOnError));
    Include(ElTypeResolved.Flags,rrfReadable);
    if i=0 then
      begin
      FirstElTypeResolved:=ElTypeResolved;
      Include(FirstElTypeResolved.Flags,rrfWritable);
      end
    else if CheckAssignResCompatibility(FirstElTypeResolved,ElTypeResolved,Param,RaiseOnError)=cIncompatible then
      exit(cIncompatible);
    end;
  Result:=cExact;
end;

procedure TPasResolver.BI_ConcatArray_OnGetCallResult(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult);
begin
  ComputeElement(Params.Params[0],ResolvedEl,[]);
  ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable];
  ResolvedEl.ExprEl:=Params;
  ResolvedEl.IdentEl:=nil;
  if ResolvedEl.BaseType=btArrayOrSet then
    ResolvedEl.BaseType:=btArrayLit;
  if Proc=nil then ;
end;

function TPasResolver.BI_ConcatString_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  i: Integer;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  for i:=0 to length(Params.Params)-1 do
    begin
    // all params: char or string
    Param:=Params.Params[i];
    ComputeElement(Param,ParamResolved,[]);
    if not (rrfReadable in ParamResolved.Flags)
        or not (ParamResolved.BaseType in btAllStringAndChars) then
      exit(CheckRaiseTypeArgNo(20181219230329,i+1,Param,ParamResolved,'string',RaiseOnError));
    end;
  Result:=cExact;
end;

procedure TPasResolver.BI_ConcatString_OnGetCallResult(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult);
var
  i: Integer;
  Param: TPasExpr;
  ParamResolved, CombinedResolved: TPasResolverResult;
  ParamsArr: TPasExprArray;
begin
  if Proc=nil then ;
  ParamsArr:=Params.Params;
  for i:=0 to length(ParamsArr)-1 do
    begin
    // all params: char or string
    Param:=ParamsArr[i];
    ComputeElement(Param,ParamResolved,[]);
    if i=0 then
      ResolvedEl:=ParamResolved
    else
      begin
      ComputeAddStringRes(ResolvedEl,ParamResolved,Params,CombinedResolved);
      ResolvedEl:=CombinedResolved;
      end;
    end;
end;

procedure TPasResolver.BI_ConcatString_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  i: Integer;
  Param: TPasExpr;
  Value, NewValue: TResEvalValue;
  ok: Boolean;
begin
  if Proc=nil then ;
  Value:=nil;
  Evaluated:=nil;
  ok:=false;
  try
    for i:=0 to length(Params.Params)-1 do
      begin
      // all params: char or string
      Param:=Params.Params[i];
      Value:=Eval(Param,Flags);
      if Value=nil then
        exit;
      if i=0 then
        begin
        Evaluated:=Value;
        Value:=nil;
        end
      else
        begin
        NewValue:=ExprEvaluator.EvalStringAddExpr(Param,Params.Params[i-1],Param,
          Evaluated,Value);
        ReleaseEvalValue(Evaluated);
        Evaluated:=NewValue;
        ReleaseEvalValue(Value);
        end;
      end;
    ok:=true;
  finally
    ReleaseEvalValue(Value);
    if not ok then
      ReleaseEvalValue(Evaluated);
  end;
end;

function TPasResolver.BI_CopyArray_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // first param: array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  if rrfReadable in ParamResolved.Flags then
    begin
    if ParamResolved.BaseType=btContext then
      begin
      if IsDynArray(ParamResolved.LoTypeEl) then
        Result:=cExact;
      end
    else if ParamResolved.BaseType in [btArrayLit,btArrayOrSet] then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170329153951,1,Param,ParamResolved,'dynamic array',RaiseOnError));
  if length(Params.Params)=1 then
    exit(cExact);

  // check optional Start index
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
      or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329164210,2,Param,ParamResolved,'integer',RaiseOnError));
  if length(Params.Params)=2 then
    exit(cExact);

  // check optional Count
  Param:=Params.Params[2];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
      or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329164329,3,Param,ParamResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,3,RaiseOnError);
end;

procedure TPasResolver.BI_CopyArray_OnGetCallResult(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult);
begin
  if Proc=nil then ;
  ComputeElement(Params.Params[0],ResolvedEl,[]);
  ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable];
  ResolvedEl.ExprEl:=Params;
  ResolvedEl.IdentEl:=nil;
  if ResolvedEl.BaseType=btArrayOrSet then
    ResolvedEl.BaseType:=btArrayLit;
end;

function TPasResolver.BI_Slice_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// Slice(var arr; count: integer) — an open-array view of the first `count`
// elements of arr. Only meaningful as an open-array argument.
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,2,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // first param: an array variable (static or dynamic)
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  if (ParamResolved.BaseType=btContext)
      and (ParamResolved.LoTypeEl is TPasArrayType)
      and (rrfReadable in ParamResolved.Flags) then
    Result:=cExact;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20260802120000,1,Param,ParamResolved,'array',RaiseOnError));

  // second param: element count
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
      or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20260802120001,2,Param,ParamResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_Slice_OnGetCallResult(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult);
begin
  if Proc=nil then ;
  // The result is the same array type, viewed as a read-only value; the code
  // generator emits it as an open-array (data,high=count-1) pair.
  ComputeElement(Params.Params[0],ResolvedEl,[]);
  ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable];
  ResolvedEl.ExprEl:=Params;
  ResolvedEl.IdentEl:=nil;
end;

function TPasResolver.BI_InsertArray_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// Insert(Item,var Array,Index)
var
  Params: TParamsExpr;
  Param, ItemParam: TPasExpr;
  ItemResolved, ParamResolved, ElTypeResolved: TPasResolverResult;
  ArrType: TPasArrayType;
  ElType: TPasType;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,3,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // check Item
  ItemParam:=Params.Params[0];
  ComputeElement(ItemParam,ItemResolved,[]);
  if not (rrfReadable in ItemResolved.Flags) then
    exit(CheckRaiseTypeArgNo(20170329171400,1,ItemParam,ItemResolved,'value',RaiseOnError));

  // check Array
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  if not ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if RaiseOnError then
      RaiseVarExpected(20170329171514,Param,ParamResolved.IdentEl);
    exit;
    end;
  if (ParamResolved.BaseType<>btContext)
      or not IsDynArray(ParamResolved.LoTypeEl) then
    exit(CheckRaiseTypeArgNo(20170329172024,2,Param,ParamResolved,'dynamic array',RaiseOnError));
  ArrType:=TPasArrayType(ParamResolved.LoTypeEl);
  ElType:=GetArrayElType(ArrType);
  ComputeElement(ElType,ElTypeResolved,[rcType]);
  if CheckAssignResCompatibility(ElTypeResolved,ItemResolved,ItemParam,RaiseOnError)=cIncompatible then
    exit(cIncompatible);

  // check insert Index
  Param:=Params.Params[2];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
      or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329172348,3,Param,ParamResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,3,RaiseOnError);
end;

procedure TPasResolver.BI_InsertArray_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
  Param0, Param1: TPasExpr;
  ArrayResolved, ElTypeResolved: TPasResolverResult;
  ElType: TPasType;
begin
  if Proc=nil then ;
  P:=Params.Params;
  Param0:=P[0];
  Param1:=P[1];
  FinishCallArgAccess(Param0,rraRead);
  FinishCallArgAccess(Param1,rraVarParam);
  FinishCallArgAccess(P[2],rraRead);
  if not (Param0 is TPrimitiveExpr) then
    begin
    // insert complex expression, e.g. insert([1],Arr,index)
    // -> mark array and set literals. Only applies to the dyn-array form; the
    // string form (Insert(expr,var S:string,index)) needs no marking.
    ComputeElement(Param1,ArrayResolved,[]);
    if (ArrayResolved.BaseType=btContext)
        and IsDynArray(ArrayResolved.LoTypeEl) then
      begin
      ElType:=GetArrayElType(TPasArrayType(ArrayResolved.LoTypeEl));
      ComputeElement(ElType,ElTypeResolved,[rcType]);
      if (ElTypeResolved.BaseType=btContext)
          and (ElTypeResolved.LoTypeEl.ClassType=TPasArrayType) then
        MarkArrayExprRecursive(Param0,TPasArrayType(ElTypeResolved.LoTypeEl));
      end;
    end;
end;

function TPasResolver.BI_DeleteArray_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// DeleteScope(var Array; Start, Count: integer)
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,3,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // check Array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  if not ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if RaiseOnError then
      RaiseVarExpected(20170329173421,Param,ParamResolved.IdentEl);
    exit;
    end;
  if (ParamResolved.BaseType<>btContext)
      or not IsDynArray(ParamResolved.LoTypeEl) then
    exit(CheckRaiseTypeArgNo(20170329173434,1,Param,ParamResolved,'dynamic array',RaiseOnError));

  // check param Start
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
     or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329173613,2,Param,ParamResolved,'integer',RaiseOnError));

  // check param Count
  Param:=Params.Params[2];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
      or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329172348,3,Param,ParamResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,3,RaiseOnError);
end;

procedure TPasResolver.BI_DeleteArray_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  if P=nil then ;
  FinishCallArgAccess(P[0],rraVarParam);
  FinishCallArgAccess(P[1],rraRead);
  FinishCallArgAccess(P[2],rraRead);
end;

function TPasResolver.BI_TypeInfo_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  aType: TPasType;
  ParamResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  Param:=Params.Params[0];
  aType:=GetTypeInfoParamType(Param,ParamResolved,true);
  if aType=nil then
    RaiseMsg(20170411100259,nTypeIdentifierExpected,sTypeIdentifierExpected,[],Param);
  aType:=ResolveAliasType(aType);
  if not HasTypeInfo(aType) then
    RaiseMsg(20170413200118,nSymbolCannotBePublished,sSymbolCannotBePublished,[],Param);
  // Enums with holes have no type information (trtti22/23). Inside a generic
  // specialization a generic-parameter enum-with-holes is allowed — TypeInfo
  // returns nil at runtime instead of being a compile error (trtti21).
  if (aType is TPasEnumType) and EnumHasHoles(TPasEnumType(aType))
      and not FInSpecialize then
    RaiseMsg(20260301130000,nSymbolCannotBePublished,sSymbolCannotBePublished,[],Param);

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_TypeInfo_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  if Proc=nil then;
  if Params=nil then ;
  SetResolverTypeExpr(ResolvedEl,btPointer,
    FBaseTypes[btPointer],FBaseTypes[btPointer],[rrfReadable]);
end;

function TPasResolver.BI_GetTypeKind_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  aType: TPasType;
  ParamResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);
  Param:=Params.Params[0];
  aType:=GetTypeInfoParamType(Param,ParamResolved,true);
  if aType=nil then
    RaiseMsg(20200826205441,nTypeIdentifierExpected,sTypeIdentifierExpected,[],Param);

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_GetTypeKind_OnGetCallResult(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult);
var
  El: TPasElement;
  EnumType: TPasEnumType;
begin
  El:=FindSystemIdentifier('system','ttypekind',Params);
  if not (El is TPasEnumType) then
    RaiseXExpectedButYFound(20200826211458,'enum type System.TTypeKind',GetElementTypeName(El),Params);
  EnumType:=TPasEnumType(El);
  SetResolverTypeExpr(ResolvedEl,btContext,EnumType,EnumType,[rrfReadable]);

  if Proc=nil then ;
end;

procedure TPasResolver.BI_GetTypeKind_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  aType: TPasType;
  El: TPasElement;
  TypeKindType: TPasEnumType;
  C: TClass;
  aClass: TPasClassType;
  bt: TResolverBaseType;
  Value: TPasEnumValue;
  aName: String;
  i: Integer;
  ParamResolved: TPasResolverResult;
begin
  Evaluated:=nil;

  aType:=GetTypeInfoParamType(Params.Params[0],ParamResolved,true);
  C:=aType.ClassType;
  aName:='tkUnknown';
  if C=TPasEnumType then
    aName:='tkEnumeration'
  else if C=TPasSetType then
    aName:='tkSet'
  else if C=TPasRecordType then
    aName:='tkRecord'
  else if C=TPasClassType then
    begin
    aClass:=TPasClassType(aType);
    case aClass.ObjKind of
    okObject:  aName:='tkObject';
    okInterface:
      case aClass.InterfaceType of
      citCom: aName:='tkInterface';
      else aName:='tkInterfaceRaw';
      end;
    okClassHelper, okRecordHelper, okTypeHelper: aName:='tkHelper';
    else
      aName:='tkClass';
    end;
    end
  else if C=TPasClassOfType then
    aName:='tkClassRef'
  else if C.InheritsFrom(TPasProcedure) then
    aName:='tkMethod'
  else if C.InheritsFrom(TPasProcedureType) then
    aName:='tkProcVar'
  else
    begin
    bt:=ParamResolved.BaseType;
    case bt of
    btChar: {$ifdef FPC_HAS_CPSTRING}if BaseTypeChar=btAnsiChar then aName:='tkChar' else {$ENDIF}aName:='tkWChar';
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiChar: aName:='tkChar';
    {$endif}
    btWideChar: aName:='tkWChar';
    btString: {$ifdef FPC_HAS_CPSTRING}if BaseTypeString=btAnsiString then aName:='tkAString' else {$ENDIF}aName:='tkUString';
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiString,
    btShortString,
    btRawByteString: aName:='tkAString';
    {$endif}
    btWideString: aName:='tkWString';
    btUnicodeString: aName:='tkUString';
    btPointer: aName:='tkPointer';
    {$ifdef HasInt64}
    btQWord,
    btInt64,
    btComp: aName:='tkInt64';
    {$endif}
    else
      if bt in btAllBooleans then
        aName:='tkBool'
      else if bt in btAllInteger then
        aName:='tkInteger'
      else if bt in btAllFloats then
        aName:='tkFloat';
    end;
    end;

  El:=FindSystemIdentifier('system','ttypekind',Params);
  TypeKindType:=El as TPasEnumType;

  for i:=0 to TypeKindType.Values.Count-1 do
    begin
    Value:=TPasEnumValue(TypeKindType.Values[i]);
    if SameText(aName,Value.Name) then
      begin
      Evaluated:=TResEvalEnum.CreateValue(Integer(GetEnumValueOrdinal(Value)),Value);
      exit;
      end;
    end;

  if Proc=nil then ;
  if Flags=[] then ;
end;

function TPasResolver.BI_Assert_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built-in procedure 'Assert'
//  Assert(bool)
//  Assert(bool,string)
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: boolean
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
     or not (ParamResolved.BaseType in btAllBooleans) then
    exit(CheckRaiseTypeArgNo(20180117123819,1,Param,ParamResolved,'boolean',RaiseOnError));

  // optional second parameter: string
  if length(Params.Params)>1 then
    begin
    Param:=Params.Params[1];
    ComputeElement(Param,ParamResolved,[]);
    if not (rrfReadable in ParamResolved.Flags)
       or not (ParamResolved.BaseType in btAllStringAndChars) then
      exit(CheckRaiseTypeArgNo(20180117123932,2,Param,ParamResolved,'string',RaiseOnError));
    end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_Assert_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
begin
  FinishAssertCall(Proc,Params);
end;

function TPasResolver.BI_New_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param, Param2, CtorExpr: TPasExpr;
  TypeEl, SubTypeEl: TPasType;
  ParamResolved: TPasResolverResult;
  CtorName: string;
  Found: Boolean;
  i: Integer;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: var PRecord
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.BI_New_OnGetCallCompatibility ParamResolved=',GetResolverResultDbg(ParamResolved));
  {$ENDIF}
  Result:=cIncompatible;
  // Expr must be a variable
  if not ResolvedElCanBeVarParam(ParamResolved,Expr) then
    begin
    if RaiseOnError then
      RaiseVarExpected(20180425005303,Expr,ParamResolved.IdentEl);
    exit;
    end;
  SubTypeEl:=nil;
  if ParamResolved.BaseType=btContext then
    begin
    TypeEl:=ParamResolved.LoTypeEl;
    if TypeEl.ClassType=TPasPointerType then
      begin
      SubTypeEl:=ResolveAliasType(TPasPointerType(TypeEl).DestType);
      if SubTypeEl.ClassType=TPasRecordType then
        Result:=cExact
      else if (SubTypeEl.ClassType=TPasClassType)
          and (TPasClassType(SubTypeEl).ObjKind=okObject) then
        Result:=cExact;
      end;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20180425005421,1,Param,ParamResolved,'pointer of record or object',RaiseOnError));

  // Handle 2nd param: constructor identifier (e.g., New(pp, Init(3,4)))
  if Length(Params.Params) >= 2 then
  begin
    Param2 := Params.Params[1];
    // Extract the constructor name from Init(args) or bare Init
    if Param2 is TParamsExpr then
      CtorExpr := TParamsExpr(Param2).Value
    else
      CtorExpr := Param2;

    if not (CtorExpr is TPrimitiveExpr) then
    begin
      if RaiseOnError then
        RaiseMsg(20250212100001,nXExpectedButYFound,sXExpectedButYFound,
          ['constructor identifier','expression'],Param2);
      exit(cIncompatible);
    end;
    CtorName := TPrimitiveExpr(CtorExpr).Value;

    // Verify CtorName is a constructor of the pointed-to object type
    Found := False;
    if (SubTypeEl <> nil) and (SubTypeEl.ClassType = TPasClassType) then
      for i := 0 to TPasClassType(SubTypeEl).Members.Count - 1 do
        if TObject(TPasClassType(SubTypeEl).Members[i]) is TPasConstructor then
          if CompareText(TPasElement(TPasClassType(SubTypeEl).Members[i]).Name, CtorName) = 0 then
          begin
            Found := True;
            Break;
          end;
    if not Found then
    begin
      if RaiseOnError then
        RaiseMsg(20250212100002,nIdentifierNotFound,sIdentifierNotFound,
          [CtorName],Param2);
      exit(cIncompatible);
    end;

    // Resolve constructor params (they were skipped in ResolveParamsExprParams)
    if Param2 is TParamsExpr then
      for i := 0 to Length(TParamsExpr(Param2).Params) - 1 do
        ResolveExpr(TParamsExpr(Param2).Params[i], rraRead);
  end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_New_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
begin
  if Proc=nil then ;
  FinishCallArgAccess(Params.Params[0],rraOutParam);
  // Note: 2nd param (constructor) is NOT a normal resolved expression,
  // so we skip FinishCallArgAccess for it.
end;

function TPasResolver.BI_Dispose_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param, Param2, DtorExpr: TPasExpr;
  TypeEl, SubTypeEl: TPasType;
  ParamResolved: TPasResolverResult;
  DtorName: string;
  Found: Boolean;
  i: Integer;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: var PRecord
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.BI_Dispose_OnGetCallCompatibility ParamResolved=',GetResolverResultDbg(ParamResolved));
  {$ENDIF}
  Result:=cIncompatible;
  SubTypeEl:=nil;
  if (rrfReadable in ParamResolved.Flags) then
    if ParamResolved.BaseType=btContext then
      begin
      TypeEl:=ParamResolved.LoTypeEl;
      if TypeEl.ClassType=TPasPointerType then
        begin
        SubTypeEl:=ResolveAliasType(TPasPointerType(TypeEl).DestType);
        if SubTypeEl.ClassType=TPasRecordType then
          Result:=cExact
        else if (SubTypeEl.ClassType=TPasClassType)
            and (TPasClassType(SubTypeEl).ObjKind=okObject) then
          Result:=cExact;
        end;
      end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20180425010620,1,Param,ParamResolved,'pointer of record or object',RaiseOnError));

  // Handle 2nd param: destructor identifier (e.g., Dispose(pp, Done))
  if Length(Params.Params) >= 2 then
  begin
    Param2 := Params.Params[1];
    // Extract the destructor name from Done or Done()
    if Param2 is TParamsExpr then
      DtorExpr := TParamsExpr(Param2).Value
    else
      DtorExpr := Param2;

    if not (DtorExpr is TPrimitiveExpr) then
    begin
      if RaiseOnError then
        RaiseMsg(20250212100003,nXExpectedButYFound,sXExpectedButYFound,
          ['destructor identifier','expression'],Param2);
      exit(cIncompatible);
    end;
    DtorName := TPrimitiveExpr(DtorExpr).Value;

    // Verify DtorName is a destructor of the pointed-to object type
    Found := False;
    if (SubTypeEl <> nil) and (SubTypeEl.ClassType = TPasClassType) then
      for i := 0 to TPasClassType(SubTypeEl).Members.Count - 1 do
        if TObject(TPasClassType(SubTypeEl).Members[i]) is TPasDestructor then
          if CompareText(TPasElement(TPasClassType(SubTypeEl).Members[i]).Name, DtorName) = 0 then
          begin
            Found := True;
            Break;
          end;
    if not Found then
    begin
      if RaiseOnError then
        RaiseMsg(20250212100004,nIdentifierNotFound,sIdentifierNotFound,
          [DtorName],Param2);
      exit(cIncompatible);
    end;
  end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_Dispose_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
begin
  if Proc=nil then ;
  FinishCallArgAccess(Params.Params[0],rraRead);
  // Note: 2nd param (destructor) is NOT a normal resolved expression,
  // so we skip FinishCallArgAccess for it.
end;

function TPasResolver.MembersHoldFileType(aType: TPasType): boolean;
// True when aType is a record or TP object with a FILE-typed field (directly or
// through a nested one).
var
  Members: TFPList;
  i: Integer;
  El, FieldType: TPasElement;
  Res: TPasResolverResult;
begin
  Result:=false;
  aType:=ResolveAliasType(aType);
  if aType=nil then exit;
  if aType.ClassType=TPasRecordType then
    Members:=TPasRecordType(aType).Members
  else if (aType.ClassType=TPasClassType)
      and (TPasClassType(aType).ObjKind=okObject) then
    Members:=TPasClassType(aType).Members
  else
    exit;
  if Members=nil then exit;
  for i:=0 to Members.Count-1 do
    begin
    El:=TPasElement(Members[i]);
    if El.ClassType<>TPasVariable then continue;
    FieldType:=TPasVariable(El).VarType;
    if FieldType=nil then continue;
    ComputeElement(FieldType,Res,[rcType]);
    if Res.BaseType in [btText,btFile] then
      exit(true);
    if MembersHoldFileType(TPasType(FieldType)) then
      exit(true);
    end;
end;


function TPasResolver.BI_Default_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  Decl: TPasElement;
  aType: TPasType;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // check type or var
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  Decl:=ParamResolved.IdentEl;
  aType:=nil;
  if (Decl<>nil) and (ParamResolved.LoTypeEl<>nil) then
    begin
    if Decl is TPasType then
      aType:=TPasType(Decl)
    else if Decl is TPasVariable then
      aType:=TPasVariable(Decl).VarType
    else if Decl.ClassType=TPasArgument then
      aType:=TPasArgument(Decl).ArgType;
    {$IFDEF VerbosePasResolver}
    {AllowWriteln}
    if aType=nil then
      writeln('TPasResolver.BI_Default_OnGetCallCompatibility Decl=',GetObjName(Decl));
    {AllowWriteln-}
    {$ENDIF}
    end;
  if aType=nil then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.BI_Default_OnGetCallCompatibility ',GetResolverResultDbg(ParamResolved));
    {$ENDIF}
    RaiseMsg(20180501004009,nTypeIdentifierExpected,sTypeIdentifierExpected,[],Param);
    end;

  // An unspecialized generic cannot be used with Default() (tdefault11/12),
  // unless it only stands in for a template parameter of a partial
  // specialization (fcl-stl gvector's `Default(T)` under TVector<TTreeNode>).
  // Inside a generic, `Default(TPair<TKey,TValue>)` names a partial
  // specialization, which answers with the bare generic
  // (PartialSpecsAsGeneric). At top level the same text really is an
  // unspecialized generic and stays an error (tdefault11/12).
  if (aType is TPasGenericType)
      and (GetTypeParameterCount(TPasGenericType(aType))>0)
      and not InPartialSpecWithTypeArg(aType)
      and not (FPartialSpecsAsGeneric and InUnboundGeneric) then
    RaiseMsg(20260622100002,nXExpectedButYFound,sXExpectedButYFound,
      ['specialized type',GetTypeDescription(aType)],Param);

  // A record or TP object holding a FILE field has no default value outside
  // Delphi mode: ppcx64 rejects `Default(TRec)` there and IGNORES the field in
  // Delphi mode (tdefault5/6/7). Declaring such a field is perfectly legal in
  // both - fcl-res' yyinclude keeps a `Text` in its stack record - so the rule
  // belongs HERE, on the use, not on the declaration.
  if not (msDelphi in CurrentParser.CurrentModeswitches)
      and MembersHoldFileType(aType) then
    RaiseMsg(20250305020,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
      [IntToStr(1),'non-file type','TextFile'],Param);

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Default_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
var
  Param: TPasExpr;
begin
  if Proc=nil then ;
  Param:=Params.Params[0];
  ComputeElement(Param,ResolvedEl,[rcNoImplicitProc]);
  ResolvedEl.Flags:=[rrfReadable];
  ResolvedEl.IdentEl:=nil;
end;

procedure TPasResolver.BI_Default_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  TypeEl: TPasType;
  EnumType: TPasEnumType;
  ArrayEl: TPasArrayType;
  bt: TResolverBaseType;
begin
  if Proc=nil then ;
  Evaluated:=nil;
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  TypeEl:=ParamResolved.LoTypeEl;
  if (TypeEl is TPasSetType) or (ParamResolved.BaseType=btSet) then
    begin
    // any set, `set of Char` too (lazedit's Default(TCharSet)): the empty set,
    // the same value as a plain `[]`
    Evaluated:=TResEvalSet.CreateEmpty(revskNone);
    exit;
    end;
  if ParamResolved.BaseType=btContext then
    begin
    if TypeEl.ClassType=TPasArrayType then
      begin
      // An ARRAY has no compile-time constant form here: a dynamic one defaults
      // to nil and a static one to a zeroed aggregate, both of which the code
      // generator emits directly. Leave Evaluated nil, exactly as
      // Default(variant) below does, instead of failing the unit
      // (vcl-compat writes `Result := Default(TREStringDynArray)`).
      Evaluated:=nil;
      exit;
      ArrayEl:=TPasArrayType(TypeEl);
      if length(ArrayEl.Ranges)=0 then
        begin
        // dyn or open array
        end
      else
        begin
        // static array
        end;
      end
    else if TypeEl.ClassType=TPasEnumType then
      begin
      EnumType:=TPasEnumType(TypeEl);
      Evaluated:=TResEvalEnum.CreateValue(Integer(GetEnumMinOrdinal(EnumType)),TPasEnumValue(EnumType.Values[0]))
      end;
    end
  else if (TypeEl is TPasUnresolvedSymbolRef)
      and (TypeEl.CustomData is TResElDataBaseType) then
    begin
    // default(base type)
    bt:=TResElDataBaseType(TypeEl.CustomData).BaseType;
    bt:=GetActualBaseType(bt);
    if bt in btAllBooleans then
      Evaluated:=TResEvalBool.CreateValue(false)
    {$ifdef HasInt64}
    else if bt=btQWord then
      Evaluated:=TResEvalInt.CreateValue(0)
    {$endif}
    else if bt in btAllIntegerNoQWord then
      // the range of every integer base type contains 0
      Evaluated:=TResEvalInt.CreateValue(0)
    {$ifdef FPC_HAS_CPSTRING}
    else if bt in [btAnsiString,btShortString,btRawByteString] then
      // RawByteString belongs with the other ansi strings: its default is the
      // empty string too (fcl-pdf's `Default(RawBytestring)`).
      Evaluated:=TResEvalString.CreateValue('')
    {$endif}
    else if bt in [btUnicodeString,btWideString] then
      Evaluated:=TResEvalUTF16.CreateValue('')
    {$ifdef FPC_HAS_CPSTRING}
    else if bt=btAnsiChar then
      Evaluated:=TResEvalString.CreateValue(#0)
    {$endif}
    else if bt=btWideChar then
      Evaluated:=TResEvalUTF16.CreateValue(#0)
    else if bt in btAllFloats then
      Evaluated:=TResEvalFloat.CreateValue(0.0)
    else if bt=btVariant then
      // default(variant) is the empty/unassigned variant. It is not a
      // compile-time constant here, so leave Evaluated nil: the caller then
      // treats default(variant) as a run-time zero value (classes reader.inc:
      // Result := default(variant)).
      Evaluated:=nil
    else if bt=btPointer then
      Evaluated:=TResEvalValue.CreateKind(revkNil)
    else if bt in [btCurrency,btComp] then
      // a run-time zero value, the same as Default(Variant) above
      Evaluated:=nil
    else if bt in [btText,btFile] then
      // Default(TextFile) is refused, as by fpc (tdefault2)
      RaiseMsg(20260927140000,nXExpectedButYFound,sXExpectedButYFound,
        ['a type with a default value',GetTypeDescription(TypeEl)],Params)
    else
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.BI_Default_OnEval ',GetResolverResultDbg(ParamResolved));
      {$ENDIF}
      RaiseNotYetImplemented(20180501005645,Params);
      end;
    end
  else if ParamResolved.LoTypeEl is TPasRangeType then
    begin
    // e.g. type t = 2..10;
    Evaluated:=EvalRangeLimit(TPasRangeType(TypeEl).RangeExpr,FLags,true,Param);
    end
  else if ParamResolved.BaseType=btSet then
    begin
    if ParamResolved.SubType=btContext then
      begin
      if ParamResolved.LoTypeEl.ClassType=TPasEnumType then
        Evaluated:=TResEvalSet.CreateEmpty(revskEnum,TPasEnumType(ParamResolved.LoTypeEl))
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.BI_Default_OnEval ',GetResolverResultDbg(ParamResolved));
        {$ENDIF}
        RaiseNotYetImplemented(20180501125138,Param);
        end;
      end
    else
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.BI_Default_OnEval ',GetResolverResultDbg(ParamResolved));
      {$ENDIF}
      RaiseNotYetImplemented(20180501125014,Param);
      end;
    end
  else
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.BI_Default_OnEval ',GetResolverResultDbg(ParamResolved));
    {$ENDIF}
    RaiseNotYetImplemented(20180501004839,Param);
    end;
end;

function TPasResolver.BI_NameOf_GetIdentEl(Params: TParamsExpr;
  RaiseOnError: boolean): TPasElement;
// NameOf(identifier) - check that the parameter is a symbol reference and
// return its declaration. Returns nil on error, unless RaiseOnError.
var
  Param, SubExpr, ComputeExpr: TPasExpr;
  ParamResolved: TPasResolverResult;
  IsInlineSpec: boolean;
  SpecItem: TPRSpecializedItem;
begin
  Result:=nil;
  Param:=Params.Params[0];

  // only an identifier or a dotted identifier chain is allowed,
  // not an arbitrary expression
  ComputeExpr:=Param;
  SubExpr:=Param;
  IsInlineSpec:=false;
  repeat
    if (SubExpr.ClassType=TBinaryExpr)
        and (TBinaryExpr(SubExpr).OpCode=eopSubIdent) then
      SubExpr:=TBinaryExpr(SubExpr).Right
    else if SubExpr.ClassType=TInlineSpecializeExpr then
      begin
      // NameOf(TBird<boolean>) gives the name of the generic type: 'TBird'
      ComputeExpr:=TInlineSpecializeExpr(SubExpr).NameExpr;
      SubExpr:=ComputeExpr;
      IsInlineSpec:=true;
      end
    else
      break;
  until false;
  if not ((SubExpr.ClassType=TPrimitiveExpr) and (SubExpr.Kind=pekIdent)) then
    begin
    if RaiseOnError then
      RaiseMsg(20260908150001,nXExpectedButYFound,sXExpectedButYFound,
        ['identifier',ExprKindNames[SubExpr.Kind]],Param);
    exit;
    end;

  // do not turn a procedure reference into a call
  ComputeElement(ComputeExpr,ParamResolved,[rcNoImplicitProc]);
  Result:=ParamResolved.IdentEl;

  if IsInlineSpec and (Result<>nil) and (Result.CustomData is TPasGenericScope) then
    begin
    // an inline specialization resolves to the specialized element, whose name
    // is e.g. 'TBird<System.Boolean>' - use the name of the generic declaration
    SpecItem:=TPasGenericScope(Result.CustomData).SpecializedFromItem;
    if (SpecItem<>nil) and (SpecItem.GenericEl<>nil) then
      Result:=SpecItem.GenericEl;
    end;

  if (Result=nil) or (Result.Name='') then
    begin
    Result:=nil;
    if RaiseOnError then
      RaiseMsg(20260908150002,nXExpectedButYFound,sXExpectedButYFound,
        ['identifier',GetResolverResultDescription(ParamResolved)],Param);
    end;
end;

function TPasResolver.BI_NameOf_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);
  if BI_NameOf_GetIdentEl(Params,RaiseOnError)=nil then
    exit;
  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_NameOf_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,btString,Proc.Proc,
    FBaseTypes[btString],FBaseTypes[btString],[rrfReadable]);
  if Params=nil then ;
end;

procedure TPasResolver.BI_NameOf_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
var
  IdentEl: TPasElement;
begin
  Evaluated:=nil;
  IdentEl:=BI_NameOf_GetIdentEl(Params,true);
  if IdentEl=nil then
    exit;
  // the declared name, with the original case
  {$ifdef FPC_HAS_CPSTRING}
  Evaluated:=TResEvalString.CreateValue(IdentEl.Name);
  {$else}
  Evaluated:=TResEvalUTF16.CreateValue(IdentEl.Name);
  {$endif}
  if Proc=nil then ;
  if Flags=[] then ;
end;

function TPasResolver.BI_IsConstValue_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // check value
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags) then
    begin
    if RaiseOnError then
      RaiseMsg(20260911200001,nXExpectedButYFound,sXExpectedButYFound,
        ['value',GetResolverResultDescription(ParamResolved)],Param);
    exit;
    end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_IsConstValue_OnGetCallResult(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,btBoolean,Proc.Proc,
                     FBaseTypes[btBoolean],FBaseTypes[btBoolean],[rrfReadable]);
  if Params=nil then ;
end;

procedure TPasResolver.BI_IsConstValue_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
// IsConstValue(Value) is true if Value is a compile time constant.
// Like FPC, a typed constant is not a constant value.
var
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  Value: TResEvalValue;
  IsConstant: boolean;
begin
  Evaluated:=nil;
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  IsConstant:=false;
  if (ParamResolved.IdentEl is TPasConst)
      and (TPasConst(ParamResolved.IdentEl).VarType<>nil) then
    // typed const
  else
    begin
    // evaluate without refConst, so that a non constant gives nil, not an error
    Value:=Eval(Param,[],false);
    try
      IsConstant:=(Value<>nil) and (Value.Kind<>revkExternal);
    finally
      ReleaseEvalValue(Value);
    end;
    end;
  Evaluated:=TResEvalBool.CreateValue(IsConstant);
  if Proc=nil then ;
  if Flags=[] then ;
end;
