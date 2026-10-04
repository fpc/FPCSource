{%MainUnit pasresolver.pp}
{ **********************************************************************

    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - Type compatibility handling
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }

function TPasResolver.IsDynArrayElementExpr(El: TPasElement): boolean;
// True for aDynArray[i]: const protects the array reference, not its elements.
var
  Params: TParamsExpr;
  Res: TPasResolverResult;
begin
  Result:=false;
  if not (El is TParamsExpr) then exit;
  Params:=TParamsExpr(El);
  // look through a typecast, e.g. PAnsiChar(aDynArray[i])
  if (Params.Kind=pekFuncParams) and (length(Params.Params)=1)
      and (Params.Value<>nil) then
    begin
    ComputeElement(Params.Value,Res,[rcNoImplicitProc]);
    if (Res.IdentEl is TPasType) and not (rrfReadable in Res.Flags) then
      exit(IsDynArrayElementExpr(Params.Params[0]));
    end;
  if (Params.Kind<>pekArrayParams) or (Params.Value=nil) then exit;
  ComputeElement(Params.Value,Res,[rcNoImplicitProc]);
  Result:=IsDynArray(Res.LoTypeEl);
end;


function TPasResolver.IsVariableConst(El, PosEl: TPasElement;
  RaiseIfConst: boolean): boolean;
var
  CurEl: TPasElement;
  VarResolved: TPasResolverResult;
  Loop: TPasImplForLoop;
begin
  Result:=false;
  CurEl:=PosEl;
  while CurEl<>nil do
    begin
    if (CurEl.ClassType=TPasImplForLoop) then
      begin
      Loop:=TPasImplForLoop(CurEl);
      if (Loop.VariableName<>PosEl) then
        begin
        ComputeElement(Loop.VariableName,VarResolved,[rcNoImplicitProc]);
        if VarResolved.IdentEl=El then
          begin
          if RaiseIfConst then
            RaiseMsg(20180430100719,nIllegalAssignmentToForLoopVar,
              sIllegalAssignmentToForLoopVar,[El.Name],PosEl);
          exit(true);
          end;
        end;
      end;
    CurEl:=CurEl.Parent;
    end;
end;

function TPasResolver.ResolvedElCanBeVarParam(
  const ResolvedEl: TPasResolverResult; PosEl: TPasElement;
  RaiseIfConst: boolean): boolean;

  function NotLocked(El: TPasElement): boolean;
  begin
    Result:=not IsVariableConst(El,PosEl,RaiseIfConst);
  end;

var
  IdentEl: TPasElement;
begin
  Result:=false;
  if [rrfReadable,rrfWritable]*ResolvedEl.Flags<>[rrfReadable,rrfWritable] then
    exit;
  if ResolvedEl.IdentEl=nil then
    exit(true);

  IdentEl:=ResolvedEl.IdentEl;
  if IdentEl.ClassType=TPasVariable then
    exit(NotLocked(IdentEl));
  if (IdentEl.ClassType=TPasConst) then
    begin
    if TPasConst(IdentEl).IsConst then
      begin
      if RaiseIfConst then
        RaiseMsg(20180430100719,nCantAssignValuesToConstVariable,sCantAssignValuesToConstVariable,[],PosEl);
      exit(false);
      end;
    exit(NotLocked(IdentEl));
    end;
  if (IdentEl.ClassType=TPasArgument) then
    begin
    // A const dynamic-array argument only protects the array reference: its
    // elements stay writable, so aData[i] may be passed to an untyped var
    // parameter (vcl-compat's TJSONBool.ToBytes does Move(B^,aData[aOffset],L)).
    if (TPasArgument(IdentEl).Access in [argConst,argConstRef])
        and not IsDynArrayElementExpr(PosEl) then
      begin
      if RaiseIfConst then
        RaiseMsg(20180430100843,nCantAssignValuesToConstVariable,sCantAssignValuesToConstVariable,[],PosEl);
      exit(false);
      end;
    Result:=(TPasArgument(IdentEl).Access in [argDefault, argVar, argOut])
        or IsDynArrayElementExpr(PosEl);
    exit(Result and NotLocked(IdentEl));
    end;
  if IdentEl.ClassType=TPasResultElement then
    exit(NotLocked(IdentEl));
  if (proPropertyAsVarParam in Options)
      and (IdentEl.ClassType=TPasProperty) then
    exit(NotLocked(IdentEl));
end;

function TPasResolver.IsStringCharIndexLValue(
  const ResolvedEl: TPasResolverResult): boolean;
begin
  Result:=([rrfReadable,rrfAssignable]*ResolvedEl.Flags=[rrfReadable,rrfAssignable])
    and (ResolvedEl.BaseType in btAllChars)
    and (ResolvedEl.ExprEl is TParamsExpr)
    and (TParamsExpr(ResolvedEl.ExprEl).Kind=pekArrayParams)
    and ((ResolvedEl.IdentEl<>nil)
      // Indexing through a DEREFERENCE has no IdentEl (a deref result never
      // does), but `sp^[i]` is just as writable a character slot as `s[i]`.
      // lnfodwrf appends to a PShortstring with
      // `Move(bufp^, sp^[1 + length(sp^)], nmove)`.
      or ((TParamsExpr(ResolvedEl.ExprEl).Value is TUnaryExpr)
        and (TUnaryExpr(TParamsExpr(ResolvedEl.ExprEl).Value).OpCode=eopDeref)));
end;

function TPasResolver.ResolvedElIsClassOrRecordInstance(
  const ResolvedEl: TPasResolverResult): boolean;
var
  TypeEl: TPasType;
  C: TClass;
begin
  Result:=false;
  if ResolvedEl.BaseType<>btContext then exit;
  TypeEl:=ResolvedEl.LoTypeEl;
  if TypeEl=nil then exit;
  if TypeEl.ClassType=TPasClassType then
    begin
    if TPasClassType(TypeEl).ObjKind<>okClass then exit;
    end
  else if TypeEl.ClassType=TPasRecordType then
  else
    exit;
  if ResolvedEl.IdentEl<>nil then
    begin
    C:=ResolvedEl.IdentEl.ClassType;
    if C.InheritsFrom(TPasVariable)
        or (C=TPasArgument)
        or (C=TPasResultElement) then
      exit(true);
    end;
end;

function TPasResolver.GetResolver(El: TPasElement): TPasResolver;
var
  Module: TPasModule;
  Scope: TPasModuleScope;
begin
  Result:=nil;
  if El=nil then exit;
  Module:=El.GetModule;
  if Module=nil then exit;
  Scope:=Module.CustomData as TPasModuleScope;
  if Scope=nil then exit;
  Result:=Scope.Owner as TPasResolver;
end;

function TPasResolver.ElHasModeSwitch(El: TPasElement; ms: TModeSwitch
  ): boolean;
begin
  Result:=ms in GetElModeSwitches(El);
end;

function TPasResolver.GetElModeSwitches(El: TPasElement): TModeSwitches;
var
  C: TClass;
begin
  while El<>nil do
    begin
    if El.CustomData<>nil then
      begin
      C:=El.CustomData.ClassType;
      if C.InheritsFrom(TPasProcedureScope) then
        exit(TPasProcedureScope(El.CustomData).ModeSwitches)
      else if C.InheritsFrom(TPasSectionScope) then
        exit(TPasSectionScope(El.CustomData).ModeSwitches);
      end;
    El:=El.Parent;
    end;
  Result:=CurrentParser.CurrentModeswitches;
end;

function TPasResolver.ElHasBoolSwitch(El: TPasElement; bs: TBoolSwitch
  ): boolean;
begin
  Result:=bs in GetElBoolSwitches(El);
end;

function TPasResolver.GetElBoolSwitches(El: TPasElement): TBoolSwitches;
var
  C: TClass;
begin
  Result:=CurrentParser.Scanner.CurrentBoolSwitches;
  while El<>nil do
    begin
    if El.CustomData<>nil then
      begin
      C:=El.CustomData.ClassType;
      if C.InheritsFrom(TPasProcedureScope) then
        exit(TPasProcedureScope(El.CustomData).BoolSwitches)
      else if C.InheritsFrom(TPasSectionScope) then
        exit(TPasSectionScope(El.CustomData).BoolSwitches)
      else if C.InheritsFrom(TPasModuleScope) then
        exit(TPasModuleScope(El.CustomData).BoolSwitches);
      end;
    El:=El.Parent;
    end;
end;

function TPasResolver.GetProcTypeDescription(ProcType: TPasProcedureType;
  Flags: TPRProcTypeDescFlags): string;
var
  Args, Templates: TFPList;
  i: Integer;
  Arg: TPasArgument;
  ArgType: TPasType;
  Proc: TPasProcedure;
begin
  if ProcType=nil then exit('nil');
  Result:=ProcType.TypeName;
  if ProcType.IsReferenceTo then
    Result:=ProcTypeModifiers[ptmReferenceTo]+' '+Result;
  if ProcType.Parent is TPasProcedure then
    begin
    Proc:=TPasProcedure(ProcType.Parent);
    if (prptdUseName in Flags) then
      begin
      if prptdAddPaths in Flags then
        Result:=Result+' '+Proc.FullName
      else
        Result:=Result+' '+Proc.Name;
      end;
    Templates:=GetProcTemplateTypes(Proc);
    if Templates<>nil then
      Result:=Result+GetGenericParamCommas(Templates.Count);
    end;
  Args:=ProcType.Args;
  if Args.Count>0 then
    begin
    Result:=Result+'(';
    for i:=0 to Args.Count-1 do
      begin
      if i>0 then Result:=Result+';';
      Arg:=TPasArgument(Args[i]);
      if AccessNames[Arg.Access]<>'' then
        Result:=Result+AccessNames[Arg.Access];
      if Arg.ArgType=nil then
        Result:=Result+'untyped'
      else
        begin
        ArgType:=Arg.ArgType;
        if prptdResolveSimpleAlias in Flags then
          ArgType:=ResolveSimpleAliasType(ArgType);
        Result:=Result+GetTypeDescription(ArgType,prptdAddPaths in Flags);
        end;
      end;
    Result:=Result+')';
    end;
  if ProcType.IsOfObject then
    Result:=Result+' '+ProcTypeModifiers[ptmOfObject];
  if ProcType.IsNested then
    Result:=Result+' '+ProcTypeModifiers[ptmIsNested];
  if cCallingConventions[ProcType.CallingConvention]<>'' then
    Result:=Result+';'+cCallingConventions[ProcType.CallingConvention];
end;

function TPasResolver.GetResolverResultDescription(const T: TPasResolverResult;
  OnlyType: boolean): string;

  function GetSubTypeName: string;
  begin
    if (T.LoTypeEl<>nil) and (T.LoTypeEl.Name<>'') then
      Result:=T.LoTypeEl.Name
    else
      Result:=BaseTypeNames[T.SubType];
  end;

var
  ArrayEl: TPasArrayType;
begin
  case T.BaseType of
  btModule: exit(GetElementTypeName(T.IdentEl)+' '+T.IdentEl.Name);
  btNil: exit('nil');
  btRange:
    Result:='range of '+GetSubTypeName;
  btSet:
    Result:='set of '+GetSubTypeName;
  btArrayLit:
    Result:='array of '+GetSubTypeName;
  btArrayOrSet:
    Result:='set/array literal of '+GetSubTypeName;
  btContext:
    begin
    if T.LoTypeEl.ClassType=TPasClassOfType then
      Result:='class of '+TPasClassOfType(T.LoTypeEl).DestType.Name
    else if T.LoTypeEl.ClassType=TPasAliasType then
      Result:=TPasAliasType(T.LoTypeEl).DestType.Name
    else if T.LoTypeEl.ClassType=TPasTypeAliasType then
      Result:='type '+TPasAliasType(T.LoTypeEl).DestType.Name
    else if T.LoTypeEl.ClassType=TPasArrayType then
      begin
      ArrayEl:=TPasArrayType(T.LoTypeEl);
      if length(ArrayEl.Ranges)=0 then
        begin
        if ArrayEl.ElType=nil then
          Result:='array of const'
        else
          begin
          Result:='array of '+ArrayEl.ElType.Name;
          if IsOpenArray(ArrayEl) then
            Result:='open '+Result;
          end;
        end
      else
        Result:='static array[] of '+ArrayEl.ElType.Name;
      end
    else if T.LoTypeEl is TPasProcedureType then
      Result:=GetProcTypeDescription(TPasProcedureType(T.LoTypeEl),[])
    else if T.LoTypeEl.Name<>'' then
      Result:=T.LoTypeEl.Name
    else
      Result:=T.LoTypeEl.ElementTypeName;
    end;
  btCustom:
    Result:=T.LoTypeEl.Name;
  else
    Result:=BaseTypeNames[T.BaseType];
  end;
  if (not OnlyType) and (T.LoTypeEl<>T.IdentEl) and (T.IdentEl<>nil) then
    Result:=T.IdentEl.Name+':'+Result;
end;

function TPasResolver.GetTypeDescription(aType: TPasType; AddPath: boolean): string;

  function GetName: string;
  var
    s: String;
    Spec: TPasSpecializeType;
    P: TPasElement;
    i: Integer;
    GenScope: TPasGenericScope;
    Params: TPasTypeArray;
  begin
    Result:=aType.Name;
    if Result='' then
      begin
      if aType is TPasArrayType then
        begin
        if length(TPasArrayType(aType).Ranges)>0 then
          Result:='static array'
        else if TPasArrayType(aType).ElType=nil then
          Result:='array of const'
        else if IsOpenArray(aType) then
          Result:='open array'
        else
          Result:='dynamic array';
        end
      else if aType is TPasSpecializeType then
        begin
        Spec:=TPasSpecializeType(aType);
        if Spec.CustomData is TPasSpecializeTypeData then
          exit(GetTypeDescription(TPasSpecializeTypeData(Spec.CustomData).SpecializedType));
        Result:=GetTypeDescription(Spec.DestType,true)+'<';
        for i:=0 to Spec.Params.Count-1 do
          begin
          P:=TPasElement(Spec.Params[i]);
          if P is TPasType then
            Result:=Result+GetTypeDescription(TPasType(P));
          if i>0 then
            Result:=Result+',';
          end;
        Result:=Result+'>';
        end
      else
        Result:=GetElementTypeName(aType);
      end
    else if aType is TPasGenericType then
      begin
      i:=GetTypeParameterCount(TPasGenericType(aType));
      if i>0 then
        // generic, not specialized
        Result:=Result+GetGenericParamCommas(GetTypeParameterCount(TPasGenericType(aType)))
      else if aType.CustomData is TPasGenericScope then
        begin
        GenScope:=TPasGenericScope(aType.CustomData);
        if (GenScope.SpecializedFromItem<>nil) and IsValidIdent(aType.Name) then
          begin
          // specialized without params in name -> append params
          Params:=GenScope.SpecializedFromItem.Params;
          Result:=Result+'<';
          for i:=0 to length(Params)-1 do
            begin
            Result:=Result+GetTypeDescription(Params[i],AddPath);
            if i>0 then
              Result:=Result+',';
            end;
          Result:=Result+'>';
          end
        end;
      end;
    if AddPath then
      begin
      s:=aType.ParentPath;
      if (s<>'') and (s<>'.') then
        Result:=s+'.'+Result;
      end;
  end;

begin
  if aType=nil then exit('untyped');
  Result:=GetName;
  if (aType.ClassType=TPasUnresolvedSymbolRef) then
    begin
    if TPasUnresolvedSymbolRef(aType).CustomData is TResElDataBuiltInProc then
      Result:=Result+'()';
    exit;
    end;
end;

function TPasResolver.GetTypeDescription(const R: TPasResolverResult;
  AddPath: boolean): string;
var
  s: String;
begin
  Result:=GetTypeDescription(R.LoTypeEl,AddPath);
  if R.BaseType in [btSet,btArrayLit,btArrayOrSet] then
    Result:=BaseTypeNames[R.BaseType]+' of '+Result;
  if (R.LoTypeEl<>nil) and (R.IdentEl=R.LoTypeEl) then
    begin
    s:=GetElementTypeName(R.LoTypeEl);
    if s<>'' then
      Result:=s+' '+Result
    else
      Result:='type '+Result;
    end;
end;

function TPasResolver.GetBaseDescription(const R: TPasResolverResult;
  AddPath: boolean): string;
begin
  if R.BaseType=btContext then
    Result:=GetTypeDescription(R,AddPath)
  else if (R.BaseType=btPointer) and not IsBaseType(R.LoTypeEl,btPointer) then
    Result:='^'+GetTypeDescription(R,AddPath)
  else
    Result:=BaseTypeNames[R.BaseType];
end;

function TPasResolver.GetProcFirstImplEl(Proc: TPasProcedure): TPasImplElement;
var
  Scope: TPasProcedureScope;
  Body: TPasImplBlock;
begin
  Result:=nil;
  if Proc=nil then exit;
  if Proc.Body<>nil then
    Body:=Proc.Body.Body
  else
    Body:=nil;
  if Body=nil then
    begin
    if Proc.CustomData=nil then exit;
    Scope:=Proc.CustomData as TPasProcedureScope;
    Proc:=Scope.ImplProc;
    if Proc=nil then exit;
    if Proc.Body=nil then exit;
    Body:=Proc.Body.Body;
    if Body=nil then exit;
    end;
  if Body.Elements=nil then exit;
  if Body.Elements.Count=0 then exit;
  Result:=TPasImplElement(Body.Elements[0]);
end;

function TPasResolver.GetProcTemplateTypes(Proc: TPasProcedure): TFPList;
var
  NameParts: TProcedureNamePart;
begin
  if Proc.NameParts=nil then
    exit(nil);
  NameParts:=TProcedureNamePart(Proc.NameParts[Proc.NameParts.Count-1]);
  Result:=NameParts.Templates;
  if (Result<>nil) and (Result.Count=0) then
    exit(nil);
end;

function TPasResolver.GetProcName(Proc: TPasProcedure; WithTemplates: boolean
  ): string;
var
  NameParts: TProcedureNameParts;
  i, j: Integer;
  NamePart: TProcedureNamePart;
  TemplType: TPasGenericTemplateType;
  Templates: TFPList;
begin
  if Proc=nil then exit('(nil)');
  Result:=Proc.Name;
  if WithTemplates then
    begin
    NameParts:=Proc.NameParts;
    if NameParts=nil then exit;
    Result:='';
    for i:=0 to NameParts.Count-1 do
      begin
      NamePart:=TProcedureNamePart(NameParts[i]);
      if i>0 then
        Result:=Result+'.';
      Result:=Result+NamePart.Name;
      Templates:=NamePart.Templates;
      if (Templates<>nil) and (Templates.Count>0) then
        begin
        for j:=0 to Templates.Count-1 do
          begin
          TemplType:=TPasGenericTemplateType(NamePart.Templates[j]);
          if j=0 then
            Result:=Result+'<'
          else
            Result:=Result+',';
          Result:=Result+TemplType.Name;
          end;
        Result:=Result+'>';
        end;
      end;
    end;
end;

function TPasResolver.GetPasPropertyAncestor(El: TPasProperty;
  WithRedeclarations: boolean): TPasProperty;
begin
  Result:=nil;
  if El=nil then exit;
  if (not WithRedeclarations) and (El.VarType<>nil) then exit;
  if El.CustomData=nil then exit;
  Result:=TPasPropertyScope(El.CustomData).AncestorProp;
end;

function TPasResolver.GetPasPropertyType(El: TPasProperty): TPasType;
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.VarType<>nil then
      exit(El.VarType);
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.GetPasPropertyArgs(El: TPasProperty): TFPList;
begin
  while El<>nil do
    begin
    if El.VarType<>nil then
      exit(El.Args);
    El:=GetPasPropertyAncestor(El);
    end;
  Result:=nil;
end;

function TPasResolver.GetPasPropertyGetter(El: TPasProperty): TPasElement;
// search the member variable or getter function of a property
var
  DeclEl: TPasElement;
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.ReadAccessor<>nil then
      begin
      DeclEl:=GetAccessorDeclaration(El.ReadAccessor);
      Result:=DeclEl;
      exit;
      end;
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.ProcTypesHaveSameSignature(T1, T2: TPasType): boolean;
// Two procedure types declared apart but with the same signature: ppcx64 calls
// them equal (defcmp's proc_to_procvar_equal), so a property of one may read a
// field of the other (the LCL's deprecated TCSLVFileAddedEvent).
begin
  Result:=false;
  if (T1=nil) or (T2=nil) then exit;
  T1:=ResolveAliasType(T1);
  T2:=ResolveAliasType(T2);
  if not (T1 is TPasProcedureType) or not (T2 is TPasProcedureType) then exit;
  Result:=CheckProcTypeCompatibility(TPasProcedureType(T1),TPasProcedureType(T2),
    true,nil,false);
end;

function TPasResolver.GetRemainingRangesArray(ArrType: TPasArrayType
  ): TPasArrayType;
// `array[A, B, C] of T` without its first range: array[B, C] of T. Made once
// per array and owned by the resolver; it shares the ranges and element type.
var
  i: Integer;
begin
  if FRemainingRangeArrays=nil then
    FRemainingRangeArrays:=TFPList.Create;
  i:=0;
  while i<FRemainingRangeArrays.Count-1 do
    begin
    if FRemainingRangeArrays[i]=Pointer(ArrType) then
      exit(TPasArrayType(FRemainingRangeArrays[i+1]));
    inc(i,2);
    end;
  Result:=TPasArrayType(CreateOwnedElement(TPasArrayType,'',ArrType.Parent));
  Result.Ranges:=Copy(ArrType.Ranges,1,length(ArrType.Ranges)-1);
  Result.ElType:=ArrType.ElType;
  Result.PackMode:=ArrType.PackMode;
  FRemainingRangeArrays.Add(ArrType);
  FRemainingRangeArrays.Add(Result);
end;

function TPasResolver.TypeChainReaches(FromType, ToType: TPasType): boolean;
// True when FromType is ToType or one of the types it is declared from, through
// aliases and `type X` copies.
var
  Depth: Integer;
begin
  Result:=false;
  if ToType=nil then exit;
  Depth:=0;
  while (FromType<>nil) and (Depth<32) do
    begin
    if FromType=ToType then
      exit(true);
    // a built-in type such as String may be a different symbol object in a
    // unit read back from a saved file, with the same name and meaning
    if (FromType is TPasUnresolvedSymbolRef) and (ToType is TPasUnresolvedSymbolRef)
        and SameText(FromType.Name,ToType.Name) then
      exit(true);
    if FromType is TPasAliasType then
      FromType:=TPasAliasType(FromType).DestType
    else
      exit;
    inc(Depth);
    end;
end;

function TPasResolver.StringCodePageExprOf(T: TPasType): string;
// The code page a string type declares, as written (`CP_UTF8` for Utf8String),
// '' for a string without one.
begin
  Result:='';
  while T<>nil do
    begin
    if T is TPasStringType then
      exit(Trim(TPasStringType(T).CodePageExpr));
    if T is TPasAliasType then
      T:=TPasAliasType(T).DestType
    else
      exit;
    end;
end;

function TPasResolver.GetPasPropertyReadAccessor(El: TPasProperty): TPasExpr;
// The read accessor expression, also for a property re-published without one
// (the LCL's `property OKButton;` over `read FButtons[pbOK]`).
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.ReadAccessor<>nil then
      exit(El.ReadAccessor);
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.GetPasPropertyWriteAccessor(El: TPasProperty): TPasExpr;
// The write accessor expression, also for a re-published property.
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.WriteAccessor<>nil then
      exit(El.WriteAccessor);
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.IsFieldProperty(El: TPasProperty): boolean;
// True for a record property without index that reads a plain field by its
// bare name, so the property stands for that field's storage.
var
  Getter: TPasElement;
begin
  Result:=false;
  if (El=nil) or not (El.Parent is TPasRecordType) then exit;
  if (El.IndexExpr<>nil) or ((El.Args<>nil) and (El.Args.Count>0)) then exit;
  if not (El.ReadAccessor is TPrimitiveExpr) then exit;
  Getter:=GetPasPropertyGetter(El);
  Result:=(Getter is TPasVariable) and not (Getter is TPasProperty)
    and (Getter.Parent=El.Parent);
end;

function TPasResolver.GetAccessorDeclaration(Expr: TPasExpr): TPasElement;
// The element a property accessor names. A QUALIFIED accessor (`read Data.Key`,
// which real FPC accepts) has its reference on the rightmost part.
begin
  Result:=nil;
  // `read FButtons[pbOK]`: the array field
  if (Expr is TParamsExpr) and (TParamsExpr(Expr).Kind=pekArrayParams) then
    Expr:=TParamsExpr(Expr).Value;
  while (Expr is TBinaryExpr) and (TBinaryExpr(Expr).OpCode=eopSubIdent) do
    Expr:=TBinaryExpr(Expr).right;
  if (Expr<>nil) and (Expr.CustomData is TResolvedReference) then
    Result:=TResolvedReference(Expr.CustomData).Declaration;
end;

function TPasResolver.GetPasPropertySetter(El: TPasProperty): TPasElement;
// search the member variable or setter procedure of a property
var
  DeclEl: TPasElement;
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.WriteAccessor<>nil then
      begin
      DeclEl:=GetAccessorDeclaration(El.WriteAccessor);
      Result:=DeclEl;
      exit;
      end;
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.GetPropertyAccessorForParams(El: TPasProperty;
  Params: TParamsExpr; IsSetter: boolean): TPasElement;
// The accessor an indexed property access actually calls. FPC picks the
// overload matching the brackets at the access site, not the one matching the
// property's declared index parameters: versiontypes.pp declares GetValue for
// an integer and for a string behind two properties that share the name.
var
  Decl: TPasElement;
  Members: TFPList;
  Owner: TPasElement;
  ClassEl: TPasClassType;
  i, WantArgs: Integer;
  Cand: TPasProcedure;

  function ArgsMatch(aProc: TPasProcedure): boolean;
  var
    j: Integer;
  begin
    Result:=false;
    if aProc.ProcType.Args.Count<>WantArgs then exit;
    for j:=0 to length(Params.Params)-1 do
      if CheckParamCompatibility(Params.Params[j],
          TPasArgument(aProc.ProcType.Args[j]),j,false)=cIncompatible then
        exit;
    Result:=true;
  end;

begin
  if IsSetter then
    Result:=GetPasPropertySetter(El)
  else
    Result:=GetPasPropertyGetter(El);
  if not (Result is TPasProcedure) then exit;
  if (Params=nil) or (length(Params.Params)=0) then exit;
  if GetPasPropertyIndex(El)<>nil then exit;
  WantArgs:=length(Params.Params);
  if IsSetter then inc(WantArgs);
  Decl:=Result;
  if ArgsMatch(TPasProcedure(Decl)) then exit;
  // the declared accessor does not fit these brackets: look for a sibling
  Owner:=Decl.Parent;
  while Owner is TPasMembersType do
    begin
    Members:=TPasMembersType(Owner).Members;
    for i:=0 to Members.Count-1 do
      begin
      if not (TPasElement(Members[i]) is TPasProcedure) then continue;
      Cand:=TPasProcedure(Members[i]);
      if (Cand=Decl) or not SameText(Cand.Name,Decl.Name) then continue;
      if ArgsMatch(Cand) then
        exit(Cand);
      end;
    if not (Owner is TPasClassType) then break;
    ClassEl:=TPasClassType(Owner);
    Owner:=ResolveAliasType(GetPasClassAncestor(ClassEl,true));
    end;
end;

function TPasResolver.GetPropertySetterForValue(El: TPasProperty;
  ValueExpr: TPasExpr): TPasElement;
// The setter a plain property assignment calls. As with an indexed access, FPC
// picks the overload matching the assigned value: fpreportjson has one
// SetJSON taking a string and one taking a TJSONData behind one property.
var
  Decl: TPasElement;
  Members: TFPList;
  Owner: TPasElement;
  ClassEl: TPasClassType;
  i: Integer;
  Cand: TPasProcedure;

  function ValueFits(aProc: TPasProcedure): boolean;
  begin
    Result:=(aProc.ProcType.Args.Count=1)
      and (CheckParamCompatibility(ValueExpr,
             TPasArgument(aProc.ProcType.Args[0]),0,false)<>cIncompatible);
  end;

begin
  Result:=GetPasPropertySetter(El);
  if not (Result is TPasProcedure) then exit;
  if ValueExpr=nil then exit;
  if GetPasPropertyIndex(El)<>nil then exit;
  if GetPasPropertyArgs(El).Count>0 then exit;
  Decl:=Result;
  if ValueFits(TPasProcedure(Decl)) then exit;
  Owner:=Decl.Parent;
  while Owner is TPasMembersType do
    begin
    Members:=TPasMembersType(Owner).Members;
    for i:=0 to Members.Count-1 do
      begin
      if not (TPasElement(Members[i]) is TPasProcedure) then continue;
      Cand:=TPasProcedure(Members[i]);
      if (Cand=Decl) or not SameText(Cand.Name,Decl.Name) then continue;
      if ValueFits(Cand) then
        exit(Cand);
      end;
    if not (Owner is TPasClassType) then break;
    ClassEl:=TPasClassType(Owner);
    Owner:=ResolveAliasType(GetPasClassAncestor(ClassEl,true));
    end;
end;

function TPasResolver.GetPasPropertyIndex(El: TPasProperty): TPasExpr;
// search the index expression of a property
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.IndexExpr<>nil then
      begin
      Result:=El.IndexExpr;
      exit;
      end;
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.GetPasPropertyStoredExpr(El: TPasProperty): TPasExpr;
// search the stored expression of a property
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.StoredAccessor<>nil then
      begin
      Result:=El.StoredAccessor;
      exit;
      end;
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.GetPasPropertyDefaultExpr(El: TPasProperty): TPasExpr;
// search the stored expression of a property
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.DefaultExpr<>nil then
      begin
      Result:=El.DefaultExpr;
      exit;
      end
    else if El.IsNodefault then
      exit(nil);
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.CheckParamCompatibility(Expr: TPasExpr;
  Param: TPasArgument; ParamNo: integer; RaiseOnError: boolean;
  SetReferenceFlags: boolean): integer;
var
  ExprResolved, ParamResolved, ElTypeResolved: TPasResolverResult;
  NeedVar: Boolean;
  ElCompat: integer;
  PtDestRes, ExDestRes: TPasResolverResult;

  function ArraySliceFitsOpenArray: boolean;
  // arr[a..b] passed to an open-array parameter is an FPC array-slice: it
  // denotes a sub-array view over arr's storage (array, dynarray, pointer or
  // string base). Accept it when the slice's element type is compatible with
  // the open array's element type; codegen materialises the (data pointer,
  // high) pair. Works for const and var open-array params.
  var
    SliceParams: TParamsExpr;
    LastParam: TPasExpr;
    OpenArr: TPasType;
    BaseRes, ElRes, ParamElRes: TPasResolverResult;
    ElType: TPasType;
  begin
    Result:=false;
    if Param.ArgType=nil then exit;
    if not ((Expr is TParamsExpr) and (TParamsExpr(Expr).Kind=pekArrayParams)) then exit;
    SliceParams:=TParamsExpr(Expr);
    if length(SliceParams.Params)=0 then exit;
    LastParam:=SliceParams.Params[length(SliceParams.Params)-1];
    if not ((LastParam is TBinaryExpr) and (TBinaryExpr(LastParam).Kind=pekRange)) then exit;
    OpenArr:=ResolveAliasType(Param.ArgType);
    if not IsOpenArray(OpenArr) then exit;
    // element type expected by the open array parameter
    ElType:=GetArrayElType(TPasArrayType(OpenArr));
    if ElType=nil then exit;
    ComputeElement(ElType,ParamElRes,[rcType]);
    // element type produced by the slice = element type of the sliced base
    ComputeElement(SliceParams.Value,BaseRes,[]);
    if BaseRes.BaseType in btAllStrings then
      // string slice yields chars
      ComputeElement(FBaseTypes[BaseTypeChar],ElRes,[rcType])
    else if (BaseRes.LoTypeEl is TPasArrayType) then
      ComputeElement(GetArrayElType(TPasArrayType(BaseRes.LoTypeEl)),ElRes,[rcType])
    else if (BaseRes.LoTypeEl is TPasPointerType) then
      ComputeElement(TPasPointerType(BaseRes.LoTypeEl).DestType,ElRes,[rcType])
    else if BaseRes.BaseType=btPointer then
      exit  // untyped pointer slice: no element type to check
    else
      exit;
    // element-type compatibility (integers by family, chars by family, else same)
    if (ParamElRes.BaseType in btAllInteger) and (ElRes.BaseType in btAllInteger) then
      Result:=true
    else if (ParamElRes.BaseType in btAllChars) and (ElRes.BaseType in btAllChars) then
      Result:=true
    else if (ParamElRes.BaseType=ElRes.BaseType)
        and ((ParamElRes.BaseType<>btContext)
             or IsSameType(ParamElRes.LoTypeEl,ElRes.LoTypeEl,prraNone)) then
      Result:=true;
  end;

begin
  Result:=cIncompatible;

  ComputeArgumentAndExpr(Param,ParamResolved,Expr,ExprResolved,SetReferenceFlags);

  if ArraySliceFitsOpenArray then
    exit(cExact);

  // @OverloadedProc for a procedure-type parameter: another overload may fit
  // (lazarus' TAVLTree.Create(@CompareCTCSVariables)). While candidates are
  // only tried, just say so; the final pass over the chosen routine's arguments
  // (SetReferenceFlags) or its error check switches the reference.
  if not (Param.Access in [argVar, argOut])
      and (Expr is TUnaryExpr) and (TUnaryExpr(Expr).OpCode=eopAddress)
      and RefineProcAddrForTarget(ParamResolved,Expr,RaiseOnError or SetReferenceFlags) then
    begin
    if not (RaiseOnError or SetReferenceFlags) then
      exit(cExact);
    ComputeArgumentAndExpr(Param,ParamResolved,Expr,ExprResolved,SetReferenceFlags);
    end;

  NeedVar:=Param.Access in [argVar, argOut];
  if NeedVar then
    begin
    // Untyped var/out param accepting a pointer expression `@x`: FPC uses the
    // pointer value directly as the address (e.g. Move(@a,@b,n)). The address
    // expression is not itself a writable variable, so accept it here before the
    // general "must be a variable" check. Only for untyped params (ArgType=nil).
    if (Param.ArgType=nil) and (rrfReadable in ExprResolved.Flags)
        and (Expr is TUnaryExpr) and (TUnaryExpr(Expr).OpCode=eopAddress) then
      exit(cExact);
    // A parameterless function's own name, used inside its body and passed to this
    // var/out parameter (e.g. FpGetcwd in SetCodePage(FpGetcwd,...)), denotes the
    // Result variable, not a recursive call (lnfodwrf does
    // `ReadNext(ReadUHalf, sizeof(ReadUHalf))`).
    RedirectSelfNameToResult(Expr,ExprResolved,SetReferenceFlags);
    // Expr must be a variable. An untyped var/out additionally accepts a writable
    // string char-index l-value (s[i], the Stream.ReadBuffer(s[1],..) idiom):
    // ComputeArrayParams marks it rrfAssignable (not rrfWritable),
    // Only a real call may report the loop variable; an overload candidate
    // being tried must just not fit (TXMLConfig.GetValue(Path, i) has an
    // untyped `out` overload next to the Integer one).
    if not ResolvedElCanBeVarParam(ExprResolved,Expr,RaiseOnError)
        and not ((Param.ArgType=nil) and IsStringCharIndexLValue(ExprResolved)) then
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.CheckParamCompatibility NeedWritable: ',GetResolverResultDbg(ExprResolved));
      {$ENDIF}
      if RaiseOnError then
        begin
        if ExprResolved.IdentEl is TPasConst then
          RaiseMsg(20180430012609,nCantAssignValuesToConstVariable,sCantAssignValuesToConstVariable,[],Expr)
        else
          RaiseVarExpected(20180430012457,Expr,ExprResolved.IdentEl);
        end;
      exit;
      end;
    // a bit-packed ordinal field/element cannot be passed by reference
    if IsBitPackedOrdinalAccess(Expr) then
      begin
      if RaiseOnError then
        RaiseMsg(20260622120200,nCannotTakeAddrOfBitPackedElement,
          sCannotTakeAddrOfBitPackedElement,[],Expr);
      exit;
      end;
    if (Param.ArgType=nil) then
      // Untyped var/out parameter: a catch-all that accepts any writable l-value,
      // but ranked BELOW an exact typed match so an overload with a specific
      // parameter type wins the tie 
      exit(cCompatible); // untyped argument
    // WideString and UnicodeString are ONE storage type off Windows (there FPC
    // defines FPC_WIDESTRING_EQUAL_UNICODESTRING), so either satisfies the
    // other's var/out parameter — cwstring.pp hands a WideString to
    // DefaultAnsi2UnicodeMove's `var UnicodeString`. They stay DISTINCT
    // everywhere else: ppcx64 keeps separate type helpers for the two (tthlp3)
    // and lets them overload each other (tcpstr22), which is why this must not
    // be widened into GetActualBaseType.
    if WideStringMatchesUnicodeString(ParamResolved.BaseType,ExprResolved.BaseType) then
      exit(cExact);
    // Two AnsiStrings of DIFFERENT code pages are different types for a var
    // parameter, as in ppcx64: the LCL's `UTF8Delete(Value, ..)` with a TCaption
    // (`type String`) takes the `var s: String` overload, not the Utf8String one.
    if (GetActualBaseType(ParamResolved.BaseType)=btAnsiString)
        and (GetActualBaseType(ExprResolved.BaseType)=btAnsiString)
        and not SameText(StringCodePageExprOf(Param.ArgType),
                         StringCodePageExprOf(ExprResolved.HiTypeEl)) then
      begin
      if RaiseOnError then
        RaiseIncompatibleTypeRes(20260927130000,nIncompatibleTypeArgNoVarParamMustMatchExactly,
          [IntToStr(ParamNo+1)],ExprResolved,ParamResolved,Expr);
      exit(cIncompatible);
      end;
    if GetActualBaseType(ParamResolved.BaseType)=GetActualBaseType(ExprResolved.BaseType) then
      begin
      // Two `File`/`Text` types with matching element type are the same type for a
      // var/out param.
      if (ParamResolved.BaseType in [btFile,btText])
          and (ExprResolved.BaseType in [btFile,btText])
          and (ParamResolved.LoTypeEl is TPasFileType)
          and (ExprResolved.LoTypeEl is TPasFileType) then
        begin
        if (TPasFileType(ParamResolved.LoTypeEl).ElType=nil)
            and (TPasFileType(ExprResolved.LoTypeEl).ElType=nil) then
          exit(cExact)   // both untyped `File`/`Text`
        else if (TPasFileType(ParamResolved.LoTypeEl).ElType<>nil)
            and (TPasFileType(ExprResolved.LoTypeEl).ElType<>nil)
            and IsSameType(TPasFileType(ParamResolved.LoTypeEl).ElType,
                           TPasFileType(ExprResolved.LoTypeEl).ElType,prraNone) then
          exit(cExact);  // `file of T` with matching T
        end;
      if msDelphi in CurrentParser.CurrentModeswitches then
        begin
        // Delphi allows passing alias, but not type alias to a var arg
        if IsSameType(ParamResolved.HiTypeEl,ExprResolved.HiTypeEl,prraSimple) then
          exit(cExact);
        end
      else if IsSameType(ParamResolved.LoTypeEl,ExprResolved.LoTypeEl,prraNone) then
        begin
        // ObjFPC allows passing type alias to a var arg, but simple alias wins
        if IsSameType(ParamResolved.HiTypeEl,ExprResolved.HiTypeEl,prraSimple)
            {$IFNDEF PAS2JS}
            // Variant and OleVariant share btVariant but rank as distinct named
            // types for overload resolution: an exact-name match (Variant->Variant)
            // must beat the alias (Variant->OleVariant), so VarClear(Variant) wins
            // over VarClear(OleVariant) for a plain Variant arg (variants unit).
            and not ((ParamResolved.BaseType=btVariant)
                and (ParamResolved.HiTypeEl<>nil) and (ExprResolved.HiTypeEl<>nil)
                and (CompareText(ParamResolved.HiTypeEl.Name,ExprResolved.HiTypeEl.Name)<>0))
            {$ENDIF}
            then
          exit(cExact)
        // An argument whose declarations lead to the parameter's own type fits
        // better than one that only shares its base type: the LCL passes a
        // TCaption (TTranslateString = type String) to UTF8Delete, overloaded
        // for `var s: String` and `var s: Utf8String`.
        else if TypeChainReaches(ExprResolved.HiTypeEl,ParamResolved.HiTypeEl) then
          exit(cAliasExact)
        else
          exit(cAliasExact+1);
        end;
      if (ParamResolved.BaseType=btContext)
          and (ParamResolved.LoTypeEl.ClassType=TPasArrayType)
          and (ExprResolved.LoTypeEl.ClassType=TPasArrayType) then
        begin
        Result:=CheckAssignResCompatibility(ParamResolved,ExprResolved,Expr,false);
        if Result<>cIncompatible then exit;
        end;
      end;
    // Two named pointer types whose target is the SAME simple base type are the
    // same type for a var/out param, even though they are distinct TPasPointerType
    // instances -- e.g. passing a PChar (^Char) to a var PAnsiChar (^AnsiChar)
    // parameter. 
    if (ParamResolved.LoTypeEl is TPasPointerType)
        and (ExprResolved.LoTypeEl is TPasPointerType)
        and (TPasPointerType(ParamResolved.LoTypeEl).DestType<>nil)
        and (TPasPointerType(ExprResolved.LoTypeEl).DestType<>nil) then
      begin
      // ppcx64 (defcmp.pas, pointer -> pointer) calls two pointer types equal
      // when their targets are equal: gtk2's PGdkPixmap and PGdkBitmap both
      // point to an alias of TGdkDrawable.
      if ResolveAliasType(TPasPointerType(ParamResolved.LoTypeEl).DestType)
          =ResolveAliasType(TPasPointerType(ExprResolved.LoTypeEl).DestType) then
        exit(cAliasExact);
      ComputeElement(TPasPointerType(ParamResolved.LoTypeEl).DestType,PtDestRes,[]);
      ComputeElement(TPasPointerType(ExprResolved.LoTypeEl).DestType,ExDestRes,[]);
      if (GetActualBaseType(PtDestRes.BaseType)=GetActualBaseType(ExDestRes.BaseType))
          and (GetActualBaseType(PtDestRes.BaseType) in
               (btAllChars+btAllInteger+btAllFloats+btAllBooleans)) then
        exit(cAliasExact);
      end;
    // A var/out argument whose type is a generic type parameter cannot be
    // exact-matched against the parameter type yet: the concrete type is only
    // known after specialization, so defer the check (e.g. Val(s, ResultOfT,
    // code) inside a generic). Symmetric with the parameter-side template case.
    if IsGenericTemplType(ParamResolved) or IsGenericTemplType(ExprResolved) then
      exit(cGenericExact);

    // A formal untyped `Pointer` var/out parameter accepts ANY typed pointer.
    if (ParamResolved.BaseType=btPointer)
        and (Param.Access in [argOut, argVar])
        and ((ExprResolved.BaseType=btPointer)
            or ((ExprResolved.BaseType=btContext)
                and (ExprResolved.LoTypeEl is TPasPointerType))) then
      exit(cCompatible);

    // ...and the MIRROR: an untyped `Pointer` VARIABLE is accepted for a var/out
    // parameter of a typed pointer type. ppcx64 takes it and writes back through
    // it - fv's histlist.inc calls `DecodeSizeUInt(P2)` where the parameter is
    // `var P: PByte` and P2 is a plain Pointer.
    if (Param.Access in [argOut, argVar])
        and (ParamResolved.BaseType=btContext)
        and (ParamResolved.LoTypeEl is TPasPointerType)
        and (ExprResolved.BaseType=btPointer)
        and IsBaseType(ExprResolved.LoTypeEl,btPointer) then
      exit(cCompatible);

    // A call through a PROCEDURAL VARIABLE is laxer about var/out exactness
    // than a direct call: ppcx64 accepts an ordinal of the SAME SIZE there (a
    // Cardinal for a `var Int32`) and rejects every other size. pastojs'
    // library callback declares `Var AFileDataLen: Int32` and is handed a
    // Cardinal.
    if (Param.Access in [argOut, argVar])
        and (Param.Parent is TPasProcedureType)
        and not (Param.Parent.Parent is TPasProcedure)
        and (ParamResolved.BaseType in btAllInteger)
        and (ExprResolved.BaseType in btAllInteger)
        and (SameOrdinalWidth(ParamResolved.BaseType,ExprResolved.BaseType)) then
      exit(cCompatible);

    {$ifdef FPC_HAS_CPSTRING}
    // An RTL routine declared `rtlproc` (or `compilerproc`) takes ANY
    // ansistring-family variable for a RawByteString var/out parameter. ppcx64
    // has exactly this exemption - defcmp's cpo_rtlproc/cpo_compilerproc skip
    // the encoding comparison when one side is CP_NONE - and it is why
    // sysutils' DoDirSeparators(var FileName: RawByteString) accepts a plain
    // `string` (pastojs' pas2jsfiler). Ordinary routines still must match.
    if (Param.Access in [argOut, argVar])
        and (GetActualBaseType(ParamResolved.BaseType)=btRawByteString)
        and (GetActualBaseType(ExprResolved.BaseType) in
             [btAnsiString,btRawByteString])
        and (Param.Parent is TPasProcedureType)
        and (Param.Parent.Parent is TPasProcedure)
        and (TPasProcedure(Param.Parent.Parent).Modifiers*[pmRtlProc,pmCompilerProc]<>[]) then
      exit(cCompatible);
    {$endif}

    // ppcx64 (defcmp.pas) calls two integer types EQUAL when they are stored
    // the same way and the argument's range lies inside the parameter's: the
    // LCL passes a `TConstraintSize = 0..MaxInt` field to `var Value: Integer`.
    if (Param.Access in [argOut,argVar])
        and IntegerRangeFitsSameStorage(ExprResolved,ParamResolved) then
      exit(cCompatible);

    //writeln('TPasResolver.CheckParamCompatibility NeedVar ParamResolved=',GetResolverResultDbg(ParamResolved),' ExprResolved=',GetResolverResultDbg(ExprResolved));
    // Either side still PARTIALLY specialized: judged at specialization.
    if IsPartiallySpecializedType(ParamResolved.LoTypeEl)
        and IsPartiallySpecializedType(ExprResolved.LoTypeEl) then
      exit(cPartialSpecDefer);
    if RaiseOnError then
      RaiseIncompatibleTypeRes(20170216152452,nIncompatibleTypeArgNoVarParamMustMatchExactly,
        [IntToStr(ParamNo+1)],ExprResolved,ParamResolved,
        Expr);
    exit(cIncompatible);
    end;

  // A SINGLE value where an OPEN ARRAY is declared becomes a one-element array:
  // ppcx64 accepts `Show(B, 1)` for `Show(const D: array of Byte; ...)` and
  // reports Length(D)=1 (paszlib's inflate_set_dictionary(.., dictionary^, ..)).
  // Ranked as a conversion so a real array argument always wins.
  // `array of const` (ElType=nil) is excluded: FPC demands the bracket form
  // there, and computing its element type raises "not yet implemented: nil".
  if (ParamResolved.BaseType=btContext)
      and (ParamResolved.LoTypeEl is TPasArrayType)
      and IsOpenArray(ParamResolved.LoTypeEl)
      and (TPasArrayType(ParamResolved.LoTypeEl).ElType<>nil)
      and (rrfReadable in ExprResolved.Flags)
      and not (ExprResolved.BaseType in [btArrayLit,btArrayOrSet])
      and not ((ExprResolved.BaseType=btContext)
               and (ExprResolved.LoTypeEl is TPasArrayType)) then
    begin
    ComputeElement(TPasArrayType(ParamResolved.LoTypeEl).ElType,ElTypeResolved,
                   [rcType]);
    ElCompat:=CheckAssignResCompatibility(ElTypeResolved,ExprResolved,Expr,false);
    if ElCompat<cIncompatible then
      begin
      // Rank by how well the value fits the ELEMENT, or two open-array
      // overloads differing only in element type score the same and the call is
      // ambiguous. An EXACT element is priced just BELOW cTypeConversion,
      // because the candidate it competes with is a real conversion of the same
      // value - a string reaches `array of AnsiChar` at exactly
      // cTypeConversion - and FPC picks the exact element:
      // `S.Split(SepString, Opts)` takes `array of AnsiString`, while a CHAR
      // argument takes `array of AnsiChar` (fpmkunit's TStringHelper.Split,
      // 16 corpus units). A real array argument still wins: it reaches cExact
      // through the ordinary path below.
      if ElCompat=cExact then
        exit(cTypeConversion-1);
      exit(cTypeConversion+ElCompat);
      end;
    end;
  if Param.ArgType=nil then
    begin
    // Untyped CONST/value parameter: a catch-all, ranked BELOW a typed match so
    // an overload with a specific parameter type wins the tie - exactly as the
    // untyped var/out case above. ppcx64 picks
    // `FpWrite(fd; buf: PAnsiChar; nbytes)` over
    // `FpWrite(fd; const buf; nbytes)` for an array of AnsiChar; without this
    // the two ranked equally and the call was "Can't determine which overloaded
    // function to call" (rtl-console's video.pp).
    if rrfReadable in ExprResolved.Flags then
      exit(cUntypedParam);
    // A value whose type is left for the specialization, inside a generic not
    // yet specialized (fpdebug's `WriteData(.., _BREAK._CODE)`, _BREAK a type
    // parameter): checked for real when the generic is specialized.
    if FPartialSpecsAsGeneric and InUnboundGeneric
        and ((ExprResolved.BaseType=btNone)
          or ((ExprResolved.LoTypeEl<>nil) and ParamIsUndecided(ExprResolved.LoTypeEl))) then
      exit(cUntypedParam);
    if RaiseOnError then
      RaiseMsg(20260927130000,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        [IntToStr(ParamNo+1),GetResolverResultDescription(ExprResolved),'untyped'],Expr);
    exit(cIncompatible);
    end;
  Result:=CheckParamResCompatibility(Expr,ExprResolved,ParamResolved,ParamNo,
                                     RaiseOnError,SetReferenceFlags);
  (* An address-of expression (@x) yields an untyped pointer in the default {$T-}  mode; 
     freely compatible with any pointer-typed value  parameter — not a cTypeConversion-priced conversion. *)
  if (Result>=cTypeConversion) and (Result<cIncompatible)
      and (Expr is TUnaryExpr) and (TUnaryExpr(Expr).OpCode=eopAddress)
      and ((ParamResolved.BaseType=btPointer)
           or ((ParamResolved.BaseType=btContext)
               and (ParamResolved.LoTypeEl is TPasPointerType))) then
    Result:=cCompatible;
end;

function TPasResolver.IsUntypedPointerDeref(Expr: TPasExpr): boolean;
// True for `SomeUntypedPointer^` - the shape that is only ever an untyped
// argument. A TYPED pointer deref (PByte^ -> Byte) is not this.
var
  OperandResolved: TPasResolverResult;
begin
  Result:=false;
  if not (Expr is TUnaryExpr) then exit;
  if TUnaryExpr(Expr).OpCode<>eopDeref then exit;
  if TUnaryExpr(Expr).Operand=nil then exit;
  ComputeElement(TUnaryExpr(Expr).Operand,OperandResolved,[rcNoImplicitProc]);
  Result:=OperandResolved.BaseType=btPointer;
end;


function TPasResolver.CheckParamResCompatibility(Expr: TPasExpr;
  const ExprResolved, ParamResolved: TPasResolverResult; ParamNo: integer;
  RaiseOnError: boolean; SetReferenceFlags: boolean): integer;
var
  UseAssignError: Boolean;
begin
  UseAssignError:=false;
  if RaiseOnError and (ExprResolved.BaseType in [btArrayLit,btArrayOrSet]) then
    // e.g. Call([1,2]) -> on mismatch jump to the wrong param expression
    UseAssignError:=true;

  { Dereferencing an UNTYPED Pointer yields an UNTYPED value: `Pointer(S)^` is
    only ever an untyped argument. It resolves back to Pointer (so that
    `Pointer(Dest^) := x` stays a valid l-value), which made it look compatible
    with a TYPED pointer parameter - `FNV1_32a(Pointer(aData)^, ..)` then chose
    the `const Buf: PByte` overload over the `const Buf` one and passed the
    string's DATA where its ADDRESS belonged. }
  if IsUntypedPointerDeref(Expr)
      and (ParamResolved.BaseType=btContext)
      and (ParamResolved.LoTypeEl is TPasPointerType) then
    exit(cIncompatible);

  Result:=CheckAssignResCompatibility(ParamResolved,ExprResolved,Expr,UseAssignError);
  { A Boolean argument is NOT compatible with an enum parameter - fpc says
    "Incompatible type for arg no. N: Got Boolean, expected TEndian". This was
    once relaxed for two-value enums to make syshelp.inc's
    TGuidHelper.Create(..., DataEndian: TEndian) compile, but that call really
    resolves to TGUID's OWN Create(..., aBigEndian: Boolean) overload; accepting
    the Boolean here made the helper call ITSELF and recurse until the stack ran
    out (vcl-compat's TestDigestAsStringGUID). }
  // Either side still PARTIALLY specialized: judged at specialization
  // (rtl-generics passes one nested PNode where another is declared).
  if (Result=cIncompatible)
      and IsPartiallySpecializedType(ParamResolved.LoTypeEl)
      and IsPartiallySpecializedType(ExprResolved.LoTypeEl) then
    exit(cPartialSpecDefer);
  if (Result=cIncompatible) and RaiseOnError then
    RaiseIncompatibleTypeRes(20170216152454,nIncompatibleTypeArgNo,
      [IntToStr(ParamNo+1)],ExprResolved,ParamResolved,Expr);

  if SetReferenceFlags and (ParamResolved.BaseType=btContext)
      and (ParamResolved.LoTypeEl.ClassType=TPasArrayType) then
    MarkArrayExprRecursive(Expr,TPasArrayType(ParamResolved.LoTypeEl));
end;

function TPasResolver.IsInsideSpecialization: boolean;
// True while resolving inside the body of a generic SPECIALIZATION.
var
  i: Integer;
  ScopeEl: TPasElement;
  Scope: TPasGenericScope;
begin
  Result:=false;
  for i:=ScopeCount-1 downto 0 do
    begin
    ScopeEl:=Scopes[i].Element;
    while ScopeEl<>nil do
      begin
      if ScopeEl.CustomData is TPasGenericScope then
        begin
        Scope:=TPasGenericScope(ScopeEl.CustomData);
        if Scope.SpecializedFromItem<>nil then
          exit(true);
        end;
      ScopeEl:=ScopeEl.Parent;
      end;
    end;
end;

function TPasResolver.SpecializedSourceModule(El: TPasElement): TPasModule;
// The module a member's declaration really comes from. A SPECIALIZATION is
// created in the module that instantiates it, but its members are the GENERIC's,
// so `private` visibility has to be judged against the generic's module -
// rtl-generics specializes classes across its two units and reads their private
// nested types.
var
  Cur: TPasElement;
  Scope: TPasGenericScope;
begin
  Result:=nil;
  if El=nil then exit;
  Cur:=El;
  while Cur<>nil do
    begin
    if Cur.CustomData is TPasGenericScope then
      begin
      Scope:=TPasGenericScope(Cur.CustomData);
      if (Scope.SpecializedFromItem<>nil)
          and (Scope.SpecializedFromItem.GenericEl<>nil) then
        exit(Scope.SpecializedFromItem.GenericEl.GetModule);
      end;
    Cur:=Cur.Parent;
    end;
  Result:=El.GetModule;
end;

function TPasResolver.InUnboundGeneric: boolean;
// True when the code being resolved belongs to a generic that has not been
// given its type arguments - a generic's own declarations and method bodies.
// A reference to another generic is undecided there, and is checked for real on
// the specialized copy.
var
  i: Integer;
  Scope: TPasScope;
  El: TPasElement;
begin
  Result:=false;
  for i:=ScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    if not (Scope is TPasGenericScope) then continue;
    if TPasGenericScope(Scope).SpecializedFromItem<>nil then continue;
    El:=Scope.Element;
    if (El is TPasProcedure) and (GetProcTemplateTypes(TPasProcedure(El))<>nil) then
      exit(true);
    // A method body has its own scope on the stack, not its class's: the class
    // is reached through the procedure scope.
    if (Scope is TPasProcedureScope)
        and (TPasProcedureScope(Scope).ClassRecScope<>nil) then
      begin
      if TPasProcedureScope(Scope).ClassRecScope.SpecializedFromItem=nil then
        El:=TPasProcedureScope(Scope).ClassRecScope.Element;
      end;
    if (El is TPasGenericType)
        and (GetTypeParameterCount(TPasGenericType(El))>0) then
      exit(true);
    end;
end;

function TPasResolver.UndecidedPair(A, B: TPasType): boolean;
// True when either side still depends on a type parameter, so comparing the two
// decides nothing. Only with PartialSpecsAsGeneric: a reference like
// `TPair<TREE_CONSTRAINTS>` then answers with the bare generic, so a member of
// it has the GENERIC's parameter while the user of it has the enclosing
// type's. Both are settled - and the same check runs again - when the enclosing
// generic is specialized with real arguments.
begin
  Result:=FPartialSpecsAsGeneric
      and (ParamIsUndecided(A) or ParamIsUndecided(B));
end;

function TPasResolver.TemplateIsInScope(TemplType: TPasGenericTemplateType;
  ErrorEl: TPasElement): boolean;
var
  Data: TPRFindData;
  Abort: boolean;
begin
  Data:=Default(TPRFindData);
  Data.ErrorPosEl:=ErrorEl;
  Abort:=false;
  IterateElements(TemplType.Name,@OnFindFirst,@Data,Abort);
  Result:=Data.Found=TemplType;
end;

function TPasResolver.ParamIsUndecided(El: TPasElement): boolean;
// Whether a type still depends on a type parameter: it IS one, it is declared
// inside a generic that has not been given its arguments, or it is a
// specialization one of whose arguments is itself undecided.
// `TEnumerable<T>.PT` and the same `PT` taken from the generic are both
// undecided, and must be judged alike - a body that assigns one to the other is
// correct, and is checked for real when the specialization is built.

var
  Visited: TFPList;

  function IsUndecided(Cur: TPasElement; Depth: Integer): boolean;
  var
    Scope: TPasGenericScope;
    SpecItem: TPRSpecializedItem;
    i: Integer;
  begin
    Result:=false;
    if Depth>10 then exit;
    while Cur<>nil do
      begin
      if Cur is TPasGenericTemplateType then
        exit(true);
      // an element already looked at answered false: nested specializations
      // share their arguments, and without this the search is exponential
      if Visited.IndexOf(Cur)>=0 then
        exit;
      Visited.Add(Cur);
      if Cur.CustomData is TPasGenericScope then
        begin
        Scope:=TPasGenericScope(Cur.CustomData);
        SpecItem:=Scope.SpecializedFromItem;
        if SpecItem<>nil then
          begin
          for i:=0 to length(SpecItem.Params)-1 do
            if IsUndecided(TPasElement(SpecItem.Params[i]),Depth+1) then
              exit(true);
          end
        else if (Cur is TPasGenericType)
            and (GetTypeParameterCount(TPasGenericType(Cur))>0) then
          exit(true);
        end;
      Cur:=Cur.Parent;
      end;
  end;

begin
  Visited:=TFPList.Create;
  try
    Result:=IsUndecided(El,0);
  finally
    Visited.Free;
  end;
end;

function TPasResolver.IsPartiallySpecializedType(El: TPasType): boolean;
// True when El is, or is declared inside, a generic specialization whose type
// arguments are not decided yet: nothing about it is settled, so it is judged
// when the specialization is built rather than here.
var
  Cur: TPasElement;
  Scope: TPasGenericScope;
  SpecItem: TPRSpecializedItem;
  i: Integer;
begin
  Result:=false;
  Cur:=El;
  while Cur<>nil do
    begin
    if Cur.CustomData is TPasGenericScope then
      begin
      Scope:=TPasGenericScope(Cur.CustomData);
      SpecItem:=Scope.SpecializedFromItem;
      if SpecItem<>nil then
        for i:=0 to length(SpecItem.Params)-1 do
          if ParamIsUndecided(TPasElement(SpecItem.Params[i])) then
            exit(true);
      end;
    Cur:=Cur.Parent;
    end;
end;

function TPasResolver.CheckAssignCompatibilityUserType(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
var
  RTypeEl, LTypeEl: TPasType;
  SrcResolved, DstResolved: TPasResolverResult;
  LArray, RArray: TPasArrayType;
  GotDesc, ExpDesc: String;
  CurTVarRec: TPasRecordType;
  LeftClass, RightClass: TPasClassType;

  function RaiseIncompatType(Id: TMaxPrecInt): integer;
  begin
    // Either side still PARTIALLY specialized: judged at specialization.
    if IsPartiallySpecializedType(LHS.LoTypeEl)
        and IsPartiallySpecializedType(RHS.LoTypeEl) then
      exit(cPartialSpecDefer);
    Result:=cIncompatible;
    if not RaiseOnIncompatible then exit;
    RaiseIncompatibleTypeRes(Id,nIncompatibleTypesGotExpected,
      [],RHS,LHS,ErrorEl);
  end;

begin
  if (RHS.LoTypeEl=nil) then
    RaiseInternalError(20160922163645);
  if (LHS.LoTypeEl=nil) then
    RaiseInternalError(20160922163648);
  LTypeEl:=LHS.LoTypeEl;
  RTypeEl:=RHS.LoTypeEl;
  // Note: do not check if LHS is writable, because this method is used for 'const' too.
  if (LTypeEl=RTypeEl) and (rrfReadable in RHS.Flags) then
    exit(cExact);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckAssignCompatibilityUserType LTypeEl=',GetObjName(LTypeEl),' RTypeEl=',GetObjName(RTypeEl));
  {$ENDIF}
  Result:=-1;
  if LTypeEl.ClassType=TPasClassType then
    begin
    if RHS.BaseType=btNil then
      begin
      // an old-style object is a value, not a reference: nil is no object
      // (fpdebug's Create(CU, nil) over Create(.., Pointer) and Create(.., TDwarfScopeInfo))
      if TPasClassType(LTypeEl).ObjKind=okObject then
        exit(RaiseIncompatType(20260927120000));
      Result:=cExact;
      end
    else if RTypeEl.ClassType=TPasClassType then
      begin
      Result:=cIncompatible;
      if not (rrfReadable in RHS.Flags) then
        exit(RaiseIncompatType(20190215112914));
      LeftClass:=TPasClassType(LTypeEl);
      RightClass:=TPasClassType(RTypeEl);
      if LeftClass.ObjKind=RightClass.ObjKind then
        Result:=CheckSrcIsADstType(RHS,LHS)
      else if LeftClass.ObjKind=okInterface then
        begin
        if (RightClass.ObjKind=okClass)
            and (not RightClass.IsExternal) then
          begin
          // IntfVar:=ClassInstVar
          if GetClassImplementsIntf(RightClass,LeftClass)<>nil then
            exit(cTypeConversion);
          end;
        end;
      if Result=cIncompatible then
        Result:=CheckAssignCompatibilityClasses(LeftClass,RightClass);
      // Either side still PARTIALLY specialized: judged at specialization.
      if (Result=cIncompatible)
          and IsPartiallySpecializedType(LeftClass)
          and IsPartiallySpecializedType(RightClass) then
        exit(cPartialSpecDefer);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseIncompatibleType(20170216152458,nIncompatibleTypesGotExpected,
          [],RTypeEl,LTypeEl,ErrorEl);
      end
    else
      exit(RaiseIncompatType(20190215112919));
    end
  else if LTypeEl.ClassType=TPasClassOfType then
    begin
    if RHS.BaseType=btNil then
      Result:=cExact
    else if (RTypeEl.ClassType=TPasClassOfType) then
      begin
      if RHS.IdentEl is TPasType then
        begin
        Result:=cIncompatible;
        if RaiseOnIncompatible then
          begin
          if ResolveAliasType(TPasType(RHS.IdentEl)) is TPasClassOfType then
            RaiseMsg(20180317103206,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
              ['type class-of','class of '+TPasClassOfType(LTypeEl).DestType.Name],ErrorEl)
          else
            RaiseMsg(20180511123859,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
              [GetResolverResultDescription(RHS),'class of '+TPasClassOfType(LTypeEl).DestType.Name],ErrorEl)
          end;
        end
      else
        begin
        // e.g. ImageClass:=AnotherImageClass;
        Result:=CheckClassIsClass(TPasClassOfType(RTypeEl).DestType,
          TPasClassOfType(LTypeEl).DestType);
        if (Result=cIncompatible) and RaiseOnIncompatible then
          RaiseMsg(20170216152500,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
            ['class of '+TPasClassOfType(RTypeEl).DestType.PathName,'class of '+TPasClassOfType(LTypeEl).DestType.PathName],ErrorEl);
        end;
      end
    else if (RHS.IdentEl is TPasType)
        and (ResolveAliasType(TPasType(RHS.IdentEl)).ClassType=TPasClassType) then
      begin
      // e.g. ImageClass:=TFPMemoryImage;
      Result:=CheckClassIsClass(RTypeEl,TPasClassOfType(LTypeEl).DestType);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseMsg(20170216152501,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [RTypeEl.Name,'class of '+TPasClassOfType(LTypeEl).DestType.PathName],ErrorEl);
      // do not check rrfReadable -> exit
      exit;
      end;
    end
  else if LTypeEl is TPasProcedureType then
    begin
    if RHS.BaseType=btNil then
      exit(cExact);
    // Delphi's `@` yields an untyped address, so `ProcVar:=@Routine` does not
    // have its signature checked. ppcx64 accepts
    // generics.hashes' `mORMotHasher:=@xxHash32` on exactly this basis (the
    // routine's len is Integer, the field's is Cardinal) and REJECTS the very
    // same assignment written WITHOUT the `@`, or written in objfpc mode. {$T}
    // does NOT change this - ppcx64 accepts it under {$T+} too, so it is not
    // tested here. Asked of the ELEMENT: the parser's CURRENT mode is wherever
    // it happens to be, not where this expression was written.
    if (ErrorEl is TUnaryExpr)
        and (TUnaryExpr(ErrorEl).OpCode=eopAddress)
        and (RTypeEl is TPasProcedureType)
        and (msDelphi in GetElModeSwitches(ErrorEl)) then
      exit(cExact);
    //writeln('TPasResolver.CheckAssignCompatibilityUserType LTypeEl=',GetObjName(LTypeEl),' RHS.BaseType=',BaseTypeNames[RHS.BaseType],' RTypeEl=',GetObjName(RTypeEl),' RHS.IdentEl=',GetObjName(RHS.IdentEl),' RHS.ExprEl=',GetObjName(RHS.ExprEl),' rrfReadable=',rrfReadable in RHS.Flags);
    if (LTypeEl.ClassType=RTypeEl.ClassType)
        and (rrfReadable in RHS.Flags) then
      begin
      // e.g. ProcVar1:=ProcVar2
      if CheckProcTypeCompatibility(TPasProcedureType(LTypeEl),TPasProcedureType(RTypeEl),
          true,ErrorEl,RaiseOnIncompatible) then
        exit(cExact);
      end;
    if RaiseOnIncompatible then
      begin
      if (RTypeEl is TPasProcedureType) and (rrfReadable in RHS.Flags) then
        RaiseMsg(20170404154738,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [GetElementTypeName(RTypeEl),GetElementTypeName(LTypeEl)],ErrorEl);
      end;
    end
  else if LTypeEl.ClassType=TPasArrayType then
    begin
    LArray:=TPasArrayType(LTypeEl);
    if (length(LArray.Ranges)=0) and (RTypeEl.ClassType=TPasArrayType) then
      begin
      // DynOrOpenArr:=array
      RArray:=TPasArrayType(RTypeEl);
      if length(RArray.Ranges)=1 then
        begin
        // DynOrOpenArr:=SingleDimStaticArr
        if not IsOpenArray(LArray) then
          begin
          // DynArr:=SingleDimStaticArr is forbidden in ALL modes — real FPC
          // (ppcx64) rejects it in objfpc too, not just Delphi. Accepting it made
          // a fixed array wrongly rank cExact against a `var/const B: TBytes`
          // overload, so `ms.Read(fixedBuf,n)`/`ms.Write(fixedBuf,n)` picked the
          // TBytes overload instead of the untyped `Read(var B)`/`Write(const B)`.
          // OpenArr:=StaticArr stays allowed (an open-array PARAM views storage).
          if RaiseOnIncompatible then
            RaiseIncompatibleTypeDesc(20180620115341,nIncompatibleTypesGotExpected,
              [],'static array','dynamic array',ErrorEl);
          exit(cIncompatible);
          end;
        end
      else if length(RArray.Ranges)>1 then
        begin
        // DynOrOpenArr:=MultiDimStaticArr  -> no
        if RaiseOnIncompatible then
          RaiseIncompatibleTypeDesc(20180620115235,nIncompatibleTypesGotExpected,
            [],'multi dimensional static array','dynamic array',ErrorEl);
        exit(cIncompatible);
        end
      else if not (proOpenAsDynArrays in Options) then
        begin
        if IsOpenArray(LArray) then
          // OpenArray:=OpenOrDynArr -> ok
        else if IsOpenArray(RArray) then
          begin
          // DynArray:=OpenArray
          if RaiseOnIncompatible then
            RaiseIncompatibleTypeDesc(20180620115515,nIncompatibleTypesGotExpected,
              [],'open array','dynamic array',ErrorEl);
          exit(cIncompatible)
          end
        else
          begin
          // DynArray:=DynArr
          if (msDelphi in CurrentParser.CurrentModeswitches)
              and (LArray<>RArray) then
            begin
            // Delphi does not allow assigning arrays with same element types
            exit(RaiseIncompatType(20190215112626));
            end;
          end;
        end;

      // check element type
      if LArray.ElType=nil then
        begin
        // ArrayOfConst:=SingleDimArr
        if RArray.ElType=nil then
          // ArrayOfConst:=ArrayOfConst
          Result:=cExact
        else
          begin
          CurTVarRec:=GetTVarRec(LArray);
          if ResolveAliasType(RArray.ElType)=CurTVarRec then
            // ArrayOfConst:=ArrayOfTVarRec
            Result:=cExact
          else
            // ArrayOfConst:=SingleDimArr
            exit(RaiseIncompatType(20190215112715));
          end;
        end
      else if RArray.ElType=nil then
        // ArrayOfNonConst:=ArrayOfConst
        exit(RaiseIncompatType(20190215112907))
      else
        begin
        Result:=CheckElTypeCompatibility(LArray.ElType,RArray.ElType,prraAlias);
        {$ifdef FPC_HAS_CPSTRING}
        // Arrays of String ($H+) and of RawByteString are compatible, as arrays of
        // AnsiString and RawByteString are (strutils' SplitCommandLine into a
        // TStringArray, fcl-passrc's pparser under FPC 3.3.1).
        if Result=cIncompatible then
          begin
          ComputeElement(LArray.ElType,SrcResolved,[rcType]);
          ComputeElement(RArray.ElType,DstResolved,[rcType]);
          if (GetActualBaseType(SrcResolved.BaseType) in [btAnsiString,btRawByteString])
              and (GetActualBaseType(DstResolved.BaseType) in [btAnsiString,btRawByteString]) then
            Result:=cAliasExact;
          end;
        {$endif}
        if Result=cIncompatible then
          if RaiseOnIncompatible then
            begin
            GetIncompatibleTypeDesc(LArray.ElType,RArray.ElType,GotDesc,ExpDesc);
            RaiseMsg(20170328110050,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
              ['array of '+GotDesc,
               'array of '+ExpDesc],ErrorEl)
            end
          else
            exit(cIncompatible);
        end;
      // A named dynamic-array argument passed to an OPEN-array parameter is a valid
      // but non-exact match — rank it below an exact dynamic-array parameter of the
      // same type so overloads Test(array of X) / Test(TXArray) disambiguate (a
      // dynamic-array arg picks the dynamic overload). Array literals never reach
      // here (btArrayLit/btArrayOrSet), and open->open forwarding keeps its cost.
      if (Result in [cExact,cGenericExact,cAliasExact])
          and IsOpenArray(LArray) and IsDynArray(RArray,false) then
        Result:=cCompatible;
      end;
    // StaticArr:=StaticArr — distinct static array type instances are
    // assignment/parameter compatible when they have the same index bounds and
    // a compatible element type (matches the FPC compiler; fcl-passrc only
    // handled dyn/open-array LHS above). Needed e.g. to pass a plain
    // "array[0..N] of T" to a "specialize TGenArray<T>" parameter (tgenfunc15).
    if (Result=-1) and (length(LArray.Ranges)>0)
        and (RTypeEl.ClassType=TPasArrayType)
        and (rrfReadable in RHS.Flags) then
      begin
      RArray:=TPasArrayType(RTypeEl);
      if (LArray.ElType<>nil) and (RArray.ElType<>nil)
          and SameArrayRanges(LArray,RArray) then
        Result:=CheckElTypeCompatibility(LArray.ElType,RArray.ElType,prraAlias);
      end;
    // StaticCharArr := PChar — copy the (null-terminated) string into the
    // char array (codegen emits strlen + memset + memcpy). Matches FPC.
    if (Result=-1) and (length(LArray.Ranges)>0) and (LArray.ElType<>nil)
        and (RTypeEl.ClassType=TPasPointerType)
        and (rrfReadable in RHS.Flags) then
      begin
      ComputeElement(LArray.ElType,SrcResolved,[rcType]);
      ComputeElement(TPasPointerType(RTypeEl).DestType,DstResolved,[rcType]);
      if (SrcResolved.BaseType in btAllChars)
          and (DstResolved.BaseType in btAllChars) then
        Result:=cCompatible;
      end;
    end
  else if LTypeEl.ClassType=TPasRecordType then
    begin
    if (RTypeEl is TPasClassType) and (TPasClassType(RTypeEl).ObjKind=okInterface)
        and IsTGUID(TPasRecordType(LTypeEl)) then
      begin
      // GUIDVar := IntfTypeOrVar
      exit(cInterfaceToTGUID);
      end;
    // records of different type
    end
  else if LTypeEl.ClassType=TPasEnumType then
    begin
    // enums of different type
    end
  else if RTypeEl.ClassType=TPasSetType then
    begin
    // sets of different type are compatible if enum types are compatible
    if LTypeEl.ClassType=TPasSetType then
      begin
      ComputeElement(TPasSetType(LTypeEl).EnumType,DstResolved,[]);
      ComputeElement(TPasSetType(RTypeEl).EnumType,SrcResolved,[]);
      if (SrcResolved.LoTypeEl<>nil)
      and (SrcResolved.LoTypeEl=DstResolved.LoTypeEl) then
        Result:=cExact
      else if (SrcResolved.LoTypeEl.CustomData is TResElDataBaseType)
          and (DstResolved.LoTypeEl.CustomData is TResElDataBaseType)
          and (CompareText(SrcResolved.LoTypeEl.Name,DstResolved.LoTypeEl.Name)=0) then
        Result:=cExact
      else if RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20170216152510,nIncompatibleTypesGotExpected,
          [],SrcResolved,DstResolved,ErrorEl)
      else
        exit(cIncompatible);
      end
    else
      exit(RaiseIncompatType(20190215112924));
    end
  else if LTypeEl.ClassType=TPasPointerType then
    begin
    if RTypeEl.ClassType=TPasPointerType then
      begin
      // TypedPointer:=TypedPointer
      Result:=CheckAssignCompatibilityPointerType(TPasPointerType(LTypeEl).DestType,
        TPasPointerType(RTypeEl).DestType,ErrorEl,false);
      if Result=cIncompatible then
        exit(RaiseIncompatType(20190215112927));
      end
    else if (RTypeEl.ClassType=TPasArrayType)
        and (TPasArrayType(RTypeEl).ElType<>nil)
        and (length(TPasArrayType(RTypeEl).Ranges)>0) then
      begin
      // array[..] of char -> PChar, array[..] of widechar -> PWideChar: decay to
      // a pointer to the first element (FPC extension, e.g. P:=Buffer). The
      // array's element char width must match the pointer's pointed-to width.
      ComputeElement(TPasArrayType(RTypeEl).ElType,SrcResolved,[rcType]);
      ComputeElement(TPasPointerType(LTypeEl).DestType,DstResolved,[rcType]);
      if (SrcResolved.BaseType in btAllChars) and (DstResolved.BaseType in btAllChars)
          and (GetActualBaseType(SrcResolved.BaseType)=GetActualBaseType(DstResolved.BaseType)) then
        Result:=cTypeConversion
      else if (TPasPointerType(LTypeEl).DestType<>nil)
          and IsSameType(ResolveAliasType(TPasPointerType(LTypeEl).DestType),
                         ResolveAliasType(TPasArrayType(RTypeEl).ElType),
                         prraAlias) then
        // any static array decays to a pointer to its first element, e.g.
        // fcl-db passes `array[0..19] of ISC_STATUS` to a PISC_STATUS parameter
        Result:=cTypeConversion
      else
        exit(RaiseIncompatType(20260717000000));
      end;
    end
  else
    {$IFDEF VerbosePasResolver}
    RaiseNotYetImplemented(20160922163654,ErrorEl);
    {$ELSE}
    ;
    {$ENDIF}

  if Result=-1 then
    exit(RaiseIncompatType(20190215112931));
  if not (rrfReadable in RHS.Flags) then
    exit(RaiseIncompatType(20190215112934));
end;

function TPasResolver.CheckAssignCompatibilityArrayType(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;

  procedure Check_ArrayOfChar_String(ArrType: TPasArrayType;
    ArrLength: integer; const ElTypeResolved: TPasResolverResult;
    Expr: TPasExpr; ErrorEl: TPasElement);
  // check if assigning a string to an array of char fits
  var
    Value: TResEvalValue;
    ElBT: TResolverBaseType;
    l: Integer;
    S: String;
    {$ifdef FPC_HAS_CPSTRING}
    US: UnicodeString;
    {$endif}
  begin
    if Expr=nil then exit;
    ElBT:=GetActualBaseType(ElTypeResolved.BaseType);
    if length(ArrType.Ranges)=0 then
      begin
      // dynamic array of char can hold any string
      // ToDo: check if value can be converted without loss
      Result:=cExact;
      exit;
      end;
    // static array -> check length of string
    Value:=Eval(Expr,[refAutoConst]); // no external const allowed
    if Value=nil then
      begin
      // A RUNTIME string has no length to check here; FPC accepts it and copies
      // what fits (fcl-db's dbf_dbffile assigns
      // `FBackLink + StringOfChar(#0, ..)` to an array[0..262] of AnsiChar).
      // Dereferencing the nil result crashed the compiler outright.
      Result:=cCompatible;
      exit;
      end;
    try
      case Value.Kind of
      {$ifdef FPC_HAS_CPSTRING}
      revkString:
        if ElBT=btAnsiChar then
          l:=length(TResEvalString(Value).S)
        else
          begin
          US:=fExprEvaluator.GetUnicodeStr(TResEvalString(Value).S,ErrorEl);
          l:=length(US);
          end;
      {$endif}
      revkUnicodeString:
        begin
        if ElBT=btWideChar then
          l:=length(TResEvalUTF16(Value).S)
        else
          begin
          S:=String(TResEvalUTF16(Value).S);
          l:=length(S);
          end;
        end;
      else
        {$IFDEF VerbosePasResolver}
        writeln('Check_ArrayOfChar_String Value=',Value.AsDebugString);
        {$ENDIF}
        exit; // incompatible
      end;
      if l>ArrLength then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('Check_ArrayOfChar_String ElType=',ElBT,'=',GetResolverResultDbg(ElTypeResolved),' Value=',Value.AsDebugString);
        {$ENDIF}
        RaiseMsg(20170913113216,nExpectXArrayElementsButFoundY,sExpectXArrayElementsButFoundY,
          [IntToStr(ArrLength),IntToStr(l)],ErrorEl);
        end;
      Result:=cExact;
    finally
      ReleaseEvalValue(Value);
    end;
  end;

  procedure CheckRange(ArrType: TPasArrayType; RangeIndex: integer;
    Values: TPasResolverResult; ErrorEl: TPasElement);
  var
    ElTypeResolved: TPasResolverResult;

    procedure CheckArrOfCharAssignString;
    begin
      ComputeElement(ArrType.ElType,ElTypeResolved,[rcType]);
      if ElTypeResolved.BaseType in btAllChars then
        Result:=cTypeConversion; // ArrOfChar:=aString
    end;

  var
    Range, Value, Expr: TPasExpr;
    RangeResolved, ValueResolved: TPasResolverResult;
    i, ExpectedCount, ValCnt: Integer;
    IsLastRange, IsConstExpr: Boolean;
    ArrayValues: TPasExprArray;
    LeftResult: integer;
    ExprCompFlags: TPasResolverComputeFlags;
    BuiltInProc: TResElDataBuiltInProc;
    Ref: TResolvedReference;
    RArrayType: TPasArrayType;
  begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckAssignCompatibilityArrayType.CheckRange ArrType=',GetObjName(ArrType),' RgIndex=',RangeIndex,' Values=',GetResolverResultDbg(Values));
    {$ENDIF}
    if not (rrfReadable in RHS.Flags) then
      exit;
    if (Values.BaseType=btContext) and (RangeIndex=0) and (Values.LoTypeEl=ArrType) then
      begin
      Result:=cExact;
      exit;
      end;

    Expr:=Values.ExprEl;
    if (Expr=nil) and (Values.IdentEl is TPasConst)
        and (TPasConst(Values.IdentEl).VarType=nil) then
      Expr:=TPasVariable(Values.IdentEl).Expr;
    IsConstExpr:=(Expr<>nil) and ExprEvaluator.IsConst(Expr);
    if IsConstExpr then
      ExprCompFlags:=[rcConstant]
    else
      ExprCompFlags:=[];

    if Expr<>nil then
      begin
      if IsEmptyArrayExpr(Values) then
        begin
        if length(ArrType.Ranges)=0 then
          begin
          if RaiseOnIncompatible then
            MarkArrayExprRecursive(Values.ExprEl,ArrType);
          Result:=cExact; // empty set fits open and dyn array
          exit;
          end;
        end
      else if IsArrayOperatorAdd(Expr) and not (Values.BaseType in btAllStrings) then
        begin
        // a:=left+right
        if length(ArrType.Ranges)>0 then
          exit; // ToDo: StaticArray:=A+B
        // check a:=left
        ComputeElement(TBinaryExpr(Expr).Left,ValueResolved,ExprCompFlags);
        CheckRange(ArrType,RangeIndex,ValueResolved,ErrorEl);
        if Result=cIncompatible then exit;
        LeftResult:=Result;
        // check a:=right
        Result:=cIncompatible;
        ComputeElement(TBinaryExpr(Expr).Right,ValueResolved,ExprCompFlags);
        CheckRange(ArrType,RangeIndex,ValueResolved,ErrorEl);
        if Result=cIncompatible then exit;
        if Result<LeftResult then
          Result:=LeftResult;
        exit;
        end
      else if (Expr<>nil) and (Expr.ClassType=TParamsExpr)
          and (TParamsExpr(Expr).Kind=pekFuncParams) then
        begin
        if TParamsExpr(Expr).Value.CustomData is TResolvedReference then
          begin
          Ref:=TResolvedReference(TParamsExpr(Expr).Value.CustomData);
          if (Ref.Declaration is TPasUnresolvedSymbolRef)
              and (Ref.Declaration.CustomData is TResElDataBuiltInProc) then
            begin
            BuiltInProc:=TResElDataBuiltInProc(Ref.Declaration.CustomData);
            ArrayValues:=TParamsExpr(Expr).Params;
            if BuiltInProc.BuiltIn=bfConcatArray then
              begin
              // check Concat(array1,array2,...)
              Result:=cExact;
              for i:=0 to length(ArrayValues)-1 do
                begin
                LeftResult:=Result;
                Result:=cIncompatible;
                ComputeElement(ArrayValues[i],ValueResolved,ExprCompFlags);
                CheckRange(ArrType,RangeIndex,ValueResolved,ErrorEl);
                if Result=cIncompatible then exit;
                if Result<LeftResult then
                  Result:=LeftResult;
                end;
              exit;
              end
            else if BuiltInProc.BuiltIn=bfCopyArray then
              begin
              // check Copy(A...)
              ComputeElement(ArrayValues[0],ValueResolved,ExprCompFlags);
              CheckRange(ArrType,RangeIndex,ValueResolved,ErrorEl);
              exit;
              end;
            end;
          end;
        end;
      end;

    ExpectedCount:=-1;
    if length(ArrType.Ranges)=0 then
      begin
      // dynamic or open array
      if (Expr<>nil) then
        begin
        if Expr.ClassType=TArrayValues then
          ExpectedCount:=length(TArrayValues(Expr).Values)
        else if (Expr.ClassType=TParamsExpr) and (TParamsExpr(Expr).Kind=pekSet) then
          ExpectedCount:=length(TParamsExpr(Expr).Params)
        else if (Values.BaseType in btAllStringAndChars) then
          begin
          // e.g. const a: dynarray = string
          // or e.g. pass a string literal to an open array
          CheckArrOfCharAssignString;
          exit;
          end
        else
          begin
          // invalid
          exit;
          end;
        end
      else
        begin
        // type check
        if (Values.BaseType<>btContext) or (Values.LoTypeEl.ClassType<>TPasArrayType) then
          begin
          // RHS is not an array
          if (Values.BaseType in btAllStringAndChars) then
            begin
            // e.g. pass a string literal to an open array
            CheckArrOfCharAssignString;
            end;
          exit;
          end;
        RArrayType:=TPasArrayType(Values.LoTypeEl);
        if length(RArrayType.Ranges)>0 then
          begin
          if RaiseOnIncompatible then
            RaiseXExpectedButYFound(20180622104834,'dynamic array','static array',ErrorEl);
          exit;
          end;
        // dynarr:=dynarr -> check element type
        ComputeElement(GetArrayElType(ArrType),ElTypeResolved,[rcType]);
        Include(ElTypeResolved.Flags,rrfWritable);
        ComputeElement(GetArrayElType(RArrayType),ValueResolved,[rcType]);
        Include(ValueResolved.Flags,rrfReadable);
        Result:=CheckAssignResCompatibility(ElTypeResolved,ValueResolved,ErrorEl,RaiseOnIncompatible);
        exit;
        end;
      Range:=nil;
      IsLastRange:=true;
      end
    else
      begin
      // static array
      Range:=ArrType.Ranges[RangeIndex];
      ExpectedCount:=GetRangeLength(Range);
      if ExpectedCount=0 then
        begin
        ComputeElement(Range,RangeResolved,[rcConstant]);
        RaiseNotYetImplemented(20170222232409,Expr,'range '+GetResolverResultDbg(RangeResolved));
        end;
      IsLastRange:=RangeIndex+1=length(ArrType.Ranges);
      if Expr=nil then
        begin
        if (ValueResolved.BaseType=btContext) and (ValueResolved.LoTypeEl.ClassType=TPasArrayType) then
          begin
          {$IFDEF VerbosePasResolver}
          writeln('CheckRange TODO StaticArr:=Arr');
          {$ENDIF}
          end
        else if (Values.BaseType in btAllStringAndChars) then
          begin
          // static array-of-char := string variable
          CheckArrOfCharAssignString;
          end;
        exit;
        end;
      end;

    if IsLastRange then
      begin
      ComputeElement(ArrType.ElType,ElTypeResolved,[rcType]);
      // A SUBRANGE element keeps its own range expression: the check of each
      // value evaluates it, and the array's index range there made the LCL's
      // `array[..] of TPanelButton = (pbOK, ...)` compare enum values with 0..3.
      if ElTypeResolved.BaseType<>btRange then
        ElTypeResolved.ExprEl:=Range;
      Include(ElTypeResolved.Flags,rrfWritable);
      end
    else
      ElTypeResolved.BaseType:=btNone;

    if (Expr<>nil)
        and ((Expr.ClassType=TArrayValues)
          or ((Expr is TParamsExpr) and (TParamsExpr(Expr).Kind=pekSet))) then
      begin
      // array literal

      if (ErrorEl.Parent is TPasVariable) then
        begin
        // array initialization  e.g.  var a: tarray = []
        if msDelphi in CurrentParser.CurrentModeswitches then
          begin
          // Delphi expects square brackets for dynamic arrays
          //   and round brackets for static arrays
          if length(ArrType.Ranges)>0 then
            begin
            // static array
            if Expr.ClassType<>TArrayValues then
              begin
              if RaiseOnIncompatible then
                RaiseXExpectedButYFound(20180615121203,'(','[',ErrorEl);
              exit;
              end;
            end
          else
            begin
            // dyn array
            if Expr.ClassType=TArrayValues then
              begin
              if RaiseOnIncompatible then
                RaiseXExpectedButYFound(20180615122953,'[','(',ErrorEl);
              exit;
              end;
            end;
          end
        else
          begin
          // ObjFPC always expects round brackets in initialization
          if Expr.ClassType<>TArrayValues then
            begin
            if RaiseOnIncompatible then
              RaiseXExpectedButYFound(20170913181208,'(','[',ErrorEl);
            exit;
            end;
          end;
        end;

      // check each value
      if Expr.ClassType=TArrayValues then
        ArrayValues:=TArrayValues(Expr).Values
      else
        ArrayValues:=TParamsExpr(Expr).Params;
      ValCnt:=length(ArrayValues);
      Include(ExprCompFlags,rcNoImplicitProcType);
      for i:=0 to ExpectedCount-1 do
        begin
        if i=ValCnt then
          begin
          // not enough values
          if ValCnt>0 then
            ErrorEl:=ArrayValues[ValCnt-1];
          RaiseMsg(20170222233001,nExpectXArrayElementsButFoundY,sExpectXArrayElementsButFoundY,
            [IntToStr(ExpectedCount),IntToStr(ValCnt)],ErrorEl);
          end;
        Value:=ArrayValues[i];
        ComputeElement(Value,ValueResolved,ExprCompFlags);
        if IsLastRange then
          begin
          // last dimension -> check element type
          Result:=CheckAssignResCompatibility(ElTypeResolved,ValueResolved,Value,RaiseOnIncompatible);
          if Result=cIncompatible then
            exit;
          CheckAssignExprRange(ElTypeResolved,Value);
          end
        else
          begin
          // multi dimensional array -> check next range
          CheckRange(ArrType,RangeIndex+1,ValueResolved,Value);
          end;
        end;
      if ExpectedCount<ValCnt then
        begin
        // too many values
        ErrorEl:=ArrayValues[ExpectedCount];
        if RaiseOnIncompatible then
          RaiseMsg(20170222233605,nExpectXArrayElementsButFoundY,sExpectXArrayElementsButFoundY,
            [IntToStr(ExpectedCount),IntToStr(ValCnt)],ErrorEl);
        exit;
        end;

      if RaiseOnIncompatible and (Expr.ClassType=TParamsExpr) then
        // mark [] expression as an array
        MarkArrayExpr(TParamsExpr(Expr),ArrType);

      end
    else
      begin
      // single value
      // Note: the parser does not store the difference between (1) and 1
      if not IsLastRange then
        begin
        if RaiseOnIncompatible then
          RaiseMsg(20170223095307,nExpectXArrayElementsButFoundY,sExpectXArrayElementsButFoundY,
            [IntToStr(ExpectedCount),'1'],ErrorEl);
        exit;
        end;
      if (ElTypeResolved.BaseType in btAllChars) then
        if (Values.BaseType in btAllStrings) then
          begin
          // e.g. array of char = ''
          Check_ArrayOfChar_String(ArrType,ExpectedCount,ElTypeResolved,Expr,ErrorEl);
          exit;
          end
        else if (Values.BaseType in btAllChars) then
          begin
          // A single char fills an array of char, exactly as a string does -
          // fcl-db's TStringField.SetValue writes `Buf := #0` over an
          // array[0..dsMaxStringSize] of AnsiChar. Only btWidechar was listed
          // here, so an AnsiChar was read as a one-element array literal.
          Check_ArrayOfChar_String(ArrType,ExpectedCount,ElTypeResolved,Expr,ErrorEl);
          exit;
          end;
      if (ExpectedCount>1) then
        begin
        if RaiseOnIncompatible then
          begin
          {$IFDEF VerbosePasResolver}
          writeln('CheckRange Values=',GetResolverResultDbg(Values),' ElTypeResolved=',GetResolverResultDbg(ElTypeResolved));
          {$ENDIF}
          RaiseMsg(20170913103143,nExpectXArrayElementsButFoundY,sExpectXArrayElementsButFoundY,
            [IntToStr(ExpectedCount),'1'],ErrorEl);
          end;
        exit;
        end;
      // check element type
      Result:=CheckAssignResCompatibility(ElTypeResolved,Values,ErrorEl,RaiseOnIncompatible);
      if Result=cIncompatible then
        exit;
      if Expr<>nil then
        CheckAssignExprRange(ElTypeResolved,Expr);
      end;
  end;

var
  LArrType: TPasArrayType;
begin
  Result:=cIncompatible;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckAssignCompatibilityArrayType LHS=',GetResolverResultDbg(LHS),' RHS=',GetResolverResultDbg(RHS));
  {$ENDIF}
  if (LHS.BaseType<>btContext) or (not (LHS.LoTypeEl is TPasArrayType)) then
    RaiseInternalError(20170222230012);
  LArrType:=TPasArrayType(LHS.LoTypeEl);
  if (LArrType.ElType=nil) and (rrfReadable in RHS.Flags)
      and (RHS.BaseType in [btArrayLit,btArrayOrSet]) then
    begin
    // ArrayOfConst:=[]
    exit(cExact);
    end;

  // A user `operator :=` may build the array from a single value of another
  // type, which is not an array literal at all (real48utils' Real48).
  if not (RHS.BaseType in [btArrayLit,btArrayOrSet,btSet])
      and HasImplicitOperatorTo(LArrType,RHS) then
    exit(cCompatible);

  CheckRange(LArrType,0,RHS,ErrorEl);

  // For an array-constructor argument (`[1,2,3]`), an OPEN-array parameter is the
  // better match than a dynamic-array parameter, so overloads Test(array of X) /
  // Test(TXArray) disambiguate to the open one. Rank an otherwise-exact dynamic-array
  // param one step below. (A lone dynamic-array candidate still wins — single match.)
  if (Result in [cExact,cGenericExact,cAliasExact])
      and (RHS.BaseType in [btArrayLit,btArrayOrSet])
      and (length(LArrType.Ranges)=0) and not IsOpenArray(LArrType) then
    Result:=cCompatible;

  if (Result=cIncompatible) and RaiseOnIncompatible then
    RaiseIncompatibleTypeRes(20180622104721,nIncompatibleTypesGotExpected,[],RHS,LHS,ErrorEl);
end;

function TPasResolver.CheckAssignCompatibilityPointerType(LTypeEl,
  RTypeEl: TPasType; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
var
  LeftResolved, RightResolved: TPasResolverResult;
begin
  ComputeElement(LTypeEl,LeftResolved,[rcNoImplicitProc]);
  ComputeElement(RTypeEl,RightResolved,[rcNoImplicitProc]);
  Include(LeftResolved.Flags,rrfWritable);
  Include(RightResolved.Flags,rrfReadable);
  Result:=CheckAssignResCompatibility(LeftResolved,RightResolved,ErrorEl,RaiseOnIncompatible);
end;

function TPasResolver.CheckEqualCompatibilityUserType(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
// LHS.BaseType=btContext=RHS.BaseType and both rrfReadable
var
  LTypeEl, RTypeEl: TPasType;
  AResolved, BResolved: TPasResolverResult;

  function IncompatibleElements: integer;
  begin
    Result:=cIncompatible;
    if not RaiseOnIncompatible then exit;
    RaiseIncompatibleType(20170216152513,nIncompatibleTypesGotExpected,
      [],LTypeEl,RTypeEl,ErrorEl);
  end;

  // Maps an enum subrange (e.g. tsub = e2..e5) to its base enum so that
  // "set of tsub" is recognized as compatible with "set of te".
  function SetElemHostType(const R: TPasResolverResult): TPasType;
  var
    HostRes: TPasResolverResult;
  begin
    Result:=R.LoTypeEl;
    if (Result is TPasRangeType) and (TPasRangeType(Result).RangeExpr<>nil) then
      begin
      ComputeElement(TPasRangeType(Result).RangeExpr.Left,HostRes,[]);
      if HostRes.LoTypeEl<>nil then
        Result:=HostRes.LoTypeEl;
      end;
  end;

begin
  if (LHS.LoTypeEl=nil) then
    RaiseInternalError(20161007223118);
  if (RHS.LoTypeEl=nil) then
    RaiseInternalError(20161007223119);
  LTypeEl:=LHS.LoTypeEl;
  RTypeEl:=RHS.LoTypeEl;
  if LTypeEl=RTypeEl then
    exit(cExact);

  if LTypeEl.ClassType=TPasClassType then
    begin
    if RTypeEl.ClassType=TPasClassType then
      begin
      // e.g. if Sender=Button1 then
      Result:=CheckSrcIsADstType(LHS,RHS);
      if Result=cIncompatible then
        Result:=CheckSrcIsADstType(RHS,LHS);
      if (Result=cIncompatible)
          and (TPasClassType(LTypeEl).ObjKind<>TPasClassType(RTypeEl).ObjKind)
          and (TPasClassType(LTypeEl).ObjKind in [okClass,okInterface])
          and (TPasClassType(RTypeEl).ObjKind in [okClass,okInterface])
          and not TPasClassType(LTypeEl).IsExternal
          and not TPasClassType(RTypeEl).IsExternal then
        // A class reference compared with an INTERFACE: the reference converts
        // to the interface, exactly as it does when assigned to one, and the
        // comparison is of the resulting references. ppcx64 accepts it -
        // system.threading writes `_CurrentTask=(FParams.Parent as ITask)`.
        Result:=cCompatible;
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20180324190757,nTypesAreNotRelatedXY,[],LHS,RHS,ErrorEl);
      exit;
      end
    else if RTypeEl.ClassType=TPasRecordType then
      begin
      if (TPasClassType(LTypeEl).ObjKind=okInterface)
          and IsTGUID(TPasRecordType(RTypeEl)) then
        // IntfVar=GuidVar
        exit(cInterfaceToTGUID);
      end;
    exit(IncompatibleElements);
    end
  else if LTypeEl.ClassType=TPasClassOfType then
    begin
    if RTypeEl.ClassType=TPasClassOfType then
      begin
      // for example: if ImageClass=ImageClass then
      Result:=CheckClassIsClass(TPasClassOfType(LTypeEl).DestType,
                                TPasClassOfType(RTypeEl).DestType);
      if Result=cIncompatible then
        Result:=CheckClassIsClass(TPasClassOfType(RTypeEl).DestType,
                                  TPasClassOfType(LTypeEl).DestType);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20180324190804,nTypesAreNotRelatedXY,[],LHS,RHS,ErrorEl);
      exit;
      end;
    exit(IncompatibleElements);
    end
  else if LTypeEl.ClassType=TPasEnumType then
    begin
    // enums of different type
    if not RaiseOnIncompatible then
      exit(cIncompatible);
    if RTypeEl.ClassType=TPasEnumValue then
      RaiseIncompatibleType(20170216152523,nIncompatibleTypesGotExpected,
        [],TPasEnumType(LTypeEl),TPasEnumType(RTypeEl),ErrorEl)
    else
      exit(IncompatibleElements);
    end
  else if LTypeEl.ClassType=TPasRecordType then
    begin
    if RTypeEl.ClassType=TPasClassType then
      begin
      if (TPasClassType(RTypeEl).ObjKind=okInterface)
          and IsTGUID(TPasRecordType(LTypeEl)) then
        // GuidVar=IntfVar
        exit(cInterfaceToTGUID);
      end;
    end
  else if LTypeEl.ClassType=TPasSetType then
    begin
    if RTypeEl.ClassType=TPasSetType then
      begin
      ComputeElement(TPasSetType(LTypeEl).EnumType,AResolved,[]);
      ComputeElement(TPasSetType(RTypeEl).EnumType,BResolved,[]);
      if (AResolved.LoTypeEl<>nil)
      and (AResolved.LoTypeEl=BResolved.LoTypeEl) then
        exit(cExact);
      if (AResolved.LoTypeEl.CustomData is TResElDataBaseType)
          and (BResolved.LoTypeEl.CustomData is TResElDataBaseType)
          and (CompareText(AResolved.LoTypeEl.Name,BResolved.LoTypeEl.Name)=0) then
        exit(cExact);
      // a set of an enum subrange is compatible with a set of its base enum
      // (or with a set of another subrange sharing the same base enum)
      if (SetElemHostType(AResolved)<>nil)
          and (SetElemHostType(AResolved)=SetElemHostType(BResolved)) then
        exit(cExact);
      if RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20170216152524,nIncompatibleTypesGotExpected,
          [],AResolved,BResolved,ErrorEl)
      else
        exit(cIncompatible);
      end
    else
      exit(IncompatibleElements);
    end
  else if LTypeEl is TPasProcedureType then
    begin
    if RTypeEl is TPasProcedureType then
      begin
      // e.g. ProcVar1 = ProcVar2
      if CheckProcTypeCompatibility(TPasProcedureType(LTypeEl),TPasProcedureType(RTypeEl),
          false,nil,false) then
        exit(cExact);
      end
    else
      exit(IncompatibleElements);
    end
  else if LTypeEl.ClassType=TPasPointerType then
    begin
    if RTypeEl.ClassType=TPasPointerType then
      // TypedPointer=TypedPointer
      exit(cExact);
    end
  else if IsStaticCharArray(LTypeEl) and IsStaticCharArray(RTypeEl) then
    // two static arrays of char compare as strings, e.g. lazutils'
    // `Signature<>FilerSignature`
    exit(cExact)
  else if IsDynArray(LTypeEl) and IsDynArray(RTypeEl)
      and (CheckAssignResCompatibility(LHS,RHS,nil,false)<cIncompatible) then
    // two dynamic arrays that may be assigned to each other compare as
    // references, as in fpc: lazedit's `FFilteredList = FNodeInfoList`, both
    // declared `array of TLazEditFoldNodeInfo`
    exit(cExact);
  exit(IncompatibleElements);
end;

function TPasResolver.IsStaticCharArray(El: TPasType): boolean;
// Whether El is a static array (with an index range) of a char type.
var
  ElResolved: TPasResolverResult;
begin
  Result:=false;
  if not (El is TPasArrayType) or (length(TPasArrayType(El).Ranges)=0)
      or (TPasArrayType(El).ElType=nil) then
    exit;
  ComputeElement(TPasArrayType(El).ElType,ElResolved,[rcType]);
  Result:=ElResolved.BaseType in btAllChars;
end;

function TPasResolver.CheckTypeCast(El: TPasType; Params: TParamsExpr;
  RaiseOnError: boolean): integer;
// for example  if TClassA(AnObject)=nil then ;
var
  Param: TPasExpr;
  ParamResolved, ResolvedEl: TPasResolverResult;
begin
  if length(Params.Params)<>1 then
    begin
    if RaiseOnError then
      RaiseMsg(20170216152526,nWrongNumberOfParametersForTypeCast,
        sWrongNumberOfParametersForTypeCast,[El.Name],Params);
    exit(cIncompatible);
    end;
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProcType]);
  ComputeElement(El,ResolvedEl,[rcType]);
  Result:=CheckTypeCastRes(ParamResolved,ResolvedEl,Param,RaiseOnError);
end;

function TPasResolver.HasExplicitOperatorFor(aType: TPasType): boolean;
// True when a free-standing `operator Explicit(a: aType): ...` is visible: in
// the section that declares aType, or in the unit being compiled.

  function ScanSection(Sec: TPasElement): boolean;
  var
    i: Integer;
    El: TPasElement;
  begin
    Result:=false;
    if not (Sec is TPasSection) then exit;
    for i:=0 to TPasSection(Sec).Declarations.Count-1 do
      begin
      El:=TPasElement(TPasSection(Sec).Declarations[i]);
      if (El is TPasOperator) and not (El is TPasClassOperator)
         and (TPasOperator(El).OperatorType=otExplicit)
         and (TPasOperator(El).ProcType.Args.Count=1)
         and (ResolveAliasType(TPasArgument(TPasOperator(El).ProcType.Args[0]).ArgType)
              =aType) then
        exit(true);
      end;
  end;

begin
  Result:=false;
  aType:=ResolveAliasType(aType);
  if aType=nil then exit;
  if ScanSection(aType.Parent) then exit(true);
  if RootElement<>nil then
    begin
    if ScanSection(RootElement.InterfaceSection) then exit(true);
    if ScanSection(RootElement.ImplementationSection) then exit(true);
    end;
end;


function TPasResolver.FindImplicitOperatorTo(aType: TPasType;
  const RHS: TPasResolverResult): TPasOperator;
// True when a free-standing `operator :=` / `operator Implicit` turns the RHS
// into aType. The mirror of HasExplicitOperatorFor, needed where aType is not a
// record and so has no class operators: rtl-extra's real48utils assigns a
// Double to a Real48, which is an array of 6 bytes.

  function ScanSection(Sec: TPasElement): TPasOperator;
  var
    i: Integer;
    El: TPasElement;
    Op: TPasOperator;
    ArgResolved: TPasResolverResult;
  begin
    Result:=nil;
    if not (Sec is TPasSection) then exit;
    for i:=0 to TPasSection(Sec).Declarations.Count-1 do
      begin
      El:=TPasElement(TPasSection(Sec).Declarations[i]);
      if not ((El is TPasOperator) and not (El is TPasClassOperator)) then continue;
      Op:=TPasOperator(El);
      if not (Op.OperatorType in [otAssign,otImplicit]) then continue;
      if Op.ProcType.Args.Count<>1 then continue;
      if not (Op.ProcType is TPasFunctionType) then continue;
      if TPasFunctionType(Op.ProcType).ResultEl=nil then continue;
      if ResolveAliasType(TPasFunctionType(Op.ProcType).ResultEl.ResultType)<>aType then
        continue;
      ComputeElement(TPasArgument(Op.ProcType.Args[0]).ArgType,ArgResolved,[rcType]);
      Include(ArgResolved.Flags,rrfWritable);
      if CheckAssignResCompatibility(ArgResolved,RHS,nil,false)<>cIncompatible then
        exit(Op);
      end;
  end;

begin
  Result:=nil;
  aType:=ResolveAliasType(aType);
  if aType=nil then exit;
  Result:=ScanSection(aType.Parent);
  if Result<>nil then exit;
  if RootElement<>nil then
    begin
    Result:=ScanSection(RootElement.InterfaceSection);
    if Result<>nil then exit;
    Result:=ScanSection(RootElement.ImplementationSection);
    end;
end;

function TPasResolver.HasImplicitOperatorTo(aType: TPasType;
  const RHS: TPasResolverResult): boolean;
begin
  Result:=FindImplicitOperatorTo(aType,RHS)<>nil;
end;

function TPasResolver.FindGlobalConvOperator(aSourceType,
  aTargetType: TPasType; OpTypes: TOperatorTypes): TPasOperator;
// A free-standing conversion operator from one named type to another. Neither
// side need be a record, so neither can have it as a member.

  function ScanSection(Sec: TPasElement): TPasOperator;
  var
    i: Integer;
    El: TPasElement;
    Op: TPasOperator;
  begin
    Result:=nil;
    if not (Sec is TPasSection) then exit;
    for i:=0 to TPasSection(Sec).Declarations.Count-1 do
      begin
      El:=TPasElement(TPasSection(Sec).Declarations[i]);
      if not ((El is TPasOperator) and not (El is TPasClassOperator)) then continue;
      Op:=TPasOperator(El);
      if not (Op.OperatorType in OpTypes) then continue;
      if Op.ProcType.Args.Count<>1 then continue;
      if not (Op.ProcType is TPasFunctionType) then continue;
      if TPasFunctionType(Op.ProcType).ResultEl=nil then continue;
      if ResolveAliasType(TPasArgument(Op.ProcType.Args[0]).ArgType)<>aSourceType then
        continue;
      if ResolveAliasType(TPasFunctionType(Op.ProcType).ResultEl.ResultType)<>aTargetType then
        continue;
      exit(Op);
      end;
  end;

begin
  Result:=nil;
  aSourceType:=ResolveAliasType(aSourceType);
  aTargetType:=ResolveAliasType(aTargetType);
  if (aSourceType=nil) or (aTargetType=nil) then exit;
  Result:=ScanSection(aSourceType.Parent);
  if Result<>nil then exit;
  Result:=ScanSection(aTargetType.Parent);
  if Result<>nil then exit;
  if RootElement<>nil then
    begin
    Result:=ScanSection(RootElement.InterfaceSection);
    if Result<>nil then exit;
    Result:=ScanSection(RootElement.ImplementationSection);
    end;
end;

function TPasResolver.CheckTypeCastRes(const FromResolved,
  ToResolved: TPasResolverResult; ErrorEl: TPasElement; RaiseOnError: boolean
  ): integer;

  procedure WarnClassTypesAreNotRelated(GotType, ExpType: TPasClassType);
  var
    GotDesc, ExpDesc: String;
  begin
    GetIncompatibleTypeDesc(GotType,ExpType,GotDesc,ExpDesc);
    LogMsg(20200209140450,mtWarning,nClassTypesAreNotRelatedXY,
      sClassTypesAreNotRelatedXY,[GotDesc,ExpDesc],ErrorEl);
  end;

  function IsConstSingleCharString(aExpr: TPasExpr): boolean;
  // True if aExpr is a constant string that holds exactly one code unit, so it
  // can be typecast to a char (e.g. WideChar('é')). Non-constants → False (Eval
  // with [] returns nil and does not raise), so a string variable still errors.
  var
    lVal: TResEvalValue;
    w: WideChar;
  begin
    Result:=false;
    if aExpr=nil then exit;
    lVal:=Eval(aExpr,[]);
    if lVal=nil then exit;
    try
      case lVal.Kind of
      {$ifdef FPC_HAS_CPSTRING}
      revkString:
        Result:=fExprEvaluator.GetWideChar(TResEvalString(lVal).S,w);
      {$endif}
      revkUnicodeString:
        Result:=length(TResEvalUTF16(lVal).S)=1;
      end;
    finally
      ReleaseEvalValue(lVal);
    end;
  end;

var
  ToTypeEl, FromTypeEl: TPasType;
  ToTypeBaseType: TResolverBaseType;
  C: TClass;
  ToProcType, FromProcType: TPasProcedureType;
  TemplType: TPasGenericTemplateType;
  i: Integer;
  ConToken: TToken;
  ConEl: TPasElement;
  ToClassType, FromClassType: TPasClassType;
  PtrDestResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  ToTypeEl:=ToResolved.LoTypeEl;
  // A set type computes with its ELEMENT type as LoTypeEl, so the class of that
  // element must not pick the branch: `TOptions([oC])` (SynEdit's
  // `TSynGutterMarksOptions([..])`) is a set cast, not an enum cast.
  if (ToResolved.BaseType=btSet) and (rrfReadable in FromResolved.Flags)
      and (FromResolved.BaseType in [btSet,btArrayOrSet]) then
    exit(cExact);
  if (ToTypeEl<>nil)
      and (rrfReadable in FromResolved.Flags) then
    begin
    C:=ToTypeEl.ClassType;
    if (FromResolved.BaseType=btUntyped)
        or ((FromResolved.BaseType=btPointer)
            and (FromResolved.ExprEl is TUnaryExpr)
            and (TUnaryExpr(FromResolved.ExprEl).OpCode=eopDeref)) then
      begin
      // typecast an untyped parameter, or the dereference of an UNTYPED pointer,
      // which names memory of no type at all and so reads as any type - it is the
      // only way to use such a deref. fcl-db's TDataSet.DataConvert converts
      // through `Pointer` parameters: `TDateTimeRec(aDest^) := ..`,
      // `CurrToBCD(Currency(aSource^))`.
      Result:=cCompatible;
      end
    else if C=TPasUnresolvedSymbolRef then
      begin
      if ToTypeEl.CustomData is TResElDataBaseType then
        begin
        // type cast to base type, e.g. double(aninteger)
        if ToTypeEl=FromResolved.LoTypeEl then
          exit(cExact);
        if (FromResolved.BaseType=btContext)
            and (FromResolved.LoTypeEl.ClassType=TPasGenericTemplateType) then
          exit(cExact); // e.g. double(T) -> will be checked when specialized
        ToTypeBaseType:=(ToTypeEl.CustomData as TResElDataBaseType).BaseType;
        if ToTypeBaseType=FromResolved.BaseType then
          Result:=cExact
        else if ToTypeBaseType in btAllInteger then
          begin
          if FromResolved.BaseType in (btArrayRangeTypes+[btRange,btCurrency]) then
            Result:=cCompatible
          else if FromResolved.BaseType=btPointer then
            Result:=cCompatible // pointer to integer (e.g. PtrUInt(p), Int64(p))
          else if FromResolved.BaseType=btNil then
            Result:=cCompatible // nil to integer -> 0 (e.g. HModule(nil))
          else if FromResolved.BaseType=btContext then
            begin
            FromTypeEl:=FromResolved.LoTypeEl;
            if FromTypeEl.ClassType=TPasEnumType then
              // e.g. longint(TEnum)
              Result:=cCompatible
            else if (FromTypeEl.ClassType=TPasPointerType)
                or (FromTypeEl.ClassType=TPasClassType)
                or (FromTypeEl.ClassType=TPasClassOfType)
                // A dynamic array IS one machine pointer, so it reads as a
                // pointer-sized integer too - fcl-db's fpjsondataset writes
                // `NativeInt(ABookMark)` where the bookmark is a TBytes.
                or IsDynArray(FromTypeEl) then
              // e.g. PtrUInt(PMyRecord), Int64(SomeObject)
              Result:=cCompatible;
            end;
          end
        else if ToTypeBaseType in btAllFloats then
          begin
          if FromResolved.BaseType in btAllFloats then
            Result:=cCompatible
          else if FromResolved.BaseType in btAllInteger then
            Result:=cCompatible
            ;
          end
        else if ToTypeBaseType in btAllBooleans then
          begin
          if FromResolved.BaseType in btAllBooleans then
            Result:=cCompatible
          else if FromResolved.BaseType in btAllInteger then
            Result:=cCompatible
          else if FromResolved.BaseType in btAllChars then
            // Boolean(aChar): both are ordinals, and the mirror Char(aBoolean)
            // is already allowed below through btArrayRangeTypes. fcl-db's
            // dbf_prscore writes `Boolean(Args[0]^) and Boolean(Args[1]^)`.
            Result:=cCompatible;
          end
        else if ToTypeBaseType in btAllChars then
          begin
          if FromResolved.BaseType in (btArrayRangeTypes+[btRange]) then
            Result:=cCompatible
          else if FromResolved.BaseType=btContext then
            begin
            FromTypeEl:=FromResolved.LoTypeEl;
            if FromTypeEl.ClassType=TPasEnumType then
              // e.g. Char(TEnum)
              Result:=cCompatible;
            end
          else if (FromResolved.BaseType in btAllStrings)
              and IsConstSingleCharString(FromResolved.ExprEl) then
            // e.g. WideChar('é') / UnicodeChar('é') — a constant string holding
            // exactly one code unit can be typecast to a char. The value fold is
            // done later by the const evaluator.
            Result:=cCompatible;
          end
        {$IFNDEF PAS2JS}
        else if ToTypeBaseType=btVariant then
          begin
          // e.g. Variant('Bar'), Variant(1): the conversion is done at runtime,
          // same as Variant:=value in CheckAssignResCompatibility
          if FromResolved.BaseType in (btAllInteger+btAllFloats+[btCurrency]
              +btAllBooleans+btAllStringAndChars) then
            Result:=cCompatible;
          end
        {$ENDIF}
        else if ToTypeBaseType in btAllStrings then
          begin
          if FromResolved.BaseType in btAllStringAndChars then
            Result:=cCompatible
          else if (FromResolved.BaseType=btPointer)
              and (ToTypeBaseType in btAllStringPointer) then
            Result:=cExact
          else if (FromResolved.BaseType=btContext)
              and (FromResolved.LoTypeEl is TPasPointerType) then
            begin
            { A typed char pointer (PAnsiChar/PWideChar) can be typecast to ANY
              string type: it builds a string from the null-terminated data. }
            ComputeElement(TPasPointerType(FromResolved.LoTypeEl).DestType,
                           PtrDestResolved,[rcType]);
            if PtrDestResolved.BaseType in btAllChars then
              Result:=cCompatible;
            end
          else if (FromResolved.BaseType=btContext)
              and (FromResolved.LoTypeEl is TPasArrayType)
              and (TPasArrayType(FromResolved.LoTypeEl).ElType<>nil) then
            begin
            { An array of char builds a string, e.g. AnsiString(Buffer). }
            ComputeElement(TPasArrayType(FromResolved.LoTypeEl).ElType,
                           PtrDestResolved,[rcType]);
            if PtrDestResolved.BaseType in btAllChars then
              Result:=cCompatible
            else if IsDynArray(FromResolved.LoTypeEl)
                and (GetActualBaseType(ToTypeBaseType) in [btAnsiString,btRawByteString,btUnicodeString]) then
              { any other dynamic array reinterprets as a refcounted string
                (String(TBytes)), the mirror of TBytes(String) }
              Result:=cCompatible;
            end;
          end
        else if ToTypeBaseType=btPointer then
          begin
          // btNil belongs here as much as in the typed-pointer branch below:
          // `Pointer(nil)` is a pointer-typed nil. paszlib's gzio returns
          // `gzopen := gzFile(nil)` where `gzFile = pointer`.
          if FromResolved.BaseType in ([btPointer,btNil]+btAllStringPointer) then
            Result:=cExact
          else if FromResolved.BaseType in btAllInteger then
            Result:=cCompatible // integer to pointer (e.g. Pointer(i))
          else if FromResolved.BaseType=btContext then
            begin
            FromTypeEl:=FromResolved.LoTypeEl;
            C:=FromTypeEl.ClassType;
            if (C=TPasClassType)
                or (C=TPasClassOfType)
                or (C=TPasPointerType)
                or ((C=TPasArrayType) and IsDynArray(FromTypeEl)) then
              Result:=cExact
            else if (C=TPasProcedureType) or (C=TPasFunctionType) then
              begin
              // from procvar to pointer
              FromProcType:=TPasProcedureType(FromTypeEl);
              if FromProcType.IsOfObject then
                begin
                if proMethodAddrAsPointer in Options then
                  Result:=cCompatible
                else if RaiseOnError then
                  RaiseMsg(20170416183615,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
                    [GetElementTypeName(FromProcType)+' '+ProcTypeModifiers[ptmOfObject],
                     BaseTypeNames[btPointer]],ErrorEl);
                end
              else if FromProcType.IsNested then
                begin
                if RaiseOnError then
                  RaiseMsg(20170416183800,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
                    [GetElementTypeName(FromProcType)+' '+ProcTypeModifiers[ptmIsNested],
                     BaseTypeNames[btPointer]],ErrorEl);
                end
              else if FromProcType.IsReferenceTo then
                begin
                if proProcTypeWithoutIsNested in Options then
                  Result:=cCompatible
                else if RaiseOnError then
                  RaiseMsg(20170419144311,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
                    [GetElementTypeName(FromProcType)+' '+ProcTypeModifiers[ptmReferenceTo],
                     BaseTypeNames[btPointer]],ErrorEl);
                end
              else
                Result:=cCompatible;
              end;
            end;
          end;
        end;
      end
    else if C=TPasClassType then
      begin
      ToClassType:=TPasClassType(ToTypeEl);
      // to class
      if FromResolved.BaseType=btContext then
        begin
        FromTypeEl:=FromResolved.LoTypeEl;
        if FromTypeEl.ClassType=TPasClassType then
          begin
          FromClassType:=TPasClassType(FromTypeEl);
          if FromResolved.IdentEl is TPasType then
            RaiseMsg(20170404162606,nCannotTypecastAType,sCannotTypecastAType,[],ErrorEl);
          if FromClassType.ObjKind=ToClassType.ObjKind then
            begin
            // type cast upwards or downwards
            Result:=CheckSrcIsADstType(FromResolved,ToResolved);
            if Result=cIncompatible then
              Result:=CheckSrcIsADstType(ToResolved,FromResolved);
            end
          else if ToClassType.ObjKind=okInterface then
            begin
            if (FromClassType.ObjKind=okClass)
                and (not FromClassType.IsExternal) then
              begin
              // e.g. intftype(classinstvar)
              Result:=cCompatible;
              end;
            end
          else if FromClassType.ObjKind=okInterface then
            begin
            if (ToClassType.ObjKind=okClass)
                and (not ToClassType.IsExternal) then
              begin
              // e.g. classtype(intfvar)
              Result:=cCompatible;
              end;
            end;
          if Result=cIncompatible then
            Result:=CheckTypeCastClassInstanceToClass(FromResolved,ToResolved,ErrorEl);
          if (Result=cIncompatible) and (FromClassType.ObjKind=ToClassType.ObjKind) then
            begin
            if RaiseOnError then
              WarnClassTypesAreNotRelated(FromClassType,ToClassType);
            Result:=cTypeConversion;
            end;
          end
        else if FromTypeEl.ClassType=TPasGenericTemplateType then
          begin
          // e.g. aClassType(T)
          TemplType:=TPasGenericTemplateType(FromTypeEl);
          if length(TemplType.Constraints)=0 then
            begin
            // typecast unconstrained template to a classtype
            // -> check when specialize
            Result:=cExact;
            end
          else
            for i:=0 to length(TemplType.Constraints)-1 do
              begin
              ConEl:=TemplType.Constraints[i];
              ConToken:=GetGenericConstraintKeyword(ConEl);
              case ConToken of
              tkrecord: ; // invalid type cast
              tkClass, tkconstructor:
                Result:=cExact;
              else
                // identifier constraint: class or interface -> allow
                Result:=cExact;
                break;
              end;
              end;
          end;
        end
      else if FromResolved.BaseType=btPointer then
        begin
        if IsBaseType(FromResolved.LoTypeEl,btPointer) then
          Result:=cExact; // untyped pointer to class instance
        end
      else if FromResolved.BaseType=btNil then
        Result:=cExact; // nil to class or interface
      end
    else if C=TPasGenericTemplateType then
      begin
      // e.g. T(var)
      TemplType:=TPasGenericTemplateType(ToTypeEl);
      FromTypeEl:=FromResolved.LoTypeEl;
      if (FromTypeEl<>nil)
          and (FromTypeEl.ClassType=TPasGenericTemplateType) then
        exit(cExact); // e.g. T(S)  -> will be checked when specialized
      for i:=0 to length(TemplType.Constraints)-1 do
        begin
        ConEl:=TemplType.Constraints[i];
        ConToken:=GetGenericConstraintKeyword(ConEl);
        case ConToken of
        tkrecord:
          if FromResolved.BaseType=btContext then
            begin
            if FromTypeEl.ClassType=TPasRecordType then
              // typecast record to template record
              Result:=cExact
            else if FromTypeEl.ClassType=TPasGenericType then
              // typecast template to template record
              Result:=cExact;
            end;
        tkClass, tkconstructor:
          Result:=cExact;
        else
          // identifier constraint: class or interface -> allow
          Result:=cExact;
          break;
        end;
        end;
      end
    else if C=TPasClassOfType then
      begin
      //writeln('TPasResolver.CheckTypeCast class-of FromRes.TypeEl=',GetObjName(FromResolved.LoTypeEl),' FromRes.IdentEl=',GetObjName(FromResolved.IdentEl));
      if FromResolved.BaseType=btContext then
        begin
        if FromResolved.LoTypeEl.ClassType=TPasClassOfType then
          begin
          if (FromResolved.IdentEl is TPasType) then
            RaiseMsg(20170404162604,nCannotTypecastAType,sCannotTypecastAType,[],ErrorEl);
          // type cast  classof(classof-var)  upwards or downwards
          ToTypeEl:=TPasClassOfType(ToTypeEl).DestType;
          FromTypeEl:=TPasClassOfType(FromResolved.LoTypeEl).DestType;
          Result:=CheckClassesAreRelated(ToTypeEl,FromTypeEl);
          end;
        end
      else if FromResolved.BaseType=btPointer then
        begin
        if IsBaseType(FromResolved.LoTypeEl,btPointer) then
          Result:=cExact; // untyped pointer to class-of
        end
      else if FromResolved.BaseType=btNil then
        Result:=cExact; // nil to class-of
      end
    else if C=TPasRecordType then
      begin
      if FromResolved.BaseType=btContext then
        begin
        if FromResolved.LoTypeEl.ClassType=TPasRecordType then
          begin
          // typecast record to record
          Result:=cExact;
          end
        else if (FromResolved.LoTypeEl is TPasEnumType)
            or (FromResolved.LoTypeEl is TPasRangeType) then
          { An enum or a subrange is an ordinal of a definite size, so it
            reinterprets as a same-size record exactly as a plain integer does:
            cgbase.pas writes `TRegisterRec(result).regtype := rt` over a
            TRegister enum spanning the whole LongInt range. }
          Result:=cCompatible;
        end
      else if FromResolved.BaseType in [btFile,btText] then
        { A Text/File variable IS its control record under the hood; 
          allow to reinterprete it as that record }
        Result:=cExact
      else if FromResolved.BaseType in (btAllInteger+btAllChars+btAllBooleans+btAllFloats) then
        { A scalar reinterpreted as a (same-size) record should be allowed }
        Result:=cCompatible;
      end
    else if (C=TPasEnumType)
        or (C=TPasRangeType) then
      begin
      // Honour RaiseOnError: a caller PROBING compatibility (a descendant that
      // adds cast rules of its own, an overload scorer) must get cIncompatible
      // back, not an exception raised from under it.
      if CheckIsOrdinal(FromResolved,ErrorEl,RaiseOnError) then
        Result:=cExact;
      end
    else if C=TPasSetType then
      begin
      // reinterpret an integer or another set as this set, e.g.
      // TByteSet(LongIntValue) or TByteSet(StringToSet(...))
      if FromResolved.BaseType in (btAllInteger+[btSet,btArrayOrSet]) then
        Result:=cCompatible;
      end
    else if C=TPasArrayType then
      begin
      if FromResolved.BaseType=btContext then
        begin
        if FromResolved.LoTypeEl.ClassType=TPasArrayType then
          Result:=CheckTypeCastArray(TPasArrayType(FromResolved.LoTypeEl),
            TPasArrayType(ToTypeEl),ErrorEl,RaiseOnError)
        else if FromResolved.LoTypeEl.ClassType=TPasGenericTemplateType then
          { T -> array[0..sizeof(T)-1] of byte: a same-size byte reinterpret whose
            concrete size is only known after specialization }
          Result:=cCompatible
        else if (FromResolved.LoTypeEl.ClassType=TPasPointerType)
            and IsDynArray(ToResolved.LoTypeEl) then
          { a TYPED pointer holds the same machine word as a dynamic array's data
            pointer, so it reinterprets like an untyped one (libffi ffi.manager) }
          Result:=cExact;
        end
      else if (GetActualBaseType(FromResolved.BaseType) in [btAnsiString,btRawByteString,btUnicodeString])
          and IsDynArray(ToResolved.LoTypeEl) then
        { a refcounted string reinterprets as a dynamic array of its data:
          lazutils' ReadFileToString passes TBytes(s) to TStream.Read64 }
        Result:=cCompatible
      else if FromResolved.BaseType=btPointer then
        begin
        if IsDynArray(ToResolved.LoTypeEl)
            and IsBaseType(FromResolved.LoTypeEl,btPointer) then
          Result:=cExact; // untyped pointer to dynamic array
        end
      else if FromResolved.BaseType=btNil then
        begin
        if IsDynArray(ToResolved.LoTypeEl) then
          Result:=cExact; // nil to dynamic array
        end;
      end
    else if (C=TPasProcedureType) or (C=TPasFunctionType) then
      begin
      ToProcType:=TPasProcedureType(ToTypeEl);
      if IsBaseType(FromResolved.LoTypeEl,btPointer) then
        begin
        // type cast untyped pointer value to proctype
        if ToProcType.IsOfObject then
          begin
          if proMethodAddrAsPointer in Options then
            Result:=cCompatible
          else if RaiseOnError then
            RaiseMsg(20170416183940,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
              [BaseTypeNames[btPointer],
               ToProcType.ElementTypeName+' '+ProcTypeModifiers[ptmOfObject]],ErrorEl);
          end
        else if ToProcType.IsNested then
          begin
          if RaiseOnError then
            RaiseMsg(20170416184149,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
              [BaseTypeNames[btPointer],
               ToProcType.ElementTypeName+' '+ProcTypeModifiers[ptmIsNested]],ErrorEl);
          end
        else if ToProcType.IsReferenceTo then
          begin
          if proMethodAddrAsPointer in Options then
            Result:=cCompatible
          else if RaiseOnError then
            RaiseMsg(20170419144357,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
              [BaseTypeNames[btPointer],
               ToProcType.ElementTypeName+' '+ProcTypeModifiers[ptmReferenceTo]],ErrorEl);
          end
        else
          Result:=cCompatible;
        end
      else if FromResolved.BaseType=btContext then
        begin
        FromTypeEl:=FromResolved.LoTypeEl;
        if FromTypeEl is TPasProcedureType then
          begin
          // type cast procvar to proctype
          FromProcType:=TPasProcedureType(FromTypeEl);
          if ToProcType.IsReferenceTo then
            Result:=cCompatible
          else if FromProcType.IsReferenceTo then
            Result:=cCompatible
          else if (FromProcType.IsOfObject<>ToProcType.IsOfObject)
              and not (proMethodAddrAsPointer in Options) then
            begin
            if RaiseOnError then
              RaiseMsg(20170416183109,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
                [GetElementTypeName(FromProcType)+BoolToStr(FromProcType.IsOfObject,' '+ProcTypeModifiers[ptmOfObject],''),
                 ToProcType.ElementTypeName+BoolToStr(ToProcType.IsOfObject,' '+ProcTypeModifiers[ptmOfObject],'')],ErrorEl);
            end
          else if FromProcType.IsNested<>ToProcType.IsNested then
            begin
            if RaiseOnError then
              RaiseMsg(20170416183305,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
                [GetElementTypeName(FromProcType)+BoolToStr(FromProcType.IsNested,' '+ProcTypeModifiers[ptmIsNested],''),
                 ToProcType.ElementTypeName+BoolToStr(ToProcType.IsNested,' '+ProcTypeModifiers[ptmIsNested],'')],ErrorEl);
            end
          else
            Result:=cCompatible;
          end
        end
      else if FromResolved.BaseType=btProc then
        begin
        FromTypeEl:=FromResolved.LoTypeEl;
        if FromTypeEl is TPasProcedureType then
          begin
          // typecast procedure (or anonymous procedure) to proctype
          FromProcType:=TPasProcedureType(FromTypeEl);
          if (msDelphi in CurrentParser.CurrentModeswitches)
              and (FromResolved.IdentEl=nil)
              and (FromResolved.LoTypeEl.Name<>'') then
            // Delphi forbids typecast (non anonymous) procedure to proctype
          else if ToProcType.IsReferenceTo then
            Result:=cCompatible
          else if FromResolved.IdentEl=nil then
            // anonymous proc to proctype
            Result:=cCompatible
          else if (FromProcType.IsOfObject<>ToProcType.IsOfObject)
              and not (proMethodAddrAsPointer in Options) then
            begin
            // e.g. TProcedure(Obj.DoIt)
            if RaiseOnError then
              RaiseMsg(20181210151058,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
                [GetElementTypeName(FromProcType)+BoolToStr(FromProcType.IsOfObject,' '+ProcTypeModifiers[ptmOfObject],''),
                 ToProcType.ElementTypeName+BoolToStr(ToProcType.IsOfObject,' '+ProcTypeModifiers[ptmOfObject],'')],ErrorEl);
            end
          else if FromProcType.IsNested<>ToProcType.IsNested then
            begin
            if RaiseOnError then
              RaiseMsg(20181210151102,nIllegalTypeConversionTo,sIllegalTypeConversionTo,
                [GetElementTypeName(FromProcType)+BoolToStr(FromProcType.IsNested,' '+ProcTypeModifiers[ptmIsNested],''),
                 ToProcType.ElementTypeName+BoolToStr(ToProcType.IsNested,' '+ProcTypeModifiers[ptmIsNested],'')],ErrorEl);
            end
          else
            Result:=cCompatible;
          end;
        end
      else if FromResolved.BaseType=btNil then
        // typecast nil to procedure type
        Result:=cExact;
      end
    else if C=TPasPointerType then
      begin
      // typecast to typedpointer
      if FromResolved.BaseType in [btPointer,btNil] then
        Result:=cExact
      else if FromResolved.BaseType in btAllStringAndChars then
        // PChar(string) or PChar(char) cast
        Result:=cCompatible
      else if FromResolved.BaseType in btAllInteger then
        // e.g. PByte(IntegerValue)
        Result:=cCompatible
      else if FromResolved.BaseType=btContext then
        begin
        FromTypeEl:=FromResolved.LoTypeEl;
        C:=FromTypeEl.ClassType;
        if (C=TPasPointerType)
            or (C=TPasClassOfType)
            or (C=TPasClassType)
            or (C.InheritsFrom(TPasProcedureType))
            { An array variable cast to a typed pointer decays to the address of
              its first element, e.g. PSizeInt(StaticSizeIntArray). Dynamic
              arrays already hold that address. }
            or (C=TPasArrayType) then
          Result:=cCompatible;
        end;
      end
    end
  else if ToTypeEl<>nil then
    begin
    // FromResolved is not readable
    if IsGenericTemplType(FromResolved) then
      // A generic TYPE PARAMETER cast as a value: what it stands for is only
      // known at specialization, where the cast is checked again. rtl-generics'
      // TOrdinalComparer<T,THashFactory> writes
      // TExtendedHashFactoryClass(THashFactory).
      Result:=cCompatible
    else if FromResolved.BaseType=btContext then
      begin
      FromTypeEl:=FromResolved.LoTypeEl;
      if (FromTypeEl.ClassType=TPasClassType)
          and (FromTypeEl=FromResolved.IdentEl) then
        begin
        if (ToResolved.BaseType=btContext) then
          begin
          ToTypeEl:=ToResolved.LoTypeEl;
          if (ToTypeEl.ClassType=TPasClassOfType)
              and (ToTypeEl=ToResolved.IdentEl) then
            begin
            // for example  class-of(Self) in a class function
            ToTypeEl:=TPasClassOfType(ToTypeEl).DestType;
            Result:=CheckClassesAreRelated(ToTypeEl,FromTypeEl);
            end
          else if (ToTypeEl.ClassType=TPasPointerType) then
            // class reference (its VMT address) cast to a typed pointer,
            // e.g. PVmt(TFoo) to introspect the class VMT.
            Result:=cCompatible;
          end
        else if (ToResolved.BaseType=btPointer) then
          // class reference cast to the untyped Pointer type.
          Result:=cCompatible
        else if ToResolved.BaseType in btAllInteger then
          // class reference cast to an integer, as fpc allows, e.g. lazarus'
          // PtrUInt(TCodeContextInfoItem)
          Result:=cCompatible;
        end;
      end;
    if (Result=cIncompatible) and RaiseOnError then
      begin
      if FromResolved.IdentEl is TPasType then
        RaiseMsg(20170404162610,nCannotTypecastAType,sCannotTypecastAType,[],ErrorEl);
      end;
    end;

  // Check for an Explicit/Implicit operator backing the cast before raising the
  // error: a class operator declared in the record, OR a free-standing (global)
  // Explicit operator `operator Explicit(a: TRec): TTarget` (e.g. Integer(foo)).
  if (Result=cIncompatible) and (FromResolved.BaseType=btContext)
      and (FromResolved.LoTypeEl<>nil) then
    begin
    if FromResolved.LoTypeEl is TPasRecordType then
      for i:=0 to TPasRecordType(FromResolved.LoTypeEl).Members.Count-1 do
        begin
        ConEl:=TPasElement(TPasRecordType(FromResolved.LoTypeEl).Members[i]);
        if (ConEl is TPasClassOperator) and
           (TPasOperator(ConEl).OperatorType in [otExplicit,otImplicit]) then
          begin
          Result:=cCompatible;
          Break;
          end;
        end;
    // A free-standing `operator Explicit(a: T): U` needs no record on either
    // side, and it is not necessarily declared where T is: rtl-extra's
    // real48utils declares one for the SYSTEM unit's Real48 and casts with it
    // in the same breath. Look where T was declared AND in the unit being
    // compiled.
    if Result=cIncompatible then
      if HasExplicitOperatorFor(FromResolved.LoTypeEl) then
        Result:=cCompatible;
    end;

  // A cast with a type parameter on either side decides nothing here: inside a
  // generic, `_TItem(AItems[i])` casts from the bare generic's T once a partial
  // specialization answers with the generic (PartialSpecsAsGeneric). The
  // specialized copy is cast-checked for real.
  if (Result=cIncompatible)
      and UndecidedPair(FromResolved.LoTypeEl,ToResolved.LoTypeEl) then
    Result:=cCompatible;

  if Result=cIncompatible then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckTypeCastRes From={',GetResolverResultDbg(FromResolved),'} To={',GetResolverResultDbg(ToResolved),'}');
    {$ENDIF}
    if RaiseOnError then
      RaiseIncompatibleTypeRes(20170216152528,nIllegalTypeConversionTo,
        [],FromResolved,ToResolved,ErrorEl);
    exit;
    end;
end;

function TPasResolver.CheckTypeCastArray(FromType, ToType: TPasArrayType;
  ErrorEl: TPasElement; RaiseOnError: boolean): integer;

  function NextDim(var ArrType: TPasArrayType; var NextIndex: integer;
    out ElTypeResolved: TPasResolverResult): boolean;
  begin
    inc(NextIndex);
    if NextIndex<length(ArrType.Ranges) then
      begin
      ElTypeResolved.BaseType:=btNone;
      exit(true);
      end;
    ComputeElement(GetArrayElType(ArrType),ElTypeResolved,[rcType]);
    if (ElTypeResolved.BaseType<>btContext)
        or (ElTypeResolved.LoTypeEl.ClassType<>TPasArrayType) then
      exit(false);
    ArrType:=TPasArrayType(ElTypeResolved.LoTypeEl);
    NextIndex:=0;
    Result:=true;
  end;

var
  FromIndex, ToIndex: Integer;
  FromElTypeRes, ToElTypeRes: TPasResolverResult;
  StartFromType, StartToType: TPasArrayType;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckTypeCastArray From=',GetTypeDescription(FromType),' ToType=',GetTypeDescription(ToType));
  {$ENDIF}
  if not RaiseOnError then
    begin
    if (ToType.GenericTemplateTypes<>nil) and (ToType.GenericTemplateTypes.Count>0) then
      exit(cCompatible); // is later checked when specialized
    end;
  StartFromType:=FromType;
  StartToType:=ToType;
  Result:=cIncompatible;
  // check dimensions
  FromIndex:=0;
  ToIndex:=0;
  repeat
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckTypeCastArray From=',GetTypeDescription(FromType),' FromIndex=',FromIndex,' ToType=',GetTypeDescription(ToType),' ToIndex=',ToIndex);
    {$ENDIF}
    if length(ToType.Ranges)=0 then
      // ToType is dynamic/open array -> fits any size
    else
      begin
      // ToType is ranged
      // ToDo: check size of dimension
      end;
    // check next dimension
    if not NextDim(FromType,FromIndex,FromElTypeRes) then
      begin
      // at end of FromType
      if NextDim(ToType,ToIndex,ToElTypeRes) then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.CheckTypeCastArray To has more dims than From: From=',GetTypeDescription(FromType),' FromIndex=',FromIndex,', ToType=',GetTypeDescription(ToType),' ToIndex=',ToIndex);
        {$ENDIF}
        break; // ToType has more dimensions
        end;
      // have same dimension -> check ElType
      Include(FromElTypeRes.Flags,rrfReadable);
      FromElTypeRes.IdentEl:=nil;
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.CheckTypeCastArray check ElType From=',GetResolverResultDbg(FromElTypeRes),' To=',GetResolverResultDbg(ToElTypeRes));
      {$ENDIF}
      Result:=CheckTypeCastRes(FromElTypeRes,ToElTypeRes,ErrorEl,false);
      break;
      end
    else
      begin
      // FromType has more dimensions
      if not NextDim(ToType,ToIndex,ToElTypeRes) then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.CheckTypeCastArray From has more dims than To: From=',GetTypeDescription(FromType),' FromIndex=',FromIndex,', ToType=',GetTypeDescription(ToType),' ToIndex=',ToIndex);
        {$ENDIF}
        break; // ToType has less dimensions
        end;
      end;
  until false;
  if (Result=cIncompatible) and RaiseOnError then
    RaiseIncompatibleType(20170331124643,nIllegalTypeConversionTo,
      [],StartFromType,StartToType,ErrorEl);
end;
