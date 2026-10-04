{%MainUnit pasresolver.pp}
{ **********************************************************************

    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - Symbol lookup

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }

{ TPasResolver }

// inline
function TPasResolver.GetBaseTypes(bt: TResolverBaseType
  ): TPasUnresolvedSymbolRef;
begin
  Result:=FBaseTypes[bt];
end;

// inline
function TPasResolver.GetScopes(Index: integer): TPasScope;
begin
  Result:=FScopes[Index];
end;

// inline
function TPasResolver.IsNameExpr(El: TPasExpr): boolean;
begin
  Result:=(El.ClassType=TPrimitiveExpr) and (TPrimitiveExpr(El).Kind=pekIdent);
end;

// inline
function TPasResolver.ExprNeedsSpecialization(Expr: TPasExpr): boolean;
// Whether an expression still waits for a type argument: it names something
// that is not bound to a declaration, and it lives inside a generic that has
// not been given its arguments. rtl-generics declares
// `array[0..Pred(TCuckooCfg.D)]`, where TCuckooCfg is a type PARAMETER - the
// bound is evaluated, never resolved, so `Pred` is still a bare name and
// nothing about the bound can be computed until the generic is specialized.

  function HasUnboundName(E: TPasExpr; Depth: integer): boolean;
  var
    i: Integer;
  begin
    Result:=false;
    if (E=nil) or (Depth>20) then exit;
    if (E is TPrimitiveExpr) and (TPrimitiveExpr(E).Kind=pekIdent)
        and not (E.CustomData is TResolvedReference) then
      exit(true);
    if E is TBinaryExpr then
      Result:=HasUnboundName(TBinaryExpr(E).Left,Depth+1)
           or HasUnboundName(TBinaryExpr(E).Right,Depth+1)
    else if E is TUnaryExpr then
      Result:=HasUnboundName(TUnaryExpr(E).Operand,Depth+1)
    else if E is TParamsExpr then
      begin
      Result:=HasUnboundName(TParamsExpr(E).Value,Depth+1);
      for i:=0 to length(TParamsExpr(E).Params)-1 do
        if HasUnboundName(TParamsExpr(E).Params[i],Depth+1) then
          exit(true);
      end;
  end;

var
  Cur: TPasElement;
begin
  Result:=false;
  if not HasUnboundName(Expr,0) then
    exit;
  Cur:=Expr;
  while Cur<>nil do
    begin
    if (Cur is TPasGenericType)
        and (GetTypeParameterCount(TPasGenericType(Cur))>0) then
      exit(true);
    Cur:=Cur.Parent;
    end;
end;

function TPasResolver.IsGenericTemplType(const ResolvedEl: TPasResolverResult
  ): boolean;
begin
  Result:=(ResolvedEl.BaseType=btContext)
      and (ResolvedEl.LoTypeEl.ClassType=TPasGenericTemplateType);
end;

function TPasResolver.IsPartialSpecTypeArg(El, GenEl: TPasElement): boolean;
// True when El is a specialization that took GenEl as a type argument, i.e. a
// PARTIAL one: fcl-stl's gtree declares
// `TTreeNodeList = specialize TVector<TTreeNode>` inside generic TTreeNode, so
// TVector's own `PT = ^T`, `const Value: T` and `Default(T)` all see the bare
// generic. Nothing is instantiated yet - every check runs again on the real
// specialization - so such a reference must not be refused there.
var
  Item: TPRSpecializedItem;
  i: Integer;
begin
  Result:=False;
  if (El=nil) or not (El.CustomData is TPasGenericScope) then
    exit;
  Item:=TPasGenericScope(El.CustomData).SpecializedFromItem;
  if Item=nil then
    exit;
  for i:=0 to Length(Item.Params)-1 do
    if Item.Params[i]=GenEl then
      exit(True);
end;


function TPasResolver.InPartialSpecWithTypeArg(GenEl: TPasElement): boolean;
// IsPartialSpecTypeArg asked of every scope currently open.
var
  i: Integer;
  Scope: TPasScope;
begin
  Result:=False;
  for i:=ScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    if IsPartialSpecTypeArg(Scope.Element,GenEl) then
      exit(True);
    if (Scope is TPasProcedureScope)
        and (TPasProcedureScope(Scope).ClassRecScope<>nil)
        and IsPartialSpecTypeArg(TPasProcedureScope(Scope).ClassRecScope.Element,GenEl) then
      exit(True);
    end;
end;


function TPasResolver.DerefPointerToArray(var R: TPasResolverResult;
  PosEl: TPasElement): boolean;
// Delphi reads `P[i]` on a pointer to an ARRAY as `P^[i]` - the same implicit
// dereference `P.Field` already gets from msAutoDeref. regexpr allocates
// `Stack: ^TStackArray` with GetMem and indexes it directly.
var
  PtrType: TPasPointerType;
  DestType: TPasType;
begin
  Result:=false;
  if (R.BaseType<>btContext) or (R.LoTypeEl=nil)
      or (R.LoTypeEl.ClassType<>TPasPointerType) then
    exit;
  if not ElHasModeSwitch(PosEl,msAutoDeref) then
    exit;
  PtrType:=TPasPointerType(R.LoTypeEl);
  DestType:=ResolveAliasType(PtrType.DestType);
  if not (DestType is TPasArrayType) then
    exit;
  ComputeElement(PtrType.DestType,R,[rcType],PosEl);
  R.IdentEl:=nil;
  R.ExprEl:=nil;
  R.Flags:=R.Flags+[rrfReadable,rrfWritable];
  Result:=true;
end;


function TPasResolver.IsDeferredTemplMember(Expr: TPasExpr): boolean;
// True when Expr is the member name in T.Member and T is a generic template
// type that is not bound yet. ResolveSubIdent leaves those unresolved on
// purpose, so every consumer of the value must defer to specialization too.
var
  ResolvedEl: TPasResolverResult;
begin
  Result:=false;
  if (Expr=nil) or (Expr.CustomData is TResolvedReference) then exit;
  if not ((Expr.Parent is TBinaryExpr)
      and (TBinaryExpr(Expr.Parent).OpCode=eopSubIdent)
      and (TBinaryExpr(Expr.Parent).right=Expr)) then exit;
  ComputeElement(TBinaryExpr(Expr.Parent).left,ResolvedEl,[]);
  Result:=IsGenericTemplType(ResolvedEl) or (ClassOfTemplType(ResolvedEl)<>nil)
    or ((ResolvedEl.LoTypeEl is TPasRecordType)
        and IsRecordComposingTemplate(TPasRecordType(ResolvedEl.LoTypeEl)));
end;

function TPasResolver.IsRecordComposingTemplate(El: TPasRecordType): boolean;
begin
  Result:=(El.CustomData is TPasRecordScope)
      and (TPasRecordScope(El.CustomData).ComposedTemplate<>nil);
end;

function TPasResolver.ClassOfTemplType(const ResolvedEl: TPasResolverResult
  ): TPasGenericTemplateType;
// The type parameter T when ResolvedEl is a `class of T`, else nil.
var
  DestEl: TPasType;
begin
  Result:=nil;
  if not (ResolvedEl.LoTypeEl is TPasClassOfType) then exit;
  DestEl:=ResolveAliasType(TPasClassOfType(ResolvedEl.LoTypeEl).DestType);
  if DestEl is TPasGenericTemplateType then
    Result:=TPasGenericTemplateType(DestEl);
end;


// inline
function TPasResolver.GetLocalScope: TPasScope;
begin
  Result:=TopScope;
  if Result.ClassType=TPasGroupScope then
    Result:=TPasGroupScope(Result).Scopes[0];
end;

// inline
function TPasResolver.GetParentLocalScope: TPasScope;
begin
  Result:=Scopes[ScopeCount-2];
  if Result.ClassType=TPasGroupScope then
    Result:=TPasGroupScope(Result).Scopes[0];
end;

function TPasResolver.GetNameExprValue(El: TPasExpr): string;
begin
  if El=nil then
    Result:=''
  else if El.ClassType=TPrimitiveExpr then
    begin
    if TPrimitiveExpr(El).Kind=pekIdent then
      Result:=TPrimitiveExpr(El).Value
    else
      Result:='';
    end
  else
    Result:='';
end;

function TPasResolver.GetNextDottedExpr(El: TPasExpr): TPasExpr;
// returns TPrimitiveExpr (Kind=pekIdent)
var
  Bin: TBinaryExpr;
  C: TClass;
begin
  Result:=nil;
  if El=nil then exit;
  repeat
    if not (El.Parent is TBinaryExpr) then exit;
    Bin:=TBinaryExpr(El.Parent);
    if Bin.OpCode<>eopSubIdent then exit;
    if El=Bin.Right then
      El:=Bin
    else
      begin
      El:=Bin.Right;
      // find left most
      repeat
        C:=El.ClassType;
        if C=TPrimitiveExpr then
          begin
          if TPrimitiveExpr(El).Kind<>pekIdent then
            RaiseNotYetImplemented(20170502163825,El);
          exit(El);
          end
        else if C=TBinaryExpr then
          begin
          if TBinaryExpr(El).OpCode<>eopSubIdent then
            RaiseNotYetImplemented(20170502163718,El);
          El:=TBinaryExpr(El).Left;
          end
        else if C=TParamsExpr then
          begin
          if not (TParamsExpr(El).Kind in [pekFuncParams,pekArrayParams]) then
            RaiseNotYetImplemented(20170502163908,El);
          El:=TParamsExpr(El).Value;
          end
        else if C=TInlineSpecializeExpr then
          // e.g. Unit.specialize TGen<T>.Member — descend into the generic
          // name so the leftmost identifier (the unit qualifier) is reached.
          El:=TInlineSpecializeExpr(El).NameExpr
        else
          // Unknown node class: bail out rather than spin forever (the loop
          // body must always make progress or this becomes an infinite loop).
          RaiseNotYetImplemented(20260604120000,El);
      until El=nil;
      RaiseNotYetImplemented(20170502163953,Bin);
      end;
  until false;
end;

function TPasResolver.GetLeftMostExpr(El: TPasExpr): TPasExpr;
var
  C: TClass;
begin
  Result:=El;
  while Result<>nil do
    begin
    El:=Result;
    C:=Result.ClassType;
    if C=TBinaryExpr then
      begin
      if TBinaryExpr(Result).OpCode<>eopSubIdent then
        exit;
      Result:=TBinaryExpr(Result).Left;
      end
    else if C=TParamsExpr then
      begin
      if not (TParamsExpr(Result).Kind in [pekFuncParams,pekArrayParams]) then
        exit;
      Result:=TParamsExpr(Result).Value;
      end
    else
      exit;
    end;
end;

function TPasResolver.GetRightMostExpr(El: TPasExpr): TPasExpr;
var
  C: TClass;
begin
  Result:=El;
  while Result<>nil do
    begin
    El:=Result;
    C:=Result.ClassType;
    if C=TBinaryExpr then
      begin
      if TBinaryExpr(Result).OpCode<>eopSubIdent then
        exit;
      Result:=TBinaryExpr(Result).Right;
      end
    else
      exit;
    end;
end;

procedure TPasResolver.GetParamsOfNameExpr(El: TPasExpr; out
  ParentParams: TPRParentParams);
// Checks if El is the name expression of a call or array access
// For example: a.b.El()  a.El[]
// Note: TPasParser guarantees that there is at most one TBinaryExpr
//       and one TInlineSpecializeExpr between El and TParamsExpr
var
  Parent: TPasElement;
  Bin: TBinaryExpr;
  Params: TParamsExpr;
  InlineSpec: TInlineSpecializeExpr;
begin
  ParentParams.InlineSpec:=nil;
  ParentParams.Params:=nil;
  if not IsNameExpr(El) then exit;
  Parent:=El.Parent;
  if Parent=nil then exit;
  if Parent.ClassType=TInlineSpecializeExpr then
    begin
    InlineSpec:=TInlineSpecializeExpr(Parent);
    if InlineSpec.NameExpr<>El then exit;
    ParentParams.InlineSpec:=InlineSpec;
    El:=InlineSpec;
    Parent:=El.Parent;
    if Parent=nil then exit;
    end;
  if Parent.ClassType=TBinaryExpr then
    begin
    Bin:=TBinaryExpr(Parent);
    if (Bin.OpCode<>eopSubIdent) or (Bin.Right<>El) then
      exit;
    El:=Bin;
    Parent:=El.Parent;
    end;
  if Parent.ClassType<>TParamsExpr then exit;
  Params:=TParamsExpr(Parent);
  if Params.Value<>El then exit;
  if not (Params.Kind in [pekFuncParams,pekArrayParams]) then exit;
  ParentParams.Params:=Params;
end;

function TPasResolver.GetInlineSpecOfNameExpr(El: TPasExpr
  ): TInlineSpecializeExpr;
var
  Parent: TPasElement;
begin
  Result:=nil;
  if not IsNameExpr(El) then exit;
  Parent:=El.Parent;
  if Parent=nil then exit;
  if Parent is TBinaryExpr then
    begin
    if (TBinaryExpr(Parent).OpCode<>eopSubIdent)
        or (TBinaryExpr(Parent).Right<>El) then
      exit;
    El:=TBinaryExpr(Parent); // continue
    Parent:=El.Parent;
    end;
  if Parent.ClassType<>TInlineSpecializeExpr then exit;
  Result:=TInlineSpecializeExpr(Parent);
  if Result.NameExpr<>El then
    Result:=nil;
end;

function TPasResolver.GetUsesUnitInFilename(InFileExpr: TPasExpr): string;
var
  Value: TResEvalValue;
begin
  if not (InFileExpr is TPrimitiveExpr) then
    RaiseXExpectedButYFound(20180221234828,'string literal',GetElementTypeName(InFileExpr),InFileExpr);
  Value:=ExprEvaluator.Eval(TPrimitiveExpr(InFileExpr),[refConst]);
  try
    if (Value=nil) then
      RaiseXExpectedButYFound(20180222000004,'string literal',GetElementTypeName(InFileExpr),InFileExpr);
    case Value.Kind of
    {$ifdef FPC_HAS_CPSTRING}
    revkString:
      Result:=ExprEvaluator.GetUTF8Str(TResEvalString(Value).S,InFileExpr);
    revkUnicodeString:
      Result:=UTF8Encode(TResEvalUTF16(Value).S);
    {$else}
    revkUnicodeString:
      Result:=TResEvalUTF16(Value).S;
    {$endif}
    else
      RaiseXExpectedButYFound(20180222000122,'string literal',Value.AsDebugString,InFileExpr);
    end;
  finally
    ReleaseEvalValue(Value);
  end;
end;

function TPasResolver.GetPathStart(El: TPasExpr): TPasExpr;
// get leftmost name element (e.g. TPrimitiveExpr)
// nil if not found
var
  C: TClass;
begin
  Result:=nil;
  while El<>nil do
    begin
    C:=El.ClassType;
    if C=TPrimitiveExpr then
      exit(El)
    else if C=TBinaryExpr then
      begin
      if TBinaryExpr(El).OpCode=eopSubIdent then
        El:=TBinaryExpr(El).Left
      else
        exit;
      end
    else if C=TParamsExpr then
      El:=TParamsExpr(El).Value
    else
      exit;
    end;
end;

function TPasResolver.GetPathEndIdent(El: TPasExpr; AllowCall: boolean
  ): TPasExpr;
// a -> a
// a.b  -> b
// a.b() -> b
// a()() -> nil
// a[] -> nil
var
  Bin: TBinaryExpr;
begin
  Result:=nil;
  if AllowCall and (El is TParamsExpr) then
    El:=TParamsExpr(El).Value;
  while El is TBinaryExpr do
    begin
    Bin:=TBinaryExpr(El);
    if Bin.OpCode=eopSubIdent then
      El:=Bin.Right
    else
      exit(nil);
    end;
  if (El is TPrimitiveExpr) and (TPrimitiveExpr(El).Kind=pekIdent) then
    Result:=El;
end;

function TPasResolver.GetNewInstanceExpr(El: TPasExpr): TPasExpr;
// if the expression is a constructor newinstance call,
// return the element referring the constructor
// else nil
var
  C: TClass;
begin
  Result:=nil;
  while El<>nil do
    begin
    if (El.CustomData is TResolvedReference)
        and (rrfNewInstance in TResolvedReference(El.CustomData).Flags) then
      exit(El);
    C:=El.ClassType;
    if C=TBinaryExpr then
      begin
      if TBinaryExpr(El).OpCode=eopSubIdent then
        El:=TBinaryExpr(El).Right
      else
        exit;
      end
    else if C=TParamsExpr then
      El:=TParamsExpr(El).Value
    else
      exit;
    end;
end;

procedure TPasResolver.ClearResolveDataList(Kind: TResolveDataListKind);
var
  El: TPasElement;
  RData: TResolveData;
begin
  // clear CustomData
  while FLastCreatedData[Kind]<>nil do
    begin
    RData:=FLastCreatedData[Kind];
    El:=RData.Element;
    El.CustomData:=nil;
    FLastCreatedData[Kind]:=RData.Next;
    RData.Free;
    end;
end;

function TPasResolver.GetBaseTypeNames(bt: TResolverBaseType): string;
begin
  if FBaseTypes[bt]<>nil then
    Result:=FBaseTypes[bt].Name
  else
    Result:=ResBaseTypeNames[bt];
end;

function TPasResolver.GetBuiltInProcs(bp: TResolverBuiltInProc
  ): TResElDataBuiltInProc;
begin
  Result:=FBuiltInProcs[bp];
end;

function TPasResolver.GetMaximizeFPCCompatibility: Boolean;
begin
  Result:=(proMaximizeFPCompatibility in Self.Options);
end;

procedure TPasResolver.SetMaximizeFPCCompatibility(AValue: Boolean);
begin
  if aValue then
    Include(FOptions,proMaximizeFPCompatibility)
  else
    Exclude(FOptions,proMaximizeFPCompatibility);
end;

procedure TPasResolver.SetRootElement(const AValue: TPasModule);
begin
  if FRootElement=AValue then Exit;
  FRootElement:=AValue;
end;

procedure TPasResolver.OnFindFirst_PreferNoParams(El: TPasElement; ElScope,
  StartScope: TPasScope; FindFirstElementData: Pointer; var Abort: boolean);
var
  Data: PPRFindData absolute FindFirstElementData;
  ok: Boolean;
  Proc: TPasProcedure;
  Templates: TFPList;
begin
  ok:=true;
  if WithMemberIsHidden(El,StartScope) then
    exit; // keep searching the enclosing scopes
  if PrivateMemberIsHidden(El,StartScope) then
    exit;
  if (El is TPasProcedure) then
    begin
    Proc:=TPasProcedure(El);
    if Data^.SkipGenerics then
      begin
      Templates:=GetProcTemplateTypes(Proc);
      if (Templates<>nil) and (Templates.Count>0) then
        ok:=false;
      end;
    if ok and ProcNeedsParams(Proc.ProcType) then
      // found a proc, but it needs parameters -> remember the first and continue
      ok:=false;
    end
  else if Data^.SkipGenerics then
    begin
    if El is TPasGenericType then
      begin
      if GetTypeParameterCount(TPasGenericType(El))>0 then
        ok:=false;
      end;
    end;
  if ok and (Data^.Found<>nil) and (ElScope<>Data^.ElScope)
      and (Data^.Found is TPasProcedure)
      and IsSelfRefResultName(Data^.ErrorPosEl,TPasProcedure(Data^.Found)) then
    begin
    // The enclosing function itself was found in a NEARER scope: inside its own
    // body its bare name is its Result, and a same-named identifier further out
    // does not shadow that (ncurses/ocrt: nested "function CP" next to a unit
    // variable "cp"). A same-scope OVERLOAD is a different matter and still wins.
    Abort:=true;
    exit;
    end;
  if ok and (Data^.Found<>nil) and (ElScope=DefaultScope) then
    // a built-in never shadows a declaration of the same name found in a nearer
    // scope, not even when that declaration needs parameters (iso7185.FilePos)
    exit;
  if (Data^.Found is TPasProcedure) and (El is TPasVariable) then
    begin
    // A routine hides a variable, field or property of the same name further
    // out, whether or not it needs parameters: the LCL's `@FForm.FOnClick`, where
    // FOnClick is a method of the form and a private field of TControl, and
    // `@ReleaseGDIObject` in a method next to a global procvar of that name.
    Abort:=true;
    exit;
    end;
  if ok or (Data^.Found=nil) then
    begin
    Data^.Found:=El;
    Data^.ElScope:=ElScope;
    Data^.StartScope:=StartScope;
    end;
  if ok then
    Abort:=true;
end;

procedure TPasResolver.OnFindProcAddrForType(El: TPasElement; ElScope,
  StartScope: TPasScope; FindProcAddrData: Pointer; var Abort: boolean);
// Collect the first overload of a name whose signature is assignment-compatible
// with the target procedure type (used to disambiguate @overloadedProc := procvar).
var
  Data: PPRFindProcAddrData absolute FindProcAddrData;
begin
  if ElScope=nil then ; if StartScope=nil then ;
  if not (El is TPasProcedure) then exit;
  if TPasProcedure(El).ProcType=nil then exit;
  if CheckProcTypeCompatibility(Data^.TargetType,TPasProcedure(El).ProcType,true,nil,false) then
    begin
    Data^.Found:=TPasProcedure(El);
    Abort:=true;
    end;
end;

function TPasResolver.WithMemberIsHidden(El: TPasElement;
  StartScope: TPasScope): boolean;
// A member reached through a `with` that this module may not touch does not hide
// an identifier of the same name further out - FPC keeps searching. fcl-db's
// TBufBlobStream.Create writes `with FDataSet do .. FModified := True`, where
// FModified is private to TDataSet in another unit AND a field of the class
// being compiled. Only PRIVATE members qualify: protected has its own rules
// (descendants, helpers) that CheckFoundElementVisibility judges.
var
  Context: TPasElement;
begin
  Result:=false;
  if not (StartScope is TPasWithExprScope) then exit;
  if not (El is TPasVariable) then exit;
  if not (El.Visibility in [visPrivate,visProtected]) then exit;
  if not (El.Parent is TPasMembersType) then exit;
  Context:=GetVisibilityContext;
  if Context=nil then exit;
  if El.Parent.GetModule=Context.GetModule then exit; // same module -> accessible
  if El.Visibility=visProtected then
    begin
    // Protected stays reachable from a DESCENDANT.
    if (Context is TPasType)
        and (CheckClassIsClass(TPasType(Context),TPasMembersType(El.Parent))<>cIncompatible) then
      exit;
    // ... and only step over it when the name DOES resolve further out. Hiding
    // it unconditionally took accessible members away (bufdataset lost its own
    // BufferCount); requiring an alternative keeps the plain error intact.
    if not NameExistsOutsideWith(El.Name) then
      exit;
    end;
  Result:=true;
end;

function TPasResolver.NameExistsOutsideWith(const AName: String): boolean;
// Is AName declared in a scope that is NOT a `with` (nor the dot-scope a with
// pushes)? That is the identifier FPC lands on when the with's own member is
// out of reach.
var
  i: Integer;
  Scope: TPasScope;
begin
  Result:=false;
  for i:=ScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    if Scope is TPasWithExprScope then continue;
    if Scope is TPasDotClassOrRecordScope then continue;
    if not (Scope is TPasIdentifierScope) then continue;
    if TPasIdentifierScope(Scope).FindElement(AName)<>nil then
      exit(true);
    end;
end;

function TPasResolver.PrivateMemberIsHidden(El: TPasElement;
  StartScope: TPasScope): boolean;
// A private member of a class declared in another unit does not hide an
// identifier of the same name further out: FPC and Delphi keep searching.

  function AncestorHasMember: boolean;
  // Does an ancestor of El's class declare a member of the same name?
  var
    Scope: TPasClassScope;
  begin
    Result:=false;
    if not (El.Parent.CustomData is TPasClassScope) then exit;
    Scope:=TPasClassScope(El.Parent.CustomData).AncestorScope;
    while Scope<>nil do
      begin
      if Scope.FindLocalIdentifier(El.Name)<>nil then
        exit(true);
      Scope:=Scope.AncestorScope;
      end;
  end;

var
  Context: TPasElement;
begin
  Result:=false;
  if FFindingAttributeCreate then exit;
  if StartScope is TPasWithExprScope then exit;
  if not ((El is TPasVariable) or (El is TPasProcedure)) then exit;
  if El.Visibility<>visPrivate then exit;
  if not (El.Parent is TPasMembersType) then exit;
  Context:=GetVisibilityContext;
  if Context=nil then exit;
  if El.Parent.GetModule=Context.GetModule then exit;
  // Without an alternative the member stays found and gets the usual error.
  // The search goes on in the ancestors, or further out for a bare name.
  if AncestorHasMember then
    exit(true);
  if (El is TPasVariable) and not (StartScope is TPasDotBaseScope) then
    Result:=NameExistsOutsideMembers(El);
end;

function TPasResolver.NameExistsOutsideMembers(El: TPasElement): boolean;
// Is El's name declared in a scope that is not a class, record or with scope?
var
  i: Integer;
  Scope: TPasScope;
  Found: TPasElement;
  Item: TPasIdentifier;
begin
  Result:=false;
  for i:=ScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    if Scope is TPasWithExprScope then continue;
    if Scope is TPasDotClassOrRecordScope then continue;
    if not (Scope is TPasIdentifierScope) then continue;
    if Scope.Element is TPasMembersType then continue;
    if Scope is TPasProcedureScope then
      begin
      // Only the routine's own identifiers, not those of its class.
      Item:=TPasIdentifierScope(Scope).FindLocalIdentifier(El.Name);
      if Item<>nil then
        Found:=Item.Element
      else
        Found:=nil;
      end
    else
      Found:=TPasIdentifierScope(Scope).FindElement(El.Name);
    if (Found<>nil) and (Found<>El) then
      exit(true);
    end;
end;

procedure TPasResolver.OnFindFirst(El: TPasElement; ElScope,
  StartScope: TPasScope; FindFirstElementData: Pointer; var Abort: boolean);
var
  Data: PPRFindData absolute FindFirstElementData;
begin
  if WithMemberIsHidden(El,StartScope) then
    exit; // keep searching the enclosing scopes
  if PrivateMemberIsHidden(El,StartScope) then
    exit;
  Data^.Found:=El;
  Data^.ElScope:=ElScope;
  Data^.StartScope:=StartScope;
  Abort:=true;
end;

procedure TPasResolver.OnFindFirst_GenericEl(El: TPasElement; ElScope,
  StartScope: TPasScope; FindFirstGenericData: Pointer; var Abort: boolean);
var
  Data: PPRFindGenericData absolute FindFirstGenericData;
  GenericTemplateTypes: TFPList;
  Proc: TPasProcedure;
  ProcScope: TPasProcedureScope;
begin
  Proc:=nil;
  if El is TPasGenericType then
    GenericTemplateTypes:=TPasGenericType(El).GenericTemplateTypes
  else if El is TPasProcedure then
    begin
    Proc:=TPasProcedure(El);
    ProcScope:=Proc.CustomData as TPasProcedureScope;
    if ProcScope.DeclarationProc<>nil then
      begin
      // this proc has a forward declaration -> use that instead
      Proc:=ProcScope.DeclarationProc;
      El:=Proc;
      end;

    if (Data^.LastProc<>nil) then
      begin
      if not IsProcOverloading(Data^.LastProc,Proc) then
        begin
        Abort:=true;
        exit;
        end;
      end;
    Data^.LastProc:=Proc;

    GenericTemplateTypes:=GetProcTemplateTypes(Proc);
    end
  else
    exit;

  if GenericTemplateTypes=nil then exit;
  if GenericTemplateTypes.Count<>Data^.TemplateCount then
    exit;

  if Data^.Find.Found<>nil then
    begin
    // there was already a generic proc, but it needed params
    if ProcNeedsParams(Proc.ProcType) then
      begin
      // this one needs params too
      // -> keep the first found and continue searching
      exit;
      end;
    end;

  Data^.Find.Found:=El;
  Data^.Find.ElScope:=ElScope;
  Data^.Find.StartScope:=StartScope;

  if Proc<>nil then
    begin
    if (msDelphi in ProcScope.ModeSwitches) and (not IsProcOverload(Proc)) then
      // stop searching after this proc
    else if ProcNeedsParams(Proc.ProcType) then
      begin
      // continue searching for an overload proc without params
      exit;
      end;
    end;

  Abort:=true;
end;

function TPasResolver.IsVariantOverloadAmbiguous(CandA, CandB: TPasElement;
  Params: TParamsExpr): Boolean;
// Check if two overload candidates should be treated as ambiguous when
// the argument is a Variant. FPC considers certain type pairs ambiguous
// even when graduated scoring would pick a winner:
// 1. Boolean vs any non-boolean numeric type (tiers 1-4)
// 2. Unsigned integer vs signed integer where unsigned width >= signed width
// 3. Untyped var/out vs numeric/float/boolean type
var
  ProcA, ProcB: TPasProcedure;
  ParamA, ParamB: TPasArgument;
  TypeResA, TypeResB, ExprRes: TPasResolverResult;
  BTA, BTB: TResolverBaseType;

  function GetUnsignedWidth(BT: TResolverBaseType): Integer;
  begin
    case BT of
      btByte: Result := 8;
      btWord: Result := 16;
      btLongWord: Result := 32;
      {$IFDEF HasInt64}
      btQWord: Result := 64;
      {$ENDIF}
    else Result := 0;
    end;
  end;

  function GetSignedWidth(BT: TResolverBaseType): Integer;
  begin
    case BT of
      btShortInt: Result := 8;
      btSmallInt: Result := 16;
      btLongint: Result := 32;
      {$IFDEF HasInt64}
      btInt64: Result := 64;
      {$ENDIF}
    else Result := 0;
    end;
  end;

var
  UnsignedW, SignedW: Integer;
begin
  Result := False;
  // Variant overload ambiguity only applies in Delphi mode.
  if not (msDelphi in CurrentParser.CurrentModeswitches) then Exit;
  // Only for single-argument calls.
  if (Params = nil) or (Length(Params.Params) <> 1) then Exit;
  if not (CandA is TPasProcedure) or not (CandB is TPasProcedure) then Exit;
  ProcA := TPasProcedure(CandA);
  ProcB := TPasProcedure(CandB);
  if (ProcA.ProcType.Args.Count <> 1) or (ProcB.ProcType.Args.Count <> 1) then Exit;
  ParamA := TPasArgument(ProcA.ProcType.Args[0]);
  ParamB := TPasArgument(ProcB.ProcType.Args[0]);
  // The argument must be a variant (the expensive check).
  ComputeElement(TPasExpr(Params.Params[0]), ExprRes, []);
  {$IFNDEF PAS2JS}
  if ExprRes.BaseType <> btVariant then Exit;
  {$ENDIF}
  // Get param base types
  if (ParamA.ArgType = nil) and (ParamA.Access in [argVar, argOut]) then
    BTA := btUntyped
  else if ParamA.ArgType <> nil then begin
    ComputeElement(ParamA.ArgType, TypeResA, []);
    BTA := TypeResA.BaseType;
  end
  else
    Exit;
  if (ParamB.ArgType = nil) and (ParamB.Access in [argVar, argOut]) then
    BTB := btUntyped
  else if ParamB.ArgType <> nil then begin
    ComputeElement(ParamB.ArgType, TypeResB, []);
    BTB := TypeResB.BaseType;
  end
  else
    Exit;
  // Rule 1: Boolean vs numeric core types (tiers 1-4) -> ambiguous.
  if (BTA in btAllBooleans) and (BTB in [btByte, btShortInt, btWord, btSmallInt,
      btLongWord, btLongint, btSingle]) then
    Exit(True);
  if (BTB in btAllBooleans) and (BTA in [btByte, btShortInt, btWord, btSmallInt,
      btLongWord, btLongint, btSingle]) then
    Exit(True);
  // Rule 2: Unsigned vs signed integer, unsigned width >= signed width -> ambiguous.
  UnsignedW := GetUnsignedWidth(BTA);
  SignedW := GetSignedWidth(BTB);
  if (UnsignedW > 0) and (SignedW > 0) and (UnsignedW >= SignedW) then
    Exit(True);
  UnsignedW := GetUnsignedWidth(BTB);
  SignedW := GetSignedWidth(BTA);
  if (UnsignedW > 0) and (SignedW > 0) and (UnsignedW >= SignedW) then
    Exit(True);
  // Rule 3: Untyped var/out vs numeric types (tiers 1-5) -> ambiguous.
  if (BTA = btUntyped) and (BTB in [btByte, btShortInt, btWord, btSmallInt,
      btLongWord, btLongint, btSingle, btDouble, btCurrency,{$IFDEF HasInt64} btComp,{$ENDIF} btExtended,
      btBoolean, btByteBool, btWordBool, btLongBool {$IFDEF HasInt64}, btQWordBool{$endif}]) then
    Exit(True);
  if (BTB = btUntyped) and (BTA in [btByte, btShortInt, btWord, btSmallInt,
      btLongWord, btLongint, btSingle, btDouble, btCurrency,{$IFDEF HasInt64} btComp,{$ENDIF} btExtended,
      btBoolean, btByteBool, btWordBool, btLongBool {$IFDEF HasInt64}, btQWordBool{$ENDIF}]) then
    Exit(True);
end;

procedure TPasResolver.OnFindCallElements(El: TPasElement; ElScope,
  StartScope: TPasScope; FindCallElData: Pointer; var Abort: boolean);
var
  Data: PFindCallElData absolute FindCallElData;
  Proc, PrevProc: TPasProcedure;
  Distance: integer;
  BuiltInProc: TResElDataBuiltInProc;
  CandidateFound: Boolean;
  VarType, TypeEl: TPasType;
  C: TClass;
  ProcScope: TPasProcedureScope;
  Templates: TFPList;
  MinArgs: Integer;
  InvokeProcType: TPasProcedureType;
  SrcFam, ElFam, FoundFam, ArgFamIdx: Integer;
  ElBase, FoundBase: TResolverBaseType;


  function BTStrFamily(bt: TResolverBaseType): Integer;
  // -1 = ansi-family string/char, +1 = unicode-family, 0 = neither
  begin
    case GetActualBaseType(bt) of
    btAnsiString,btShortString,btRawByteString,btAnsiChar: Result:=-1;
    btUnicodeString,btWideString,btWideChar: Result:=1;
    else Result:=0;
    end;
  end;

  function ExprStrFamily(E: TPasExpr): Integer;
  var R: TPasResolverResult;
  begin
    Result:=0;
    if E=nil then exit;
    ComputeElement(E,R,[]);
    Result:=BTStrFamily(R.BaseType);
  end;

  function FirstParamFamily(P: TPasElement): Integer;
  var R: TPasResolverResult;
  begin
    Result:=0;
    if (P is TPasProcedure)
        and (TPasProcedure(P).ProcType.Args.Count>0)
        and (TPasArgument(TPasProcedure(P).ProcType.Args[0]).ArgType<>nil) then
      begin
      ComputeElement(TPasArgument(TPasProcedure(P).ProcType.Args[0]).ArgType,R,[rcType]);
      Result:=BTStrFamily(R.BaseType);
      end;
  end;

  function ParamFamilyAt(P: TPasElement; Idx: Integer): Integer;
  // The string family of parameter Idx, or 0 when there is none.
  var R: TPasResolverResult;
  begin
    Result:=0;
    if (P is TPasProcedure)
        and (TPasProcedure(P).ProcType.Args.Count>Idx)
        and (TPasArgument(TPasProcedure(P).ProcType.Args[Idx]).ArgType<>nil) then
      begin
      ComputeElement(TPasArgument(TPasProcedure(P).ProcType.Args[Idx]).ArgType,R,[rcType]);
      Result:=BTStrFamily(R.BaseType);
      end;
  end;

  function FirstParamCharPtrFamily(P: TPasElement): Integer;
  // 1 for a PAnsiChar-like first parameter, -1 for PWideChar-like, else 0.
  var
    R: TPasResolverResult;
    T: TPasType;
  begin
    Result:=0;
    if not ((P is TPasProcedure) and (TPasProcedure(P).ProcType.Args.Count>0)) then exit;
    T:=TPasArgument(TPasProcedure(P).ProcType.Args[0]).ArgType;
    if T=nil then exit;
    T:=ResolveAliasType(T);
    if not (T is TPasPointerType) then exit;
    if TPasPointerType(T).DestType=nil then exit;
    ComputeElement(TPasPointerType(T).DestType,R,[rcType]);
    case GetActualBaseType(R.BaseType) of
      btAnsiChar: Result:=1;
      btWideChar: Result:=-1;
    end;
  end;

  function ArgIsUntypedPointer(E: TPasExpr): Boolean;
  var
    R: TPasResolverResult;
  begin
    Result:=false;
    if E=nil then exit;
    ComputeElement(E,R,[]);
    Result:=(R.BaseType=btPointer)
            and ((R.LoTypeEl=nil) or IsBaseType(R.LoTypeEl,btPointer));
  end;

  function FirstParamStrBase(P: TPasElement): TResolverBaseType;
  var R: TPasResolverResult;
  begin
    Result:=btNone;
    if (P is TPasProcedure)
        and (TPasProcedure(P).ProcType.Args.Count>0)
        and (TPasArgument(TPasProcedure(P).ProcType.Args[0]).ArgType<>nil) then
      begin
      ComputeElement(TPasArgument(TPasProcedure(P).ProcType.Args[0]).ArgType,R,[rcType]);
      Result:=GetActualBaseType(R.BaseType);
      end;
  end;

  // Tie-break of two equally good candidates: ppcx64 ranks an integer passed to a
  // SINGLE better than to a double or extended (defcmp: te_convert_l3 vs l4), so
  // Ldexp(1, n) picks the single overload. 1 when P1 wins, -1 when P2 wins, 0
  // when they differ in any other way.
  function IntToSingleTie(P1, P2: TPasElement): Integer;
  var
    a, ArgCount: Integer;
    R1, R2, RA: TPasResolverResult;
    B1, B2: TResolverBaseType;
  begin
    Result:=0;
    if not (P1 is TPasProcedure) or not (P2 is TPasProcedure)
        or (Data^.Params=nil) then
      exit;
    ArgCount:=length(Data^.Params.Params);
    if (TPasProcedure(P1).ProcType.Args.Count<ArgCount)
        or (TPasProcedure(P2).ProcType.Args.Count<ArgCount) then
      exit;
    for a:=0 to ArgCount-1 do
      begin
      if (TPasArgument(TPasProcedure(P1).ProcType.Args[a]).ArgType=nil)
          or (TPasArgument(TPasProcedure(P2).ProcType.Args[a]).ArgType=nil) then
        exit(0);
      ComputeElement(TPasArgument(TPasProcedure(P1).ProcType.Args[a]).ArgType,R1,[rcType]);
      ComputeElement(TPasArgument(TPasProcedure(P2).ProcType.Args[a]).ArgType,R2,[rcType]);
      B1:=GetActualBaseType(R1.BaseType);
      B2:=GetActualBaseType(R2.BaseType);
      if (B1=B2) and (R1.LoTypeEl=R2.LoTypeEl) then
        continue;
      if not ((B1 in btAllFloats) and (B2 in btAllFloats)) then
        exit(0);
      ComputeElement(Data^.Params.Params[a],RA,[rcNoImplicitProcType]);
      if not (RA.BaseType in btAllInteger) then
        exit(0);
      if (B1=btSingle) and (B2<>btSingle) then
        begin
        if Result<0 then exit(0);
        Result:=1;
        end
      else if (B2=btSingle) and (B1<>btSingle) then
        begin
        if Result>0 then exit(0);
        Result:=-1;
        end
      else
        exit(0);
      end;
  end;

begin

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnFindCallElements START --------- ',GetObjName(El),' at ',GetElementSourcePosStr(El));
  {$ENDIF}
  CandidateFound:=false;

  if (El is TPasProcedure) then
    begin
    // identifier is a proc
    Proc:=TPasProcedure(El);
    PrevProc:=nil;

    // Symbolic operators are invoked through operator syntax (e.g. a ** b), never
    // by their internal name ("power" for **). Exclude those from by-name call
    // resolution so a call like power(a,b) unambiguously selects the like-named
    // function. Inc/Dec operators are the exception: they overload the Inc/Dec
    // standard procedures and ARE invoked by name (inc(x)/dec(x), toperator11).
    if (Proc is TPasOperator)
        and not (TPasOperator(Proc).OperatorType in [otInc, otDec]) then
      exit;

    if Data^.Found=Proc then
      begin
      // this proc was already found. This happens when this is the forward
      // declaration or a previously found implementation.
      exit;
      end;

    ProcScope:=Proc.CustomData as TPasProcedureScope;
    if ProcScope.DeclarationProc<>nil then
      begin
      // this proc has a forward declaration -> use that instead
      Proc:=ProcScope.DeclarationProc;
      El:=Proc;
      end;

    if (msDelphi in ProcScope.ModeSwitches) and not IsProcOverload(Proc)
        and not ProcHasGroupOverload(Proc)
        and not IsHelperMethodOverloadGroup(Data^.LastProc,Proc)
        and not IsDefinitelyInaccessible(Proc) then
      begin
      // A generic proc implicitly overloaded via implicitfunctionspecialization
      // (ProcHasGroupOverload) and an "overload" helper method both keep earlier
      // same-named procs as candidates rather than hiding them (timpfuncspez35, tchlp33).
      Abort:=true; // stop searching after this proc
      if Data^.LastProc<>nil then
        exit;
      end;

    if (Data^.Found is TPasProcedure) and IsProcOverride(Proc,TPasProcedure(Data^.Found)) then
      begin
      // already checked the override -> skip
      Data^.LastProc:=Proc;
      exit;
      end;

    if (Data^.LastProc<>nil) then
      begin
      // A member the call site cannot reach at all does not hide a same-named
      // declaration in an outer scope, so it must not stop the search either:
      // a private SetString in another unit's ancestor left the system SetString
      // unseen (gnutlssockets.pp).
      if not IsProcOverloading(Data^.LastProc,Proc)
          and not IsHelperMethodOverloadGroup(Data^.LastProc,Proc)
          and not IsDefinitelyInaccessible(Data^.LastProc) then
        begin
        Abort:=true;
        exit;
        end;
      end;

    if Data^.Found is TPasProcedure then
      begin
      // there is already a previous proc
      PrevProc:=TPasProcedure(Data^.Found);

      if IsProcOverride(Proc,TPasProcedureScope(PrevProc.CustomData).OverriddenProc) then
        begin
        // already checked the override -> skip
        Data^.LastProc:=Proc;
        exit;
        end;

      if (Data^.Distance=cExact) and (PrevProc.Parent<>Proc.Parent)
          and (PrevProc.Parent.ClassType=TPasClassType) then
        begin
        // there was already a perfect proc in a descendant
        Abort:=true;
        exit;
        end;
      end;

    CandidateFound:=true;
    if Data^.TemplCnt>0 then
      begin
      // proc must have templates
      Templates:=GetProcTemplateTypes(Proc);
      if (Templates=nil) or (Templates.Count<>Data^.TemplCnt) then
        Distance:=cIncompatible
      else
        begin
        Distance:=CheckCallProcCompatibility(Proc.ProcType,Data^.Params,false);
        // Explicit specialization (specialize Proc<...>(...)): the generic
        // signature scores a bare template param "const T" as a catch-all
        // (cGenericExact), so an array/set literal wrongly ties "const T" with
        // "const array of T". Strengthen the check by verifying each bare
        // template param accepts its argument under the explicit substitution.
        // This is a DRY check (no GetSpecializedEl / persisted specialization),
        // so it does not emit dead specialization code (pas2js-safe).
        if (Distance<>cIncompatible) and (Data^.TemplParams<>nil)
            and not CheckExplicitSpecBareTemplateArgs(Proc,Data^.TemplParams,Data^.Params) then
          Distance:=cIncompatible;
        end;
      end
    else
      begin
      Templates:=GetProcTemplateTypes(Proc);
      if (Templates<>nil) and (Templates.Count>0)
          and (msImplicitFunctionSpec in CurrentParser.CurrentModeswitches)
          and UseTentativeImplicitSpecMatch then
        begin
        // implicit function specialization (GenericProc(args) with no explicit
        // <...>): a native/FPC target accepts the candidate as a TENTATIVE match —
        // the template params can only be scored once inferred, which happens
        // later in ResolveFuncParamsExprName. Only filter by arg count here
        // (timpfuncspez4/20). Base keeps the full type check so testpassrc's
        // multi-overload disambiguation is unaffected (TestGenProc_Infer_Overload).
        Distance:=cGenericExact;
        if length(Data^.Params.Params)>Proc.ProcType.Args.Count then
          Distance:=cIncompatible // too many args
        else if not (ptmVarargs in Proc.ProcType.Modifiers) then
          begin
          MinArgs:=Proc.ProcType.Args.Count;
          while (MinArgs>0) and (TPasArgument(Proc.ProcType.Args[MinArgs-1]).ValueExpr<>nil) do
            dec(MinArgs);
          if length(Data^.Params.Params)<MinArgs then
            Distance:=cIncompatible; // too few args
          end;
        end
      else if (Templates<>nil) and (Templates.Count>0)
          and not (msImplicitFunctionSpec in CurrentParser.CurrentModeswitches) then
        // A generic proc called without <...> and without implicit function
        // specialization cannot be used, so it must not beat a plain overload:
        // sysutils' generic IfThen<T> hid strutils' IfThen(Boolean;string;string).
        Distance:=cIncompatible
      else
        Distance:=CheckCallProcCompatibility(Proc.ProcType,Data^.Params,false);
      end;

    // An overload the caller cannot reach must lose to one it can: classes'
    // TMemoryStream republishes only the Int64 SetSize as public, so
    // `Stream.SetSize(1)` from another unit has to pick that one rather than
    // TStream's protected LongInt overload (fpmkunit, fcl-res/strtable).
    if (Distance<cInaccessibleMember) and IsDefinitelyInaccessible(Proc) then
      Distance:=Distance+cInaccessibleMember;
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallElements Proc Distance=',Distance,
      ' Data^.Found=',Data^.Found<>nil,' Data^.Distance=',Data^.Distance,
      ' Signature={',GetProcTypeDescription(Proc.ProcType,[prptdUseName,prptdAddPaths]),'}',
      ' Abort=',Abort);
    {$ENDIF}
    Data^.LastProc:=Proc;
    end
  else if El is TPasType then
    begin
    TypeEl:=ResolveAliasType(TPasType(El));
    C:=TypeEl.ClassType;
    if Data^.TemplCnt<>0 then
      begin
      if (not C.InheritsFrom(TPasGenericType))
          or (GetTypeParameterCount(TPasGenericType(TypeEl))<>Data^.TemplCnt)
      then
        exit;
      end;

    if C=TPasUnresolvedSymbolRef then
      begin
      if TypeEl.CustomData.ClassType=TResElDataBuiltInProc then
        begin
        // call of built-in proc
        BuiltInProc:=TResElDataBuiltInProc(TypeEl.CustomData);

        // A non-overloaded user routine of the same name hides the built-in
        // intrinsic, mirroring the proc-vs-proc hiding rule above (FPC: a user
        // Write/Read/etc. declared without 'overload' shadows the system
        // intrinsic instead of tying with it). tforin26/tforin27.
        // A user METHOD (class/record member) hides the intrinsic even when it
        // is marked 'overload': it lives in an inner scope (the class), and
        // 'overload' only groups its same-named siblings, it does not merge with
        // the system intrinsic from the outermost scope. Without this, a method
        // like TStream.Read(var Buffer; Count) whose untyped-var overload scores
        // worse than the intrinsic would lose to the system Read (streams.inc).

        // ...but only while some same-named member actually MATCHES the call.
        // When every one of them was incompatible there is nothing to hide
        // behind, and the intrinsic is what the call meant: fcl-css's tests say
        // Insert(x,DynArray,i) inside a TComponent descendant, where the
        // inherited Insert(AComponent) takes ONE argument. Judging this on the
        // single LastProc instead (by argument count) breaks the streams.inc
        // case above - LastProc is only ONE member of the overload group, and
        // there the matching sibling is a different one.
        if (Data^.LastProc<>nil)
            and (not IsProcOverload(Data^.LastProc)
                 or (Data^.LastProc.Parent is TPasMembersType))
            and (Data^.Found<>nil) and (Data^.Distance<cIncompatible) then
          begin
          Abort:=true;
          exit;
          end;
        if (BuiltInProc.BuiltIn in [bfStrProc,bfStrFunc])
            and ((BuiltInProc.BuiltIn=bfStrProc) = ParentNeedsExprResult(Data^.Params)) then
          begin
          // str function can only be used within an expression
          // str procedure can only be used outside an expression
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.OnFindCallElements BuiltInProc=',El.Name,' skip');
          {$ENDIF}
          exit;
          end;
        Distance:=BuiltInProc.GetCallCompatibility(BuiltInProc,Data^.Params,false);
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.OnFindCallElements BuiltInProc=',El.Name,' Distance=',Distance);
        {$ENDIF}
        CandidateFound:=true;
        end
      else if TypeEl.CustomData is TResElDataBaseType then
        begin
        // type cast to base type
        Abort:=true; // can't be overloaded
        if Data^.Found<>nil then exit;
        Distance:=CheckTypeCast(TPasType(El),Data^.Params,false);
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.OnFindCallElements Base type cast=',El.Name,' Distance=',Distance);
        {$ENDIF}
        CandidateFound:=true;
        end;
      end
    else if (C=TPasClassType)
        or (C=TPasClassOfType)
        or (C=TPasPointerType)
        or (C=TPasRecordType)
        or (C=TPasEnumType)
        or (C=TPasSetType)
        or (C=TPasProcedureType)
        or (C=TPasFunctionType)
        or (C=TPasArrayType)
        or (C=TPasRangeType)
        or (C=TPasStringType)
        or (C=TPasGenericTemplateType) then
      begin
      // type cast to user type (incl. codepage string types)
      Abort:=true; // can't be overloaded
      if Data^.Found<>nil then exit;
      Distance:=CheckTypeCast(TPasType(El),Data^.Params,false);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements type cast to "',GetObjName(El),'" Distance=',Distance);
      {$ENDIF}
      CandidateFound:=true;
      end;
    end
  else if El is TPasVariable then
    begin
    Abort:=true; // can't be overloaded
    if Data^.Found<>nil then exit;
    if Data^.TemplCnt<>0 then exit;
    if El.ClassType=TPasProperty then
      VarType:=GetPasPropertyType(TPasProperty(El))
    else
      VarType:=TPasVariable(El).VarType;
    VarType:=ResolveAliasType(VarType);
    if VarType is TPasProcedureType then
      begin
      Distance:=CheckCallProcCompatibility(TPasProcedureType(VarType),Data^.Params,false);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements call var of proctype=',El.Name,' Distance=',Distance);
      {$ENDIF}
      CandidateFound:=true;
      end
    else
      begin
      InvokeProcType:=GetFuncRefInvokeProcType(VarType,length(Data^.Params.Params));
      if InvokeProcType<>nil then
        begin
        Distance:=CheckCallProcCompatibility(InvokeProcType,Data^.Params,false);
        CandidateFound:=true;
        end;
      end;
    end
  else if El.ClassType=TPasArgument then
    begin
    Abort:=true; // can't be overloaded
    if Data^.Found<>nil then exit;
    if Data^.TemplCnt<>0 then exit;
    VarType:=ResolveAliasType(TPasArgument(El).ArgType);
    if VarType is TPasProcedureType then
      begin
      Distance:=CheckCallProcCompatibility(TPasProcedureType(VarType),Data^.Params,false);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements call arg of proctype=',El.Name,' Distance=',Distance);
      {$ENDIF}
      CandidateFound:=true;
      end
    else
      begin
      InvokeProcType:=GetFuncRefInvokeProcType(VarType,length(Data^.Params.Params));
      if InvokeProcType<>nil then
        begin
        Distance:=CheckCallProcCompatibility(InvokeProcType,Data^.Params,false);
        CandidateFound:=true;
        end;
      end;
    end;

  if (not CandidateFound) and (Data^.Found=nil)
      and (GetTypeOfCastRoot(Data^.Params)<>nil)
      and (GetTypeOfCastValueType(El)<>nil) then
    begin
    // "type of Value(Param)" -> typecast, see ResolveFuncParamsExprName
    Abort:=true;
    Distance:=cIncompatible;
    CandidateFound:=true;
    end;

  if not CandidateFound then
    begin
    // El does not support the () operator
    Abort:=true;
    if Data^.Found=nil then
      begin
      // El is the first element found -> raise error
      // ToDo: use the ( as error position
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements El=',GetObjPath(El));
      {$ENDIF}
      RaiseMsg(20170216151525,nIllegalQualifierAfter,sIllegalQualifierAfter,
        ['(',El.ElementTypeName],Data^.Params);
      end;
    exit;
    end;

  // El is a candidate (might be incompatible)
  if (Data^.Found=nil)
      or ((Data^.Distance=cIncompatible) and (Distance<cIncompatible)) then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallElements Found first candidate Distance=',Distance);
    {$ENDIF}
    Data^.Found:=El;
    Data^.ElScope:=ElScope;
    Data^.StartScope:=StartScope;
    Data^.Distance:=Distance;
    Data^.Count:=1;
    if Data^.List<>nil then
      begin
      Data^.List.Clear;
      Data^.List.Add(El);
      end;
    end
  else if Distance=cIncompatible then
    // another candidate, but it is incompatible -> ignore
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallElements Found another candidate, but it is incompatible -> ignore')
    {$ENDIF}
  else if (Data^.Distance=Distance) then
    begin
    { A class/record/helper member hides an equally-good unit-level global:
      the member is in a closer scope, so FPC picks it without ambiguity. 
      E.g. inside TStringHelper.Contains, an unqualified LowerCase
      resolves to the helper's own static LowerCase over the identically-signed
      global LowerCase. Only fires when exactly one candidate is a member. }
    if ((El.Parent is TPasClassType) or (El.Parent is TPasRecordType))
        <> ((Data^.Found.Parent is TPasClassType) or (Data^.Found.Parent is TPasRecordType)) then
      begin
      if (Data^.Found.Parent is TPasClassType) or (Data^.Found.Parent is TPasRecordType) then
        exit  // keep the member already found; ignore the outer global
      else
        begin
        // El (member) supersedes the global found first
        Data^.Found:=El;
        Data^.ElScope:=ElScope;
        Data^.StartScope:=StartScope;
        Data^.Distance:=Distance;
        Data^.Count:=1;
        if Data^.List<>nil then
          begin
          Data^.List.Clear;
          Data^.List.Add(El);
          end;
        exit;
        end;
      end;
    // Integer argument: a SINGLE parameter beats a double/extended one.
    case IntToSingleTie(El,Data^.Found) of
    1:
      begin
      Data^.Found:=El;
      Data^.ElScope:=ElScope;
      Data^.StartScope:=StartScope;
      Data^.Distance:=Distance;
      Data^.Count:=1;
      if Data^.List<>nil then
        begin
        Data^.List.Clear;
        Data^.List.Add(El);
        end;
      exit;
      end;
    -1:
      exit; // keep the single candidate already found
    end;
    // String-family tie-break: when the first argument is a string/char, prefer the
    // candidate whose first PARAMETER is the SAME family (ansi vs unicode) over one
    // of the OPPOSITE family. Verified vs ppcx64: StringReplace(ShortString, AnsiChar,
    // AnsiChar, ..) picks the AnsiString overload over the UnicodeString;WideChar one
    // (the same-family primary-string match dominates the char args' cross-width
    // advantage). Only fires when the two candidates' first params are opposite
    // families and the source arg is a known family — narrow, does not disturb ties
    // within one family.
    if (Data^.Params<>nil) and (length(Data^.Params.Params)>0) then
      begin
      // Look for the FIRST parameter position where the two candidates belong
      // to OPPOSITE string families. Position 0 is merely the commonest case:
      // fcl-json's TJSONObject.Add pair differs only in its SECOND parameter
      // (UTF8String vs UnicodeString), and Add(name, anObject.ClassName) - a
      // ShortString, so ansi family - has to pick the ansi one.
      SrcFam:=0;
      ElFam:=0;
      FoundFam:=0;
      for ArgFamIdx:=0 to length(Data^.Params.Params)-1 do
        begin
        ElFam:=ParamFamilyAt(El,ArgFamIdx);
        FoundFam:=ParamFamilyAt(Data^.Found,ArgFamIdx);
        if (ElFam<>0) and (FoundFam=-ElFam) then
          begin
          SrcFam:=ExprStrFamily(Data^.Params.Params[ArgFamIdx]);
          if SrcFam<>0 then
            break;
          end;
        ElFam:=0;
        FoundFam:=0;
        end;
      if SrcFam<>0 then
        begin
        if (FoundFam=SrcFam) and (ElFam=-SrcFam) then
          exit  // keep Found (matches source family); ignore El (opposite family)
        else if (ElFam=SrcFam) and (FoundFam=-SrcFam) then
          begin
          // El matches the source family; supersede the opposite-family Found
          Data^.Found:=El;
          Data^.ElScope:=ElScope;
          Data^.StartScope:=StartScope;
          Data^.Distance:=Distance;
          Data^.Count:=1;
          if Data^.List<>nil then
            begin
            Data^.List.Clear;
            Data^.List.Add(El);
            end;
          exit;
          end;
        end;
      // An untyped Pointer argument prefers the ANSI char-pointer overload, as
      // ppcx64 does whatever the declaration order: fcl-db's dsparams.inc calls
      // StrPas(Buffer) with a Pointer, and sysutils declares StrPas for both
      // PAnsiChar and PWideChar.
      if (Data^.Params<>nil) and (length(Data^.Params.Params)>0)
          and ArgIsUntypedPointer(Data^.Params.Params[0]) then
        begin
        ElFam:=FirstParamCharPtrFamily(El);
        FoundFam:=FirstParamCharPtrFamily(Data^.Found);
        if (FoundFam=1) and (ElFam=-1) then
          exit  // keep the PAnsiChar candidate
        else if (ElFam=1) and (FoundFam=-1) then
          begin
          Data^.Found:=El;
          Data^.ElScope:=ElScope;
          Data^.StartScope:=StartScope;
          Data^.Distance:=Distance;
          Data^.Count:=1;
          if Data^.List<>nil then
            begin
            Data^.List.Clear;
            Data^.List.Add(El);
            end;
          exit;
          end;
        end;
      // A ShortString and an AnsiString/RawByteString overload tie: FPC prefers
      // the AnsiString one (ShortString is legacy). E.g. two Pos overloads over a
      // char concatenation or a LowerCase result. Independent of the source
      // family so it also fires when the source type is indeterminate.
      ElBase:=FirstParamStrBase(El);
      FoundBase:=FirstParamStrBase(Data^.Found);
      if (FoundBase in [btAnsiString,btRawByteString]) and (ElBase=btShortString) then
        exit  // keep the AnsiString candidate; ignore the ShortString one
      else if (ElBase in [btAnsiString,btRawByteString]) and (FoundBase=btShortString) then
        begin
        // El is the AnsiString candidate; supersede the ShortString Found
        Data^.Found:=El;
        Data^.ElScope:=ElScope;
        Data^.StartScope:=StartScope;
        Data^.Distance:=Distance;
        Data^.Count:=1;
        if Data^.List<>nil then
          begin
          Data^.List.Clear;
          Data^.List.Add(El);
          end;
        exit;
        end;
      end;
    // The NEARER declaration wins a tie between a class and its ancestor: with
    // 'overload' the two sets merge, but proximity still decides, exactly as
    // ppcx64 does. vcl-compat's TJSONNumber redeclares TJSONString's
    // Create(const aValue: UnicodeString) - an identical signature - and
    // fcl-base's TLZWDecompressionStream adds a THIRD parameter with a default
    // to its ancestor's Create, which ties just as well. Both pick the
    // descendant there.
    if (El is TPasProcedure) and (Data^.Found is TPasProcedure)
        and (El<>Data^.Found)
        and (El.Parent is TPasClassType) and (Data^.Found.Parent is TPasClassType)
        and (El.Parent<>Data^.Found.Parent) then
      begin
      if CheckClassIsClass(TPasType(Data^.Found.Parent),TPasType(El.Parent))<cIncompatible then
        exit  // Found is the descendant; ignore the inherited El
      else if CheckClassIsClass(TPasType(El.Parent),TPasType(Data^.Found.Parent))<cIncompatible then
        begin
        // El is the descendant; it supersedes the inherited candidate
        Data^.Found:=El;
        Data^.ElScope:=ElScope;
        Data^.StartScope:=StartScope;
        Data^.Distance:=Distance;
        Data^.Count:=1;
        if Data^.List<>nil then
          begin
          Data^.List.Clear;
          Data^.List.Add(El);
          end;
        exit;
        end;
      end;
    // Unit-scope hiding: when two equally exact candidates come from different
    // units, a declaration in the unit being compiled (RootElement) hides an
    // identical-signature one from a used unit -- FPC's normal scope hiding, not
    // ambiguity. E.g. sysutils redeclares StrPas(PAnsiChar):string over the
    // system unit's StrPas(PAnsiChar):shortstring; inside sysutils the local one
    // wins. Only fires at the exact/alias tie level and when exactly one
    // candidate lives in the root module, so genuine same-unit overload ties are
    // still reported as ambiguous.
    if (Data^.Distance<=cAliasExact) and (RootElement<>nil)
        and (El is TPasProcedure) and (Data^.Found is TPasProcedure) then
      begin
      if (El.GetModule=RootElement)<>(Data^.Found.GetModule=RootElement) then
        begin
        if Data^.Found.GetModule=RootElement then
          exit  // keep the local (root-module) candidate; ignore the used-unit one
        else
          begin
          // El is in the root module; it supersedes the used-unit candidate
          Data^.Found:=El;
          Data^.ElScope:=ElScope;
          Data^.StartScope:=StartScope;
          Data^.Distance:=Distance;
          Data^.Count:=1;
          if Data^.List<>nil then
            begin
            Data^.List.Clear;
            Data^.List.Add(El);
            end;
          exit;
          end;
        end;
      end;
    // Unit-scope hiding, second case: neither candidate is in the root module,
    // but the two live in different units and exactly one is the SYSTEM unit.
    // The system unit is always used first (outermost scope), so any other
    // unit's same-named routine hides it -- FPC scope hiding, not ambiguity.
    // E.g. sysutils' StrPas(PAnsiChar):string hides system's
    // StrPas(PAnsiChar):shortstring for a unit using both (classes/stringl.inc).
    // Only at the exact/alias tie level.
    if (Data^.Distance<=cAliasExact)
        and (El is TPasProcedure) and (Data^.Found is TPasProcedure)
        and (El.GetModule<>nil) and (Data^.Found.GetModule<>nil)
        and (El.GetModule<>Data^.Found.GetModule) then
      begin
      if IsSystemUnit(El.GetModule) and not IsSystemUnit(Data^.Found.GetModule) then
        exit  // keep the non-system candidate; ignore the system-unit one
      else if IsSystemUnit(Data^.Found.GetModule) and not IsSystemUnit(El.GetModule) then
        begin
        // El is non-system; it supersedes the system-unit candidate
        Data^.Found:=El;
        Data^.ElScope:=ElScope;
        Data^.StartScope:=StartScope;
        Data^.Distance:=Distance;
        Data^.Count:=1;
        if Data^.List<>nil then
          begin
          Data^.List.Clear;
          Data^.List.Add(El);
          end;
        exit;
        end;
      end;
    // found another similar compatible one -> collect
    // Note: cLossyConversion is better than cIntToFloatConversion, not similar
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallElements Found another candidate Distance=',Distance,' OldDistance=',Data^.Distance);
    {$ENDIF}
    inc(Data^.Count);
    if (Data^.List<>nil) then
      begin
      if (Data^.List.IndexOf(El)>=0) then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.OnFindCallElements Found El twice: ',GetTreeDbg(El),
          ' ',GetElementSourcePosStr(El),
          ' PrevElScope=',GetObjName(Data^.ElScope),' ',GetTreeDbg(Data^.ElScope.Element),
          ' ElScope=',GetObjName(ElScope),' ',GetTreeDbg(ElScope.Element)
          );
        {$ENDIF}
        RaiseInternalError(20160924230805);
        end;
      Data^.List.Add(El);
      end;
    end
  else if (Distance<Data^.Distance) then
    begin
    // found a better one
    if IsVariantOverloadAmbiguous(El, Data^.Found, Data^.Params) then
      begin
      // FPC treats certain type pairs as ambiguous for a Variant argument even
      // when graduated scoring would pick a winner -> keep both, treat as tied.
      if Data^.List<>nil then
        begin
        if Data^.List.IndexOf(Data^.Found)<0 then
          Data^.List.Add(Data^.Found);
        Data^.List.Add(El);
        end;
      inc(Data^.Count);
      Data^.Found:=El;
      Data^.ElScope:=ElScope;
      Data^.StartScope:=StartScope;
      Data^.Distance:=Distance;
      end
    else
      begin
      // A strictly smaller distance wins outright, also inside the lossy band:
      // candidates there are not interchangeable when one is exact on the
      // argument that separates them (variants.pp DumpVariant).
      Data^.Count:=1;
      if Data^.List<>nil then
        Data^.List.Clear;
      Data^.Found:=El;
      Data^.ElScope:=ElScope;
      Data^.StartScope:=StartScope;
      Data^.Distance:=Distance;
      if Data^.List<>nil then
        Data^.List.Add(El);
      end;
    end
  else
    begin
    // found a worse one
    if IsVariantOverloadAmbiguous(El, Data^.Found, Data^.Params) then
      begin
      // variant-specific ambiguity -> treat as tied
      inc(Data^.Count);
      if Data^.List<>nil then
        Data^.List.Add(El);
      end;
    end;
end;

procedure TPasResolver.OnFindProc(El: TPasElement; ElScope,
  StartScope: TPasScope; FindProcData: Pointer; var Abort: boolean);
var
  Data: PFindProcData absolute FindProcData;
  Proc, DataProc: TPasProcedure;
  Store, SameScope: Boolean;
  ProcScope: TPasProcedureScope;
  CurResolver: TPasResolver;
  ProcModes: TModeSwitches;

  procedure CountProcInSameScope;
  begin
    inc(Data^.FoundInSameScope);
    if Proc.IsOverload then
      Data^.FoundOverloadModifier:=true;
  end;

  function ProcHidesOuterNonProc(P: TPasProcedure; NonProc: TPasElement): boolean;
  // True when P is a nested (local) procedure declared inside another routine
  // that does NOT enclose NonProc. Such a P legally SHADOWS the outer NonProc
  // (e.g. a nested "procedure QuickSort" hiding a unit-level "const QuickSort"),
  // rather than clashing with it — even though both live in the same module.
  var
    El: TPasElement;
  begin
    Result:=false;
    if (P=nil) or (NonProc=nil) then exit;
    El:=P.Parent;
    while El<>nil do
      begin
      if El is TPasProcedure then
        begin
        // P is nested inside procedure El; if NonProc lives outside El, P hides it
        if not NonProc.HasParent(El) then
          exit(true);
        end;
      El:=El.Parent;
      end;
  end;

begin
  //writeln('TPasResolver.OnFindProc START ',El.Name,':',GetElementTypeName(El),' itself=',El=Data^.Proc);
  DataProc:=Data^.Proc;
  if not (El is TPasProcedure) then
    begin
    // identifier is not a proc
    if (El is TPasVariable) then
      begin
      if TPasVariable(El).Visibility=visStrictPrivate then
        exit; // not visible
      if (TPasVariable(El).Visibility=visPrivate)
          and (El.GetModule<>StartScope.Element.GetModule) then
        exit; // not visible
      end;

    Data^.FoundNonProc:=El;
    Abort:=true;
    if (El.CustomData is TResElDataBuiltInProc) then
      begin
      if Data^.FoundOverloadModifier or IsProcOverload(DataProc) then
        exit; // no hint
      end;
    case Data^.Kind of
    fpkProc:
      // proc hides a non proc
      if (DataProc.GetModule=El.GetModule)
          and not ProcHidesOuterNonProc(DataProc,El) then
        // forbidden within same module
        RaiseMsg(20170216151649,nDuplicateIdentifier,sDuplicateIdentifier,
          [El.Name,GetElementSourcePosStr(El)],DataProc.ProcType)
      else
        begin
        // give a hint
        if DataProc.Parent is TPasMembersType then
          begin
          if El.Visibility=visStrictPrivate then
          else if (El.Visibility=visPrivate) and (El.GetModule<>DataProc.GetModule) then
          else
            LogMsg(20171118205344,mtHint,nFunctionHidesIdentifier_NonProc,sFunctionHidesIdentifier,
              [GetElementSourcePosStr(El)],DataProc.ProcType);
          end;
        end;
    fpkMethod:
      // method hides a non proc
      begin
      ProcScope:=TPasProcedureScope(DataProc.CustomData);
      CurResolver:=ProcScope.Owner as TPasResolver;
      if msDelphi in CurResolver.CurrentParser.CurrentModeswitches then
        // ok in delphi
      else if El.Parent=DataProc.Parent then
        // Same class: a method and a field or property of one name clash.
        RaiseMsg(20171118232543,nDuplicateIdentifier,sDuplicateIdentifier,
          [El.Name,GetElementSourcePosStr(El)],DataProc.ProcType);
        // A member of an ANCESTOR, or of an outer scope, is merely HIDDEN, and
        // objfpc allows that silently - fcl-report's TFPReportExporter declares
        // `class function Name` over TComponent's published `property Name`.
      end;
    end;
    exit;
    end;

  // identifier is a proc
  Proc:=TPasProcedure(El);
  if El=DataProc then
    begin
    // found itself -> this is normal when searching for overloads
    CountProcInSameScope;
    exit;
    end;

  // An operator and a same-named ordinary method live in separate namespaces and
  // never overload nor clash (e.g. `class operator :=` vs a method `Assign`, whose
  // internal operator name is also "assign"). FPC allows both to coexist.
  if (Proc is TPasOperator)<>(DataProc is TPasOperator) then
    exit;

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnFindProc ',GetTreeDbg(El,2));
  {$ENDIF}
  Store:=CheckProcOverloadCompatibility(DataProc,Proc);
  case Data^.Kind of
  fpkProc:
    // A nested routine hides a routine declared outside its enclosing routine,
    // as it hides a non-routine (fpdebug's local GetSignInfo in a method whose
    // class inherits a GetSignInfo method).
    SameScope:=(DataProc.GetModule=Proc.GetModule)
      and not ProcHidesOuterNonProc(DataProc,Proc);
  fpkMethod:
    SameScope:=DataProc.Parent=Proc.Parent;
  else
    // use OnFindProcDeclaration instead
    RaiseNotYetImplemented(20191010123525,DataProc);
  end;
  // The mode the declaration was written in: a specialized method keeps its
  // generic's, whatever the mode of the unit that specializes it.
  ProcModes:=GetElModeSwitches(DataProc);
  if SameScope then
    begin
    // same scope
    if (msObjfpc in ProcModes) then
      begin
        if ProcHasGroupOverload(DataProc) then
          Include(TPasProcedureScope(Proc.CustomData).Flags,ppsfIsGroupOverload)
        else if ProcHasGroupOverload(Proc) then
          Include(TPasProcedureScope(DataProc.CustomData).Flags,ppsfIsGroupOverload);
      end;
    if Store then
      begin
      // same scope, same signature
      // Note: forward declaration was already handled in FinishProcedureHeader
      RaiseMsg(20171118221821,nDuplicateIdentifier,sDuplicateIdentifier,
                [Proc.Name,GetElementSourcePosStr(Proc)],DataProc.ProcType);
      end
    else
      begin
      // same scope, different signature
      if (msDelphi in ProcModes) then
        begin
        // Delphi does not allow different procs without 'overload' in a scope
        // Exception: with implicitfunctionspecialization, generic procs with
        // different template parameter counts are implicitly overloaded.
        if not IsProcOverload(Proc) then
          begin
          if ((msImplicitFunctionSpec in ProcModes) and
              (GetProcTemplateTypes(Proc) <> nil) and
              (GetProcTemplateTypes(DataProc) <> nil))
             or
             ((GetProcTemplateTypes(Proc) = nil) <> (GetProcTemplateTypes(DataProc) = nil)) then
            begin
            // generic procs are implicitly overloaded with implicit function
            // specialization; also a generic + non-generic method of the same name
            // (different generic arity) — timpfuncspez35.
            Include(TPasProcedureScope(Proc.CustomData).Flags, ppsfIsGroupOverload);
            Include(TPasProcedureScope(DataProc.CustomData).Flags, ppsfIsGroupOverload);
            end
          else if ProcIsUnimplementedForward(DataProc) then
            begin
            // The earlier one is still a forward: fpc does not complain here
            // either (pparautl.pas: `if not(fwpd.forwarddef)`), and openssl.pas
            // relies on it. They overload each other from now on - without this
            // the CALL search stops at whichever is found first, and a call that
            // only the other one accepts is rejected (`BioRead(B, Result, L)`
            // with Result an AnsiString, against a `TBytes` sibling).
            Include(TPasProcedureScope(Proc.CustomData).Flags, ppsfIsGroupOverload);
            Include(TPasProcedureScope(DataProc.CustomData).Flags, ppsfIsGroupOverload);
            end
          else
            RaiseMsg(20171118222112,nPreviousDeclMissesOverload,sPreviousDeclMissesOverload,
              [Proc.Name,GetElementSourcePosStr(Proc)],DataProc.ProcType);
          end
        else if (not DataProc.IsOverload) then
          begin
          // Note: the OverriddenProc might not yet be set
          if DataProc.IsOverride
              or ((TPasProcedureScope(DataProc.CustomData).OverriddenProc<>nil)
                and (ppsfIsOverrideOverload in TPasProcedureScope(DataProc.CustomData).Flags)) then
            // is override or inherited override
          else if ProcIsUnimplementedForward(DataProc) then
            // as above: fpc's check skips a forward declaration
          else
            RaiseMsg(20171118222147,nOverloadedProcMissesOverload,sOverloadedProcMissesOverload,
              [GetElementSourcePosStr(Proc)],DataProc.ProcType);
          end;
        end
      else
        begin
        // ObjFPC allows different procs without 'overload' modifier
        end;
      CountProcInSameScope;
      end;
    end
  else
    begin
    // different scopes
    if DataProc.IsOverride then
      // overrides are not overloads
    else if DataProc.IsReintroduced then
      // reintroduce do not warn
    else if ProcHasGroupOverload(DataProc) then
      // overload
    else
      begin
      if Store
          or (Data^.FoundInSameScope=1) // missing 'overload' hints only for the first proc in a scope
             then
        begin
        if (Data^.Kind=fpkMethod) and (Proc.IsVirtual or Proc.IsOverride) then
          // give a hint, that method hides a virtual method in ancestor
          LogMsg(20170216151712,mtWarning,nMethodHidesMethodOfBaseType,
            sMethodHidesMethodOfBaseType,
            [DataProc.Name,Proc.Parent.Name,GetElementSourcePosStr(Proc)],DataProc.ProcType)
        else
          begin
          // Delphi/FPC do not give a message when hiding a non virtual method
          // -> emit Hint with other message id
          if (DataProc.Parent is TPasMembersType) then
            begin
            ProcScope:=Proc.CustomData as TPasProcedureScope;
            if (Proc.Visibility=visStrictPrivate)
                or ((Proc.Visibility=visPrivate)
                  and (Proc.GetModule<>DataProc.GetModule)) then
              // a private method is hidden by definition -> no hint
            else if (Proc.Visibility=visPublished) then
              // a published can hide (used for overloading rtti) -> no hint
            else if (ProcScope.ImplProc<>nil)  // not abstract, external
                and (not ProcHasImplElements(ProcScope.ImplProc)) then
              // hidden method has implementation, but no statements -> useless
              // -> do not give a hint for hiding this useless method
              // Note: if this happens in the same unit, the body was not yet parsed
            else if (Proc is TPasConstructor)
                and (DataProc.ClassType=Proc.ClassType) then
              // do not give a hint for hiding a constructor
            else if Store then
              begin
              // method hides ancestor method with same signature
              LogMsg(20190316152656,mtHint,
                nMethodHidesNonVirtualMethodExactly,sMethodHidesNonVirtualMethodExactly,
                [GetElementSourcePosStr(Proc)],DataProc.ProcType);
              end
            else
              begin
              //writeln('TPasResolver.OnFindProc Proc=',Proc.PathName,' DataProc=',DataProc.PathName,' ',Proc.Visibility);
              LogMsg(20171118214523,mtHint,
                nFunctionHidesIdentifier_NonVirtualMethod,sFunctionHidesIdentifier,
                [GetElementSourcePosStr(Proc)],DataProc.ProcType);
              end;
            end;
          end;
        Abort:=true;
        end;
      end;
    end;

  if Store then
    begin
    Data^.Found:=Proc;
    Data^.ElScope:=ElScope;
    Data^.StartScope:=StartScope;
    Abort:=true;
    end;
end;

procedure TPasResolver.OnFindProcDeclaration(El: TPasElement; ElScope,
  StartScope: TPasScope; FindProcData: Pointer; var Abort: boolean);
var
  Data: PFindProcData absolute FindProcData;
  Proc: TPasProcedure;
  Store: Boolean;
begin
  //writeln('TPasResolver.OnFindProcDeclaration START ',El.Name,':',GetElementTypeName(El),' itself=',El=Data^.Proc);
  if not (El is TPasProcedure) then
    begin
    // identifier is not a proc
    Data^.FoundNonProc:=El;
    Abort:=true;
    exit;
    end;
  if El=Data^.Proc then
    // found itself -> this is normal when searching for overloads
    exit;

  // identifier is a proc
  Proc:=TPasProcedure(El);

  // An operator and a same-named ordinary function live in separate namespaces
  // (e.g. `function Power` and `operator **`, whose internal name is also
  // "power"). One must never be matched as the forward/implementation of the
  // other. FPC allows both to coexist.
  if (Proc is TPasOperator)<>(Data^.Proc is TPasOperator) then
    exit;

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnFindProcDeclaration ',GetTreeDbg(El,2));
  {$ENDIF}
  Store:=CheckProcOverloadCompatibility(Data^.Proc,Proc);

  if Store then
    begin
    Data^.Found:=Proc;
    Data^.ElScope:=ElScope;
    Data^.StartScope:=StartScope;
    Abort:=true;
    end;
end;

function TPasResolver.IsSameProcContext(ProcParentA, ProcParentB: TPasElement
  ): boolean;
begin
  if ProcParentA=ProcParentB then exit(true);
  if (ProcParentA.ClassType=TInterfaceSection) then
    begin
    if (ProcParentB.ClassType=TImplementationSection)
        and (ProcParentB.Parent=ProcParentA.Parent) then
      exit(true);
    end
  else if (ProcParentB.ClassType=TInterfaceSection) then
    begin
    if (ProcParentA.ClassType=TImplementationSection)
        and (ProcParentA.Parent=ProcParentB.Parent) then
      exit(true);
    end;
  Result:=false;
end;

function TPasResolver.IsSelfRefCandidate(El: TPasExpr): boolean;
// False when El is the NAME side of a qualified access, which always names a
// member of whatever is on the left and never the enclosing function's Result.
// Two shapes, and both bit:
//   "BottomRight.Ceiling" inside `function TRectF.Ceiling: TRectF` (types.pp) -
//     treating it as a self-reference silently turned the RHS into a TRectF;
//   "inherited HasAggregate" inside `function T.HasAggregate: Boolean`
//     (fpexprpars) - an inherited call is a TBinaryExpr too, with the inherited
//     marker on the left and eopNone as the opcode.
// A qualifier binds TIGHTER than the qualification, so `inherited GetFirst^`
// parses with the name's parent being the DEREF, not the binary expression.
// Look through those wrappers, or the name reads as this function's own Result
// and the deref is then refused ("illegal qualifier ^ in front of Result") -
// rtl/objpas/fgl.pp writes `T(inherited GetFirst^)`.
var
  Node: TPasExpr;
begin
  Result:=true;
  if El=nil then exit;
  Node:=El;
  while (Node.Parent is TUnaryExpr) and (TUnaryExpr(Node.Parent).Operand=Node) do
    Node:=TPasExpr(Node.Parent);
  if not (Node.Parent is TBinaryExpr) then exit;
  if TBinaryExpr(Node.Parent).right<>Node then exit;
  if (TBinaryExpr(Node.Parent).OpCode=eopSubIdent)
      or (TBinaryExpr(Node.Parent).left is TInheritedExpr) then
    Result:=false;
end;

function TPasResolver.IsSelfRefResultName(El: TPasElement; Proc: TPasProcedure
  ): boolean;
// A bare function name inside that function's own body names its Result, so a
// same-named identifier in an OUTER scope must not shadow it - ncurses/ocrt has
// a nested "function CP(...): word" next to a unit variable "cp: array of ...".
var
  ParentEl: TPasElement;
  ImplProc: TPasProcedure;
begin
  Result:=false;
  if (El=nil) or (Proc=nil) then exit;
  if not (Proc.ProcType is TPasFunctionType) then exit;
  if (El is TPasExpr) and not IsSelfRefCandidate(TPasExpr(El)) then exit;
  // Identity, not just the name: an OVERLOAD of the same name that needs
  // parameters must still be skipped in favour of the parameterless one, which
  // in Delphi mode is a real recursive call (TestProcOverloadDelphiOverride).
  ImplProc:=nil;
  if Proc.CustomData is TPasProcedureScope then
    ImplProc:=TPasProcedureScope(Proc.CustomData).ImplProc;
  if ImplProc=nil then
    ImplProc:=Proc;
  // The function itself, or a routine nested in it: lazarus' codetools does
  // `Include(GetHintModifiers,..)` in a nested function of GetHintModifiers.
  ParentEl:=El;
  while ParentEl<>nil do
    begin
    if (ParentEl=ImplProc) or (ParentEl=Proc) then
      exit(ParentEl is TPasFunction);
    ParentEl:=ParentEl.Parent;
    end;
end;

function TPasResolver.EnclosingFunctionNamed(El: TPasElement;
  const aName: string): TPasFunction;
// The innermost function enclosing El whose name is aName, also through routines
// nested in it; nil if there is none.
begin
  Result:=nil;
  if El=nil then exit;
  El:=El.Parent;
  while El<>nil do
    begin
    if (El is TPasFunction) and SameSelfRefProcName(El.Name,aName) then
      exit(TPasFunction(El));
    El:=El.Parent;
    end;
end;

function TPasResolver.SameSelfRefProcName(const EnclosingName,
  ProcName: String): boolean;
// Whether a bare use of ProcName inside EnclosingName's body names that same
// routine, i.e. is its Result. A METHOD's implementation has the QUALIFIED
// name ("TEReader.ReadNext"), so a plain compare said no and an overloaded
// method could not assign to its own function name at all - lnfodwrf writes
// `ReadNext := size = 0` inside `TEReader.ReadNext(var dest; size)`. A generic
// body's name may also have a "<...>" specialization suffix.
// The suffix can sit at either level - "TSet<System.Integer>.Insert" (a method
// of a specialized generic class) and "TFoo.Bar<System.Integer>" (a specialized
// generic method) - so only the dots and the '<' OUTSIDE a "<...>" group
// separate the parts. Cutting at the first '<' left "TSet", and an overloaded
// method of a specialized generic could not name its own Result (fcl-stl gset).

  function ShortProcName(const aName: String): String;
  var
    I, Depth, LastDot: Integer;
  begin
    Depth:=0;
    LastDot:=0;
    for I:=1 to length(aName) do
      case aName[I] of
      '<': inc(Depth);
      '>': if Depth>0 then dec(Depth);
      '.': if Depth=0 then LastDot:=I;
      end;
    Result:=Copy(aName,LastDot+1,length(aName));
    I:=Pos('<',Result);
    if I>0 then
      Result:=LeftStr(Result,I-1);
  end;

begin
  // Only the ENCLOSING name is trimmed. A specialized generic ROUTINE keeps its
  // suffix in ProcName ("Fly<System.Word>"), and a bare `Fly` inside `Fly` is
  // then a recursive CALL, not the result variable (testpassrc
  // TestGenProc_CallSelfNoParams).
  Result:=CompareText(ShortProcName(EnclosingName),ProcName)=0;
end;


function TPasResolver.RedirectSelfNameToResult(Expr: TPasExpr;
  var ExprResolved: TPasResolverResult; SetReferenceFlags: boolean): boolean;
// A parameterless function's OWN NAME, written inside its own body and handed to
// something that needs a variable, denotes the Result - not a recursive call.
// Re-points the reference at the ResultEl and recomputes. False when the shape
// does not match, and then nothing is changed.
var
  ArgRef: TResolvedReference;
  SelfProc: TPasProcedure;
  EnclEl: TPasElement;
begin
  Result:=false;
  // only a question here: a loop variable is reported by the caller, if at all
  if ResolvedElCanBeVarParam(ExprResolved,Expr,false) then exit;
  if not ((Expr is TPrimitiveExpr) and (TPrimitiveExpr(Expr).Kind=pekIdent)
          and (Expr.CustomData is TResolvedReference)
          and not ExprIsAddrTarget(Expr)) then
    exit;
  ArgRef:=TResolvedReference(Expr.CustomData);
  if not ((ArgRef.Declaration is TPasProcedure)
          and (TPasProcedure(ArgRef.Declaration).ProcType is TPasFunctionType)) then
    exit;
  SelfProc:=TPasProcedure(ArgRef.Declaration);
  EnclEl:=Expr;
  while (EnclEl<>nil) and not (EnclEl is TPasProcedure) do
    EnclEl:=EnclEl.Parent;
  // Compare BASE names: the enclosing name may have a "<...>" specialization
  // suffix AND, for a METHOD, its qualifying type ("TEReader.ReadUHalf").
  if not ((EnclEl is TPasFunction)
          and SameSelfRefProcName(TPasFunction(EnclEl).Name,SelfProc.Name)) then
    exit;
  ArgRef.Declaration:=TPasFunctionType(TPasFunction(EnclEl).ProcType).ResultEl;
  if SetReferenceFlags then
    ComputeElement(Expr,ExprResolved,[rcSetReferenceFlags])
  else
    ComputeElement(Expr,ExprResolved,[]);
  Result:=true;
end;

function TPasResolver.IsProcOverloading(LastProc, CurProc: TPasProcedure
  ): boolean;
begin
  if msDelphi in TPasProcedureScope(LastProc.CustomData).ModeSwitches then
    begin
    // Generic procs implicitly overloaded via implicitfunctionspecialization have
    // ppsfIsGroupOverload (ProcHasGroupOverload) and overload each other without an
    // explicit "overload" directive, in Delphi mode too (timpfuncspez35).
    if (not IsProcOverload(LastProc) and not ProcHasGroupOverload(LastProc))
        or (not IsProcOverload(CurProc) and not ProcHasGroupOverload(CurProc)) then
      begin
      // fpc refuses the missing `overload` only when the EARLIER declaration is
      // no longer a FORWARD one (pparautl.pas: `if not(fwpd.forwarddef)`). Two
      // declarations in a unit's INTERFACE are both still forward, so they
      // overload each other without the directive - openssl.pas declares
      // BioRead twice that way, once taking `var AnsiString` and once `TBytes`.
      // Treating them as non-overloading hides the first, and a call that only
      // it accepts is then rejected.
      if not ((LastProc.Parent is TInterfaceSection)
              and (CurProc.Parent = LastProc.Parent)) then
        exit(false);
      end;
    end
  else
    begin
    // mode objfpc
    if IsSameProcContext(LastProc.Parent,CurProc.Parent) then
      // mode objfpc: procs in same context have implicit overload
    else
      begin
      // mode objfpc, different context
      if not ProcHasGroupOverload(LastProc) then
        exit(false);
      end;
    end;

  Result:=true;
end;

function TPasResolver.IsHelperMethodOverloadGroup(LastProc,
  CurProc: TPasProcedure): boolean;
// True when LastProc/CurProc form a helper overload group: a class/record helper
// method marked "overload" does not hide the same-named methods of the extended
// type, so they overload each other even when the extended method has no
// "overload" directive (tchlp33).
var
  HelperProc, OtherProc: TPasProcedure;
  ExtType: TPasType;
begin
  Result:=false;
  if (LastProc=nil) or (CurProc=nil) then exit;
  // identify which one is the overload-marked helper method
  if (LastProc.Parent is TPasClassType)
      and (TPasClassType(LastProc.Parent).ObjKind in okAllHelpers)
      and IsProcOverload(LastProc) then
    begin HelperProc:=LastProc; OtherProc:=CurProc; end
  else if (CurProc.Parent is TPasClassType)
      and (TPasClassType(CurProc.Parent).ObjKind in okAllHelpers)
      and IsProcOverload(CurProc) then
    begin HelperProc:=CurProc; OtherProc:=LastProc; end
  else
    exit;
  // the other proc must belong to the type the helper extends (or an ancestor)
  ExtType:=TPasClassType(HelperProc.Parent).HelperForType;
  if ExtType=nil then exit;
  ExtType:=ResolveAliasType(ExtType);
  if not (OtherProc.Parent is TPasType) then exit;
  if ExtType=TPasType(OtherProc.Parent) then
    Result:=true
  else if (ExtType is TPasClassType) and (OtherProc.Parent is TPasClassType) then
    Result:=CheckClassIsClass(ExtType,TPasType(OtherProc.Parent))<>cIncompatible;
end;

function TPasResolver.FindProcSameSignature(const ProcName: string;
  Proc: TPasProcedure; Scope: TPasIdentifierScope; OnlyLocal: boolean
  ): TPasProcedure;
var
  FindData: TFindProcData;
  Abort: boolean;
begin
  FindData:=Default(TFindProcData);
  FindData.Proc:=Proc;
  FindData.Args:=Proc.ProcType.Args;
  FindData.Kind:=fpkProcDeclaration;
  Abort:=false;
  //writeln('TPasResolver.FindProcSameSignature ',ProcName,' OnlyLocal=',OnlyLocal);
  if OnlyLocal then
    Scope.IterateLocalElements(ProcName,Scope,@OnFindProcDeclaration,@FindData,Abort)
  else
    Scope.IterateElements(ProcName,Scope,@OnFindProcDeclaration,@FindData,Abort);
  Result:=FindData.Found;
end;

procedure TPasResolver.SetCurrentParser(AValue: TPasParser);
var
  Scanner: TPascalScanner;
begin
  //writeln('TPasResolver.SetCurrentParser ',AValue<>nil);
  if AValue=CurrentParser then exit;
  Clear;
  inherited SetCurrentParser(AValue);
  if CurrentParser<>nil then
    begin
    CurrentParser.Options:=CurrentParser.Options+po_Resolver;
    if CurrentParser.Scanner<>nil then
      begin
      Scanner:=CurrentParser.Scanner;
      if (Scanner.OnWarnDirective=nil) then
        Scanner.OnWarnDirective:=@ScannerWarnDirective;
      Scanner.SetNonToken(tkself);
      end;
    end;
end;

procedure TPasResolver.ScannerWarnDirective(Sender: TObject;
  Identifier: TPasScannerString; State: TWarnMsgState; var Handled: boolean);
var
  MsgNumbers: TIntegerDynArray;
  i: Integer;
begin
  if not GetWarnIdentifierNumbers(Identifier,MsgNumbers) then exit;
  Handled:=true;
  for i:=0 to length(MsgNumbers)-1 do
    TPascalScanner(Sender).WarnMsgState[MsgNumbers[i]]:=State;
end;

procedure TPasResolver.CheckTopScope(ExpectedClass: TPasScopeClass;
  AllowDescendants: boolean);
var
  Scope: TPasScope;
begin
  Scope:=TopScope;
  if Scope=nil then
    RaiseInternalError(20160922163319,'Expected TopScope='+ExpectedClass.ClassName+' but found nil');
  if Scope.ClassType<>ExpectedClass then
    if (not AllowDescendants) or (not Scope.InheritsFrom(ExpectedClass)) then
      RaiseInternalError(20160922163323,'Expected TopScope='+ExpectedClass.ClassName+' but found '+Scope.ClassName);
end;

function TPasResolver.AddIdentifier(Scope: TPasIdentifierScope;
  const aName: String; El: TPasElement; const Kind: TPasIdentifierKind
  ): TPasIdentifier;

  function SkipGenericTypes(Identifier: TPasIdentifier;
    TypeParamCnt: integer): TPasIdentifier;
  var
    CurEl: TPasElement;
  begin
    while Identifier<>nil do
      begin
      CurEl:=Identifier.Element;
      if CurEl is TPasGenericType then
        begin
        if GetTypeParameterCount(TPasGenericType(CurEl))=TypeParamCnt then
          break;
        end
      else
        begin
        if TypeParamCnt=0 then
          break;
        end;
      Identifier:=Identifier.NextSameIdentifier;
      end;
    Result:=Identifier;
  end;

var
  Group: TPasGroupScope;
  Identifier, OlderIdentifier: TPasIdentifier;
  OlderEl: TPasElement;
  C: TClass;
  i, TypeParamCnt: Integer;
  OtherScope: TPasIdentifierScope;
  ParentScope: TPasScope;
  IsGeneric, IsDelphi: Boolean;
begin
  if aName='' then exit(nil);

  // The mode of the unit that DECLARES the name, not of the one being compiled.
  // A member of a SPECIALIZED class is the generic's own declaration, and
  // rtl-generics is written in Delphi mode, where a nested type may hide the
  // ancestor's type of the same name (TCustomList<T>.PT over TEnumerable<T>.PT).
  // Judged by the consumer's mode instead, that redeclaration is a duplicate.
  IsDelphi:=msDelphi in GetElModeSwitches(El);

  if Scope is TPasGroupScope then
    begin
    Group:=TPasGroupScope(Scope);
    Scope:=Group.Scopes[0];
    end
  else
    Group:=nil;
  if El is TPasGenericType then
    begin
    IsGeneric:=true;
    TypeParamCnt:=GetTypeParameterCount(TPasGenericType(El));
    end
  else
    begin
    IsGeneric:=false;
    TypeParamCnt:=0;
    end;

  if (El.Visibility=visPublished) then
    begin
    C:=El.ClassType;
    if (C=TPasProperty) or (C=TPasVariable) then
      // Note: VarModifiers are not yet set
    else if C=TPasConst then
      // a constant in a published section has no RTTI, so it is harmless
    else if (C=TPasProcedure) or (C=TPasFunction)
        or (C=TPasConstructor) or (C=TPasDestructor) then
      // ok — instance methods, constructors and destructors may be published
      // (uabstrcl); class/static methods may NOT (TestClass_PublishedClassFunctionFail)
    else if ((C=TPasClassProcedure) or (C=TPasClassFunction))
        and AllowPublishedClassMethod then
      // ok where the target allows it
    else
      RaiseMsg(20170403223024,nSymbolCannotBePublished,sSymbolCannotBePublished,[],El);
    end;

  if (Kind=pikSimple) and (Group<>nil) and (El.ClassType<>TPasProperty)
      and not IsDelphi then
    begin
    // check duplicate in ancestors and helpers
    for i:=1 to Group.Count-1 do
      begin
      OtherScope:=Group.Scopes[i];
      OlderIdentifier:=OtherScope.FindLocalIdentifier(aName);
      if IsGeneric then
        OlderIdentifier:=SkipGenericTypes(OlderIdentifier,TypeParamCnt);
      while OlderIdentifier<>nil do
        begin
        OlderEl:=OlderIdentifier.Element;
        OlderIdentifier:=OlderIdentifier.NextSameIdentifier;
        // A hidden ancestor member cannot clash, whatever it is: a field, or a
        // nested type like lazutils' private TCapacityOrdT, which a descendant
        // in another unit declares again.
        if OlderEl.Visibility=visStrictPrivate then
          continue; // OlderEl is hidden
        if (OlderEl.Visibility=visPrivate)
            and (OlderEl.GetModule<>El.GetModule) then
          continue; // OlderEl is hidden
        RaiseMsg(20170221130001,nDuplicateIdentifier,sDuplicateIdentifier,
                 [aName,GetElementSourcePosStr(OlderEl)],El);
        end;
      end;
    end;

  Identifier:=Scope.AddIdentifier(aName,El,Kind);

  // check duplicate in current scope
  OlderIdentifier:=Identifier.NextSameIdentifier;
  if IsGeneric and IsDelphi then
    OlderIdentifier:=SkipGenericTypes(OlderIdentifier,TypeParamCnt);
  if OlderIdentifier<>nil then
    begin
    OlderEl:=OlderIdentifier.Element;
    if (OlderEl.ClassType=TPasEnumValue)
        and (OlderEl.Parent.Parent<>Scope.Element) then
      begin
      // this enum was propagated from a sub type -> remove enum from this scope
      if OlderIdentifier.NextSameIdentifier<>nil then
        RaiseNotYetImplemented(20190807114726,El,GetElementSourcePosStr(OlderEl));
      Scope.RemoveLocalIdentifier(OlderEl);
      OlderIdentifier:=nil;
      OlderEl:=nil;
      end
    else if (El.Visibility=visPublished) and (El is TPasProcedure)
        and (OlderEl is TPasProcedure)
        and not AllowPublishedMethodOverload then
      // published method bites method in same scope
      RaiseMsg(20190626175432,nDuplicatePublishedMethodXAtY,
               sDuplicatePublishedMethodXAtY,
               [aName,GetElementSourcePosStr(OlderEl)],El)
    else if (Identifier.Kind=pikNamespace) or (OlderIdentifier.Kind=pikNamespace) then
      // a namespace identifier never clashes (a real declaration shadows it)
    else if (OlderEl is TPasUsesUnit) or (OlderEl is TPasModule) then
      // a genuine declaration (or a used unit) may share a name with a used unit
      // name / namespace (FPC allows a unit `tudots` alongside namespace
      // `tudots.dot.next` — tdotunits3)
    else if (Identifier.Kind=pikSimple)
        or (OlderIdentifier.Kind=pikSimple) then
      // duplicate identifier
      RaiseMsg(20170216151530,nDuplicateIdentifier,sDuplicateIdentifier,
              [aName,GetElementSourcePosStr(OlderEl)],El);
    end;

  if (Scope=TopScope) and (Scope is TPasSectionScope) then
    begin
    ParentScope:=Scopes[ScopeCount-2];
    if ParentScope is TPasSectionScope then
      begin
      // check unit interface and implementation duplicates
      OlderIdentifier:=TPasSectionScope(ParentScope).FindLocalIdentifier(aName);
      repeat
        if IsGeneric then
          OlderIdentifier:=SkipGenericTypes(OlderIdentifier,TypeParamCnt);
        if OlderIdentifier=nil then break;
        OlderEl:=OlderIdentifier.Element;
        if (Identifier.Kind=pikNamespace)
            or (OlderIdentifier.Kind=pikNamespace) then
          // namespace cannot clash
        else if (Identifier.Kind=pikSimple)
            or (OlderIdentifier.Kind=pikSimple) then
          RaiseMsg(20190818141630,nDuplicateIdentifier,sDuplicateIdentifier,
                   [aName,GetElementSourcePosStr(OlderEl)],El);
        OlderIdentifier:=OlderIdentifier.NextSameIdentifier;
      until OlderIdentifier=nil;
      end;
    end;

  Result:=Identifier;
end;

function TPasResolver.FindSoleUnimplementedForward(const ProcName: string;
  Scope: TPasIdentifierScope; ExceptProc: TPasProcedure): TPasProcedure;
// FPC lets the implementation of a forward/interface routine OMIT its parameter
// list (`procedure p;` implementing `procedure p(x:T); forward;`). Such an impl
// has zero args and won't match the forward by signature, so fall back to a
// by-name search: return the SOLE same-name forward still needing a body (nil if
// none or if ambiguous — more than one candidate). ExceptProc is the impl itself.
var
  Ident: TPasIdentifier;
  El: TPasElement;
  Cand: TPasProcedure;
  Count: Integer;
begin
  Result:=nil;
  if Scope=nil then exit;
  Count:=0;
  Ident:=Scope.FindLocalIdentifier(ProcName);
  while Ident<>nil do
    begin
    El:=Ident.Element;
    if (El is TPasProcedure) and (El<>ExceptProc) then
      begin
      Cand:=TPasProcedure(El);
      if ProcNeedsImplProc(Cand)
          and (Cand.CustomData is TPasProcedureScope)
          and (TPasProcedureScope(Cand.CustomData).ImplProc=nil) then
        begin
        inc(Count);
        Result:=Cand;
        end;
      end;
    Ident:=Ident.NextSameIdentifier;
    end;
  if Count<>1 then
    Result:=nil;
end;
