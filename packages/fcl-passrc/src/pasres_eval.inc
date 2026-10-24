{%MainUnit pasresolver.pp}
{ **********************************************************************
 
    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - Evaluation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }


procedure TPasResolver.CheckIsClass(El: TPasElement;
  const ResolvedEl: TPasResolverResult);
var
  TypeEl: TPasType;
begin
  if (ResolvedEl.BaseType<>btContext) then
    RaiseXExpectedButYFound(20170216152245,'class',BaseTypeNames[ResolvedEl.BaseType],El);
  TypeEl:=ResolvedEl.LoTypeEl;
  if (TypeEl.ClassType<>TPasClassType)
      or (TPasClassType(TypeEl).ObjKind<>okClass) then
    RaiseXExpectedButYFound(20170216152246,'class',GetElementTypeName(ResolvedEl.LoTypeEl),El);
end;

function TPasResolver.CheckTypeCastClassInstanceToClass(const FromClassRes,
  ToClassRes: TPasResolverResult; ErrorEl: TPasElement): integer;
// called when type casting a class instance into an unrelated class
begin
  if FromClassRes.BaseType=btNone then ;
  if ToClassRes.BaseType=btNone then ;
  if ErrorEl=nil then ;
  Result:=cIncompatible;
end;

procedure TPasResolver.CheckSetLitElCompatible(Left, Right: TPasExpr;
  const LHS, RHS: TPasResolverResult);
var
  LBT, RBT: TResolverBaseType;
begin
  // check both are values
  if not (rrfReadable in LHS.Flags) then
    begin
    if LHS.LoTypeEl<>nil then
      RaiseXExpectedButYFound(20170216152645,'ordinal',GetElementTypeName(LHS.LoTypeEl),Left)
    else
      RaiseXExpectedButYFound(20170216152648,'ordinal',BaseTypeNames[LHS.BaseType],Left);
    end;
  if not (rrfReadable in RHS.Flags) then
    begin
    if RHS.LoTypeEl<>nil then
      RaiseXExpectedButYFound(20170216152651,'ordinal',GetElementTypeName(RHS.LoTypeEl),Right)
    else
      RaiseXExpectedButYFound(20170216152653,'ordinal',BaseTypeNames[RHS.BaseType],Right);
    end;
  // check both have the same ordinal type
  LBT:=GetActualBaseType(LHS.BaseType);
  RBT:=GetActualBaseType(RHS.BaseType);
  if LBT in btAllBooleans then
    begin
    if RBT in btAllBooleans then
      exit;
    RaiseXExpectedButYFound(20170216152656,'boolean',BaseTypeNames[RHS.BaseType],Right);
    end
  else if LBT in btAllInteger then
    begin
    if RBT in btAllInteger then
      exit;
    RaiseXExpectedButYFound(20170216152658,'integer',BaseTypeNames[RHS.BaseType],Right);
    end
  else if LBT in btAllChars then
    begin
    if RBT in btAllChars then
      exit;
    RaiseXExpectedButYFound(20170216152702,'char',BaseTypeNames[RHS.BaseType],Right);
    end
  else if LBT=btContext then
    begin
    if LHS.LoTypeEl.ClassType=TPasEnumType then
      begin
      if LHS.LoTypeEl=RHS.LoTypeEl then
        exit;
      if RHS.LoTypeEl.ClassType<>TPasEnumType then
        RaiseXExpectedButYFound(20170216152707,LHS.LoTypeEl.Parent.Name,GetElementTypeName(RHS.LoTypeEl),Right);
      if LHS.LoTypeEl.Parent<>RHS.LoTypeEl.Parent then
        RaiseXExpectedButYFound(20170216152710,LHS.LoTypeEl.Parent.Name,RHS.LoTypeEl.Parent.Name,Right);
      end
    else
      RaiseXExpectedButYFound(20170216152712,'ordinal',BaseTypeNames[LHS.BaseType],Left);
    end
  else
    RaiseXExpectedButYFound(20170216152714,'ordinal',BaseTypeNames[LHS.BaseType],Left);
end;

function TPasResolver.CheckIsOrdinal(
  const ResolvedEl: TPasResolverResult; ErrorEl: TPasElement;
  RaiseOnError: boolean): boolean;
begin
  Result:=false;
  if ResolvedEl.BaseType in btAllRanges then
  else if (ResolvedEl.BaseType=btContext) then
    begin
    if ResolvedEl.LoTypeEl.ClassType=TPasEnumType then
    else if RaiseOnError then
      RaiseXExpectedButYFound(20170216152718,'ordinal value',GetElementTypeName(ResolvedEl.LoTypeEl),ErrorEl)
    else
      exit;
    end
  else if RaiseOnError then
    RaiseXExpectedButYFound(20170216152720,'ordinal value',BaseTypeNames[ResolvedEl.BaseType],ErrorEl)
  else
    exit;
  Result:=true;
end;

procedure TPasResolver.CombineArrayLitElTypes(Left, Right: TPasExpr;
  var LHS: TPasResolverResult; const RHS: TPasResolverResult);
// LHS defines the array element type
// check if RHS
var
  LBT, RBT: TResolverBaseType;
  C: TClass;
begin
  if (LHS.LoTypeEl=RHS.LoTypeEl) and (LHS.BaseType=RHS.BaseType) then
    exit; // exact same type

  LBT:=GetActualBaseType(LHS.BaseType);
  RBT:=GetActualBaseType(RHS.BaseType);
  if rrfReadable in LHS.Flags then
    begin
    if not (rrfReadable in RHS.Flags) then
      RaiseIncompatibleTypeRes(20170420004759,nIncompatibleTypesGotExpected,
        [],RHS,LHS,Right);
    // array of values
    if LBT in btAllBooleans then
      begin
      if RBT in btAllBooleans then
        begin
        LHS.BaseType:=GetCombinedBoolean(LBT,RBT,Right);
        exit;
        end;
      RaiseXExpectedButYFound(20170420093015,'boolean',BaseTypeNames[RHS.BaseType],Right);
      end
    else if LBT in btAllInteger then
      begin
      if RBT in btAllInteger then
        begin
        LHS.BaseType:=GetCombinedInt(LHS,RHS,Right);
        exit;
        end;
      RaiseXExpectedButYFound(20170420093019,'integer',BaseTypeNames[RHS.BaseType],Right);
      end
    else if LBT in btAllChars then
      begin
      if RBT in btAllChars then
        begin
        LHS.BaseType:=GetCombinedChar(LHS,RHS,Right);
        exit;
        end;
      RaiseXExpectedButYFound(20170420093024,'char',BaseTypeNames[RHS.BaseType],Right);
      end
    else if LBT in btAllStrings then
      begin
      if RBT in btAllStringAndChars then
        begin
        LHS.BaseType:=GetCombinedString(LHS,RHS,Right);
        exit;
        end;
      RaiseXExpectedButYFound(20170420102832,'string',BaseTypeNames[RHS.BaseType],Right);
      end
    else if LBT=btNil then
      begin
      if RBT=btNil then
        exit
      else if RBT=btPointer then
        begin
        LHS:=RHS;
        exit;
        end
      else if RBT=btContext then
       begin
       C:=RHS.LoTypeEl.ClassType;
       if (C=TPasClassType)
           or (C=TPasClassOfType)
           or (C=TPasPointerType)
           or ((C=TPasArrayType) and IsDynArray(RHS.LoTypeEl))
           or (C=TPasProcedureType)
           or (C=TPasFunctionType) then
         begin
         LHS:=RHS;
         exit;
         end;
       end;
      end
    else if LBT=btContext then
      begin
      C:=LHS.LoTypeEl.ClassType;
      if C=TPasEnumType then
        begin
        if LHS.LoTypeEl=RHS.LoTypeEl then
          exit;
        end
      else if C=TPasClassType then
        begin
        // array of class instances
        if RHS.LoTypeEl.ClassType<>TPasClassType then
          RaiseIncompatibleTypeRes(20170420135637,nIncompatibleTypesGotExpected,
            [],RHS,LHS,Right);
        if CheckClassIsClass(LHS.LoTypeEl,RHS.LoTypeEl)<cIncompatible then
          begin
          // right class type is a left class type -> ok
          exit;
          end
        else if CheckClassIsClass(RHS.LoTypeEl,LHS.LoTypeEl)<cIncompatible then
          begin
          // left class type is a right class type -> right is the new base class type
          LHS:=RHS;
          exit;
          end;
        end;
      end;
    end
  else
    begin
    // array of types
    if rrfReadable in RHS.Flags then
      RaiseIncompatibleTypeRes(20170420004925,nIncompatibleTypesGotExpected,
        [],RHS,LHS,Right);
    if LBT=btContext then
      begin
      if LHS.LoTypeEl.ClassType=TPasClassType then
        begin
        // array of class type
        if RHS.LoTypeEl.ClassType<>TPasClassType then
          RaiseIncompatibleTypeRes(20170420091839,nIncompatibleTypesGotExpected,
            [],RHS,LHS,Right);
        if CheckClassIsClass(LHS.LoTypeEl,RHS.LoTypeEl)<cIncompatible then
          begin
          // right class type is a left class type -> ok
          exit;
          end
        else if CheckClassIsClass(RHS.LoTypeEl,LHS.LoTypeEl)<cIncompatible then
          begin
          // left class type is a right class type -> right is the new base class type
          LHS:=RHS;
          exit;
          end;
        end;
      end;
    end;

  // can't combine
  if LHS.LoTypeEl=nil then
    RaiseXExpectedButYFound(20170420004537,'array element',BaseTypeNames[LHS.BaseType],Left);
  if RHS.LoTypeEl=nil then
    RaiseXExpectedButYFound(20170420004602,'array element',BaseTypeNames[RHS.BaseType],Right);

  RaiseIncompatibleTypeRes(20170420092625,nIncompatibleTypesGotExpected,
    [],RHS,LHS,Right);
end;

procedure TPasResolver.ConvertRangeToElement(
  var ResolvedEl: TPasResolverResult);
var
  TypeEl: TPasType;
begin
  if ResolvedEl.BaseType<>btRange then
    RaiseInternalError(20161001155732);
  if ResolvedEl.LoTypeEl=nil then
    if ResolvedEl.IdentEl<>nil then
      RaiseNotYetImplemented(20161001155747,ResolvedEl.IdentEl)
    else
      RaiseNotYetImplemented(20161001155834,ResolvedEl.ExprEl);
  TypeEl:=ResolvedEl.LoTypeEl;
  if TypeEl is TPasRangeType then
    ComputeElement(TPasRangeType(TypeEl).RangeExpr.Left,ResolvedEl,[rcConstant])
  else
    begin
    ResolvedEl.BaseType:=ResolvedEl.SubType;
    ResolvedEl.SubType:=btNone;
    end;
end;

function TPasResolver.HasWideCharCode(const Value: string): Boolean;
// True when a string literal token has a #-escape in the SURROGATE range,
// $D800..$DFFF. Those are the only codes a narrow encoding cannot hold: UTF-8
// has no form for a lone surrogate, so `#$D83D#$DE00` typed btString picked
// UTF8Encode's AnsiString overload and its two units were dropped. The type has
// to say UTF-16, which is what the evaluator already produces.
// Deliberately NOT every code above 255: those encode and round-trip through
// UTF-8 perfectly well, and calling them wide changes the meaning of a narrow
// literal (tutf8cpl's `'e'#$20DD#$1DE0'b'` is a UTF-8 AnsiString).
var
  p, l, base: Integer;
  u: LongWord;
  c: AnsiChar;
begin
  Result:=false;
  l:=length(Value);
  p:=1;
  while p<=l do
    begin
    c:=Value[p];
    if c='''' then
      begin
      // quoted segment; '' is an embedded quote, and a # inside it is content
      inc(p);
      while p<=l do
        if Value[p]='''' then
          begin
          inc(p);
          if (p<=l) and (Value[p]='''') then
            inc(p)
          else
            break;
          end
        else
          inc(p);
      end
    else if c='#' then
      begin
      inc(p);
      if p>l then exit;
      base:=10;
      case Value[p] of
      '$': begin base:=16; inc(p); end;
      '&': begin base:=8; inc(p); end;
      '%': begin base:=2; inc(p); end;
      end;
      u:=0;
      while p<=l do
        begin
        c:=Value[p];
        case c of
        '0'..'9':
          if ord(c)-ord('0')<base then
            u:=u*LongWord(base)+LongWord(ord(c)-ord('0'))
          else
            break;
        'a'..'f':
          if base=16 then
            u:=u*16+LongWord(ord(c)-ord('a'))+10
          else
            break;
        'A'..'F':
          if base=16 then
            u:=u*16+LongWord(ord(c)-ord('A'))+10
          else
            break;
        else
          break;
        end;
        if (u>=$D800) and (u<=$DFFF) then exit(true);
        inc(p);
        end;
      end
    else
      inc(p);
    end;
end;

function TPasResolver.IsCharLiteral(const Value: string; ErrorPos: TPasElement
  ): TResolverBaseType;

{$IFDEF FPC_HAS_CPSTRING}
{$IF SIZEOF(CHAR)=1}
  {$DEFINE USESINGLEBYTE}
{$ENDIF}
{$ENDIF}

// returns true if Value is a Pascal char literal
// btAnsiChar: #65, #$50, ^G, 'a'
// btWideChar: #10000, 'ä'
var
  i: SizeInt;
  p, base, l: Integer;
begin
  Result:=btNone;
  //writeln('TPasResolver.IsCharLiteral ',BaseTypeChar,' "',Value,'" l=',length(Value));
  l:=length(Value);
  if l=0 then exit;
  p:=1;
  case Value[1] of
  '''':
    begin
    inc(p);
    if p>l then exit;
    {$ifdef USESINGLEBYTE}
    case Value[2] of
    '''':
      if Value='''''''''' then
        Result:=btAnsiChar; // ''''
    #32..#38,#40..#191:
      if (l=3) and (Value[3]='''') then
        Result:=btAnsiChar; // e.g. 'a'
    #192..#255:
      if BaseTypeChar=btWideChar then
        begin
        // default AnsiChar is widechar: UTF-8 'ä' is a widechar
        i:=Utf8CodePointLen(@Value[2],4,false);
        //writeln('TPasResolver.IsCharLiteral "',Value,'" ',length(Value),' i=',i);
        if i<2 then
          exit;
        p:=2+i;
        if (p=l) and (Value[p]='''') then
          // single UTF-8 codepoint
          Result:=btWideChar;
        end;
    end;
    {$else}
    case Value[p] of
    '''':
      if (p+2=l) and (Value[p+1]='''') and (Value[p+2]='''') then
        Result:=btWideChar; // ''''
    #$DC00..#$DFFF: ;
    else
      if (l=3) and (Value[3]='''') then
        if Ord(Value[2])<128 then
          {$IFNDEF PAS2JS}
          Result:=btAnsiChar // e.g. 'a'
          {$ELSE}
          Result:=btWideChar
          {$ENDIF}
        else
          Result:=btWideChar; // e.g. 'a'
    end;
    {$endif}
    end;
  '#':
    begin
    inc(p);
    if p>l then exit;
    case Value[p] of
    '$': begin base:=16; inc(p); end;
    '&': begin base:=8; inc(p); end;
    '%': begin base:=2; inc(p); end;
    '0'..'9': base:=10;
    else RaiseNotYetImplemented(20170728142709,ErrorPos);
    end;
    i:=0;
    while p<=l do
      begin
      case Value[p] of
      '0'..'9': i:=i*base+ord(Value[p])-ord('0');
      'A'..'Z': i:=i*base+ord(Value[p])-ord('A')+10;
      'a'..'z': i:=i*base+ord(Value[p])-ord('a')+10;
      else break;
      end;
      inc(p);
      end;
    if p>l then
      begin
      {$ifdef FPC_HAS_CPSTRING}
      if i<256 then
        Result:=btAnsiChar
      else
      {$endif}
        Result:=btWideChar;
      end;
    end;
  '^':
    begin
    if (l=2) and (Value[2] in ['a'..'z','A'..'Z']) then
      Result:={$ifdef FPC_HAS_CPSTRING}btAnsiChar{$else}btWideChar{$endif};
    end;
  end;
  if Result in [{$ifdef FPC_HAS_CPSTRING}btAnsiChar,{$endif}btWideChar] then
    begin
    if FBaseTypes[Result]=nil then
      begin
      {$ifdef FPC_HAS_CPSTRING}
      if Result=btAnsiChar then
        Result:=btWideChar
      else
      {$endif}
        Result:=btChar;
      end;
    if Result=BaseTypeChar then
      Result:=btChar;
    end;
end;

function TPasResolver.CheckForIn(Loop: TPasImplForLoop; const VarResolved,
  InResolved: TPasResolverResult): boolean;
begin
  Result:=false;
  if Loop=nil then ;
  if VarResolved.BaseType=btCustom then ;
  if InResolved.BaseType=btCustom then ;
end;

function TPasResolver.CheckForInClassOrRec(Loop: TPasImplForLoop; const VarResolved,
  InResolved: TPasResolverResult): boolean;
var
  LoTypeEl: TPasType;
  EnumeratorClass: TPasClassType;
  EnumeratorScope: TPasDotClassOrRecordScope;
  Getter, MoveNext, Current: TPasIdentifier;
  GetterFunc, MoveNextFunc: TPasFunction;
  ptm: TProcTypeModifier;
  ResultResolved, MoveNextResolved, CurrentResolved: TPasResolverResult;
  CurrentProp: TPasProperty;
  ForScope: TPasForLoopScope;
  DotScope: TPasDotBaseScope;
begin
  Result:=false;
  if InResolved.IdentEl is TPasType then
    RaiseMsg(20190120180525,nCannotFindEnumeratorForType,sCannotFindEnumeratorForType,
      [GetBaseDescription(InResolved)],Loop.StartExpr);
  if not (rrfReadable in InResolved.Flags) then
    RaiseMsg(20171221195421,nCannotFindEnumeratorForType,sCannotFindEnumeratorForType,
      [GetBaseDescription(InResolved)],Loop.StartExpr);

  LoTypeEl:=InResolved.LoTypeEl;
  if LoTypeEl=nil then exit;

  // check function InVar.GetEnumerator
  DotScope:=PushDotScope(InResolved.HiTypeEl);
  if DotScope=nil then
    exit;
  // find aRecord.GetEnumerator
  Getter:=DotScope.FindIdentifier('GetEnumerator');
  PopScope;
  if Getter=nil then
    begin
    if LoTypeEl is TPasMembersType then
      RaiseIdentifierNotFound(20171221191511,'GetEnumerator',Loop.StartExpr)
    else
      exit;
    end;
  // check is function
  if Getter.Element.ClassType<>TPasFunction then
    RaiseContextXExpectedButYFound(20171221191638,'GetEnumerator','function',GetElementTypeName(Getter.Element),Loop.StartExpr);
  GetterFunc:=TPasFunction(Getter.Element);
  // check visibility
  if not (GetterFunc.Visibility in [visPublic,visPublished]) then
    RaiseContextXExpectedButYFound(20171221191824,'function GetEnumerator','public',VisibilityNames[GetterFunc.Visibility],Loop.StartExpr);
  // check arguments
  if GetterFunc.FuncType.Args.Count>0 then
    RaiseContextXExpectedButYFound(20171221191944,'function GetEnumerator','no arguments',IntToStr(GetterFunc.ProcType.Args.Count),Loop.StartExpr);
  // check proc type modifiers
  for ptm in GetterFunc.ProcType.Modifiers do
    if not (ptm in [ptmOfObject]) then
      RaiseContextXInvalidY(20171221193455,'function GetEnumerator','modifier '+ProcTypeModifiers[ptm],Loop.StartExpr);
  // check result type
  ComputeResultElement(GetterFunc.FuncType.ResultEl,ResultResolved,[rcCall]);
  if (ResultResolved.BaseType<>btContext) then
    RaiseContextXExpectedButYFound(20171221193749,'function GetEnumerator','result class',GetTypeDescription(ResultResolved),Loop.StartExpr);
  LoTypeEl:=ResultResolved.LoTypeEl;
  // The enumerator may be a class (classic) or a record (advanced records).
  if not ((LoTypeEl is TPasClassType) or (LoTypeEl is TPasRecordType)) then
    RaiseContextXExpectedButYFound(20171221193749,'function GetEnumerator','result class',GetTypeDescription(ResultResolved.LoTypeEl),Loop.StartExpr);
  if not (rrfReadable in ResultResolved.Flags) then
    RaiseContextXExpectedButYFound(20171221195506,'function GetEnumerator','result class instance',GetTypeDescription(ResultResolved.LoTypeEl),Loop.StartExpr);

  // find function MoveNext: boolean in the Enumerator class/record. Push the
  // matching member scope so MoveNext/Current resolve against its own members.
  if LoTypeEl is TPasClassType then
    begin
    EnumeratorClass:=TPasClassType(LoTypeEl);
    EnumeratorScope:=PushClassDotScope(EnumeratorClass);
    end
  else
    EnumeratorScope:=PushRecordDotScope(TPasRecordType(LoTypeEl));
  MoveNext:=EnumeratorScope.FindIdentifier('MoveNext');
  if MoveNext=nil then
    RaiseIdentifierNotFound(20171221195632,'MoveNext',Loop.StartExpr);
  // check is function
  if MoveNext.Element.ClassType<>TPasFunction then
    RaiseContextXExpectedButYFound(20171221195651,'MoveNext','function',GetElementTypeName(MoveNext.Element),Loop.StartExpr);
  MoveNextFunc:=TPasFunction(MoveNext.Element);
  // check visibility
  if not (MoveNextFunc.Visibility in [visPublic,visPublished]) then
    RaiseContextXExpectedButYFound(20171221195712,'function MoveNext','public',VisibilityNames[MoveNextFunc.Visibility],Loop.StartExpr);
  // check arguments
  if MoveNextFunc.FuncType.Args.Count>0 then
    RaiseContextXExpectedButYFound(20171221195723,'function MoveNext','no arguments',IntToStr(MoveNextFunc.ProcType.Args.Count),Loop.StartExpr);
  // check proc type modifiers
  for ptm in MoveNextFunc.ProcType.Modifiers do
    if not (ptm in [ptmOfObject]) then
      RaiseContextXInvalidY(20171221195732,'function MoveNext','modifier '+ProcTypeModifiers[ptm],Loop.StartExpr);
  // check result type
  ComputeResultElement(MoveNextFunc.FuncType.ResultEl,MoveNextResolved,[rcCall]);
  if not (MoveNextResolved.BaseType in btAllBooleans) then
    RaiseContextXExpectedButYFound(20171221200337,'function MoveNext','result boolean',GetTypeDescription(MoveNextResolved),Loop.StartExpr);

  // check property Current
  Current:=EnumeratorScope.FindIdentifier('Current');
  if Current=nil then
    RaiseIdentifierNotFound(20171221200433,'Current',Loop.StartExpr);
  // check is property
  if Current.Element.ClassType<>TPasProperty then
    RaiseContextXExpectedButYFound(20171221200508,'Current','property',GetElementTypeName(Current.Element),Loop.StartExpr);
  CurrentProp:=TPasProperty(Current.Element);
  // check visibility
  if not (CurrentProp.Visibility in [visPublic,visPublished]) then
    RaiseContextXExpectedButYFound(20171221200546,'property Current','public',VisibilityNames[CurrentProp.Visibility],Loop.StartExpr);
  // check arguments
  if CurrentProp.Args.Count>0 then
    RaiseContextXExpectedButYFound(20171221200638,'property Current','no arguments',IntToStr(CurrentProp.Args.Count),Loop.StartExpr);
  // check readable
  if GetPasPropertyGetter(CurrentProp)=nil then
    RaiseContextXInvalidY(20171221200823,'property Current','read accessor',Loop.StartExpr);
  // check result type fits for-loop variable
  ComputeElement(CurrentProp,CurrentResolved,[rcType]);
  if CheckAssignResCompatibility(VarResolved,CurrentResolved,Loop.VariableName,false)=cIncompatible then
    RaiseIncompatibleTypeRes(20171221200018,nIncompatibleTypesGotExpected,[],VarResolved,CurrentResolved,Loop.VariableName);

  PopScope; // pop EnumeratorScope

  ForScope:=Loop.CustomData as TPasForLoopScope;
  ForScope.GetEnumerator:=GetterFunc;
  ForScope.MoveNext:=MoveNextFunc;
  ForScope.Current:=CurrentProp;
  Result:=true;
end;

function TPasResolver.CheckBuiltInMinParamCount(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; MinCount: integer; RaiseOnError: boolean): boolean;
begin
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)<MinCount) then
    begin
    if RaiseOnError then
      RaiseMsg(20170216152248,nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Expr);
    exit(false);
    end;
  Result:=true;
end;

function TPasResolver.CheckBuiltInMaxParamCount(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; MaxCount: integer; RaiseOnError: boolean;
  Signature: string): integer;
begin
  if length(Params.Params)>MaxCount then
    begin
    if RaiseOnError then
      begin
      if Signature='' then Signature:=Proc.Signature;
      RaiseMsg(20170329154348,nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Signature],Params.Params[MaxCount]);
      end;
    exit(cIncompatible);
    end;

  Result:=cExact;
end;

function TPasResolver.CheckRaiseTypeArgNo(id: TMaxPrecInt; ArgNo: integer;
  Param: TPasExpr; const ParamResolved: TPasResolverResult; Expected: string;
  RaiseOnError: boolean): integer;
begin
  // An argument that is still an unbound generic template - or a member read off
  // one, which ResolveSubIdent deliberately leaves unresolved - can only be
  // judged at specialization, where the built-in is checked again
  // (rtl-generics writes Length(FQueue.FItems) with FQueue's type still open).
  if IsGenericTemplType(ParamResolved) or IsDeferredTemplMember(Param) then
    exit(cGenericExact);
  if RaiseOnError then
    RaiseMsg(id,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
      [IntToStr(ArgNo),GetResolverResultDescription(ParamResolved,true),Expected],Param);
  Result:=cIncompatible;
end;

function TPasResolver.FindUsedUnitnameInSection(const aName: string; Section: TPasSection): TPasModule;
var
  Clause: TPasUsesClause;
  i: Integer;
  Use: TPasUsesUnit;
  ModName: String;
begin
  Result:=nil;
  if (Section=nil) then exit;
  Clause:=Section.UsesClause;
  for i:=0 to length(Clause)-1 do
    begin
    Use:=Clause[i];
    if (Use.Module=nil) or not (Use.Module is TPasModule) then continue;
    ModName:=Use.Module.Name;
    if CompareText(ModName,aName)=0 then
      exit(TPasModule(Use.Module));
    end;
end;

function TPasResolver.FindUsedUnitname(const aName: string; aMod: TPasModule): TPasModule;
var
  C: TClass;
begin
  C:=aMod.ClassType;
  if C.InheritsFrom(TPasProgram) then
    Result:=FindUsedUnitnameInSection(aName,TPasProgram(aMod).ProgramSection)
  else if C.InheritsFrom(TPasLibrary) then
    Result:=FindUsedUnitnameInSection(aName,TPasLibrary(aMod).LibrarySection)
  else
    begin
    Result:=FindUsedUnitnameInSection(aName,aMod.InterfaceSection);
    if Result<>nil then exit;
    Result:=FindUsedUnitnameInSection(aName,aMod.ImplementationSection);
    end
end;

procedure TPasResolver.FinishAssertCall(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr);
var
  aMod: TPasModule;
  ModScope: TPasModuleScope;
  aConstructor: TPasConstructor;
begin
  if Proc=nil then ;
  aMod:=RootElement;
  ModScope:=aMod.CustomData as TPasModuleScope;
  if not (pmsfAssertSearched in ModScope.Flags) then
    FindAssertExceptionConstructors(nil); // no ErrorEl
  if ModScope.AssertClass=nil then exit;
  if length(Params.Params)>1 then
    aConstructor:=ModScope.AssertMsgConstructor
  else
    aConstructor:=ModScope.AssertDefConstructor;
  if aConstructor=nil then exit;
  CreateReference(aConstructor,Params,rraRead);
end;

function TPasResolver.FindSystemIdentifier(const aUnitName, aName: string;
  ErrorEl: TPasElement): TPasElement;
var
  aMod, UtilsMod: TPasModule;
  SectionScope: TPasSectionScope;
  Identifier: TPasIdentifier;
begin
  Result:=nil;

  // find unit in uses clauses
  aMod:=RootElement;
  UtilsMod:=FindUsedUnitname(aUnitName,aMod);
  if UtilsMod=nil then
    if ErrorEl<>nil then
      RaiseIdentifierNotFound(20200523224738,'unit '+aUnitName,ErrorEl)
    else
      exit;

  // find class in interface
  if UtilsMod.InterfaceSection=nil then
    if ErrorEl<>nil then
      RaiseIdentifierNotFound(20200523224831,aUnitName+'.'+aName,ErrorEl)
    else
      exit;
  SectionScope:=NoNil(UtilsMod.InterfaceSection.CustomData) as TPasSectionScope;
  Identifier:=SectionScope.FindLocalIdentifier(aName);
  if Identifier=nil then
    if ErrorEl<>nil then
      RaiseIdentifierNotFound(20200523224841,aUnitName+'.'+aName,ErrorEl)
    else
      exit;
  Result:=Identifier.Element;
end;

function TPasResolver.FindSystemClassType(const aUnitName, aClassName: string;
  ErrorEl: TPasElement): TPasClassType;
var
  El: TPasElement;
begin
  Result:=nil;

  El:=FindSystemIdentifier(aUnitName,aClassName,ErrorEl);
  if not (El is TPasClassType) then
    if ErrorEl<>nil then
      RaiseXExpectedButYFound(20180119172517,'class '+aClassName,GetElementTypeName(El),ErrorEl)
    else
      exit;
  Result:=TPasClassType(El);

  if Result.IsForward then
    if ErrorEl<>nil then
      RaiseXExpectedButYFound(20200523225546,'class '+aClassName,'forward '+GetTypeDescription(Result,true),ErrorEl)
    else
      exit;

  if Result.ObjKind<>okClass then
    if ErrorEl<>nil then
      RaiseXExpectedButYFound(20180321163200,'class '+aClassName,GetTypeDescription(Result,true),ErrorEl)
    else
      exit;
end;

function TPasResolver.FindSystemClassTypeAndConstructor(const aUnitName,
  aClassName: string; out aClass: TPasClassType; out
  aConstructor: TPasConstructor; ErrorEl: TPasElement): boolean;
var
  Identifier: TPasIdentifier;
  ClassScope: TPasClassScope;
begin
  Result:=false;
  aClass:=nil;
  aConstructor:=nil;

  aClass:=FindSystemClassType(aUnitName,aClassName,ErrorEl);
  if aClass=nil then exit;

  ClassScope:=NoNil(aClass.CustomData) as TPasClassScope;
  repeat
    Identifier:=ClassScope.FindIdentifier('create');
    while Identifier<>nil do
      begin
      if Identifier.Element.ClassType=TPasConstructor then
        begin
        aConstructor:=TPasConstructor(Identifier.Element);
        if aConstructor.ProcType.Args.Count=0 then
          exit(true);
        end;
      Identifier:=Identifier.NextSameIdentifier;
      end;
    ClassScope:=ClassScope.AncestorScope;
  until ClassScope=nil;
  aConstructor:=nil;

  if ErrorEl<>nil then
    RaiseIdentifierNotFound(20200523224856,'constructor '+aClassName,ErrorEl);
end;

procedure TPasResolver.FindAssertExceptionConstructors(ErrorEl: TPasElement);
var
  aMod: TPasModule;
  ModScope: TPasModuleScope;
  Identifier: TPasIdentifier;
  aClass: TPasClassType;
  ClassScope: TPasClassScope;
  aConstructor: TPasConstructor;
  Arg: TPasArgument;
  ArgResolved: TPasResolverResult;
begin
  aMod:=RootElement;
  ModScope:=aMod.CustomData as TPasModuleScope;
  if pmsfAssertSearched in ModScope.Flags then exit;
  Include(ModScope.Flags,pmsfAssertSearched);
  FindSystemClassTypeAndConstructor('sysutils','EAssertionFailed',aClass,aConstructor,ErrorEl);
  if aClass=nil then
    exit;
  ClassScope:=NoNil(aClass.CustomData) as TPasClassScope;
  ModScope.AssertClass:=aClass;
  repeat
    Identifier:=ClassScope.FindIdentifier('create');
    while Identifier<>nil do
      begin
      if Identifier.Element.ClassType=TPasConstructor then
        begin
        aConstructor:=TPasConstructor(Identifier.Element);
        //writeln('TPasResolver.FindAssertExceptionConstructors ',aConstructor.Name,' ',aConstructor.ProcType.Args.Count);
        if aConstructor.ProcType.Args.Count=0 then
          begin
          if ModScope.AssertDefConstructor=nil then
            ModScope.AssertDefConstructor:=aConstructor;
          end
        else if aConstructor.ProcType.Args.Count=1 then
          begin
          if ModScope.AssertMsgConstructor=nil then
            begin
            Arg:=TPasArgument(aConstructor.ProcType.Args[0]);
            //writeln('TPasResolver.FindAssertExceptionConstructors ',GetObjName(Arg.ArgType),' ',GetObjName(BaseTypes[BaseTypeString]));
            ComputeElement(Arg.ArgType,ArgResolved,[rcType]);
            if ArgResolved.BaseType in btAllStrings then
              ModScope.AssertMsgConstructor:=aConstructor;
            end;
          end;
        end;
      Identifier:=Identifier.NextSameIdentifier;
      end;
    ClassScope:=ClassScope.AncestorScope;
  until ClassScope=nil;
end;

procedure TPasResolver.FindRangeErrorConstructors(ErrorEl: TPasElement);
var
  aMod: TPasModule;
  ModScope: TPasModuleScope;
  aClass: TPasClassType;
  aConstructor: TPasConstructor;
begin
  aMod:=RootElement;
  ModScope:=aMod.CustomData as TPasModuleScope;
  if pmsfRangeErrorSearched in ModScope.Flags then exit;
  Include(ModScope.Flags,pmsfRangeErrorSearched);

  FindSystemClassTypeAndConstructor('sysutils','ERangeError',aClass,aConstructor,ErrorEl);
  ModScope.RangeErrorClass:=aClass;
  ModScope.RangeErrorConstructor:=aConstructor;
end;

function TPasResolver.FindTVarRec(ErrorEl: TPasElement): TPasRecordType;
var
  aMod, UtilsMod: TPasModule;
  SectionScope: TPasSectionScope;
  Identifier: TPasIdentifier;
  El: TPasElement;
  ModScope: TPasModuleScope;
begin
  aMod:=RootElement;
  ModScope:=aMod.CustomData as TPasModuleScope;
  Result:=ModScope.SystemTVarRec;
  if Result<>nil then exit;

  // find unit in uses clauses; the system unit itself has no uses clause, so
  // its own `array of const` parameters must look the record up locally
  if SameText(aMod.Name,'system') then
    UtilsMod:=aMod
  else
    UtilsMod:=FindUsedUnitname('system',aMod);
  if UtilsMod=nil then
    RaiseIdentifierNotFound(20190215101210,'System.TVarRec',ErrorEl);

  // find class in interface
  if UtilsMod.InterfaceSection=nil then
    RaiseIdentifierNotFound(20190215101231,'System.TVarRec',ErrorEl);

  SectionScope:=NoNil(UtilsMod.InterfaceSection.CustomData) as TPasSectionScope;
  Identifier:=SectionScope.FindLocalIdentifier('TVarRec');
  if Identifier=nil then
    RaiseIdentifierNotFound(20190215101253,'System.TVarRec',ErrorEl);
  El:=Identifier.Element;
  if not (El is TPasRecordType) then
    RaiseXExpectedButYFound(20190215101310,'record TVarRec',GetElementTypeName(El),ErrorEl);
  Result:=TPasRecordType(El);
  ModScope.SystemTVarRec:=Result;
end;

function TPasResolver.GetTVarRec(El: TPasArrayType): TPasRecordType;
var
  aModule: TPasModule;
  ModScope: TPasModuleScope;
begin
  aModule:=El.GetModule;
  ModScope:=aModule.CustomData as TPasModuleScope;
  Result:=ModScope.SystemTVarRec;
  if Result=nil then
    RaiseNotYetImplemented(20190215111924,El,'missing System.TVarRec');
end;

function TPasResolver.FindDefaultConstructor(aClass: TPasClassType
  ): TPasConstructor;
var
  ClassScope: TPasClassScope;
  Identifier: TPasIdentifier;
  El: TPasElement;
  HasOverload: Boolean;
  Proc: TPasProcedure;
begin
  Result:=nil;
  if (aClass=nil) or aClass.IsExternal or (aClass.ObjKind<>okClass) then exit;
  ClassScope:=aClass.CustomData as TPasClassScope;
  repeat
    Identifier:=ClassScope.FindLocalIdentifier('create');
    if Identifier<>nil then
      begin
      HasOverload:=false;
      while Identifier<>nil do
        begin
        El:=Identifier.Element;
        if not (El is TPasProcedure) then exit;
        Proc:=TPasProcedure(El);
        if Proc.ClassType=TPasConstructor then
          begin
          if Proc.ProcType.Args.Count=0 then
            exit(TPasConstructor(El));
          end;
        if IsProcOverload(Proc) then
          HasOverload:=true;
        Identifier:=Identifier.NextSameIdentifier;
        end;
      if not HasOverload then exit;
      end;
    ClassScope:=ClassScope.AncestorScope;
  until false;
end;

function TPasResolver.GetTypeInfoParamType(Param: TPasExpr; out
  ParamResolved: TPasResolverResult; LoType: boolean): TPasType;
var
  Decl: TPasElement;
begin
  Result:=nil;
  // check type or var
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  Decl:=ParamResolved.IdentEl;
  if Decl=nil then exit;
  if Decl is TPasType then
    Result:=TPasType(Decl)
  else if Decl is TPasVariable then
    Result:=TPasVariable(Decl).VarType
  else if Decl.ClassType=TPasArgument then
    Result:=TPasArgument(Decl).ArgType
  else if Decl.ClassType=TPasResultElement then
    Result:=TPasResultElement(Decl).ResultType
  else if (Decl is TPasProcedure)
      and (TPasProcedure(Decl).ProcType is TPasFunctionType) then
    Result:=TPasFunctionType(TPasProcedure(Decl).ProcType).ResultEl.ResultType;
  {$IFDEF VerbosePasResolver}
  {AllowWriteln}
  if Result=nil then
    writeln('TPasResolver.GetTypeInfoParamType Decl=',GetObjName(Decl),' ParamResolved=',GetResolverResultDbg(ParamResolved));
  {AllowWriteln-}
  {$ENDIF}
  if LoType then
    Result:=ResolveAliasType(Result);
end;

procedure TPasResolver.OnExprEvalLog(Sender: TResExprEvaluator;
  const id: TMaxPrecInt; MsgType: TMessageType; MsgNumber: integer;
  const Fmt: String; Args: array of const;
  PosEl: TPasElement);
begin
  if MsgType<=mtError then
    RaiseMsg(id,MsgNumber,Fmt,Args,PosEl)
  else
    LogMsg(id,MsgType,MsgNumber,Fmt,Args,PosEl);
  if Sender=nil then ;
end;

function TPasResolver.CreateExprEvaluator: TResExprEvaluator;
begin
  Result:=TResExprEvaluator.Create;
end;


function TPasResolver.OnExprEvalIdentifier(Sender: TResExprEvaluator;
  Expr: TPrimitiveExpr; Flags: TResEvalFlags): TResEvalValue;
var
  Ref: TResolvedReference;
  Decl: TPasElement;
  C: TClass;
  ResolvedType: TPasResolverResult;
  EnumValue: TPasEnumValue;
  EvalFlags: TResEvalFlags;
  ParentEl: TPasElement;
  i: Integer;
  Ident: TPasIdentifier;
begin
  Result:=nil;
  if not (Expr.CustomData is TResolvedReference) then
    begin
    // A member of a still-unbound template type has no constant value yet — nil
    // means "not a constant" and the specialized copy is evaluated instead.
    // fcl-stl's ghashmap range-checks the assignment bs:=(FData[Fh]).size.
    if IsDeferredTemplMember(Expr) then
      exit;
    RaiseNotYetImplemented(20170518203134,Expr,GetObjName(Expr.CustomData));
    end;
  Ref:=TResolvedReference(Expr.CustomData);
  Decl:=Ref.Declaration;
  {$IFDEF VerbosePasResEval}
  writeln('TPasResolver.OnExprEvalIdentifier Value=',Expr.Value,' Decl=',GetObjName(Decl));
  {$ENDIF}
  C:=Decl.ClassType;
  if C=TPasConst then
    begin
    if (TPasConst(Decl).Expr<>nil)
        and (TPasConst(Decl).IsConst or (TPasConst(Decl).VarType=nil)) then
      begin
      if TPasConst(Decl).VarType<>nil then
        begin
        // typed const
        ComputeElement(TPasConst(Decl).VarType,ResolvedType,[rcType]);
        end
      else
        ResolvedType.BaseType:=btNone;
      EvalFlags:=Flags;
      if not (refConstExt in EvalFlags) then
        Include(EvalFlags,refConst);
      Result:=fExprEvaluator.Eval(TPasConst(Decl).Expr,EvalFlags);
      if Result<>nil then
        begin
        if (Result.Element<>nil) and (Result.Element<>TPasConst(Decl).Expr) then
          Result:=Result.Clone;
        Result.IdentEl:=Decl;
        if TPasConst(Decl).VarType<>nil then
          begin
          // typed const
          if Result.Kind=revkInt then
            case ResolvedType.BaseType of
            btByte: TResEvalInt(Result).Typed:=reitByte;
            btShortInt: TResEvalInt(Result).Typed:=reitShortInt;
            btWord: TResEvalInt(Result).Typed:=reitWord;
            btSmallInt: TResEvalInt(Result).Typed:=reitSmallInt;
            btUIntSingle: TResEvalInt(Result).Typed:=reitUIntSingle;
            btIntSingle: TResEvalInt(Result).Typed:=reitIntSingle;
            btLongWord: TResEvalInt(Result).Typed:=reitLongWord;
            btLongint: TResEvalInt(Result).Typed:=reitLongInt;
            btUIntDouble: TResEvalInt(Result).Typed:=reitUIntDouble;
            {$ifdef HasInt64}
            btIntDouble: TResEvalInt(Result).Typed:=reitIntDouble;
            btInt64: TResEvalInt(Result).Typed:=reitNone; // default
            // A QWord-typed constant whose value came back SIGNED fits Int64,
            // or the evaluator would have produced a revkUInt - so the default
            // full-range marker is right. cutils.pas has `const c: SizeUInt`.
            btQWord: TResEvalInt(Result).Typed:=reitNone;
            {$else}
            btIntDouble: TResEvalInt(Result).Typed:=reitNone; // default
            {$endif}
            else
              ReleaseEvalValue(Result);
              RaiseNotYetImplemented(20170624181050,TPasConst(Decl).VarType);
            end;
          end;
        exit;
        end;
      end
    else if vmExternal in TPasConst(Decl).VarModifiers then
      begin
      Result:=TResEvalExternal.Create;
      Result.IdentEl:=Decl;
      exit;
      end;
    if refConst in Flags then
      begin
      ReleaseEvalValue(Result);
      RaiseConstantExprExp(20170518214928,Expr);
      end;
    end
  else if C=TPasResString then
    begin
    // A resourcestring is a valid constant value ONLY as the DIRECT initializer
    // of a typed constant/variable, or a direct element of a typed-const array
    // (it takes the resourcestring's default string value; runtime-localizable).
    // It is NOT foldable inside a compound expression (e.g. 'Pre'+Foo), and not
    // valid for an untyped const. Real FPC: tstring3 accepts (Foo, Bar) as a
    // typed const array; 'Pre'+Foo and untyped `const B = Foo` are rejected.
    // A field of a typed-const record initializer counts too, as in FPC and Delphi.
    ParentEl:=Expr.Parent;
    if (((ParentEl is TPasVariable) and (TPasVariable(ParentEl).VarType<>nil))
         or (ParentEl is TArrayValues) or (ParentEl is TParamsExpr)
         or (ParentEl is TRecordValues))
        and (TPasResString(Decl).Expr<>nil) then
      begin
      Result:=fExprEvaluator.Eval(TPasResString(Decl).Expr,Flags);
      if Result<>nil then
        begin
        if (Result.Element<>nil) and (Result.Element<>TPasResString(Decl).Expr) then
          Result:=Result.Clone;
        Result.IdentEl:=Decl;
        exit;
        end;
      end;
    // else fall through -> nil -> "Constant expression expected" (compound/untyped)
    end
  else if C=TPasEnumValue then
    begin
    EnumValue:=TPasEnumValue(Decl);
    //EnumType:=EnumValue.Parent as TPasEnumType;
    Result:=TResEvalEnum.CreateValue(Integer(GetEnumValueOrdinal(EnumValue)),EnumValue);
    exit;
    end
  else if (C=TPasGenericTemplateType) and TPasGenericTemplateType(Decl).IsConst then
    begin
    // Const generic template param: during specialization, a synthetic TPasConst
    // may hold the actual value in the scope chain (the resolved reference still
    // points to the original template type) — look it up and evaluate it.
    for i:=ScopeCount-1 downto 0 do
      if Scopes[i] is TPasIdentifierScope then
        begin
        Ident:=TPasIdentifierScope(Scopes[i]).FindIdentifier(Decl.Name);
        if (Ident<>nil) and (Ident.Element<>Decl)
            and (Ident.Element.ClassType=TPasConst)
            and TPasConst(Ident.Element).IsConst
            and (TPasConst(Ident.Element).Expr<>nil) then
          begin
          EvalFlags:=Flags;
          if not (refConstExt in EvalFlags) then
            Include(EvalFlags,refConst);
          Result:=fExprEvaluator.Eval(TPasConst(Ident.Element).Expr,EvalFlags);
          if Result<>nil then
            begin
            if (Result.Element<>nil) and (Result.Element<>TPasConst(Ident.Element).Expr) then
              Result:=Result.Clone;
            Result.IdentEl:=Ident.Element;
            exit;
            end;
          if [refConst,refConstExt]*Flags<>[] then
            RaiseConstantExprExp(20170518213616,Expr);
          exit;
          end;
        end;
    // No substitution yet — this is the generic DEFINITION. Return an external
    // const placeholder so refConstExt callers accept it (the value is folded
    // later, at specialization) rather than raising "Constant expression expected".
    Result:=TResEvalExternal.Create;
    Result.IdentEl:=Decl;
    exit;
    end
  else if C.InheritsFrom(TPasType) then
    Result:=EvalTypeRange(TPasType(Decl),Flags);
  {$IFDEF VerbosePasResEval}
  writeln('TPasResolver.OnExprEvalIdentifier END Result=',dbgs(Result),' refConst=',refConst in Flags,' refConstExt=',refConstExt in Flags);
  {$ENDIF}
  if (Result=nil) and ([refConst,refConstExt]*Flags<>[]) then
    begin
    Result:=EvalNativeAddressOf(Expr,Flags);
    if Result=nil then
      RaiseConstantExprExp(20170518213616,Expr);
    end;
  if Sender=nil then ;
end;

function TPasResolver.OnExprEvalParams(Sender: TResExprEvaluator;
  Params: TParamsExpr; Flags: TResEvalFlags): TResEvalValue;
var
  Ref: TResolvedReference;
  Decl: TPasElement;
  C: TClass;
  BuiltInProc: TResElDataBuiltInProc;
  bt: TResolverBaseType;
  ResolvedEl: TPasResolverResult;
  TypeEl: TPasType;
  ValueExpr: TPasExpr;
begin
  Result:=nil;
  case Params.Kind of
  pekArrayParams: ;
  pekFuncParams:
    begin
    // `System.THandle(x)`: the reference is on the name after the dot
    ValueExpr:=Params.Value;
    while (ValueExpr is TBinaryExpr) and (TBinaryExpr(ValueExpr).OpCode=eopSubIdent)
        and not (ValueExpr.CustomData is TResolvedReference) do
      ValueExpr:=TBinaryExpr(ValueExpr).right;
    if ValueExpr.CustomData is TResolvedReference then
      begin
      Ref:=TResolvedReference(ValueExpr.CustomData);
      Decl:=Ref.Declaration;
      if Decl is TPasType then
        Decl:=ResolveAliasType(TPasType(Decl));
      C:=Decl.ClassType;

      if C=TPasUnresolvedSymbolRef then
        begin
        if Decl.CustomData is TResElDataBuiltInProc then
          begin
          BuiltInProc:=TResElDataBuiltInProc(Decl.CustomData);
          {$IFDEF VerbosePasResEval}
          writeln('TPasResolver.OnExprEvalParams Calling BuiltInProc ',Decl.Name,' ',ResolverBuiltInProcNames[BuiltInProc.BuiltIn]);
          {$ENDIF}
          if BuiltInProc.Eval<>nil then
            BuiltInProc.Eval(BuiltInProc,Params,Flags,Result)
          else
            case BuiltInProc.BuiltIn of
              bfAssigned: Result:=nil;
              bfConcatArray: Result:=nil;
              bfCopyArray: Result:=nil;
              bfCopyString: Result:=nil; // native-only; not const-foldable
              bfTypeInfo: Result:=nil;
            else
              {$IFDEF VerbosePasResEval}
              writeln('TPasResolver.OnExprEvalParams Unhandled BuiltInProc ',Decl.Name,' ',ResolverBuiltInProcNames[BuiltInProc.BuiltIn]);
              {$ENDIF}
              RaiseNotYetImplemented(20170624192324,Params);
            end;
          {$IFDEF VerbosePasResEval}
          {AllowWriteln}
          if Result<>nil then
            writeln('TPasResolver.OnExprEvalParams Called BuiltInProc ',Decl.Name,' ',ResolverBuiltInProcNames[BuiltInProc.BuiltIn],' Result=',Result.AsString)
          else
            writeln('TPasResolver.OnExprEvalParams Called BuiltInProc ',Decl.Name,' ',ResolverBuiltInProcNames[BuiltInProc.BuiltIn],' Result=nil');
          {AllowWriteln-}
          {$ENDIF}
          exit;
          end
        else if Decl.CustomData is TResElDataBaseType then
          begin
          // typecast to basetype
          bt:=TResElDataBaseType(Decl.CustomData).BaseType;
          Result:=EvalBaseTypeCast(Params,bt);
          end;
        {$IFDEF VerbosePasResEval}
        writeln('TPasResolver.OnExprEvalParams BuiltInProc ',Decl.Name,' ',GetObjName(Decl.CustomData));
        {$ENDIF}
        end
      else if C=TPasEnumType then
        begin
        // typecast to enumtype
        Result:=fExprEvaluator.EnumTypeCast(TPasEnumType(Decl),Params.Params[0],Flags);
        end
      else if C=TPasRangeType then
        begin
        // typecast to custom range
        ComputeElement(TPasRangeType(Decl).RangeExpr.Left,ResolvedEl,[rcConstant]);
        if ResolvedEl.BaseType=btContext then
          begin
          TypeEl:=ResolvedEl.LoTypeEl;
          if TypeEl.ClassType=TPasEnumType then
            begin
            // typecast to enumtype
            Result:=fExprEvaluator.EnumTypeCast(TPasEnumType(TypeEl),Params.Params[0],Flags);
            end
          else
            RaiseNotYetImplemented(20171009223403,Params);
          end
        else if ResolvedEl.BaseType in (btAllInteger+btAllChars+btAllBooleans) then
          // typecast to a custom ordinal range (e.g. byte(0)..byte(15) → tbytes(0)):
          // cast the value to the range's underlying ordinal base type.
          Result:=EvalBaseTypeCast(Params,ResolvedEl.BaseType)
        else
          RaiseNotYetImplemented(20171009223303,Params);
        end
      else if C=TPasPointerType then
        // typecast to a named pointer type: base leaves unfolded (nil), a
        // native resolver may fold a constant integer operand.
        Result:=EvalNativeNamedPointerCast(Params)
      else if (C=TPasSetType) and (length(Params.Params)=1) then
        begin
        // `TCharSet(['A'..'Z'])` - a set cast to its own type keeps its value
        Result:=fExprEvaluator.Eval(Params.Params[0],Flags);
        if (Result<>nil) and not (Result is TResEvalSet) then
          ReleaseEvalValue(Result);
        end
      else if C=TPasStringType then
        // typecast a string constant to a custom ansistring-with-codepage type,
        // e.g. const z = str866('abc') where str866 = type ansistring(866). Fold
        // to the ansistring value; the declared codepage is applied by codegen
        // from z's type (an FPC_ANSISTR_TO_ANSISTR conversion), so the fold need
        // only produce the string constant.
        {$IFDEF FPC_HAS_CPSTRING}
        Result:=EvalBaseTypeCast(Params,btAnsiString);
        {$ELSE}
        Result:=EvalBaseTypeCast(Params,btUnicodeString);
        {$ENDIF}
      end;
    end;
  pekSet: ;
  end;
  if Flags=[] then ;
  if Sender=nil then ;
end;

procedure TPasResolver.OnRangeCheckEl(Sender: TResExprEvaluator;
  El: TPasElement; var MsgType: TMessageType);
begin
  if El=nil then exit;
  // Note: range checks are off for the operand of "type of"
  if (MsgType=mtWarning)
      and (FTypeOfOperandLevel=0)
      and (bsRangeChecks in CurrentParser.Scanner.CurrentBoolSwitches) then
    MsgType:=mtError;
  if Sender=nil then ;
end;

function TPasResolver.EvalNativePointerCast(Params: TParamsExpr;
  bt: TResolverBaseType): TResEvalValue;
begin
  // Base default: not folded (stock raises). TPasNativeResolver overrides this
  // to fold a constant integer cast to a pointer base type.
  Result:=nil;
  if Params=nil then ;
  if bt=btNone then ;
end;

function TPasResolver.EvalNativeNamedPointerCast(Params: TParamsExpr): TResEvalValue;
begin
  // Base default: not folded (falls through unresolved, as stock does for a
  // named pointer-type cast). TPasNativeResolver overrides this to fold a
  // constant integer cast to a named pointer type (e.g. PAnsiChar(1)).
  Result:=nil;
  if Params=nil then ;
end;

function TPasResolver.EvalNativeAddressOf(Expr: TPrimitiveExpr;
  Flags: TResEvalFlags): TResEvalValue;
begin
  // Base default: not folded (stock raises). TPasNativeResolver overrides this
  // to degrade an address-of (@X) to an opaque pointer value.
  Result:=nil;
  if Expr<>nil then ;
  if FLags=[] then ;
end;

function TPasResolver.EvalBaseTypeCast(Params: TParamsExpr;
  bt: TResolverBaseType): TResEvalvalue;

  procedure TCFloatToInt(Value: TResEvalValue; Flo: TMaxPrecFloat);
  var
    Int, MinIntVal, MaxIntVal: TMaxPrecInt;
  begin
    if bt in btAllIntegerNoQWord then
      begin
      // float to int
      GetIntegerRange(bt,MinIntVal,MaxIntVal);
      if (Flo<MinIntVal) or (Flo>MaxIntVal) then
        fExprEvaluator.EmitRangeCheckConst(20170711001228,
          Value.AsString,MinIntVal,MaxIntVal,Params,mtError);
      {$R-}
      try
        Int:=Round(Flo);
      except
        RaiseMsg(20170711002218,nRangeCheckError,sRangeCheckError,[],Params);
      end;
      case bt of
        btByte: Result:=TResEvalInt.CreateValue(Int,reitByte);
        btShortInt: Result:=TResEvalInt.CreateValue(Int,reitShortInt);
        btWord: Result:=TResEvalInt.CreateValue(Int,reitWord);
        btSmallInt: Result:=TResEvalInt.CreateValue(Int,reitSmallInt);
        btUIntSingle: Result:=TResEvalInt.CreateValue(Int,reitUIntSingle);
        btIntSingle: Result:=TResEvalInt.CreateValue(Int,reitIntSingle);
        btLongWord: Result:=TResEvalInt.CreateValue(Int,reitLongWord);
        btLongint: Result:=TResEvalInt.CreateValue(Int,reitLongInt);
        btUIntDouble: Result:=TResEvalInt.CreateValue(Int,reitUIntDouble);
        {$ifdef HasInt64}
        btIntDouble: Result:=TResEvalInt.CreateValue(Int,reitIntDouble);
        btInt64: Result:=TResEvalInt.CreateValue(Int); // default
        {$else}
        btIntDouble: Result:=TResEvalInt.CreateValue(Int); // default
        {$endif}
      else
        RaiseNotYetImplemented(20170711001513,Params);
      end;
      {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
      exit;
      end
    else if bt=btSingle then
      begin
      // float to single
      try
        Result:=TResEvalFloat.CreateValue({$ifdef pas2js}double{$else}single{$endif}(Flo));
      except
        RaiseMsg(20170711002315,nRangeCheckError,sRangeCheckError,[],Params);
      end;
      end
    else if bt=btDouble then
      begin
      // float to double
      try
        Result:=TResEvalFloat.CreateValue(double(Flo));
      except
        RaiseMsg(20170711002327,nRangeCheckError,sRangeCheckError,[],Params);
      end;
      end
    else if bt=btCurrency then
      begin
      // float to currency
      try
        Result:=TResEvalCurrency.CreateValue(Currency(Flo));
      except
        RaiseMsg(20180421171840,nRangeCheckError,sRangeCheckError,[],Params);
      end;
      end
    else if bt=btExtended then
      begin
      // float to extended (pas2llvm and pas2js map Extended to double)
      try
        Result:=TResEvalFloat.CreateValue(double(Flo));
      except
        RaiseMsg(20260731120001,nRangeCheckError,sRangeCheckError,[],Params);
      end;
      end
    else
      begin
      {$IFDEF VerbosePasResEval}
      writeln('TPasResolver.OnExprEvalParams typecast float to ',bt);
      {$ENDIF}
      RaiseNotYetImplemented(20170711002542,Params);
      end;
  end;

var
  Value: TResEvalValue;
  Int, MinIntVal, MaxIntVal: TMaxPrecInt;
  Flo: TMaxPrecFloat;
  w: WideChar;
begin
  Result:=nil;
  {$IFDEF VerbosePasResEval}
  writeln('TPasResolver.EvalBaseTypeCast bt=',bt);
  {$ENDIF}
  Value:=Eval(Params.Params[0],[refAutoConstExt]);
  if Value=nil then exit;
  try
    // An INTEGER cast of a one-character string is its ORDINAL: dbus writes
    // `DBUS_TYPE_BYTE = Integer('y')`. Convert here so the integer branch below
    // does the range checking and the target typing.
    if (bt in btAllInteger) and (Value.Kind in revkAllStrings) then
      begin
      Int:=fExprEvaluator.StringToOrd(Value,Params);
      ReleaseEvalValue(Value);
      Value:=TResEvalInt.CreateValue(Int);
      end;
    // `Pointer(nil)` - through an alias too, SynEdit's `NullRange =
    // TSynEditRange(nil)` - stays nil
    if (bt=btPointer) and (Value.Kind=revkNil) then
      begin
      Result:=Value;
      Value:=nil;
      exit;
      end;
    case Value.Kind of
    revkInt:
      begin
      Int:=TResEvalInt(Value).Int;
      {$ifdef HasInt64}
      if bt=btQWord then
        begin
        // int to qword
        {$R-}
        Result:=TResEvalUInt.CreateValue(TMaxPrecUInt(Int));
        {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
        end
      else
      {$endif}
      if bt in btAllIntegerNoQWord then
        begin
        // int to int
        GetIntegerRange(bt,MinIntVal,MaxIntVal);
        if (Int<MinIntVal) or (Int>MaxIntVal) then
          begin
          {$R-}
          case bt of
            btByte: Result:=TResEvalInt.CreateValue(byte(Int),reitByte);
            btShortInt: Result:=TResEvalInt.CreateValue(shortint(Int),reitShortInt);
            btWord: Result:=TResEvalInt.CreateValue(word(Int),reitWord);
            btSmallInt: Result:=TResEvalInt.CreateValue(smallint(Int),reitSmallInt);
            btLongWord: Result:=TResEvalInt.CreateValue(longword(Int),reitLongWord);
            btLongint: Result:=TResEvalInt.CreateValue(longint(Int),reitLongInt);
            {$ifdef HasInt64}
            btInt64: Result:=TResEvalInt.CreateValue(Int);
            {$endif}
            btUIntSingle,
            btIntSingle,
            btUIntDouble,
            btIntDouble:
              fExprEvaluator.EmitRangeCheckConst(20170624194534,
                Value.AsString,MinIntVal,MaxIntVal,Params,mtError);
          else
            RaiseNotYetImplemented(20170624200109,Params);
          end;
          {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
          end
        else
          begin
          {$R-}
          case bt of
            btByte: Result:=TResEvalInt.CreateValue(Int,reitByte);
            btShortInt: Result:=TResEvalInt.CreateValue(Int,reitShortInt);
            btWord: Result:=TResEvalInt.CreateValue(Int,reitWord);
            btSmallInt: Result:=TResEvalInt.CreateValue(Int,reitSmallInt);
            btUIntSingle: Result:=TResEvalInt.CreateValue(Int,reitUIntSingle);
            btIntSingle: Result:=TResEvalInt.CreateValue(Int,reitIntSingle);
            btLongWord: Result:=TResEvalInt.CreateValue(Int,reitLongWord);
            btLongint: Result:=TResEvalInt.CreateValue(Int,reitLongInt);
            btUIntDouble: Result:=TResEvalInt.CreateValue(Int,reitUIntDouble);
            {$ifdef HasInt64}
            btIntDouble: Result:=TResEvalInt.CreateValue(Int,reitIntDouble);
            btInt64: Result:=TResEvalInt.CreateValue(Int); // default
            {$else}
            btIntDouble: Result:=TResEvalInt.CreateValue(Int); // default
            {$endif}
          else
            RaiseNotYetImplemented(20170624200109,Params);
          end;
          {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
          end;
        exit;
        end
      else if bt in btAllBooleans then
        case Int of
        0: Result:=TResEvalBool.CreateValue(false);
        1: Result:=TResEvalBool.CreateValue(true);
        else
          fExprEvaluator.EmitRangeCheckConst(20170710203254,
            Value.AsString,0,1,Params,mtError);
        end
      {$ifdef FPC_HAS_CPSTRING}
      else if (bt=btAnsiChar) or ((bt=btChar) and (BaseTypeChar=btAnsiChar)) then
        try
          Result:=TResEvalString.CreateValue(AnsiChar(Int));
        except
          RaiseMsg(20180125112510,nRangeCheckError,sRangeCheckError,[],Params);
        end
      {$endif}
      else if (bt=btWideChar) or ((bt=btChar) and (BaseTypeChar=btWideChar)) then
        try
          w:=WideChar(Int);
          Result:=TResEvalUTF16.CreateValue(w);
        except
          RaiseMsg(20180125112716,nRangeCheckError,sRangeCheckError,[],Params);
        end
      else if bt=btSingle then
        try
          Result:=TResEvalFloat.CreateValue({$ifdef pas2js}double{$else}single{$endif}(Int));
        except
          RaiseMsg(20170711002015,nRangeCheckError,sRangeCheckError,[],Params);
        end
      else if bt=btDouble then
        try
          Result:=TResEvalFloat.CreateValue(Double(Int));
        except
          RaiseMsg(20170711002016,nRangeCheckError,sRangeCheckError,[],Params);
        end
      else if bt=btCurrency then
        try
          Result:=TResEvalCurrency.CreateValue(Currency(Int));
        except
          RaiseMsg(20180422093631,nRangeCheckError,sRangeCheckError,[],Params);
        end
      else
        begin
        Result:=EvalNativePointerCast(Params,bt);
        if Result=nil then
          begin
          {$IFDEF VerbosePasResEval}
          writeln('TPasResolver.OnExprEvalParams typecast int to ',bt);
          {$ENDIF}
          RaiseNotYetImplemented(20170624194308,Params);
          end;
        end;
      end;
    revkBool:
      begin
      if TResEvalBool(Value).B then
        Int:=1
      else
        Int:=0;
      if bt in btAllBooleans then
        begin
        Result:=Value;
        Value:=nil;
        end
      else if bt in btAllInteger then
        Result:=TResEvalInt.CreateValue(Int)
      else if bt in btAllFloats then
        begin
        Flo:=Int;
        TCFloatToInt(Value,Flo);
        end;
      end;
    revkUInt:
      begin
      {$ifdef HasInt64}
      if bt=btQWord then
        begin
        // qword to qword
        Result:=Value;
        Value:=nil;
        end
      else
      {$endif}
      if bt in btAllIntegerNoQWord then
        begin
        // uint to int — truncate via Int64
        {$R-}
        Int:=Int64(TResEvalUInt(Value).UInt);
        {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
        case bt of
          btByte: Result:=TResEvalInt.CreateValue(byte(Int),reitByte);
          btShortInt: Result:=TResEvalInt.CreateValue(shortint(Int),reitShortInt);
          btWord: Result:=TResEvalInt.CreateValue(word(Int),reitWord);
          btSmallInt: Result:=TResEvalInt.CreateValue(smallint(Int),reitSmallInt);
          btLongWord: Result:=TResEvalInt.CreateValue(longword(Int),reitLongWord);
          btLongint: Result:=TResEvalInt.CreateValue(longint(Int),reitLongInt);
          {$ifdef HasInt64}
          btInt64: Result:=TResEvalInt.CreateValue(Int);
          {$endif}
        end;
        end
      else if bt in btAllBooleans then
        begin
        case TResEvalUInt(Value).UInt of
        0: Result:=TResEvalBool.CreateValue(false);
        1: Result:=TResEvalBool.CreateValue(true);
        end;
        end
      else if bt in btAllFloats then
        begin
        Flo:=TResEvalUInt(Value).UInt;
        TCFloatToInt(Value,Flo);
        end;
      end;
    revkFloat:
      begin
      Flo:=TResEvalFloat(Value).FloatValue;
      TCFloatToInt(Value,Flo);
      end;
    revkCurrency:
      begin
      if bt=btCurrency then
        begin
        Result:=Value;
        Value:=nil;
        end
      else
        begin
        Flo:=TResEvalCurrency(Value).Value;
        TCFloatToInt(Value,Flo);
        end;
      end;
    {$ifdef FPC_HAS_CPSTRING}
    revkString:
      begin
      // The ANSIchar branch must test BaseTypeChar=btAnsiChar; testing btWideChar
      // made `Char(#219)` fall through to nil on a target whose default Char is
      // AnsiChar, and the constant was refused. The revkInt branch above already
      // spells the same test correctly.
      if (bt=btAnsiChar) or ((bt=btChar) and (BaseTypeChar=btAnsiChar)) then
        begin
        // ansichar(ansistring)
        if fExprEvaluator.StringToOrd(Value,nil)>$ffff then
          RaiseXExpectedButYFound(20181005141025,'char','string',Params);
        Result:=Value;
        Value:=nil;
        end
      else if (bt=btWideChar) or ((bt=btChar) and (BaseTypeChar=btWideChar)) then
        begin
        // widechar(ansistring)
        if fExprEvaluator.GetWideChar(TResEvalString(Value).S,w) then
          begin
          Result:=Value;
          Value:=nil;
          end
        else
          RaiseXExpectedButYFound(20181005141058,'char','string',Params);
        end
      else if (bt=btAnsiString) or ((bt=btString) and (BaseTypeString=btAnsiString)) then
        begin
        // ansistring(ansistring)
        Result:=Value;
        Value:=nil;
        end
      else if (bt=btUnicodeString) or (bt=btWideString)
          or ((bt=btString) and (BaseTypeString=btUnicodeString)) then
        begin
        // unicodestring(ansistring)
        Result:=TResEvalUTF16.CreateValue(
                  fExprEvaluator.GetUnicodeStr(TResEvalString(Value).S,Params));
        end
      else if bt=btRawByteString then
        begin
        // rawbytestring(ansistring)
        SetCodePage(TResEvalString(Value).S,CP_NONE,false);
        end;
      end;
    {$endif}
    revkUnicodeString:
      if (length(TResEvalUTF16(Value).S)=1) and (bt in btAllChars) then
        begin
        w:=TResEvalUTF16(Value).S[1];
        {$ifdef FPC_HAS_CPSTRING}
        if (bt=btAnsiChar) or ((bt=btChar) and (BaseTypeChar=btAnsiChar)) then
          begin
          // ansichar(unicodestring)
          if ord(w)<=255 then
            begin
            Result:=Value;
            Value:=nil;
            end
          else
            RaiseMsg(20181005141632,nRangeCheckError,sRangeCheckError,[],Params);
          end
        else
        {$endif}
        if (bt=btWideChar) or ((bt=btChar) and (BaseTypeChar=btWideChar)) then
          begin
          // widechar(unicodestring)
          Result:=Value;
          Value:=nil;
          end;
        end
      {$ifdef FPC_HAS_CPSTRING}
      else if (bt=btAnsiString) or ((bt=btString) and (BaseTypeString=btAnsiString)) then
        begin
        // ansistring(unicodestring)
        Result:=TResEvalString.CreateValue(
                   fExprEvaluator.GetRawByteString(TResEvalUTF16(Value).S,CP_ACP,Params));
        end
      else if bt=btRawByteString then
        begin
        // rawbytestring(unicodestring)
        Result:=TResEvalString.CreateValue(
             fExprEvaluator.GetRawByteString(TResEvalUTF16(Value).S,CP_NONE,Params));
        end
      {$endif}
      else if (bt=btUnicodeString) or ((bt=btString) and (BaseTypeString=btUnicodeString)) then
        begin
        // unicodestring(unicodestring)
        Result:=Value;
        Value:=nil;
        end;
    revkExternal:
      begin
      // not known until specialization, e.g. a const template parameter
      Result:=Value;
      Value:=nil;
      end;
    revkEnum:
      begin
      // Cast of an enum constant to an ordinal type, e.g. Word(SomeEnumValue).
      // Use the enum's ordinal (Index) and truncate to the target integer type.
      Int:=TResEvalEnum(Value).Index;
      {$ifdef HasInt64}
      if bt=btQWord then
        Result:=TResEvalUInt.CreateValue(TMaxPrecUInt(Int))
      else
      {$endif}
      if bt in btAllIntegerNoQWord then
        begin
        {$R-}
        case bt of
          btByte: Result:=TResEvalInt.CreateValue(byte(Int),reitByte);
          btShortInt: Result:=TResEvalInt.CreateValue(shortint(Int),reitShortInt);
          btWord: Result:=TResEvalInt.CreateValue(word(Int),reitWord);
          btSmallInt: Result:=TResEvalInt.CreateValue(smallint(Int),reitSmallInt);
          btLongWord: Result:=TResEvalInt.CreateValue(longword(Int),reitLongWord);
          btLongint: Result:=TResEvalInt.CreateValue(longint(Int),reitLongInt);
          {$ifdef HasInt64}
          btInt64: Result:=TResEvalInt.CreateValue(Int);
          {$endif}
        else
          RaiseNotYetImplemented(20170624193436,Params);
        end;
        {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
        end
      else
        RaiseNotYetImplemented(20170624193436,Params);
      end;
    revkNil:
      // nil cast to an ordinal is the null address = 0 (e.g. HModule(nil)); to a
      // pointer/other type it is not const-folded here (Result stays nil so the
      // code generator emits it).
      if bt in btAllInteger then
        Result:=TResEvalInt.CreateValue(0);
    else
      {$IFDEF VerbosePasResEval}
      writeln('TPasResolver.OnExprEvalParams typecast to ',bt);
      {$ENDIF}
      RaiseNotYetImplemented(20170624193436,Params);
    end;
  finally
    ReleaseEvalValue(Value);
  end;
end;

function TPasResolver.EvalLengthOfString(ParamResolved: TPasResolverResult;
  Param: TPasExpr; Flags: TResEvalFlags): TResEvalValue;
var
  Value: TResEvalValue;
begin
  Result:=nil;
  if rrfReadable in ParamResolved.Flags then
    begin
    Value:=Eval(Param,Flags);
    if Value=nil then exit;
    case Value.Kind of
    {$ifdef FPC_HAS_CPSTRING}
    revkString:
      Result:=TResEvalInt.CreateValue(length(TResEvalString(Value).S));
    {$endif}
    revkUnicodeString:
      Result:=TResEvalInt.CreateValue(length(TResEvalUTF16(Value).S));
    end;
    ReleaseEvalValue(Value);
    end
end;
