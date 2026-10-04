{%MainUnit pasresolver.pp}
{ **********************************************************************

    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - Operator handling
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }

procedure TPasResolver.WriteScopes;
{AllowWriteln}
var
  i: Integer;
  Scope: TPasScope;
begin
  writeln('TPasResolver.WriteScopes ScopeCount=',ScopeCount);
  for i:=ScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    writeln('  ',i,'/',ScopeCount,' ',GetObjName(Scope));
    Scope.WriteIdentifiers('  ');
    end;
  {AllowWriteln-}
end;

procedure TPasResolver.ComputeBinaryExpr(Bin: TBinaryExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
var
  LeftResolved, RightResolved: TPasResolverResult;
  Left: TPasExpr;
  SubBin: TBinaryExpr;
  lFlags : TPasResolverComputeFlags;
begin
  if (Bin.OpCode=eopSubIdent)
  or ((Bin.OpCode=eopNone) and (Bin.Left is TInheritedExpr)) then
    begin
    // Note: bin.Left was already resolved via ResolveSubIdent
    ComputeElement(Bin.Right,ResolvedEl,Flags,StartEl);
    exit;
    end;

  if Bin.OpCode in [eopEqual,eopNotEqual] then
    begin
    // Try operator overloading first for equality/inequality
    lFlags:=Flags-[rcNoImplicitProc,rcNoImplicitProcType,rcSetReferenceFlags];
    if MaximizeFPCCompatibility then
      Include(lFlags,rcSetReferenceFlags);
    ComputeElement(Bin.Left,LeftResolved,lFlags,StartEl);
    ComputeElement(Bin.Right,RightResolved,lFlags,StartEl);
    if TryResolveOperatorOverload(Bin,ResolvedEl,LeftResolved,RightResolved) then
      exit;
    // No operator overload found — use standard equality check
    if CheckEqualElCompatibility(Bin.Left,Bin.Right,nil,true,
        rcSetReferenceFlags in Flags)=cIncompatible then
      RaiseInternalError(20161007215912);
    SetResolverValueExpr(ResolvedEl,btBoolean,FBaseTypes[btBoolean],FBaseTypes[btBoolean],
                         Bin,[rrfReadable]);
    exit;
    end;

  Flags:=Flags-[rcNoImplicitProc,rcNoImplicitProcType];
  if Bin.OpCode=eopAdd then
    begin
    // handle multi-adds without stack
    Left:=Bin.Left;
    while Left.ClassType=TBinaryExpr do
      begin
      SubBin:=TBinaryExpr(Left);
      if SubBin.OpCode<>eopAdd then break;
      Left:=SubBin.Left;
      end;
    // Left is now left-most of multi add
    ComputeElement(Left,LeftResolved,Flags,StartEl);
    if FTypeOfOperandLevel>0 then
      TypeOfOperandTypeToValue(LeftResolved,Left);
    repeat
      SubBin:=TBinaryExpr(Left.Parent);
      ComputeElement(SubBin.Right,RightResolved,Flags,StartEl);
      if FTypeOfOperandLevel>0 then
        TypeOfOperandTypeToValue(RightResolved,SubBin.Right);

      if not TryResolveOperatorOverload(SubBin,ResolvedEl,LeftResolved,RightResolved) then
        ComputeBinaryExprRes(SubBin,ResolvedEl,Flags,LeftResolved,RightResolved);
      LeftResolved:=ResolvedEl;
      Left:=SubBin;
    until Left=Bin;
    end
  else
    begin
    ComputeElement(Bin.Left,LeftResolved,Flags,StartEl);
    ComputeElement(Bin.Right,RightResolved,Flags,StartEl);
    if (FTypeOfOperandLevel>0)
        and not (Bin.OpCode in [eopIs,eopIsNot,eopAs,eopSubIdent,eopNone]) then
      begin
      TypeOfOperandTypeToValue(LeftResolved,Bin.Left);
      TypeOfOperandTypeToValue(RightResolved,Bin.Right);
      end;

    if not TryResolveOperatorOverload(Bin,ResolvedEl,LeftResolved,RightResolved) then
      ComputeBinaryExprRes(Bin,ResolvedEl,Flags,LeftResolved,RightResolved);
    end;
end;

procedure TPasResolver.TypeOfOperandTypeToValue(var ResolvedEl: TPasResolverResult;
  Expr: TPasExpr);
// In the operand of "type of" an operand of a binary operator can be a type,
// e.g. "type of (S+T)" with type parameters S and T -> treat it as a value
var
  BaseType: TResolverBaseType;
  LoType, HiType: TPasType;
begin
  if (ResolvedEl.IdentEl is TPasType)
      and not (rrfReadable in ResolvedEl.Flags)
      and (ResolvedEl.LoTypeEl<>nil) then
    begin
    BaseType:=ResolvedEl.BaseType;
    LoType:=ResolvedEl.LoTypeEl;
    HiType:=ResolvedEl.HiTypeEl;
    SetResolverValueExpr(ResolvedEl,BaseType,LoType,HiType,Expr,[rrfReadable]);
    end;
end;

procedure TPasResolver.ComputeBinaryExprRes(Bin: TBinaryExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  var LeftResolved, RightResolved: TPasResolverResult);

  procedure SetBaseType(BaseType: TResolverBaseType);
  begin
    SetResolverValueExpr(ResolvedEl,BaseType,FBaseTypes[BaseType],FBaseTypes[BaseType],
                         Bin,[rrfReadable]);
  end;

  procedure SetLeftValueExpr(Flags: TPasResolverResultFlags);
  begin
    SetResolverValueExpr(ResolvedEl,LeftResolved.BaseType,
      LeftResolved.LoTypeEl,LeftResolved.HiTypeEl,Bin,Flags);
  end;

  procedure SetRightValueExpr(Flags: TPasResolverResultFlags);
  begin
    SetResolverValueExpr(ResolvedEl,RightResolved.BaseType,
      RightResolved.LoTypeEl,RightResolved.HiTypeEl,Bin,Flags);
  end;

  function RightIsCharPointer: Boolean;
  // A pointer to a char type (PAnsiChar/PWideChar/PUnicodeChar) concatenates with a
  // string as a null-terminated string operand: 
  // accept `s := s + PChar`
  begin
    Result := IsCharPointerRes(RightResolved);
  end;

  procedure CharPointerToStringRes(var R: TPasResolverResult);
  // Retype a char-pointer operand as the string it concatenates into, so the
  // result type of the concat is a string and not the pointer itself.
  var
    SubRes: TPasResolverResult;
    BT: TResolverBaseType;
  begin
    ComputeElement(TPasPointerType(R.LoTypeEl).DestType,SubRes,[rcType]);
    if (SubRes.BaseType=btWideChar) and (FBaseTypes[btUnicodeString]<>nil) then
      BT:=btUnicodeString
    else if (SubRes.BaseType<>btWideChar) and (FBaseTypes[btAnsiString]<>nil) then
      BT:=btAnsiString
    else
      BT:=btString;
    SetResolverValueExpr(R,BT,FBaseTypes[BT],FBaseTypes[BT],R.ExprEl,[rrfReadable]);
  end;

  function IsCharArrayRes(const R: TPasResolverResult): Boolean;
  // An array (static or open) whose elements are characters.
  var
    SubRes: TPasResolverResult;
  begin
    Result:=false;
    if (R.BaseType<>btContext) or not (R.LoTypeEl is TPasArrayType) then exit;
    if TPasArrayType(R.LoTypeEl).ElType=nil then exit;
    ComputeElement(TPasArrayType(R.LoTypeEl).ElType,SubRes,[rcType]);
    Result:=SubRes.BaseType in btAllChars;
  end;

  procedure CharArrayToStringRes(var R: TPasResolverResult);
  // Retype a char-ARRAY operand as the string it holds, so a concat yields a
  // string and not the array.
  var
    SubRes: TPasResolverResult;
    BT: TResolverBaseType;
  begin
    ComputeElement(TPasArrayType(R.LoTypeEl).ElType,SubRes,[rcType]);
    if (SubRes.BaseType=btWideChar) and (FBaseTypes[btUnicodeString]<>nil) then
      BT:=btUnicodeString
    else if (SubRes.BaseType<>btWideChar) and (FBaseTypes[btAnsiString]<>nil) then
      BT:=btAnsiString
    else
      BT:=btString;
    SetResolverValueExpr(R,BT,FBaseTypes[BT],FBaseTypes[BT],R.ExprEl,[rrfReadable]);
  end;

  function LeftIsCharPointer(out CharBT: TResolverBaseType): Boolean;
  // The mirror of RightIsCharPointer: accept `s := PChar + s`
  var
    SubRes: TPasResolverResult;
  begin
    Result := False;
    CharBT := btNone;
    if (LeftResolved.BaseType=btContext)
        and (LeftResolved.LoTypeEl is TPasPointerType)
        and (TPasPointerType(LeftResolved.LoTypeEl).DestType<>nil) then
      begin
      ComputeElement(TPasPointerType(LeftResolved.LoTypeEl).DestType,SubRes,[rcType]);
      Result := SubRes.BaseType in btAllChars;
      if Result then
        CharBT := SubRes.BaseType;
      end;
  end;

  procedure UnwrapConstParam(var R: TPasResolverResult);
  // A TYPED const generic parameter (`const I: SomeType`) has a KNOWN type, so an
  // operator on it can be decided at the generic definition rather than deferred to
  // specialization. Resolve it to its constraint type as a readable value, so the
  // normal operator check below runs (tgenconst15: `I div I` with `I: string` must
  // be rejected). A type parameter (not IsConst) or an untyped const is left alone.
  var
    Templ: TPasGenericTemplateType;
    CType: TPasType;
    TypeRes: TPasResolverResult;
    i: Integer;
  begin
    if not IsGenericTemplType(R) then exit;
    Templ:=TPasGenericTemplateType(R.LoTypeEl);
    if not Templ.IsConst then exit;
    CType:=nil;
    for i:=0 to length(Templ.Constraints)-1 do
      if Templ.Constraints[i] is TPasType then
        begin
        CType:=TPasType(Templ.Constraints[i]);
        break;
        end;
    if CType=nil then exit;
    ComputeElement(CType,TypeRes,[rcType]);
    SetResolverValueExpr(R,TypeRes.BaseType,TypeRes.LoTypeEl,TypeRes.HiTypeEl,
      R.ExprEl,[rrfReadable]);
  end;

var
  ElTypeResolved: TPasResolverResult;
  LeftTypeEl, RightTypeEl: TPasType;
  CharBT: TResolverBaseType;
  RightStrResolved, LeftStrResolved: TPasResolverResult;
begin
  if LeftResolved.BaseType=btRange then
    ConvertRangeToElement(LeftResolved);
  if RightResolved.BaseType=btRange then
    ConvertRangeToElement(RightResolved);

  UnwrapConstParam(LeftResolved);
  UnwrapConstParam(RightResolved);

  //writeln('TPasResolver.ComputeBinaryExpr ',OpcodeStrings[Bin.OpCode],' Left=',GetResolverResultDbg(LeftResolved),' Right=',GetResolverResultDbg(RightResolved));

  if IsGenericTemplType(LeftResolved) or IsGenericTemplType(RightResolved) then
    begin
    // cannot yet be decided
    case Bin.OpCode of
    eopEqual, eopNotEqual,
    eopLessThan,eopGreaterThan, eopLessthanEqual,eopGreaterThanEqual,
    eopIn,eopNotIn,eopIs,eopIsNot:
      begin
      SetBaseType(btBoolean);
      exit;
      end;
    eopAs:
      begin
      SetRightValueExpr([rrfReadable]);
      exit;
      end;
    end;
    ResolvedEl:=LeftResolved;
    ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable];
    exit;
    end;

  if LeftResolved.BaseType in btAllInteger then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (rrfReadable in RightResolved.Flags) then
      begin
        if (RightResolved.BaseType in (btAllInteger+btAllFloats)) then
          case Bin.OpCode of
          eopNone:
            if (Bin.Kind=pekRange) then
              begin
              if not (RightResolved.BaseType in btAllInteger) then
                RaiseXExpectedButYFound(20170216152600,'integer',BaseTypeNames[RightResolved.BaseType],Bin.Right);
              // use left type for result
              SetLeftValueExpr([rrfReadable]);
              if Bin.Parent is TPasRangeType then
                begin
                ResolvedEl.LoTypeEl:=TPasRangeType(Bin.Parent);
                ResolvedEl.HiTypeEl:=ResolvedEl.LoTypeEl;
                end;
              exit;
              end;
          eopAdd, eopSubtract,
          eopMultiply, eopDiv, eopMod,
          eopPower,
          eopShl, eopShr,
          eopAnd, eopOr, eopXor:
            begin
            if RightResolved.BaseType in btAllFloats then
              // use right type for result
              SetRightValueExpr([rrfReadable])
            {$IFDEF HasInt64}  
            else if (Bin.OpCode in [eopAdd,eopSubtract,eopMultiply,eopDiv,eopMod,eopAnd,eopOr,eopXor])
                and (LeftResolved.BaseType<>RightResolved.BaseType)
                and not ((LeftResolved.BaseType in [btInt64,btQWord,btComp])
                         and (RightResolved.BaseType in [btInt64,btQWord,btComp])) then
              // Mixed integer types: FPC combines them — mixing a signed and an
              // unsigned type of the SAME size promotes to the next size, so
              // longint+cardinal -> int64 (not a truncated 32-bit add). tintuint.
              // Shl/Shr/Power keep the left type. Guards: GetCombinedInt returns
              // garbage for equal base types (call only when they differ); and it
              // would overflow to a 65-bit result for a mixed 64-bit pair
              // (qword+int64) — no wider type exists, so keep left-type there.
              SetBaseType(GetCombinedInt(LeftResolved,RightResolved,Bin))
            {$ENDIF}  
            else
              // use left type for result
              SetLeftValueExpr([rrfReadable]);
            exit;
            end;
          eopLessThan,
          eopGreaterThan,
          eopLessthanEqual,
          eopGreaterThanEqual:
            begin
            SetBaseType(btBoolean);
            exit;
            end;
          eopDivide:
            begin
            SetBaseType(BaseTypeExtended);
            exit;
            end;
          end
        else if (RightResolved.BaseType in [btSet,btArrayOrSet]) then
          begin
          if (Bin.OpCode in [eopIn,eopNotIn]) and (RightResolved.SubType in btAllInteger) then
            begin
            SetBaseType(btBoolean);
            exit;
            end;
          end
        else if RightResolved.BaseType=btPointer then
          begin
          if (Bin.OpCode in [eopAdd,eopSubtract])
              and (ElHasBoolSwitch(Bin,bsPointerMath)
                or IsPointerMathType(RightResolved.LoTypeEl)) then
            begin
            // integer+CanonicalPointer
            SetResolverValueExpr(ResolvedEl,btPointer,
              RightResolved.LoTypeEl,RightResolved.HiTypeEl,Bin,[rrfReadable]);
            exit;
            end;
          end
        else if RightResolved.BaseType=btContext then
          begin
          RightTypeEl:=RightResolved.LoTypeEl;
          if RightTypeEl.ClassType=TPasPointerType then
            begin
            if (Bin.OpCode in [eopAdd,eopSubtract])
                and (PointerMathBoolSwitchEnabled(Bin)
                  or IsPointerMathType(RightTypeEl)) then
              begin
              // integer+TypedPointer — the mirror of TypedPointer+Integer, which
              // keeps the POINTER's own type. Unwrapping to the pointee here made
              // `inLeft + FCharBuf` (fcl-xml xmltextreader.pp) an AnsiChar, so
              // comparing it with a PAnsiChar reported "Char" <= "PAnsiChar".
              SetRightValueExpr([rrfReadable]);
              exit;
              end;
            end;
          end;
      end;
    end
  else if LeftResolved.BaseType in btAllBooleans then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (RightResolved.BaseType in btAllBooleans)
        and (rrfReadable in RightResolved.Flags) then
      case Bin.OpCode of
      eopNone:
        if Bin.Kind=pekRange then
          begin
          SetResolverValueExpr(ResolvedEl,btRange,
            FBaseTypes[LeftResolved.BaseType],FBaseTypes[LeftResolved.BaseType],
            Bin,[rrfReadable]);
          ResolvedEl.SubType:=LeftResolved.BaseType;
          exit;
          end;
      eopAnd, eopOr, eopXor:
        begin
        // use left type for result
        SetLeftValueExpr([rrfReadable]);
        exit;
        end;
      end
    else if (rrfReadable in LeftResolved.Flags)
        and (rrfReadable in RightResolved.Flags)
        and (RightResolved.BaseType in [btSet,btArrayOrSet])
        and (RightResolved.SubType in btAllBooleans) then
      // boolean in set of boolean (mirrors the char-in-set-of-char path)
      case Bin.OpCode of
      eopIn,eopNotIn:
        begin
        SetBaseType(btBoolean);
        exit;
        end;
      end;
    end
  else if (Bin.OpCode=eopAdd) and IsCharArrayRes(LeftResolved)
      and (rrfReadable in LeftResolved.Flags)
      and (rrfReadable in RightResolved.Flags)
      and ((RightResolved.BaseType in btAllStringAndChars)
           or IsCharArrayRes(RightResolved)) then
    begin
    // A char ARRAY on the LEFT concatenates like the string it holds - the
    // mirror of the char-array operand already accepted on the RIGHT below.
    // chm's chmreader builds `'/'+Data+'.hhk'` out of a char buffer.
    LeftStrResolved:=LeftResolved;
    CharArrayToStringRes(LeftStrResolved);
    if IsCharArrayRes(RightResolved) then
      begin
      RightStrResolved:=RightResolved;
      CharArrayToStringRes(RightStrResolved);
      if ComputeAddStringRes(LeftStrResolved,RightStrResolved,Bin,ResolvedEl) then
        exit;
      end
    else if ComputeAddStringRes(LeftStrResolved,RightResolved,Bin,ResolvedEl) then
      exit;
    end
  else if LeftResolved.BaseType in btAllStringAndChars then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (rrfReadable in RightResolved.Flags) then
      begin
      if (RightResolved.BaseType in btAllStringAndChars)
          or ((RightResolved.BaseType=btContext) and (RightResolved.LoTypeEl<>nil)
              and (RightResolved.LoTypeEl.ClassType=TPasArrayType))
          or RightIsCharPointer then
        case Bin.OpCode of
        eopNone:
          if (Bin.Kind=pekRange) and (LeftResolved.BaseType in btAllStringAndChars) then
            begin
            if not (RightResolved.BaseType in btAllStringAndChars) then
              RaiseXExpectedButYFound(20170216152603,'string or char',BaseTypeNames[RightResolved.BaseType],Bin.Right);
            SetResolverValueExpr(ResolvedEl,btRange,
              FBaseTypes[LeftResolved.BaseType],FBaseTypes[LeftResolved.BaseType],
              Bin,[rrfReadable]);
            ResolvedEl.SubType:=LeftResolved.BaseType;
            exit;
            end;
        eopAdd:
          // a char-array or char-pointer operand concatenates like a string
          // (result = left string)
          if RightIsCharPointer then
            begin
            // a char POINTER stands for the string it points at, so the result
            // is that string and never the pointer (`Char + PWideChar`)
            RightStrResolved:=RightResolved;
            CharPointerToStringRes(RightStrResolved);
            if ComputeAddStringRes(LeftResolved,RightStrResolved,Bin,ResolvedEl) then
              exit;
            end
          else if (RightResolved.BaseType in btAllStringAndChars)
              or ((RightResolved.BaseType=btContext) and (RightResolved.LoTypeEl<>nil)
                  and (RightResolved.LoTypeEl.ClassType=TPasArrayType)) then
            if ComputeAddStringRes(LeftResolved,RightResolved,Bin,ResolvedEl) then
              exit;
        eopLessThan,
        eopGreaterThan,
        eopLessthanEqual,
        eopGreaterThanEqual:
          begin
          SetBaseType(btBoolean);
          exit;
          end;
        end
      else if (RightResolved.BaseType in [btSet,btArrayOrSet])
          and (RightResolved.SubType in btAllChars)
          and (LeftResolved.BaseType in btAllChars) then
        begin
        case Bin.OpCode of
        eopIn,eopNotIn:
          begin
          SetBaseType(btBoolean);
          exit;
          end;
        end;
        end
      end
    end
  else if LeftResolved.BaseType in btAllFloats then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (RightResolved.BaseType in (btAllInteger+btAllFloats))
        and (rrfReadable in RightResolved.Flags) then
      case Bin.OpCode of
      eopAdd, eopSubtract,
      eopMultiply, eopDivide, eopMod,
      eopPower:
        begin
        if (RightResolved.BaseType=btCurrency)
            or ((RightResolved.BaseType in btAllFloats)
                and (RightResolved.BaseType>LeftResolved.BaseType)) then
          // use right side as result
          SetRightValueExpr([rrfReadable])
        else
          // use left side as result
          SetLeftValueExpr([rrfReadable]);
        exit;
        end;
      eopLessThan,
      eopGreaterThan,
      eopLessthanEqual,
      eopGreaterThanEqual:
        begin
        SetBaseType(btBoolean);
        exit;
        end;
      end;
    end
  else if LeftResolved.BaseType=btPointer then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (rrfReadable in RightResolved.Flags) then
      begin
      if (RightResolved.BaseType in btAllInteger) then
        case Bin.OpCode of
        eopAdd,eopSubtract:
          if (ElHasBoolSwitch(Bin,bsPointerMath)
              or IsPointerMathType(LeftResolved.LoTypeEl)) then
            begin
            // pointer+integer -> pointer
            SetResolverValueExpr(ResolvedEl,btPointer,
              LeftResolved.LoTypeEl,LeftResolved.HiTypeEl,Bin,[rrfReadable]);
            exit;
            end;
        end
      else if RightResolved.BaseType=btPointer then
        case Bin.OpCode of
        eopLessThan,
        eopGreaterThan,
        eopLessthanEqual,
        eopGreaterThanEqual:
          begin
          SetBaseType(btBoolean);
          exit;
          end;
        {$IFNDEF PAS2JS}
        eopSubtract:
          begin
          // untyped Pointer - untyped Pointer -> byte difference (PtrInt).
          SetBaseType(btInt64);
          exit;
          end;
        {$ENDIF}
        end
      else if (RightResolved.BaseType=btContext)
          and (RightResolved.LoTypeEl is TPasPointerType)
          and (Bin.OpCode in [eopLessThan,eopGreaterThan,
                              eopLessthanEqual,eopGreaterThanEqual]) then
        begin
        (* An untyped Pointer ORDERED against a typed one, as the mirror case
           already allows the other way round: generics.hashes walks a buffer
           with `until not (P <= PLimit)` where P is Pointer and PLimit a
           PAnsiChar. Both are addresses; comparing them needs no $POINTERMATH. *)
        SetBaseType(btBoolean);
        exit;
        end
      {$IFNDEF PAS2JS}
      else if (Bin.OpCode=eopSubtract)
          and (RightResolved.BaseType=btContext)
          and (RightResolved.LoTypeEl is TPasPointerType) then
        begin
        (* An untyped-pointer left operand (e.g. @arrayElem or @rec.field, whose
          address result has btPointer) minus a typed pointer: the element
          difference. Allowed for any two pointers, independent of {$POINTERMATH} *)
        SetBaseType(btInt64);
        exit;
        end
      {$ENDIF}
      ;
      end;
    end
  else if LeftResolved.BaseType=btContext then
    begin
    LeftTypeEl:=LeftResolved.LoTypeEl;
    case Bin.OpCode of
    eopNone:
      if Bin.Kind=pekRange then
        begin
        if (rrfReadable in LeftResolved.Flags)
            and (rrfReadable in RightResolved.Flags) then
          begin
          CheckSetLitElCompatible(Bin.Left,Bin.Right,LeftResolved,RightResolved);
          ResolvedEl:=LeftResolved;
          ResolvedEl.IdentEl:=nil;
          ResolvedEl.SubType:=ResolvedEl.BaseType;
          ResolvedEl.BaseType:=btRange;
          ResolvedEl.ExprEl:=Bin;
          exit;
          end;
        end;
    eopIn,eopNotIn:
      if (rrfReadable in LeftResolved.Flags)
      and (rrfReadable in RightResolved.Flags) then
        begin
        if TryResolveOperatorOverload(Bin, ResolvedEl, LeftResolved, RightResolved) then
          exit;
        if LeftResolved.BaseType in btArrayRangeTypes then
          begin
          if not (RightResolved.BaseType in [btSet,btArrayOrSet]) then
            RaiseXExpectedButYFound(20170216152607,'set of '+BaseTypeNames[LeftResolved.BaseType],GetElementTypeName(LeftResolved.LoTypeEl),Bin.Right);
          if LeftResolved.BaseType in btAllBooleans then
            begin
            if not (RightResolved.SubType in btAllBooleans) then
              RaiseXExpectedButYFound(20170216152610,'set of '+BaseTypeNames[LeftResolved.BaseType],'set of '+BaseTypeNames[RightResolved.SubType],Bin.Right);
            end
          else if LeftResolved.BaseType in btAllChars then
            begin
            if not (RightResolved.SubType in btAllChars) then
              RaiseXExpectedButYFound(20170216152609,'set of '+BaseTypeNames[LeftResolved.BaseType],'set of '+BaseTypeNames[RightResolved.SubType],Bin.Right);
            end
          else if not (RightResolved.SubType in btAllInteger) then
            RaiseXExpectedButYFound(20170216152612,'set of '+BaseTypeNames[LeftResolved.BaseType],'set of '+BaseTypeNames[RightResolved.SubType],Bin.Right);
          SetBaseType(btBoolean);
          exit;
          end
        else if (LeftResolved.BaseType=btContext)
            and (LeftTypeEl.ClassType=TPasEnumType) then
          begin
          if not (RightResolved.BaseType in [btSet,btArrayOrSet]) then
            RaiseXExpectedButYFound(20170216152615,'set of '+LeftResolved.LoTypeEl.Name,GetElementTypeName(LeftResolved.LoTypeEl),Bin.Right);
          RightTypeEl:=RightResolved.LoTypeEl;
          if RightTypeEl=nil then
            // empty set literal []: element type unknown, "enum in []" is valid (always false)
          else if LeftTypeEl=RightTypeEl then
            // enum in setofenum
          else if RightResolved.LoTypeEl.ClassType=TPasRangeType then
            begin
            ComputeElement(TPasRangeType(RightTypeEl).RangeExpr.Left,ElTypeResolved,[rcConstant]);
            if LeftTypeEl<>ElTypeResolved.LoTypeEl then
              RaiseXExpectedButYFound(20171109215833,'set of '+LeftResolved.LoTypeEl.Name,'set of '+RightResolved.LoTypeEl.Name,Bin.Right);
            end
          else
            RaiseXExpectedButYFound(20170216152618,'set of '+LeftResolved.LoTypeEl.Name,'set of '+RightResolved.LoTypeEl.Name,Bin.Right);
          SetBaseType(btBoolean);
          exit;
          end
        else
          RaiseMsg(20170216152228,nInOperatorExpectsSetElementButGot,
            sInOperatorExpectsSetElementButGot,[GetElementTypeName(LeftResolved.LoTypeEl)],Bin);
        end;
    eopIs,eopIsNot:
      begin
      RightTypeEl:=RightResolved.LoTypeEl;
      if (LeftTypeEl is TPasClassType) then
        begin
        if not (rrfReadable in LeftResolved.Flags) then
          RaiseIncompatibleTypeRes(20180204124637,nOperatorIsNotOverloadedAOpB,
            [OpcodeStrings[Bin.OpCode]],LeftResolved,RightResolved,Bin);
        if (LeftResolved.IdentEl is TPasType) then
          RaiseIncompatibleTypeRes(20180204124638,nOperatorIsNotOverloadedAOpB,
            [OpcodeStrings[Bin.OpCode]],LeftResolved,RightResolved,Bin);
        // left side is a class instance
        if (RightResolved.IdentEl is TPasType)
            and (RightTypeEl is TPasClassType) then
          begin
          if TPasClassType(LeftTypeEl).ObjKind=TPasClassType(RightTypeEl).ObjKind then
            begin
            if CheckSrcIsADstType(RightResolved,LeftResolved)<>cIncompatible then
              begin
              // e.g. if obj is TFPMemoryImage then ;
              // Note: at compile time the check is reversed: right must inherit from left
              SetBaseType(btBoolean);
              exit;
              end
            else if CheckSrcIsADstType(LeftResolved,RightResolved)<>cIncompatible then
              begin
              // e.g. if Image is TObject then ;
              // This is useful after some unchecked typecast -> allow
              SetBaseType(btBoolean);
              exit;
              end;
            end
          else if TPasClassType(RightTypeEl).ObjKind=okInterface then
            begin
            if (TPasClassType(LeftTypeEl).ObjKind=okClass)
                and (not TPasClassType(LeftTypeEl).IsExternal) then
              begin
              // e.g. if classintvar is intftype then ;
              SetBaseType(btBoolean);
              exit;
              end;
            end
          else if TPasClassType(LeftTypeEl).ObjKind=okInterface then
            begin
            if (TPasClassType(RightTypeEl).ObjKind=okClass)
                and (not TPasClassType(RightTypeEl).IsExternal) then
              begin
              // e.g. if intfvar is classtype then ;
              SetBaseType(btBoolean);
              exit;
              end;
            end;
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ComputeBinaryExprRes LeftClass=',GetClassAncestorsDbg(TPasClassType(LeftResolved.LoTypeEl)));
          writeln('TPasResolver.ComputeBinaryExprRes RightClass=',GetClassAncestorsDbg(TPasClassType(RightResolved.IdentEl)));
          {$ENDIF}
          end
        else if (RightTypeEl is TPasClassOfType)
            and (rrfReadable in RightResolved.Flags) then
          begin
          // e.g. if Image is ImageClass then ;
          if (CheckClassesAreRelated(LeftResolved.LoTypeEl,
              TPasClassOfType(RightTypeEl).DestType)<>cIncompatible) then
            begin
            SetBaseType(btBoolean);
            exit;
            end;
          end
        else
          RaiseXExpectedButYFound(20170216152625,'class type',GetElementTypeName(RightResolved.LoTypeEl),Bin.Right);
        end
      else if (proClassOfIs in Options) and (LeftTypeEl is TPasClassOfType)
          and (rrfReadable in LeftResolved.Flags) then
        begin
        if (LeftResolved.IdentEl=nil) or (LeftResolved.IdentEl is TPasType) then
          RaiseIncompatibleTypeRes(20180204124657,nOperatorIsNotOverloadedAOpB,
            [OpcodeStrings[Bin.OpCode]],LeftResolved,RightResolved,Bin);
        // left side is class-of variable
        LeftTypeEl:=ResolveAliasType(TPasClassOfType(LeftTypeEl).DestType);
        if (RightResolved.IdentEl is TPasType)
            and (ResolveAliasType(TPasType(RightResolved.IdentEl)) is TPasClassType) then
          begin
          // e.g. if ImageClass is TFPMemoryImage then ;
          // Note: at compile time the check is reversed: right must inherit from left
          if CheckClassIsClass(RightResolved.LoTypeEl,LeftTypeEl)<>cIncompatible then
            begin
            SetBaseType(btBoolean);
            exit;
            end
          end
        else if (RightTypeEl is TPasClassOfType) then
          begin
          // e.g. if ImageClassA is ImageClassB then ;
          // or   if ImageClassA is TFPImageClass then ;
          RightTypeEl:=ResolveAliasType(TPasClassOfType(RightTypeEl).DestType);
          if (CheckClassesAreRelated(LeftTypeEl,RightTypeEl)<>cIncompatible) then
            begin
            SetBaseType(btBoolean);
            exit;
            end
          end
        else
          RaiseXExpectedButYFound(20170322105252,'class type',GetElementTypeName(RightResolved.LoTypeEl),Bin.Right);
        end
      else if LeftResolved.LoTypeEl=nil then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ComputeBinaryExprRes is-operator: left=',GetResolverResultDbg(LeftResolved),' right=',GetResolverResultDbg(RightResolved));
        {$ENDIF}
        RaiseMsg(20170216152232,nLeftSideOfIsOperatorExpectsAClassButGot,sLeftSideOfIsOperatorExpectsAClassButGot,
                 [BaseTypeNames[LeftResolved.BaseType]],Bin.Left);
        end
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ComputeBinaryExprRes is-operator: left=',GetResolverResultDbg(LeftResolved),' right=',GetResolverResultDbg(RightResolved));
        {$ENDIF}
        RaiseMsg(20170216152234,nLeftSideOfIsOperatorExpectsAClassButGot,sLeftSideOfIsOperatorExpectsAClassButGot,
                 [GetElementTypeName(LeftResolved.LoTypeEl)],Bin.Left);
        end;
      end;
    eopAs:
      begin
      if (LeftTypeEl.ClassType=TPasClassType) then
        begin
        if (LeftResolved.IdentEl is TPasType)
            or (not (rrfReadable in LeftResolved.Flags)) then
          begin
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ComputeBinaryExprRes as-operator: left=',GetResolverResultDbg(LeftResolved),' right=',GetResolverResultDbg(RightResolved));
          {$ENDIF}
          RaiseIncompatibleTypeRes(20180204124711,nOperatorIsNotOverloadedAOpB,
            [OpcodeStrings[Bin.OpCode]],LeftResolved,RightResolved,Bin);
          end;
        if (RightResolved.BaseType=btContext)
            and (rrfReadable in RightResolved.Flags)
            and (RightResolved.LoTypeEl is TPasClassOfType)
            and (TPasClassType(LeftTypeEl).ObjKind=okClass) then
          begin
          // obj as ClassRefValue (lazutils' masks: Item as FMaskClass): checked
          // against the class held at run time; typed as the class-of's class.
          RightTypeEl:=ResolveAliasType(TPasClassOfType(RightResolved.LoTypeEl).DestType);
          if (RightTypeEl is TPasClassType)
              and (TPasClassType(RightTypeEl).ObjKind=okClass)
              and ((CheckClassIsClass(RightTypeEl,LeftTypeEl)<>cIncompatible)
                or (AsOperatorAllowsUpcast
                    and (CheckClassIsClass(LeftTypeEl,RightTypeEl)<>cIncompatible))) then
            begin
            SetResolverValueExpr(ResolvedEl,btContext,RightTypeEl,RightTypeEl,Bin,[rrfReadable]);
            exit;
            end;
          end;
        if RightResolved.IdentEl=nil then
          RaiseXExpectedButYFound(20170216152630,'class',GetElementTypeName(RightResolved.LoTypeEl),Bin.Right);
        if not (RightResolved.IdentEl is TPasType) then
          RaiseXExpectedButYFound(20170216152632,'class',RightResolved.IdentEl.Name,Bin.Right);
        if not (RightResolved.BaseType=btContext) then
          RaiseXExpectedButYFound(20180426195816,'class',RightResolved.IdentEl.Name,Bin.Right);
        RightTypeEl:=RightResolved.LoTypeEl;
        if RightTypeEl is TPasClassType then
          begin
          if TPasClassType(LeftTypeEl).ObjKind=TPasClassType(RightTypeEl).ObjKind then
            begin
            // Two INTERFACES need no relation at all: the cast is a
            // QueryInterface at RUN time, and ppcx64 accepts it for any pair
            // (fcl-sdo asks an ISDODataObject for its IDataObjectObserver).
            if (TPasClassType(LeftTypeEl).ObjKind=okInterface)
                and (not TPasClassType(LeftTypeEl).IsExternal)
                and (not TPasClassType(RightTypeEl).IsExternal) then
              begin
              SetRightValueExpr([rrfReadable]);
              exit;
              end;
            // e.g. classinst as classtype. ppcx64 lets `as` UPCAST as well - it
            // only demands that the two types be RELATED, and the runtime check
            // decides the rest: chm's TFileEntryList does
            // `(Self as TList).Add(..)` to reach the ancestor's method. pas2js
            // keeps the stricter rule (TestClass_OperatorAsOnNonDescendantFail),
            // so the upcast goes through a HOOK that only pas2llvm turns on.
            if (CheckSrcIsADstType(RightResolved,LeftResolved)<>cIncompatible)
                or (AsOperatorAllowsUpcast
                    and (CheckSrcIsADstType(LeftResolved,RightResolved)<>cIncompatible)) then
              begin
              SetRightValueExpr([rrfReadable]);
              exit;
              end;
            end
          else if TPasClassType(LeftTypeEl).ObjKind=okInterface then
            begin
            if (TPasClassType(RightTypeEl).ObjKind=okClass)
                and (not TPasClassType(RightTypeEl).IsExternal) then
              begin
              // e.g. intfvar as classtype
              SetRightValueExpr([rrfReadable]);
              exit;
              end;
            if (TPasClassType(RightTypeEl).ObjKind=okInterface)
                and (not TPasClassType(LeftTypeEl).IsExternal)
                and (not TPasClassType(RightTypeEl).IsExternal) then
              begin
              // intfvar as intftype - two interfaces need no relation at all,
              // the cast is a QueryInterface at RUN time (fcl-sdo asks an
              // ISDODataObject for its IDataObjectObserver).
              SetRightValueExpr([rrfReadable]);
              exit;
              end;
            end
          else if TPasClassType(RightTypeEl).ObjKind=okInterface then
            begin
            if (TPasClassType(LeftTypeEl).ObjKind=okClass)
                and (not TPasClassType(LeftTypeEl).IsExternal) then
              begin
              // e.g. classinst as intftype
              SetRightValueExpr([rrfReadable]);
              exit;
              end;
            end;
          end;
        RaiseIncompatibleTypeRes(20180324190713,nTypesAreNotRelatedXY,[],LeftResolved,RightResolved,Bin);
        end
      else if LeftTypeEl.ClassType=TPasGenericTemplateType then
        begin
        // genericvar as ...
        if (LeftResolved.IdentEl is TPasType)
            or (not (rrfReadable in LeftResolved.Flags)) then
          RaiseIncompatibleTypeRes(20190908191127,nOperatorIsNotOverloadedAOpB,
            [OpcodeStrings[Bin.OpCode]],LeftResolved,RightResolved,Bin);
        if RightResolved.IdentEl=nil then
          RaiseXExpectedButYFound(20190908191202,'class',GetElementTypeName(RightResolved.LoTypeEl),Bin.Right);
        if not (RightResolved.IdentEl is TPasType) then
          RaiseXExpectedButYFound(20190908191204,'class',RightResolved.IdentEl.Name,Bin.Right);
        if not (RightResolved.BaseType=btContext) then
          RaiseXExpectedButYFound(20190908191206,'class',RightResolved.IdentEl.Name,Bin.Right);
        RightTypeEl:=RightResolved.LoTypeEl;
        if RightTypeEl is TPasClassType then
          begin
          // e.g. genericvar as classtype
          SetRightValueExpr([rrfReadable]);
          exit;
          end;
        RaiseIncompatibleTypeRes(20190908192345,nTypesAreNotRelatedXY,[],LeftResolved,RightResolved,Bin);
        end;
      end;
    eopLessThan,eopGreaterThan, eopLessthanEqual,eopGreaterThanEqual:
      if (rrfReadable in LeftResolved.Flags)
          and (rrfReadable in RightResolved.Flags) then
        begin
        RightTypeEl:=RightResolved.LoTypeEl;
        if (LeftTypeEl.ClassType=TPasEnumType) and (LeftTypeEl=RightTypeEl) then
          begin
          SetBaseType(btBoolean);
          exit;
          end
        else if (LeftTypeEl.ClassType=TPasPointerType)
            and (RightResolved.BaseType in btAllInteger) then
          begin
          SetBaseType(btBoolean);
          exit;
          end
        else if (LeftTypeEl.ClassType=TPasPointerType) then
          begin
          if (RightResolved.BaseType in [btPointer, btNil]) then
            begin
            // Typed pointer vs untyped pointer or nil
            if (bsPointerMath in CurrentParser.Scanner.CurrentBoolSwitches)
                or IsPointerMathType(LeftTypeEl) then
              begin
              SetBaseType(btBoolean);
              exit;
              end;
            end
          else if (RightResolved.BaseType = btContext)
              and (RightResolved.LoTypeEl.ClassType = TPasPointerType) then
            begin
            // Typed pointer vs typed pointer. Matching FPC (nadd.pas): equal
            // pointer types are always comparable; two DIFFERENT pointer types
            // are comparable only in Delphi mode, and then only if $pointermath
            // is active at the use site OR both types were declared under
            // {$POINTERMATH ON} (tpointermath4). In non-Delphi mode comparing
            // different pointer types is incompatible even with $pointermath.
            if (LeftTypeEl = RightResolved.LoTypeEl)
                or ((msDelphi in CurrentParser.CurrentModeswitches)
                    and ((bsPointerMath in CurrentParser.Scanner.CurrentBoolSwitches)
                         or (IsPointerMathType(LeftTypeEl)
                             and IsPointerMathType(RightResolved.LoTypeEl)))) then
              begin
              SetBaseType(btBoolean);
              exit;
              end;
            end;
          end;
        end;
    eopSubIdent:
      begin
      ResolvedEl:=RightResolved;
      exit;
      end;
    eopAdd,eopSubtract:
      if (rrfReadable in LeftResolved.Flags)
          and (rrfReadable in RightResolved.Flags) then
        begin
        if (LeftTypeEl.ClassType=TPasArrayType) then
          begin
          if IsDynArray(LeftTypeEl)
              and (Bin.OpCode=eopAdd)
              and ElHasModeSwitch(Bin,msArrayOperators)
              and ((RightResolved.BaseType in [btArrayOrSet,btArrayLit])
                or IsDynArray(RightResolved.LoTypeEl)) then
            begin
            // dynarr+[...]
            CheckAssignCompatibilityArrayType(LeftResolved,RightResolved,Bin,true);
            SetLeftValueExpr([rrfReadable]);
            exit;
            end;
          end
        else if LeftTypeEl.ClassType=TPasPointerType then
          begin
          // PAnsiChar/PWideChar + string/char/chararray concatenates like a
          // string, the mirror of the RightIsCharPointer case (libxml xmlxsd)
          if (Bin.OpCode=eopAdd) and LeftIsCharPointer(CharBT)
              and ((RightResolved.BaseType in btAllStringAndChars)
                or ((RightResolved.BaseType=btContext) and (RightResolved.LoTypeEl<>nil)
                    and (RightResolved.LoTypeEl.ClassType=TPasArrayType))
                or RightIsCharPointer) then
            begin
            if (CharBT=btWideChar) and (FBaseTypes[btUnicodeString]<>nil) then
              SetBaseType(btUnicodeString)
            else if (CharBT<>btWideChar) and (FBaseTypes[btAnsiString]<>nil) then
              SetBaseType(btAnsiString)
            else
              SetBaseType(btString);
            exit;
            end;
          if (RightResolved.BaseType in btAllInteger)
              and (PointerMathBoolSwitchEnabled(Bin)
                or IsPointerMathType(LeftTypeEl)) then
            begin
            // TypedPointer+Integer
            SetLeftValueExpr([rrfReadable]);
            exit;
            end;
          // Pointer - Pointer (incl. PChar): the difference in elements (PtrInt).
          // Always allowed for two pointers, independent of {$POINTERMATH}.
          {$IFNDEF PAS2JS}
          if (Bin.OpCode=eopSubtract)
              and ((RightResolved.BaseType=btPointer)
                or ((RightResolved.BaseType=btContext)
                    and (RightResolved.LoTypeEl is TPasPointerType))) then
            begin
            SetBaseType(btInt64);
            exit;
            end;
          {$ENDIF}  
          end;
        end;
    end;

    end
  else if LeftResolved.BaseType in [btSet,btArrayOrSet] then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (rrfReadable in RightResolved.Flags) then
      begin
      if (RightResolved.BaseType in [btSet,btArrayOrSet]) then
        case Bin.OpCode of
        eopAdd,
        eopSubtract,
        eopMultiply,
        eopSymmetricaldifference,
        eopLessthanEqual,
        eopGreaterThanEqual:
          begin
          if RightResolved.LoTypeEl=nil then
            begin
            // right is empty set/array
            if Bin.OpCode in [eopLessthanEqual,eopGreaterThanEqual] then
              SetBaseType(btBoolean)
            else
              begin
              ResolvedEl:=LeftResolved;
              ResolvedEl.IdentEl:=nil;
              ResolvedEl.ExprEl:=Bin;
              end;
            exit;
            end
          else if LeftResolved.LoTypeEl=nil then
            begin
            // left is empty set/array
            if Bin.OpCode in [eopLessthanEqual,eopGreaterThanEqual] then
              SetBaseType(btBoolean)
            else
              begin
              ResolvedEl:=RightResolved;
              ResolvedEl.IdentEl:=nil;
              ResolvedEl.ExprEl:=Bin;
              end;
            exit;
            end
          else if (LeftResolved.SubType=RightResolved.SubType)
              or ((LeftResolved.SubType in btAllBooleans)
                and (RightResolved.SubType in btAllBooleans))
              or ((LeftResolved.SubType in btAllInteger)
                and (RightResolved.SubType in btAllInteger)) then
            begin
            // compatible set
            if Bin.OpCode in [eopLessthanEqual,eopGreaterThanEqual] then
              SetBaseType(btBoolean)
            else
              begin
              ResolvedEl:=LeftResolved;
              ResolvedEl.IdentEl:=nil;
              ResolvedEl.ExprEl:=Bin;
              end;
            exit;
            end;
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ComputeBinaryExprRes + - * >< Sets LeftSubType='+BaseTypeNames[LeftResolved.SubType]
            +' RightSubType='+BaseTypeNames[RightResolved.SubType]);
          {$ENDIF}
          end;
        end
      else if RightResolved.BaseType=btContext then
        begin
        RightTypeEl:=RightResolved.LoTypeEl;
        if RightTypeEl.ClassType=TPasArrayType then
          begin
          if IsDynArray(RightTypeEl) then
            begin
            // [...]+dynarr
            CheckAssignCompatibilityArrayType(RightResolved,LeftResolved,Bin,true);
            SetRightValueExpr([rrfReadable]);
            exit;
            end;
          end;
        end;
      end;
    end
  else if LeftResolved.BaseType=btArrayLit then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (rrfReadable in RightResolved.Flags)
        and (Bin.OpCode=eopAdd)
        and ElHasModeSwitch(Bin,msArrayOperators) then
      begin
      if RightResolved.BaseType=btArrayLit then
        begin
        if LeftResolved.LoTypeEl<>nil then
          ResolvedEl:=LeftResolved
        else
          ResolvedEl:=RightResolved;
        ResolvedEl.IdentEl:=nil;
        ResolvedEl.ExprEl:=Bin;
        exit;
        end
      else if (RightResolved.BaseType=btContext)
          and (RightResolved.LoTypeEl.ClassType=TPasArrayType) then
        begin
        ResolvedEl:=RightResolved;
        ResolvedEl.IdentEl:=nil;
        ResolvedEl.ExprEl:=Bin;
        exit;
        end;
      end;
    end
  else if LeftResolved.BaseType=btModule then
    begin
    if Bin.OpCode=eopSubIdent then
      begin
      ResolvedEl:=RightResolved;
      exit;
      end;
    end;
  FOperatorLastChance:=true;
  try
    if TryResolveOperatorOverload(Bin, ResolvedEl, LeftResolved, RightResolved) then
      exit;
  finally
    FOperatorLastChance:=false;
  end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeBinaryExprRes OpCode=',OpcodeStrings[Bin.OpCode],' Kind=',Bin.Kind,' Left=',GetResolverResultDbg(LeftResolved),' Right=',GetResolverResultDbg(RightResolved));
  {$ENDIF}
  RaiseIncompatibleTypeRes(20180204114631,nOperatorIsNotOverloadedAOpB,
    [OpcodeStrings[Bin.OpCode]],LeftResolved,RightResolved,Bin);
  if Flags=[] then ;
end;

function TPasResolver.TryResolveOperatorOverload(Bin: TBinaryExpr;
  out ResolvedEl: TPasResolverResult;
  var LeftResolved, RightResolved: TPasResolverResult): Boolean;
begin
  Result := False;
  ResolvedEl:=Default(TPasResolverResult);
  if Bin=nil then ;
  if LeftResolved.BaseType=btNone then ;
  if RightResolved.BaseType=btNone then ;
end;

function TPasResolver.TryResolveUnaryOperator(El: TUnaryExpr;
  var ResolvedEl: TPasResolverResult;
  Flags: TPasResolverComputeFlags): Boolean;
begin
  Result := False;
  if El=nil then ;
  if ResolvedEl.BaseType=btNone then ;
  if Flags=[] then ;
end;

procedure TPasResolver.CheckOperatorOverloadable(Op: TPasOperator);

  function FixWideCharBT(BT: TResolverBaseType; ArgType: TPasType): TResolverBaseType;
  begin
    // In our system unit, WideChar=Word and UnicodeChar=WideChar,
    // so they resolve to btWord. Fix to btWideChar for char-type checks.
    Result := BT;
    if (BT in btAllInteger) and (ArgType <> nil) then
      if SameText(ArgType.Name, 'WideChar') or SameText(ArgType.Name, 'UnicodeChar') then
        Result := btWideChar;
  end;

var
  Args: TFPList;
  LeftResolved, RightResolved, ResultResolved: TPasResolverResult;
  LeftBT, RightBT: TResolverBaseType;
  LeftTypeEl, RightTypeEl: TPasType;
  IsBinary: Boolean;
  OpType: TOperatorType;
begin
  Args := Op.ProcType.Args;
  if Args.Count < 1 then exit;

  // Resolve left (first) parameter type
  ComputeElement(TPasArgument(Args[0]).ArgType, LeftResolved, [rcType]);
  LeftBT := FixWideCharBT(LeftResolved.BaseType, TPasArgument(Args[0]).ArgType);
  LeftTypeEl := LeftResolved.LoTypeEl;

  // Conversion operators (Explicit/Implicit): at least one of the source or
  // result type must be a non-simple (user) type — a conversion between two
  // built-in simple types (e.g. LongInt->Boolean) is not overloadable.
  if (Op.OperatorType in [otExplicit, otImplicit]) and
     (Op.ProcType is TPasFunctionType) and
     (TPasFunctionType(Op.ProcType).ResultEl <> nil) then
    begin
    ComputeElement(TPasFunctionType(Op.ProcType).ResultEl.ResultType,
      ResultResolved, [rcType]);
    if (LeftResolved.BaseType <> btContext) and
       (ResultResolved.BaseType <> btContext) then
      RaiseMsg(20260303120002, nImpossibleOperatorOverload,
        sImpossibleOperatorOverload, [], Op);
    end;

  IsBinary := Args.Count >= 2;
  if IsBinary then
    begin
    ComputeElement(TPasArgument(Args[1]).ArgType, RightResolved, [rcType]);
    RightBT := FixWideCharBT(RightResolved.BaseType, TPasArgument(Args[1]).ArgType);
    RightTypeEl := RightResolved.LoTypeEl;
    end
  else
    begin
    RightBT := btNone;
    RightTypeEl := nil;
    end;

  // Normalize operator type: parser assigns unary form for +/- before
  // correcting based on arg count (correction happens after FinishProcedureType)
  OpType := Op.OperatorType;
  if IsBinary then
    case OpType of
      otPositive: OpType := otPlus;
      otNegative: OpType := otMinus;
    end;

  if IsBinary then
    begin
    if not IsBinaryOperatorOverloadable(OpType,
        LeftBT, LeftTypeEl, RightBT, RightTypeEl) then
      RaiseMsg(20260303120000, nImpossibleOperatorOverload,
        sImpossibleOperatorOverload, [], Op);
    end
  else
    begin
    if not IsUnaryOperatorOverloadable(OpType, LeftBT, LeftTypeEl) then
      RaiseMsg(20260303120001, nImpossibleOperatorOverload,
        sImpossibleOperatorOverload, [], Op);
    end;
end;

function TPasResolver.IsUnaryOperatorOverloadable(OpType: TOperatorType;
  LeftBT: TResolverBaseType; LeftTypeEl: TPasType): Boolean;
begin
  case OpType of
    otNegative, otPositive:
      begin
      if LeftBT in (btAllInteger + btAllFloats + btAllBooleans + [btCurrency]) then
        exit(False);
      if (LeftBT = btContext) and (LeftTypeEl is TPasEnumType) then
        exit(False);
      end;
    otLogicalNot:
      begin
      if LeftBT in (btAllInteger + btAllBooleans) then
        exit(False);
      end;
  end;
  Result := True;
end;

function TPasResolver.IsBinaryOperatorOverloadable(OpType: TOperatorType;
  LeftBT: TResolverBaseType; LeftTypeEl: TPasType;
  RightBT: TResolverBaseType; RightTypeEl: TPasType): Boolean;
const
  otIdentity = [otEqual, otNotEqual];
  otOrderTheoretic = [otEqual, otNotEqual, otLessThan, otLessEqualThan,
                      otGreaterThan, otGreaterEqualThan];
  otArithmetic = [otPlus, otMinus, otMul, otDivision, otDiv, otMod];
  otRational = [otPlus, otMinus, otMul, otDivision];
  otNumerical = [otPlus, otMinus, otMul, otDivision, otDiv, otMod];
  // Parser may assign logical or bitwise variant for and/or/xor; include both
  otBitManip = [otBitwiseAnd, otBitWiseOr, otbitwiseXor,
                otLogicalAnd, otLogicalOr, otLogicalXor,
                otLeftShift, otRightShift];
  otLogical = [otLogicalAnd, otLogicalOr, otLogicalXor,
               otBitwiseAnd, otBitWiseOr, otbitwiseXor];
  otPointerArith = [otPlus, otMinus];
  otSetOps = [otEqual, otNotEqual, otLessThan, otLessEqualThan,
              otGreaterThan, otGreaterEqualThan,
              otPlus, otMinus, otMul, otSymmetricalDifference];
  otStringOps = [otEqual, otNotEqual, otLessThan, otLessEqualThan,
                 otGreaterThan, otGreaterEqualThan, otPlus];

  function IsOrdOrFloat(BT: TResolverBaseType): Boolean;
  begin
    Result := BT in (btAllInteger + btAllFloats + btAllBooleans + [btCurrency]);
  end;

  function IsInteger(BT: TResolverBaseType): Boolean;
  begin
    Result := BT in btAllInteger;
  end;

  function IsFloat(BT: TResolverBaseType): Boolean;
  begin
    Result := BT in btAllFloats;
  end;

  function IsCharName(El: TPasType): Boolean;
  begin
    // In our system unit, WideChar=Word and UnicodeChar=WideChar,
    // so they resolve to btWord, not btWideChar. Check name as fallback.
    Result := (El <> nil) and
      (SameText(El.Name, 'WideChar') or SameText(El.Name, 'UnicodeChar'));
  end;

  function IsCharBT(BT: TResolverBaseType): Boolean;
  begin
    Result := BT in btAllChars;
  end;

  function IsBoolBT(BT: TResolverBaseType): Boolean;
  begin
    Result := BT in btAllBooleans;
  end;

  function IsStringBT(BT: TResolverBaseType): Boolean;
  begin
    Result := BT in btAllStrings;
  end;

  function IsSet(BT: TResolverBaseType): Boolean;
  begin
    Result := BT in [btSet, btArrayOrSet];
  end;

  function IsEnum(BT: TResolverBaseType; El: TPasType): Boolean;
  begin
    Result := (BT = btContext) and (El is TPasEnumType);
  end;

  function IsRecord(BT: TResolverBaseType; El: TPasType): Boolean;
  begin
    Result := (BT = btContext) and (El is TPasRecordType);
  end;

  function IsPointerType(BT: TResolverBaseType; El: TPasType): Boolean;
  begin
    Result := (BT = btPointer) or ((BT = btContext) and (El is TPasPointerType));
  end;

  function IsClass(BT: TResolverBaseType; El: TPasType): Boolean;
  begin
    Result := (BT = btContext) and (El is TPasClassType);
  end;

  function IsPCharType(BT: TResolverBaseType; El: TPasType): Boolean;
  var
    SubResolved: TPasResolverResult;
    DestType: TPasType;
  begin
    // PChar = pointer to char type
    Result := False;
    if (BT = btContext) and (El is TPasPointerType) then
      begin
      DestType := TPasPointerType(El).DestType;
      ComputeElement(DestType, SubResolved, []);
      if SubResolved.BaseType in btAllChars then
        Result := True
      else if IsCharName(DestType) then
        Result := True;
      end;
  end;

  function IsCharArray(BT: TResolverBaseType; El: TPasType): Boolean;
  var
    ArrType: TPasArrayType;
    SubResolved: TPasResolverResult;
    ElType: TPasType;
  begin
    // array of char (open or static)
    Result := False;
    if (BT = btContext) and (El is TPasArrayType) then
      begin
      ArrType := TPasArrayType(El);
      ElType := ArrType.ElType;
      if ElType = nil then exit;
      ComputeElement(ElType, SubResolved, []);
      if SubResolved.BaseType in btAllChars then
        Result := True
      else if IsCharName(ElType) then
        Result := True;
      end;
  end;

  function IsStringLike(BT: TResolverBaseType; El: TPasType): Boolean;
  begin
    Result := (BT in btAllStringAndChars) or IsPCharType(BT, El) or IsCharArray(BT, El);
  end;

  function InternalCheck(LBT: TResolverBaseType; LEl: TPasType;
    RBT: TResolverBaseType; REl: TPasType): Boolean;
  begin
    Result := True; // default: allow

    // Records, variants: always allowed
    if IsRecord(LBT, LEl){$IFNDEF PAS2JS} or (LBT = btVariant) {$ENDIF} then
      exit(True);

    // Enum left
    if IsEnum(LBT, LEl) then
      begin
      if IsSet(RBT) and (OpType = otIn) then exit(False);
      if IsEnum(RBT, REl) and (OpType in otOrderTheoretic + [otPlus, otMinus]) then exit(False);
      if IsInteger(RBT) and (OpType in otOrderTheoretic + otBitManip + otArithmetic) then exit(False);
      exit(True);
      end;

    // Set left
    if IsSet(LBT) then
      begin
      if IsSet(RBT) and (OpType in otSetOps) then exit(False);
      exit(True);
      end;

    // Ordinal/float left
    if IsOrdOrFloat(LBT) then
      begin
      // Reject comparisons when both are ordinal/float
      if IsOrdOrFloat(RBT) and (OpType in otOrderTheoretic) then
        exit(False);

      if IsCharBT(LBT) then
        begin
        if IsStringLike(RBT, REl) and (OpType in otStringOps) then exit(False);
        exit(True);
        end;

      if IsBoolBT(LBT) then
        begin
        if IsBoolBT(RBT) and (OpType in otLogical) then exit(False);
        exit(True);
        end;

      if IsInteger(LBT) or (LBT = btCurrency) then
        begin
        if (IsInteger(RBT) or (RBT = btCurrency)) and (OpType in otBitManip + otNumerical) then
          exit(False);
        if IsFloat(RBT) and (OpType in otRational) then exit(False);
        if IsPointerType(RBT, REl) and (OpType in (otPointerArith - [otMinus])) then exit(False);
        exit(True);
        end;

      // Float left
      if (IsFloat(RBT) or IsInteger(RBT) or (RBT = btCurrency)) and (OpType in otRational) then
        exit(False);
      exit(True);
      end;

    // String left (pure string types - not PChar, not array-of-char)
    if IsStringBT(LBT) then
      begin
      if IsStringLike(RBT, REl) and (OpType in otStringOps) then exit(False);
      exit(True);
      end;

    // PChar or array-of-char left (pointer-like string types)
    if IsPCharType(LBT, LEl) or IsCharArray(LBT, LEl) then
      begin
      if IsStringLike(RBT, REl) and (OpType in otStringOps) then exit(False);
      if IsPointerType(RBT, REl) and (OpType in (otPointerArith - [otPlus]) + otOrderTheoretic) then
        exit(False);
      if IsInteger(RBT) and (OpType in otPointerArith) then exit(False);
      exit(True);
      end;

    // Pointer left (non-PChar)
    if IsPointerType(LBT, LEl) then
      begin
      if IsInteger(RBT) and (OpType in otPointerArith) then exit(False);
      if IsPointerType(RBT, REl) and (OpType in (otPointerArith - [otPlus]) + otOrderTheoretic) then
        exit(False);
      if IsClass(RBT, REl) and (OpType in otIdentity) then exit(False);
      exit(True);
      end;

    // Class left
    if IsClass(LBT, LEl) then
      begin
      if (IsClass(RBT, REl) or IsPointerType(RBT, REl)) and (OpType in otIdentity) then
        exit(False);
      exit(True);
      end;

    // Default: allow
    Result := True;
  end;

begin
  // Symmetric check: try both orderings
  if not InternalCheck(LeftBT, LeftTypeEl, RightBT, RightTypeEl) then
    exit(False);
  if not InternalCheck(RightBT, RightTypeEl, LeftBT, LeftTypeEl) then
    exit(False);
  Result := True;
end;

function TPasResolver.ComputeAddStringRes(const LeftResolved,
  RightResolved: TPasResolverResult; ExprEl: TPasExpr; out
  ResolvedEl: TPasResolverResult): boolean;

  procedure SetBaseType(BaseType: TResolverBaseType);
  begin
    SetResolverValueExpr(ResolvedEl,BaseType,FBaseTypes[BaseType],FBaseTypes[BaseType],
                         ExprEl,[rrfReadable]);
  end;

  procedure SetLeftValueExpr(Flags: TPasResolverResultFlags);
  begin
    SetResolverValueExpr(ResolvedEl,LeftResolved.BaseType,
      LeftResolved.LoTypeEl,LeftResolved.HiTypeEl,ExprEl,Flags);
  end;

  procedure SetRightValueExpr(Flags: TPasResolverResultFlags);
  begin
    SetResolverValueExpr(ResolvedEl,RightResolved.BaseType,
      RightResolved.LoTypeEl,RightResolved.HiTypeEl,ExprEl,Flags);
  end;

begin
  Result:=true;
  case LeftResolved.BaseType of
  btChar:
    begin
    case RightResolved.BaseType of
    btChar: SetBaseType(btString);
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiChar:
      if BaseTypeChar=btAnsiChar then
        SetBaseType(btString)
      else
        SetBaseType(btUnicodeString);
    {$endif}
    btWideChar:
      if BaseTypeChar=btWideChar then
        SetBaseType(btString)
      else
        SetBaseType(btUnicodeString);
    else
      // use right type for result
      SetRightValueExpr([rrfReadable]);
    end;
    exit;
    end;
  {$ifdef FPC_HAS_CPSTRING}
  btAnsiChar:
    begin
    case RightResolved.BaseType of
    btChar:
      if BaseTypeChar=btAnsiChar then
        SetBaseType(btString)
      else
        SetBaseType(btUnicodeString);
    btAnsiChar:
      if BaseTypeChar=btAnsiChar then
        SetBaseType(btString)
      else
        SetBaseType(btAnsiString);
    btWideChar:
      if BaseTypeChar=btWideChar then
        SetBaseType(btString)
      else
        SetBaseType(btUnicodeString);
    else
      // use right type for result
      SetRightValueExpr([rrfReadable]);
    end;
    exit;
    end;
  {$endif}
  btWideChar:
    begin
      case RightResolved.BaseType of
      btChar,{$ifdef FPC_HAS_CPSTRING}btAnsiChar,{$endif}btWideChar:
        if BaseTypeChar=btWideChar then
          SetBaseType(btString)
        else
          SetBaseType(btUnicodeString);
      else
        // use right type for result
        SetRightValueExpr([rrfReadable]);
      end;
      exit;
    end;
  {$ifdef FPC_HAS_CPSTRING}
  btShortString:
    begin
      case RightResolved.BaseType of
      btChar,btAnsiChar,btShortString,btWideChar:
        // use left type for result
        SetLeftValueExpr([rrfReadable]);
      else
        // shortstring + string => string
        SetRightValueExpr([rrfReadable]);
      end;
      exit;
    end;
  {$endif}
  btString,{$ifdef FPC_HAS_CPSTRING}btAnsiString,btRawByteString,{$endif}btWideString,btUnicodeString:
    begin
      // string + x => string. RawByteString and WideString were previously omitted,
      // so `RawByteString + RawByteString` (unix RTL path building a search dirlist)
      // wrongly reported "operator not overloaded".
      {$ifdef FPC_HAS_CPSTRING}
      // A narrow left operand widens to the wide right one: fpc gives
      // `ansistring + unicodestring` a unicodestring result, and overload
      // selection reads this type.
      if (BaseTypeChar=btAnsiChar)
          and (GetActualBaseType(LeftResolved.BaseType) in [btAnsiString,btRawByteString])
          and (GetActualBaseType(RightResolved.BaseType) in [btWideString,btUnicodeString,btWideChar]) then
        begin
        if GetActualBaseType(RightResolved.BaseType)=btWideString then
          SetBaseType(btWideString)
        else
          SetBaseType(btUnicodeString);
        exit;
        end;
      {$endif}
      SetLeftValueExpr([rrfReadable]);
      exit;
    end;
  end;
  Result:=false;
end;

procedure TPasResolver.ComputeArgumentAndExpr(Arg: TPasArgument; out
  ArgResolved: TPasResolverResult; Expr: TPasExpr; out
  ExprResolved: TPasResolverResult; SetReferenceFlags: boolean);
begin
  ComputeElement(Arg,ArgResolved,[]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeArgumentAndExpr Arg=',GetTreeDbg(Arg,2),' ArgResolved=',GetResolverResultDbg(ArgResolved));
  {$ENDIF}
  if (ArgResolved.LoTypeEl=nil) and (Arg.ArgType<>nil) then
    RaiseInternalError(20160922163628,'TypeEl=nil for '+GetTreeDbg(Arg));

  ComputeArgumentExpr(ArgResolved,Arg.Access,Expr,ExprResolved,SetReferenceFlags);
end;

procedure TPasResolver.ComputeArgumentExpr(
  const ArgResolved: TPasResolverResult; Access: TArgumentAccess;
  Expr: TPasExpr; out ExprResolved: TPasResolverResult;
  SetReferenceFlags: boolean);
var
  NeedVar: Boolean;
  RHSFlags: TPasResolverComputeFlags;
begin
  RHSFlags:=[];
  NeedVar:=Access in [argVar, argOut];
  if NeedVar then
    Include(RHSFlags,rcNoImplicitProc)
  else if IsProcedureType(ArgResolved,true)
      or (ArgResolved.BaseType=btPointer)
      or ((ArgResolved.LoTypeEl=nil) and (ArgResolved.IdentEl is TPasArgument)) then
  begin
    Include(RHSFlags,rcNoImplicitProcType);
    { In Delphi mode a bare function name passed to a PROCEDURAL parameter denotes the
      function itself, not a call: Fly(Run) = Fly(@Run). An untyped Pointer parameter is
      different -- there Delphi/FPC require the @ -- so a parameterless function call
      must still be called, e.g. Fly(Bird.ClassInfo) passes ClassInfo's Pointer result. }
    if (msDelphi in GetElModeSwitches(Expr))
        and (ArgResolved.BaseType<>btPointer) then
      Include(RHSFlags,rcNoImplicitProc);
  end;
  if SetReferenceFlags then
    Include(RHSFlags,rcSetReferenceFlags);
  ComputeElement(Expr,ExprResolved,RHSFlags);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeArgumentExpr Expr=',GetTreeDbg(Expr,2),' ExprResolved=',GetResolverResultDbg(ExprResolved),' RHSFlags=',dbgs(RHSFlags));
  {$ENDIF}
end;

procedure TPasResolver.ComputeArrayParams(Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);

  procedure ComputeIndexProperty(Prop: TPasProperty);
  begin
    if [rcConstant,rcType]*Flags<>[] then
      RaiseConstantExprExp(20170216152635,Params);
    ComputeElement(GetPasPropertyType(Prop),ResolvedEl,[rcType],StartEl);
    ResolvedEl.IdentEl:=Prop;
    ResolvedEl.Flags:=[];
    if GetPasPropertyGetter(Prop)<>nil then
      Include(ResolvedEl.Flags,rrfReadable);
    if GetPasPropertySetter(Prop)<>nil then
      Include(ResolvedEl.Flags,rrfWritable);
  end;

  procedure ComputeArrayPointer(TypeEl: TPasType);
  begin
    if TypeEl=nil then
      RaiseInternalError(20180423092254);
    ComputeElement(TypeEl,ResolvedEl,[rcType],Params);
    ResolvedEl.IdentEl:=nil;
    ResolvedEl.ExprEl:=Params;
    ResolvedEl.Flags:=ResolvedEl.Flags+[rrfReadable,rrfWritable];
  end;

  procedure StringToCharElement(var R: TPasResolverResult);
  begin
    case GetActualBaseType(R.BaseType) of
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiString,btRawByteString,btShortString:
      if BaseTypeChar=btAnsiChar then
        R.BaseType:=btChar
      else
        R.BaseType:=btAnsiChar;
    {$endif}
    btWideString,btUnicodeString:
      if BaseTypeChar=btWideChar then
        R.BaseType:=btChar
      else
        R.BaseType:=btWideChar;
    else
      RaiseNotYetImplemented(20170417202354,Params);
    end;
    R.LoTypeEl:=FBaseTypes[R.BaseType];
    R.HiTypeEl:=R.LoTypeEl;
    R.ExprEl:=Params;
    // A string element is assignable everywhere; whether it is also a real
    // WRITABLE l-value - one whose address can be passed to a var/out parameter
    // - is a backend question. ppcx64 takes `Bump(s[2])` and writes back through
    // it (making the string unique first); a JS backend has no addressable
    // character, so the base keeps saying no.
    if StringElementIsWritable and (rrfWritable in R.Flags) then
      R.Flags:=R.Flags-[rrfCanBeStatement]+[rrfAssignable]
    else
      R.Flags:=R.Flags-[rrfWritable,rrfCanBeStatement]+[rrfAssignable];
  end;

var
  TypeEl, ElType, StrElType, NextTypeEl: TPasType;
  ArrayEl, PartialSub: TPasArrayType;
  ArgNo: Integer;
  OrigResolved: TPasResolverResult;
  ClassOrRecordScope: TPasClassOrRecordScope;
  Ref: TResolvedReference;
begin
  ComputeElement(Params.Value,ResolvedEl,
    Flags-[rcNoImplicitProc,rcNoImplicitProcType],StartEl);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeArrayParams ResolvedEl=',GetResolverResultDbg(ResolvedEl));
  {$ENDIF}
  if DerefPointerToArray(ResolvedEl,Params) then
    ResolvedEl.ExprEl:=Params.Value;
  if ResolvedEl.BaseType in btAllStrings then
    // stringvar[] => char; ResolvedEl.IdentEl stays the string var
    StringToCharElement(ResolvedEl)
  else if ResolvedEl.BaseType=btPointer then
    // (@something)[]
    ComputeArrayPointer(ResolvedEl.LoTypeEl)
  {$IFNDEF PAS2JS}
  else if ResolvedEl.BaseType=btVariant then
    begin
    // Variant[i] => a Variant element (VarArrayGet/VarArrayPut at runtime). Keep
    // the variant type; the element is a readable, assignable l-value.
    ResolvedEl.IdentEl:=nil;
    ResolvedEl.ExprEl:=Params;
    ResolvedEl.Flags:=ResolvedEl.Flags+[rrfReadable,rrfWritable,rrfAssignable];
    end
  {$ENDIF}
  else if (Params.Value is TParamsExpr)
      and (TParamsExpr(Params.Value).Kind=pekArrayParams)
      and (Params.CustomData is TResolvedReference)
      and (TResolvedReference(Params.CustomData).Declaration is TPasProperty) then
    // (a[i])[j]: the second index uses the default property of a[i]'s TYPE. The
    // property that produced a[i] is already consumed, so it must not be applied
    // twice. fcl-stl's ghashmap reads (FData[Fh])[Fp].
    ComputeIndexProperty(TPasProperty(TResolvedReference(Params.CustomData).Declaration))
  else if (ResolvedEl.IdentEl is TPasProperty)
      and (GetPasPropertyArgs(TPasProperty(ResolvedEl.IdentEl)).Count>0)
      and not ((Params.Value is TParamsExpr)
               and (TParamsExpr(Params.Value).Kind=pekArrayParams)) then
    // property with args. Not when Params.Value already passed them: in
    // Obj.Prop['a'][2] the [2] indexes the value Prop['a'] returns.
    ComputeIndexProperty(TPasProperty(ResolvedEl.IdentEl))
  else if ResolvedEl.BaseType=btContext then
    begin
    TypeEl:=ResolvedEl.LoTypeEl;
    if (TypeEl.ClassType=TPasClassType)
        or (TypeEl.ClassType=TPasRecordType)
        or (TypeEl.ClassType=TPasClassOfType) then
      begin
      if not (Params.CustomData is TResolvedReference) then
        RaiseNotYetImplemented(20190125143203,Params,GetObjName(Params.CustomData));
      Ref:=TResolvedReference(Params.CustomData);
      if Ref.Declaration is TPasProperty then
        ComputeIndexProperty(TPasProperty(Ref.Declaration))
      else if TypeEl is TPasMembersType then
        begin
        ClassOrRecordScope:=NoNil(TypeEl.CustomData) as TPasClassOrRecordScope;
        ComputeArrayParams_Class(Params,ResolvedEl,ClassOrRecordScope,Flags,StartEl);
        end
      else
        RaiseNotYetImplemented(20161010174916,Params);
      end
    else if TypeEl.ClassType=TPasArrayType then
      begin
      if not (rrfReadable in ResolvedEl.Flags) then
        RaiseMsg(20170517001140,nIllegalQualifierAfter,sIllegalQualifierAfter,
          ['[',TypeEl.ElementTypeName],Params);
      ArrayEl:=TPasArrayType(TypeEl);
      ArgNo:=0;
      PartialSub:=nil;
      StrElType:=nil;
      repeat
        if length(ArrayEl.Ranges)=0 then
          begin
          inc(ArgNo); // dynamic/open array has one dimension
          if IsDynArray(ArrayEl) then
            Include(ResolvedEl.Flags,rrfWritable); // dynamic array elements are writable
          end
        else
          begin
          // Partial index of a multi-range static array (fewer subscripts than
          // dimensions, e.g. a[0] on array[0..2,0..2]) yields a sub-array of
          // the remaining dimensions.
          if length(Params.Params)-ArgNo<length(ArrayEl.Ranges) then
            begin
            PartialSub:=GetPartialArrayType(ArrayEl,length(Params.Params)-ArgNo);
            ArgNo:=length(Params.Params);
            break;
            end;
          inc(ArgNo,length(ArrayEl.Ranges)); // static array has several dimensions
          end;
        if ArgNo>length(Params.Params) then
          RaiseInternalError(20161010185535);
        if ArgNo=length(Params.Params) then
          break;
        // continue in sub array
        NextTypeEl:=ResolveAliasType(ArrayEl.ElType);
        if NextTypeEl=nil then
          RaiseInternalError(20260803142000);
        if NextTypeEl.ClassType<>TPasArrayType then
          begin
          // a[i,j] on an array of string: the last index picks a character of
          // the string element.
          StrElType:=NextTypeEl;
          break;
          end;
        ArrayEl:=TPasArrayType(NextTypeEl);
      until false;
      OrigResolved:=ResolvedEl;
      // Array slice: arr[a..b] with a range index yields a sub-array of the
      // same element type (not a single element), e.g. usable in for-in. The
      // last index being a pekRange marks the slice. fcl-passrc does not model
      // this; the real compiler does.
      if StrElType<>nil then
        begin
        ComputeElement(StrElType,ResolvedEl,Flags,StartEl);
        ResolvedEl.IdentEl:=OrigResolved.IdentEl;
        ResolvedEl.Flags:=OrigResolved.Flags*[rrfReadable,rrfWritable];
        if IsDynArray(ArrayEl) then
          Include(ResolvedEl.Flags,rrfWritable);
        StringToCharElement(ResolvedEl);
        end
      else if PartialSub<>nil then
        begin
        // Partial multi-dim index: result is the synthesized sub-array type.
        ComputeElement(PartialSub,ResolvedEl,Flags,StartEl);
        ResolvedEl.IdentEl:=OrigResolved.IdentEl;
        ResolvedEl.ExprEl:=OrigResolved.ExprEl;
        ResolvedEl.Flags:=OrigResolved.Flags*[rrfReadable,rrfWritable];
        end
      else if (length(Params.Params)>0)
          and (Params.Params[length(Params.Params)-1] is TBinaryExpr)
          and (TBinaryExpr(Params.Params[length(Params.Params)-1]).Kind=pekRange) then
        begin
        ComputeElement(ArrayEl,ResolvedEl,Flags,StartEl);
        ResolvedEl.IdentEl:=OrigResolved.IdentEl;
        ResolvedEl.ExprEl:=Params;
        ResolvedEl.Flags:=OrigResolved.Flags*[rrfReadable,rrfWritable];
        end
      else
        begin
        ElType:=GetArrayElType(ArrayEl);
        ComputeElement(ElType,ResolvedEl,Flags,StartEl);
        // identifier and value is the array itself
        ResolvedEl.IdentEl:=OrigResolved.IdentEl;
        ResolvedEl.ExprEl:=OrigResolved.ExprEl;
        ResolvedEl.Flags:=OrigResolved.Flags*[rrfReadable,rrfWritable];
        if IsDynArray(ArrayEl) then
          // dyn array elements are writable independent of the array
          Include(ResolvedEl.Flags,rrfWritable);
        end;
      end
    else if TypeEl.ClassType=TPasPointerType then
      ComputeArrayPointer(TPasPointerType(TypeEl).DestType)
    else if TypeEl.ClassType=TPasGenericTemplateType then
      begin
      // value[i] where value has a generic template type: the element type is
      // unknown until specialization. Keep ResolvedEl as the template type so
      // callers (IsGenericTemplType) defer; the specialized copy computes the
      // real element type.
      ResolvedEl.ExprEl:=Params;
      Include(ResolvedEl.Flags,rrfReadable);
      end
    else
      RaiseNotYetImplemented(20161010151727,Params,GetResolverResultDbg(ResolvedEl));
    end
  else
    RaiseNotYetImplemented(20160928174212,Params,GetResolverResultDbg(ResolvedEl));
end;

procedure TPasResolver.ComputeArrayParams_Class(Params: TParamsExpr;
  var ResolvedEl: TPasResolverResult; ClassOrRecScope: TPasClassOrRecordScope;
  Flags: TPasResolverComputeFlags; StartEl: TPasElement);
begin
  RaiseNotYetImplemented(20190125142240,Params);
  if Params=nil then ;
  if ClassOrRecScope=nil then ;
  if Flags=[] then ;
  if StartEl=nil then ;
  SetResolverIdentifier(ResolvedEl,btNone,nil,nil,nil,[]);
end;

procedure TPasResolver.ComputeFuncParams(Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
var
  DeclEl: TPasElement;
  BuiltInProc: TResElDataBuiltInProc;
  Proc: TPasProcedure;
  ParamResolved: TPasResolverResult;
  Ref: TResolvedReference;
  DeclType: TPasType;
  Param0: TPasExpr;
  InvokeProcType: TPasProcedureType;

begin
  Ref:=GetParamsValueRef(Params);
  if Ref=nil then
    begin
    // inline call of anonymous proc: procedure begin ... end(args)
    if Params.Value is TProcedureExpr then
      begin
      DeclEl:=TProcedureExpr(Params.Value).Proc;
      if TPasProcedure(DeclEl).ProcType is TPasFunctionType then
        ComputeResultElement(TPasFunctionType(TPasProcedure(DeclEl).ProcType).ResultEl,
          ResolvedEl,Flags+[rcCall],StartEl)
      else
        SetResolverTypeExpr(ResolvedEl,btProc,
          TPasProcedure(DeclEl).ProcType,TPasProcedure(DeclEl).ProcType,[]);
      Include(ResolvedEl.Flags,rrfCanBeStatement);
      exit;
      end;
    // T.Method(x) on a still-unbound generic template type: ResolveSubIdent
    // left it unresolved, so the result type is unknown until specialization.
    // Keep the template type so callers (IsGenericTemplType) defer.
    if (Params.Value is TBinaryExpr)
        and (TBinaryExpr(Params.Value).OpCode=eopSubIdent) then
      begin
      ComputeElement(TBinaryExpr(Params.Value).Left,ResolvedEl,Flags,StartEl);
      if IsGenericTemplType(ResolvedEl) then
        begin
        ResolvedEl.ExprEl:=Params;
        Include(ResolvedEl.Flags,rrfReadable);
        exit;
        end;
      end;
    RaiseNotYetImplemented(20160928174124,Params);
    end;
  DeclEl:=Ref.Declaration;
  // `List[i](..)`: List[i] refers to the default array property; what is
  // called is the procedure type of its elements
  if (DeclEl is TPasProperty) and (Params.Value is TParamsExpr) then
    begin
    ComputeElement(Params.Value,ResolvedEl,[rcNoImplicitProcType]);
    if IsProcedureType(ResolvedEl,true) then
      DeclEl:=ResolvedEl.LoTypeEl;
    end;
  {$IFDEF FPC}
  // Variant dispatch: v.Foo(args) where DeclEl = the variant variable
  if (DeclEl is TPasVariable) then
    begin
    ComputeElement(DeclEl,ResolvedEl,[]);
    if ResolvedEl.BaseType=btVariant then
      begin
      // Result of variant dispatch call is Variant
      SetResolverTypeExpr(ResolvedEl,btVariant,
        ResolvedEl.LoTypeEl,ResolvedEl.HiTypeEl,
        [rrfReadable,rrfCanBeStatement]);
      exit;
      end;
    end;
  {$ENDIF}
  if DeclEl.ClassType=TPasUnresolvedSymbolRef then
    begin
    if DeclEl.CustomData.ClassType=TResElDataBuiltInProc then
      begin
      BuiltInProc:=TResElDataBuiltInProc(DeclEl.CustomData);
      if Assigned(BuiltInProc.GetCallResult) then
        // built-in function
        BuiltInProc.GetCallResult(BuiltInProc,Params,ResolvedEl)
      else
        // built-in procedure
        SetResolverIdentifier(ResolvedEl,btProc,BuiltInProc.Proc,
          BuiltInProc.Proc,BuiltInProc.Proc,[]);
      if bipfCanBeStatement in BuiltInProc.Flags then
        Include(ResolvedEl.Flags,rrfCanBeStatement);
      end
    else if DeclEl.CustomData is TResElDataBaseType then
      begin
      // type cast to base type
      DeclType:=TPasUnresolvedSymbolRef(DeclEl);
      if length(Params.Params)<>1 then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ComputeFuncParams DeclEl=',GetObjName(DeclEl));
        {$ENDIF}
        RaiseMsg(20180503105409,nWrongNumberOfParametersForTypeCast,
          sWrongNumberOfParametersForTypeCast,[DeclType.Name],Params);
        end;
      Param0:=Params.Params[0];
      ComputeElement(Param0,ParamResolved,[]);
      ComputeTypeCast(DeclType,DeclType,Param0,ParamResolved,ResolvedEl,Flags);
      end
    else
      RaiseNotYetImplemented(20161006133040,Params,GetResolverResultDbg(ResolvedEl));
    end
  else
    begin
    // normal identifier (not built-in)
    ComputeElement(DeclEl,ResolvedEl,Flags+[rcNoImplicitProc],StartEl);
    if ResolvedEl.BaseType=btProc then
      begin
      if not (ResolvedEl.IdentEl is TPasProcedure) then
        RaiseNotYetImplemented(20160928180201,Params,GetResolverResultDbg(ResolvedEl));
      Proc:=TPasProcedure(ResolvedEl.IdentEl);
      if rcConstant in Flags then
        RaiseConstantExprExp(20170216152637,Params);
      if Proc.ProcType is TPasFunctionType then
        // function call => return result
        ComputeResultElement(TPasFunctionType(Proc.ProcType).ResultEl,ResolvedEl,
          Flags+[rcCall],StartEl)
      else if Proc.IsAsync and ComputeProcAsyncResult(Proc,ResolvedEl,Flags,StartEl) then
        // async proc => return promise
      else if (Proc.ClassType=TPasConstructor) then
        begin
        // constructor -> return value of type class
        ResolvedEl:=GetReference_ConstructorType(Ref,Params.Value);
        end
      else
        // procedure call, result is neither readable nor writable
        SetResolverIdentifier(ResolvedEl,btProc,Proc,Proc.ProcType,Proc.ProcType,[]);
      Include(ResolvedEl.Flags,rrfCanBeStatement);
      end
    else if ResolvedEl.LoTypeEl is TPasProcedureType then
      begin
      if Params.Value is TParamsExpr then
        begin
        // e.g. Name()() or Name[]()
        Include(ResolvedEl.Flags,rrfReadable);
        end;
      if rrfReadable in ResolvedEl.Flags then
        begin
        // call procvar
        if rcConstant in Flags then
          RaiseConstantExprExp(20170216152639,Params);
        if ResolvedEl.LoTypeEl is TPasFunctionType then
          // function call => return result
          ComputeResultElement(TPasFunctionType(ResolvedEl.LoTypeEl).ResultEl,
            ResolvedEl,Flags+[rcCall],StartEl)
        else if (ResolvedEl.LoTypeEl is TPasProcedureType)
            and TPasProcedureType(ResolvedEl.LoTypeEl).IsAsync
            and ComputeProcAsyncResult(ResolvedEl.LoTypeEl,ResolvedEl,Flags,StartEl) then
          // async proc => return promise
        else
          // procedure call, result is neither readable nor writable
          SetResolverTypeExpr(ResolvedEl,btProc,
            ResolvedEl.LoTypeEl,ResolvedEl.HiTypeEl,[]);
        Include(ResolvedEl.Flags,rrfCanBeStatement);
        end
      else
        begin
        // typecast to proctype
        if length(Params.Params)<>1 then
          begin
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ComputeFuncParams DeclEl=',GetObjName(DeclEl),' ',GetResolverResultDbg(ResolvedEl));
          {$ENDIF}
          RaiseMsg(20170416185211,nWrongNumberOfParametersForTypeCast,
            sWrongNumberOfParametersForTypeCast,[ResolvedEl.LoTypeEl.Name],Params);
          end;
        Param0:=Params.Params[0];
        ComputeElement(Param0,ParamResolved,[]);
        ComputeTypeCast(ResolvedEl.LoTypeEl,ResolvedEl.HiTypeEl,Param0,
                        ParamResolved,ResolvedEl,Flags);
        end;
      end
    else if (DeclEl is TPasType) then
      begin
      // type cast
      Param0:=Params.Params[0];
      ComputeElement(Param0,ParamResolved,Flags);
      // A set type resolves with LoTypeEl = its ELEMENT type, so casting to it
      // (e.g. TByteSet(intval)) would yield the element, not the set. Pass the
      // set type itself so the cast result is the set.
      if ResolveAliasType(TPasType(DeclEl)) is TPasSetType then
        ComputeTypeCast(ResolveAliasType(TPasType(DeclEl)),
                        ResolveAliasType(TPasType(DeclEl)),
                        Param0,ParamResolved,ResolvedEl,Flags)
      else
        ComputeTypeCast(ResolvedEl.LoTypeEl,ResolvedEl.HiTypeEl,Param0,
                        ParamResolved,ResolvedEl,Flags);
      end
    else
      begin
      // funcref-derived interface variable call (subclass synthesizes $FuncRef$)
      InvokeProcType:=GetFuncRefInvokeProcType(ResolvedEl.LoTypeEl,length(Params.Params));
      if (InvokeProcType<>nil) and (rrfReadable in ResolvedEl.Flags) then
        begin
        if rcConstant in Flags then
          RaiseConstantExprExp(20260319173500,Params);
        if InvokeProcType is TPasFunctionType then
          ComputeResultElement(TPasFunctionType(InvokeProcType).ResultEl,
            ResolvedEl,Flags+[rcCall],StartEl)
        else
          SetResolverTypeExpr(ResolvedEl,btProc,
            InvokeProcType,InvokeProcType,[]);
        Include(ResolvedEl.Flags,rrfCanBeStatement);
        end
      else
        RaiseNotYetImplemented(20160928180048,Params,GetResolverResultDbg(ResolvedEl));
      end;
    end;
end;

procedure TPasResolver.ComputeTypeCast(ToLoType, ToHiType: TPasType;
  Param: TPasExpr; const ParamResolved: TPasResolverResult; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);

  function ParamIsVar: boolean;
  var
    IdentEl: TPasElement;
  begin
    IdentEl:=ParamResolved.IdentEl;
    if IdentEl=nil then
      begin
      // A cast whose operand is an l-value without a named identifier — a pointer
      // dereference (p^) or a pointer/array index (p[i]) — is still a var context,
      // so the cast stays a writable l-value. ex:  cardinal(pointer(x)^):=n ;
      Result:= ((ParamResolved.ExprEl is TUnaryExpr)
                  and (TUnaryExpr(ParamResolved.ExprEl).OpCode=eopDeref))
            or ((ParamResolved.ExprEl is TParamsExpr)
                  and (TParamsExpr(ParamResolved.ExprEl).Kind=pekArrayParams));
      exit;
      end;
    if [rcConstant,rcType]*Flags<>[] then
      Result:=(IdentEl.ClassType=TPasConst) and (TPasConst(IdentEl).IsConst)
    else
      Result:=(IdentEl is TPasVariable)
           or (IdentEl.ClassType=TPasArgument)
           or (IdentEl.ClassType=TPasResultElement);
  end;

  function OrdinalBitSize(B: TResolverBaseType; const R: TPasResolverResult;
    out ABits: word): boolean;
  // Width of an ordinal, so a variable typecast can be checked for the
  // same-size rule. Anything that is not a plain ordinal answers False.
  var
    Signed: boolean;
  begin
    Result:=false;
    ABits:=0;
    if B=btRange then
      B:=R.SubType;
    if B in btAllInteger then
      begin
      GetIntegerProps(B,ABits,Signed);
      Exit(ABits>0);
      end;
    case B of
    btChar{$ifdef FPC_HAS_CPSTRING},btAnsiChar{$endif},
    btBoolean,btByteBool: ABits:=8;
    btWideChar,btWordBool: ABits:=16;
    btLongBool: ABits:=32;
    {$ifdef HasInt64}
    btQWordBool: ABits:=64;
    {$endif}
    btContext:
      if R.LoTypeEl is TPasEnumType then
        ABits:=32
      else
        Exit(false);
    else
      Exit(false);
    end;
    Result:=true;
  end;

  function SameOrdinalSize(ToBT: TResolverBaseType;
    const FromRes: TPasResolverResult): boolean;
  var
    ToBits, FromBits: word;
    ToRes: TPasResolverResult;
  begin
    ToRes:=Default(TPasResolverResult);
    ToRes.BaseType:=ToBT;
    Result:=OrdinalBitSize(ToBT,ToRes,ToBits)
        and OrdinalBitSize(FromRes.BaseType,FromRes,FromBits)
        and (ToBits=FromBits);
  end;

var
  WriteFlags: TPasResolverResultFlags;
  KeepWriteFlags: Boolean;
  bt: TResolverBaseType;
  Expr: TPasExpr;
  OperandIsLValueExpr: Boolean;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeFuncParams START ToLoType=',GetObjName(ToLoType),' ',BoolToStr(ToLoType<>ToHiType,'ToHiType='+GetObjName(ToHiType),''),' ',GetResolverResultDbg(ParamResolved));
  {$ENDIF}
  if ToLoType.CustomData is TResElDataBaseType then
    begin
    // type cast to base type (or alias of base type)
    bt:=GetActualBaseType(TResElDataBaseType(ToLoType.CustomData).BaseType);
    SetResolverValueExpr(ResolvedEl,
      TResElDataBaseType(ToLoType.CustomData).BaseType,
      ToLoType,ToHiType,
      Param,[rrfReadable]);
    ResolvedEl.IdentEl:=ParamResolved.IdentEl;

    WriteFlags:=ParamResolved.Flags*[rrfWritable,rrfAssignable];
    // A dereference/index l-value operand (p^, p[i]) is writable by nature even
    // though the untyped-Pointer deref has no rrfWritable flag; give the cast
    // its writability so cardinal(pointer(x)^):=n stays an l-value.
    OperandIsLValueExpr:=((ParamResolved.ExprEl is TUnaryExpr)
                            and (TUnaryExpr(ParamResolved.ExprEl).OpCode=eopDeref))
                      or ((ParamResolved.ExprEl is TParamsExpr)
                            and (TParamsExpr(ParamResolved.ExprEl).Kind=pekArrayParams));
    if (WriteFlags=[]) and OperandIsLValueExpr then
      WriteFlags:=[rrfWritable];
    if (WriteFlags<>[]) and (ParamIsVar or OperandIsLValueExpr) then
      begin
      KeepWriteFlags:=false;
      // Param is writable -> check if typecast keeps this
      if (bt=btPointer) then
        begin
        // typecast to pointer
        if (ParamResolved.BaseType=btPointer)
        or (ParamResolved.BaseType in [btString,btUnicodeString,btWideString])
        or (ParamResolved.LoTypeEl=nil) // untyped
        or (ParamResolved.LoTypeEl.ClassType=TPasClassType)
        or (ParamResolved.LoTypeEl is TPasProcedureType) // Pointer(ProcVar):= — the unix
                                                          // sigaction idiom
                                                          // pointer(act.sa_handler):=...
        or (ParamResolved.LoTypeEl is TPasPointerType)   // Pointer(TypedPtrVar):=
        or IsDynArray(ParamResolved.LoTypeEl)
        // Pointer(TemplateVar) inside a generic body: what T turns out to be is
        // only known at specialization, and FPC accepts it there. syncobjs'
        // generic TInterlocked.Exchange<T> passes Pointer(Target) as a var arg.
        or (ParamResolved.LoTypeEl is TPasGenericTemplateType)
        // A pointer-sized RECORD or static ARRAY is one machine word too, so
        // reinterpreting it as a Pointer addresses the same slot and stays an
        // l-value. The cast-to-pointer arm short-circuits the whole chain
        // below, so the general same-size rule has to be asked here as well
        // (rtl-extra's ipc.pp, `pointer(arg)` on a variant record).
        or TypeCastKeepsLValue(ToLoType,ParamResolved)
        then
          // e.g. pointer(ObjVar)
          KeepWriteFlags:=true;
        end
      else if (ParamResolved.LoTypeEl=nil) then
        // untyped var reinterpreted as a base type as a writable l-value, e.g.
        // Currency(untypedOut):=.. in sysutils' InternalTextToFloat, where Value
        // is an "out" untyped parameter (mirrors the custom-type branch below).
        KeepWriteFlags:=true
      else if IsSameType(ToLoType,ParamResolved.LoTypeEl,prraNone) then
        // e.g. Byte(TAliasByte)
        KeepWriteFlags:=true
      else if OperandIsLValueExpr then
        // a pointer-deref/index l-value reinterpreted as a base type, e.g.
        // cardinal(pointer(x)^):=n in the RTL's StrAlloc.
        KeepWriteFlags:=true
      else if SameOrdinalSize(bt,ParamResolved) then
        // A VARIABLE TYPECAST between two ordinals of the same size stays an
        // l-value, exactly as in FPC and Delphi: Inc(word(WideCharVar),32) in
        // fcl-xml's WStrLower, Inc(ShortInt(ByteVar)). Without this the cast
        // came back read-only and Inc rejected its own argument, blaming the
        // CALLER's line.
        KeepWriteFlags:=true
      else if TypeCastKeepsLValue(ToLoType,ParamResolved) then
        // Same rule, for the types SameOrdinalSize cannot measure on its own:
        // a subrange (system.uiconsts' TColor) or a float (syncobjs' Single).
        KeepWriteFlags:=true;
      if KeepWriteFlags then
        ResolvedEl.Flags:=ResolvedEl.Flags+WriteFlags;
      end;
    end
  else if ToLoType is TPasProcedureType then
    begin
    // typecast to proctype
    if ParamIsVar then
      WriteFlags:=ParamResolved.Flags*[rrfWritable,rrfAssignable]
    else
      WriteFlags:=[];
    SetResolverValueExpr(ResolvedEl,btContext,
      ToLoType,ToHiType,
      Param,[rrfReadable]+WriteFlags);
    ResolvedEl.IdentEl:=ParamResolved.IdentEl;
    end
  else
    begin
    // typecast to custom type, e.g. to classtype, recordtype, arraytype, range, set
    if (Param.Parent is TParamsExpr) then
      Expr:=TParamsExpr(Param.Parent)
    else
      Expr:=Param;
    ComputeElement(ToHiType,ResolvedEl,Flags,Expr);
    ResolvedEl.ExprEl:=Expr;
    ResolvedEl.IdentEl:=ParamResolved.IdentEl;
    ResolvedEl.Flags:=[rrfReadable];

    WriteFlags:=ParamResolved.Flags*[rrfWritable,rrfAssignable];
    // Same rescue as the base-type branch above: a dereference/index operand
    // (p^, p[i]) is writable by nature although an untyped-Pointer deref has
    // no rrfWritable flag of its own.
    OperandIsLValueExpr:=((ParamResolved.ExprEl is TUnaryExpr)
                            and (TUnaryExpr(ParamResolved.ExprEl).OpCode=eopDeref))
                      or ((ParamResolved.ExprEl is TParamsExpr)
                            and (TParamsExpr(ParamResolved.ExprEl).Kind=pekArrayParams));
    if (WriteFlags=[]) and OperandIsLValueExpr then
      WriteFlags:=[rrfWritable];
    if (WriteFlags<>[]) and (ParamIsVar or OperandIsLValueExpr) then
      begin
      KeepWriteFlags:=false;
      if (rrfReadable in ResolvedEl.Flags) then
        begin
        // typecast a value
        if ToLoType is TPasGenericTemplateType then
          // T(Dest^):= inside a generic body — what T turns out to be is only
          // known at specialization, so the cast keeps its operand's l-value
          // status, as FPC does (fgl.pp's TFPGList.CopyItem).
          KeepWriteFlags:=true
        else if ParamResolved.LoTypeEl is TPasGenericTemplateType then
          // The mirror: TObject(TemplateVar):= — what the template turns out to
          // be is decided at specialization (rtl-generics' TCustomTreeEnumerator
          // writes `TObject(FTree) := ATree`).
          KeepWriteFlags:=true
        else if (IsDynArray(ToLoType) and (ParamResolved.LoTypeEl is TPasPointerType))
            or ((ToLoType.ClassType=TPasPointerType) and IsDynArray(ParamResolved.LoTypeEl)) then
          // A typed pointer and a dynamic array hold the same machine word, so
          // reinterpreting one as the other stays an l-value (libffi's
          // ffi.manager writes `Tpffi_typeArray(Result^.elements) := elements`).
          KeepWriteFlags:=true
        else if ParamResolved.BaseType=btPointer then
          begin
          if (ToLoType.ClassType=TPasClassType)
              or (ToLoType.ClassType=TPasPointerType)
              or IsDynArray(ParamResolved.LoTypeEl)
              // The MIRROR of the typed-pointer rule above: an untyped Pointer
              // reinterpreted as a dynamic array is the same machine word, so it
              // stays an l-value - odata's JSONArrayToDynArray does
              // `SetLength(TBooleanArray(Result), L)` on a Pointer result.
              or IsDynArray(ToLoType) then
            // aClassType(aPointer) or aPointerType(aPointer)
            KeepWriteFlags:=true;
          end
        else if ParamResolved.LoTypeEl=nil then
          // e.g. TAliasType(untyped)
          KeepWriteFlags:=true
        else if ToLoType=ParamResolved.LoTypeEl then
          // e.g. TAliasType(ActualType)
          KeepWriteFlags:=true
        else if (ToLoType.ClassType=TPasClassType)
            and (ParamResolved.LoTypeEl.ClassType=TPasClassType) then
          begin
          // e.g. aClassType(ObjVar)
          if (TPasClassType(ToLoType).ObjKind<>TPasClassType(ParamResolved.LoTypeEl).ObjKind) then
            // e.g. IntfType(ObjVar)
          else
            KeepWriteFlags:=true;
          end
        else if (ToLoType.ClassType=TPasPointerType)
            and (ParamResolved.LoTypeEl.ClassType=TPasClassType) then
          // PAnsiChar(ObjVar): a class reference IS a pointer, so casting it to
          // a DECLARED pointer type stays an l-value - exactly as the untyped
          // Pointer(ObjVar) does in the base-type branch above, which is the
          // only reason that one worked. fcl-xml's dom.pp walks its node pool
          // with Dec(PAnsiChar(FCurrBlock), FElementSize).
          KeepWriteFlags:=true
        else if (ToLoType.ClassType=TPasClassType)
            and (ParamResolved.LoTypeEl.ClassType=TPasPointerType) then
          // ...and the mirror, TSomeClass(TypedPtrVar):= - lazarus' codetools
          // keeps a free list in a typed pointer field this way.
          KeepWriteFlags:=true
        else if (ToLoType.ClassType=TPasClassType)
            and (ParamResolved.BaseType in btAllInteger)
            and TypeCastKeepsLValue(ToLoType,ParamResolved) then
          // a pointer-sized integer seen as a class instance, same machine word:
          // the LCL's FreeThenNil(TLMGtkPaintData(Msg^.WParam))
          KeepWriteFlags:=true
        else if (ToLoType.ClassType=TPasRecordType)
            and (ParamResolved.LoTypeEl.ClassType=TPasRecordType) then
          // typecast record
          KeepWriteFlags:=true
        else if (ToLoType.ClassType=TPasRecordType)
            and (ParamResolved.BaseType in [btFile,btText]) then
          // typecast a file variable to its control record: TextRec(F):=... in the
          // unix RTL. A file IS its record under the hood, so the cast stays an
          // l-value (mirrors the record->record case above).
          KeepWriteFlags:=true
        else if (ToLoType.ClassType=TPasRecordType)
            and (ParamResolved.BaseType in (btAllInteger+btAllChars+btAllBooleans+btAllFloats)) then
          // typecast a scalar variable to a (same-size) record as a writable
          // l-value — sysutils' bit/nibble overlays write through it, e.g.
          // TByteOverlay(aByte).AsNibble[i]:=v (PutNibble), and the float overlays
          // TDoubleRec(aDouble).Frac:=..
          KeepWriteFlags:=true
        else if (ToLoType.ClassType=TPasRecordType)
            and ((ParamResolved.LoTypeEl is TPasEnumType)
                 or (ParamResolved.LoTypeEl is TPasRangeType))
            and TypeCastKeepsLValue(ToLoType,ParamResolved) then
          // The mirror of the scalar case above for an ordinal that resolves as
          // btContext: cgbase.pas writes `TRegisterRec(result).regtype := rt`
          // over a TRegister enum spanning the whole LongInt range. The size
          // test keeps the view honest.
          KeepWriteFlags:=true
        else if (ToLoType.ClassType=TPasRecordType)
            and (ParamResolved.BaseType=btVariant) then
          // typecast a Variant to a (same-layout) record as a writable l-value —
          // a Variant IS a TVarData under the hood, so the variants unit's helpers
          // write through it: `TVarData(v).VType := varEmpty` (sysvarinit et al.).
          KeepWriteFlags:=true
        else if (ToLoType.ClassType=TPasArrayType)
            and (ParamResolved.LoTypeEl.ClassType=TPasArrayType)
            and IsDynArray(ToLoType)
            and IsDynArray(ParamResolved.LoTypeEl) then
          // typecast dyn array to dyn array
          KeepWriteFlags:=true
        else if (ToLoType.ClassType=TPasPointerType)
            and (ParamResolved.LoTypeEl is TPasPointerType) then
          // typecast typed pointer to typed pointer, e.g. PByte(PIntegerVar)
          KeepWriteFlags:=true
        else if (ResolvedEl.BaseType=btPointer)
            and ((ParamResolved.LoTypeEl is TPasProcedureType)
              or (ParamResolved.LoTypeEl is TPasClassType)
              or (ParamResolved.LoTypeEl is TPasPointerType)) then
          // typecast a pointer-sized reference variable (procvar/class/typed
          // pointer) to the untyped Pointer as an l-value — reinterprets the
          // variable's storage. FPC allows this (unix RTL: the
          // `pointer(act.sa_handler):=pointer(SIG_DFL)` sigaction idiom).
          KeepWriteFlags:=true;
        // The same rescue the base-type branch above ends with, and it must be
        // its own test rather than another arm of the chain: an earlier arm
        // (e.g. the btPointer one) matches the operand and leaves
        // KeepWriteFlags false, so an appended `else if` would never run.
        // A dereference or index operand (p^, p[i]) NAMES a memory location, so
        // reading it as another type stays a writable l-value whatever that
        // type is - fcl-db's TDataSet.DataConvert writes
        // `TDateTimeRec(aDest^) := ..` through an untyped Pointer parameter.
        if (not KeepWriteFlags) and OperandIsLValueExpr then
          KeepWriteFlags:=true;
        end
      else
        begin
        // typecast a type to a value, e.g. Pointer(TObject)
        end;
      if KeepWriteFlags then
        ResolvedEl.Flags:=ResolvedEl.Flags+WriteFlags;
      end;
    end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeFuncParams END ToLoType=',GetObjName(ToLoType),' ',BoolToStr(ToLoType<>ToHiType,'ToHiType='+GetObjName(ToHiType),''),' ',GetResolverResultDbg(ParamResolved),' Result=',GetResolverResultDbg(ResolvedEl));
  {$ENDIF}
end;

procedure TPasResolver.ComputeSetParams(Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
// [param,param,...]
var
  ParamResolved, FirstResolved: TPasResolverResult;
  i: Integer;
  Param: TPasExpr;
  IsRange, IsArray: Boolean;
  ArrayType: TPasArrayType;
begin
  ArrayType:=IsArrayExpr(Params);
  IsArray:=ArrayType<>nil;
  if length(Params.Params)=0 then
    begin
    SetResolverValueExpr(ResolvedEl,btArrayOrSet,nil,nil,Params,[rrfReadable]);
    if IsArray then
      ResolvedEl.BaseType:=btArrayLit;
    exit;
    end;
  FirstResolved:=Default(TPasResolverResult);
  Flags:=Flags-[rcNoImplicitProc]+[rcNoImplicitProcType];
  for i:=0 to length(Params.Params)-1 do
    begin
    Param:=Params.Params[i];
    ComputeElement(Params.Params[0],ParamResolved,Flags,StartEl);
    IsRange:=ParamResolved.BaseType=btRange;
    if IsRange then
      begin
      if IsArray then
        RaiseXExpectedButYFound(20180615111713,'array value','range expression',Param);
      ConvertRangeToElement(ParamResolved);
      end;
    if FirstResolved.BaseType=btNone then
      begin
      // first value -> check if type usable in a set/array
      FirstResolved:=ParamResolved;
      if IsRange then
        CheckIsOrdinal(FirstResolved,Param,true);
      if rrfReadable in FirstResolved.Flags then
        begin
        // has a value
        if (not IsArray) and (not IsRange)
            and (not CheckIsOrdinal(FirstResolved,Param,false)) then
          begin
          // can't be a set
          IsArray:=true;
          end;
        end
      else
        begin
        IsArray:=true;
        if (FirstResolved.BaseType=btContext) then
          begin
          if FirstResolved.IdentEl is TPasClassType then
            // array of classtypes
          else
            begin
            {$IFDEF VerbosePasResolver}
            writeln('TPasResolver.ComputeSetParams ',GetResolverResultDbg(FirstResolved));
            {$ENDIF}
            RaiseXExpectedButYFound(20170420002328,'array value','type',Param);
            end;
          end
        else
          begin
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ComputeSetParams ',GetResolverResultDbg(FirstResolved));
          {$ENDIF}
          RaiseXExpectedButYFound(20170420002332,'array value','type',Param);
          end;
        end;
      end
    else
      begin
      // next value
      CombineArrayLitElTypes(Params.Params[0],Param,FirstResolved,ParamResolved);
      end;
    end;

  FirstResolved.IdentEl:=nil;
  FirstResolved.ExprEl:=Params;
  FirstResolved.SubType:=FirstResolved.BaseType;
  if IsArray then
    FirstResolved.BaseType:=btArrayLit
  else
    FirstResolved.BaseType:=btArrayOrSet;
  FirstResolved.Flags:=[rrfReadable];
  ResolvedEl:=FirstResolved;
end;

procedure TPasResolver.ComputeDereference(El: TUnaryExpr;
  var ResolvedEl: TPasResolverResult);

  procedure Deref(TypeEl: TPasType);
  begin
    ComputeElement(TypeEl,ResolvedEl,[rcNoImplicitProc],El);
    ResolvedEl.IdentEl:=nil;
    ResolvedEl.ExprEl:=El; // El = TUnaryExpr(eopDeref), needed by CheckCanBeLHS
    ResolvedEl.Flags:=ResolvedEl.Flags+[rrfReadable,rrfWritable];
  end;

var
  TypeEl: TPasType;
begin
  if ResolvedEl.BaseType=btPointer then
    begin
    Deref(ResolvedEl.LoTypeEl);
    exit;
    end
  else if ResolvedEl.BaseType=btContext then
    begin
    TypeEl:=ResolvedEl.LoTypeEl;
    if TypeEl.ClassType=TPasPointerType then
      begin
      Deref(TPasPointerType(TypeEl).DestType);
      exit;
      end;
    if TypeEl.ClassType=TPasGenericTemplateType then
      begin
      // the pointee is unknown until specialization, so stay deferred
      ResolvedEl.ExprEl:=El;
      ResolvedEl.Flags:=ResolvedEl.Flags+[rrfReadable,rrfWritable];
      exit;
      end;
    end
  else if (ResolvedEl.BaseType=btProc) and (ResolvedEl.IdentEl is TPasFunction)
      and (TPasFunction(ResolvedEl.IdentEl).ProcType.Args.Count=0) then
    begin
    // func^ : a parameterless function is implicitly called, then its pointer
    // result is dereferenced (GPC/ISO), e.g. `ip1^` where ip1 returns ^Integer.
    TypeEl:=ResolveAliasType(TPasFunction(ResolvedEl.IdentEl).FuncType.ResultEl.ResultType);
    if TypeEl is TPasPointerType then
      begin
      Deref(TPasPointerType(TypeEl).DestType);
      exit;
      end;
    end
  {$IFDEF PAS2JS}  
  ;
  {$ELSE}
  else if ResolvedEl.BaseType in [btFile,btText] then
    begin
    // ISO/Standard-Pascal file buffer variable: f^ is the file's component
    // type — the element type for `file of T`, or Char for a text file.
    TypeEl:=ResolvedEl.LoTypeEl;
    if (ResolvedEl.BaseType=btFile) and (TypeEl is TPasFileType)
        and (TPasFileType(TypeEl).ElType<>nil) then
      Deref(TPasFileType(TypeEl).ElType)
    else
      begin
      SetResolverIdentifier(ResolvedEl,btChar,nil,
        FBaseTypes[btChar],FBaseTypes[btChar],[rrfReadable,rrfWritable]);
      ResolvedEl.ExprEl:=El; // El = TUnaryExpr(eopDeref), needed by CheckCanBeLHS
      end;
    exit;
    end;
  {$ENDIF}
  if (ResolvedEl.BaseType=btNil) and AllowNilDereference then
    begin
    // `nil^` is an untyped location at address nil - the idiom for handing an
    // untyped var parameter "no buffer at all".
    SetResolverIdentifier(ResolvedEl,btUntyped,nil,nil,nil,[rrfReadable,rrfWritable]);
    ResolvedEl.ExprEl:=El;
    exit;
    end;
  RaiseMsg(20180422191139,nIllegalQualifierInFrontOf,sIllegalQualifierInFrontOf,
    [OpcodeStrings[eopDeref],GetResolverResultDescription(ResolvedEl)],El);
end;

procedure TPasResolver.ComputeArrayValuesExpectedType(El: TArrayValues; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
// (expr, expr, ...)
var
  Parent: TPasElement;
  HiTypeEl, LoTypeEl: TPasType;
  Field: PRecordValuesItem;
  Ref: TResolvedReference;
  Member: TPasVariable;
  i: Integer;
  ArrType: TPasArrayType;
begin
  Parent:=El.Parent;
  if Parent is TPasVariable then
    begin
    HiTypeEl:=TPasVariable(Parent).VarType;
    if HiTypeEl=nil then
      RaiseMsg(20180429171628,nSyntaxErrorExpectedButFound,sSyntaxErrorExpectedButFound,
        ['const','array values'],El);
    LoTypeEl:=ResolveAliasType(HiTypeEl);
    if LoTypeEl.ClassType=TPasArrayType then
      // ok
    else
      RaiseIncompatibleTypeDesc(20180429171714,nIncompatibleTypesGotExpected,
        [],'array value',GetTypeDescription(HiTypeEl),El);
    SetResolverValueExpr(ResolvedEl,btContext,LoTypeEl,HiTypeEl,
       El,[rrfReadable]);
    end
  else if Parent.ClassType=TRecordValues then
    begin
    // record field array
    // get field
    i:=length(TRecordValues(Parent).Fields)-1;
    while (i>=0) and (TRecordValues(Parent).Fields[i].ValueExp<>El) do
      dec(i);
    if i<0 then
      RaiseInternalError(20180429181150);
    Field:=@TRecordValues(Parent).Fields[i];
    // get member
    Ref:=Field^.NameExp.CustomData as TResolvedReference;
    Member:=Ref.Declaration as TPasVariable;
    if Member=nil then
      RaiseInternalError(20180429181210);
    ComputeElement(Member,ResolvedEl,[],StartEl);
    ResolvedEl.Flags:=[rrfReadable];
    end
  else if Parent.ClassType=TArrayValues then
    begin
    // array of array
    ComputeArrayValuesExpectedType(TArrayValues(Parent),ResolvedEl,Flags,StartEl);
    if (ResolvedEl.BaseType=btContext)
        and (ResolvedEl.LoTypeEl.ClassType=TPasArrayType) then
      begin
      ArrType:=TPasArrayType(ResolvedEl.LoTypeEl);
      if length(ArrType.Ranges)>1 then
        begin
        // `array[A, B] of T`: the inner (...) is an array[B] of T (the LCL's
        // CalcBtnPos: array[TCalculatorLayout, TCalcBtnKind] of TPoint)
        HiTypeEl:=GetRemainingRangesArray(ArrType);
        SetResolverValueExpr(ResolvedEl,btContext,HiTypeEl,HiTypeEl,El,[rrfReadable]);
        exit;
        end;
      HiTypeEl:=ArrType.ElType;
      LoTypeEl:=ResolveAliasType(HiTypeEl);
      if LoTypeEl.ClassType<>TPasArrayType then
        RaiseIncompatibleTypeDesc(20180429180938,nIncompatibleTypesGotExpected,
          [],'array values',GetTypeDescription(HiTypeEl),El);
      SetResolverValueExpr(ResolvedEl,btContext,LoTypeEl,HiTypeEl,
         El,[rrfReadable]);
      end
    else
      RaiseIncompatibleTypeDesc(20180429173143,nIncompatibleTypesGotExpected,
        [],'array values',GetTypeDescription(ResolvedEl),El);
    end
  else
    SetResolverValueExpr(ResolvedEl,btArrayLit,nil,nil,El,[rrfReadable]);
end;

procedure TPasResolver.ComputeRecordValues(El: TRecordValues; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
// (name:expr; name:expr; ...)
var
  Parent, Member: TPasElement;
  LoTypeEl, HiTypeEl: TPasType;
  i: Integer;
  Field: PRecordValuesItem;
  Ref: TResolvedReference;
  ArrType: TPasArrayType;
begin
  Parent:=El.Parent;
  if Parent is TPasVariable then
    begin
    HiTypeEl:=TPasVariable(Parent).VarType;
    if HiTypeEl=nil then
      RaiseMsg(20180429105451,nSyntaxErrorExpectedButFound,sSyntaxErrorExpectedButFound,
        ['const','record values'],El);
    LoTypeEl:=ResolveAliasType(HiTypeEl);
    if GetRecordValuesMembersType(LoTypeEl)=nil then
      RaiseIncompatibleTypeDesc(20180429104135,nIncompatibleTypesGotExpected,
        [],'record value',GetTypeDescription(HiTypeEl),El);
    SetResolverValueExpr(ResolvedEl,btContext,LoTypeEl,HiTypeEl,
       El,[rrfReadable]);
    end
  else if Parent.ClassType=TRecordValues then
    begin
    // nested record
    // get field
    i:=length(TRecordValues(Parent).Fields)-1;
    while (i>=0) and (TRecordValues(Parent).Fields[i].ValueExp<>El) do
      dec(i);
    if i<0 then
      RaiseInternalError(20180429130244);
    Field:=@TRecordValues(Parent).Fields[i];
    // get member
    Ref:=Field^.NameExp.CustomData as TResolvedReference;
    Member:=Ref.Declaration as TPasVariable;
    if Member=nil then
      RaiseInternalError(20180429130548);
    ComputeElement(Member,ResolvedEl,[],StartEl);
    ResolvedEl.Flags:=[rrfReadable];
    end
  else if Parent.ClassType=TArrayValues then
    begin
    // array of record
    ComputeArrayValuesExpectedType(TArrayValues(Parent),ResolvedEl,Flags,StartEl);
    if (ResolvedEl.BaseType=btContext)
        and (ResolvedEl.LoTypeEl.ClassType=TPasArrayType) then
      begin
      ArrType:=TPasArrayType(ResolvedEl.LoTypeEl);
      if length(ArrType.Ranges)>1 then
        RaiseNotYetImplemented(20180429180450,El);
      HiTypeEl:=ArrType.ElType;
      LoTypeEl:=ResolveAliasType(HiTypeEl);
      if LoTypeEl.ClassType<>TPasRecordType then
        RaiseIncompatibleTypeDesc(20180429180642,nIncompatibleTypesGotExpected,
          [],'record values',GetTypeDescription(HiTypeEl),El);
      SetResolverValueExpr(ResolvedEl,btContext,LoTypeEl,HiTypeEl,
         El,[rrfReadable]);
      end
    else
      RaiseIncompatibleTypeDesc(20180429173143,nIncompatibleTypesGotExpected,
        [],'array values',GetTypeDescription(ResolvedEl),El);
    end
  else
    RaiseMsg(20180429110227,nSyntaxErrorExpectedButFound,sSyntaxErrorExpectedButFound,
     ['const','(name:'],El);
end;
