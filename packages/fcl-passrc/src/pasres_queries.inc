{%MainUnit pasresolver.pp}
{ **********************************************************************

    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - Query all kinds of things
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }

procedure TPasResolver.ComputeElement(El: TPasElement; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);

  procedure ComputeIdentifier(Expr: TPasExpr);
  var
    Ref: TResolvedReference;
    Proc: TPasProcedure;
    ProcType: TPasProcedureType;
  begin
    Ref:=TResolvedReference(Expr.CustomData);
    ComputeElement(Ref.Declaration,ResolvedEl,Flags+[rcNoImplicitProc],StartEl);
    if rrfConstInherited in Ref.Flags then
      Exclude(ResolvedEl.Flags,rrfWritable);
    {$IFDEF VerbosePasResolver}
    {AllowWriteln}
    if Expr is TPrimitiveExpr then
      writeln('TPasResolver.ComputeElement.ComputeIdentifier TPrimitiveExpr "',TPrimitiveExpr(Expr).Value,'" ',GetResolverResultDbg(ResolvedEl),' Flags=',dbgs(Flags))
    else
      writeln('TPasResolver.ComputeElement.ComputeIdentifier "',GetObjName(Expr),'" ',GetResolverResultDbg(ResolvedEl),' Flags=',dbgs(Flags));
    {AllowWriteln-}
    {$ENDIF}
    //if (Expr is TPrimitiveExpr) and (Expr.Parent is TParamsExpr) and (TPrimitiveExpr(Expr).Value='FA') then
    //  RaiseNotYetImplemented(20180621235200,Expr);

    if not (rcSetReferenceFlags in Flags)
        and (rrfNoImplicitCallWithoutParams in Ref.Flags) then
      exit;

    if (ResolvedEl.BaseType=btProc) then
      begin
      // proc
      if rcNoImplicitProc in Flags then
        begin
        if rcSetReferenceFlags in Flags then
          Include(Ref.Flags,rrfNoImplicitCallWithoutParams);
        end
      else if [rcConstant,rcType]*Flags=[] then
        begin
        // implicit call without params is allowed -> check if possible
        Proc:=ResolvedEl.IdentEl as TPasProcedure;
        if not ProcNeedsParams(Proc.ProcType) then
          begin
          // parameter less proc -> implicit call possible
          if ResolvedEl.IdentEl is TPasFunction then
            begin
            // function => return result
            ComputeResultElement(TPasFunction(ResolvedEl.IdentEl).FuncType.ResultEl,
              ResolvedEl,Flags+[rcCall],StartEl);
            end
          else if (ResolvedEl.IdentEl is TPasProcedure)
              and TPasProcedure(ResolvedEl.IdentEl).IsAsync
              and ComputeProcAsyncResult(ResolvedEl.IdentEl,ResolvedEl,Flags,StartEl) then
            // async proc => return promise
          else if (ResolvedEl.IdentEl.ClassType=TPasConstructor) then
            begin
            // constructor -> return value of type class
            ResolvedEl:=GetReference_ConstructorType(Ref,Expr);
            end
          else if ParentNeedsExprResult(Expr) then
            begin
            // a procedure address
            exit;
            end;
          if rcSetReferenceFlags in Flags then
            begin
            Exclude(Ref.Flags,rrfNoImplicitCallWithoutParams);
            Include(Ref.Flags,rrfImplicitCallWithoutParams);
            end;
          Include(ResolvedEl.Flags,rrfCanBeStatement);
          end;
        end;
      end
    else if IsProcedureType(ResolvedEl,true) then
      begin
      // proc type
      if [rcNoImplicitProc,rcNoImplicitProcType]*Flags<>[] then
        begin
        if rcSetReferenceFlags in Flags then
          begin
          // A first look that allowed the call (the operator-overload probe of
          // `FMax <> nil`) may have set the opposite flag: this one decides.
          Exclude(Ref.Flags,rrfImplicitCallWithoutParams);
          Include(Ref.Flags,rrfNoImplicitCallWithoutParams);
          end;
        end
      else if [rcConstant,rcType]*Flags=[] then
        begin
        // implicit call without params is allowed -> check if possible
        ProcType:=TPasProcedureType(ResolvedEl.LoTypeEl);
        if not ProcNeedsParams(ProcType) then
          begin
          // parameter less proc type -> implicit call possible
          if ResolvedEl.LoTypeEl is TPasFunctionType then
            // function => return result
            ComputeResultElement(TPasFunctionType(ResolvedEl.LoTypeEl).ResultEl,
              ResolvedEl,Flags+[rcCall],StartEl)
          else if (ResolvedEl.LoTypeEl is TPasProcedureType)
              and TPasProcedureType(ResolvedEl.LoTypeEl).IsAsync
              and ComputeProcAsyncResult(ResolvedEl.LoTypeEl,ResolvedEl,Flags,StartEl) then
            // async proc => return promise
          else if ParentNeedsExprResult(Expr) then
            begin
            // a procedure has no result
            exit;
            end;
          if rcSetReferenceFlags in Flags then
            begin
            Exclude(Ref.Flags,rrfNoImplicitCallWithoutParams);
            Include(Ref.Flags,rrfImplicitCallWithoutParams);
            end;
          Include(ResolvedEl.Flags,rrfCanBeStatement);
          end;
        end;
      end;
  end;

  procedure ComputeInherited(Expr: TInheritedExpr);
  var
    Ref: TResolvedReference;
    Proc: TPasProcedure;
    TypeEl: TPasProcedureType;
    HasName: Boolean;
  begin
    // "inherited;"
    Ref:=TResolvedReference(El.CustomData);
    Proc:=NoNil(Ref.Declaration) as TPasProcedure;
    TypeEl:=TPasProcedure(Proc).ProcType;
    SetResolverIdentifier(ResolvedEl,btProc,Proc,
      TypeEl,TypeEl,[rrfCanBeStatement]);
    HasName:=(El.Parent.ClassType=TBinaryExpr)
       and (TBinaryExpr(El.Parent).OpCode=eopNone); // true if 'inherited Proc;'
    if HasName or (rcNoImplicitProc in Flags) then
      exit;

    // inherited;  -> implicit call possible
    if Proc is TPasFunction then
      begin
      // function => return result
      ComputeResultElement(TPasFunction(Proc).FuncType.ResultEl,
        ResolvedEl,Flags+[rcCall],StartEl);
      Exclude(ResolvedEl.Flags,rrfWritable);
      end
    else if (Proc.ClassType=TPasConstructor)
        and (rrfNewInstance in Ref.Flags) then
      begin
      // new instance constructor -> return value of type class
      ResolvedEl:=GetReference_ConstructorType(Ref,Expr);
      end
    else if ParentNeedsExprResult(Expr) then
      begin
      // a procedure
      exit;
      end;
    if rcSetReferenceFlags in Flags then
      begin
      Exclude(Ref.Flags,rrfNoImplicitCallWithoutParams);
      Include(Ref.Flags,rrfImplicitCallWithoutParams);
      end;
    Include(ResolvedEl.Flags,rrfCanBeStatement);
  end;

  procedure ComputeSpecializeType(SpecType: TPasSpecializeType);
  var
    TypeEl: TPasType;
  begin
    if SpecType.SubType<>nil then
      ComputeElement(SpecType.SubType,ResolvedEl,Flags,StartEl)
    else if SpecType.CustomData is TPasSpecializeTypeData then
      begin
      TypeEl:=TPasSpecializeTypeData(SpecType.CustomData).SpecializedType;
      if TypeEl=nil then
        RaiseNotYetImplemented(20190908153503,El);
      SetResolverIdentifier(ResolvedEl,btContext,TypeEl,TypeEl,TypeEl,[]);
      end
    else
      begin
      TypeEl:=SpecType.DestType;
      if TypeEl=nil then
        RaiseNotYetImplemented(20190908153434,El);
      SetResolverIdentifier(ResolvedEl,btContext,SpecType,TypeEl,SpecType,[]);
      end;
  end;

  procedure ComputeExportSymbol(ExpSymbol: TPasExportSymbol);
  var
    Ref: TResolvedReference;
  begin
    if ExpSymbol.CustomData is TResolvedReference then
      begin
      Ref:=TResolvedReference(El.CustomData);
      ComputeElement(Ref.Declaration,ResolvedEl,Flags+[rcNoImplicitProc],StartEl);
      end
    else if ExpSymbol.NameExpr<>nil then
      ComputeElement(ExpSymbol.NameExpr,ResolvedEl,Flags,StartEl)
    else
      RaiseNotYetImplemented(20210106225512,ExpSymbol);
  end;

var
  DeclEl: TPasElement;
  ElClass: TClass;
  bt: TResolverBaseType;
  TypeEl: TPasType;
  Value: TResEvalValue;
  Int: TMaxPrecInt;
  TemplEl: TPasType;
begin
  if StartEl=nil then StartEl:=El;
  ResolvedEl:=Default(TPasResolverResult);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeElement El=',GetObjName(El));
  {$ENDIF}
  if El=nil then
    exit;
  ElClass:=El.ClassType;
  if ElClass=TPrimitiveExpr then
    begin
    case TPrimitiveExpr(El).Kind of
      pekIdent,pekSelf:
        begin
        if not (El.CustomData is TResolvedReference) then
          begin
          // A member of a still-unbound template type (`T(Dest^)._Release` in
          // rtl/objpas/fgl.pp): ResolveSubIdent deliberately left it unresolved
          // because only specialization can say what T has. Yield the template
          // type itself so the enclosing expression stays deferred too, instead
          // of reporting the member as not-yet-implemented.
          if (El.Parent is TBinaryExpr)
              and (TBinaryExpr(El.Parent).OpCode=eopSubIdent)
              and (TBinaryExpr(El.Parent).right=El) then
            begin
            ComputeElement(TBinaryExpr(El.Parent).left,ResolvedEl,Flags,StartEl);
            // `class of T`.Member: for the check here it is a value of T
            TemplEl:=ClassOfTemplType(ResolvedEl);
            if TemplEl<>nil then
              begin
              SetResolverValueExpr(ResolvedEl,btContext,TemplEl,TemplEl,
                TPasExpr(El),[rrfReadable]);
              exit;
              end;
            // a member of a record composing a template type
            if (ResolvedEl.LoTypeEl is TPasRecordType)
                and IsRecordComposingTemplate(TPasRecordType(ResolvedEl.LoTypeEl)) then
              begin
              TemplEl:=TPasRecordScope(ResolvedEl.LoTypeEl.CustomData).ComposedTemplate;
              SetResolverValueExpr(ResolvedEl,btContext,TemplEl,TemplEl,
                TPasExpr(El),[rrfReadable,rrfWritable]);
              exit;
              end;
            // The mirror of the deferral in ResolveSubIdent: the left is a type
            // parameter, or something that still depends on one, so what this
            // name means is settled at specialization.
            if IsGenericTemplType(ResolvedEl)
                or (FPartialSpecsAsGeneric and (ResolvedEl.LoTypeEl<>nil)
                    and (ParamIsUndecided(ResolvedEl.LoTypeEl)
                      or ((ResolvedEl.HiTypeEl<>nil)
                        and ParamIsUndecided(ResolvedEl.HiTypeEl)))) then
              begin
              // Only the DECLARED type depends on a type parameter, the final
              // one is the constraint's stand-in: answer with a template type,
              // so the enclosing expression is left to the specialization too.
              if not IsGenericTemplType(ResolvedEl)
                  and not ParamIsUndecided(ResolvedEl.LoTypeEl) then
                begin
                TemplEl:=OwnTemplateTypeOf(ResolvedEl.HiTypeEl);
                if TemplEl<>nil then
                  begin
                  ResolvedEl.BaseType:=btContext;
                  ResolvedEl.LoTypeEl:=TemplEl;
                  ResolvedEl.HiTypeEl:=TemplEl;
                  end;
                end;
              ResolvedEl.IdentEl:=nil;
              ResolvedEl.ExprEl:=TPasExpr(El);
              exit;
              end;
            ResolvedEl:=Default(TPasResolverResult);
            end;
          RaiseNotYetImplemented(20160922163658,El,'Value="'+TPrimitiveExpr(El).Value+'" CustomData='+GetObjName(El.CustomData)+' '+GetElementSourcePosStr(El));
          end;
        ComputeIdentifier(TPrimitiveExpr(El));
        end;
      pekNumber:
        begin
          if NumberIsFloat(TPrimitiveExpr(El).Value) then
            bt:=BaseTypeExtended
          else if length(TPrimitiveExpr(El).Value)<9 then
            bt:=btLongint
          else
            begin
            // with 9+ it could be longword: e.g. $87654321
            Value:=Eval(TPrimitiveExpr(El),[]);
            if Value=nil then
              RaiseNotYetImplemented(20190130162601,El);
            try
              case Value.Kind of
              revkInt:
                begin
                Int:=TResEvalInt(Value).Int;
                bt:=GetSmallestIntegerBaseType(Int,Int);
                end;
              {$IFDEF HasInt64}
              revkUInt:
                bt:=btQWord;
              {$ENDIF}
              else
                bt:=BaseTypeExtended;
              end;
            finally
              ReleaseEvalValue(Value);
            end;
            end;
        SetResolverValueExpr(ResolvedEl,bt,FBaseTypes[bt],FBaseTypes[bt],
                    TPrimitiveExpr(El),[rrfReadable])
        end;
      pekString:
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ComputeElement pekString Value="',TPrimitiveExpr(El).Value,'"');
        {$ENDIF}
        bt:=IsCharLiteral(TPrimitiveExpr(El).Value,El);
        if bt in btAllChars then
          begin
          if bt=BaseTypeChar then
            bt:=btChar;
          SetResolverValueExpr(ResolvedEl,bt,FBaseTypes[bt],FBaseTypes[bt],
                               TPrimitiveExpr(El),[rrfReadable]);
          end
        else if HasWideCharCode(TPrimitiveExpr(El).Value)
            and (FBaseTypes[btUnicodeString]<>nil) then
          // A #-escape above the byte range: the evaluator makes this a UTF-16
          // constant, so the type must say so too.
          SetResolverValueExpr(ResolvedEl,btUnicodeString,
                               FBaseTypes[btUnicodeString],FBaseTypes[btUnicodeString],
                               TPrimitiveExpr(El),[rrfReadable])
        else
          SetResolverValueExpr(ResolvedEl,btString,
                               FBaseTypes[btString],FBaseTypes[btString],
                               TPrimitiveExpr(El),[rrfReadable]);
        end;
      pekStringMultiLine:
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ComputeElement pekStringMultiLine Value="',LeftStr(TPrimitiveExpr(El).Value,500),'"');
        {$ENDIF}
        SetResolverValueExpr(ResolvedEl,btString,
                             FBaseTypes[btString],FBaseTypes[btString],
                             TPrimitiveExpr(El),[rrfReadable]);
        end;
      pekNil:
        SetResolverValueExpr(ResolvedEl,btNil,FBaseTypes[btNil],FBaseTypes[btNil],
                             TPrimitiveExpr(El),[rrfReadable]);
      pekBoolConst:
        SetResolverValueExpr(ResolvedEl,btBoolean,FBaseTypes[btBoolean],FBaseTypes[btBoolean],
                             TPrimitiveExpr(El),[rrfReadable]);
    else
      RaiseNotYetImplemented(20160922163701,El);
    end;
    end
  else if ElClass=TPasUnresolvedSymbolRef then
    begin
    // built-in type
    if El.CustomData is TResElDataBaseType then
      SetResolverIdentifier(ResolvedEl,TResElDataBaseType(El.CustomData).BaseType,
        El,TPasUnresolvedSymbolRef(El),TPasUnresolvedSymbolRef(El),[])
    else if El.CustomData is TResElDataBuiltInProc then
      begin
      SetResolverIdentifier(ResolvedEl,btBuiltInProc,El,
        TPasUnresolvedSymbolRef(El),TPasUnresolvedSymbolRef(El),[]);
      if bipfCanBeStatement in TResElDataBuiltInProc(El.CustomData).Flags then
        Include(ResolvedEl.Flags,rrfCanBeStatement);
      end
    else
      RaiseNotYetImplemented(20160926194756,El);
    end
  else if ElClass=TBoolConstExpr then
    SetResolverValueExpr(ResolvedEl,btBoolean,FBaseTypes[btBoolean],FBaseTypes[btBoolean],
                         TBoolConstExpr(El),[rrfReadable])
  else if ElClass=TBinaryExpr then
    ComputeBinaryExpr(TBinaryExpr(El),ResolvedEl,Flags,StartEl)
  else if ElClass=TIfExpr then
    ComputeIfExpr(TIfExpr(El),ResolvedEl,Flags,StartEl)
  else if ElClass=TCaseExpr then
    ComputeCaseExpr(TCaseExpr(El),ResolvedEl,Flags,StartEl)
  else if ElClass=TTryExceptExpr then
    ComputeTryExceptExpr(TTryExceptExpr(El),ResolvedEl,Flags,StartEl)
  else if ElClass=TUnaryExpr then
    begin
    if TUnaryExpr(El).OpCode=eopTypeOf then
      begin
      ComputeTypeOfExpr(TUnaryExpr(El),ResolvedEl,Flags,StartEl);
      exit;
      end;
    if TUnaryExpr(El).OpCode in [eopAddress,eopMemAddress] then
      // The ADDRESS is the constant, not the operand's value: `@X` is a
      // load-time address for anything addressable, including a typed constant
      // or a global variable. Passing rcConstant down rejected the operand as
      // "Constant expression expected" - rtl-console's video.pp builds
      // `array[0..11] of Ptermcodes = (@term_codes_ansi, ...)` over typed
      // constants, which a scalar `p: PR = @r1` already accepts.
      ComputeElement(TUnaryExpr(El).Operand,ResolvedEl,
        Flags+[rcNoImplicitProc]-[rcConstant],StartEl)
    else if TUnaryExpr(El).OpCode=eopDeref then
      // `P^` needs P's VALUE, so a parameterless function there must be CALLED
      // even when the deref itself is a var/out argument - that asks for
      // rcNoImplicitProc, which used to reach P and make it a REFERENCE.
      // gmp's `mpf_init(x.ptr^)`, where ptr is an interface method.
      ComputeElement(TUnaryExpr(El).Operand,ResolvedEl,Flags-[rcNoImplicitProc],StartEl)
    else
      ComputeElement(TUnaryExpr(El).Operand,ResolvedEl,Flags,StartEl);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ComputeElement Unary Kind=',TUnaryExpr(El).Kind,' OpCode=',TUnaryExpr(El).OpCode,' OperandResolved=',GetResolverResultDbg(ResolvedEl),' ',GetElementSourcePosStr(El));
    {$ENDIF}
    case TUnaryExpr(El).OpCode of
      eopAdd:
        if ResolvedEl.BaseType in (btAllInteger+btAllFloats) then
          exit
        else if ResolvedEl.BaseType=btVariant then
          // unary + on a Variant yields a Variant (variant manager at runtime)
          exit
        else if IsGenericTemplType(ResolvedEl) then
          exit
        else if TryResolveUnaryOperator(TUnaryExpr(El), ResolvedEl, Flags) then
          exit
        else
          RaiseMsg(20170216152532,nIllegalQualifierInFrontOf,sIllegalQualifierInFrontOf,
            [OpcodeStrings[TUnaryExpr(El).OpCode],GetResolverResultDescription(ResolvedEl)],El);
      eopSubtract:
        if ResolvedEl.BaseType in (btAllSignedInteger+btAllFloats) then
          exit
        else if ResolvedEl.BaseType in btAllInteger then
          begin
          case ResolvedEl.BaseType of
          btByte,btWord:
            ResolvedEl.BaseType:=btLongint;
          btLongWord,btUIntDouble:
            ResolvedEl.BaseType:=btIntDouble;
          end;
          exit;
          end
        else if ResolvedEl.BaseType=btVariant then
          // unary - on a Variant yields a Variant (variants unit: -Variant(...))
          exit
        else if IsGenericTemplType(ResolvedEl) then
          exit
        else if TryResolveUnaryOperator(TUnaryExpr(El), ResolvedEl, Flags) then
          exit
        else
          RaiseMsg(20210815225815,nIllegalQualifierInFrontOf,sIllegalQualifierInFrontOf,
            [OpcodeStrings[TUnaryExpr(El).OpCode],GetResolverResultDescription(ResolvedEl)],El);
      eopNot:
        begin
          if ResolvedEl.BaseType in (btAllInteger+btAllBooleans) then
          else if ResolvedEl.BaseType=btVariant then
            // unary not on a Variant yields a Variant (variant manager at runtime)
          else if IsGenericTemplType(ResolvedEl) then
            // the operand type is unknown until specialization
          else
            ComputeUnaryNot(TUnaryExpr(El),ResolvedEl,Flags);
          exit;
        end;
      eopAddress:
        if (ResolvedEl.BaseType=btProc) and (ResolvedEl.IdentEl is TPasProcedure) then
          begin
          // In {$mode tp} typed-address is off, so a NESTED procedure's address
          // is a plain Pointer, NOT a procedural value — compatible with a
          // Pointer/codepointer parameter but NOT with a procedure-variable
          // parameter nor assignable to a procvar. In objfpc/delphi/fpc modes
          // @nested keeps its procedural type, so the tp gate is load-bearing to
          // avoid regressing them.
          if (TPasProcedure(ResolvedEl.IdentEl).Parent is TProcedureBody)
              and (msTP7 in GetElModeSwitches(El))
              and (msNestedProcVars in GetElModeSwitches(El))
              and (FBaseTypes[btPointer]<>nil) then
            SetResolverValueExpr(ResolvedEl,btPointer,
              FBaseTypes[btPointer],FBaseTypes[btPointer],
              TUnaryExpr(El).Operand,[rrfReadable])
          else
            SetResolverValueExpr(ResolvedEl,btContext,
              ResolvedEl.LoTypeEl,ResolvedEl.HiTypeEl,TUnaryExpr(El).Operand,[rrfReadable]);
          exit;
          end
        else if IsBitPackedOrdinalAccess(TUnaryExpr(El).Operand) then
          RaiseMsg(20260622120000,nCannotTakeAddrOfBitPackedElement,
            sCannotTakeAddrOfBitPackedElement,[],TUnaryExpr(El).Operand)
        else if (rrfReadable in ResolvedEl.Flags)
            and ((ResolvedEl.BaseType<>btPointer)
                 // @ of a pointer LVALUE (a pointer variable / field / array
                 // element, or a by-ref pointer parameter) yields a pointer-to-
                 // pointer — FPC allows it (classes lists.inc: @FList^[0]). A
                 // pointer VALUE (a temporary, e.g. @@x) has no address and is
                 // still rejected below.
                 or (rrfWritable in ResolvedEl.Flags)
                 or (ResolvedEl.IdentEl is TPasVariable)
                 or (ResolvedEl.IdentEl is TPasArgument)) then
          begin
          SetResolverValueExpr(ResolvedEl,btPointer,
            ResolvedEl.LoTypeEl,ResolvedEl.HiTypeEl,TUnaryExpr(El).Operand,[rrfReadable]);
          // Delphi mode allows `@ProcVar := Value`, which assigns the
          // procedural VARIABLE. CheckCanBeLHS has to see the UNARY node to
          // recognise that - exactly as the eopDeref path below keeps its own
          // node - and the result has to be writable, or the l-value check
          // refuses it a step later (opengl's gl.pp, ~350 entry points).
          if IsDelphiProcVarAddr(TPasExpr(El)) then
            begin
            ResolvedEl.ExprEl:=TPasExpr(El);
            Include(ResolvedEl.Flags,rrfWritable);
            end;
          exit;
          end
        else if IsGenericTemplType(ResolvedEl) then
          begin
          // `@T.Method` in a generic body: only the specialization knows what
          // it is, and it is checked again there.
          SetResolverValueExpr(ResolvedEl,btPointer,FBaseTypes[btPointer],
            FBaseTypes[btPointer],TUnaryExpr(El).Operand,[rrfReadable]);
          exit;
          end
        else
          RaiseMsg(20180208121541,nIllegalQualifierInFrontOf,sIllegalQualifierInFrontOf,
            [OpcodeStrings[TUnaryExpr(El).OpCode],GetResolverResultDescription(ResolvedEl)],El);
      eopDeref:
        begin
        ComputeDereference(TUnaryExpr(El),ResolvedEl);
        exit;
        end;
      eopMemAddress:
        if (ResolvedEl.BaseType=btContext)
            and ((ResolvedEl.LoTypeEl is TPasProcedureType)
              or IsGenericTemplType(ResolvedEl)) then
          // @@ProcVar
          exit
        else
          RaiseMsg(20180208121549,nIllegalQualifierInFrontOf,sIllegalQualifierInFrontOf,
            [OpcodeStrings[TUnaryExpr(El).OpCode],GetResolverResultDescription(ResolvedEl)],El);
    end;
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ComputeElement OpCode=',TUnaryExpr(El).OpCode);
    {$ENDIF}
    RaiseNotYetImplemented(20160926142426,El);
    end
  else if ElClass=TParamsExpr then
    case TParamsExpr(El).Kind of
      pekArrayParams: // a[]
        ComputeArrayParams(TParamsExpr(El),ResolvedEl,Flags,StartEl);
      pekFuncParams: // a()
        ComputeFuncParams(TParamsExpr(El),ResolvedEl,Flags,StartEl);
      pekSet: // []
        ComputeSetParams(TParamsExpr(El),ResolvedEl,Flags,StartEl);
    else
      RaiseNotYetImplemented(20161010184559,El);
    end
  else if ElClass=TInheritedExpr then
    begin
    // writeln('TPasResolver.ComputeElement TInheritedExpr El.CustomData=',GetObjName(El.CustomData));
    if El.CustomData is TResolvedReference then
      ComputeInherited(TInheritedExpr(El))
    else
      // no ancestor proc
      SetResolverIdentifier(ResolvedEl,btBuiltInProc,nil,nil,nil,[rrfCanBeStatement]);
    end
  else if (ElClass=TPasAliasType) or (ElClass=TPasTypeAliasType) then
    begin
    // e.g. 'type a = b' -> compute b
    ComputeElement(TPasAliasType(El).DestType,ResolvedEl,Flags+[rcType],StartEl);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.HiTypeEl:=TPasAliasType(El);
    end
  else if ElClass=TPasTypeOfType then
    begin
    // e.g. 'type a = type of b' -> compute type of b
    if TPasTypeOfType(El).DestType=nil then
      RaiseNotYetImplemented(20260922100001,El);
    ComputeElement(TPasTypeOfType(El).DestType,ResolvedEl,Flags+[rcType],StartEl);
    if El.Name<>'' then
      ResolvedEl.IdentEl:=El;
    end
  else if (ElClass=TPasVariable) then
    begin
    // e.g. 'var a:b' -> compute b, use a as IdentEl
    if rcConstant in Flags then
      RaiseConstantExprExp(20170216152737,StartEl);
    ComputeElement(TPasVariable(El).VarType,ResolvedEl,Flags+[rcType],StartEl);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[rrfReadable,rrfWritable];
    end
  else if (ElClass=TPasConst) then
    begin
    // e.g. 'var a:b' -> compute b, use a as IdentEl
    if TPasConst(El).VarType<>nil then
      begin
      {  typed const
         A typed constant (IsConst=false) is a writable initialised variable, 
         so it is rejected only where a compile-time constant value is required
         (rcConstant) - exactly like a TPasVariable above. 
         It must NOT be rejected for rcType alone: SizeOf(typedConst) legitimately asks for its type. }
      if (not TPasConst(El).IsConst) and (rcConstant in Flags) then
        RaiseConstantExprExp(20170216152739,StartEl);
      ComputeElement(TPasConst(El).VarType,ResolvedEl,Flags+[rcType],StartEl);
      ResolvedEl.IdentEl:=El;
      if TPasConst(El).IsConst then
        ResolvedEl.Flags:=[rrfReadable]
      else
        ResolvedEl.Flags:=[rrfReadable,rrfWritable];
      end
    else
      begin
      // untyped const
      ComputeElement(TPasConst(El).Expr,ResolvedEl,Flags+[rcConstant],StartEl);
      ResolvedEl.IdentEl:=El;
      ResolvedEl.Flags:=[rrfReadable];
      end;
    end
  else if (ElClass=TPasEnumValue) then
    begin
    TypeEl:=NoNil(El.Parent) as TPasEnumType;
    SetResolverIdentifier(ResolvedEl,btContext,El,TypeEl,TypeEl,[rrfReadable])
    end
  else if (ElClass=TPasEnumType) then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasEnumType(El),TPasEnumType(El),[])
  else if (ElClass=TPasProperty) then
    begin
    if rcConstant in Flags then
      RaiseConstantExprExp(20170216152741,StartEl);
    if GetPasPropertyArgs(TPasProperty(El)).Count=0 then
      begin
      ComputeElement(GetPasPropertyType(TPasProperty(El)),ResolvedEl,
        Flags+[rcType],StartEl);
      ResolvedEl.IdentEl:=El;
      ResolvedEl.Flags:=[];
      if GetPasPropertyGetter(TPasProperty(El))<>nil then
        Include(ResolvedEl.Flags,rrfReadable);
      if GetPasPropertySetter(TPasProperty(El))<>nil then
        Include(ResolvedEl.Flags,rrfWritable);
      if IsProcedureType(ResolvedEl,true) then
        Include(ResolvedEl.Flags,rrfCanBeStatement);
      end
    else
      begin
      // index property without name
      // Note: computing the pekArrayParams TParamsExpr will convert this to the type
      SetResolverIdentifier(ResolvedEl,btArrayProperty,El,nil,nil,[]);
      end;
    end
  else if ElClass=TPasArgument then
    begin
    if rcConstant in Flags then
      RaiseConstantExprExp(20170216152744,StartEl);
    if TPasArgument(El).ArgType=nil then
      // untyped parameter
      SetResolverIdentifier(ResolvedEl,btUntyped,El,nil,nil,[])
    else
      begin
      // typed parameter -> use param as IdentEl, compute type
      ComputeElement(TPasArgument(El).ArgType,ResolvedEl,Flags+[rcType],StartEl);
      ResolvedEl.IdentEl:=El;
      end;
    ResolvedEl.Flags:=[rrfReadable];
    if TPasArgument(El).Access in [argDefault, argVar, argOut] then
      Include(ResolvedEl.Flags,rrfWritable);
    if IsProcedureType(ResolvedEl,true) then
      Include(ResolvedEl.Flags,rrfCanBeStatement);
    end
  else if ElClass=TPasClassType then
    begin
    if TPasClassType(El).IsForward and (El.CustomData<>nil) then
      begin
      DeclEl:=(TPasClassType(El).CustomData as TResolvedReference).Declaration;
      TypeEl:=NoNil(DeclEl) as TPasClassType;
      end
    else
      TypeEl:=TPasClassType(El);
    SetResolverIdentifier(ResolvedEl,btContext,
                          TypeEl,TypeEl,TypeEl,[]);
    end
  else if ElClass=TPasClassOfType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasClassOfType(El),TPasClassOfType(El),[])
  else if ElClass=TPasPointerType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasPointerType(El),TPasPointerType(El),[])
  else if ElClass=TPasRecordType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasRecordType(El),TPasRecordType(El),[])
  else if ElClass=TPasRangeType then
    begin
    ComputeElement(TPasRangeType(El).RangeExpr,ResolvedEl,[rcConstant],StartEl);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.LoTypeEl:=TPasRangeType(El);
    ResolvedEl.HiTypeEl:=ResolvedEl.LoTypeEl;
    if ResolvedEl.ExprEl=nil then
      ResolvedEl.ExprEl:=TPasRangeType(El).RangeExpr;
    ResolvedEl.Flags:=[];
    end
  else if ElClass=TPasSetType then
    begin
    ComputeElement(TPasSetType(El).EnumType,ResolvedEl,[rcConstant],StartEl);
    if ResolvedEl.BaseType=btRange then
      begin
      ConvertRangeToElement(ResolvedEl);
      ResolvedEl.LoTypeEl:=TPasSetType(El).EnumType;
      ResolvedEl.HiTypeEl:=ResolvedEl.LoTypeEl;
      end;
    ResolvedEl.SubType:=ResolvedEl.BaseType;
    ResolvedEl.BaseType:=btSet;
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[];
    end
  else if ElClass=TPasResultElement then
    begin
    if rcConstant in Flags then
      RaiseConstantExprExp(20170216152746,StartEl);
    ComputeResultElement(TPasResultElement(El),ResolvedEl,Flags,StartEl);
    end
  else if ElClass=TPasUsesUnit then
    begin
    if TPasUsesUnit(El).Module is TPasModule then
      SetResolverIdentifier(ResolvedEl,btModule,TPasUsesUnit(El).Module,nil,nil,[])
    else
      RaiseNotYetImplemented(20170429112047,TPasUsesUnit(El).Module);
    end
  else if El.InheritsFrom(TPasModule) then
    SetResolverIdentifier(ResolvedEl,btModule,El,nil,nil,[])
  else if ElClass=TNilExpr then
    SetResolverValueExpr(ResolvedEl,btNil,FBaseTypes[btNil],FBaseTypes[btNil],
                         TNilExpr(El),[rrfReadable])
  else if El.InheritsFrom(TPasProcedure) then
    begin
    TypeEl:=TPasProcedure(El).ProcType;
    SetResolverIdentifier(ResolvedEl,btProc,El,TypeEl,TypeEl,[rrfCanBeStatement]);
    if (TPasProcedure(El).ProcType is TPasFunctionType)
        or (ElClass=TPasConstructor) then
      Include(ResolvedEl.Flags,rrfReadable);
    // Note: implicit calls are handled in TPrimitiveExpr
    end
  else if El.InheritsFrom(TPasProcedureType) then
    begin
    SetResolverIdentifier(ResolvedEl,btContext,El,
               TPasProcedureType(El),TPasProcedureType(El),[rrfCanBeStatement]);
    // Note: implicit calls are handled in TPrimitiveExpr
    end
  else if ElClass=TProcedureExpr then
    begin
    TypeEl:=TProcedureExpr(El).Proc.ProcType;
    SetResolverValueExpr(ResolvedEl,btProc,TypeEl,TypeEl,TProcedureExpr(El),[rrfReadable]);
    end
  else if ElClass=TPasArrayType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasArrayType(El),TPasArrayType(El),[])
  {$IFNDEF PAS2JS}
  else if ElClass=TPasFileType then
    SetResolverIdentifier(ResolvedEl,btFile,El,TPasFileType(El),TPasFileType(El),[])
  {$ENDIF}  
  else if ElClass=TArrayValues then
    SetResolverValueExpr(ResolvedEl,btArrayLit,nil,nil,TArrayValues(El),[rrfReadable])
  else if ElClass=TRecordValues then
    ComputeRecordValues(TRecordValues(El),ResolvedEl,Flags,StartEl)
  else if ElClass=TPasStringType then
    begin
    {$ifdef FPC_HAS_CPSTRING}
    if TPasStringType(El).CodePageExpr<>'' then
      SetResolverTypeExpr(ResolvedEl,btAnsiString,
                 BaseTypes[btAnsiString],BaseTypes[btAnsiString],[rrfReadable])
    else
      SetResolverTypeExpr(ResolvedEl,btShortString,
                 BaseTypes[btShortString],BaseTypes[btShortString],[rrfReadable]);
    if BaseTypes[btShortString]=nil then
    {$endif}
      RaiseMsg(20170419203146,nIllegalQualifier,sIllegalQualifier,['['],El);
    end
  else if ElClass=TPasResString then
    SetResolverIdentifier(ResolvedEl,btString,El,
                        FBaseTypes[btString],FBaseTypes[btString],[rrfReadable])
  else if ElClass=TPasGenericTemplateType then
    begin
    if TPasGenericTemplateType(El).IsConst
        and (length(TPasGenericTemplateType(El).Constraints)>0)
        and (TPasGenericTemplateType(El).Constraints[0] is TPasType) then
      begin
      // Const generic param: resolve using constraint type so built-in
      // functions (high, low, sizeof) accept the param as ordinal
      ComputeElement(TPasGenericTemplateType(El).Constraints[0],
        ResolvedEl,Flags+[rcType],StartEl);
      ResolvedEl.IdentEl:=El;
      Include(ResolvedEl.Flags,rrfReadable);
      end
    else
      SetResolverIdentifier(ResolvedEl,btContext,El,TPasGenericTemplateType(El),
        TPasGenericTemplateType(El),[]);
    end
  else if ElClass=TPasSpecializeType then
    ComputeSpecializeType(TPasSpecializeType(El))
  else if ElClass=TInlineSpecializeExpr then
    ComputeElement(TInlineSpecializeExpr(El).NameExpr,ResolvedEl,Flags,StartEl)
  else if ElClass=TPasExportSymbol then
    ComputeExportSymbol(TPasExportSymbol(El))
  else
    RaiseNotYetImplemented(20160922163705,El);
  {$IF defined(nodejs) and defined(VerbosePasResolver)}
  if not isNumber(ResolvedEl.BaseType) then
    begin
    {AllowWriteln}
    writeln('TPasResolver.ComputeElement ',GetObjName(El),' typeof ResolvedEl.BaseType=',jsTypeOf(ResolvedEl.BaseType),' ResolvedEl=',GetResolverResultDbg(ResolvedEl));
    RaiseInternalError(20181101123527,jsTypeOf(ResolvedEl.LoTypeEl));
    {AllowWriteln-}
    end;
  {$ENDIF}
end;

procedure TPasResolver.ComputeResultElement(El: TPasResultElement; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
begin
  if El.ResultType=nil then
    RaiseNotYetImplemented(20200524214458,El);
  ComputeElement(El.ResultType,ResolvedEl,Flags+[rcType,rcNoImplicitProc],StartEl);
  ResolvedEl.IdentEl:=El;
  ResolvedEl.Flags:=[rrfReadable,rrfWritable];
end;

function TPasResolver.ComputeProcAsyncResult(El: TPasElement; var ResolvedEl: TPasResolverResult;
  Flags: TPasResolverComputeFlags; StartEl: TPasElement): boolean;
begin
  Result:=false;
  if El=nil then ;
  if Flags=[] then ;
  if StartEl=nil then ;
  if ResolvedEl.IdentEl=nil then ;
end;

function TPasResolver.Eval(Expr: TPasExpr; Flags: TResEvalFlags;
  Store: boolean): TResEvalValue;
// Important: Caller must free result with ReleaseEvalValue(Result)
begin
  Result:=fExprEvaluator.Eval(Expr,Flags);
  if Result=nil then exit;
  {$IFDEF VerbosePasResEval}
  writeln('TPasResolver.Eval Expr=',GetObjName(Expr),' Result=',Result.AsDebugString);
  {$ENDIF}

  if Store
      and (Expr.CustomData=nil)
      and (Result.Element=nil)
      and (not fExprEvaluator.IsSimpleExpr(Expr))
      and (Expr.GetModule=RootElement) then
    begin
    //writeln('TPasResolver.Eval STORE Expr=',GetObjName(Expr),' Result=',Result.AsDebugString);
    AddResolveData(Expr,Result,lkModule);
    end;
end;

function TPasResolver.Eval(const Value: TPasResolverResult;
  Flags: TResEvalFlags; Store: boolean): TResEvalValue;
var
  Expr: TPasExpr;
begin
  Result:=nil;
  if Value.ExprEl<>nil then
    Result:=Eval(Value.ExprEl,Flags,Store)
  else if Value.IdentEl is TPasConst then
    begin
    Expr:=TPasVariable(Value.IdentEl).Expr;
    if Expr=nil then exit;
    Result:=Eval(Expr,Flags,Store)
    end;
end;

function TPasResolver.IsSameType(TypeA, TypeB: TPasType;
  ResolveAlias: TPRResolveAlias): boolean;
var
  btA, btB: TResolverBaseType;
begin
  if (TypeA=nil) or (TypeB=nil) then exit(false);
  case ResolveAlias of
  prraSimple:
    begin
    TypeA:=ResolveSimpleAliasType(TypeA);
    TypeB:=ResolveSimpleAliasType(TypeB);
    end;
  prraAlias:
    begin
    TypeA:=ResolveAliasType(TypeA);
    TypeB:=ResolveAliasType(TypeB);
    end;
  end;
  if TypeA=TypeB then exit(true);
  if (TypeA.ClassType=TPasSpecializeType) and (TypeB.ClassType=TPasSpecializeType)
      and SameSpecializeType(TPasSpecializeType(TypeA),TPasSpecializeType(TypeB),ResolveAlias) then
    exit(true);
  if (TypeA.ClassType=TPasUnresolvedSymbolRef)
      and (TypeB.ClassType=TPasUnresolvedSymbolRef) then
    begin
    if CompareText(TypeA.Name,TypeB.Name)=0 then
      exit(true);
    btA:=TResElDataBaseType(TypeA.CustomData).BaseType;
    btB:=TResElDataBaseType(TypeB.CustomData).BaseType;
    Result:=GetActualBaseType(btA)=GetActualBaseType(btB);
    exit;
    end;
  Result:=false;
end;

function TPasResolver.HasExactType(const ResolvedEl: TPasResolverResult
  ): boolean;
var
  IdentEl: TPasElement;
  Expr: TPasExpr;
begin
  IdentEl:=ResolvedEl.IdentEl;
  if IdentEl<>nil then
    begin
    if IdentEl is TPasVariable then
      exit(TPasVariable(IdentEl).VarType<>nil)
    else if IdentEl.ClassType=TPasArgument then
      exit(TPasArgument(IdentEl).ArgType<>nil)
    else if IdentEl.ClassType=TPasResultElement then
      exit(TPasResultElement(IdentEl).ResultType<>nil)
    else if IdentEl is TPasType then
      exit(true)
    else
      exit(false);
    end;
  Expr:=ResolvedEl.ExprEl;
  if Expr<>nil then
    begin
    if Expr.Kind in [pekNumber,pekString,pekStringMultiLine,pekNil,pekBoolConst] then
      exit(true)
    else
      exit(false);
    end;
  Result:=false;
end;

function TPasResolver.IndexOfGenericParam(Params: TPasExprArray): integer;
var
  i: Integer;
  ParamResolved: TPasResolverResult;
begin
  for i:=0 to length(Params)-1 do
    begin
    ComputeElement(Params[i],ParamResolved,[]);
    if ParamResolved.LoTypeEl is TPasGenericTemplateType then
      exit(i);
    end;
  Result:=-1;
end;

function TPasResolver.MapGenericSelfRef(DeclEl, PosEl: TPasElement): TPasElement;
// Inside a specialization of a generic, the generic's bare name means that
// specialization, also from a method of a type nested in it.
var
  El: TPasElement;
  SpecItem: TPRSpecializedItem;
  j: Integer;
begin
  Result:=DeclEl;
  if not ((DeclEl is TPasGenericType)
      and (GetTypeParameterCount(TPasGenericType(DeclEl))>0)) then
    exit;
  El:=PosEl;
  while El<>nil do
    begin
    if (El.CustomData is TPasGenericScope)
        and (TPasGenericScope(El.CustomData).SpecializedFromItem<>nil)
        and (TPasGenericScope(El.CustomData).SpecializedFromItem.GenericEl=DeclEl) then
      begin
      // not when the generic is one of its own type arguments
      SpecItem:=TPasGenericScope(El.CustomData).SpecializedFromItem;
      for j:=0 to length(SpecItem.Params)-1 do
        if SpecItem.Params[j]=DeclEl then
          exit;
      exit(El);
      end;
    // an implementation proc hangs off the implementation section, not off its
    // class, so continue with the class
    if (El is TPasProcedure) and (El.CustomData is TPasProcedureScope)
        and (TPasProcedureScope(El.CustomData).ClassRecScope<>nil) then
      El:=TPasProcedureScope(El.CustomData).ClassRecScope.Element
    else
      El:=El.Parent;
    end;
end;

function TPasResolver.IsInsideGenericTypeAt(GenEl: TPasGenericType;
  PosEl: TPasElement): Boolean;
// Whether PosEl lies in GenEl or in a specialization of it, where the bare
// generic name may be used (objfpc).
var
  i, j: Integer;
  Scope: TPasScope;
  ScopeEl: TPasElement;
  ProcScope: TPasProcedureScope;
  GenScope: TPasGenericScope;
  SpecItem: TPRSpecializedItem;
  El: TPasElement;
begin
    Result:=True;
    // ObjFPC allows a bare self-reference to the enclosing generic; Delphi
    // requires the full specialization, so it is not treated as "inside". The
    // mode is the one the generic was written in, not that of the unit being
    // parsed, which may only be specializing it.
    if msDelphi in GetElModeSwitches(GenEl) then
      exit(False);
    // Direct parent check
    if PosEl.HasParent(GenEl) then
      exit;
    // In a specialization of GenEl, also from a method of a type nested in it.
    if MapGenericSelfRef(GenEl,PosEl)<>GenEl then
      exit;
    // Check scope chain for method bodies of the generic type
    for i:=ScopeCount-1 downto 0 do
      begin
      Scope:=Scopes[i];
      ScopeEl:=Scope.Element;
      if ScopeEl=GenEl then
        exit;
      if (ScopeEl<>nil) and ScopeEl.HasParent(GenEl) then
        exit;
      // Check if scope element is a specialization of GenEl
      if (ScopeEl<>nil) and (ScopeEl.CustomData is TPasGenericScope) then
        begin
        GenScope:=TPasGenericScope(ScopeEl.CustomData);
        if (GenScope.SpecializedFromItem<>nil)
            and (GenScope.SpecializedFromItem.GenericEl=GenEl) then
          begin
          // We are inside a specialization of GenEl. Allow self-reference ONLY
          // if GenEl is not one of the type arguments (which would mean the
          // reference came from substitution, not from the original body).
          SpecItem:=GenScope.SpecializedFromItem;
          Result:=True;
          for j:=0 to Length(SpecItem.Params)-1 do
            if SpecItem.Params[j]=GenEl then
              begin
              Result:=False;
              break;
              end;
          exit; // definitive answer, stop searching
          end;
        end;
      if IsPartialSpecTypeArg(ScopeEl,GenEl) then
        exit;
      if (Scope is TPasProcedureScope) then
        begin
        ProcScope:=TPasProcedureScope(Scope);
        if (ProcScope.ClassRecScope<>nil)
            and (ProcScope.ClassRecScope.Element=GenEl) then
          exit;
        if (ProcScope.ClassRecScope<>nil)
            and (ProcScope.ClassRecScope.Element<>nil)
            and ProcScope.ClassRecScope.Element.HasParent(GenEl) then
          exit;
        if (ProcScope.ClassRecScope<>nil)
            and IsPartialSpecTypeArg(ProcScope.ClassRecScope.Element,GenEl) then
          exit;
        end;
      end;
    Result:=False;
end;

procedure TPasResolver.CheckUseAsType(aType: TPasElement; id: TMaxPrecInt;
  PosEl: TPasElement);

  function IsInsidePartialSpec: Boolean;
  // True when PosEl belongs to a PARTIAL specialization - one whose type
  // arguments are themselves templates or still-unspecialized generics. Nothing
  // in it is instantiable yet, so a bare generic reference among its members is
  // only judged when the enclosing specialization becomes concrete
  // (rtl-generics binds TCustomTreeEnumerator's TTree to TCustomAVLTreeMap,
  // the very generic the specialization sits in).
  var
    El: TPasElement;
    Scope: TPasGenericScope;
    SpecItem: TPRSpecializedItem;
    i: Integer;
    Param: TPasElement;
  begin
    Result:=false;
    El:=PosEl;
    while El<>nil do
      begin
      if El.CustomData is TPasGenericScope then
        begin
        Scope:=TPasGenericScope(El.CustomData);
        SpecItem:=Scope.SpecializedFromItem;
        if SpecItem<>nil then
          for i:=0 to length(SpecItem.Params)-1 do
            begin
            Param:=SpecItem.Params[i];
            if (Param is TPasGenericTemplateType)
                or ((Param is TPasGenericType)
                    and not IsFullySpecialized(TPasGenericType(Param))) then
              exit(true);
            end;
        end;
      // an implementation proc hangs off the implementation section, not off its
      // class, so the parent chain alone never reaches the specialization
      if (El is TPasProcedure) and (El.CustomData is TPasProcedureScope)
          and (TPasProcedureScope(El.CustomData).ClassRecScope<>nil) then
        El:=TPasProcedureScope(El.CustomData).ClassRecScope.Element
      else
        El:=El.Parent;
      end;
  end;

begin
  if aType=nil then exit;
  if aType is TPasGenericType then
    begin
    if aType.ClassType=TPasClassType then
      begin
      if TPasClassType(aType).HelperForType<>nil then
        RaiseHelpersCannotBeUsedAsType(id,PosEl);
      end;
    if (TPasGenericType(aType).GenericTemplateTypes<>nil)
        and (TPasGenericType(aType).GenericTemplateTypes.Count>0) then
      begin
      // ref to generic type without specialization
      if IsInsideGenericTypeAt(TPasGenericType(aType),PosEl) then
        // inside the generic type, self-reference is allowed in both modes
      else if IsInsidePartialSpec then
        // deferred: judged when the enclosing specialization becomes concrete
      else
        RaiseMsg(id,nGenericsWithoutSpecializationAsType,sGenericsWithoutSpecializationAsType,
            [PosEl.ElementTypeName],PosEl);
      end;
    end;
end;

function TPasResolver.GetPasClassAncestor(ClassEl: TPasClassType;
  SkipAlias: boolean): TPasType;
var
  DeclEl: TPasElement;
  ClassScope: TPasClassScope;
begin
  Result:=nil;
  if ClassEl=nil then
    exit;
  if ClassEl.CustomData=nil then
    exit;
  if ClassEl.IsForward then
    begin
    DeclEl:=(ClassEl.CustomData as TResolvedReference).Declaration;
    ClassEl:=NoNil(DeclEl) as TPasClassType;
    Result:=ClassEl;
    end
  else
    begin
    ClassScope:=ClassEl.CustomData as TPasClassScope;
    if not (pcsfAncestorResolved in ClassScope.Flags) then
      exit;
    if SkipAlias then
      begin
      if ClassScope.AncestorScope=nil then
        exit;
      Result:=TPasClassType(ClassScope.AncestorScope.Element);
      end
    else
      Result:=ClassScope.DirectAncestor;
    end;
end;

function TPasResolver.GetPasClassForward(ClassEl: TPasClassType): TPasClassType;
var
  Parent: TPasElement;
  i: Integer;
  CurClass: TPasClassType;
  Ref: TResolvedReference;
  Decls: TFPList;
begin
  Result:=nil;
  if ClassEl=nil then exit;
  Parent:=ClassEl.Parent;
  if not (Parent is TPasDeclarations) then
    RaiseNotYetImplemented(20200926214106,ClassEl);
  Decls:=TPasDeclarations(Parent).Classes;
  for i:=0 to Decls.Count-1 do
    begin
    CurClass:=TPasClassType(Decls[i]);
    if CurClass=ClassEl then exit;
    if not CurClass.IsForward then continue;
    Ref:=TResolvedReference(CurClass.CustomData);
    if Ref.Declaration=ClassEl then
      exit(TPasClassType(Ref.Declaration));
    end;
end;

function TPasResolver.GetParentProcBody(El: TPasElement): TProcedureBody;
begin
  while El<>nil do
    begin
    if El is TProcedureBody then
      exit(TProcedureBody(El));
    El:=El.Parent;
    end;
  Result:=nil;
end;

function TPasResolver.ProcHasImplElements(Proc: TPasProcedure): boolean;
begin
  Result:=GetProcFirstImplEl(Proc)<>nil;
end;

function TPasResolver.IndexOfImplementedInterface(ClassEl: TPasClassType;
  aType: TPasType): integer;
var
  List: TFPList;
  i: Integer;
begin
  if aType=nil then exit(-1);
  aType:=ResolveAliasType(aType);
  List:=ClassEl.Interfaces;
  for i:=0 to List.Count-1 do
    if ResolveAliasType(TPasType(List[i]))=aType then
      exit(i);
  Result:=-1;
end;

function TPasResolver.GetLoop(El: TPasElement): TPasImplElement;
begin
  while El<>nil do
    begin
    if (El.ClassType=TPasImplRepeatUntil)
        or (El.ClassType=TPasImplWhileDo)
        or (El.ClassType=TPasImplForLoop) then
      exit(TPasImplElement(El));
    El:=El.Parent;
    end;
  Result:=nil;
end;

function TPasResolver.ResolveAliasType(aType: TPasType; SkipTypeAlias: boolean
  ): TPasType;
var
  C: TClass;
  SpecType: TPasSpecializeType;
begin
  while aType<>nil do
    begin
    C:=aType.ClassType;
    if C=TPasAliasType then
      aType:=TPasAliasType(aType).DestType
    else if C=TPasTypeOfType then
      aType:=TPasTypeOfType(aType).DestType
    else if (C=TPasTypeAliasType) and SkipTypeAlias then
      aType:=TPasAliasType(aType).DestType
    else if (C=TPasClassType) and TPasClassType(aType).IsForward
        and (aType.CustomData is TResolvedReference) then
      aType:=NoNil(TResolvedReference(aType.CustomData).Declaration) as TPasType
    else if C=TPasSpecializeType then
      begin
      SpecType:=TPasSpecializeType(aType);
      if SpecType.SubType<>nil then
        // e.g. a<b>.c
        aType:=SpecType.SubType
      else
        begin
        if SpecType.CustomData is TPasSpecializeTypeData then
          exit(TPasSpecializeTypeData(SpecType.CustomData).SpecializedType);
        aType:=SpecType.DestType;
        end;
      end
    else
      exit(aType);
    end;
  Result:=nil;
end;

function TPasResolver.ResolveAliasTypeEl(El: TPasElement): TPasType;
begin
  if (El is TPasType) then
    Result:=ResolveAliasType(TPasType(El))
  else
    Result:=nil;
end;

function TPasResolver.ExprIsAddrTarget(El: TPasExpr): boolean;
{ returns true if El is
  a) the last element of an @ operator expression
  e.g. '@p().o[].El' or '@El[]'
  b) mode delphi: the last element of a right side of an assignment
  c) an accessor function, e.g. property P read El;
  d) an export
}
var
  Parent: TPasElement;
  Prop: TPasProperty;
  C: TClass;
begin
  Result:=false;
  if El=nil then exit;
  if not IsNameExpr(El) then
    exit;
  repeat
    Parent:=El.Parent;
    //writeln('TPasResolver.ExprIsAddrTarget El=',GetObjName(El),' Parent=',GetObjName(Parent));
    C:=Parent.ClassType;
    if C=TUnaryExpr then
      begin
      if TUnaryExpr(Parent).OpCode=eopAddress then exit(true);
      end
    else if C=TBinaryExpr then
      begin
      if TBinaryExpr(Parent).Right<>El then exit;
      if TBinaryExpr(Parent).OpCode<>eopSubIdent then exit;
      end
    else if C=TParamsExpr then
      begin
      if TParamsExpr(Parent).Value<>El then exit;
      end
    else if C=TPasProperty then
      begin
      Prop:=TPasProperty(Parent);
      Result:=(Prop.ReadAccessor=El) or (Prop.WriteAccessor=El) or (Prop.StoredAccessor=El);
      exit;
      end
    else if C=TPasImplAssign then
      begin
      if TPasImplAssign(Parent).Right<>El then exit;
      // A bare proc name on an assignment RHS is a proc-address target (not a
      // 0-arg call) in Delphi mode AND in the TP-style procvar modes
      // (tp/macpas/gpc — all have msTPProcVar). This lets `ProcVar:=Proc`
      // without @ resolve for a proc that needs params (tprocvar2), instead of
      // raising "wrong number of parameters". A non-procvar LHS still fails
      // later with an incompatible-types error. (pas2js strips msTPProcVar from
      // its Delphi mode, so the explicit msDelphi arm is kept too.)
      // The mode of the unit that WROTE the assignment, not of the one being
      // compiled: a Delphi-mode generic's body re-resolved for an objfpc
      // consumer took `FDict.OnKeyNotify := InternalDictionaryNotify` for a
      // 0-arg call and reported the wrong number of parameters.
      if (msDelphi in GetElModeSwitches(El))
          or (msTPProcVar in GetElModeSwitches(El)) then exit(true);
      exit;
      end
    else if C=TRecordValues then
      begin
      // A bare proc name used as a record-const field value (e.g. `MyProc: SomeProc`)
      // is a proc-address target, not a 0-arg call — same rule as an assignment RHS.
      if (msDelphi in GetElModeSwitches(El)) then exit(true);
      exit;
      end
    else if C=TPasExportSymbol then
      exit(true)
    else
      exit;
    El:=TPasExpr(Parent);
  until false;
end;

function TPasResolver.ParentNeedsExprResult(El: TPasExpr): boolean;
var
  C: TClass;
  P: TPasElement;
begin
  if (El=nil) or (El.Parent=nil) then exit(false);
  Result:=false;
  P:=El.Parent;
  C:=P.ClassType;
  if C=TBinaryExpr then
    begin
    if TBinaryExpr(P).Right=El then
      begin
      if (TBinaryExpr(P).OpCode=eopSubIdent)
          or ((TBinaryExpr(P).OpCode=eopNone) and (TBinaryExpr(P).Left is TInheritedExpr)) then
        Result:=ParentNeedsExprResult(TBinaryExpr(P))
      else
        Result:=true;
      end
    else
      Result:=true;
    end
  else if C=TInlineSpecializeExpr then
    Result:=ParentNeedsExprResult(TInlineSpecializeExpr(P))
  else if C.InheritsFrom(TPasExpr) then
    Result:=true
  else if (C=TPasEnumValue)
      or (C=TPasArgument)
      or (C=TPasVariable)
      or (C=TPasExportSymbol) then
    Result:=true
  else if C=TPasClassType then
    Result:=TPasClassType(P).GUIDExpr=El
  else if C=TPasProperty then
    Result:=(TPasProperty(P).IndexExpr=El)
        or (TPasProperty(P).DispIDExpr=El)
        or (TPasProperty(P).DefaultExpr=El)
  else if C=TPasProcedure then
    Result:=(TPasProcedure(P).LibraryExpr=El)
         or (TPasProcedure(P).DispIDExpr=El)
  else if C=TPasImplRepeatUntil then
    Result:=(TPasImplRepeatUntil(P).ConditionExpr=El)
  else if C=TPasImplIfElse then
    Result:=(TPasImplIfElse(P).ConditionExpr=El)
  else if C=TPasImplWhileDo then
    Result:=(TPasImplWhileDo(P).ConditionExpr=El)
  else if C=TPasImplWithDo then
    Result:=(TPasImplWithDo(P).Expressions.IndexOf(El)>=0)
  else if C=TPasImplCaseOf then
    Result:=(TPasImplCaseOf(P).CaseExpr=El)
  else if C=TPasImplCaseStatement then
    Result:=(TPasImplCaseStatement(P).Expressions.IndexOf(El)>=0)
  else if C=TPasImplForLoop then
    Result:=(TPasImplForLoop(P).StartExpr=El)
         or (TPasImplForLoop(P).EndExpr=El)
  else if C=TPasImplAssign then
    Result:=(TPasImplAssign(P).Right=El)
  else if C=TPasImplRaise then
    Result:=(TPasImplRaise(P).ExceptAddr=El);
end;

function TPasResolver.GetReference_ConstructorType(Ref: TResolvedReference;
  Expr: TPasExpr): TPasResolverResult;
var
  TypeEl: TPasType;
begin
  TypeEl:=(Ref.Context as TResolvedRefCtxConstructor).Typ;
  if TypeEl=nil then
    RaiseNotYetImplemented(20190125205339,Expr)
  else if (TypeEl is TPasClassType) and (TPasClassType(TypeEl).ObjKind = okObject) then
    SetResolverValueExpr(Result,btBoolean,FBaseTypes[btBoolean],
      FBaseTypes[btBoolean],Expr,[rrfReadable])
  else if (TypeEl is TPasMembersType) or (TypeEl is TPasGenericTemplateType) then
    { A class/record instance, or a generic type-parameter instance:
      T.Create yields a readable T value, not the type T, 
      so member access on the  result is instance member access. }
    SetResolverValueExpr(Result,btContext,TypeEl,TypeEl,Expr,[rrfReadable])
  else
    begin
    ComputeElement(TypeEl,Result,[rcType]);
    Result.ExprEl:=Expr;
    Result.Flags:=[rrfReadable];
    end;
end;

function TPasResolver.GetParamsValueRef(Params: TParamsExpr): TResolvedReference;
var
  El: TPasExpr;
begin
  Result:=nil;
  if Params=nil then exit;
  El:=Params.Value;
  while El<>nil do
    begin
    if El.CustomData is TResolvedReference then
      exit(TResolvedReference(El.CustomData));
    if El.ClassType=TInlineSpecializeExpr then
      El:=TInlineSpecializeExpr(El).NameExpr
    else if (El.ClassType=TBinaryExpr)
        and (TBinaryExpr(El).OpCode=eopSubIdent) then
      El:=TBinaryExpr(El).Right
    else
      exit;
    end;
end;

function TPasResolver.GetSetType(const ResolvedSet: TPasResolverResult
  ): TPasSetType;
var
  IdentEl: TPasElement;
  aType: TPasType;
  C: TClass;
begin
  Result:=nil;
  if ResolvedSet.BaseType=btSet then
    begin
    IdentEl:=ResolvedSet.IdentEl;
    if IdentEl=nil then exit;
    C:=IdentEl.ClassType;
    if (C=TPasVariable)
        or (C=TPasConst) then
      aType:=TPasVariable(IdentEl).VarType
    else if C=TPasProperty then
      aType:=GetPasPropertyType(TPasProperty(IdentEl))
    else if C=TPasArgument then
      aType:=TPasArgument(IdentEl).ArgType
    else if C.InheritsFrom(TPasProcedure)
        and (TPasProcedure(IdentEl).ProcType is TPasFunctionType) then
      aType:=TPasFunctionType(TPasProcedure(IdentEl).ProcType).ResultEl.ResultType
    else if C=TPasSetType then
      exit(TPasSetType(IdentEl))
    else
      exit;
    if aType.ClassType=TPasSetType then
      Result:=TPasSetType(aType);
    end
  else if ResolvedSet.BaseType=btContext then
    begin
    if ResolvedSet.LoTypeEl.ClassType=TPasSetType then
      if ResolvedSet.HiTypeEl.ClassType=TPasSetType then
        Result:=TPasSetType(ResolvedSet.HiTypeEl)
      else
        Result:=TPasSetType(ResolvedSet.LoTypeEl);
    end;
end;

function TPasResolver.StringElementIsWritable: boolean;
begin
  Result:=false;
end;

function TPasResolver.AsOperatorAllowsUpcast: boolean;
// ppcx64 accepts `Inst as Ancestor`; pas2js refuses it. Off by default so the
// shared test suite keeps its expectation.

begin
  Result:=false;
end;


function TPasResolver.IsConstIntFitting(const R: TPasResolverResult;
  TargetBT: TResolverBaseType): boolean;
// True when R is a CONSTANT integer whose value fits TargetBT's range, so the
// conversion loses nothing.

var
  Val: TResEvalValue;
  MinVal, MaxVal, N: TMaxPrecInt;
begin
  Result:=false;
  if R.ExprEl=nil then exit;
  // Only a SCALAR literal or a constant identifier is probed: Eval raises on
  // some composite nodes (an array/set constructor answers
  // "not yet implemented TResEvalSet"), and those are never an integer value
  // anyway.
  if not ((R.ExprEl.ClassType=TPrimitiveExpr)
          or ((R.ExprEl is TUnaryExpr)
              and (TUnaryExpr(R.ExprEl).OpCode in [eopAdd,eopSubtract]))) then
    exit;
  if not GetIntegerRange(TargetBT,MinVal,MaxVal) then exit;
  Val:=Eval(R.ExprEl,[]);
  if Val=nil then exit;
  try
    if Val.Kind=revkInt then
      N:=TResEvalInt(Val).Int
    else if (Val.Kind=revkUInt)
        and (TResEvalUInt(Val).UInt<=TMaxPrecUInt(High(TMaxPrecInt))) then
      N:=TMaxPrecInt(TResEvalUInt(Val).UInt)
    else
      exit;
    Result:=(N>=MinVal) and (N<=MaxVal);
  finally
    ReleaseEvalValue(Val);
  end;
end;


function TPasResolver.IsCharPointerRes(const R: TPasResolverResult): boolean;
// A pointer to a char type (PAnsiChar/PWideChar/PUnicodeChar) concatenates with
// a string as a null-terminated string operand.

var
  SubRes: TPasResolverResult;
begin
  Result:=false;
  if (R.BaseType=btContext)
      and (R.LoTypeEl is TPasPointerType)
      and (TPasPointerType(R.LoTypeEl).DestType<>nil) then
    begin
    ComputeElement(TPasPointerType(R.LoTypeEl).DestType,SubRes,[rcType]);
    Result:=SubRes.BaseType in btAllChars;
    end;
end;


function TPasResolver.IsDelphiProcVarAddr(El: TPasExpr): boolean;
// True for `@ProcVar` written as an ASSIGNMENT TARGET in Delphi mode, where it
// means the procedural variable itself rather than a call. opengl's gl.pp
// clears ~350 entry points with `@glAccum := nil`. FPC allows this ONLY in
// Delphi mode and ONLY for a procedural VARIABLE - `@IntVar :=` and
// `@PtrVar :=` stay "Can't assign values to an address".

var
  OpResolved: TPasResolverResult;
begin
  Result:=false;
  if not (El is TUnaryExpr) then exit;
  if TUnaryExpr(El).OpCode<>eopAddress then exit;
  if TUnaryExpr(El).Operand=nil then exit;
  if not (msDelphi in GetElModeSwitches(El)) then exit;
  ComputeElement(TUnaryExpr(El).Operand,OpResolved,[rcNoImplicitProc]);
  Result:=(OpResolved.BaseType=btContext)
      and (OpResolved.LoTypeEl is TPasProcedureType)
      and (OpResolved.IdentEl is TPasVariable)
      and not (OpResolved.IdentEl is TPasProperty);
end;


function TPasResolver.OverrideRootModule(El: TPasElement): TPasModule;
// The module that first DECLARED El, following an override chain to its root.
// An `override` does not narrow access: the visibility that counts is the one
// written on the original declaration, and that is where FPC checks it.
// fppkg's pkgoptions declares `protected class function GetKey; virtual` and
// calls it on a descendant whose override lives in another unit.
// Answers nil when there is no root above El, so it can only ever widen access.

var
  Proc: TPasProcedure;
  ProcScope: TPasProcedureScope;
  Ancestor: TPasClassType;
  AncType: TPasType;
  Root, Member: TPasElement;
  i: integer;
begin
  Result:=nil;
  if not (El is TPasProcedure) then exit;
  Proc:=TPasProcedure(El);
  if not Proc.IsOverride then exit;
  // The resolver's own link, when it is already set.
  while Proc.IsOverride and (Proc.CustomData is TPasProcedureScope) do
    begin
    ProcScope:=TPasProcedureScope(Proc.CustomData);
    if ProcScope.OverriddenProc=nil then break;
    Proc:=ProcScope.OverriddenProc;
    end;
  if Proc<>El then
    exit(Proc.GetModule);
  // OverriddenProc is NOT always set: two units that use each other are resolved
  // interleaved, and fppkg's descendant is bound while pkgoptions is still being
  // parsed. Walk the ancestor chain by name instead, and take the HIGHEST class
  // that declares it - that is the declaration whose visibility applies.
  if not (Proc.Parent is TPasClassType) then exit;
  Ancestor:=TPasClassType(Proc.Parent);
  Root:=nil;
  while Ancestor<>nil do
    begin
    if Ancestor.Members<>nil then
      for i:=0 to Ancestor.Members.Count-1 do
        begin
        Member:=TPasElement(Ancestor.Members[i]);
        if (Member is TPasProcedure) and SameText(Member.Name,Proc.Name) then
          begin
          Root:=Member;
          break;
          end;
        end;
    if not (Ancestor.CustomData is TPasClassScope) then break;
    AncType:=TPasClassScope(Ancestor.CustomData).DirectAncestor;
    if AncType=nil then break;
    AncType:=ResolveAliasType(AncType);
    if not (AncType is TPasClassType) then break;
    Ancestor:=TPasClassType(AncType);
    end;
  if (Root<>nil) and (Root<>El) then
    Result:=Root.GetModule;
end;


function TPasResolver.IsDefinitelyInaccessible(El: TPasElement): boolean;
// Mirrors the private/protected arms of CheckFoundElement, but only where they
// are unambiguous - it must never claim something is out of reach that the
// checker would accept.
var
  Context: TPasElement;
  FoundContext: TPasMembersType;
  CurScope: TPasScope;
begin
  Result:=false;
  if El=nil then exit;
  if not (El.Visibility in [visPrivate,visProtected]) then
    exit; // strict private/protected and everything else: leave alone
  if not (El.Parent is TPasMembersType) then exit;
  FoundContext:=TPasMembersType(El.Parent);
  Context:=GetVisibilityContext;
  if Context=nil then exit;
  if FoundContext.GetModule=Context.GetModule then
    exit; // same module -> reachable
  if IsInsideSpecialization or FInSpecialize then
    exit; // resolved inside a specialization body: checked where it was written
  if (El.Visibility=visProtected) then
    begin
    if (Context is TPasType)
        and (CheckClassIsClass(TPasType(Context),FoundContext)<>cIncompatible) then
      exit; // context is the class or a descendant
    CurScope:=TopScope;
    if (CurScope is TPasDotClassOrRecordScope)
        and (TPasDotClassOrRecordScope(CurScope).ClassRecScope.Element.GetModule=Context.GetModule) then
      exit;
    if (CurScope is TPasWithExprScope)
        and (TPasWithExprScope(CurScope).Scope.Element<>nil)
        and (TPasWithExprScope(CurScope).Scope.Element.GetModule=Context.GetModule) then
      exit;
    if (Context is TPasClassType)
        and (TPasClassType(Context).ObjKind in okAllHelpers) then
      exit; // a helper may reach the extended type's protected members
    if OverrideRootModule(El)=Context.GetModule then
      exit; // declared protected in THIS module; only the override is elsewhere
    end;
  Result:=true;
end;

function TPasResolver.GetRecordValuesMembersType(TypeEl: TPasType
  ): TPasMembersType;
// The type a `(name: value; ...)` typed constant may initialise: a record, or a
// Turbo Pascal OBJECT. fv's views.inc has `MinWinSize: TPoint = (X: 16; Y: 6)`
// over objects.pp's `TPoint = object`, which ppcx64 accepts. Anything else
// (a class, an interface) is nil.
begin
  Result:=nil;
  if TypeEl=nil then exit;
  if TypeEl.ClassType=TPasRecordType then
    Result:=TPasRecordType(TypeEl)
  else if (TypeEl.ClassType=TPasClassType)
      and (TPasClassType(TypeEl).ObjKind=okObject) then
    Result:=TPasMembersType(TypeEl);
end;

function TPasResolver.ObjectHasVMT(TypeEl: TPasType): boolean;
// True when a TP object has a VMT, i.e. it or an ancestor declares a
// virtual/dynamic method. ppcx64: "Constants of objects containing a VMT are
// not allowed".
var
  aClass: TPasClassType;
  i: Integer;
  Member: TPasElement;
begin
  Result:=false;
  while (TypeEl<>nil) and (TypeEl.ClassType=TPasClassType) do
    begin
    aClass:=TPasClassType(TypeEl);
    for i:=0 to aClass.Members.Count-1 do
      begin
      Member:=TPasElement(aClass.Members[i]);
      if (Member is TPasProcedure)
          and (TPasProcedure(Member).IsVirtual
               or TPasProcedure(Member).IsDynamic
               or TPasProcedure(Member).IsOverride) then
        exit(true);
      end;
    TypeEl:=ResolveAliasType(aClass.AncestorType);
    end;
end;

function TPasResolver.IsDynArray(TypeEl: TPasType; OptionalOpenArray: boolean
  ): boolean;
begin
  TypeEl:=ResolveAliasType(TypeEl);
  if (TypeEl=nil) or (TypeEl.ClassType<>TPasArrayType) then
    exit(false);
  if length(TPasArrayType(TypeEl).Ranges)<>0 then
    exit(false);
  // Note: Array of Const is an open array of TVarRec
  if OptionalOpenArray and (proOpenAsDynArrays in Options) then
    Result:=true
  else
    Result:=(TypeEl.Parent=nil) or (TypeEl.Parent.ClassType<>TPasArgument);
end;

function TPasResolver.IsOpenArray(TypeEl: TPasType): boolean;
begin
  Result:=(TypeEl<>nil)
      and (TypeEl.ClassType=TPasArrayType)
      and (length(TPasArrayType(TypeEl).Ranges)=0)
      and (TypeEl.Parent<>nil)
      and (TypeEl.Parent.ClassType=TPasArgument);
end;

function TPasResolver.IsDynOrOpenArray(TypeEl: TPasType): boolean;
begin
  TypeEl:=ResolveAliasType(TypeEl);
  Result:=(TypeEl<>nil) and (TypeEl.ClassType=TPasArrayType)
      and (length(TPasArrayType(TypeEl).Ranges)=0);
end;

function TPasResolver.IsArrayOfConst(TypeEl: TPasType): boolean;
begin
  Result:=(TypeEl<>nil) and (TypeEl.ClassType=TPasArrayType)
      and (TPasArrayType(TypeEl).ElType=nil);
end;

function TPasResolver.GetArrayElType(ArrType: TPasArrayType): TPasType;
begin
  Result:=ArrType.ElType;
  if Result=nil then
    Result:=GetTVarRec(ArrType);
end;

function TPasResolver.GetPartialArrayType(ArrType: TPasArrayType;
  ConsumedDims: Integer): TPasArrayType;
// Result type of a partial multi-dimensional index: a static array of the
// dimensions of ArrType after skipping ConsumedDims leading ranges, e.g.
// a[0] on array[0..2,0..2] of Integer -> array[0..2] of Integer.
// The synthesized type is resolver-owned (auto-freed at resolver destruction);
// its ElType and remaining Range expressions are plain references into ArrType
// (their Parent<>Result), so Result.FreeChildren won't free them.
var
  i: Integer;
begin
  Result:=TPasArrayType(CreateOwnedElement(TPasArrayType,'',ArrType.Parent));
  Result.ElType:=ArrType.ElType;
  Result.PackMode:=ArrType.PackMode;
  SetLength(Result.Ranges,length(ArrType.Ranges)-ConsumedDims);
  for i:=ConsumedDims to High(ArrType.Ranges) do
    Result.Ranges[i-ConsumedDims]:=ArrType.Ranges[i];
end;

function TPasResolver.SameArrayRanges(A, B: TPasArrayType): Boolean;
// True if A and B are static arrays with the same number of dimensions and
// the same index bounds (low/high) in every dimension. Used to decide
// structural compatibility of distinct static array type instances.

  function RangeBounds(E: TPasExpr; out Lo, Hi: TMaxPrecInt): Boolean;
  var
    R: TResEvalValue;
  begin
    Result:=False; Lo:=0; Hi:=0;
    R:=Eval(E,[refConst]);
    if R=nil then exit;
    try
      case R.Kind of
      revkRangeInt:
        begin
        Lo:=TResEvalRangeInt(R).RangeStart;
        Hi:=TResEvalRangeInt(R).RangeEnd;
        Result:=True;
        end;
      revkRangeUInt:
        begin
        Lo:=TMaxPrecInt(TResEvalRangeUInt(R).RangeStart);
        Hi:=TMaxPrecInt(TResEvalRangeUInt(R).RangeEnd);
        Result:=True;
        end;
      end;
    finally
      ReleaseEvalValue(R);
    end;
  end;

var
  i: Integer;
  LoA, HiA, LoB, HiB: TMaxPrecInt;
begin
  Result:=False;
  if (length(A.Ranges)=0) or (length(A.Ranges)<>length(B.Ranges)) then exit;
  for i:=0 to length(A.Ranges)-1 do
    begin
    if not RangeBounds(A.Ranges[i],LoA,HiA) then exit;
    if not RangeBounds(B.Ranges[i],LoB,HiB) then exit;
    if (LoA<>LoB) or (HiA<>HiB) then exit;
    end;
  Result:=True;
end;

function TPasResolver.IsVarInit(Expr: TPasExpr): boolean;
var
  C: TClass;
begin
  Result:=false;
  if Expr=nil then exit;
  if Expr.Parent=nil then exit;
  C:=Expr.Parent.ClassType;
  if C.InheritsFrom(TPasVariable) then
    Result:=(TPasVariable(Expr.Parent).Expr=Expr)
  else if C=TPasArgument then
    Result:=(TPasArgument(Expr.Parent).ValueExpr=Expr);
end;

function TPasResolver.IsEmptyArrayExpr(const ResolvedEl: TPasResolverResult): boolean;
begin
  Result:=(ResolvedEl.BaseType in [btSet,btArrayOrSet,btArrayLit])
      and (ResolvedEl.SubType=btNone);
end;

function TPasResolver.IsClassMethod(El: TPasElement): boolean;
var
  C: TClass;
begin
  if El=nil then exit(false);
  C:=El.ClassType;;
  Result:=(C=TPasClassConstructor)
       or (C=TPasClassDestructor)
       or (C=TPasClassProcedure)
       or (C=TPasClassFunction)
       or (C=TPasClassOperator);
end;

function TPasResolver.IsClassField(El: TPasElement): boolean;
var
  C: TClass;
begin
  if ((El.ClassType=TPasVariable) or (El.ClassType=TPasConst))
      and ([vmClass,vmStatic]*TPasVariable(El).VarModifiers<>[]) then
    begin
    C:=El.Parent.ClassType;
    Result:=(C=TPasClassType) or (C=TPasRecordType);
    end
  else
    Result:=false;
end;

function TPasResolver.GetFunctionType(El: TPasElement): TPasFunctionType;
var
  ProcType: TPasProcedureType;
begin
  if not (El is TPasProcedure) then exit(nil);
  ProcType:=TPasProcedure(El).ProcType;
  if ProcType is TPasFunctionType then
    Result:=TPasFunctionType(ProcType)
  else
    Result:=nil;
end;

function TPasResolver.MethodIsStatic(El: TPasProcedure): boolean;
begin
  Result:=El.IsStatic
    or (El.ClassType=TPasClassConstructor)
    or (El.ClassType=TPasClassDestructor);
end;

function TPasResolver.IsMethod(El: TPasProcedure): boolean;
var
  ProcScope: TPasProcedureScope;
begin
  Result:=false;
  if El=nil then exit;
  if El.Parent is TPasMembersType then exit(true);
  if not (El.CustomData is TPasProcedureScope) then exit;
  ProcScope:=TPasProcedureScope(El.CustomData);
  Result:=IsMethod(ProcScope.DeclarationProc);
end;

function TPasResolver.IsMethod_SelfIsClass(El: TPasElement): boolean;
var
  C: TClass;
begin
  if (El=nil) then exit(false);
  C:=El.ClassType;
  Result:=((C=TPasClassProcedure) or (C=TPasClassFunction) or (C=TPasClassOperator))
      and not TPasProcedure(El).IsStatic;
end;

function TPasResolver.IsHelperMethod(El: TPasElement): boolean;
begin
  Result:=(El is TPasProcedure) and (El.Parent is TPasClassType)
          and (TPasClassType(El.Parent).HelperForType<>nil);
end;

function TPasResolver.IsHelper(El: TPasElement): boolean;
begin
  Result:=(El<>nil) and (El.ClassType=TPasClassType) and (TPasClassType(El).HelperForType<>nil);
end;

function TPasResolver.IsExternalClass_Name(aClass: TPasClassType;
  const ExtName: string): boolean;
var
  AncestorScope: TPasClassScope;
begin
  Result:=false;
  if aClass=nil then exit;
  while aClass<>nil do
    begin
    if aClass.ExternalName=ExtName then exit(true);
    AncestorScope:=(aClass.CustomData as TPasClassScope).AncestorScope;
    if AncestorScope=nil then exit;
    aClass:=NoNil(AncestorScope.Element) as TPasClassType;
    end;
end;

function TPasResolver.IsProcedureType(const ResolvedEl: TPasResolverResult;
  HasValue: boolean): boolean;
var
  TypeEl: TPasType;
begin
  if (ResolvedEl.BaseType<>btContext) then
    exit(false);
  TypeEl:=ResolvedEl.LoTypeEl;
  if not (TypeEl is TPasProcedureType) then
    exit(false);
  if HasValue and not (rrfReadable in ResolvedEl.Flags) then
    exit(false);
  Result:=true;
end;

function TPasResolver.IsArrayType(const ResolvedEl: TPasResolverResult
  ): boolean;
begin
  Result:=(ResolvedEl.BaseType=btContext) and (ResolvedEl.LoTypeEl is TPasArrayType);
end;

function TPasResolver.IsArrayExpr(Expr: TParamsExpr): TPasArrayType;
var
  Ref: TResolvedReference;
begin
  Result:=nil;
  if Expr=nil then exit;
  if Expr.Kind<>pekSet then exit;
  if not (Expr.CustomData is TResolvedReference) then exit;
  Ref:=TResolvedReference(Expr.CustomData);
  if Ref.Declaration is TPasArrayType then
    Result:=TPasArrayType(Ref.Declaration);
end;

function TPasResolver.IsArrayOperatorAdd(Expr: TPasExpr): boolean;
begin
  Result:=(Expr<>nil) and (Expr.ClassType=TBinaryExpr) and (Expr.OpCode=eopAdd)
          and ElHasModeSwitch(Expr,msArrayOperators);
end;

function TPasResolver.GetTypeOfOperandType(Operand: TPasExpr;
  ErrorEl: TPasElement): TPasType;
// returns the type of the already resolved operand of "type of"
var
  ResolvedEl: TPasResolverResult;
begin
  ComputeElement(Operand,ResolvedEl,[]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.GetTypeOfOperandType ',GetObjName(Operand),' ',GetResolverResultDbg(ResolvedEl));
  {$ENDIF}
  if (ResolvedEl.IdentEl is TPasType) and not (rrfReadable in ResolvedEl.Flags)
      and IsTypeOfTypeRefOperand(Operand) then
    // "type of TypeIdentifier"
    RaiseMsg(20260922100020,nXExpectedButYFound,sXExpectedButYFound,
      ['expression','type'],ErrorEl);
  Result:=ResolvedEl.HiTypeEl;
  if (Result=nil) and (ResolvedEl.BaseType in
      btAllInteger+btAllFloats+btAllBooleans+btAllStringAndChars+[btPointer,btVariant]) then
    Result:=FBaseTypes[ResolvedEl.BaseType];
  if (Result=nil)
      or ((ResolvedEl.BaseType=btProc) and (ResolvedEl.IdentEl is TPasProcedure)
        and not (rrfReadable in ResolvedEl.Flags)) then
    // e.g. "type of nil", "type of [1,2]", "type of SomeProcedure"
    RaiseMsg(20260922100021,nIllegalExpression,sIllegalExpression,[],ErrorEl);
end;

function TPasResolver.IsTypeOfTypeRefOperand(Operand: TPasExpr): boolean;
// true if Operand is a plain type reference, e.g. "Integer", "System.Integer" or "TList<Word>",
// false for type expressions, e.g. "S+T" of type parameters
var
  C: TClass;
begin
  C:=Operand.ClassType;
  if C=TPrimitiveExpr then
    Result:=TPrimitiveExpr(Operand).Kind=pekIdent
  else if C=TBinaryExpr then
    Result:=TBinaryExpr(Operand).OpCode=eopSubIdent
  else if C=TInlineSpecializeExpr then
    Result:=true
  else
    Result:=false;
end;

function TPasResolver.GetTypeOfCastRoot(Params: TParamsExpr): TUnaryExpr;
// returns the "type of" expression, if Params is the typecast of a "type of",
// e.g. "type of a(b)", "type of r.a(b).c", "type of a[1](b)^"
var
  Child, Parent, El: TPasExpr;
  C: TClass;
  Ref: TResolvedReference;
begin
  Result:=nil;
  if (Params=nil) or (Params.Kind<>pekFuncParams) then exit;
  // the first typecast in the operand chain only
  El:=Params.Value;
  while El<>nil do
    begin
    Ref:=nil;
    if El.CustomData is TResolvedReference then
      Ref:=TResolvedReference(El.CustomData);
    if (Ref<>nil) and (rrfTypeOfCast in Ref.Flags) then
      exit;
    C:=El.ClassType;
    if (C=TBinaryExpr) and (TBinaryExpr(El).OpCode=eopSubIdent) then
      El:=TBinaryExpr(El).Left
    else if C=TParamsExpr then
      El:=TParamsExpr(El).Value
    else if (C=TUnaryExpr) and (TUnaryExpr(El).OpCode=eopDeref) then
      El:=TUnaryExpr(El).Operand
    else
      break;
    end;
  // search the "type of" in the parents
  Child:=Params;
  while Child.Parent is TPasExpr do
    begin
    Parent:=TPasExpr(Child.Parent);
    C:=Parent.ClassType;
    if C=TUnaryExpr then
      begin
      if TUnaryExpr(Parent).Operand<>Child then exit;
      case TUnaryExpr(Parent).OpCode of
      eopTypeOf: exit(TUnaryExpr(Parent));
      eopDeref: ;
      else exit;
      end;
      end
    else if C=TBinaryExpr then
      begin
      if (TBinaryExpr(Parent).OpCode<>eopSubIdent)
          or (TBinaryExpr(Parent).Left<>Child) then exit;
      end
    else if C=TParamsExpr then
      begin
      if TParamsExpr(Parent).Value<>Child then exit;
      end
    else
      exit;
    Child:=Parent;
    end;
end;

function TPasResolver.GetTypeOfCastValueType(Value: TPasElement): TPasType;
// returns the type of Value, if Value can be used in "type of Value(Param)"
var
  ResolvedEl: TPasResolverResult;
begin
  Result:=nil;
  if not ((Value is TPasVariable) or (Value is TPasArgument)
      or (Value is TPasResultElement) or (Value is TPasEnumValue)) then
    exit;
  ComputeElement(Value,ResolvedEl,[rcNoImplicitProc]);
  if IsProcedureType(ResolvedEl,true) then exit;
  if not (rrfReadable in ResolvedEl.Flags) then exit;
  Result:=ResolvedEl.HiTypeEl;
  if (Result<>nil) and (GetFuncRefInvokeProcType(Result)<>nil) then
    Result:=nil;
end;

function TPasResolver.IsTypeOfCastExpr(El: TUnaryExpr): boolean;
// true if El is a "type of" typecast, e.g. "type of a(b)"
var
  Expr: TPasExpr;
  C: TClass;
begin
  Result:=false;
  if El.OpCode<>eopTypeOf then exit;
  Expr:=El.Operand;
  while Expr<>nil do
    begin
    C:=Expr.ClassType;
    if C=TParamsExpr then
      begin
      if (TParamsExpr(Expr).Kind=pekFuncParams)
          and (GetParamsValueRef(TParamsExpr(Expr))<>nil)
          and (rrfTypeOfCast in GetParamsValueRef(TParamsExpr(Expr)).Flags) then
        exit(true);
      Expr:=TParamsExpr(Expr).Value;
      end
    else if (C=TBinaryExpr) and (TBinaryExpr(Expr).OpCode=eopSubIdent) then
      Expr:=TBinaryExpr(Expr).Left
    else if (C=TUnaryExpr) and (TUnaryExpr(Expr).OpCode=eopDeref) then
      Expr:=TUnaryExpr(Expr).Operand
    else
      exit;
    end;
end;

procedure TPasResolver.ComputeTypeOfExpr(El: TUnaryExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
begin
  if El.CustomData is TResolvedReference then
    // "type of a" -> a type
    ComputeElement(TResolvedReference(El.CustomData).Declaration,ResolvedEl,
      Flags+[rcNoImplicitProc],StartEl)
  else if IsTypeOfCastExpr(El) then
    // "type of a(b)" -> a value
    ComputeElement(El.Operand,ResolvedEl,Flags,StartEl)
  else
    RaiseNotYetImplemented(20260922100030,El);
end;

function TPasResolver.IsTypeCast(Params: TParamsExpr): boolean;
var
  Value: TPasExpr;
  Ref: TResolvedReference;
  Decl: TPasElement;
  C: TClass;
begin
  Result:=false;
  if (Params=nil) or (Params.Kind<>pekFuncParams) then exit;
  Value:=Params.Value;
  if not (Value.CustomData is TResolvedReference) then exit;
  Ref:=TResolvedReference(Value.CustomData);
  if not IsNameExpr(Value)
      and not (rrfTypeOfCast in Ref.Flags)
      and not ((Value.ClassType=TUnaryExpr) and (TUnaryExpr(Value).OpCode=eopTypeOf)) then
    exit;
  Decl:=Ref.Declaration;
  C:=Decl.ClassType;
  if (C=TPasAliasType) or (C=TPasTypeAliasType) or (C=TPasTypeOfType) then
    begin
    Decl:=ResolveAliasType(TPasType(Decl));
    C:=Decl.ClassType;
    end;
  if (C=TPasProcedureType)
      or (C=TPasFunctionType) then
    exit(true)
  else if (C=TPasClassType)
      or (C=TPasClassOfType)
      or (C=TPasEnumType)
      or (C=TPasRecordType)
      or (C=TPasArrayType)
      or (C=TPasSpecializeType)
      or (C=TPasGenericTemplateType) then
    exit(true)
  else if (C=TPasUnresolvedSymbolRef)
      and (Decl.CustomData is TResElDataBaseType) then
    exit(true);
end;

function TPasResolver.GetTypeParameterCount(aType: TPasGenericType): integer;
begin
  if aType=nil then exit(0);
  if aType.GenericTemplateTypes=nil then exit(0);
  Result:=aType.GenericTemplateTypes.Count;
end;

function TPasResolver.GetGenericConstraintKeyword(El: TPasElement): TToken;
var
  Prim: TPrimitiveExpr;
begin
  if (El=nil) or (El.ClassType<>TPrimitiveExpr) then
    exit(tkEOF);
  Prim:=TPrimitiveExpr(El);
  if Prim.Kind<>pekIdent then
    exit(tkEOF);
  case lowercase(Prim.Value) of
  'record': Result:=tkrecord;
  'class': Result:=tkclass;
  'constructor': Result:=tkconstructor;
  else Result:=tkEOF;
  end;
end;

function TPasResolver.HasClassConstraint(
  TemplType: TPasGenericTemplateType): Boolean;
// True if the generic template type has a "class" constraint (T: class).
var
  i: Integer;
begin
  Result:=false;
  for i:=0 to length(TemplType.Constraints)-1 do
    if GetGenericConstraintKeyword(TemplType.Constraints[i])=tkclass then
      exit(true);
end;


function TPasResolver.HasClassTypeConstraint(
  TemplType: TPasGenericTemplateType): Boolean;
// True if the generic template type is constrained to a concrete class type
// (T: TSomeClass). Such a T is guaranteed to be a class after specialization,
// so it may be used as an ancestor (class(T)) just like a "class"-keyword
// constraint.
var
  i: Integer;
  TypeRes: TPasResolverResult;
begin
  Result:=false;
  for i:=0 to length(TemplType.Constraints)-1 do
    if TemplType.Constraints[i] is TPasType then
      begin
      ComputeElement(TPasType(TemplType.Constraints[i]),TypeRes,[rcType]);
      if (TypeRes.LoTypeEl is TPasClassType)
          and (TPasClassType(TypeRes.LoTypeEl).ObjKind=okClass) then
        exit(true);
      end;
end;

function TPasResolver.GetClassTypeConstraint(
  TemplType: TPasGenericTemplateType): TPasClassType;
// The class a template type is constrained to (T: TSomeClass), nil if none.
var
  i: Integer;
  TypeRes: TPasResolverResult;
begin
  Result:=nil;
  for i:=0 to length(TemplType.Constraints)-1 do
    if TemplType.Constraints[i] is TPasType then
      begin
      ComputeElement(TPasType(TemplType.Constraints[i]),TypeRes,[rcType]);
      if (TypeRes.LoTypeEl is TPasClassType)
          and (TPasClassType(TypeRes.LoTypeEl).ObjKind=okClass) then
        exit(TPasClassType(TypeRes.LoTypeEl));
      end;
end;

function TPasResolver.HasRecordConstraint(
  TemplType: TPasGenericTemplateType): Boolean;
// True if the generic template type has a "record" constraint (T: record).
var
  i: Integer;
begin
  Result:=false;
  for i:=0 to length(TemplType.Constraints)-1 do
    if GetGenericConstraintKeyword(TemplType.Constraints[i])=tkrecord then
      exit(true);
end;

function TPasResolver.GetGenericConstraintErrorEl(ConstraintEl,
  TemplType: TPasElement): TPasElement;
begin
  if (ConstraintEl is TPasExpr) or (ConstraintEl.Parent=TemplType) then
    Result:=ConstraintEl
  else
    Result:=TemplType;
end;

function TPasResolver.GetSpecializeParamAsType(Param: TPasElement): TPasType;
// A specialize argument parsed by ReadSpecializeArguments may be a TPasType
// directly, or a TPasExpr whose CustomData is a TResolvedReference to the type.
var
  Ref: TResolvedReference;
begin
  if Param is TPasType then
    Result:=TPasType(Param)
  else if Param is TPasExpr then
    begin
    if (Param.CustomData<>nil) and (Param.CustomData is TResolvedReference) then
      begin
      Ref:=TResolvedReference(Param.CustomData);
      if Ref.Declaration is TPasType then
        Result:=TPasType(Ref.Declaration)
      else
        Result:=nil;
      end
    else
      Result:=nil;
    end
  else
    Result:=nil;
end;

function TPasResolver.GetSpecializedEl(El: TPasElement; GenericEl: TPasElement;
  Params: TFPList): TPasElement;
var
  Data: TPasSpecializeTypeData;
  GenScope: TPasGenericScope;
  GenericTemplateList: TFPList;
  i, j: Integer;
  Param: TPasElement;
  ParamsResolved: TPasTypeArray;
  ConstExprsResolved: array of TPasExpr;
  ResolvedEl: TPasResolverResult;
  SpecializedElList: TObjectList;
  Item: TPRSpecializedItem;
  SrcModule: TPasModule;
  SrcModuleScope: TPasModuleScope;
  SrcResolver: TPasResolver;
  IsSelf: Boolean;
  GenericType: TPasGenericType;
  GenericProc: TPasProcedure;
  ProcScope: TPasProcedureScope;
  TemplType: TPasGenericTemplateType;
  Val1, Val2: TResEvalValue;
  Match: Boolean;
  ConstRangeValue, ConstArgValue: TResEvalValue;
  AnnotationType: TPasType;
  AnnotationResolved: TPasResolverResult;
  RangeMinVal, RangeMaxVal: TMaxPrecInt;
begin
  Result:=nil;
  if (El.ClassType=TPasSpecializeType) and (El.CustomData<>nil) then
    RaiseNotYetImplemented(20190726142522,El);

  // Specializing a procedural (proc-var) type that has a const generic parameter
  // is not supported (FPC rejects this — timpfuncspez15).
  if GenericEl is TPasProcedureType then
    begin
    GenericTemplateList:=TPasGenericType(GenericEl).GenericTemplateTypes;
    if GenericTemplateList<>nil then
      for i:=0 to GenericTemplateList.Count-1 do
        if (TObject(GenericTemplateList[i]) is TPasGenericTemplateType)
            and TPasGenericTemplateType(GenericTemplateList[i]).IsConst then
          RaiseMsg(20260606120000,nXIsNotSupported,sXIsNotSupported,
            ['specializing a procedural type with a const generic parameter'],El);
    end;

  // check if there is already such a specialization
  GenScope:=nil;
  GenericType:=nil;
  GenericProc:=nil;
  if GenericEl is TPasGenericType then
    begin
    GenericType:=TPasGenericType(GenericEl);
    if not (GenericEl.CustomData is TPasGenericScope) then
      RaiseMsg(20190726194316,nTypeXIsNotYetCompletelyDefined,sTypeXIsNotYetCompletelyDefined,
        [GetTypeDescription(GenericType)],El);
    GenScope:=TPasGenericScope(GenericEl.CustomData);

    // A generic `reference to` funcref may reference its own (yet-unfinished)
    // specialization in its result — e.g.
    // M<T> = reference to function(const A: T): M<T> (tanonfunc40/41/67).
    // A generic record may also self-reference in a method signature (a record
    // method returning its own type, tgeneric76) — but only on a native/FPC
    // target: the base rejects it (TestGen_Record_ReferGenericSelfFail), so the
    // record exception is gated on the AllowRecordGenericSelfReference seam.
    if (not (GenericType is TPasClassType))
        and (not ((GenericType is TPasProcedureType) and TPasProcedureType(GenericType).IsReferenceTo))
        and (not ((GenericType is TPasRecordType) and AllowRecordGenericSelfReference))
        and (GenScope.GenericStep<psgsInterfaceParsed) then
      RaiseMsg(20190807205038,nTypeXIsNotYetCompletelyDefined,sTypeXIsNotYetCompletelyDefined,
        [GetTypeDescription(GenericType)],El);
    GenericTemplateList:=GenericType.GenericTemplateTypes;
    end
  else if GenericEl is TPasProcedure then
    begin
    GenericProc:=TPasProcedure(GenericEl);
    if not (GenericProc.CustomData is TPasProcedureScope) then
      RaiseMsg(20190919132733,nIdentifierNotFound,sIdentifierNotFound,
        [GenericProc.Name],El);
    ProcScope:=TPasProcedureScope(GenericProc.CustomData);
    if ProcScope.DeclarationProc<>nil then
      RaiseNotYetImplemented(20190920182602,El);
    GenScope:=ProcScope;

    if GenScope.GenericStep<psgsInterfaceParsed then
      RaiseMsg(20190920120649,nTypeXIsNotYetCompletelyDefined,sTypeXIsNotYetCompletelyDefined,
        [GetElementDbgPath(GenericProc)],El);
    GenericTemplateList:=GetProcTemplateTypes(GenericProc);
    end
  else
    RaiseNotYetImplemented(20190919132603,GenericEl);

  SpecializedElList:=GenScope.SpecializedItems;
  if GenericTemplateList=nil then
    RaiseMsg(20190905111703,nXExpectedButYFound,sXExpectedButYFound,
      ['generic templates',GenericEl.Name],El);
  if GenericTemplateList.Count<>Params.Count then
    RaiseMsg(20190905111704,nXExpectedButYFound,sXExpectedButYFound,
      ['type with '+IntToStr(Params.Count)+' generic template(s)',
       GenericEl.Name+GetGenericParamCommas(GenericTemplateList.Count)],El);

  SetLength(ParamsResolved{%H-},Params.Count);
  SetLength(ConstExprsResolved{%H-},Params.Count);
  IsSelf:=true;
  for i:=0 to Params.Count-1 do
    begin
    Param:=TPasElement(Params[i]);
    TemplType:=TPasGenericTemplateType(GenericTemplateList[i]);
    if TemplType.IsConst then
      begin
      // Const generic param: resolve as expression
      if Param is TPasExpr then
        begin
        ComputeElement(Param,ResolvedEl,[rcConstant]);
        ParamsResolved[i]:=ResolvedEl.LoTypeEl;
        ConstExprsResolved[i]:=TPasExpr(Param);
        end
      else
        begin
        // Identifier parsed as type - try resolving as constant value
        ComputeElement(Param,ResolvedEl,[]);
        // Forwarding another const template param (generic TList<T;const U> ...
        // specialize THelperA<U>): a const param computes with its value-type in
        // LoTypeEl (integer) but the param itself is in IdentEl. Keep the
        // template param as the argument so THelperA<U> stays deferred (U bound
        // when TList is specialized), not collapsed to "THelperA<integer>".
        if (ResolvedEl.IdentEl is TPasGenericTemplateType)
            and TPasGenericTemplateType(ResolvedEl.IdentEl).IsConst then
          begin
          ParamsResolved[i]:=TPasType(ResolvedEl.IdentEl);
          ConstExprsResolved[i]:=nil;
          end
        else
          begin
          ParamsResolved[i]:=ResolvedEl.LoTypeEl;
          if ResolvedEl.IdentEl is TPasConst then
            ConstExprsResolved[i]:=TPasConst(ResolvedEl.IdentEl).Expr
          else
            ConstExprsResolved[i]:=nil;
          end;
        end;
      // Step 5c: validate const value against type annotation
      if (ConstExprsResolved[i]<>nil)
          and (length(TemplType.Constraints)>0) then
        begin
        ComputeElement(TemplType.Constraints[0],AnnotationResolved,[rcType]);
        ConstArgValue:=Eval(ConstExprsResolved[i],[refConst]);
        if ConstArgValue<>nil then
          try
            AnnotationType:=AnnotationResolved.LoTypeEl;
            if AnnotationType is TPasRangeType then
              begin
              ConstRangeValue:=Eval(TPasRangeType(AnnotationType).RangeExpr,[refConst]);
              try
                if not fExprEvaluator.IsInRange(ConstArgValue,
                    ConstExprsResolved[i],ConstRangeValue,
                    TPasRangeType(AnnotationType).RangeExpr,false) then
                  RaiseRangeCheck(20260304200002,ConstExprsResolved[i]);
              finally
                ReleaseEvalValue(ConstRangeValue);
              end;
              end
            else if (AnnotationResolved.BaseType in btAllIntegerNoQWord)
                and GetIntegerRange(AnnotationResolved.BaseType,RangeMinVal,RangeMaxVal) then
              begin
              case ConstArgValue.Kind of
              revkInt:
                if (RangeMinVal>TResEvalInt(ConstArgValue).Int)
                    or (RangeMaxVal<TResEvalInt(ConstArgValue).Int) then
                  RaiseRangeCheck(20260304200003,ConstExprsResolved[i]);
              revkUInt:
                if (TResEvalUInt(ConstArgValue).UInt>High(TMaxPrecInt))
                    or (RangeMinVal>TMaxPrecInt(TResEvalUInt(ConstArgValue).UInt))
                    or (RangeMaxVal<TMaxPrecInt(TResEvalUInt(ConstArgValue).UInt)) then
                  RaiseRangeCheck(20260304200004,ConstExprsResolved[i]);
              else
                // a non-integer constant (e.g. a string literal) cannot specialize
                // an integer-typed const generic param (tgenconst7).
                RaiseMsg(20260304200005,nIncompatibleTypesGotExpected,
                  sIncompatibleTypesGotExpected,
                  ['constant',BaseTypeNames[AnnotationResolved.BaseType]],ConstExprsResolved[i]);
              end;
              end;
          finally
            ReleaseEvalValue(ConstArgValue);
          end;
        end;
      IsSelf:=false;
      end
    else
      begin
      // Type param
      if Param is TPasExpr then
        begin
        // Identifier parsed as expression by ReadSpecializeArguments —
        // check if it resolves to a type (e.g. specialize TList<Integer>, or the
        // record's own template T in a self-reference tgeneric76).
        ComputeElement(Param,ResolvedEl,[rcType]);
        if (ResolvedEl.LoTypeEl=nil) or not (ResolvedEl.IdentEl is TPasType) then
          RaiseMsg(20260304120001,nXExpectedButYFound,sXExpectedButYFound,
            ['type','constant expression'],El);
        end
      else
        ComputeElement(Param,ResolvedEl,[rcType]);
      // A reference to a SET type resolves with its ELEMENT type in LoTypeEl
      // (that is what set operations need), while the set type itself is in
      // IdentEl. For a generic type argument we want the set type as written,
      // so prefer IdentEl when it denotes a set — otherwise "TArr<TSet>" would
      // specialize to "array of <enum>" instead of "array of TSet".
      if (ResolvedEl.BaseType=btSet) and (ResolvedEl.IdentEl is TPasType)
          and (ResolveAliasType(TPasType(ResolvedEl.IdentEl)) is TPasSetType) then
        ParamsResolved[i]:=TPasType(ResolvedEl.IdentEl)
      else
        ParamsResolved[i]:=ResolvedEl.LoTypeEl;
      ConstExprsResolved[i]:=nil;
      if ParamsResolved[i]<>TPasType(GenericTemplateList[i]) then
        IsSelf:=false;
      end;
    end;
  if IsSelf then
    exit(GenericEl);

  // The same answer when an argument is merely UNDECIDED rather than the
  // generic's own parameter - see PartialSpecsAsGeneric.
  if FPartialSpecsAsGeneric then
    for i:=0 to length(ParamsResolved)-1 do
      if (ConstExprsResolved[i]=nil) and ParamIsUndecided(ParamsResolved[i]) then
        exit(GenericEl);

  if SpecializedElList=nil then
    begin
    SpecializedElList:=TObjectList.Create(true);
    if GenScope<>nil then
      GenScope.SpecializedItems:=SpecializedElList
    else
      RaiseNotYetImplemented(20190919133159,El);
    end;
  i:=SpecializedElList.Count-1;
  Item:=nil;
  while i>=0 do
    begin
    Item:=TPRSpecializedItem(SpecializedElList[i]);
    j:=length(Item.Params)-1;
    while j>=0 do
      begin
      TemplType:=TPasGenericTemplateType(GenericTemplateList[j]);
      if TemplType.IsConst then
        begin
        // Compare const values
        if (j<length(Item.ConstExprs)) and (Item.ConstExprs[j]<>nil)
            and (ConstExprsResolved[j]<>nil) then
          begin
          Val1:=Eval(Item.ConstExprs[j],[refConst]);
          Val2:=Eval(ConstExprsResolved[j],[refConst]);
          try
            Match:=false;
            if (Val1<>nil) and (Val2<>nil) and (Val1.Kind=Val2.Kind) then
              case Val1.Kind of
                revkBool: Match:=TResEvalBool(Val1).B=TResEvalBool(Val2).B;
                revkInt: Match:=TResEvalInt(Val1).Int=TResEvalInt(Val2).Int;
                revkUInt: Match:=TResEvalUInt(Val1).UInt=TResEvalUInt(Val2).UInt;
                revkFloat: Match:=TResEvalFloat(Val1).FloatValue=TResEvalFloat(Val2).FloatValue;
                {$ifdef FPC_HAS_CPSTRING}
                revkString: Match:=TResEvalString(Val1).S=TResEvalString(Val2).S;
                {$endif}
                revkUnicodeString: Match:=TResEvalUTF16(Val1).S=TResEvalUTF16(Val2).S;
                revkEnum: Match:=TResEvalEnum(Val1).Index=TResEvalEnum(Val2).Index;
              end;
            if not Match then break;
          finally
            ReleaseEvalValue(Val1);
            ReleaseEvalValue(Val2);
          end;
          end
        else
          break; // mismatch (one has const expr, other doesn't)
        end
      else
        begin
        // Type param comparison. Two types DECLARED SIDE BY SIDE IN THEIR OWN
        // UNITS are two specializations even when their structures are
        // interchangeable: accepting a merely COMPATIBLE one let the second
        // reuse the first's specialization, so the `TypeInfo(T)` inside it named
        // the wrong type - two units each declaring
        // `TIntegerArray = array of Integer` shared one AsType<T>, and the JSON
        // serializer's Deserialize<T> raised EInvalidCast on a value whose type
        // info was the other unit's. Everything without that identity keeps the
        // compatibility rule: an ANONYMOUS type (`array of Word` written inline
        // twice), a still-unbound TEMPLATE parameter, a type declared INSIDE a
        // generic (generics.collections' `PT = ^T`, one per specialization), and
        // a specialized type itself.
        if not IsSameType(Item.Params[j],ParamsResolved[j],prraAlias) then
          begin
          if (Item.Params[j].Name<>'') and (ParamsResolved[j].Name<>'')
              and (Item.Params[j].Parent is TPasSection)
              and (ParamsResolved[j].Parent is TPasSection)
              and not (Item.Params[j] is TPasGenericTemplateType)
              and not (ParamsResolved[j] is TPasGenericTemplateType)
              and not IsSpecializedTypeEl(Item.Params[j])
              and not IsSpecializedTypeEl(ParamsResolved[j]) then
            break
          else if CheckElTypeCompatibility(Item.Params[j],ParamsResolved[j],prraNone)>cExact then
            break;
          end;
        end;
      dec(j);
      end;
    if j<0 then
      break;
    Item:=nil;
    dec(i);
    end;
  if Item=nil then
    begin
    // new specialization
    SrcModule:=GenericEl.GetModule;
    SrcModuleScope:=SrcModule.CustomData as TPasModuleScope;
    SrcResolver:=SrcModuleScope.Owner as TPasResolver;
    Item:=SrcResolver.CreateSpecializedItem(El,GenericEl,ParamsResolved,ConstExprsResolved);
    end;

  Result:=Item.SpecializedEl;

  if El.ClassType=TPasSpecializeType then
    begin
    Data:=TPasSpecializeTypeData.Create;
    // add to free list
    AddResolveData(El,Data,lkModule);
    Data.SpecializedType:=Result as TPasGenericType; // no AddRef
    end;
end;

function TPasResolver.ResolverOfElement(El: TPasElement): TPasResolver;
// The resolver of the module an element belongs to.
var
  aModule: TPasModule;
begin
  Result:=nil;
  if El=nil then
    exit;
  aModule:=El.GetModule;
  if (aModule=nil) or not (aModule.CustomData is TPasModuleScope) then
    exit;
  Result:=TPasModuleScope(aModule.CustomData).Owner as TPasResolver;
end;

procedure TPasResolver.RegisterSpecializedItem(El: TPasElement;
  Item: TPRSpecializedItem);
// Index a specialized element on the resolver that OWNS it - the item is
// created by the resolver of the GENERIC's module, but the element itself can
// be parented in the consumer's, and a later lookup asks the owner.
var
  Res: TPasResolver;
begin
  Res:=ResolverOfElement(El);
  if Res=nil then
    Res:=Self;
  if Res.FSpecItemsByEl=nil then
    Res.FSpecItemsByEl:=TFPList.Create;
  Res.FSpecItemsByEl.Add(El);
  Res.FSpecItemsByEl.Add(Item);
  if Res<>Self then
    begin
    if FSpecItemsByEl=nil then
      FSpecItemsByEl:=TFPList.Create;
    FSpecItemsByEl.Add(El);
    FSpecItemsByEl.Add(Item);
    end;
end;

function TPasResolver.IsSpecializedTypeEl(El: TPasElement): boolean;
// True when El is itself the product of a specialization. Such a type has no
// stable identity across the places it is built, so two of them are compared by
// structure, not by element.
begin
  Result:=(El<>nil) and (El.CustomData is TPasGenericScope)
      and (TPasGenericScope(El.CustomData).SpecializedFromItem<>nil);
end;


function TPasResolver.FindSpecializedItemOfEl(El: TPasElement
  ): TPRSpecializedItem;
// The item a specialized element was created from, so an element whose
// interface was never built can still be finished.
var
  i: Integer;
begin
  Result:=nil;
  if (El=nil) or (FSpecItemsByEl=nil) then
    exit;
  i:=0;
  while i+1<FSpecItemsByEl.Count do
    begin
    if TPasElement(FSpecItemsByEl[i])=El then
      exit(TPRSpecializedItem(FSpecItemsByEl[i+1]));
    inc(i,2);
    end;
end;

procedure TPasResolver.FinishGenericClassOrRecIntf(Scope: TPasGenericScope);
var
  El: TPasGenericType;
  SpecializedItems: TObjectList;
  i: Integer;
  SpecializedItem: TPRSpecializedTypeItem;
  OldScopeState: TScopeStashState;
begin
  El:=Scope.Element as TPasGenericType;
  if Scope.GenericStep<>psgsNone then
    RaiseNotYetImplemented(20200219124544,El);
  Scope.GenericStep:=psgsInterfaceParsed;
  SpecializedItems:=Scope.SpecializedItems;
  if SpecializedItems<>nil then
    // finish interfaces of started specializations
    for i:=0 to SpecializedItems.Count-1 do
      begin
      SpecializedItem:=TPRSpecializedTypeItem(SpecializedItems[i]);
      SpecializedItem.GenericEl:=El;
      if SpecializedItem.Step<>prssNone then continue;
      InitSpecializeScopes(El,OldScopeState);
      {$IFDEF VerbosePasResolver}
      WriteScopesShort('TPasResolver.FinishSpecializedClassOrRecIntf Finishing specialize interface: '+GetObjName(SpecializedItem.SpecializedType));
      {$ENDIF}
      SpecializeGenericIntf(SpecializedItem);

      {$IFDEF VerbosePasResolver}
      WriteScopesShort('TPasResolver.FinishSpecializedClassOrRecIntf Finished specialize interface: '+GetObjName(SpecializedItem.SpecializedType));
      {$ENDIF}

      RestoreSpecializeScopes(OldScopeState);
      {$IFDEF VerbosePasResolver}
      WriteScopesShort('TPasResolver.FinishSpecializedClassOrRecIntf RestoreStashedScopes '+GetObjName(SpecializedItem.SpecializedType));
      {$ENDIF}
      end;
end;

procedure TPasResolver.FinishSpecializations(Scope: TPasGenericScope);
// The generic's implementation has been parsed, so its specializations COULD
// have their bodies built now. They are only owed: see CreateSpecializedItem.
var
  SpecializedItems: TObjectList;
  i: Integer;
begin
  SpecializedItems:=Scope.SpecializedItems;
  if SpecializedItems=nil then exit;
  for i:=0 to SpecializedItems.Count-1 do
    if not TPRSpecializedItem(SpecializedItems[i]).ImplOwed then
      SpecializeGenericImpl(TPRSpecializedItem(SpecializedItems[i]));
end;

procedure TPasResolver.CheckPendingForwardTypes(El: TPasElement);
// The ObjFPC half of the forward-TYPE check deferred by FinishTypeSectionEl: by
// now every type section of this declaration section has been seen. Unlike the
// forward-PROC check this also runs under interface-only resolution — a forward
// declared in an interface has to be completed there, never in the (unparsed)
// implementation.
var
  i: Integer;
  DeclEl: TPasElement;
begin
  if IsElementSkipped(El) then exit;
  if not (El is TPasDeclarations) then exit;
  for i:=0 to TPasDeclarations(El).Declarations.Count-1 do
    begin
    DeclEl:=TPasElement(TPasDeclarations(El).Declarations[i]);
    if (DeclEl.ClassType=TPasClassType)
        and TPasClassType(DeclEl).IsForward
        and not (TPasClassType(DeclEl).CustomData is TResolvedReference) then
      RaiseMsg(20260813151500,nForwardTypeNotResolved,sForwardTypeNotResolved,
        [DeclEl.Name],DeclEl);
    end;
end;

function TPasResolver.IsInsideSpecialization(El: TPasElement): boolean;
// Whether El is a specialization, or is declared inside one. Only the outermost
// specialized type has the TPRSpecializedItem; everything nested in it is a
// copy made along with it.
var
  Cur: TPasElement;
begin
  Result:=false;
  Cur:=El;
  while Cur<>nil do
    begin
    if (Cur.CustomData is TPasGenericScope)
        and (TPasGenericScope(Cur.CustomData).SpecializedFromItem<>nil) then
      exit(true);
    Cur:=Cur.Parent;
    end;
end;

procedure TPasResolver.CheckPendingForwardProcs(El: TPasElement);
var
  i: Integer;
  DeclEl: TPasElement;
  Proc: TPasProcedure;
  aClassOrRec: TPasMembersType;
  ClassOrRecScope: TPasClassOrRecordScope;
begin
  if IsElementSkipped(El) then exit;
  if El is TPasDeclarations then
    begin
    for i:=0 to TPasDeclarations(El).Declarations.Count-1 do
      begin
      DeclEl:=TPasElement(TPasDeclarations(El).Declarations[i]);
      if DeclEl is TPasProcedure then
        begin
        Proc:=TPasProcedure(DeclEl);
        // A SPECIALIZED routine has no source body of its own - it is a copy of
        // the generic's, made when something needs the code. Until then it has
        // no implementation proc, and that is not a missing forward.
        if FDeferSpecImpls and IsInsideSpecialization(Proc) then
          continue;
        if ProcNeedsImplProc(Proc)
            and (TPasProcedureScope(Proc.CustomData).ImplProc=nil) then
          RaiseMsg(20170216152219,nForwardProcNotResolved,sForwardProcNotResolved,
            [GetElementTypeName(Proc),Proc.Name],Proc);
        end;
      end;
    end
  else if El is TPasMembersType then
    begin
    aClassOrRec:=TPasMembersType(El);
    if (aClassOrRec is TPasClassType) then
      begin
      if (TPasClassType(aClassOrRec).ObjKind in [okInterface,okDispInterface]) then
        exit;
      if TPasClassType(aClassOrRec).IsForward then
        exit;
      if TPasClassType(aClassOrRec).IsExternal then
        exit;
      end;
    ClassOrRecScope:=aClassOrRec.CustomData as TPasClassOrRecordScope;
    // A specialized type, or a type NESTED in one: its members are copies of
    // the generic's, and their bodies are made when something needs the code
    // (EnsureSpecializedImpl). A missing implementation proc here is a body
    // still owed, not an unresolved forward - `TCustomListWithPointers<T>`
    // nests TPointersEnumerator, whose own scope has no item of its own.
    if FDeferSpecImpls and IsInsideSpecialization(aClassOrRec) then
      exit;
    // finish implementation of (generic) class/record
    if ClassOrRecScope.GenericStep<>psgsInterfaceParsed then
      RaiseNotYetImplemented(20190804115324,El);
    for i:=0 to aClassOrRec.Members.Count-1 do
      begin
      DeclEl:=TPasElement(aClassOrRec.Members[i]);
      if DeclEl is TPasProcedure then
        begin
        Proc:=TPasProcedure(DeclEl);
        if Proc.IsAbstract or Proc.IsExternal then continue;
        // A specialization of a generic METHOD is appended to the class it was
        // declared in, so an ordinary class can hold one (`Add<System.Longint>`
        // beside the generic `Add<T>`). Its body is owed, not missing.
        if FDeferSpecImpls and IsInsideSpecialization(Proc) then continue;
        if TPasProcedureScope(Proc.CustomData).ImplProc=nil then
          begin
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.CheckPendingForwardProcs Proc.ParentPath=',Proc.PathName);
          {$ENDIF}
          RaiseMsg(20170216152221,nForwardProcNotResolved,sForwardProcNotResolved,
            [GetElementTypeName(Proc),Proc.Name],Proc);
          end;
        end;
      end;
    ClassOrRecScope.GenericStep:=psgsImplementationParsed;
    if ClassOrRecScope.SpecializedItems<>nil then
      FinishSpecializations(ClassOrRecScope);
    end;
end;

function TPasResolver.IsSpecialized(El: TPasGenericType): boolean;
begin
  Result:=(El<>nil) and (El.CustomData is TPasGenericScope)
      and (TPasGenericScope(El.CustomData).SpecializedFromItem<>nil);
end;

function TPasResolver.IsFullySpecialized(El: TPasGenericType): boolean;
var
  GenScope: TPasGenericScope;
  Params: TPasTypeArray;
  i: Integer;
begin
  if (El.GenericTemplateTypes<>nil) and (El.GenericTemplateTypes.Count>0) then
    exit(false);
  if not (El.CustomData is TPasGenericScope) then exit(true);
  GenScope:=TPasGenericScope(El.CustomData);
  if GenScope.SpecializedFromItem=nil then exit(true);
  Params:=GenScope.SpecializedFromItem.Params;
  for i:=0 to length(Params)-1 do
    if Params[i] is TPasGenericTemplateType then exit(false);
  Result:=true;
end;

function TPasResolver.IsFullySpecialized(Proc: TPasProcedure): boolean;
var
  Templates: TFPList;
  ProcScope: TPasProcedureScope;
  Params: TPasTypeArray;
  i: Integer;
begin
  if Proc.CustomData=nil then exit(false);
  ProcScope:=TPasProcedureScope(Proc.CustomData);
  if ProcScope.DeclarationProc<>nil then
    begin
    Proc:=ProcScope.DeclarationProc;
    ProcScope:=TPasProcedureScope(Proc.CustomData);
    end;
  Templates:=GetProcTemplateTypes(Proc);
  if (Templates<>nil) and (Templates.Count>0) then
    exit(false);
  if ProcScope.SpecializedFromItem=nil then
    exit(true);
  Params:=ProcScope.SpecializedFromItem.Params;
  for i:=0 to length(Params)-1 do
    if Params[i] is TPasGenericTemplateType then exit(false);
  Result:=true;
end;

function TPasResolver.IsInterfaceType(const ResolvedEl: TPasResolverResult;
  IntfType: TPasClassInterfaceType): boolean;
begin
  if ResolvedEl.BaseType<>btContext then exit(false);
  Result:=IsInterfaceType(ResolvedEl.LoTypeEl,IntfType);
end;

function TPasResolver.IsInterfaceType(TypeEl: TPasType;
  IntfType: TPasClassInterfaceType): boolean;
begin
  if TypeEl=nil then exit(false);
  TypeEl:=ResolveAliasType(TypeEl);
  Result:=(TypeEl.ClassType=TPasClassType)
    and (TPasClassType(TypeEl).ObjKind=okInterface)
    and (TPasClassType(TypeEl).InterfaceType=IntfType);
end;

function TPasResolver.IsTGUID(RecTypeEl: TPasRecordType): boolean;
var
  Members: TFPList;
  El: TPasElement;
  i, MemberIndex: Integer;
begin
  Result:=false;
  if not SameText(RecTypeEl.Name,'TGUID') then exit;
  if SameText(RecTypeEl.GetModule.Name,'system') then exit(true);
  Members:=RecTypeEl.Members;
  i:=1;
  for MemberIndex:=0 to Members.Count-1 do
    begin
    El:=TPasElement(Members[MemberIndex]);
    if (El.ClassType<>TPasVariable) then continue;
    if SameText(El.Name,'D'+IntToStr(i)) then
      begin
      if i=4 then exit(true);
      inc(i);
      end;
    end;
  Result:=true;
end;

function TPasResolver.IsTGUIDString(const ResolvedEl: TPasResolverResult
  ): boolean;
var
  TypeEl: TPasType;
  C: TClass;
  IdentEl: TPasElement;
begin
  if not (ResolvedEl.BaseType in btAllStrings) then
    exit(false);
  if (ResolvedEl.ExprEl<>nil) and (ResolvedEl.LoTypeEl<>nil) then
    exit(true); // untyped string literal
  IdentEl:=ResolvedEl.IdentEl;
  if IdentEl<>nil then
    begin
    C:=IdentEl.ClassType;
    if C.InheritsFrom(TPasVariable) then
      TypeEl:=TPasVariable(IdentEl).VarType
    else if C=TPasArgument then
      TypeEl:=TPasArgument(IdentEl).ArgType
    else if C=TPasResultElement then
      TypeEl:=TPasResultElement(IdentEl).ResultType
    else
      TypeEl:=nil;
    while TypeEl<>nil do
      begin
      if (TypeEl.ClassType=TPasAliasType)
          or (TypeEl.ClassType=TPasTypeAliasType) then
        begin
        if SameText(TypeEl.Name,'TGUIDString') then
          exit(true);
        TypeEl:=TPasAliasType(TypeEl).DestType;
        end
      else
        break;
      end;
    end;
  Result:=false;
end;

function TPasResolver.IsCustomAttribute(El: TPasElement): boolean;
var
  ClassEl: TPasClassType;
  ClassScope: TPasClassScope;
  aModule: TPasModule;
begin
  Result:=false;
  if (El=nil)
      or (El.ClassType<>TPasClassType) then exit;
  ClassEl:=TPasClassType(El);
  if (ClassEl.IsExternal) or (ClassEl.ObjKind<>okClass) then exit;
  while not SameText(ClassEl.Name,'TCustomAttribute') do
    begin
    ClassScope:=ClassEl.CustomData as TPasClassScope;
    if ClassScope.AncestorScope=nil then exit;
    ClassEl:=TPasClassType(ClassScope.AncestorScope.Element);
    end;
  if not (ClassEl.Parent is TPasSection) then
    exit; // this TCustomAttribute is not top level
  aModule:=ClassEl.GetModule;
  Result:=IsSystemUnit(aModule);
end;

function TPasResolver.IsSystemUnit(El: TPasModule): boolean;
var
  Section: TPasSection;
begin
  Result:=false;
  if El=nil then exit;
  if SameText(El.Name,'system') then exit(true);

  // tests and scripts are their own system unit: check if this is the root module
  if El.ClassType=TPasProgram then
    Section:=TPasProgram(El).ProgramSection
  else if El.ClassType=TPasLibrary then
    Section:=TPasLibrary(El).LibrarySection
  else
    Section:=El.InterfaceSection;
  Result:=length(Section.UsesClause)=0;
end;

function TPasResolver.GetAttributeCallsEl(El: TPasElement): TPasExprArray;
var
  Parent: TPasElement;
  C: TClass;
  Members: TFPList;
  i: Integer;
begin
  Result:=nil;
  if El=nil then exit;

  if (El.CustomData is TPasClassScope) and Assigned(TPasClassScope(El.CustomData).SpecializedFromItem) then
    Result := GetAttributeCallsEl(TPasClassScope(El.CustomData).SpecializedFromItem.GenericEl)
  else
  begin
    // find El in El.Parent members
    Parent:=El.Parent;
    if Parent=nil then exit;
    C:=Parent.ClassType;
    if C.InheritsFrom(TPasDeclarations) then
      Members:=TPasDeclarations(Parent).Declarations
    else if C.InheritsFrom(TPasMembersType) then
      Members:=TPasMembersType(Parent).Members
    else
      exit;

    i:=Members.IndexOf(El);
    if i<0 then exit;
    Result:=GetAttributeCalls(Members,i);
  end;
end;

function TPasResolver.GetAttributeCalls(Members: TFPList; Index: integer
  ): TPasExprArray;

  procedure AddAttributesInFront(Members: TFPList; i: integer);
  var
    j, l, k: Integer;
    Calls: TPasExprArray;
  begin
    // find attributes in front
    j:=i;
    while (j>0) and (TPasElement(Members[j-1]).ClassType=TPasAttributes) do
      dec(j);
    // collect all attribute calls
    l:=0;
    while j<i do
      begin
      Calls:=TPasAttributes(Members[j]).Calls;
      SetLength(Result,l+length(Calls));
      for k:=0 to length(Calls)-1 do
        begin
        Result[l]:=Calls[k];
        inc(l);
        end;
      inc(j);
      end;
  end;

var
  El, CurEl: TPasElement;
begin
  Result:=nil;
  El:=TPasElement(Members[Index]);
  AddAttributesInFront(Members,Index);
  if (El.ClassType=TPasClassType) and (not TPasClassType(El).IsForward) then
    repeat
      dec(Index);
      if Index<1 then break;
      CurEl:=TPasElement(Members[Index]);
      if (CurEl.ClassType=TPasClassType)
          and TPasClassType(CurEl).IsForward
          and (TPasClassType(CurEl).CustomData is TResolvedReference)
          and (TResolvedReference(TPasClassType(CurEl).CustomData).Declaration=El)
      then
        begin
        // class has a forward declaration -> add attributes
        AddAttributesInFront(Members,Index);
        break;
        end;
      if CurEl.CustomData is TPasClassScope then
        if Assigned(TPasClassScope(CurEl.CustomData).SpecializedFromItem) then
          AddAttributesInFront(Members,Index)
        else
          break;
    until false;
end;

function TPasResolver.ProcNeedsParams(El: TPasProcedureType): boolean;
begin
  Result:=(El.Args.Count>0) and (TPasArgument(El.Args[0]).ValueExpr=nil);
end;

function TPasResolver.ProcHasSelf(El: TPasProcedure): boolean;
var
  C: TClass;
begin
  if El.IsStatic then
    exit(false);
  C:=El.Parent.ClassType;
  if C.InheritsFrom(TPasSection) or (C=TProcedureBody) then
    exit(false);
  C:=El.ClassType;
  if (C=TPasClassConstructor) or (C=TPasClassDestructor) then
    exit(false);
  Result:=true;
end;

procedure TPasResolver.CreateProcSelfArg(Proc: TPasProcedure);
var
  SelfArg: TPasArgument;
  SelfType, LoSelfType: TPasType;
  ProcScope: TPasProcedureScope;
  ClassOrRecScope: TPasClassOrRecordScope;
  ClassRecType: TPasMembersType;
begin
  if Proc.IsStatic or Proc.IsExternal then exit;

  // add 'Self'
  if (Proc.ClassType=TPasClassConstructor)
      or (Proc.ClassType=TPasClassDestructor) then
    // actually class constructor/destructor are static
    exit;
  if Proc is TPasOperator then
    // operators are static too: they take every operand explicitly, and
    // rtl-objpas' TRttiContext even names one of them `self`
    exit;

  ProcScope:=TPasProcedureScope(Proc.CustomData);
  ClassOrRecScope:=ProcScope.ClassRecScope;
  if ClassOrRecScope=nil then exit;
  ClassRecType:=TPasMembersType(ClassOrRecScope.Element);

  if (Proc.ClassType=TPasClassProcedure)
      or (Proc.ClassType=TPasClassFunction) then
    begin
    if (ClassOrRecScope is TPasClassScope)
        and (TPasClassScope(ClassOrRecScope).CanonicalClassOf<>nil) then
      begin
      // 'Self' in a class method is the hidden classtype argument
      // Note: this is true in classes, adv records and helpers
      SelfArg:=TPasArgument(CreateOwnedElement(TPasArgument,'Self',Proc));
      ProcScope.SelfArg:=SelfArg;
      SelfArg.Access:=argConst;
      SelfArg.ArgType:=TPasClassScope(ClassOrRecScope).CanonicalClassOf;
      end
    else
      RaiseInternalError(20190106121745);
    end
  else
    begin
    // 'Self' in a method is the hidden instance argument
    SelfArg:=TPasArgument(CreateOwnedElement(TPasArgument,'Self',Proc));
    ProcScope.SelfArg:=SelfArg;
    SelfType:=ClassRecType;
    if (SelfType.ClassType=TPasClassType)
        and (TPasClassType(SelfType).HelperForType<>nil) then
      begin
      // in a helper Self is a var argument of the helped variable
      SelfType:=TPasClassType(SelfType).HelperForType;
      end;
    LoSelfType:=ResolveAliasType(SelfType);
    if (LoSelfType is TPasClassType)
        and (TPasClassType(LoSelfType).ObjKind=okClass) then
      SelfArg.Access:=argConst
    else
      SelfArg.Access:=argVar;
    SelfArg.ArgType:=SelfType;
    end;
end;

function TPasResolver.IsProcOverride(AncestorProc, DescendantProc: TPasProcedure
  ): boolean;
var
  Proc, OverriddenProc: TPasProcedure;
begin
  Result:=false;
  if AncestorProc=nil then exit;
  if DescendantProc=nil then exit;
  Proc:=DescendantProc;
  if not Proc.IsOverride then exit;
  if not AncestorProc.IsOverride and not AncestorProc.IsVirtual then exit;
  repeat
    OverriddenProc:=TPasProcedureScope(Proc.CustomData).OverriddenProc;
    if AncestorProc=OverriddenProc then exit(true);
    Proc:=OverriddenProc;
  until Proc=nil;
end;

function TPasResolver.IsProcOverload(Proc: TPasProcedure): boolean;
var
  ProcScope: TPasProcedureScope;
begin
  if Proc.IsOverload then
    exit(true)
  else if Proc.IsOverride and (Proc.CustomData is TPasProcedureScope) then
    begin
    ProcScope:=TPasProcedureScope(Proc.CustomData);
    if ppsfIsOverrideOverload in ProcScope.Flags then
      exit(true);
    end;
  Result:=false;
end;

function TPasResolver.GetTopLvlProc(El: TPasElement): TPasProcedure;
begin
  Result:=nil;
  while El<>nil do
    begin
    if El is TPasProcedure then
      Result:=TPasProcedure(El);
    El:=El.Parent;
    end;
end;

function TPasResolver.GetParentProc(El: TPasElement; GetDeclProc: boolean
  ): TPasProcedure;
var
  ProcScope: TPasProcedureScope;
begin
  Result:=nil;
  while El<>nil do
    begin
    if El is TPasProcedure then
      begin
      Result:=TPasProcedure(El);
      if GetDeclProc and (El.CustomData is TPasProcedureScope) then
        begin
        ProcScope:=TPasProcedureScope(El.CustomData);
        if ProcScope.DeclarationProc<>nil then
          Result:=ProcScope.DeclarationProc;
        end;
      exit;
      end;
    El:=El.Parent;
    end;
end;

function TPasResolver.GetRangeLength(RangeExpr: TPasExpr): TMaxPrecInt;
var
  Range: TResEvalValue;
begin
  Result:=0;
  Range:=Eval(RangeExpr,[refConst]);
  if Range=nil then
    RaiseNotYetImplemented(20170910210416,RangeExpr);
  try
    case Range.Kind of
    revkRangeInt:
      Result:=TResEvalRangeInt(Range).RangeEnd-TResEvalRangeInt(Range).RangeStart+1;
    revkRangeUInt:
      Result:=TResEvalRangeUInt(Range).RangeEnd-TResEvalRangeUInt(Range).RangeStart+1;
    else
      RaiseNotYetImplemented(20170910210554,RangeExpr);
    end;
  finally
    ReleaseEvalValue(Range);
  end;
  {$IFDEF VerbosePasResolver}
  {AllowWriteln}
  //if Result=0 then
    writeln('TPasResolver.GetRangeLength Result=',Result);
  {AllowWriteln-}
  {$ENDIF}
end;

function TPasResolver.EvalRangeLimit(RangeExpr: TPasExpr; Flags: TResEvalFlags;
  EvalLow: boolean; ErrorEl: TPasElement): TResEvalValue;
var
  Range: TResEvalValue;
  EnumType: TPasEnumType;
begin
  Result:=nil;
  Range:=Eval(RangeExpr,Flags+[refConst]);
  if Range=nil then
    RaiseNotYetImplemented(20170601191258,RangeExpr);
  case Range.Kind of
  revkRangeInt:
    case TResEvalRangeInt(Range).ElKind of
      revskEnum:
        begin
        EnumType:=NoNil(TResEvalRangeInt(Range).ElType) as TPasEnumType;
        if EvalLow then
          Result:=TResEvalEnum.CreateValue(
            TResEvalRangeInt(Range).RangeStart,TPasEnumValue(EnumType.Values[0]))
        else
          Result:=TResEvalEnum.CreateValue(
            TResEvalRangeInt(Range).RangeEnd,
            TPasEnumValue(EnumType.Values[EnumType.Values.Count-1]));
        end;
      revskInt:
        if EvalLow then
          Result:=TResEvalInt.CreateValue(TResEvalRangeInt(Range).RangeStart)
        else
          Result:=TResEvalInt.CreateValue(TResEvalRangeInt(Range).RangeEnd);
      revskChar:
        {$ifdef FPC_HAS_CPSTRING}
        if TResEvalRangeInt(Range).RangeEnd<256 then
          begin
          if EvalLow then
            Result:=TResEvalString.CreateValue(chr(TResEvalRangeInt(Range).RangeStart))
          else
            Result:=TResEvalString.CreateValue(chr(TResEvalRangeInt(Range).RangeEnd));
          end
        else
        {$endif}
          begin
          if EvalLow then
            Result:=TResEvalUTF16.CreateValue(widechar(TResEvalRangeInt(Range).RangeStart))
          else
            Result:=TResEvalUTF16.CreateValue(widechar(TResEvalRangeInt(Range).RangeEnd));
          end;
      revskBool:
        if EvalLow then
          Result:=TResEvalBool.CreateValue(TResEvalRangeInt(Range).RangeStart<>0)
        else
          Result:=TResEvalBool.CreateValue(TResEvalRangeInt(Range).RangeEnd<>0);
    else
      ReleaseEvalValue(Range);
      RaiseNotYetImplemented(20170601195240,ErrorEl);
    end;
  revkRangeUInt:
    if EvalLow then
      Result:=TResEvalUInt.CreateValue(TResEvalRangeUInt(Range).RangeStart)
    else
      Result:=TResEvalUInt.CreateValue(TResEvalRangeUInt(Range).RangeEnd);
  else
    ReleaseEvalValue(Range);
    RaiseNotYetImplemented(20170601195336,ErrorEl);
  end;
  ReleaseEvalValue(Range);
end;

function TPasResolver.EvalTypeRange(Decl: TPasType; Flags: TResEvalFlags
  ): TResEvalValue;
var
  C: TClass;
  BaseTypeData: TResElDataBaseType;
begin
  Result:=nil;
  Decl:=ResolveAliasType(Decl);
  C:=Decl.ClassType;
  if C=TPasRangeType then
    begin
    Result:=fExprEvaluator.Eval(TPasRangeType(Decl).RangeExpr,Flags);
    if (Result<>nil) and (Result.IdentEl=nil) then
      begin
      Result.IdentEl:=Decl;
      exit;
      end;
    end
  else if C=TPasEnumType then
    begin
    Result:=TResEvalRangeInt.CreateValue(revskEnum,TPasEnumType(Decl),
                              GetEnumMinOrdinal(TPasEnumType(Decl)),GetEnumMaxOrdinal(TPasEnumType(Decl)));
    Result.IdentEl:=Decl;
    exit;
    end
  else if (C=TPasSetType) and (TPasSetType(Decl).EnumType<>nil) then
    // a set type ranges over its element type: low(TComponentState)
    exit(EvalTypeRange(TPasSetType(Decl).EnumType,Flags))
  else if C=TPasUnresolvedSymbolRef then
    begin
    if (Decl.CustomData is TResElDataBaseType) then
      begin
      BaseTypeData:=TResElDataBaseType(Decl.CustomData);
      case BaseTypeData.BaseType of
      btChar:
        begin
        Result:=TResEvalRangeInt.Create;
        TResEvalRangeInt(Result).ElKind:=revskChar;
        TResEvalRangeInt(Result).RangeStart:=0;
        {$ifdef FPC_HAS_CPSTRING}
        if BaseTypeChar in [btChar,btAnsiChar] then
          TResEvalRangeInt(Result).RangeEnd:=$ff
        else
        {$endif}
          TResEvalRangeInt(Result).RangeEnd:=$ffff;
        end;
      {$ifdef FPC_HAS_CPSTRING}
      btAnsiChar:
        Result:=TResEvalRangeInt.CreateValue(revskChar,nil,0,$ff);
      {$endif}
      btWideChar:
        Result:=TResEvalRangeInt.CreateValue(revskChar,nil,0,$ffff);
      btBoolean,btByteBool,btWordBool{$ifdef HasInt64},btQWordBool{$endif}:
        Result:=TResEvalRangeInt.CreateValue(revskBool,nil,0,1);
      btByte,
      btShortInt,
      btWord,
      btSmallInt,
      btLongWord,
      btLongint,
      {$ifdef HasInt64}
      btInt64,
      btComp,
      {$endif}
      btIntSingle,
      btUIntSingle,
      btIntDouble,
      btUIntDouble,
      btCurrency:
        begin
        Result:=TResEvalRangeInt.Create;
        TResEvalRangeInt(Result).ElKind:=revskInt;
        GetIntegerRange(BaseTypeData.BaseType,
          TResEvalRangeInt(Result).RangeStart,TResEvalRangeInt(Result).RangeEnd);
        end;
      end;
      end;
    end;
end;

function TPasResolver.HasTypeInfo(El: TPasType): boolean;
begin
  Result:=false;
  if El=nil then exit;
  if El.CustomData is TResElDataBaseType then
    exit(true); // base type
  if El is TPasStringType then
    exit(true); // codepage AnsiString / ShortString[N] -> tkAString / tkSString
  if El.Parent=nil then exit;
  if El.Parent is TPasType then
    begin
    if not HasTypeInfo(TPasType(El.Parent)) then
      exit;
    end
  else if ElHasModeSwitch(El,msOmitRTTI) then
    exit
  else if El.Parent is TPasAnonymousProcedure then
    exit;
  Result:=true;
end;

function TPasResolver.EnumHasHoles(El: TPasEnumType): boolean;
begin
  // Base/pas2js uses the INDEX model, which never has holes.
  Result:=false;
  if El=nil then ;
end;

function TPasResolver.GetEnumValueOrdinal(EnumValue: TPasEnumValue): TMaxPrecInt;
begin
  // Base/pas2js: ordinal = position (index) in the enum's value list.
  Result:=0;
  if (EnumValue=nil) or not (EnumValue.Parent is TPasEnumType) then exit;
  Result:=TPasEnumType(EnumValue.Parent).Values.IndexOf(EnumValue);
end;

function TPasResolver.GetEnumValueForOrdinal(El: TPasEnumType; Ord: TMaxPrecInt): TPasEnumValue;
begin
  // Base/pas2js: ordinal == index into the value list.
  Result:=nil;
  if (El=nil) or (El.Values=nil) then exit;
  if (Ord>=0) and (Ord<El.Values.Count) then
    Result:=TPasEnumValue(El.Values[Ord]);
end;

function TPasResolver.GetEnumMinOrdinal(El: TPasEnumType): TMaxPrecInt;
begin
  Result:=0;
  if El=nil then ;
end;

function TPasResolver.GetEnumMaxOrdinal(El: TPasEnumType): TMaxPrecInt;
begin
  Result:=0;
  if (El=nil) or (El.Values=nil) or (El.Values.Count=0) then exit;
  Result:=El.Values.Count-1;
end;

function TPasResolver.GetFuncRefInvokeProcType(TypeEl: TPasType; ArgCount: Integer): TPasProcedureType;
begin
  // Base/pas2js: no funcref-derived-interface representation.
  Result:=nil;
  if (TypeEl=nil) or (ArgCount=0) then ;
end;

procedure TPasResolver.FinishProcTypeNativeChecks(El: TPasProcedureType; Proc: TPasProcedure);
begin
  // Base/pas2js: no native-ABI constraints. TPasNativeResolver overrides this.
  if (El=nil) or (Proc=nil) then ;
end;

function TPasResolver.IsAnonymousElType(El: TPasType): boolean;
// e.g. b$a$a
var
  aName: String;
  i, l: SizeInt;
  j: Integer;
begin
  Result:=false;
  if AnonymousElTypePostfix='' then exit;
  aName:=El.Name;
  l:=length(AnonymousElTypePostfix);
  i:=length(aName);
  repeat
    dec(i,l);
    if i>0 then
      begin
      j:=i;
      while (j<=l) and (aName[i+j]=AnonymousElTypePostfix[j]) do inc(j);
      if j>l then
        begin
        Result:=true;
        continue;
        end;
      end;
    if not Result then exit; // no postfix
    // at least one anonymous eltype postfix
    Result:=IsValidIdent(LeftStr(aName,i+l));
    exit;
  until false;
end;

function TPasResolver.GetActualBaseType(bt: TResolverBaseType
  ): TResolverBaseType;
begin
  case bt of
  btChar: Result:=BaseTypeChar;
  btString: Result:=BaseTypeString;
  btExtended: Result:=BaseTypeExtended;
  else Result:=bt;
  end;
end;

function TPasResolver.GetCombinedBoolean(Bool1, Bool2: TResolverBaseType;
  ErrorEl: TPasElement): TResolverBaseType;
begin
  if Bool1=Bool2 then exit(Bool1);
  case Bool1 of
  btBoolean: Result:=Bool2;
  btByteBool: if Bool2<>btBoolean then Result:=Bool2;
  btWordBool: if not (Bool2 in [btBoolean,btByteBool]) then Result:=Bool2;
  btLongBool: if not (Bool2 in [btBoolean,btByteBool,btWordBool]) then Result:=Bool2;
  {$ifdef HasInt64}
  btQWordBool: if not (Bool2 in [btBoolean,btByteBool,btWordBool,btLongBool]) then Result:=Bool2;
  {$endif}
  else
    RaiseNotYetImplemented(20170420093805,ErrorEl);
  end;
end;

function TPasResolver.GetCombinedInt(const Int1, Int2: TPasResolverResult;
  ErrorEl: TPasElement): TResolverBaseType;
var
  Precision1, Precision2: word;
  Signed1, Signed2: boolean;
begin
  if Int1.BaseType=Int2.BaseType then exit(Int1.BaseType);
  GetIntegerProps(Int1.BaseType,Precision1,Signed1);
  GetIntegerProps(Int2.BaseType,Precision2,Signed2);
  if Precision1=Precision2 then
    begin
    if Signed1<>Signed2 then
      Precision1:=Max(Precision1,Precision2)+1;
    end;
  Result:=GetIntegerBaseType(Max(Precision1,Precision2),Signed1 or Signed2,ErrorEl);
end;

procedure TPasResolver.GetIntegerProps(bt: TResolverBaseType; out
  Precision: word; out Signed: boolean);
begin
  case bt of
  btByte: begin Precision:=8; Signed:=false; end;
  btShortInt: begin Precision:=8; Signed:=true; end;
  btWord: begin Precision:=16; Signed:=false; end;
  btSmallInt: begin Precision:=16; Signed:=true; end;
  btIntSingle: begin Precision:=23; Signed:=true; end;
  btUIntSingle: begin Precision:=22; Signed:=false; end;
  btLongWord: begin Precision:=32; Signed:=false; end;
  btLongint: begin Precision:=32; Signed:=true; end;
  btIntDouble: begin Precision:=53; Signed:=true; end;
  btUIntDouble: begin Precision:=52; Signed:=false; end;
  {$ifdef HasInt64}
  btQWord: begin Precision:=64; Signed:=false; end;
  btInt64,btComp: begin Precision:=64; Signed:=true; end;
  {$endif}
  else
    RaiseInternalError(20170420095727);
  end;
end;

function TPasResolver.SameOrdinalWidth(bt1, bt2: TResolverBaseType): boolean;
// Two integer base types that occupy the same number of bytes.

  function Width(bt: TResolverBaseType): integer;
  var
    MinVal, MaxVal: TMaxPrecInt;
  begin
    Result:=0;
    bt:=GetActualBaseType(bt);
    if not GetIntegerRange(bt,MinVal,MaxVal) then exit;
    if (MinVal>=-128) and (MaxVal<=255) then Result:=1
    else if (MinVal>=-32768) and (MaxVal<=65535) then Result:=2
    else if (MinVal>=-2147483648) and (MaxVal<=4294967295) then Result:=4
    else Result:=8;
  end;

begin
  Result:=(Width(bt1)<>0) and (Width(bt1)=Width(bt2));
end;


function TPasResolver.IntegerStorageOf(const R: TPasResolverResult; out
  Kind: TResolverBaseType; out MinVal, MaxVal: TMaxPrecInt): boolean;
// The range of an integer type and the base type it is stored as. A subrange
// takes the smallest fitting type, signed first, as ppcx64's range_to_basetype.
var
  TypeEl: TPasType;
  V: TResEvalValue;
begin
  Result:=false;
  Kind:=btNone;
  MinVal:=0;
  MaxVal:=0;
  TypeEl:=ResolveAliasType(R.LoTypeEl);
  if TypeEl is TPasRangeType then
    begin
    V:=nil;
    try
      V:=EvalTypeRange(TypeEl,[]);
    except
      V:=nil;
    end;
    if V=nil then exit;
    try
      if not (V is TResEvalRangeInt) or (TResEvalRangeInt(V).ElKind<>revskInt) then
        exit;
      MinVal:=TResEvalRangeInt(V).RangeStart;
      MaxVal:=TResEvalRangeInt(V).RangeEnd;
    finally
      ReleaseEvalValue(V);
    end;
    if (MinVal>=-128) and (MaxVal<=127) then Kind:=btShortInt
    else if (MinVal>=0) and (MaxVal<=255) then Kind:=btByte
    else if (MinVal>=-32768) and (MaxVal<=32767) then Kind:=btSmallInt
    else if (MinVal>=0) and (MaxVal<=65535) then Kind:=btWord
    else if (MinVal>=low(longint)) and (MaxVal<=high(longint)) then Kind:=btLongint
    else if (MinVal>=0) and (MaxVal<=high(longword)) then Kind:=btLongWord
    else Kind:=btInt64;
    exit(true);
    end;
  if not (R.BaseType in btAllInteger) then exit;
  Kind:=GetActualBaseType(R.BaseType);
  Result:=GetIntegerRange(Kind,MinVal,MaxVal);
end;

function TPasResolver.IntegerRangeFitsSameStorage(const ArgRes,
  ParamRes: TPasResolverResult): boolean;
// True when the argument's integer type is stored like the parameter's and
// its range lies inside the parameter's range.
var
  ArgKind, ParamKind: TResolverBaseType;
  ArgMin, ArgMax, ParamMin, ParamMax: TMaxPrecInt;
begin
  Result:=false;
  // Only a real subrange: a `type longint` copy stays distinct (Delphi mode).
  if not (ResolveAliasType(ArgRes.LoTypeEl) is TPasRangeType)
      and not (ResolveAliasType(ParamRes.LoTypeEl) is TPasRangeType) then
    exit;
  if not IntegerStorageOf(ArgRes,ArgKind,ArgMin,ArgMax) then exit;
  if not IntegerStorageOf(ParamRes,ParamKind,ParamMin,ParamMax) then exit;
  Result:=(ArgKind=ParamKind) and (ArgMin>=ParamMin) and (ArgMax<=ParamMax);
end;

function TPasResolver.IntegerTypeFitsSameStorage(ArgType, ParamType: TPasType
  ): boolean;
// IntegerRangeFitsSameStorage for two types: an override's result may be a
// subrange stored like the ancestor's, as ppcx64 allows (the LCL's
// `function ShowModal: TModalResult; override;` over `ShowModal: Integer`).
var
  ArgRes, ParamRes: TPasResolverResult;
begin
  Result:=false;
  if (ArgType=nil) or (ParamType=nil) then exit;
  ComputeElement(ArgType,ArgRes,[rcType]);
  ComputeElement(ParamType,ParamRes,[rcType]);
  if IntegerRangeFitsSameStorage(ArgRes,ParamRes) then
    exit(true);
  // A `type PtrUInt` copy of the same integer type (the LCL's HWND over TLCLHandle).
  Result:=(ArgRes.BaseType in btAllInteger) and (ParamRes.BaseType in btAllInteger)
    and not (ResolveAliasType(ArgRes.LoTypeEl) is TPasRangeType)
    and not (ResolveAliasType(ParamRes.LoTypeEl) is TPasRangeType)
    and (GetActualBaseType(ArgRes.BaseType)=GetActualBaseType(ParamRes.BaseType));
end;

function TPasResolver.GetIntegerRange(bt: TResolverBaseType; out MinVal,
  MaxVal: TMaxPrecInt): boolean;
begin
  Result:=true;
  if bt=btExtended then bt:=BaseTypeExtended;
  case bt of
  btByte: begin MinVal:=Low(byte); MaxVal:=High(byte); end;
  btShortInt: begin MinVal:=low(ShortInt); MaxVal:=high(ShortInt); end;
  btWord: begin MinVal:=low(word); MaxVal:=high(word); end;
  btSmallInt: begin MinVal:=low(SmallInt); MaxVal:=high(SmallInt); end;
  btLongWord: begin MinVal:=low(LongWord); MaxVal:=high(LongWord); end;
  btLongint: begin MinVal:=low(LongInt); MaxVal:=high(LongInt); end;
  {$ifdef HasInt64}
  btInt64,
  btComp: begin MinVal:=low(int64); MaxVal:=high(int64); end;
  {$endif}
  btSingle,btIntSingle: begin MinVal:=MinSafeIntSingle; MaxVal:=MaxSafeIntSingle; end;
  btUIntSingle: begin MinVal:=0; MaxVal:=MaxSafeIntSingle; end;
  btDouble,btIntDouble: begin MinVal:=MinSafeIntDouble; MaxVal:=MaxSafeIntDouble; end;
  btUIntDouble: begin MinVal:=0; MaxVal:=MaxSafeIntDouble; end;
  btCurrency: begin MinVal:=MinSafeIntCurrency; MaxVal:=MaxSafeIntCurrency; end;
  else
    Result:=false;
  end;
end;

function TPasResolver.GetIntegerBaseType(Precision: word; Signed: boolean;
  ErrorEl: TPasElement): TResolverBaseType;
begin
  if Precision<=8 then
    begin
    if Signed then
      Result:=btShortInt
    else
      Result:=btByte;
    if BaseTypes[Result]<>nil then exit;
    end;
  if Precision<=16 then
    begin
    if Signed then
      Result:=btSmallInt
    else
      Result:=btWord;
    if BaseTypes[Result]<>nil then exit;
    end;
  if (Precision<=22) and (not Signed) and (BaseTypes[btUIntSingle]<>nil) then
    exit(btUIntSingle);
  if (Precision<=23) and Signed and (BaseTypes[btIntSingle]<>nil) then
    exit(btIntSingle);
  if Precision<=32 then
    begin
    if Signed then
      Result:=btLongint
    else
      Result:=btLongWord;
    if BaseTypes[Result]<>nil then exit;
    end;
  if (Precision<=52) and (not Signed) and (BaseTypes[btUIntDouble]<>nil) then
    exit(btUIntDouble);
  if (Precision<=53) and Signed and (BaseTypes[btIntDouble]<>nil) then
    exit(btIntDouble);
  {$ifdef HasInt64}
  if Precision<=64 then
    begin
    if Signed then
      Result:=btInt64
    else
      Result:=btQWord;
    if BaseTypes[Result]<>nil then exit;
    end;
  {$endif}
  if ErrorEl<>nil then
    RaiseRangeCheck(20170420100336,ErrorEl)
  else
    Result:=btNone;
end;

function TPasResolver.GetSmallestIntegerBaseType(MinVal, MaxVal: TMaxPrecInt
  ): TResolverBaseType;
// returns BaseTypeExtended if too big
var
  V: TMaxPrecInt;
begin
  if MinVal>MaxVal then
    MinVal:=MaxVal;
  if MinVal<0 then
    begin
    if MaxVal>-(MinVal+1) then
      V:=MaxVal
    else
      V:=-(MinVal+1);
    if V<=high(ShortInt) then
      Result:=btShortInt
    else if V<=high(SmallInt) then
      Result:=btSmallInt
    else if (BaseTypes[btIntSingle]<>nil) and (V<=MaxSafeIntSingle) then
      Result:=btIntSingle
    else if V<=High(Longint) then
      Result:=btLongint
    else if (BaseTypes[btIntDouble]<>nil) and (V<=MaxSafeIntDouble) then
      Result:=btIntDouble
    else
      begin
      Result:=btIntMax;
      if BaseTypes[Result]=nil then
        Result:=BaseTypeExtended;
      end;
    end
  else
    begin
    V:=MaxVal;
    if V<=high(Byte) then
      Result:=btByte
    else if V<=high(Word) then
      Result:=btWord
    else if (BaseTypes[btUIntSingle]<>nil) and (V<=MaxSafeIntSingle) then
      Result:=btUIntSingle
    else if V<=High(LongWord) then
      Result:=btLongWord
    else if (BaseTypes[btUIntDouble]<>nil) and (V<=MaxSafeIntDouble) then
      Result:=btUIntDouble
    else
      begin
      Result:=btIntMax;
      if BaseTypes[Result]=nil then
        Result:=BaseTypeExtended;
      end;
    end;
end;

function TPasResolver.GetCombinedChar(const Char1, Char2: TPasResolverResult;
  ErrorEl: TPasElement): TResolverBaseType;
var
  bt1, bt2: TResolverBaseType;
begin
  bt1:=GetActualBaseType(Char1.BaseType);
  bt2:=GetActualBaseType(Char2.BaseType);
  if bt1=bt2 then exit(bt1);
  if not (bt1 in btAllChars) then
    RaiseInternalError(20170420103128);
  Result:=btWideChar;
  if Result=BaseTypeChar then
    Result:=btChar;
  if ErrorEl=nil then ;
end;

function TPasResolver.GetCombinedString(const Str1, Str2: TPasResolverResult;
  ErrorEl: TPasElement): TResolverBaseType;
var
  bt1, bt2: TResolverBaseType;
begin
  bt1:=GetActualBaseType(Str1.BaseType);
  bt2:=GetActualBaseType(Str2.BaseType);
  if bt1=bt2 then exit(bt1);
  case bt1 of
  {$ifdef FPC_HAS_CPSTRING}
  btAnsiChar:
    case bt2 of
    btChar: Result:=btChar;
    btWideChar: Result:=btWideChar;
    else Result:=bt2;
    end;
  {$endif}
  btWideChar:
    case bt2 of
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiChar: Result:=btWideChar;
    {$endif}
    btWideString: Result:=btWideString;
    btString,btUnicodeString
    {$ifdef FPC_HAS_CPSTRING},btShortString,btAnsiString,btRawByteString{$endif}:
        Result:=btUnicodeString;
    else RaiseNotYetImplemented(20170420103808,ErrorEl);
    end;
  {$ifdef FPC_HAS_CPSTRING}
  btShortString:
    case bt2 of
    btChar,btAnsiChar: Result:=btShortString;
    btString,btAnsiString: Result:=btAnsiString;
    btRawByteString: Result:=btRawByteString;
    btWideChar,btUnicodeString: Result:=btUnicodeString;
    btWideString: Result:=btWideString;
    else RaiseNotYetImplemented(20170420120937,ErrorEl);
    end;
  {$endif}
  btString{$ifdef FPC_HAS_CPSTRING},btAnsiString{$endif}:
    case bt2 of
    {$ifdef FPC_HAS_CPSTRING}
    btChar,btAnsiChar,btString,btShortString,btRawByteString: Result:=btAnsiString;
    {$endif}
    btWideChar,btUnicodeString: Result:=btUnicodeString;
    btWideString: Result:=btWideString;
    else RaiseNotYetImplemented(20170420121201,ErrorEl);
    end;
  {$ifdef FPC_HAS_CPSTRING}
  btRawByteString:
    case bt2 of
    btChar,btAnsiChar,btRawByteString,btShortString: Result:=btRawByteString;
    btString,btAnsiString: Result:=btAnsiString;
    btWideChar,btUnicodeString: Result:=btUnicodeString;
    btWideString: Result:=btWideString;
    else RaiseNotYetImplemented(20170420121352,ErrorEl);
    end;
  {$endif}
  btWideString:
    case bt2 of
    btChar,btWideChar,{$ifdef FPC_HAS_CPSTRING}btAnsiChar,btShortString,{$endif}btWideString:
      Result:=btWideString;
    btString,{$ifdef FPC_HAS_CPSTRING}btAnsiString,{$endif}btUnicodeString:
      Result:=btUnicodeString;
    else RaiseNotYetImplemented(20170420121532,ErrorEl);
    end;
  btUnicodeString:
    Result:=btUnicodeString;
  else
    RaiseNotYetImplemented(20170420103153,ErrorEl);
  end;
  if Result=BaseTypeChar then
    Result:=btChar
  else if Result=BaseTypeString then
    Result:=btString;
end;

function TPasResolver.GetCombinedBaseType(const A, B: TPasResolverResult;
  ErrorEl: TPasElement): TResolverBaseType;
begin
  Result:=btNone;
  if A.BaseType in btAllBooleans then
    begin
    if B.BaseType in btAllBooleans then
      Result:=GetCombinedBoolean(A.BaseType,B.BaseType,ErrorEl);
    end
  else if A.BaseType in btAllInteger then
    begin
    if B.BaseType in btAllInteger then
      Result:=GetCombinedInt(A,B,ErrorEl);
    end
  else if A.BaseType in btAllChars then
    begin
    if B.BaseType in btAllChars then
      Result:=GetCombinedChar(A,B,ErrorEl)
    else if B.BaseType in btAllStrings then
      Result:=GetCombinedString(A,B,ErrorEl);
    end
  else if A.BaseType in btAllStrings then
    begin
    if B.BaseType in btAllStringAndChars then
      Result:=GetCombinedString(A,B,ErrorEl);
    end;
end;

function TPasResolver.IsElementSkipped(El: TPasElement): boolean;
begin
  Result:=El=nil;
end;

function TPasResolver.FindLocalBuiltInSymbol(El: TPasElement): TPasElement;
var
  Data: TObject;
begin
  Data:=El.CustomData;
  if Data=nil then
    RaiseInternalError(20180215185302,GetObjName(El));
  if Data.ClassType=TResElDataBaseType then
    Result:=BaseTypes[TResElDataBaseType(Data).BaseType]
  else if (Data.ClassType=TResElDataBuiltInProc)
      and (TResElDataBuiltInProc(Data).BuiltIn<>bfCustom) then
    Result:=BuiltInProcs[TResElDataBuiltInProc(Data).BuiltIn].Element
  else
    Result:=nil;
end;

function TPasResolver.GetFirstSection(WithUnitImpl: boolean): TPasSection;
var
  Module: TPasModule;
begin
  Result:=nil;
  Module:=RootElement;
  if Module=nil then exit;
  if Module is TPasProgram then
    Result:=TPasProgram(Module).ProgramSection
  else if Module is TPasLibrary then
    Result:=TPasLibrary(Module).LibrarySection
  else
    begin
    Result:=Module.InterfaceSection;
    if WithUnitImpl and (Result=nil) then
      Result:=Module.ImplementationSection;
    end;
end;

function TPasResolver.GetLastSection: TPasSection;
var
  Module: TPasModule;
begin
  Result:=nil;
  Module:=RootElement;
  if Module=nil then exit;
  if Module is TPasProgram then
    Result:=TPasProgram(Module).ProgramSection
  else if Module is TPasLibrary then
    Result:=TPasLibrary(Module).LibrarySection
  else if Module.ImplementationSection<>nil then
    Result:=Module.ImplementationSection
  else
    Result:=Module.InterfaceSection;
end;

function TPasResolver.GetParentSection(El: TPasElement): TPasSection;
begin
  while El<>nil do
    begin
    if El is TPasSection then exit(TPasSection(El));
    El:=El.Parent;
    end;
  Result:=nil;
end;

function TPasResolver.FindUsedUnitInSection(aMod: TPasModule;
  Section: TPasSection): TPasUsesUnit;
var
  Clause: TPasUsesClause;
  i: Integer;
begin
  Result:=nil;
  if Section=nil then exit;
  Clause:=Section.UsesClause;
  for i:=0 to length(Clause)-1 do
    if Clause[i].Module=aMod then exit(Clause[i]);
end;

function TPasResolver.FirstSectionUsesUnit(aModule: TPasModule): boolean;
var
  aSection: TPasSection;
begin
  Result:=false;
  aSection:=GetFirstSection(false);
  if aSection=nil then
    exit;
  Result:=FindUsedUnitInSection(aModule,aSection)<>nil;
end;

function TPasResolver.ImplementationUsesUnit(aModule: TPasModule;
  NotInIntf: boolean): boolean;
var
  MyModule: TPasModule;
begin
  Result:=false;
  MyModule:=RootElement;
  if MyModule=nil then exit;
  if FindUsedUnitInSection(aModule,MyModule.ImplementationSection)=nil then
    exit;
  if NotInIntf then
    Result:=not FirstSectionUsesUnit(aModule);
end;

function TPasResolver.GetShiftAndMaskForLoHiFunc(BaseType: TResolverBaseType;
  isLoFunc: Boolean; out Mask: LongWord): Integer;
const
  SHIFT_SIZE: array[btByte..{$IFDEF HasInt64}btComp{$ELSE}btIntDouble{$ENDIF}] of Integer = (
    4, // btByte
    8, // btShortInt FPC lo/hi(shortint) works like SmallInt
    8, 8, // btWord, btSmallInt
    16, 16, 16, 16, // btUIntSingle, btIntSingle, btLongWord, btLongint
    32, 32 // btUIntDouble, btIntDouble
    {$IFDEF HasInt64}
    , 32, 32, 32 // btQWord, btInt64, btComp
    {$endif}
  );
begin
  if (BaseType >= Low(SHIFT_SIZE)) and (BaseType <= High(SHIFT_SIZE)) then
    begin
    if msDelphi in CurrentParser.CurrentModeswitches then
      Result := 8
    else
      Result := SHIFT_SIZE[BaseType];
    case Result of
      8: Mask := $FF;
      16: Mask := $FFFF;
      32: Mask := $FFFFFFFF;
    else
      {4} Mask := $F;
    end;
    if isLoFunc then
      Result := 0;
    end
  else
    begin
    RaiseInternalError(20190130122300);
    Result := -1;
    end;
end;

function TPasResolver.CheckSrcIsADstType(const ResolvedSrcType,
  ResolvedDestType: TPasResolverResult): integer;
// finds distance between classes SrcType and DestType
begin
  Result:=CheckClassIsClass(ResolvedSrcType.LoTypeEl,ResolvedDestType.LoTypeEl);
end;

function TPasResolver.CheckClassIsClass(SrcType, DestType: TPasType): integer;
// check if Src is equal or descends from Dest
// Generics: TBird<T> is both directions a TBird<word>
//       and TBird<TMap<T>> is both directions a TBird<TMap<word>>
//       but a TBird<word> is not a TBird<char>

  function CheckSpecialized(SrcScope, DestScope: TPasGenericScope): boolean;
  var
    SrcParams, DestParams: TPasTypeArray;
    i: Integer;
    SrcParam, DestParam: TPasType;
    SrcParamScope, DestParamScope: TPasGenericScope;
    SrcSpecializedFromItem, DestSpecializedFromItem: TPRSpecializedItem;
  begin
    SrcSpecializedFromItem:=SrcScope.SpecializedFromItem;
    DestSpecializedFromItem:=DestScope.SpecializedFromItem;
    if SrcSpecializedFromItem=nil then
      exit(false);
    if DestSpecializedFromItem=nil then
      exit(false);
    if SrcSpecializedFromItem.GenericEl<>DestSpecializedFromItem.GenericEl then
      exit(false);
    // specialized from same generic -> check params
    SrcParams:=SrcSpecializedFromItem.Params;
    DestParams:=DestSpecializedFromItem.Params;
    for i:=0 to length(SrcParams)-1 do
      begin
      SrcParam:=ResolveAliasType(SrcParams[i]);
      DestParam:=ResolveAliasType(DestParams[i]);
      if (SrcParam is TPasGenericTemplateType)
          or (DestParam is TPasGenericTemplateType)
          or (SrcParam=DestParam)
      then
        // ok
      else if (SrcParam is TPasPointerType) and (DestParam is TPasPointerType)
          and (TPasPointerType(SrcParam).DestType<>nil)
          and (TPasPointerType(DestParam).DestType<>nil)
          and IsSameType(ResolveAliasType(TPasPointerType(SrcParam).DestType),
               ResolveAliasType(TPasPointerType(DestParam).DestType),prraAlias) then
        // two pointer types to the same type, as fpc: rtl-generics'
        // TEnumerator<PKey> is a TEnumerator<PT> when TKey and T are both String
      else if (SrcParam is TPasGenericType) and (DestParam is TPasGenericType) then
        begin
        // e.g. TList<Src<...>> and TList<Dest<...>>
        SrcParamScope:=SrcParam.CustomData as TPasGenericScope;
        DestParamScope:=DestParam.CustomData as TPasGenericScope;
        if not CheckSpecialized(SrcParamScope,DestParamScope) then
          exit(false);
        end
      else
        exit(false); // specialized with different params -> incompatible
      end;
    Result:=true;
  end;

var
  SrcClassEl: TPasClassType;
  SrcScope, DestScope: TPasClassScope;
  GenericType: TPasGenericType;
  TemplType: TPasGenericTemplateType;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckClassIsClass SrcType=',GetObjName(SrcType),' DestType=',GetObjName(DestType));
  {$ENDIF}
  if DestType=nil then exit(cIncompatible);
  DestType:=ResolveAliasType(DestType);
  if DestType.ClassType<>TPasClassType then
    exit(cIncompatible);
  DestScope:=DestType.CustomData as TPasClassScope;
  if DestScope=nil then
    // A specialization whose interface is still being built has no scope yet, so
    // nothing can be decided about it here.
    exit(cIncompatible);

  Result:=cExact;
  while SrcType<>nil do
    begin
    {$IFDEF VerbosePasResolver}
    writeln(' Step=',Result,' SrcType=',GetObjName(SrcType),' DestType=',GetObjName(DestType));
    {$ENDIF}
    if SrcType=DestType then
      exit
    else if SrcType.ClassType=TPasAliasType then
      // alias -> skip
      SrcType:=TPasAliasType(SrcType).DestType
    else if SrcType.ClassType=TPasTypeAliasType then
      begin
      // type alias -> increase distance
      SrcType:=TPasAliasType(SrcType).DestType;
      inc(Result);
      end
    else if SrcType.ClassType=TPasSpecializeType then
      begin
      // specialize -> skip
      if SrcType.CustomData is TPasSpecializeTypeData then
        SrcType:=TPasSpecializeTypeData(SrcType.CustomData).SpecializedType
      else
        SrcType:=TPasSpecializeType(SrcType).DestType;
      end
    else if SrcType.ClassType=TPasClassType then
      begin
      SrcClassEl:=TPasClassType(SrcType);
      if SrcClassEl.IsForward then
        begin
        // class forward -> follow to the full declaration, when resolved
        if SrcClassEl.CustomData is TResolvedReference then
          SrcType:=TResolvedReference(SrcClassEl.CustomData).Declaration as TPasType
        else if DestScope.DirectAncestor=nil then
          // Forward not yet linked to its full definition (e.g. specialized
          // before its body is parsed, tgenconstraint37). Its ancestry is
          // unknown, but every class descends from the root class (TObject),
          // so a forward satisfies only a root-class destination.
          exit
        else
          // A specific (non-root) ancestor cannot be verified against an
          // unresolved forward -> not provably related (FPC rejects this,
          // tgenconstraint39).
          exit(cIncompatible);
        end
      else
        begin
        if (SrcClassEl.GenericTemplateTypes<>nil) and (SrcClassEl.GenericTemplateTypes.Count>0) then
          begin
          // SrcType is a generic
          if DestScope.SpecializedFromItem<>nil then
            begin
            // DestType is specialized
            GenericType:=TPasGenericType(DestScope.SpecializedFromItem.GenericEl);
            {$IFDEF VerbosePasResolver}
            writeln(' DestType is specialized from ',GetObjName(GenericType));
            {$ENDIF}
            if SrcType=GenericType then
              exit; // DestType is a specialized SrcType
            end;
          end;
        SrcScope:=SrcClassEl.CustomData as TPasClassScope;
        if SrcScope=nil then
          // A specialization whose interface is still being built has no scope
          // yet, so nothing can be decided about it here.
          exit(cIncompatible);
        if (SrcScope.SpecializedFromItem<>nil)
            and (DestScope.SpecializedFromItem<>nil)
            and CheckSpecialized(SrcScope,DestScope) then
          exit;

        // class ancestor -> increase distance
        SrcType:=SrcScope.DirectAncestor;
        inc(Result);
        end;
      end
    else if SrcType is TPasGenericTemplateType then
      begin
      // class(T) inside a generic: T stands for its class-type constraint
      // (lazutils' TFreeNotifyingGeneric<_B: TObject> = class(_B) passes Self as
      // a TObject). With only a "class" constraint, T is certain to be a root
      // class descendant and nothing more.
      TemplType:=TPasGenericTemplateType(SrcType);
      SrcType:=GetClassTypeConstraint(TemplType);
      if SrcType=nil then
        begin
        if HasClassConstraint(TemplType) and (DestScope.DirectAncestor=nil) then
          exit;
        exit(cIncompatible);
        end;
      inc(Result);
      end
    else
      exit(cIncompatible);
    end;
  Result:=cIncompatible;
end;

function TPasResolver.CheckClassesAreRelated(TypeA, TypeB: TPasType): integer;
begin
  Result:=CheckClassIsClass(TypeA,TypeB);
  if Result<>cIncompatible then exit;
  Result:=CheckClassIsClass(TypeB,TypeA);
end;

function TPasResolver.CheckAssignCompatibilityClasses(LType,
  RType: TPasClassType): integer;
begin
  Result:=cIncompatible;
  if LType=nil then ;
  if RType=nil then ;
end;

function TPasResolver.GetClassImplementsIntf(ClassEl, Intf: TPasClassType
  ): TPasClassType;
begin
  Result:=nil;
  while ClassEl<>nil do
    begin
    if (ClassEl=Intf) or (IndexOfImplementedInterface(ClassEl,Intf)>=0) then
      exit(ClassEl);
    ClassEl:=GetPasClassAncestor(ClassEl,true) as TPasClassType;
    end;
end;
