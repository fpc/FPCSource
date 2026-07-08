{
  This file is part of the Free Component Libraries (FCL)
  Copyright (c) 2026 by the Free Pascal development team

  Native-target resolver extensions for fcl-passrc.

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  TPasNativeResolver descends from TPasResolver and adds the resolver semantics
  that apply to native Object Pascal compilation targets but not to the pas2js
  (JavaScript) target. This wasy the base TPasResolver stays pas2js-clean and 
  a native consumer opts in by instantiating this class instead:

  * compile-time evaluation of the size/truncation intrinsics
    SizeOf, BitSizeOf, Trunc, Round 
    (registered via AddBuiltInProc, so they are absent from the base setup);
  * the FPC compiler-intrinsic type names Boolean8/16/32/64, OleVariant and
    TypedFile, added as aliases of existing base types.
  * More to come...   

  See the header of pasresolver.pp for the general resolver contract.

}
{$IFNDEF FPC_DOTTEDUNITS}
unit pasnativeresolve;
{$ENDIF}

{$i fcl-passrc.inc}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, Pascal.Tree, Pascal.ResolveEval, Pascal.Resolver;
{$ELSE}
uses
  Classes, SysUtils, PasTree, PasResolveEval, PasResolver;
{$ENDIF}  

type

  { TPasNativeResolver }

  TPasNativeResolver = class(TPasResolver)
  private
    FTargetPointerSize: Integer;
    // SizeOf/BitSizeOf/Trunc/Round all take exactly one argument;
    function BI_OneParam_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer;
    procedure BI_SizeOf_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
    procedure BI_SizeOf_OnEval(Proc: TResElDataBuiltInProc; Params: TParamsExpr;
      Flags: TResEvalFlags; out Evaluated: TResEvalValue);
    procedure BI_TruncRound_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
    procedure BI_TruncRound_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);
    // Registers SizeOf/BitSizeOf/Trunc/Round (idempotent) 
    procedure AddNativeConstEvalIntrinsics;
    // Registers the FPC compiler-intrinsic type names as base-type aliases
    procedure AddNativeIntrinsicTypes;
  protected
    // Target-correct byte size of aType in bytes, or -1 when not determinable
    // (aggregate/opaque/target-dependent float). Uses TargetPointerSize for the
    // pointer family. A native consumer may query this for {$if sizeof(T)} folds.
    function TypeByteSize(aType: TPasType): Integer;
    // Folds a constant integer cast to a pointer base type (e.g. pointer(-1) -> -1
    // Non-integer/non-constant operands degrade to nil.
    function EvalNativePointerCast(Params: TParamsExpr;
      bt: TResolverBaseType): TResEvalValue; override;
    // Folds a constant integer cast to a named pointer type (e.g. PAnsiChar(1) -> 1). 
    // A non-constant operand degrades to nil.
    function EvalNativeNamedPointerCast(Params: TParamsExpr): TResEvalValue; override;
    // Degrades an address-of (@X) to an opaque pointer value (revkNil = unknown)
    function EvalNativeAddressOf(Expr: TPrimitiveExpr;
      Flags: TResEvalFlags): TResEvalValue; override;
    // Native memory-layout packing directives, stored per element
    procedure SetMinEnumSize(El: TPasElement; ASize: Integer); override;
    function GetMinEnumSize(El: TPasElement): Integer; override;
    procedure SetPackSet(El: TPasElement; ASize: Integer); override;
    function GetPackSet(El: TPasElement): Integer; override;
    procedure SetPackRecords(El: TPasElement; ASize: Integer); override;
    function GetPackRecords(El: TPasElement): Integer; override;
    (* 
      Native pointer arithmetic: 
      the untyped Pointer, a pointer declared under {$POINTERMATH ON} (pesfPointerMath), 
      or a ^(Ansi)Char pointer permits +/- and indexing without the use-site switch. 
    *)
    function IsPointerMathType(El: TPasType): Boolean; override;
    // Native target: Inc/Dec is allowed on any pointer, switch-independent.
    function AllowIncDecOnPointer(El: TPasType): Boolean; override;
    // Detects a bit-packed ordinal array element / record field access (an ordinal whose packed bit width is not a whole number of bytes).
    function IsBitPackedOrdinalAccess(Expr: TPasExpr): boolean; override;
  public
    // Injects the native scope subclasses that carry the packing values.
    constructor Create; reintroduce;
    // Byte size of an enum's storage (1/2/4 by value count, honouring {$MINENUMSIZE}).
    function GetEnumTypeSize(EnumType: TPasEnumType): Integer;
    // Bit width of a bit-packed ordinal access (0 if Expr is not one). 
    function BitPackedOrdinalBitSize(Expr: TPasExpr): integer;
    // Adds the base ObjFPC identifiers, then the native-only const-eval intrinsics and compiler-intrinsic type names.
    procedure AddObjFPCBuiltInIdentifiers(
      const TheBaseTypes: TResolveBaseTypes = btAllFPCTypes;
      const TheBaseProcs: TResolverBuiltInProcs = bfAllStandardProcs); override;
    // The target pointer size in bytes used by SizeOf folds; 0 (default) uses the host SizeOf(Pointer).
    property TargetPointerSize: Integer read FTargetPointerSize write FTargetPointerSize;
  end;

implementation

type
  { Native scope subclasses that carry the packing value with the element, so
    it persists across resolvers with no process-global. }
  TNativeEnumTypeScope = class(TPasEnumTypeScope)
  public
    MinEnumSize: Integer;
  end;
  TNativeRecordScope = class(TPasRecordScope)
  public
    PackRecords: Integer;
  end;
  { A set type owns no scope, so its pack-set value rides on a resolve-data
    attached to the otherwise-free CustomData slot. }
  TNativePackSetData = class(TResolveData)
  public
    PackSet: Integer;
  end;

procedure TPasNativeResolver.AddObjFPCBuiltInIdentifiers(
  const TheBaseTypes: TResolveBaseTypes; const TheBaseProcs: TResolverBuiltInProcs);

begin
  inherited AddObjFPCBuiltInIdentifiers(TheBaseTypes, TheBaseProcs);
  AddNativeConstEvalIntrinsics;
  AddNativeIntrinsicTypes;
end;


procedure TPasNativeResolver.AddNativeConstEvalIntrinsics;

begin
  if BuiltInProcs[bfSizeOf]=nil then
    AddBuiltInProc('SizeOf','function SizeOf(const TypeOrVar): sizeint',
        @BI_OneParam_OnGetCallCompatibility,@BI_SizeOf_OnGetCallResult,
        @BI_SizeOf_OnEval,nil,bfSizeOf);
  if BuiltInProcs[bfBitSizeOf]=nil then
    AddBuiltInProc('BitSizeOf','function BitSizeOf(const TypeOrVar): sizeint',
        @BI_OneParam_OnGetCallCompatibility,@BI_SizeOf_OnGetCallResult,
        @BI_SizeOf_OnEval,nil,bfBitSizeOf);
  if BuiltInProcs[bfTrunc]=nil then
    AddBuiltInProc('Trunc','function Trunc(const Float): Int64',
        @BI_OneParam_OnGetCallCompatibility,@BI_TruncRound_OnGetCallResult,
        @BI_TruncRound_OnEval,nil,bfTrunc);
  if BuiltInProcs[bfRound]=nil then
    AddBuiltInProc('Round','function Round(const Float): Int64',
        @BI_OneParam_OnGetCallCompatibility,@BI_TruncRound_OnGetCallResult,
        @BI_TruncRound_OnEval,nil,bfRound);
end;


procedure TPasNativeResolver.AddNativeIntrinsicTypes;

  procedure AddAlias(const aName: string; Typ: TResolverBaseType);
  var
    El: TPasUnresolvedSymbolRef;
    Data: TResElDataBaseType;
  begin
    El:=TPasUnresolvedSymbolRef(CreateOwnedElement(TPasUnresolvedSymbolRef,aName,nil));
    Data:=TResElDataBaseType.Create;
    Data.BaseType:=Typ;
    AddResolveData(El,Data,lkBuiltIn);
    DefaultScope.AddIdentifier(aName,El,pikBaseType);
  end;

begin
  AddAlias('Boolean8',btByteBool);
  AddAlias('Boolean16',btWordBool);
  AddAlias('Boolean32',btLongBool);
  {$ifdef HasInt64}
  AddAlias('Boolean64',btQWordBool);
  {$endif}
  {$ifdef fpc}
  AddAlias('OleVariant',btVariant);
  AddAlias('TypedFile',btFile);
  {$endif}
end;


function TPasNativeResolver.TypeByteSize(aType: TPasType): Integer;

var
  lEl: TPasType;
  lClass: TClass;
  lPtrSize: Integer;

begin
  Result:=-1;
  if aType=nil then exit;
  if FTargetPointerSize>0 then
    lPtrSize:=FTargetPointerSize
  else
    lPtrSize:=SizeOf(Pointer);
  lEl:=ResolveAliasType(aType);
  if lEl=nil then exit;
  lClass:=lEl.ClassType;
  if lClass=TPasUnresolvedSymbolRef then
    begin
    if lEl.CustomData is TResElDataBaseType then
      case TResElDataBaseType(lEl.CustomData).BaseType of
        btByte,btShortInt,btBoolean,btByteBool,
        btChar{$ifdef FPC_HAS_CPSTRING},btAnsiChar{$endif}:
          Result:=1;
        btWord,btSmallInt,btWordBool,btWideChar:
          Result:=2;
        btLongWord,btLongint,btLongBool,btSingle:
          Result:=4;
        btDouble,btCurrency:
          Result:=8;
        btExtended,btCExtended:
          // Target-dependent width and no target float-width input here.
          Result:=-1;
        {$ifdef HasInt64}
        btQWord,btInt64,btComp,btQWordBool:
          Result:=8;
        {$endif}
        btPointer:
          Result:=lPtrSize;
      else
        Result:=-1; // strings, variant, file, ... — degrade
      end;
    end
  else if lClass=TPasPointerType then
    Result:=lPtrSize
  else if lClass=TPasClassType then
    Result:=lPtrSize
  else if lClass=TPasClassOfType then
    Result:=lPtrSize
  else
    Result:=-1; // record/array/set/enum/proc/... — degrade
end;


function TPasNativeResolver.BI_OneParam_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;

var
  Params: TParamsExpr;

begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);
  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;


procedure TPasNativeResolver.BI_SizeOf_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);

begin
  SetResolverIdentifier(ResolvedEl,btInt64,Proc.Proc,
    BaseTypes[btInt64],BaseTypes[btInt64],[rrfReadable]);
  if Params=nil then ;
end;


procedure TPasNativeResolver.BI_SizeOf_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);

var
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  lSize: Integer;

begin
  Evaluated:=nil;
  if (Params=nil) or (length(Params.Params)<1) then exit;
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  lSize:=TypeByteSize(ParamResolved.LoTypeEl);
  if lSize<0 then exit; // not determinable -> degrade (never a wrong size)
  if Proc.BuiltIn=bfBitSizeOf then
    lSize:=lSize*8;      // non-bitpacked: BitSizeOf = SizeOf*8
  Evaluated:=TResEvalInt.CreateValue(lSize);
  if Flags=[] then ;
end;


procedure TPasNativeResolver.BI_TruncRound_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);

begin
  SetResolverIdentifier(ResolvedEl,btInt64,Proc.Proc,
    BaseTypes[btInt64],BaseTypes[btInt64],[rrfReadable]);
  if Params=nil then ;
end;


procedure TPasNativeResolver.BI_TruncRound_OnEval(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue);

var
  Param: TPasExpr;
  Value: TResEvalValue;
  lFloat: TMaxPrecFloat;
  lInt: TMaxPrecInt;

begin
  Evaluated:=nil;
  if (Params=nil) or (length(Params.Params)<1) then exit;
  Param:=Params.Params[0];
  Value:=Eval(Param,Flags);
  if Value=nil then exit;
  try
    case Value.Kind of
    revkFloat:
      lFloat:=TResEvalFloat(Value).FloatValue;
    revkCurrency:
      lFloat:=TResEvalCurrency(Value).Value;
    else
      exit; // non-float/currency argument -> degrade
    end;
    // FPC semantics: Trunc truncates toward zero; Round is round-half-to-even.
    if Proc.BuiltIn=bfTrunc then
      lInt:=Trunc(lFloat)
    else
      lInt:=Round(lFloat);
    Evaluated:=TResEvalInt.CreateValue(lInt);
  finally
    ReleaseEvalValue(Value);
  end;
end;


function TPasNativeResolver.EvalNativePointerCast(Params: TParamsExpr;
  bt: TResolverBaseType): TResEvalValue;

var
  lVal: TResEvalValue;

begin
  Result:=nil;
  if bt<>btPointer then exit;
  if (Params=nil) or (length(Params.Params)<1) then exit;
  lVal:=Eval(Params.Params[0],[refConst]);
  if lVal=nil then exit;
  try
    if lVal.Kind=revkInt then
      Result:=TResEvalInt.CreateValue(TResEvalInt(lVal).Int);
  finally
    ReleaseEvalValue(lVal);
  end;
end;


function TPasNativeResolver.EvalNativeNamedPointerCast(Params: TParamsExpr): TResEvalValue;

var
  lVal: TResEvalValue;

begin
  Result:=nil;
  if (Params=nil) or (length(Params.Params)<1) then exit;
  // A non-constant operand raises EPasResolve ('constant expression expected');
  // swallow it so an ordinary PChar(someVar) cast resolves as a runtime (non-
  // const) expression rather than failing the unit. 
  lVal:=nil;
  try
    lVal:=Eval(Params.Params[0],[refConst]);
  except
    on E: EPasResolve do
      lVal:=nil;
  end;
  if (lVal<>nil) and (lVal.Kind=revkInt) then
    Result:=TResEvalInt.CreateValue(TResEvalInt(lVal).Int);
  ReleaseEvalValue(lVal);
end;


function TPasNativeResolver.EvalNativeAddressOf(Expr: TPrimitiveExpr;
  Flags: TResEvalFlags): TResEvalValue;

begin
  Result:=nil;
  if (Expr<>nil) and (Expr.Parent is TUnaryExpr)
      and (TUnaryExpr(Expr.Parent).OpCode=eopAddress) then
    Result:=TResEvalValue.CreateKind(revkNil);
  if Flags=[] then ;
end;


constructor TPasNativeResolver.Create;

begin
  inherited Create;
  ScopeClass_EnumType:=TNativeEnumTypeScope;
  ScopeClass_Record:=TNativeRecordScope;
end;


procedure TPasNativeResolver.SetMinEnumSize(El: TPasElement; ASize: Integer);

begin
  if (El<>nil) and (ASize>0) and (El.CustomData is TNativeEnumTypeScope) then
    TNativeEnumTypeScope(El.CustomData).MinEnumSize:=ASize;
end;


function TPasNativeResolver.GetMinEnumSize(El: TPasElement): Integer;

begin
  if (El<>nil) and (El.CustomData is TNativeEnumTypeScope) then
    Result:=TNativeEnumTypeScope(El.CustomData).MinEnumSize
  else
    Result:=0;
end;


procedure TPasNativeResolver.SetPackRecords(El: TPasElement; ASize: Integer);

begin
  if (El<>nil) and (ASize>0) and (El.CustomData is TNativeRecordScope) then
    TNativeRecordScope(El.CustomData).PackRecords:=ASize;
end;


function TPasNativeResolver.GetPackRecords(El: TPasElement): Integer;

begin
  if (El<>nil) and (El.CustomData is TNativeRecordScope) then
    Result:=TNativeRecordScope(El.CustomData).PackRecords
  else
    Result:=0;
end;


procedure TPasNativeResolver.SetPackSet(El: TPasElement; ASize: Integer);

var
  Data: TNativePackSetData;

begin
  if (El=nil) or (ASize<=0) then exit;
  if El.CustomData is TNativePackSetData then
    TNativePackSetData(El.CustomData).PackSet:=ASize
  else if El.CustomData=nil then
    begin
    Data:=TNativePackSetData.Create;
    Data.PackSet:=ASize;
    AddResolveData(El,Data,lkModule);
    end;
end;


function TPasNativeResolver.GetPackSet(El: TPasElement): Integer;

begin
  if (El<>nil) and (El.CustomData is TNativePackSetData) then
    Result:=TNativePackSetData(El.CustomData).PackSet
  else
    Result:=0;
end;


function TPasNativeResolver.GetEnumTypeSize(EnumType: TPasEnumType): Integer;

var
  Count, MinSize: Integer;

begin
  Count:=0;
  if (EnumType<>nil) and (EnumType.Values<>nil) then
    Count:=EnumType.Values.Count;
  if Count<=256 then
    Result:=1
  else if Count<=65536 then
    Result:=2
  else
    Result:=4;
  MinSize:=GetMinEnumSize(EnumType);
  if MinSize>Result then
    Result:=MinSize;
end;


function TPasNativeResolver.IsPointerMathType(El: TPasType): Boolean;

var
  lEl, lDest: TPasType;

begin
  Result:=False;
  if El=nil then exit;
  lEl:=ResolveAliasType(El);
  if lEl=nil then exit;
  // untyped Pointer: +/- always allowed on the native target
  if (lEl.CustomData is TResElDataBaseType)
      and (TResElDataBaseType(lEl.CustomData).BaseType=btPointer) then
    exit(True);
  if lEl.ClassType=TPasPointerType then
    begin
    // a pointer type declared while {$POINTERMATH ON} was active
    if pesfPointerMath in lEl.States then exit(True);
    // structural PChar-like: a pointer to (Ansi)Char
    lDest:=ResolveAliasType(TPasPointerType(lEl).DestType);
    if (lDest<>nil) and (lDest.CustomData is TResElDataBaseType)
        and (TResElDataBaseType(lDest.CustomData).BaseType
             in [btChar{$ifdef FPC_HAS_CPSTRING},btAnsiChar{$endif}]) then
      exit(True);
    end;
end;


function TPasNativeResolver.AllowIncDecOnPointer(El: TPasType): Boolean;

begin
  // native target: Inc/Dec is allowed on any pointer, switch-independent
  Result:=True;
  if El=nil then ;
end;


function TPasNativeResolver.IsBitPackedOrdinalAccess(Expr: TPasExpr): boolean;

  function PackedBitsForRange(lo, hi: TMaxPrecInt): integer;
  var
    v: TMaxPrecInt;
  begin
    if lo>=0 then
      begin
      // unsigned 0..hi: bits to represent hi (at least 1)
      Result:=1;
      v:=hi;
      while v>1 do
        begin
        inc(Result);
        v:=v shr 1;
        end;
      end
    else
      begin
      // signed lo..hi: smallest b with -2^(b-1)<=lo and hi<=2^(b-1)-1
      Result:=2;
      while (Result<64)
          and ((lo < -(TMaxPrecInt(1) shl (Result-1)))
            or (hi > (TMaxPrecInt(1) shl (Result-1))-1)) do
        inc(Result);
      end;
  end;

  function OrdNeedsBitAccess(aType: TPasType): boolean;
  var
    V: TResEvalValue;
    Bits: integer;
  begin
    Result:=false;
    if aType=nil then exit;
    V:=EvalTypeRange(aType,[]);
    try
      if not (V is TResEvalRangeInt) then exit; // not an ordinal type
      Bits:=PackedBitsForRange(TResEvalRangeInt(V).RangeStart,
                               TResEvalRangeInt(V).RangeEnd);
      Result:=not (Bits in [8,16,32,64]);
    finally
      ReleaseEvalValue(V);
    end;
  end;

var
  Resolved: TPasResolverResult;
  ArrType: TPasType;
  IdentEl: TPasElement;

begin
  Result:=false;
  if Expr=nil then exit;
  if (Expr is TParamsExpr) and (TParamsExpr(Expr).Kind=pekArrayParams) then
    begin
    // a[i]: check the indexed array
    ComputeElement(TParamsExpr(Expr).Value,Resolved,[rcNoImplicitProc]);
    ArrType:=ResolveAliasType(Resolved.LoTypeEl);
    if (ArrType is TPasArrayType)
        and (TPasArrayType(ArrType).PackMode=pmBitPacked) then
      Result:=OrdNeedsBitAccess(TPasArrayType(ArrType).ElType);
    end
  else
    begin
    // r.f: check the accessed field's owning record
    ComputeElement(Expr,Resolved,[rcNoImplicitProc]);
    IdentEl:=Resolved.IdentEl;
    if (IdentEl is TPasVariable)
        and (IdentEl.Parent is TPasRecordType)
        and (TPasRecordType(IdentEl.Parent).PackMode=pmBitPacked) then
      Result:=OrdNeedsBitAccess(TPasVariable(IdentEl).VarType);
    end;
end;


function TPasNativeResolver.BitPackedOrdinalBitSize(Expr: TPasExpr): integer;

  function PackedBitsForRange(lo, hi: TMaxPrecInt): integer;
  var
    v: TMaxPrecInt;
  begin
    if lo>=0 then
      begin
      Result:=1;
      v:=hi;
      while v>1 do
        begin
        inc(Result);
        v:=v shr 1;
        end;
      end
    else
      begin
      Result:=2;
      while (Result<64)
          and ((lo < -(TMaxPrecInt(1) shl (Result-1)))
            or (hi > (TMaxPrecInt(1) shl (Result-1))-1)) do
        inc(Result);
      end;
  end;

  function PackedBitsForUInt(hi: TMaxPrecUInt): integer;
  var
    v: TMaxPrecUInt;
  begin
    Result:=1;
    v:=hi;
    while v>1 do
      begin
      inc(Result);
      v:=v shr 1;
      end;
  end;

  function OrdBitSize(aType: TPasType): integer;
  var
    V: TResEvalValue;
  begin
    Result:=0;
    if aType=nil then exit;
    V:=EvalTypeRange(aType,[]);
    try
      // An unsigned range whose upper bound exceeds High(Int64) evaluates to a
      // TResEvalRangeUInt.
      if V is TResEvalRangeUInt then
        Result:=PackedBitsForUInt(TResEvalRangeUInt(V).RangeEnd)
      else if V is TResEvalRangeInt then
        Result:=PackedBitsForRange(TResEvalRangeInt(V).RangeStart,
                                   TResEvalRangeInt(V).RangeEnd);
      // else: not an ordinal type -> 0
    finally
      ReleaseEvalValue(V);
    end;
  end;

var
  Resolved: TPasResolverResult;
  ArrType: TPasType;
  IdentEl: TPasElement;

begin
  Result:=0;
  if Expr=nil then exit;
  if (Expr is TParamsExpr) and (TParamsExpr(Expr).Kind=pekArrayParams) then
    begin
    ComputeElement(TParamsExpr(Expr).Value,Resolved,[rcNoImplicitProc]);
    ArrType:=ResolveAliasType(Resolved.LoTypeEl);
    if (ArrType is TPasArrayType)
        and (TPasArrayType(ArrType).PackMode=pmBitPacked) then
      Result:=OrdBitSize(TPasArrayType(ArrType).ElType);
    end
  else
    begin
    ComputeElement(Expr,Resolved,[rcNoImplicitProc]);
    IdentEl:=Resolved.IdentEl;
    if (IdentEl is TPasVariable)
        and (IdentEl.Parent is TPasRecordType)
        and (TPasRecordType(IdentEl.Parent).PackMode=pmBitPacked) then
      Result:=OrdBitSize(TPasVariable(IdentEl).VarType);
    end;
end;

end.
