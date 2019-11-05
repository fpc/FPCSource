{
  This file is part of the Free Pascal run time library.
  Copyright (C) 2018 Sven Barth
  member of the Free Pascal development team.

  Function call manager for x86_64

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

resourcestring
  SErrPlatformNotSupported = 'Invoke is not supported on this platform';

{$define SYSTEM_HAS_INVOKE}

{$ifdef windows}
function InvokeKernelWin64(aArgsStackSize: PtrUInt; aArgsStack, aArgsReg: Pointer; aFunc: CodePointer): PtrUInt; assembler; nostackframe;
asm
  { save non-volatile registers in shadow space }
  movq %rbp, 8(%rsp)
.seh_savereg %rbp, 8
  movq %rsi, 16(%rsp)
.seh_savereg %rsi, 16
  movq %rdi, 24(%rsp)
.seh_savereg %rdi, 24
  movq %r8, 32(%rsp)
.seh_savereg %r8, 32

  movq %rsp, %rbp
.seh_setframe %rbp, 0
.seh_endprologue

  { align stack size to 16 Byte }
  sub aArgsStackSize, %rsp
  and $-16, %rsp

  movq aArgsStackSize, %rax

  { copy the stack arguments as QWord entries }
  shr $3, %rcx

  mov %rdx, %rsi
  mov %rsp, %rdi
  mov %r9,  %rax

  cld
  rep movsq

  { setup general purpose registers }
  movq 0(%r8), %rcx
  movq 8(%r8), %rdx
  movq 24(%r8), %r9
  movq 16(%r8), %r8

  { also setup SSE2 registers }
  movq %rcx, %xmm0
  movq %rdx, %xmm1
  movq %r8 , %xmm2
  movq %r9 , %xmm3

  { provide shadow space }
  sub $32, %rsp

  { now call the function }
  call *%rax

  { restore non-volatile registers }
  movq %rbp, %rsp

  { we abuse the register area pointer for an eventual SSE2 result }
  movq 32(%rsp), %rdi
  movq %xmm0, (%rdi)

  movq 24(%rsp), %rdi
  movq 16(%rsp), %rsi
  movq 8(%rsp), %rbp
end;
{$endif}

resourcestring
  SErrFailedToConvertArg = 'Failed to convert argument %d of type %s';
  SErrFailedToConvertRes = 'Failed to convert result of type %s';

function ReturnResultInParam(aType: PTypeInfo): Boolean;
var
  td: PTypeData;
begin
  Result := False;
  if Assigned(aType) then begin
    case aType^.Kind of
      tkMethod,
      tkSString,
      tkAString,
      tkUString,
      tkWString,
      tkInterface,
      tkDynArray:
        Result := True;
      tkArray: begin
        td := GetTypeData(aType);
        Result := not (td^.ArrayData.Size in [1, 2, 4, 8]);
      end;
      tkRecord: begin
        td := GetTypeData(aType);
        Result := not (td^.RecSize in [1, 2, 4, 8]);
      end;
      tkSet: begin
        td := GetTypeData(aType);
        case td^.OrdType of
          otUByte:
            Result := not (td^.SetSize in [1, 2, 4, 8]);
          otUWord,
          otULong:
            Result := False;
        end;
      end;
    end;
  end;
end;

procedure SystemInvoke(aCodeAddress: CodePointer; const aArgs: TFunctionCallParameterArray; aCallConv: TCallConv;
            aResultType: PTypeInfo; aResultValue: Pointer; aFlags: TFunctionCallFlags);
var
  stackarea: array of PtrUInt;
  stackptr: Pointer;
  regs: array[0..3] of PtrUInt;
  i, regidx, stackidx: LongInt;
  val: PtrUInt;
  td: PTypeData;
  retinparam: Boolean;
  argcount, resreg: SizeInt;
begin
  if Assigned(aResultType) and not Assigned(aResultValue) then
    raise EInvocationError.Create(SErrInvokeResultTypeNoValue);
{$ifdef windows}
  retinparam := ReturnResultInParam(aResultType);

  stackidx := 0;
  regidx := 0;
  argcount := Length(aArgs);
  if retinparam then begin
    if fcfStatic in aFlags then
      resreg := 0
    else
      resreg := 1;
    regs[resreg] := PtrUInt(aResultValue);
    Inc(argcount);
  end else
    resreg := -1;
  if argcount > 4 then
    SetLength(stackarea, argcount - 4);
  for i := 0 to High(aArgs) do begin
    if pfArray in aArgs[i].Info.ParamFlags then
      val := PtrUInt(aArgs[i].ValueRef)
    else if aArgs[i].Info.ParamFlags * [pfOut, pfVar, pfConstRef] <> [] then
      val := PtrUInt(aArgs[i].ValueRef)
    else if (pfConst in aArgs[i].Info.ParamFlags) and not Assigned(aArgs[i].Info.ParamType) then
      val := PtrUInt(aArgs[i].ValueRef)
    else begin
      td := GetTypeData(aArgs[i].Info.ParamType);
      case aArgs[i].Info.ParamType^.Kind of
        tkSString,
        tkMethod:
          val := PtrUInt(aArgs[i].ValueRef);
        tkArray:
          if td^.ArrayData.Size in [1, 2, 4, 8] then
            val := PPtrUInt(aArgs[i].ValueRef)^
          else
            val := PtrUInt(aArgs[i].ValueRef);
        tkRecord:
          if td^.RecSize in [1, 2, 4, 8] then
            val := PPtrUInt(aArgs[i].ValueRef)^
          else
            val := PtrUInt(aArgs[i].ValueRef);
        { ToDo: handle object like record? }
        tkObject,
        tkWString,
        tkUString,
        tkAString,
        tkDynArray,
        tkClass,
        tkClassRef,
        tkInterface,
        tkInterfaceRaw,
        tkProcVar,
        tkPointer:
          val := PPtrUInt(aArgs[i].ValueRef)^;
        tkInt64,
        tkQWord:
          val := PInt64(aArgs[i].ValueRef)^;
        tkSet: begin
          case td^.OrdType of
            otUByte: begin
              case td^.SetSize of
                0, 1:
                  val := PByte(aArgs[i].ValueRef)^;
                2:
                  val := PWord(aArgs[i].ValueRef)^;
                3:
                  val := PtrUInt(aArgs[i].ValueRef);
                4:
                  val := PLongWord(aArgs[i].ValueRef)^;
                5..7:
                  val := PtrUInt(aArgs[i].ValueRef);
                8:
                  val := Int64(PQWord(aArgs[i].ValueRef)^);
                else
                  val := PtrUInt(aArgs[i].ValueRef);
              end;
            end;
            otUWord:
              val := PWord(aArgs[i].ValueRef)^;
            otULong:
              val := PLongWord(aArgs[i].ValueRef)^;
          end;
        end;
        tkEnumeration,
        tkInteger: begin
          case td^.OrdType of
            otSByte: val := PShortInt(aArgs[i].ValueRef)^;
            otUByte: val := PByte(aArgs[i].ValueRef)^;
            otSWord: val := PSmallInt(aArgs[i].ValueRef)^;
            otUWord: val := PWord(aArgs[i].ValueRef)^;
            otSLong: val := PLongInt(aArgs[i].ValueRef)^;
            otULong: val := PLongWord(aArgs[i].ValueRef)^;
          end;
        end;
        tkBool: begin
          case td^.OrdType of
            otUByte: val := ShortInt(System.PBoolean(aArgs[i].ValueRef)^);
            otUWord: val := Byte(PBoolean16(aArgs[i].ValueRef)^);
            otULong: val := SmallInt(PBoolean32(aArgs[i].ValueRef)^);
            otUQWord: val := QWord(PBoolean64(aArgs[i].ValueRef)^);
            otSByte: val := Word(PByteBool(aArgs[i].ValueRef)^);
            otSWord: val := LongInt(PWordBool(aArgs[i].ValueRef)^);
            otSLong: val := LongWord(PLongBool(aArgs[i].ValueRef)^);
            otSQWord: val := Int64(PQWordBool(aArgs[i].ValueRef)^);
          end;
        end;
        tkFloat: begin
          case td^.FloatType of
            ftCurr   : val := PInt64(PCurrency(aArgs[i].ValueRef))^;
            ftSingle : val := PInt64(PSingle(aArgs[i].ValueRef))^;
            ftDouble : val := PInt64(PDouble(aArgs[i].ValueRef))^;
            ftExtended: val := PInt64(PExtended(aArgs[i].ValueRef))^;
            ftComp   : val := PInt64(PComp(aArgs[i].ValueRef))^;
          end;
        end;
      else
        raise EInvocationError.CreateFmt(SErrFailedToConvertArg, [i, aArgs[i].Info.ParamType^.Name]);
      end;
    end;

    if regidx = resreg then
      Inc(regidx);

    if regidx < 4 then begin
      regs[regidx] := val;
      Inc(regidx);
    end else begin
      stackarea[stackidx] := val;
      Inc(stackidx);
    end;
  end;

  if stackidx > 0 then
    stackptr := @stackarea[0]
  else
    stackptr := Nil;
  val := InvokeKernelWin64(stackidx * SizeOf(PtrUInt), stackptr, @regs[0], aCodeAddress);

  if Assigned(aResultType) and not retinparam then begin
    PPtrUInt(aResultValue)^ := val;
    if aResultType^.Kind = tkFloat then begin
      td := GetTypeData(aResultType);
      if td^.FloatType in [ftSingle, ftDouble] then
        PPtrUInt(aResultValue)^ := regs[0];
    end;
  end;
{$else}
  raise EInvocationError.Create(SErrPlatformNotSupported);
{$endif}
end;

{$ifdef windows}
const
  PlaceholderContext = QWord($1234567812345678);
  PlaceholderAddress = QWord($8765432187654321);

label
  CallbackContext,
  CallbackAddress,
  CallbackCall,
  CallbackEnd;

const
  CallbackContextPtr: Pointer = @CallbackContext;
  CallbackAddressPtr: Pointer = @CallbackAddress;
  CallbackCallPtr: Pointer = @CallbackCall;
  CallbackEndPtr: Pointer = @CallbackEnd;

procedure Callback; assembler; nostackframe;
asm
  { store integer registers }

  movq %rcx, 8(%rsp)
.seh_savereg %rcx, 8
  movq %rdx, 16(%rsp)
.seh_savereg %rdx, 16
  movq %r8,  24(%rsp)
.seh_savereg %r8, 24
  movq %r9,  32(%rsp)
.seh_savereg %r9, 32

  { establish frame }
  pushq %rbp
.seh_pushreg %rbp
  movq %rsp, %rbp
.seh_setframe %rbp, 0
.seh_endprologue

  { store pointer to stack area (including GP registers) }
  lea 16(%rsp), %rdx

  sub $32, %rsp
  movq %xmm0, (%rsp)
  movq %xmm1, 8(%rsp)
  movq %xmm2, 16(%rsp)
  movq %xmm3, 24(%rsp)

  { store pointer to FP registers }
  movq %rsp, %r8

  sub $32, %rsp

  { call function with context }
CallbackContext:
  movq $0x1234567812345678, %rcx
CallbackAddress:
  movq $0x8765432187654321, %rax
CallbackCall:

  call *%rax

  { duplicate result to SSE result register }
  movq %rax, %xmm0

  { restore stack }
  movq %rbp, %rsp
  popq %rbp

  ret
CallbackEnd:
end;
{$endif}

type
  TSystemFunctionCallback = class(TFunctionCallCallback)
  {$ifdef windows}
  private type
    {$ScopedEnums On}
    TArgType = (
      GenReg,
      FPReg,
      Stack
    );
    {$ScopedEnums Off}

    TArgInfo = record
      ArgType: TArgType;
      Offset: SizeInt;
      Deref: Boolean;
    end;
  private
    fData: Pointer;
    fSize: PtrUInt;
    fFlags: TFunctionCallFlags;
    fContext: Pointer;
    fArgs: specialize TArray<TFunctionCallParameterInfo>;
    fArgInfos: specialize TArray<TArgInfo>;
    fRefArgs: specialize TArray<SizeInt>;
    fResultType: PTypeInfo;
    fResultIdx: SizeInt;
    fResultInParam: Boolean;
  private
    function Handler(aStack, aFP: Pointer): PtrUInt;
  protected
    procedure CreateCallback;
    procedure CreateArgInfos;
    function GetCodeAddress: CodePointer; override;
  {$endif}
    procedure CallHandler(constref aArgs: specialize TArray<Pointer>; aResult: Pointer; aContext: Pointer); virtual; abstract;
  public
    constructor Create(aContext: Pointer; aCallConv: TCallConv; constref aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags);
    destructor Destroy; override;
  end;

  TSystemFunctionCallbackMethod = class(TSystemFunctionCallback)
  private
    fHandler: TFunctionCallMethod;
  protected
    procedure CallHandler(constref aArgs: specialize TArray<Pointer>; aResult: Pointer; aContext: Pointer); override;
  public
    constructor Create(aHandler: TFunctionCallMethod; aContext: Pointer; aCallConv: TCallConv; constref aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags);
  end;

  TSystemFunctionCallbackProc = class(TSystemFunctionCallback)
  private
    fHandler: TFunctionCallProc;
  protected
    procedure CallHandler(constref aArgs: specialize TArray<Pointer>; aResult: Pointer; aContext: Pointer); override;
  public
    constructor Create(aHandler: TFunctionCallProc; aContext: Pointer; aCallConv: TCallConv; constref aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags);
  end;

{$ifdef windows}
function TSystemFunctionCallback.Handler(aStack, aFP: Pointer): PtrUInt;
var
  args: specialize TArray<Pointer>;
  i, len: SizeInt;
  val: PPtrUInt;
  resptr: Pointer;
begin
  len := Length(fArgInfos);
  if fResultInParam then
    Dec(len);
  SetLength(args, len);
  for i := 0 to High(fArgInfos) do begin
    if i = fResultIdx then
      Continue;
    case fArgInfos[i].ArgType of
      TArgType.GenReg,
      TArgType.Stack:
        val := @PPtrUInt(aStack)[fArgInfos[i].Offset];
      TArgType.FPReg:
        val := @PPtrUInt(aFP)[fArgInfos[i].Offset];
    end;
    if fArgInfos[i].Deref then
      args[i] := PPtrUInt(val^)
    else
      args[i] := val;
  end;

  if fResultInParam then begin
    case fArgInfos[fResultIdx].ArgType of
      TArgType.GenReg,
      TArgType.Stack:
        resptr := @PPtrUInt(aStack)[fArgInfos[fResultIdx].Offset];
      TArgType.FPReg:
        resptr := @PPtrUInt(aFP)[fArgInfos[fResultIdx].Offset];
    end;
    if fArgInfos[fResultIdx].Deref then
      resptr := PPointer(resptr)^;
  end else
    resptr := @Result;

  CallHandler(args, resptr, fContext);
end;

procedure TSystemFunctionCallback.CreateCallback;

  procedure ReplacePlaceholder(aPlaceholder: PtrUInt; aValue: PtrUInt; aOfs, aSize: PtrUInt);
  var
    found: Boolean;
    i: PtrUInt;
  begin
    found := False;
    for i := aOfs to aOfs + aSize - 1 do begin
      if PPtrUInt(@PByte(fData)[i])^ = PtrUInt(aPlaceholder) then begin
        PPtrUInt(@(PByte(fData)[i]))^ := PtrUInt(aValue);
        found := True;
        Break;
      end;
    end;

    if not found then
      raise Exception.Create(SErrMethodImplCreateFailed);
  end;

var
  src: Pointer;
  ofs, size: PtrUInt;
  method: TMethod;
begin
  fSize := PtrUInt(CallbackEndPtr) - PtrUInt(@Callback) + 1;
  fData := AllocateMemory(fSize);
  if not Assigned(fData) then
    raise Exception.Create(SErrMethodImplCreateFailed);

  src := @Callback;
  Move(src^, fData^, fSize);

  ofs := PtrUInt(CallbackContextPtr) - PtrUInt(@Callback);
  size := PtrUInt(CallbackAddressPtr) - PtrUInt(CallbackContextPtr);

  method := TMethod(@Handler);

  ReplacePlaceholder(PlaceholderContext, PtrUInt(method.Data), ofs, size);

  ofs := PtrUInt(CallbackAddressPtr) - PtrUInt(@Callback);
  size := PtrUInt(CallbackCallPtr) - PtrUInt(CallbackAddressPtr);

  ReplacePlaceholder(PlaceholderAddress, PtrUInt(method.Code), ofs, size);

  if not ProtectMemory(fData, fSize, True) then
    raise Exception.Create(SErrMethodImplCreateFailed);
end;

procedure TSystemFunctionCallback.CreateArgInfos;
var
  i, argidx, ofs: LongInt;
  td: PTypeData;
  argcount: SizeInt;
begin
  fResultInParam := ReturnResultInParam(fResultType);

  ofs := 0;
  argidx := 0;
  argcount := Length(fArgs);
  if fResultInParam then begin
    if fcfStatic in fFlags then
      fResultIdx := 0
    else
      fResultIdx := 1;
    Inc(argcount);
  end else
    fResultIdx := -1;
  SetLength(fArgInfos, argcount);
  SetLength(fRefArgs, argcount);
  if fResultIdx >= 0 then begin
    fArgInfos[fResultIdx].ArgType := TArgType.GenReg;
    fArgInfos[fResultIdx].Offset := fResultIdx;
  end;
  for i := 0 to High(fArgs) do begin
    if argidx = fResultIdx then
      Inc(argidx);
    if pfResult in fArgs[i].ParamFlags then begin
      fResultIdx := argidx;
      fResultInParam := True;
    end;
    fArgInfos[argidx].ArgType := TArgType.GenReg;
    fArgInfos[argidx].Deref := False;
    if pfArray in fArgs[i].ParamFlags then
      fArgInfos[argidx].Deref := True
    else if fArgs[i].ParamFlags * [pfOut, pfVar, pfConstRef] <> [] then
      fArgInfos[argidx].Deref := True
    else if (pfConst in fArgs[i].ParamFlags) and not Assigned(fArgs[i].ParamType) then
      fArgInfos[argidx].Deref := True
    else begin
      td := GetTypeData(fArgs[i].ParamType);
      case fArgs[i].ParamType^.Kind of
        tkSString,
        tkMethod:
          fArgInfos[argidx].Deref := True;
        tkArray:
          if not (td^.ArrayData.Size in [1, 2, 4, 8]) then
            fArgInfos[argidx].Deref := True;
        tkRecord:
          if not (td^.RecSize in [1, 2, 4, 8]) then
            fArgInfos[argidx].Deref := True;
        { ToDo: handle object like record? }
        tkObject,
        tkWString,
        tkUString,
        tkAString,
        tkDynArray,
        tkClass,
        tkClassRef,
        tkInterface,
        tkInterfaceRaw,
        tkProcVar,
        tkPointer:
          ;
        tkInt64,
        tkQWord:
          ;
        tkSet: begin
          case td^.OrdType of
            otUByte: begin
              case td^.SetSize of
                0, 1, 2, 4, 8:
                  ;
                else
                  fArgInfos[argidx].Deref := True;
              end;
            end;
            otUWord,
            otULong:
              ;
          end;
        end;
        tkEnumeration,
        tkInteger,
        tkBool:
          ;
        tkFloat: begin
          case td^.FloatType of
            ftCurr,
            ftComp:
              ;
            ftSingle,
            ftDouble : fArgInfos[argidx].ArgType := TArgType.FPReg;
            ftExtended: {val := PInt64(PExtended(aArgs[i].ValueRef))^};
          end;
        end;
      else
        raise EInvocationError.CreateFmt(SErrFailedToConvertArg, [i, fArgs[i].ParamType^.Name]);
      end;
    end;

    if (fArgInfos[argidx].ArgType = TArgType.FPReg) and (ofs >= 4) then
      fArgInfos[argidx].ArgType := TArgType.Stack;
    if (fArgInfos[argidx].ArgType = TArgType.GenReg) and (ofs >= 4) then
      fArgInfos[argidx].ArgType := TArgType.Stack;

    fArgInfos[argidx].Offset := ofs;
    Inc(ofs);
    Inc(argidx);
  end;
end;

function TSystemFunctionCallback.GetCodeAddress: CodePointer;
begin
  Result := fData;
end;
{$endif}

constructor TSystemFunctionCallback.Create(aContext: Pointer; aCallConv: TCallConv; constref aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags);
{$ifdef windows}
var
  i: SizeInt;
{$endif}
begin
{$ifdef windows}
  fContext := aContext;
  SetLength(fArgs, Length(aArgs));
  for i := 0 to High(aArgs) do
    fArgs[i] := aArgs[i];
  fResultType := aResultType;
  fFlags := aFlags;
  CreateCallback;
  CreateArgInfos;
{$else}
  raise EInvocationError.Create(SErrPlatformNotSupported);
{$endif}
end;

destructor TSystemFunctionCallback.Destroy;
begin
{$ifdef windows}
  if Assigned(fData) then
    FreeMemory(fData, fSize);
{$endif}
end;

constructor TSystemFunctionCallbackProc.Create(aHandler: TFunctionCallProc; aContext: Pointer; aCallConv: TCallConv; constref aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags);
begin
  inherited Create(aContext, aCallConv, aArgs, aResultType, aFlags);
  fHandler := aHandler;
end;

procedure TSystemFunctionCallbackProc.CallHandler(constref aArgs: specialize TArray<Pointer>; aResult: Pointer; aContext: Pointer);
begin
  fHandler(aArgs, aResult, aContext);
end;

constructor TSystemFunctionCallbackMethod.Create(aHandler: TFunctionCallMethod; aContext: Pointer; aCallConv: TCallConv; constref aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags);
begin
  inherited Create(aContext, aCallConv, aArgs, aResultType, aFlags);
  fHandler := aHandler;
end;

procedure TSystemFunctionCallbackMethod.CallHandler(constref aArgs: specialize TArray<Pointer>; aResult: Pointer; aContext: Pointer);
begin
  fHandler(aArgs, aResult, aContext);
end;

function SystemCreateCallbackProc(aHandler: TFunctionCallProc; aCallConv: TCallConv; aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
begin
  Result := TSystemFunctionCallbackProc.Create(aHandler, aContext, aCallConv, aArgs, aResultType, aFlags);
end;

function SystemCreateCallbackMethod(aHandler: TFunctionCallMethod; aCallConv: TCallConv; aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
begin
  Result := TSystemFunctionCallbackMethod.Create(aHandler, aContext, aCallConv, aArgs, aResultType, aFlags);
end;

const
  SystemFunctionCallManager: TFunctionCallManager = (
    Invoke: @SystemInvoke;
    CreateCallbackProc: @SystemCreateCallbackProc;
    CreateCallbackMethod: @SystemCreateCallbackMethod;
  );

procedure InitSystemFunctionCallManager;
begin
  SetFunctionCallManager([ccReg, ccCdecl, ccPascal, ccStdCall], SystemFunctionCallManager);
end;
