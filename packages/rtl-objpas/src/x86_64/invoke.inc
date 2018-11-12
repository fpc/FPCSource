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

  movq %rsp, %rbp
.seh_setframe %rbp, 0
.seh_endprologue

  { align stack size to 16 Byte }
  add $15, aArgsStackSize
  and $-16, aArgsStackSize
  sub aArgsStackSize, %rsp

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

  movq 24(%rsp), %rdi
  movq 16(%rsp), %rsi
  movq 8(%rsp), %rbp
end;
{$endif}

resourcestring
  SErrFailedToConvertArg = 'Failed to convert argument %d of type %s';
  SErrFailedToConvertRes = 'Failed to convert result of type %s';

procedure SystemInvoke(aCodeAddress: CodePointer; const aArgs: TFunctionCallParameterArray; aCallConv: TCallConv;
            aResultType: PTypeInfo; aResultValue: Pointer; aFlags: TFunctionCallFlags);
type
  PBoolean16 = ^Boolean16;
  PBoolean32 = ^Boolean32;
  PBoolean64 = ^Boolean64;
  PByteBool = ^ByteBool;
  PQWordBool = ^QWordBool;
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
  retinparam := False;
  if Assigned(aResultType) then begin
    case aResultType^.Kind of
      tkSString,
      tkAString,
      tkUString,
      tkWString,
      tkInterface,
      tkDynArray:
        retinparam := True;
    end;
  end;

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
            otUByte: val := ShortInt(PBoolean(aArgs[i].ValueRef)^);
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
  end;
{$else}
  raise EInvocationError.Create(SErrPlatformNotSupported);
{$endif}
end;

const
  SystemFunctionCallManager: TFunctionCallManager = (
    Invoke: @SystemInvoke;
    CreateCallbackProc: Nil;
    CreateCallbackMethod: Nil;
  );

procedure InitSystemFunctionCallManager;
begin
  SetFunctionCallManager([ccReg, ccCdecl, ccPascal, ccStdCall], SystemFunctionCallManager);
end;
