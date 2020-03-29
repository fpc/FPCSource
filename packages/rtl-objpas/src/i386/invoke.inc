{%MainUnit ../inc/rtti.pp}

{
  This file is part of the Free Pascal run time library.
  Copyright (C) 2019 Sven Barth
  member of the Free Pascal development team.

  Function call manager for i386

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$define SYSTEM_HAS_INVOKE}

function ReturnResultInParam(aType: PTypeInfo): Boolean;
var
  td: PTypeData;
begin
  { Only on Win32 structured types of sizes 1, 2 and 4 are returned directly
    instead of a result parameter }
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
{$ifdef win32}
        td := GetTypeData(aType);
        Result := not (td^.ArrayData.Size in [1, 2, 4]);
{$else}
        Result := True;
{$endif}
      end;
      tkRecord: begin
{$ifdef win32}
        td := GetTypeData(aType);
        Result := not (td^.RecSize in [1, 2, 4]);
{$else}
        Result := True;
{$endif}
      end;
      tkSet: begin
        td := GetTypeData(aType);
        case td^.OrdType of
          otUByte:
            Result := not (td^.SetSize in [1, 2, 4]);
          otUWord,
          otULong:
            Result := False;
        end;
      end;
    end;
  end;
end;

procedure InvokeKernelRegister(aCodeAddress: CodePointer; aArgs: Pointer; aArgCount: LongInt); assembler; nostackframe;
label
  nostackargs;
asm
  pushl %ebp  
  movl %esp, %ebp

  { keep stack aligned to 16 bytes? }
{$if FPC_STACKALIGNMENT=16}
  leal  -8(%esp),%esp
{$endif FPC_STACKALIGNMENT=16}
  
  pushl %edi
  pushl %esi

  pushl %eax
  pushl %edx
  
  cmpl $3, %ecx
  jle nostackargs

  { copy arguments to stack }

  subl $3, %ecx

  { allocate count (%ecx) * 4 space on stack }
  movl %ecx, %eax
  shll $2, %eax

  { keep stack aligned to 16 bytes? }
{$if FPC_STACKALIGNMENT=16}
  addl $15, %eax
  movl %eax, %esi
  andl $15, %esi
  subl %esi, %eax
{$endif FPC_STACKALIGNMENT=16}
  
  sub %eax, %esp

  movl %esp, %edi  
  lea 12(%edx), %esi

  cld
  rep movsd

nostackargs:

  movl 8(%edx), %ecx
  movl (%edx), %eax
  movl 4(%edx), %edx

{$if FPC_STACKALIGNMENT=16}
  call -20(%ebp)
  { ensure stack is cleared }
  leal -24(%ebp),%esp
{$else FPC_STACKALIGNMENT=16}  
  call -12(%ebp)
{$endif FPC_STACKALIGNMENT=16}
  
  popl %ecx
  movl %eax, (%ecx)
  movl %edx, 4(%ecx)

  popl %ecx

  popl %esi
  popl %edi

  movl %ebp, %esp
  popl %ebp
end;

resourcestring
  SErrFailedToConvertArg = 'Failed to convert argument %d of type %s';

procedure SystemInvokeRegister(aCodeAddress: CodePointer; const aArgs: TFunctionCallParameterArray; aCallConv: TCallConv;
            aResultType: PTypeInfo; aResultValue: Pointer; aFlags: TFunctionCallFlags);
var
  regstack: array of PtrUInt;
  stackargs: array of SizeInt;
  argcount, regidx, stackidx, stackcnt, i: LongInt;
  retinparam, isstack: Boolean;
  td: PTypeData;
  floatres: Extended;

  procedure AddRegArg(aValue: PtrUInt);
  begin
    if regidx < 3 then begin
      regstack[regidx] := aValue;
      Inc(regidx);
    end else begin
      if 3 + stackidx = Length(regstack) then
        SetLength(regstack, Length(regstack) * 2);
      regstack[3 + stackidx] := aValue;
      Inc(stackidx);
    end;
  end;

  procedure AddStackArg(aValue: PtrUInt);
  begin
    if 3 + stackidx = Length(regstack) then
      SetLength(regstack, Length(regstack) * 2);
    regstack[3 + stackidx] := aValue;
    Inc(stackidx);
  end;

begin
  { for the register calling convention we always have the registers EAX, EDX, ECX
    and then the stack; if a parameter does not fit into a register its moved to the
    next available stack slot and the next parameter gets a chance to be in a register }

  retinparam := ReturnResultInParam(aResultType);

  { we allocate at least three slots for EAX, ECX and EDX }
  argcount := Length(aArgs);
  if retinparam then
    Inc(argcount);
  if argcount < 3 then
    SetLength(regstack, 3)
  else
    SetLength(regstack, argcount);

  regidx := 0;
  stackidx := 0;

  SetLength(stackargs, Length(aArgs));
  stackcnt := 0;

  { first pass: handle register parameters }
  for i := 0 to High(aArgs) do begin
    if regidx >= 3 then begin
      { all register locations already used up }
      stackargs[stackcnt] := i;
      Inc(stackcnt);
      Continue;
    end;

    isstack := False;

    if pfArray in aArgs[i].Info.ParamFlags then
      AddRegArg(PtrUInt(aArgs[i].ValueRef))
    else if aArgs[i].Info.ParamFlags * [pfOut, pfVar, pfConstRef] <> [] then
      AddRegArg(PtrUInt(aArgs[i].ValueRef))
    else if (pfConst in aArgs[i].Info.ParamFlags) and not Assigned(aArgs[i].Info.ParamType) then
      AddRegArg(PtrUInt(aArgs[i].ValueRef))
    else begin
      td := GetTypeData(aArgs[i].Info.ParamType);
      case aArgs[i].Info.ParamType^.Kind of
        tkSString,
        tkMethod:
          AddRegArg(PtrUInt(aArgs[i].ValueRef));
        tkArray:
          if td^.ArrayData.Size <= 4 then
            isstack := True
          else
            AddRegArg(PtrUInt(aArgs[i].ValueRef));
        tkRecord:
          if td^.RecSize <= 4 then
            isstack := True
          else
            AddRegArg(PtrUInt(aArgs[i].ValueRef));
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
          AddRegArg(PPtrUInt(aArgs[i].ValueRef)^);
        tkInt64,
        tkQWord:
          isstack := True;
        tkSet: begin
          case td^.OrdType of
            otUByte: begin
              case td^.SetSize of
                0, 1:
                  AddRegArg(PByte(aArgs[i].ValueRef)^);
                2:
                  AddRegArg(PWord(aArgs[i].ValueRef)^);
                3:
                  AddRegArg(PtrUInt(aArgs[i].ValueRef));
                4:
                  AddRegArg(PLongWord(aArgs[i].ValueRef)^);
                else
                  AddRegArg(PtrUInt(aArgs[i].ValueRef));
              end;
            end;
            otUWord:
              AddRegArg(PWord(aArgs[i].ValueRef)^);
            otULong:
              AddRegArg(PLongWord(aArgs[i].ValueRef)^);
          end;
        end;
        tkEnumeration,
        tkInteger: begin
          case td^.OrdType of
            otSByte: AddRegArg(PShortInt(aArgs[i].ValueRef)^);
            otUByte: AddRegArg(PByte(aArgs[i].ValueRef)^);
            otSWord: AddRegArg(PSmallInt(aArgs[i].ValueRef)^);
            otUWord: AddRegArg(PWord(aArgs[i].ValueRef)^);
            otSLong: AddRegArg(PLongInt(aArgs[i].ValueRef)^);
            otULong: AddRegArg(PLongWord(aArgs[i].ValueRef)^);
          end;
        end;
        tkBool: begin
          case td^.OrdType of
            otUByte: AddRegArg(ShortInt(System.PBoolean(aArgs[i].ValueRef)^));
            otUWord: AddRegArg(Byte(PBoolean16(aArgs[i].ValueRef)^));
            otULong: AddRegArg(SmallInt(PBoolean32(aArgs[i].ValueRef)^));
            otUQWord: isstack := True;
            otSByte: AddRegArg(Word(PByteBool(aArgs[i].ValueRef)^));
            otSWord: AddRegArg(LongInt(PWordBool(aArgs[i].ValueRef)^));
            otSLong: AddRegArg(LongWord(PLongBool(aArgs[i].ValueRef)^));
            otSQWord: isstack := True;
          end;
        end;
        tkFloat:
          { all float types are passed in on stack }
          isstack := True;
      else
        raise EInvocationError.CreateFmt(SErrFailedToConvertArg, [i, aArgs[i].Info.ParamType^.Name]);
      end;
    end;

    if isstack then begin
      stackargs[stackcnt] := i;
      Inc(stackcnt);
    end;
  end;

  { then add the result parameter reference (if any) }
  if Assigned(aResultType) and retinparam then
    AddRegArg(PtrUInt(aResultValue));

  { second pass: handle stack arguments from right to left }
  if stackcnt > 0 then begin
    for i := stackcnt - 1 downto 0 do begin
      if pfArray in aArgs[stackargs[i]].Info.ParamFlags then
        AddStackArg(PtrUInt(aArgs[stackargs[i]].ValueRef))
      else if aArgs[stackargs[i]].Info.ParamFlags * [pfOut, pfVar, pfConstRef] <> [] then
        AddStackArg(PtrUInt(aArgs[stackargs[i]].ValueRef))
      else if (pfConst in aArgs[stackargs[i]].Info.ParamFlags) and not Assigned(aArgs[stackargs[i]].Info.ParamType) then
        AddStackArg(PtrUInt(aArgs[stackargs[i]].ValueRef))
      else begin
        td := GetTypeData(aArgs[stackargs[i]].Info.ParamType);
        case aArgs[stackargs[i]].Info.ParamType^.Kind of
          tkSString,
          tkMethod:
            AddStackArg(PtrUInt(aArgs[stackargs[i]].ValueRef));
          tkArray:
            if td^.ArrayData.Size <= 4 then
              AddStackArg(PPtrUInt(aArgs[stackargs[i]].ValueRef)^)
            else
              AddStackArg(PtrUInt(aArgs[stackargs[i]].ValueRef));
          tkRecord:
            if td^.RecSize <= 4 then
              AddStackArg(PPtrUInt(aArgs[stackargs[i]].ValueRef)^)
            else
              AddStackArg(PtrUInt(aArgs[stackargs[i]].ValueRef));
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
            AddStackArg(PPtrUInt(aArgs[stackargs[i]].ValueRef)^);
          tkInt64,
          tkQWord: begin
            AddStackArg(PInt32(PInt64(aArgs[stackargs[i]].ValueRef))[0]);
            AddStackArg(PInt32(PInt64(aArgs[stackargs[i]].ValueRef))[1]);
          end;
          tkSet: begin
            case td^.OrdType of
              otUByte: begin
                case td^.SetSize of
                  0, 1:
                    AddStackArg(PByte(aArgs[stackargs[i]].ValueRef)^);
                  2:
                    AddStackArg(PWord(aArgs[stackargs[i]].ValueRef)^);
                  3:
                    AddStackArg(PtrUInt(aArgs[stackargs[i]].ValueRef));
                  4:
                    AddStackArg(PLongWord(aArgs[stackargs[i]].ValueRef)^);
                  else
                    AddStackArg(PtrUInt(aArgs[stackargs[i]].ValueRef));
                end;
              end;
              otUWord:
                AddStackArg(PWord(aArgs[stackargs[i]].ValueRef)^);
              otULong:
                AddStackArg(PLongWord(aArgs[stackargs[i]].ValueRef)^);
            end;
          end;
          tkEnumeration,
          tkInteger: begin
            case td^.OrdType of
              otSByte: AddStackArg(PShortInt(aArgs[stackargs[i]].ValueRef)^);
              otUByte: AddStackArg(PByte(aArgs[stackargs[i]].ValueRef)^);
              otSWord: AddStackArg(PSmallInt(aArgs[stackargs[i]].ValueRef)^);
              otUWord: AddStackArg(PWord(aArgs[stackargs[i]].ValueRef)^);
              otSLong: AddStackArg(PLongInt(aArgs[stackargs[i]].ValueRef)^);
              otULong: AddStackArg(PLongWord(aArgs[stackargs[i]].ValueRef)^);
            end;
          end;
          tkBool: begin
            case td^.OrdType of
              otUByte: AddStackArg(ShortInt(System.PBoolean(aArgs[stackargs[i]].ValueRef)^));
              otUWord: AddStackArg(Byte(PBoolean16(aArgs[stackargs[i]].ValueRef)^));
              otULong: AddStackArg(SmallInt(PBoolean32(aArgs[stackargs[i]].ValueRef)^));
              otUQWord: AddStackArg(QWord(PBoolean64(aArgs[stackargs[i]].ValueRef)));
              otSByte: AddStackArg(Word(PByteBool(aArgs[stackargs[i]].ValueRef)^));
              otSWord: AddStackArg(LongInt(PWordBool(aArgs[stackargs[i]].ValueRef)^));
              otSLong: AddStackArg(LongWord(PLongBool(aArgs[stackargs[i]].ValueRef)^));
              otSQWord: AddStackArg(PtrUInt(PQWordBool(aArgs[stackargs[i]].ValueRef)));
            end;
          end;
          tkFloat: begin
            case td^.FloatType of
              ftCurr   : begin
                AddStackArg(PInt32(PCurrency(aArgs[stackargs[i]].ValueRef))[0]);
                AddStackArg(PInt32(PCurrency(aArgs[stackargs[i]].ValueRef))[1]);
              end;
              ftSingle : AddStackArg(PInt32(PSingle(aArgs[stackargs[i]].ValueRef))^);
              ftDouble : begin
                AddStackArg(PInt32(PDouble(aArgs[stackargs[i]].ValueRef))[0]);
                AddStackArg(PInt32(PDouble(aArgs[stackargs[i]].ValueRef))[1]);
              end;
              ftExtended: begin
                AddStackArg(PInt32(PExtended(aArgs[stackargs[i]].ValueRef))[0]);
                AddStackArg(PInt32(PExtended(aArgs[stackargs[i]].ValueRef))[1]);
                AddStackArg(PWord(PExtended(aArgs[stackargs[i]].ValueRef))[4]);
              end;
              ftComp   : begin
                AddStackArg(PInt32(PComp(aArgs[stackargs[i]].ValueRef))[0]);
                AddStackArg(PInt32(PComp(aArgs[stackargs[i]].ValueRef))[1]);
              end;
            end;
          end;
        else
          raise EInvocationError.CreateFmt(SErrFailedToConvertArg, [stackargs[i], aArgs[stackargs[i]].Info.ParamType^.Name]);
        end;
      end;
    end;
  end;

  InvokeKernelRegister(aCodeAddress, @regstack[0], 3 + stackidx);

  if Assigned(aResultType) and not retinparam then begin
    if aResultType^.Kind = tkFloat then begin
      td := GetTypeData(aResultType);
      asm
        lea floatres, %eax
        fstpt (%eax)
      end ['eax'];
      case td^.FloatType of
        ftSingle:
          PSingle(aResultValue)^ := floatres;
        ftDouble:
          PDouble(aResultValue)^ := floatres;
        ftExtended:
          PExtended(aResultValue)^ := floatres;
        ftCurr:
          PCurrency(aResultValue)^ := floatres / 10000;
        ftComp:
          PComp(aResultValue)^ := Comp(floatres);
      end;
    end else if aResultType^.Kind in [tkQWord, tkInt64] then
      PQWord(aResultValue)^ := regstack[0] or (QWord(regstack[1]) shl 32)
    else
      PPtrUInt(aResultValue)^ := regstack[0];
  end;
end;

procedure SystemInvoke(aCodeAddress: CodePointer; const aArgs: TFunctionCallParameterArray; aCallConv: TCallConv;
            aResultType: PTypeInfo; aResultValue: Pointer; aFlags: TFunctionCallFlags);
begin
  case aCallConv of
    ccReg:
      SystemInvokeRegister(aCodeAddress, aArgs, aCallConv, aResultType, aResultValue, aFlags);
    otherwise
      Assert(False, 'Unsupported calling convention');
  end;
end;

const
  PlaceholderContext = LongWord($12345678);
  PlaceholderAddress = LongWord($87654321);
  PlaceholderRetPop  = Word($1234);

  RetNear = $C2;
  RetFar = $CA;

label
  CallbackRegisterContext,
  CallbackRegisterAddress,
  CallbackRegisterCall,
  CallbackRegisterRet,
  CallbackRegisterEnd;

const
  CallbackRegisterContextPtr: Pointer = @CallbackRegisterContext;
  CallbackRegisterAddressPtr: Pointer = @CallbackRegisterAddress;
  CallbackRegisterCallPtr: Pointer = @CallbackRegisterCall;
  CallbackRegisterRetPtr: Pointer = @CallbackRegisterRet;
  CallbackRegisterEndPtr: Pointer = @CallbackRegisterEnd;

procedure CallbackRegister; assembler; nostackframe;
asm
  { establish frame }
  pushl %ebp
  movl %esp, %ebp

  { store registers }
  pushl %ecx
  pushl %edx
  pushl %eax

  { store pointer to stack area (including GP registers) }
  lea (%esp), %edx

{$if FPC_STACKALIGNMENT=16}
  { keep stack aligned, before the call below stack must be aligned to a 16 byte boundary }
  leal -8(%esp),%esp
{$endif FPC_STACKALIGNMENT=16}
  
  { also store ebx as we'll use that for the function address }
  pushl %ebx

  { call function with context }
CallbackRegisterContext:
  movl $0x12345678, %eax
CallbackRegisterAddress:
  movl $0x87654321, %ebx
CallbackRegisterCall:

  call *%ebx

  { restore ebx }
  popl %ebx

  { restore stack }
  movl %ebp, %esp
  popl %ebp

CallbackRegisterRet:
  ret $0x1234
CallbackRegisterEnd:
end;

type
  TSystemFunctionCallback = class(TFunctionCallCallback)
  private type
    {$ScopedEnums On}
    TArgType = (
      GenReg,
      Stack
    );
    {$ScopedEnums Off}

    TArgInfo = record
      ArgType: TArgType;
      ArgIdx: SizeInt;
      Slots: SizeInt;
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
    function Handler(aStack: Pointer): Int64;
  protected
    procedure CreateCallback;
    procedure CreateArgInfos;
    function GetCodeAddress: CodePointer; override;
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

function TSystemFunctionCallback.Handler(aStack: Pointer): Int64;
{
  aStack has the following layout:
    0:  EAX
    4:  EDX
    8:  ECX
    12: EBP (not needed)
    16: RET (not needed)
    20: ARGS
}
var
  args: specialize TArray<Pointer>;
  i, len: SizeInt;
  val: PPtrUInt;
  resptr: Pointer;
  genargs, stackargs: PPtrUInt;
  floatres, floattmp: Extended;
  td: PTypeData;
begin
  len := Length(fArgInfos);
  if fResultInParam then
    Dec(len);
  SetLength(args, len);
  genargs := PPtrUInt(aStack);
  stackargs := @genargs[5];
  for i := 0 to High(fArgInfos) do begin
    if i = fResultIdx then
      Continue;
    case fArgInfos[i].ArgType of
      TArgType.GenReg:
        val := @genargs[fArgInfos[i].Offset];
      TArgType.Stack:
        val := @stackargs[fArgInfos[i].Offset];
    end;
    if fArgInfos[i].Deref then
      args[fArgInfos[i].ArgIdx] := PPtrUInt(val^)
    else
      args[fArgInfos[i].ArgIdx] := val;
  end;

  if fResultInParam then begin
    case fArgInfos[fResultIdx].ArgType of
      TArgType.GenReg:
        resptr := @genargs[fArgInfos[fResultIdx].Offset];
      TArgType.Stack:
        resptr := @stackargs[fArgInfos[fResultIdx].Offset];
    end;
    if fArgInfos[fResultIdx].Deref then
      resptr := PPointer(resptr)^;
  end else if Assigned(fResultType) then begin
    if fResultType^.Kind = tkFloat then begin
      resptr := @floatres;
    end else
      resptr := @Result;
  end else
    resptr := Nil;

  CallHandler(args, resptr, fContext);

  if Assigned(fResultType) and not fResultInParam and (fResultType^.Kind = tkFloat) then begin
    td := GetTypeData(fResultType);
    case td^.FloatType of
      ftSingle:
        asm
          lea floatres, %eax
          flds (%eax)
          fwait
        end ['eax'];
      ftDouble:
        asm
          lea floatres, %eax
          fldl (%eax)
          fwait
        end ['eax'];
      ftExtended:
        asm
          lea floatres, %eax
          fldt (%eax)
          fwait
        end ['eax'];
      ftCurr,
      ftComp:
        asm
          lea floatres, %eax
          fildq (%eax)
          fwait
        end ['eax'];
    end;
  end;
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
  i, stacksize: SizeInt;
begin
  fSize := PtrUInt(CallbackRegisterEndPtr) - PtrUInt(@CallbackRegister) + 1;
  fData := AllocateMemory(fSize);
  if not Assigned(fData) then
    raise Exception.Create(SErrMethodImplCreateFailed);

  src := @CallbackRegister;
  Move(src^, fData^, fSize);

  ofs := PtrUInt(CallbackRegisterContextPtr) - PtrUInt(@CallbackRegister);
  size := PtrUInt(CallbackRegisterAddressPtr) - PtrUInt(CallbackRegisterContextPtr);

  method := TMethod(@Handler);

  ReplacePlaceholder(PlaceholderContext, PtrUInt(method.Data), ofs, size);

  ofs := PtrUInt(CallbackRegisterAddressPtr) - PtrUInt(@CallbackRegister);
  size := PtrUInt(CallbackRegisterCallPtr) - PtrUInt(CallbackRegisterAddressPtr);

  ReplacePlaceholder(PlaceholderAddress, PtrUInt(method.Code), ofs, size);

  ofs := PtrUInt(CallbackRegisterRetPtr) - PtrUInt(@CallbackRegister);
  size := PtrUInt(CallbackRegisterEndPtr) - PtrUInt(CallbackRegisterRetPtr);

  if not (PByte(fData)[ofs] = RetNear) and not (PByte(fData)[ofs] = RetFar) then
    raise Exception.Create(SErrMethodImplCreateFailed);

  stacksize := 0;
  for i := 0 to High(fArgInfos) do
    if fArgInfos[i].ArgType = TArgType.Stack then
      Inc(stacksize, fArgInfos[i].Slots);

  stacksize := stacksize * 4;

  Inc(ofs);
  if PWord(@PByte(fData)[ofs])^ = PlaceholderRetPop then
    PWord(@PByte(fData)[ofs])^ := Word(stacksize);

  if not ProtectMemory(fData, fSize, True) then
    raise Exception.Create(SErrMethodImplCreateFailed);
end;

procedure TSystemFunctionCallback.CreateArgInfos;
var
  pass, genofs, stackofs: LongInt;
  td: PTypeData;
  i, c, argcount, stackcount, idx, argidx: SizeInt;
  stackargs: array of SizeInt;
begin
  fResultInParam := ReturnResultInParam(fResultType);

  genofs := 0;
  stackofs := 0;
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

  SetLength(stackargs, argcount);
  stackcount := 0;

  for pass := 0 to 1 do begin
    if pass = 0 then
      c := High(fArgs)
    else
      c := stackcount - 1;
    for i := 0 to c do begin
      if argidx = fResultIdx then
        Inc(argidx);
      if pfResult in fArgs[i].ParamFlags then begin
        fResultIdx := argidx;
        fResultInParam := True;
      end;
      if (pass = 0) and (genofs >= 3) then begin
        stackargs[stackcount] := i;
        Inc(stackcount);
        Continue;
      end;
      if pass = 0 then
        idx := i
      else
        idx := stackargs[c - i];
      if pass = 0 then
        fArgInfos[argidx].ArgType := TArgType.GenReg
      else
        fArgInfos[argidx].ArgType := TArgType.Stack;
      fArgInfos[argidx].Deref := False;
      fArgInfos[argidx].Slots := 1;
      if pfArray in fArgs[idx].ParamFlags then
        fArgInfos[argidx].Deref := True
      else if fArgs[idx].ParamFlags * [pfOut, pfVar, pfConstRef] <> [] then
        fArgInfos[argidx].Deref := True
      else if (pfConst in fArgs[idx].ParamFlags) and not Assigned(fArgs[idx].ParamType) then
        fArgInfos[argidx].Deref := True
      else begin
        td := GetTypeData(fArgs[idx].ParamType);
        case fArgs[idx].ParamType^.Kind of
          tkSString,
          tkMethod:
            fArgInfos[argidx].Deref := True;
          tkArray:
            if td^.ArrayData.Size <= 4 then begin
              fArgInfos[argidx].Deref := True;
              fArgInfos[argidx].ArgType := TArgType.Stack;
            end;
          tkRecord:
            if td^.RecSize <= 4 then begin
              fArgInfos[argidx].Deref := True;
              fArgInfos[argidx].ArgType := TArgType.Stack;
            end;
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
          tkQWord: begin
            fArgInfos[argidx].ArgType := TArgType.Stack;
            fArgInfos[argidx].Slots := 2;
          end;
          tkSet: begin
            case td^.OrdType of
              otUByte: begin
                case td^.SetSize of
                  0, 1, 2, 4:
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
          tkInteger:
            ;
          tkBool:
            case td^.OrdType of
              otUQWord,
              otSQWord:
                fArgInfos[argidx].ArgType := TArgType.Stack;
            end;
          tkFloat: begin
            fArgInfos[argidx].ArgType := TArgType.Stack;
            case td^.FloatType of
              ftSingle:
                ;
              ftCurr,
              ftComp,
              ftDouble:
                fArgInfos[argidx].Slots := 2;
              ftExtended:
                fArgInfos[argidx].Slots := 3;
            end;
          end;
        else
          raise EInvocationError.CreateFmt(SErrFailedToConvertArg, [idx, fArgs[idx].ParamType^.Name]);
        end;
      end;

      { ignore stack arguments in first pass }
      if (pass = 0) and (fArgInfos[argidx].ArgType = TArgType.Stack) then begin
        stackargs[stackcount] := idx;
        Inc(stackcount);
        Continue;
      end;

      if fArgInfos[argidx].ArgType = TArgType.GenReg then begin
        fArgInfos[argidx].ArgIdx := idx;
        fArgInfos[argidx].Offset := genofs;
        Inc(genofs);
      end else if fArgInfos[argidx].ArgType = TArgType.Stack then begin
        fArgInfos[argidx].ArgIdx := idx;
        fArgInfos[argidx].Offset := stackofs;
        Inc(stackofs, fArgInfos[argidx].Slots);
      end;

      Inc(argidx);
    end;
  end;
end;

function TSystemFunctionCallback.GetCodeAddress: CodePointer;
begin
  Result := fData;
end;

constructor TSystemFunctionCallback.Create(aContext: Pointer; aCallConv: TCallConv; constref aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags);

  function CallConvName: String; inline;
  begin
    WriteStr(Result, aCallConv);
  end;

var
  i: SizeInt;
begin
  if not (aCallConv in [ccReg]) then
    raise ENotImplemented.CreateFmt(SErrCallConvNotSupported, [CallConvName]);
  fContext := aContext;
  SetLength(fArgs, Length(aArgs));
  for i := 0 to High(aArgs) do
    fArgs[i] := aArgs[i];
  fResultType := aResultType;
  fFlags := aFlags;
  CreateArgInfos;
  CreateCallback;
end;

destructor TSystemFunctionCallback.Destroy;
begin
  if Assigned(fData) then
    FreeMemory(fData, fSize);
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
  SetFunctionCallManager([ccReg{, ccCdecl, ccPascal, ccStdCall}], SystemFunctionCallManager);
end;
