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

  sub %eax, %esp

  movl %esp, %edi

  lea 12(%edx), %esi

  cld
  rep movsd

nostackargs:

  movl 8(%edx), %ecx
  movl (%edx), %eax
  movl 4(%edx), %edx

  call -12(%ebp)

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
type
  PBoolean16 = ^Boolean16;
  PBoolean32 = ^Boolean32;
  PBoolean64 = ^Boolean64;
  PByteBool = ^ByteBool;
  PQWordBool = ^QWordBool;
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
          PComp(aResultValue)^ := floatres;
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
  SystemFunctionCallManager: TFunctionCallManager = (
    Invoke: @SystemInvoke;
    CreateCallbackProc: Nil;
    CreateCallbackMethod: Nil;
  );

procedure InitSystemFunctionCallManager;
begin
  SetFunctionCallManager([ccReg{, ccCdecl, ccPascal, ccStdCall}], SystemFunctionCallManager);
end;
