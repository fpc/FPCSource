{
  This file is part of the Free Pascal run time library.
  Copyright (C) 2025 Nikolay Nikolov
  member of the Free Pascal development team.

  Function call manager for WebAssembly 32-bit

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$define SYSTEM_HAS_INVOKE}

resourcestring
  SErrFailedToConvertArg = 'Failed to convert argument %d of type %s';
  SErrFailedToConvertRes = 'Failed to convert result of type %s';

type
  TWasmBasicType = (
    wbt_Unknown,
    { number types }
    wbt_i32, wbt_i64, wbt_f32, wbt_f64,
    { reference types }
    wbt_funcref, wbt_externref,
    { vector types }
    wbt_v128
  );

procedure WasmInvokeHelper(CodeAddress: CodePointer; Args: Pointer; Result: Pointer); external name 'fpc_wasm_invoke_helper';

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
  ParamSize: Integer = 0;
  ParamBuf: Pointer = nil;
  CurrParamPtr: Pointer = nil;

  procedure AddParam_i32(v: UInt32; pass: Integer);
  begin
    if pass = 1 then
      Inc(ParamSize, 4)
    else
    begin
      PUInt32(CurrParamPtr)^ := v;
      Inc(CurrParamPtr, 4);
    end;
  end;

  procedure AddParam_i64(v: UInt64; pass: Integer);
  begin
    if pass = 1 then
      Inc(ParamSize, 8)
    else
    begin
      PUInt64(CurrParamPtr)^ := v;
      Inc(CurrParamPtr, 8);
    end;
  end;

  procedure AddParam_f32(v: Single; pass: Integer);
  begin
    if pass = 1 then
      Inc(ParamSize, 4)
    else
    begin
      PSingle(CurrParamPtr)^ := v;
      Inc(CurrParamPtr, 4);
    end;
  end;

  procedure AddParam_f64(v: Double; pass: Integer);
  begin
    if pass = 1 then
      Inc(ParamSize, 8)
    else
    begin
      PDouble(CurrParamPtr)^ := v;
      Inc(CurrParamPtr, 8);
    end;
  end;

  procedure AddParam(ParamNum: Integer; const param: TFunctionCallParameter; pass: Integer);
  var
    td: PTypeData;
  begin
    if pfArray in param.Info.ParamFlags then
      AddParam_i32(PtrUInt(param.ValueRef), pass)
    else if param.Info.ParamFlags * [pfOut, pfVar, pfConstRef] <> [] then
      AddParam_i32(PtrUInt(param.ValueRef), pass)
    else if (pfConst in param.Info.ParamFlags) and not Assigned(param.Info.ParamType) then
      AddParam_i32(PtrUInt(param.ValueRef), pass)
    else
    begin
      td := GetTypeData(param.Info.ParamType);
      case param.Info.ParamType^.Kind of
        tkSString,
        tkMethod:
          AddParam_i32(PtrUInt(param.ValueRef), pass);
        tkArray:
          if td^.ArrayData.Size in [1, 2, 4, 8] then
          begin
            if td^.ArrayData.Size = 8 then
              AddParam_i64(PUInt64(param.ValueRef)^, pass)
            else
              AddParam_i32(PUInt32(param.ValueRef)^, pass);
          end
          else
            AddParam_i32(PtrUInt(param.ValueRef), pass);
        tkRecord:
          if (td^.RecSize in [1, 2, 4, 8]) and not (pfConst in param.Info.ParamFlags) then
          begin
            if td^.RecSize = 8 then
              AddParam_i64(PUInt64(param.ValueRef)^, pass)
            else
              AddParam_i32(PUInt32(param.ValueRef)^, pass);
          end
          else
            AddParam_i32(PtrUInt(param.ValueRef), pass);
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
          AddParam_i32(PPtrUInt(param.ValueRef)^, pass);
        tkInt64,
        tkQWord:
          AddParam_i64(PUInt64(param.ValueRef)^, pass);
        tkSet:
          begin
            case td^.OrdType of
              otUByte:
                begin
                  case td^.SetSize of
                    0, 1:
                      AddParam_i32(PByte(param.ValueRef)^, pass);
                    2:
                      AddParam_i32(PWord(param.ValueRef)^, pass);
                    3:
                      AddParam_i32(PtrUInt(param.ValueRef), pass);
                    4:
                      AddParam_i32(PLongWord(param.ValueRef)^, pass);
                    5..7:
                      AddParam_i32(PtrUInt(param.ValueRef), pass);
                    8:
                      AddParam_i64(PQWord(param.ValueRef)^, pass);
                    else
                      AddParam_i32(PtrUInt(param.ValueRef), pass);
                  end;
                end;
              otUWord:
                AddParam_i32(PWord(param.ValueRef)^, pass);
              otULong:
                AddParam_i32(PLongWord(param.ValueRef)^, pass);
            end;
          end;
        tkEnumeration,
        tkInteger:
          begin
            case td^.OrdType of
              otSByte:
                AddParam_i32(UInt32(PInt8(param.ValueRef)^), pass);
              otUByte:
                AddParam_i32(PUInt8(param.ValueRef)^, pass);
              otSWord:
                AddParam_i32(UInt32(PInt16(param.ValueRef)^), pass);
              otUWord:
                AddParam_i32(PUInt16(param.ValueRef)^, pass);
              otSLong,
              otULong:
                AddParam_i32(PUInt32(param.ValueRef)^, pass);
            end;
          end;
        tkBool:
          begin
            case td^.OrdType of
              otUByte:
                AddParam_i32(UInt32(System.PBoolean(param.ValueRef)^), pass);
              otUWord:
                AddParam_i32(UInt32(PBoolean16(param.ValueRef)^), pass);
              otULong:
                AddParam_i32(UInt32(PBoolean32(param.ValueRef)^), pass);
              otUQWord:
                AddParam_i64(UInt64(PBoolean64(param.ValueRef)^), pass);
              otSByte:
                AddParam_i32(UInt32(PByteBool(param.ValueRef)^), pass);
              otSWord:
                AddParam_i32(UInt32(PWordBool(param.ValueRef)^), pass);
              otSLong:
                AddParam_i32(UInt32(PLongBool(param.ValueRef)^), pass);
              otSQWord:
                AddParam_i64(UInt64(PQWordBool(param.ValueRef)^), pass);
            end;
          end;
        tkFloat:
          begin
            case td^.FloatType of
              ftSingle:
                AddParam_f32(PSingle(param.ValueRef)^, pass);
              ftDouble,
              ftExtended:
                AddParam_f64(PDouble(param.ValueRef)^, pass);
              ftCurr:
                AddParam_i64(PUInt64(PCurrency(param.ValueRef))^, pass);
              ftComp:
                AddParam_i64(PUInt64(PComp(param.ValueRef))^, pass);
              else
                raise EInvocationError.CreateFmt(SErrFailedToConvertArg, [ParamNum, param.Info.ParamType^.Name]);
            end;
          end;
        else
          raise EInvocationError.CreateFmt(SErrFailedToConvertArg, [ParamNum, param.Info.ParamType^.Name]);
      end;
    end;
  end;

var
  i: Integer;
  retinparam: Boolean;
  td: PTypeData;
  ResultBuf: array [0..15] of Byte;
begin
  if Assigned(aResultType) and not Assigned(aResultValue) then
    raise EInvocationError.Create(SErrInvokeResultTypeNoValue);

  retinparam := ReturnResultInParam(aResultType);

  { pass 1 }
  if Assigned(aResultType) and retinparam then
    AddParam_i32(PtrUInt(aResultValue), 1);
  for i := Low(aArgs) to High(aArgs) do
    AddParam(i, aArgs[i], 1);

  if ParamSize > 0 then
  begin
    GetMem(ParamBuf, ParamSize);
    CurrParamPtr := ParamBuf;
    { pass 2 }
    if Assigned(aResultType) and retinparam then
      AddParam_i32(PtrUInt(aResultValue), 2);
    for i := Low(aArgs) to High(aArgs) do
      AddParam(i, aArgs[i], 2);
  end;

  WasmInvokeHelper(aCodeAddress, ParamBuf, @ResultBuf);

  if ParamSize > 0 then
    FreeMem(ParamBuf, ParamSize);

  if Assigned(aResultType) and not retinparam then
  begin
    case aResultType^.Kind of
      tkFloat:
        begin
          td := GetTypeData(aResultType);
          case td^.FloatType of
            ftSingle:
              PSingle(aResultValue)^ := PSingle(@ResultBuf)^;
            ftDouble,
            ftExtended:
              PDouble(aResultValue)^ := PDouble(@ResultBuf)^;
            ftCurr:
              PCurrency(aResultValue)^ := PCurrency(@ResultBuf)^;
            ftComp:
              PComp(aResultValue)^ := PComp(@ResultBuf)^;
          end;
        end;
      tkQWord, tkInt64:
        PUInt64(aResultValue)^ := PUInt64(@ResultBuf)^;
      else
        PUInt32(aResultValue)^ := PUInt32(@ResultBuf)^;
    end;
  end;
end;

const
  SystemFunctionCallManager: TFunctionCallManager = (
    Invoke: @SystemInvoke;
    CreateCallbackProc: @NoCreateCallbackProc;
    CreateCallbackMethod: @NoCreateCallbackMethod;
  );

procedure InitSystemFunctionCallManager;
begin
  SetFunctionCallManager([ccReg, ccCdecl, ccPascal, ccStdCall], SystemFunctionCallManager);
end;

