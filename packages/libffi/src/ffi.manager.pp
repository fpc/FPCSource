{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by the Free Pascal development team

    RTTI Function Call Manager using Foreign Function Call (libffi) library.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit ffi.manager;

{$mode objfpc}{$H+}

interface

implementation

uses
  TypInfo, Rtti, ffi;

type
  Tpffi_typeArray = array of pffi_type;

procedure FreeFFIType(t: pffi_type);
var
  elements: Tpffi_typeArray;
  i: LongInt;
begin
  if t^._type <> _FFI_TYPE_STRUCT then
    Exit;
  elements := Tpffi_typeArray(t^.elements);
  for i := Low(elements) to High(elements) do
    FreeFFIType(elements[i]);
  { with this the array will be freed }
  elements := Nil;
  Dispose(t);
end;

function TypeInfoToFFIType(aTypeInfo: PTypeInfo): pffi_type; forward;

function RecordOrObjectToFFIType(aTypeInfo: PTypeInfo): pffi_type;
var
  curindex: SizeInt;
  elements: Tpffi_typeArray;

  procedure AddElement(t: pffi_type);
  begin
    if curindex = Length(elements) then begin
      SetLength(elements, Length(elements) * 2);
    end;
    elements[curindex] := t;
    Inc(curindex);
  end;

var
  td: PTypeData;
  i, curoffset, remoffset: SizeInt;
  field: PManagedField;
begin
  td := GetTypeData(aTypeInfo);
  if td^.TotalFieldCount = 0 then
    { uhm... }
    Exit(Nil);
  New(Result);
  FillChar(Result^, SizeOf(Result), 0);
  Result^._type := _FFI_TYPE_STRUCT;
  curoffset := 0;
  curindex := 0;
  field := PManagedField(PByte(@td^.TotalFieldCount) + SizeOf(td^.TotalFieldCount));
  { assume first that there are no paddings }
  SetLength(elements, td^.TotalFieldCount);
  for i := 0 to td^.TotalFieldCount - 1 do begin
    { ToDo: what about fields that are larger that what we have currently? }
    if field^.FldOffset < curoffset then
      Continue;
    remoffset := field^.FldOffset - curoffset;
    { insert padding elements }
    while remoffset >= SizeOf(QWord) do begin
      AddElement(@ffi_type_uint64);
      Dec(remoffset, SizeOf(QWord));
    end;
    while remoffset >= SizeOf(LongWord) do begin
      AddElement(@ffi_type_uint32);
      Dec(remoffset, SizeOf(LongWord));
    end;
    while remoffset >= SizeOf(Word) do begin
      AddElement(@ffi_type_uint16);
      Dec(remoffset, SizeOf(Word));
    end;
    while remoffset >= SizeOf(Byte) do begin
      AddElement(@ffi_type_uint8);
      Dec(remoffset, SizeOf(Byte))
    end;
    { now add the real field type }
    AddElement(TypeInfoToFFIType(field^.TypeRef));
    Inc(field);
    curoffset := field^.FldOffset;
  end;
  { add a final Nil element }
  AddElement(Nil);
  { reduce array to final size }
  SetLength(elements, curindex);
  { this is a bit cheeky, but it works }
  Tpffi_typeArray(Result^.elements) := elements;
end;

function SetToFFIType(aSize: SizeInt): pffi_type;
var
  elements: Tpffi_typeArray;
  curindex: SizeInt;

  procedure AddElement(t: pffi_type);
  begin
    if curindex = Length(elements) then begin
      SetLength(elements, Length(elements) * 2);
    end;
    elements[curindex] := t;
    Inc(curindex);
  end;

begin
  if aSize = 0 then
    Exit(Nil);
  New(Result);
  Result^._type := _FFI_TYPE_STRUCT;
  curindex := 0;
  SetLength(elements, aSize);
  while aSize >= SizeOf(QWord) do begin
    AddElement(@ffi_type_uint64);
    Dec(aSize, SizeOf(QWord));
  end;
  while aSize >= SizeOf(LongWord) do begin
    AddElement(@ffi_type_uint32);
    Dec(aSize, SizeOf(LongWord));
  end;
  while aSize >= SizeOf(Word) do begin
    AddElement(@ffi_type_uint16);
    Dec(aSize, SizeOf(Word));
  end;
  while aSize >= SizeOf(Byte) do begin
    AddElement(@ffi_type_uint8);
    Dec(aSize, SizeOf(Byte));
  end;
  AddElement(Nil);
  SetLength(elements, curindex);
  Tpffi_typeArray(Result^.elements) := elements;
end;

function TypeInfoToFFIType(aTypeInfo: PTypeInfo): pffi_type;

  function TypeKindName: String;
  begin
    WriteStr(Result, aTypeInfo^.Kind);
  end;

var
  td: PTypeData;
begin
  Result := @ffi_type_void;
  if Assigned(aTypeInfo) then begin
    td := GetTypeData(aTypeInfo);
    case aTypeInfo^.Kind of
      tkInteger,
      tkEnumeration,
      tkBool,
      tkInt64,
      tkQWord:
        case td^.OrdType of
          otSByte:
            Result := @ffi_type_sint8;
          otUByte:
            Result := @ffi_type_uint8;
          otSWord:
            Result := @ffi_type_sint16;
          otUWord:
            Result := @ffi_type_uint16;
          otSLong:
            Result := @ffi_type_sint32;
          otULong:
            Result := @ffi_type_uint32;
          otSQWord:
            Result := @ffi_type_sint64;
          otUQWord:
            Result := @ffi_type_uint64;
        end;
      tkChar:
        Result := @ffi_type_uint8;
      tkFloat:
        case td^.FloatType of
          ftSingle:
            Result := @ffi_type_float;
          ftDouble:
            Result := @ffi_type_double;
          ftExtended:
            Result := @ffi_type_longdouble;
          ftComp:
{$ifndef FPC_HAS_TYPE_EXTENDED}
            Result := @ffi_type_sint64;
{$else}
            Result := @ffi_type_longdouble;
{$endif}
          ftCurr:
            Result := @ffi_type_sint64;
        end;
      tkSet:
        case td^.OrdType of
          otUByte: begin
            if td^.SetSize = 1 then
              Result := @ffi_type_uint8
            else begin
              { ugh... build a of suitable record }
              Result := SetToFFIType(td^.SetSize);
            end;
          end;
          otUWord:
            Result := @ffi_type_uint16;
          otULong:
            Result := @ffi_type_uint32;
        end;
      tkWChar,
      tkUChar:
        Result := @ffi_type_uint16;
      tkInterface,
      tkAString,
      tkUString,
      tkWString,
      tkInterfaceRaw,
      tkProcVar,
      tkDynArray,
      tkClass,
      tkClassRef,
      tkPointer:
        Result := @ffi_type_pointer;
      tkMethod:
        Result := RecordOrObjectToFFIType(TypeInfo(TMethod));
      tkSString:
        { since shortstrings are rather large they're passed as references }
        Result := @ffi_type_pointer;
      tkObject:
        { passed around as pointer as well }
        Result := @ffi_type_pointer;
      tkArray:
        { arrays are passed as pointers to be compatible to C }
        Result := @ffi_type_pointer;
      tkRecord:
        Result := RecordOrObjectToFFIType(aTypeInfo);
      tkVariant:
        Result := RecordOrObjectToFFIType(TypeInfo(tvardata));
      //tkLString: ;
      //tkHelper: ;
      //tkFile: ;
      else
        raise EInvocationError.CreateFmt(SErrTypeKindNotSupported, [TypeKindName]);
    end;
  end;
end;

function ValueToFFIValue(constref Value: TValue; var aIndirect: Pointer; aIsResult: Boolean): Pointer;
const
  ResultTypeNeedsIndirection = [
   tkAString,
   tkWString,
   tkUString,
   tkInterface,
   tkDynArray
  ];
begin
  aIndirect := Nil;
  Result := Value.GetReferenceToRawData;
  if (Value.Kind = tkSString) or (aIsResult and (Value.Kind in ResultTypeNeedsIndirection)) then begin
    aIndirect := Result;
    Result := @aIndirect;
  end;
end;

function FFIValueToValue(Value: Pointer; TypeInfo: PTypeInfo): TValue;
begin
  TValue.Make(Value, TypeInfo, Result);
end;

{ move this to type info? }
function RetInParam(aCallConv: TCallConv; aTypeInfo: PTypeInfo): Boolean;
begin
  Result := False;
  if not Assigned(aTypeInfo) then
    Exit;
  case aTypeInfo^.Kind of
    tkSString,
    tkAString,
    tkWString,
    tkUString,
    tkInterface,
    tkDynArray:
      Result := True;
  end;
end;

procedure FFIInvoke(aCodeAddress: Pointer; const aArgs: TFunctionCallParameterArray; aCallConv: TCallConv;
            aResultType: PTypeInfo; out aResultValue: TValue; aFlags: TFunctionCallFlags);

  function CallConvName: String; inline;
  begin
    WriteStr(Result, aCallConv);
  end;

var
  abi: ffi_abi;
  argtypes: array of pffi_type;
  argvalues: array of Pointer;
  argindirect: array of Pointer;
  rtype: pffi_type;
  rvalue: ffi_arg;
  i, arglen, argoffset, retidx, argstart: LongInt;
  cif: ffi_cif;
  retparam: Boolean;
begin
  aResultValue := TValue.Empty;

  if not (fcfStatic in aFlags) and (Length(aArgs) = 0) then
    raise EInvocationError.Create(SErrMissingSelfParam);

  case aCallConv of
{$if defined(CPUI386)}
    ccReg:
      abi := FFI_REGISTER;
    ccCdecl:
{$ifdef WIN32}
      abi := FFI_MS_CDECL;
{$else}
      abi := FFI_STDCALL;
{$endif}
    ccPascal:
      abi := FFI_PASCAL;
    ccStdCall:
      abi := FFI_STDCALL;
    ccCppdecl:
      abi := FFI_THISCALL;
{$else}
{$ifndef CPUM68K}
    { M68k has a custom register calling convention implementation }
    ccReg,
{$endif}
    ccCdecl,
    ccPascal,
    ccStdCall,
    ccCppdecl:
      abi := FFI_DEFAULT_ABI;
{$endif}
    else
      raise EInvocationError.CreateFmt(SErrCallConvNotSupported, [CallConvName]);
  end;

  retparam := RetInParam(aCallConv, aResultType);

  arglen := Length(aArgs);
  if retparam then begin
    Inc(arglen);
    argoffset := 1;
    retidx := 0;
  end else begin
    argoffset := 0;
    retidx := -1;
  end;

  SetLength(argtypes, arglen);
  SetLength(argvalues, arglen);
  SetLength(argindirect, arglen);

  { the order is Self/Vmt (if any), Result param (if any), other params }

  if not (fcfStatic in aFlags) and retparam then begin
    argtypes[0] := TypeInfoToFFIType(aArgs[0].Value.TypeInfo);
    argvalues[0] := ValueToFFIValue(aArgs[0].Value, argindirect[0], False);
    if retparam then
      Inc(retidx);
    argstart := 1;
  end else
    argstart := 0;

  for i := Low(aArgs) + argstart to High(aArgs) do begin
    argtypes[i - Low(aArgs) + Low(argtypes) + argoffset] := TypeInfoToFFIType(aArgs[i].Value.TypeInfo);
    argvalues[i - Low(aArgs) + Low(argtypes) + argoffset] := ValueToFFIValue(aArgs[i].Value, argindirect[i + argoffset], False);
  end;

  if retparam then begin
    argtypes[retidx] := TypeInfoToFFIType(aResultType);
    TValue.Make(Nil, aResultType, aResultValue);
    argvalues[retidx] := ValueToFFIValue(aResultValue, argindirect[retidx], True);
    rtype := @ffi_type_void;
  end else begin
    rtype := TypeInfoToFFIType(aResultType);
  end;

  if ffi_prep_cif(@cif, abi, arglen, rtype, @argtypes[0]) <> FFI_OK then
    raise EInvocationError.Create(SErrInvokeFailed);

  ffi_call(@cif, ffi_fn(aCodeAddress), @rvalue, @argvalues[0]);

  if Assigned(aResultType) and not retparam then
    aResultValue := FFIValueToValue(@rvalue, aResultType);
end;

const
  FFIManager: TFunctionCallManager = (
    Invoke: @FFIInvoke;
    CreateCallbackProc: Nil;
    CreateCallbackMethod: Nil;
    FreeCallback: Nil
  );

var
  OldManagers: TFunctionCallManagerArray;

const
  SupportedCallConvs = [ccReg, ccCdecl, ccStdCall, {ccCppdecl,} ccPascal];

procedure InitFuncCallManager;
begin
  SetFunctionCallManager(SupportedCallConvs, FFIManager, OldManagers);
end;

procedure DoneFuncCallManager;
begin
  SetFunctionCallManagers(SupportedCallConvs, OldManagers);
end;

initialization
  InitFuncCallManager;
finalization
  DoneFuncCallManager;
end.

