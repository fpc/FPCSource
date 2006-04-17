{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000,2001 by the Free Pascal development team

    This include file contains the implementation for variants
    support in FPC as far as it is part of the system unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


 **********************************************************************}


{ Variant functions }
function VariantChangeTypeEx(var VargDest: TVarData; const VargSrc: TVarData; LCID: Integer; Flags: Word; VarType: Word): HRESULT; stdcall;
function VariantClear(var Varg: TVarData): HRESULT; stdcall;
function VariantCopy(var VargDest: TVarData; const VargSrc: TVarData): HRESULT; stdcall;
function VariantCopyInd(var VargDest: TVarData;  const VargSrc: TVarData): HRESULT; stdcall;
function VariantInit(var Varg: TVarData): HRESULT; stdcall;

{  Variant array functions }

function SafeArrayAccessData(psa: PVarArray; var ppvdata: Pointer): HRESULT; stdcall;
function SafeArrayAllocData(psa: PVarArray): HRESULT; stdcall;
function SafeArrayAllocDescriptor(DimCount: longword; var psa: PVarArray): HRESULT; stdcall;
function SafeArrayCopy(psa: PVarArray; var psaout: PVarArray): HRESULT; stdcall;
function SafeArrayCopyData(psa, psaOut: PVarArray): HRESULT; stdcall;
function SafeArrayCreate(VarType, Dim: dword; const Bounds: TVarArrayBoundArray): PVarArray; stdcall;
function SafeArrayDestroy(psa: PVarArray): HRESULT; stdcall;
function SafeArrayDestroyData(psa: PVarArray): HRESULT; stdcall;
function SafeArrayDestroyDescriptor(psa: PVarArray): HRESULT; stdcall;
function SafeArrayGetDim(psa: PVarArray):HRESULT; stdcall;
function SafeArrayGetElemsize(psa: PVarArray): LongWord; stdcall;
function SafeArrayGetElement(psa: PVarArray; Indices: PVarArrayCoorArray;  Data: Pointer): HRESULT; stdcall;
function SafeArrayGetLBound(psa: PVarArray; Dim: longword;  var LBound: longint): HRESULT; stdcall;
function SafeArrayGetUBound(psa: PVarArray; Dim: longword;  var UBound: longint): HRESULT; stdcall;
function SafeArrayLock(psa: PVarArray): HRESULT; stdcall;
function SafeArrayPtrOfIndex(psa: PVarArray; Indices: PVarArrayCoorArray;  var Address: Pointer): HRESULT; stdcall;
function SafeArrayPutElement(psa: PVarArray; Indices: PVarArrayCoorArray;  const Data: Pointer): HRESULT; stdcall;
function SafeArrayRedim(psa: PVarArray; const NewBound: TVarArrayBound): HRESULT; stdcall;
function SafeArrayUnaccessData(psa: PVarArray): HRESULT; stdcall;
function SafeArrayUnlock(psa: PVarArray): HRESULT; stdcall;

{ Conversion routines NOT in windows oleaut }

Function VariantToSmallInt(Const VargSrc : TVarData) : SmallInt;
Function VariantToLongint(Const VargSrc : TVarData) : Longint;
Function VariantToShortint(Const VargSrc : TVarData) : ShortInt;
Function VariantToCardinal(Const VargSrc : TVarData) : Cardinal;
Function VariantToSingle(Const VargSrc : TVarData) : Single;
Function VariantToDouble(Const VargSrc : TVarData) : Double;
Function VariantToCurrency(Const VargSrc : TVarData) : Currency;
Function VariantToDate(Const VargSrc : TVarData) : TDateTime;
Function VariantToBoolean(Const VargSrc : TVarData) : Boolean;
Function VariantToByte(Const VargSrc : TVarData) : Byte;
Function VariantToInt64(Const VargSrc : TVarData ) : Int64;
Function VariantToQWord(Const VargSrc : TVarData ) : Qword;
Function VariantToWideString(Const VargSrc : TVarData) : WideString;
Function VariantToAnsiString(Const VargSrc : TVarData) : AnsiString;
Function VariantToShortString(Const VargSrc : TVarData) : ShortString;

{Debug routines }
Procedure DumpVariant(Const VArgSrc : TVarData);
Procedure DumpVariant(Var F : Text; Const VArgSrc : TVarData);


{$i varerror.inc}
// Names match the ones in Borland varutils unit.
const
  ARR_NONE          = $0000;
  ARR_FIXEDSIZE     = $0010;
  ARR_OLESTR        = $0100;
  ARR_UNKNOWN       = $0200;
  ARR_DISPATCH      = $0400;
  ARR_VARIANT       = $0800;


