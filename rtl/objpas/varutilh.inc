{
    $Id$
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
{$ifdef HASVARIANT}

Type

  // Types needed to make this work. These should be moved to the system unit.

  currency            = int64;
  HRESULT             = Longint;
  PSmallInt           = ^Smallint;
  PLongint            = ^Longint;
  PSingle             = ^Single;
  PDouble             = ^Double;
  PCurrency           = ^Currency;
  TDateTime           = Double;
  PDate               = ^TDateTime;
  PPWideChar          = ^PWideChar;
  Error               = Longint;
  PError              = ^Error;
  PWordBool           = ^WordBool;
  PByte               = ^Byte;

  EVarianterror = Class(Exception)
    ErrCode : longint;
    Constructor CreateCode(Code : Longint);
  end;

  TVarArrayBound = packed record
    ElementCount: Longint;
    LowBound: Longint;
  end;
  TVarArrayBoundArray = Array [0..0] of TVarArrayBound;
  PVarArrayBoundArray = ^TVarArrayBoundArray;
  TVarArrayCoorArray  = Array [0..0] of Longint;
  PVarArrayCoorArray  = ^TVarArrayCoorArray;

  PVarArray = ^TVarArray;
  TVarArray = packed record
    DimCount: Word;
    Flags: Word;
    ElementSize: Longint;
  LockCount: Integer;
    Data: Pointer;
    Bounds: TVarArrayBoundArray;
  end;

  TVarType = Word;
  PVarData = ^TVarData;
  TVarData = packed record
    VType: TVarType;
    case Integer of
      0: (Reserved1: Word;
          case Integer of
            0: (Reserved2, Reserved3: Word;
                case Integer of
                  varSmallInt: (VSmallInt: SmallInt);
                  varInteger:  (VInteger: Longint);
                  varSingle:   (VSingle: Single);
                  varDouble:   (VDouble: Double);
                  varCurrency: (VCurrency: Currency);
                  varDate:     (VDate: Double);
                  varOleStr:   (VOleStr: PWideChar);
                  varDispatch: (VDispatch: Pointer);
                  varError:    (VError: LongWord);
                  varBoolean:  (VBoolean: WordBool);
                  varUnknown:  (VUnknown: Pointer);
                  varByte:     (VByte: Byte);
                  varString:   (VString: Pointer);
                  varAny:      (VAny: Pointer);
                  varArray:    (VArray: PVarArray);
                  varByRef:    (VPointer: Pointer);
         );
            1: (VLongs: array[0..2] of LongInt);
         );
      2: (VWords: array [0..6] of Word);
      3: (VBytes: array [0..13] of Byte);
  end;
  Variant = TVarData;
  PVariant = ^Variant;

{ Variant functions }

function VariantChangeTypeEx(var VargDest: TVarData; const VargSrc: TVarData; LCID: Integer; Flags: Word; VarType: Word): HRESULT; stdcall;
function VariantClear(var Varg: TVarData): HRESULT; stdcall;
function VariantCopy(var VargDest: TVarData; const VargSrc: TVarData): HRESULT; stdcall;
function VariantCopyInd(var VargDest: TVarData;  const VargSrc: TVarData): HRESULT; stdcall;
function VariantInit(var Varg: TVarData): HRESULT; stdcall;

{  Variant array functions }

function SafeArrayAccessData(psa: PVarArray; var ppvdata: Pointer): HRESULT; stdcall;
function SafeArrayAllocData(psa: PVarArray): HRESULT; stdcall;
function SafeArrayAllocDescriptor(DimCount: Integer; var psa: PVarArray): HRESULT; stdcall;
function SafeArrayCopy(psa: PVarArray; var psaout: PVarArray): HRESULT; stdcall;
function SafeArrayCopyData(psa, psaOut: PVarArray): HRESULT; stdcall;
function SafeArrayCreate(VarType, Dim: Integer; const Bounds: TVarArrayBoundArray): PVarArray; stdcall;
function SafeArrayDestroy(psa: PVarArray): HRESULT; stdcall;
function SafeArrayDestroyData(psa: PVarArray): HRESULT; stdcall;
function SafeArrayDestroyDescriptor(psa: PVarArray): HRESULT; stdcall;
function SafeArrayGetDim(psa: PVarArray): Integer; stdcall;
function SafeArrayGetElemSize(psa: PVarArray): LongWord; stdcall;
function SafeArrayGetElement(psa: PVarArray; Indices: PVarArrayCoorArray;  Data: Pointer): HRESULT; stdcall;
function SafeArrayGetLBound(psa: PVarArray; Dim: Integer;  var LBound: Integer): HRESULT; stdcall;
function SafeArrayGetUBound(psa: PVarArray; Dim: Integer;  var UBound: Integer): HRESULT; stdcall;
function SafeArrayLock(psa: PVarArray): HRESULT; stdcall;
function SafeArrayPtrOfIndex(psa: PVarArray; Indices: PVarArrayCoorArray;  var Address: Pointer): HRESULT; stdcall;
function SafeArrayPutElement(psa: PVarArray; Indices: PVarArrayCoorArray;  const Data: Pointer): HRESULT; stdcall;
function SafeArrayRedim(psa: PVarArray; const NewBound: TVarArrayBound): HRESULT; stdcall;
function SafeArrayUnaccessData(psa: PVarArray): HRESULT; stdcall;
function SafeArrayUnlock(psa: PVarArray): HRESULT; stdcall;

{ Conversion routines NOT in windows oleaut }

Function VariantToSmallInt(Const VargSrc : TVarData) : SmallInt;
Function VariantToLongint(Const VargSrc : TVarData) : Longint;
Function VariantToSingle(Const VargSrc : TVarData) : Single;
Function VariantToDouble(Const VargSrc : TVarData) : Double;
Function VariantToCurrency(Const VargSrc : TVarData) : Currency;
Function VariantToDate(Const VargSrc : TVarData) : TDateTime;
Function VariantToBoolean(Const VargSrc : TVarData) : Boolean;
Function VariantToByte(Const VargSrc : TVarData) : Byte;


// Names match the ones in Borland varutils unit.

const
  VAR_OK            = HRESULT($00000000);
  VAR_TYPEMISMATCH  = HRESULT($80020005);
  VAR_BADVARTYPE    = HRESULT($80020008);
  VAR_EXCEPTION     = HRESULT($80020009);
  VAR_OVERFLOW      = HRESULT($8002000A);
  VAR_BADINDEX      = HRESULT($8002000B);
  VAR_ARRAYISLOCKED = HRESULT($8002000D);
  VAR_NOTIMPL       = HRESULT($80004001);
  VAR_OUTOFMEMORY   = HRESULT($8007000E);
  VAR_INVALIDARG    = HRESULT($80070057);
  VAR_UNEXPECTED    = HRESULT($8000FFFF);

  ARR_NONE          = $0000;
  ARR_FIXEDSIZE     = $0010;
  ARR_OLESTR        = $0100;
  ARR_UNKNOWN       = $0200;
  ARR_DISPATCH      = $0400;
  ARR_VARIANT       = $0800;

{$endif HASVARIANT}

{
  $Log$
  Revision 1.2  2001-08-19 21:02:02  florian
    * fixed and added a lot of stuff to get the Jedi DX( headers
      compiled

}
