{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Interface and OS-independent part of variant support
       
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE ObjFPC}
Unit varutils;

Interface

Uses sysutils;

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

Implementation

Resourcestring

  SNoWidestrings = 'No widestrings supported';
  SNoInterfaces  = 'No interfaces supported';

Procedure NoWidestrings;

begin
  Raise Exception.Create(SNoWideStrings);
end;

Procedure NoInterfaces;

begin
  Raise Exception.Create(SNoInterfaces);
end;

Constructor EVariantError.CreateCode (Code : longint);

begin
  ErrCode:=Code;
end;
  
Procedure VariantTypeMismatch;
  
begin
  Raise EVariantError.CreateCode(VAR_TYPEMISMATCH);
end;

Function ExceptionToVariantError (E : Exception): HResult;

begin
  If E is EoutOfMemory then
    Result:=VAR_OUTOFMEMORY
  else
    Result:=VAR_EXCEPTION;
end;


{$i varutils.inc}

{ ---------------------------------------------------------------------
    OS-independent functions not present in Windows
  ---------------------------------------------------------------------}
  
Function VariantToSmallInt(Const VargSrc : TVarData) : SmallInt;

begin
  With VargSrc do
    Case (VType and VarTypeMask) of
      VarSmallInt: Result:=VSmallInt;
      VarInteger : Result:=VInteger;
      VarSingle  : Result:=Round(VSingle);
      VarDouble  : Result:=Round(VDouble);
      VarCurrency: Result:=Round(VCurrency);
      VarDate    : Result:=Round(VDate);
      VarOleStr  : NoWideStrings;
      VarBoolean : Result:=SmallInt(VBoolean);
      VarByte    : Result:=VByte;
  else
    VariantTypeMismatch;
  end;
end;

Function VariantToLongint(Const VargSrc : TVarData) : Longint;

begin
  With VargSrc do
    Case (VType and VarTypeMask) of
      VarSmallInt: Result:=VSmallInt;
      VarInteger : Result:=VInteger;
      VarSingle  : Result:=Round(VSingle);
      VarDouble  : Result:=Round(VDouble);
      VarCurrency: Result:=Round(VCurrency);
      VarDate    : Result:=Round(VDate);
      VarOleStr  : NoWideStrings;
      VarBoolean : Result:=Longint(VBoolean);
      VarByte    : Result:=VByte;
  else
    VariantTypeMismatch;
  end;
end;

Function VariantToSingle(Const VargSrc : TVarData) : Single;

begin
  With VargSrc do
    Case (VType and VarTypeMask) of
      VarSmallInt: Result:=VSmallInt;
      VarInteger : Result:=VInteger;
      VarSingle  : Result:=VSingle;
      VarDouble  : Result:=VDouble;
      VarCurrency: Result:=VCurrency;
      VarDate    : Result:=VDate;
      VarOleStr  : NoWideStrings;
      VarBoolean : Result:=Longint(VBoolean);
      VarByte    : Result:=VByte;
  else
    VariantTypeMismatch;
  end;
end;

Function VariantToDouble(Const VargSrc : TVarData) : Double;

begin
  With VargSrc do
    Case (VType and VarTypeMask)  of
      VarSmallInt: Result:=VSmallInt;
      VarInteger : Result:=VInteger;
      VarSingle  : Result:=VSingle;
      VarDouble  : Result:=VDouble;
      VarCurrency: Result:=VCurrency;
      VarDate    : Result:=VDate;
      VarOleStr  : NoWideStrings;
      VarBoolean : Result:=Longint(VBoolean);
      VarByte    : Result:=VByte;
  else
    VariantTypeMismatch;
  end;
end;

Function VariantToCurrency(Const VargSrc : TVarData) : Currency;

begin
  Try 
    With VargSrc do
      Case (VType and VarTypeMask) of
        VarSmallInt: Result:=VSmallInt;
        VarInteger : Result:=VInteger;
        VarSingle  : Result:=FloatToCurr(VSingle);
        VarDouble  : Result:=FloatToCurr(VDouble);
        VarCurrency: Result:=VCurrency;
        VarDate    : Result:=FloatToCurr(VDate);
        VarOleStr  : NoWideStrings;
        VarBoolean : Result:=Longint(VBoolean);
        VarByte    : Result:=VByte;
    else
      VariantTypeMismatch;
    end;
  except
    On EConvertError do
      VariantTypeMismatch;
    else  
      Raise;  
  end;   
end;

Function VariantToDate(Const VargSrc : TVarData) : TDateTime;

begin
  Try 
    With VargSrc do
      Case (VType and VarTypeMask) of
        VarSmallInt: Result:=FloatToDateTime(VSmallInt);
        VarInteger : Result:=FloatToDateTime(VInteger);
        VarSingle  : Result:=FloatToDateTime(VSingle);
        VarDouble  : Result:=FloatToDateTime(VDouble);
        VarCurrency: Result:=FloatToDateTime(VCurrency);
        VarDate    : Result:=VDate;
        VarOleStr  : NoWideStrings;
        VarBoolean : Result:=FloatToDateTime(Longint(VBoolean));
        VarByte    : Result:=FloatToDateTime(VByte);
    else
      VariantTypeMismatch;
    end;
  except
    On EConvertError do
      VariantTypeMismatch;
    else
      Raise;
  end;   
end;

Function VariantToBoolean(Const VargSrc : TVarData) : Boolean;

begin
  With VargSrc do
    Case (VType and VarTypeMask) of
      VarSmallInt: Result:=VSmallInt<>0;
      VarInteger : Result:=VInteger<>0;
      VarSingle  : Result:=VSingle<>0;
      VarDouble  : Result:=VDouble<>0;
      VarCurrency: Result:=VCurrency<>0;
      VarDate    : Result:=VDate<>0;
      VarOleStr  : NoWideStrings;
      VarBoolean : Result:=VBoolean;
      VarByte    : Result:=VByte<>0;
  else
    VariantTypeMismatch;
  end;
end;

Function VariantToByte(Const VargSrc : TVarData) : Byte;

begin
  Try 
    With VargSrc do
      Case (VType and VarTypeMask) of
        VarSmallInt: Result:=VSmallInt;
        VarInteger : Result:=VInteger;
        VarSingle  : Result:=Round(VSingle);
        VarDouble  : Result:=Round(VDouble);
        VarCurrency: Result:=Round(VCurrency);
        VarDate    : Result:=Round(VDate);
        VarOleStr  : NoWideStrings;
        VarBoolean : Result:=Longint(VBoolean);
        VarByte    : Result:=VByte;
    else
      VariantTypeMismatch;
    end;
  except
    On EConvertError do
      VariantTypeMismatch;
    else  
      Raise;
  end;   
end;

end.
