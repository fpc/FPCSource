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

Type
	  TVarTypeToElementInfo = Record
 				   Validbase   : Boolean;	// COM basetype?
				   ValidElement: Boolean;       // allowable in varrarray ?	
				   Size	       : Longint;	// elementsize
				   Flags       : Word;		// see below
				  end;				

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


  // only the byval types.
  CVarTypeToElementInfo : array[varempty..varqword] of TVarTypeToElementInfo = (
   {varempty    }     (Validbase :  true     ;ValidElement:  true    ;Size:     0       ;Flags :  ARR_NONE),
   {varnull     }     (Validbase :  true     ;ValidElement:  true    ;Size:     0       ;Flags :  ARR_NONE),
   {varsmallint }     (Validbase :  true     ;ValidElement:  true    ;Size:     2       ;Flags :  ARR_NONE),
   {varinteger  }     (Validbase :  true     ;ValidElement:  true    ;Size:     4       ;Flags :  ARR_NONE),
   {varsingle   }     (Validbase :  true     ;ValidElement:  true    ;Size:     4       ;Flags :  ARR_NONE),
   {vardouble   }     (Validbase :  true     ;ValidElement:  true    ;Size:     8       ;Flags :  ARR_NONE),
   {varcurrency }     (Validbase :  true     ;ValidElement:  true    ;Size:     8       ;Flags :  ARR_NONE),
   {vardate     }     (Validbase :  true     ;ValidElement:  true    ;Size:     8       ;Flags :  ARR_NONE),
   {varolestr   }     (Validbase :  true     ;ValidElement:  true    ;Size:     4       ;Flags :  ARR_OLESTR),
   {vardispatch }     (Validbase :  true     ;ValidElement:  true    ;Size:     4       ;Flags :  ARR_DISPATCH),
   {varerror    }     (Validbase :  true     ;ValidElement:  true    ;Size:     4       ;Flags :  ARR_NONE),
   {varboolean  }     (Validbase :  true     ;ValidElement:  true    ;Size:     2       ;Flags :  ARR_NONE),
   {varvariant  }     (Validbase :  true     ;ValidElement:  true    ;Size:    16       ;Flags :  ARR_NONE),
   {varunknown  }     (Validbase :  true     ;ValidElement:  true    ;Size:     4       ;Flags :  ARR_VARIANT),
   {vardecimal  }     (Validbase :  false    ;ValidElement:  true    ;Size:    14       ;Flags :  ARR_UNKNOWN),
   {varunused1}	      (ValidBase :  false    ;ValidElement:  false    ;Size:    4       ;Flags :  ARR_NONE), 	
   {varshortint }     (Validbase :  true     ;ValidElement:  true    ;Size:     1       ;Flags :  ARR_NONE),
   {varbyte     }     (Validbase :  true     ;ValidElement:  true    ;Size:     1       ;Flags :  ARR_NONE),
   {varword     }     (Validbase :  true     ;ValidElement:  true    ;Size:     2       ;Flags :  ARR_NONE),
   {varlongword }     (Validbase :  true     ;ValidElement:  true    ;Size:     4       ;Flags :  ARR_NONE),
   {varint64    }     (Validbase :  true     ;ValidElement:  true    ;Size:     8       ;Flags :  ARR_NONE),
   {varqword    }     (Validbase :  true     ;ValidElement:  true    ;Size:     8       ;Flags :  ARR_NONE));
