{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{  Declarations for oleaut32 WinCE API

}

{exported functions list = to do,
 * please remove functions done *

     Exports

       E3 BstrFromVector
       DD CreateErrorInfo
       D7 CreateTypeLib2
       CF DispCallFunc
       1D DispGetIDsOfNames
       1C DispGetParam
       1E DispInvoke
       DB GetErrorInfo
       D5 LoadRegTypeLib
       D4 LoadTypeLib
       D8 OACreateTypeLib2
       D6 RegisterTypeLib
       17 SafeArrayAccessData
       20 SafeArrayAllocData
       1F SafeArrayAllocDescriptor
       1B SafeArrayCopy
       E1 SafeArrayCopyData
        F SafeArrayCreate
       E0 SafeArrayCreateVector
       10 SafeArrayDestroy
       22 SafeArrayDestroyData
       21 SafeArrayDestroyDescriptor
       11 SafeArrayGetDim
       19 SafeArrayGetElement
       12 SafeArrayGetElemsize
       14 SafeArrayGetLBound
       13 SafeArrayGetUBound
       15 SafeArrayLock
       D1 SafeArrayPtrOfIndex
       1A SafeArrayPutElement
       23 SafeArrayRedim
       18 SafeArrayUnaccessData
       16 SafeArrayUnlock
       DC SetErrorInfo
       D3 SysAllocStringByteLen
       D2 SysStringByteLen
       D9 SystemTimeToVariantTime
       87 VarBoolFromCy
       86 VarBoolFromDate
       8D VarBoolFromDec
       89 VarBoolFromDisp
       8A VarBoolFromI1
       82 VarBoolFromI2
       83 VarBoolFromI4
       84 VarBoolFromR4
       85 VarBoolFromR8
       88 VarBoolFromStr
       81 VarBoolFromUI1
       8B VarBoolFromUI2
       8C VarBoolFromUI4
       7C VarBstrFromBool
       79 VarBstrFromCy
       7A VarBstrFromDate
       80 VarBstrFromDec
       7B VarBstrFromDisp
       7D VarBstrFromI1
       75 VarBstrFromI2
       76 VarBstrFromI4
       77 VarBstrFromR4
       78 VarBstrFromR8
       74 VarBstrFromUI1
       7E VarBstrFromUI2
       7F VarBstrFromUI4
       6F VarCyFromBool
       6C VarCyFromDate
       73 VarCyFromDec
       6E VarCyFromDisp
       70 VarCyFromI1
       68 VarCyFromI2
       69 VarCyFromI4
       6A VarCyFromR4
       6B VarCyFromR8
       6D VarCyFromStr
       67 VarCyFromUI1
       71 VarCyFromUI2
       72 VarCyFromUI4
       62 VarDateFromBool
       5F VarDateFromCy
       66 VarDateFromDec
       61 VarDateFromDisp
       63 VarDateFromI1
       5B VarDateFromI2
       5C VarDateFromI4
       5D VarDateFromR4
       5E VarDateFromR8
       60 VarDateFromStr
       5A VarDateFromUI1
       64 VarDateFromUI2
       65 VarDateFromUI4
       DE VarDateFromUdate
       A4 VarDecFromBool
       A1 VarDecFromCy
       A0 VarDecFromDate
       A3 VarDecFromDisp
       A5 VarDecFromI1
       9C VarDecFromI2
       9D VarDecFromI4
       9E VarDecFromR4
       9F VarDecFromR8
       A2 VarDecFromStr
       9B VarDecFromUI1
       A6 VarDecFromUI2
       A7 VarDecFromUI4
       B1 VarI1FromBool
       AE VarI1FromCy
       AD VarI1FromDate
       B4 VarI1FromDec
       B0 VarI1FromDisp
       A9 VarI1FromI2
       AA VarI1FromI4
       AB VarI1FromR4
       AC VarI1FromR8
       AF VarI1FromStr
       A8 VarI1FromUI1
       B2 VarI1FromUI2
       B3 VarI1FromUI4
       2E VarI2FromBool
       2A VarI2FromCy
       2B VarI2FromDate
       32 VarI2FromDec
       2D VarI2FromDisp
       2F VarI2FromI1
       27 VarI2FromI4
       28 VarI2FromR4
       29 VarI2FromR8
       2C VarI2FromStr
       26 VarI2FromUI1
       30 VarI2FromUI2
       31 VarI2FromUI4
       3B VarI4FromBool
       37 VarI4FromCy
       38 VarI4FromDate
       3F VarI4FromDec
       3A VarI4FromDisp
       3C VarI4FromI1
       34 VarI4FromI2
       35 VarI4FromR4
       36 VarI4FromR8
       39 VarI4FromStr
       33 VarI4FromUI1
       3D VarI4FromUI2
       3E VarI4FromUI4
       25 VarNumFromParseNum
       24 VarParseNumFromStr
       48 VarR4FromBool
       44 VarR4FromCy
       45 VarR4FromDate
       4C VarR4FromDec
       47 VarR4FromDisp
       49 VarR4FromI1
       41 VarR4FromI2
       42 VarR4FromI4
       43 VarR4FromR8
       46 VarR4FromStr
       40 VarR4FromUI1
       4A VarR4FromUI2
       4B VarR4FromUI4
       55 VarR8FromBool
       51 VarR8FromCy
       52 VarR8FromDate
       59 VarR8FromDec
       54 VarR8FromDisp
       56 VarR8FromI1
       4E VarR8FromI2
       4F VarR8FromI4
       50 VarR8FromR4
       53 VarR8FromStr
       4D VarR8FromUI1
       57 VarR8FromUI2
       58 VarR8FromUI4
       96 VarUI1FromBool
       92 VarUI1FromCy
       93 VarUI1FromDate
       9A VarUI1FromDec
       95 VarUI1FromDisp
       97 VarUI1FromI1
       8E VarUI1FromI2
       8F VarUI1FromI4
       90 VarUI1FromR4
       91 VarUI1FromR8
       94 VarUI1FromStr
       98 VarUI1FromUI2
       99 VarUI1FromUI4
       BE VarUI2FromBool
       BB VarUI2FromCy
       BA VarUI2FromDate
       C1 VarUI2FromDec
       BD VarUI2FromDisp
       BF VarUI2FromI1
       B6 VarUI2FromI2
       B7 VarUI2FromI4
       B8 VarUI2FromR4
       B9 VarUI2FromR8
       BC VarUI2FromStr
       B5 VarUI2FromUI1
       C0 VarUI2FromUI4
       CB VarUI4FromBool
       C8 VarUI4FromCy
       C7 VarUI4FromDate
       CE VarUI4FromDec
       CA VarUI4FromDisp
       CC VarUI4FromI1
       C3 VarUI4FromI2
       C4 VarUI4FromI4
       C5 VarUI4FromR4
       C6 VarUI4FromR8
       C9 VarUI4FromStr
       C2 VarUI4FromUI1
       CD VarUI4FromUI2
       DF VarUdateFromDate
        E VariantChangeType
       D0 VariantChangeTypeEx
        B VariantClear
        C VariantCopy
        D VariantCopyInd
        A VariantInit
       DA VariantTimeToSystemTime
       E2 VectorFromBstr
}

{$ifdef read_interface}

//*****************************************************************************
// consts
//*****************************************************************************
const
  oleaut32dll   = 'oleaut32.dll';

//*****************************************************************************
// types
//*****************************************************************************


//*****************************************************************************
// functions
//*****************************************************************************

//BSTR API
function  SysAllocString(psz: pointer): Integer; external oleaut32dll name 'SysAllocString';
function  SysAllocStringLen(psz: pointer; len:dword): Integer; external oleaut32dll name 'SysAllocStringLen';
procedure SysFreeString(bstr:pointer); external oleaut32dll name 'SysFreeString';
function  SysStringLen(bstr:pointer):UINT; external oleaut32dll name 'SysStringLen';
function  SysReAllocString(var bstr:pointer;psz: pointer): Integer; external oleaut32dll name 'SysReAllocString';
function  SysReAllocStringLen(var bstr:pointer;psz: pointer; len:dword): Integer; external oleaut32dll name 'SysReAllocStringLen';


{$endif read_interface}


{$ifdef read_implementation}

{$endif read_implementation}


