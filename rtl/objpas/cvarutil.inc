{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000,2001 by the Free Pascal development team

    Interface and OS-dependent part of variant support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef HASVARIANT}

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

{ ---------------------------------------------------------------------
    OS-independent functions not present in Windows
  ---------------------------------------------------------------------}

Function VariantToSmallInt(Const VargSrc : TVarData) : SmallInt;

begin
  With VargSrc do
    Case (VType and VarTypeMask) of
      VarSmallInt: Result:=VSmallInt;
      VarShortInt: Result:=VShortInt;
      VarInteger : Result:=VInteger;
      VarSingle  : Result:=Round(VSingle);
      VarDouble  : Result:=Round(VDouble);
      VarCurrency: Result:=Round(VCurrency);
      VarDate    : Result:=Round(VDate);
      VarOleStr  : NoWideStrings;
      VarBoolean : Result:=SmallInt(VBoolean);
      VarByte    : Result:=VByte;
      VarWord    : Result:=VWord;
      VarLongWord   : Result:=VLongWord;
      VarInt64   : Result:=VInt64;
      VarQword   : Result:=VQWord;
  else
    VariantTypeMismatch;
  end;
end;

Function VariantToShortInt(Const VargSrc : TVarData) : ShortInt;

begin
  With VargSrc do
    Case (VType and VarTypeMask) of
      VarSmallInt: Result:=VSmallInt;
      VarShortInt: Result:=VShortInt;
      VarInteger : Result:=VInteger;
      VarSingle  : Result:=Round(VSingle);
      VarDouble  : Result:=Round(VDouble);
      VarCurrency: Result:=Round(VCurrency);
      VarDate    : Result:=Round(VDate);
      VarOleStr  : NoWideStrings;
      VarBoolean : Result:=SmallInt(VBoolean);
      VarByte    : Result:=VByte;
      VarWord    : Result:=VWord;
      VarLongWord   : Result:=VLongWord;
      VarInt64   : Result:=VInt64;
      VarQword   : Result:=VQWord;
  else
    VariantTypeMismatch;
  end;
end;

Function VariantToLongint(Const VargSrc : TVarData) : Longint;

begin
  With VargSrc do
    Case (VType and VarTypeMask) of
      VarSmallInt: Result:=VSmallInt;
      VarShortInt: Result:=VShortInt;
      VarInteger : Result:=VInteger;
      VarSingle  : Result:=Round(VSingle);
      VarDouble  : Result:=Round(VDouble);
      VarCurrency: Result:=Round(VCurrency);
      VarDate    : Result:=Round(VDate);
      VarOleStr  : NoWideStrings;
      VarBoolean : Result:=Longint(VBoolean);
      VarByte    : Result:=VByte;
      VarWord    : Result:=VWord;
      VarLongWord   : Result:=VLongWord;
      VarInt64   : Result:=VInt64;
      VarQword   : Result:=VQWord;
  else
    VariantTypeMismatch;
  end;
end;

Function VariantToCardinal(Const VargSrc : TVarData) : Cardinal;

begin
  With VargSrc do
    Case (VType and VarTypeMask) of
      VarSmallInt: Result:=VSmallInt;
      VarShortInt: Result:=VShortInt;
      VarInteger : Result:=VInteger;
      VarSingle  : Result:=Round(VSingle);
      VarDouble  : Result:=Round(VDouble);
      VarCurrency: Result:=Round(VCurrency);
      VarDate    : Result:=Round(VDate);
      VarOleStr  : NoWideStrings;
      VarBoolean : Result:=Longint(VBoolean);
      VarByte    : Result:=VByte;
      VarWord    : Result:=VWord;
      VarLongWord   : Result:=VLongWord;
      VarInt64   : Result:=VInt64;
      VarQword   : Result:=VQWord;
  else
    VariantTypeMismatch;
  end;
end;

Function VariantToSingle(Const VargSrc : TVarData) : Single;

begin
  With VargSrc do
    Case (VType and VarTypeMask) of
      VarSmallInt: Result:=VSmallInt;
      VarShortInt: Result:=VShortInt;
      VarInteger : Result:=VInteger;
      VarSingle  : Result:=VSingle;
      VarDouble  : Result:=VDouble;
      VarCurrency: Result:=VCurrency;
      VarDate    : Result:=VDate;
      VarOleStr  : NoWideStrings;
      VarBoolean : Result:=Longint(VBoolean);
      VarByte    : Result:=VByte;
      VarWord    : Result:=VWord;
      VarLongWord   : Result:=VLongWord;
      VarInt64   : Result:=VInt64;
      VarQword   : Result:=VQWord;
  else
    VariantTypeMismatch;
  end;
end;

Function VariantToDouble(Const VargSrc : TVarData) : Double;

begin
  With VargSrc do
    Case (VType and VarTypeMask)  of
      VarSmallInt: Result:=VSmallInt;
      VarShortInt: Result:=VShortInt;
      VarInteger : Result:=VInteger;
      VarSingle  : Result:=VSingle;
      VarDouble  : Result:=VDouble;
      VarCurrency: Result:=VCurrency;
      VarDate    : Result:=VDate;
      VarOleStr  : NoWideStrings;
      VarBoolean : Result:=Longint(VBoolean);
      VarByte    : Result:=VByte;
      VarWord    : Result:=VWord;
      VarLongWord   : Result:=VLongWord;
      VarInt64   : Result:=VInt64;
      VarQword   : Result:=VQWord;
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
        VarShortInt: Result:=VShortInt;
        VarInteger : Result:=VInteger;
        VarSingle  : Result:=FloatToCurr(VSingle);
        VarDouble  : Result:=FloatToCurr(VDouble);
        VarCurrency: Result:=VCurrency;
        VarDate    : Result:=FloatToCurr(VDate);
        VarOleStr  : NoWideStrings;
        VarBoolean : Result:=Longint(VBoolean);
        VarByte    : Result:=VByte;
        VarWord    : Result:=VWord;
        VarLongWord   : Result:=VLongWord;
        VarInt64   : Result:=VInt64;
        VarQword   : Result:=VQWord;
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
        VarShortInt: Result:=FloatToDateTime(VShortInt);
        VarInteger : Result:=FloatToDateTime(VInteger);
        VarSingle  : Result:=FloatToDateTime(VSingle);
        VarDouble  : Result:=FloatToDateTime(VDouble);
        VarCurrency: Result:=FloatToDateTime(VCurrency);
        VarDate    : Result:=VDate;
        VarOleStr  : NoWideStrings;
        VarBoolean : Result:=FloatToDateTime(Longint(VBoolean));
        VarByte    : Result:=FloatToDateTime(VByte);
        VarWord    : Result:=FloatToDateTime(VWord);
        VarLongWord    : Result:=FloatToDateTime(VLongWord);
        VarInt64   : Result:=FloatToDateTime(VInt64);
        VarQWord   : Result:=FloatToDateTime(VQword);
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
      VarShortInt: Result:=VShortInt<>0;
      VarInteger : Result:=VInteger<>0;
      VarSingle  : Result:=VSingle<>0;
      VarDouble  : Result:=VDouble<>0;
      VarCurrency: Result:=VCurrency<>0;
      VarDate    : Result:=VDate<>0;
      VarOleStr  : NoWideStrings;
      VarBoolean : Result:=VBoolean;
      VarByte    : Result:=VByte<>0;
      VarWord    : Result:=VWord<>0;
      VarLongWord   : Result:=VLongWord<>0;
      VarInt64   : Result:=Vint64<>0;
      VarQword   : Result:=VQWord<>0;
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
        VarShortInt: Result:=VShortInt;
        VarInteger : Result:=VInteger;
        VarSingle  : Result:=Round(VSingle);
        VarDouble  : Result:=Round(VDouble);
        VarCurrency: Result:=Round(VCurrency);
        VarDate    : Result:=Round(VDate);
        VarOleStr  : NoWideStrings;
        VarBoolean : Result:=Longint(VBoolean);
        VarByte    : Result:=VByte;
        VarWord    : Result:=VWord;
        VarLongWord   : Result:=VLongWord;
        VarInt64   : Result:=Vint64;
        VarQword   : Result:=VQWord;
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

Function VariantToInt64(Const VargSrc : TVarData) : Int64;

begin
  Try
    With VargSrc do
      Case (VType and VarTypeMask) of
        VarSmallInt: Result:=VSmallInt;
        VarShortInt: Result:=VShortInt;
        VarInteger : Result:=VInteger;
        VarSingle  : Result:=Trunc(VSingle);
        VarDouble  : Result:=Trunc(VDouble);
        VarCurrency: Result:=VCurrency;
        VarDate    : Result:=Trunc(VDate);
        VarOleStr  : NoWideStrings;
        VarBoolean : Result:=Longint(VBoolean);
        VarByte    : Result:=VByte;
        VarWord    : Result:=VWord;
        VarLongWord   : Result:=VLongWord;
        VarInt64   : Result:=VInt64;
        VarQword   : Result:=VQWord;
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

Function VariantToQWord(Const VargSrc : TVarData) : QWord;

begin
  Try
    With VargSrc do
      Case (VType and VarTypeMask) of
        VarSmallInt: Result:=VSmallint;
        VarShortInt: Result:=VShortInt;
        VarInteger : Result:=VInteger;
        VarSingle  : Result:=Trunc(VSingle);
        VarDouble  : Result:=Trunc(VDouble);
        VarCurrency: Result:=VCurrency;
        VarDate    : Result:=Trunc(VDate);
        VarOleStr  : NoWideStrings;
        VarBoolean : Result:=Longint(VBoolean);
        VarByte    : Result:=VByte;
        VarWord    : Result:=VWord;
        VarLongWord   : Result:=VLongWord;
        VarInt64   : Result:=VInt64;
        VarQword   : Result:=VQWord;
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

Function VariantToWideString(Const VargSrc : TVarData) : WideString;

Const
 BS : Array[Boolean] of WideString = ('False','True');

begin
  Try
    With VargSrc do
      Case (VType and VarTypeMask) of
        VarSmallInt : Result:=IntTostr(VSmallint);
        VarShortInt : Result:=IntToStr(VShortInt);
        VarInteger  : Result:=IntToStr(VInteger);
        VarSingle   : Result:=FloatToStr(VSingle);
        VarDouble   : Result:=FloatToStr(VDouble);
        VarCurrency : Result:=IntToStr(VCurrency);
        VarDate     : Result:=DateTimeToStr(VDate);
        VarOleStr   : Result:=WideString(Pointer(VOleStr));
        VarBoolean  : Result:=BS[VBoolean];
        VarByte     : Result:=IntToStr(VByte);
        VarWord     : Result:=IntToStr(VWord);
        VarLongWord : Result:=IntToStr(VLongWord);
        VarInt64    : Result:=IntToStr(VInt64);
        VarQword    : Result:=IntToStr(VQWord);
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

Function VariantToAnsiString(Const VargSrc : TVarData) : AnsiString;

begin
end;

Function VariantToShortString(Const VargSrc : TVarData) : ShortString;

begin
end;

{$endif HASVARIANT}
{
  $Log$
  Revision 1.4  2001-11-15 22:33:14  michael
  + Real/Boolean support added, Start of string support

  Revision 1.3  2001/11/14 23:00:17  michael
  + First working variant support

  Revision 1.2  2001/08/19 21:02:02  florian
    * fixed and added a lot of stuff to get the Jedi DX( headers
      compiled

}
