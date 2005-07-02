{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000,2001 by the Free Pascal development team

    Interface and OS-dependent part of variant support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

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
      VarOleStr  : Result:=StrToInt(WideCharToString(vOleStr));
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
      VarOleStr  : Result:=StrToInt(WideCharToString(vOleStr));
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
        VarOleStr  :
          if not(TryStrToInt(WideCharToString(vOleStr),Result)) then
            VariantTypeMismatch;
        VarString  :
          if not(TryStrToInt(ansistring(vString),Result)) then
            VariantTypeMismatch;
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
  var
    l : longint;
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
        VarOleStr  :
          begin
            if not(TryStrToInt(WideCharToString(vOleStr),l)) then
              VariantTypeMismatch;
            result:=l;
          end;
        VarString  :
          begin
            if not(TryStrToInt(ansistring(vString),l)) then
              VariantTypeMismatch;
            result:=l;
          end;
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
  var
    e : extended;
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
        VarOleStr  :
          begin
            if not(TryStrToFloat(WideCharToString(vOleStr),Result)) then
              VariantTypeMismatch;
             result:=e;
          end;
        VarString  :
          begin
            if not(TryStrToFloat(ansistring(vString),Result)) then
              VariantTypeMismatch;
             result:=e;
          end;

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
        VarOleStr  :
          begin
            if not(TryStrToFloat(WideCharToString(vOleStr),Result)) then
              VariantTypeMismatch;
          end;
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
          VarOleStr  :
            if not(TryStrToCurr(WideCharToString(vOleStr),Result)) then
              VariantTypeMismatch;
          VarString  :
            if not(TryStrToCurr(ansistring(vString),Result)) then
              VariantTypeMismatch;
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
        VarCurrency: Result:=Trunc(VCurrency);
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
        VarCurrency: Result:=Trunc(VCurrency);
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
        VarCurrency : Result:=FloatToStr(VCurrency);
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

Const
 BS : Array[Boolean] of AnsiString = ('False','True');

begin
  Try
    With VargSrc do
      Case (VType and VarTypeMask) of
        VarSmallInt : Result:=IntTostr(VSmallint);
        VarShortInt : Result:=IntToStr(VShortInt);
        VarInteger  : Result:=IntToStr(VInteger);
        VarSingle   : Result:=FloatToStr(VSingle);
        VarDouble   : Result:=FloatToStr(VDouble);
        VarCurrency : Result:=FloatToStr(VCurrency);
        VarDate     : Result:=DateTimeToStr(VDate);
        VarOleStr   : Result:=WideCharToString(VOleStr);
        VarBoolean  : Result:=BS[VBoolean];
        VarByte     : Result:=IntToStr(VByte);
        VarWord     : Result:=IntToStr(VWord);
        VarLongWord : Result:=IntToStr(VLongWord);
        VarInt64    : Result:=IntToStr(VInt64);
        VarQword    : Result:=IntToStr(VQWord);
        VarString   : Result:=ansistring(VString);
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

Function VariantToShortString(Const VargSrc : TVarData) : ShortString;

Var
  S : AnsiString;

begin
  S:=VariantToAnsiString(VArgSrc);
  Result:=S;
end;

{ ---------------------------------------------------------------------
    Some debug routines
  ---------------------------------------------------------------------}


Procedure DumpVariant(Const VArgSrc : TVarData);

begin
  DumpVariant(Output,VArgSrc);
end;

(*
   tvardata = packed record
      vtype : tvartype;
      case integer of
         0:(res1 : word;
            case integer of
               0:
                 (res2,res3 : word;
                  case word of
                     varsmallint : (vsmallint : smallint);
                     varinteger : (vinteger : longint);
                     varsingle : (vsingle : single);
                     vardouble : (vdouble : double);
                     varcurrency : (vcurrency : currency);
                     vardate : (vdate : tdatetime);
                     varolestr : (volestr : pwidechar);
                     vardispatch : (vdispatch : pointer);
                     varerror : (verror : dword);
                     varboolean : (vboolean : wordbool);
                     varunknown : (vunknown : pointer);
                     // vardecimal : ( : );
                     varshortint : (vshortint : shortint);
                     varbyte : (vbyte : byte);
                     varword : (vword : word);
                     varlongword : (vlongword : dword);
                     varint64 : (vint64 : int64);
                     varqword : (vqword : qword);
                     varword64 : (vword64 : qword);
                     varstring : (vstring : pointer);
                     varany :  (vany : pointer);
                     vararray : (varray : pvararray);
                     varbyref : (vpointer : pointer);
                 );
               1:
                 (vlongs : array[0..2] of longint);
           );
         1:(vwords : array[0..6] of word);
         2:(vbytes : array[0..13] of byte);
      end;

*)

Const
  VarTypeStrings : Array [varEmpty..varqword] of string = (
    'empty',  'null',  'smallint',  'integer',  'single',  'double',
    'currency',  'date',  'olestr',  'dispatch',  'error',  'boolean',
    'variant',  'unknown',  'unknown','decimal',  'shortint',  'byte',  'word',
    'longword',  'int64',  'qword');

Procedure DumpVariant(Var F : Text; Const VArgSrc : TVarData);

Var
  W : WideString;

begin
  If VArgSrc.vType in [varEmpty..varqword] then
    Writeln(F,'Variant has type : ',VarTypeStrings[VArgSrc.vType])
  else if (VArgSrc.vType=VarArray) Then
    begin
    Write(F,'Variant is array.');
    exit;
    end
  else if (VargSrc.vType=VarByRef) then
    begin
    Writeln(F,'Variant is by reference.');
    exit;
    end
  else
    begin
    Writeln(F,'Variant has unknown type: ', VargSrc.vType);
    Exit;
    end;
  If VArgSrc.vType<>varEmpty then
    With VArgSrc do
      begin
      Write(F,'Value is: ') ;
      Case vtype of
        varnull : Write(F,'Null');
        varsmallint : Write(F,vsmallint);
        varinteger : Write(F,vinteger);
        varsingle : Write(F,vsingle);
        vardouble : Write(F,vdouble);
        varcurrency : Write(F,vcurrency) ;
        vardate : Write(F,vdate) ;
        varolestr : begin
                    W:=vOleStr;
                    Write(F,W) ;
                    end;
        vardispatch : Write(F,'Not suppordted') ;
        varerror : Write(F,'Error') ;
        varboolean : Write(F,vboolean) ;
        varvariant : Write(F,'Unsupported') ;
        varunknown : Write(F,'Unsupported') ;
        vardecimal : Write(F,'Unsupported') ;
        varshortint : Write(F,vshortint) ;
        varbyte : Write(F,vbyte) ;
        varword : Write(F,vword) ;
        varlongword : Write(F,vlongword) ;
        varint64 : Write(F,vint64) ;
        varqword : Write(F,vqword) ;
      end;
      Writeln(f);
      end;
end;


