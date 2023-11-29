{%mainunit syshelpers.pp}

{ ---------------------------------------------------------------------
  TGUIDHelper
  ---------------------------------------------------------------------}

Procedure NotImplemented(S : String);

begin
  Raise Exception.Create('Not yet implemented : '+S);
end;

Class Function TGUIDHelper.Create(const Data; DataEndian: TEndian = CPUEndian): TGUID; overload; static; inline;

begin
  Result:=Create(Data,DataEndian=TEndian.Big)
end;

Class Function TGUIDHelper.Create(const B: TBytes; DataEndian: TEndian = CPUEndian): TGUID; overload; static; inline;

begin
  Result:=Create(B,0,DataEndian);
end;

Class Function TGUIDHelper.Create(const B: TBytes; AStartIndex: Cardinal; DataEndian: TEndian = CPUEndian): TGUID; overload; static;

begin
  if ((System.Length(B)-AStartIndex)<16) then
    raise EArgumentException.CreateFmt('The length of a GUID array must be at least %d',[]);
  Result:=Create(B,AStartIndex,DataEndian=TEndian.Big);
end;

Class Function TGUIDHelper.Create(const S: string): TGUID; overload; static;

begin
  Result:=StringToGUID(S);
end;

Class Function TGUIDHelper.Create(A: Integer; B: SmallInt; C: SmallInt; const D: TBytes): TGUID; overload; static;

begin
  if (System.Length(D)<>8) then
    raise EArgumentException.CreateFmt('The length of a GUID array must be %d',[]);
  Result:=Create(Cardinal(A),Word(B),Word(C),D[0],D[1],D[2],D[3],D[4],D[5],D[6],D[7]);
end;

Class Function TGUIDHelper.Create(A: Integer; B: SmallInt; C: SmallInt; D, E, F, G, H, I, J, K: Byte): TGUID; overload; static;

begin
  Result:=Create(Cardinal(A),Word(B),Word(C),D,E,F,G,H,I,J,K);
end;

Class Function TGUIDHelper.Create(A: Cardinal; B: Word; C: Word; D, E, F, G, H, I, J, K: Byte): TGUID; overload; static;

begin
  Result.D1 := Cardinal(A);
  Result.D2 := Word(B);
  Result.D3 := Word(C);
  Result.D4[0] := D;
  Result.D4[1] := E;
  Result.D4[2] := F;
  Result.D4[3] := G;
  Result.D4[4] := H;
  Result.D4[5] := I;
  Result.D4[6] := J;
  Result.D4[7] := K;
end;

Class Function TGUIDHelper.NewGuid: TGUID; static;

begin
  CreateGUID(Result)
end;

Function TGUIDHelper.ToByteArray(DataEndian: TEndian = CPUEndian): TBytes;

begin
  SetLength(Result, 16);
  if DataEndian<>CPUEndian then
    begin
    PCardinal(@Result[0])^ := SwapEndian(D1);
    PWord(@Result[4])^ := SwapEndian(D2);
    PWord(@Result[6])^ := SwapEndian(D3);
    Move(D4, Result[8], 8);
    end
  else
    Move(D1, Result[0], SizeOf(Self));

end;

Function TGUIDHelper.ToString(SkipBrackets : Boolean = False): string;

begin
  Result:=GuidToString(Self);
  If SkipBrackets then
    Result:=Copy(Result,2,Length(Result)-2);
end;

{$define TStringHelper:=TAnsiStringHelper}
{$define TStringChar:=AnsiChar}
{$define TStringType:=AnsiString}
{$define PTStringChar:=PAnsiChar}
{$define TSHStringArray:=TAnsiStringArray}
{$define IS_ANSISTRINGHELPER}
{$i syshelps.inc}
{$undef TStringHelper}
{$undef TStringChar}
{$undef TStringType}
{$undef PTStringChar}
{$undef TSHStringArray}
{$undef IS_ANSISTRINGHELPER}

{$define TStringHelper:=TWideStringHelper}
{$define TStringChar:=WideChar}
{$define TStringType:=WideString}
{$define PTStringChar:=PWideChar}
{$define TSHStringArray:=TWideStringArray}
{$define IS_WIDESTRINGHELPER}
{$i syshelps.inc}
{$undef TStringHelper}
{$undef TStringChar}
{$undef TStringType}
{$undef PTStringChar}
{$undef TSHStringArray}
{$undef IS_WIDESTRINGHELPER}

{$define TStringHelper:=TUnicodeStringHelper}
{$define TStringChar:=UnicodeChar}
{$define TStringType:=UnicodeString}
{$define PTStringChar:=PUnicodeChar}
{$define TSHStringArray:=TUnicodeStringArray}
{$define IS_UNICODESTRINGHELPER}
{$i syshelps.inc}
{$undef TStringHelper}
{$undef TStringChar}
{$undef TStringType}
{$undef PTStringChar}
{$undef TSHStringArray}
{$undef IS_UNICODESTRINGHELPER}

{$define TStringHelper:=TShortStringHelper}
{$define TStringChar:=AnsiChar}
{$define TStringType:=ShortString}
{$define IS_SHORTSTRINGHELPER}
{$define PTStringChar:=PAnsiChar}
{$define TSHStringArray:=TShortStringArray}
{$i syshelps.inc}
{$undef TStringHelper}
{$undef TStringChar}
{$undef TStringType}
{$undef PTStringChar}
{$undef TSHStringArray}
{$undef IS_SHORTSTRINGHELPER}


{ ---------------------------------------------------------------------
  TCurrencyHelper
  ---------------------------------------------------------------------}
function TCurrencyHelper.GetMaxValue: Currency;
begin
  Result:=MaxCurrency;
end;

function TCurrencyHelper.GetMinValue: Currency;
begin
  Result:=MinCurrency;
end;

function TCurrencyHelper.Ceil: Int64;
begin
  Result:=System.Trunc(Self);
  if Currency(Result)<Self then
    Result:=Result+1;
end;

function TCurrencyHelper.Floor: Int64;
begin
  Result:=System.Trunc(Self);
  if Currency(Result)>Self then
    Result:=Result-1;
end;

function TCurrencyHelper.Frac: Currency;
begin
  Result:=System.Frac(Self);
end;

class function TCurrencyHelper.Parse(const S: string; const AFormatSettings: TFormatSettings): Currency;
begin
  Result:=StrToCurr(S, AFormatSettings);
end;

class function TCurrencyHelper.Parse(const S: string): Currency;
begin
  Result:=StrToCurr(S);
end;

class function TCurrencyHelper.Size: Integer;
begin
  Result:=SizeOf(Currency);
end;

function TCurrencyHelper.ToString(const AFormatSettings: TFormatSettings): string;
begin
  Result:=CurrToStr(Self, AFormatSettings);
end;

function TCurrencyHelper.ToString: string;
begin
  Result:=CurrToStr(Self);
end;

class function TCurrencyHelper.ToString(const Value: Currency; const AFormatSettings: TFormatSettings): string;
begin
  Result:=CurrToStr(Value, AFormatSettings);
end;

class function TCurrencyHelper.ToString(const Value: Currency): string;
begin
  Result:=CurrToStr(Value);
end;

function TCurrencyHelper.Trunc: Int64;
begin
  Result:=System.Trunc(Self);
end;

class function TCurrencyHelper.TryParse(const S: string; out Value: Currency; const AFormatSettings: TFormatSettings): Boolean;
begin
  Result:=True;
  try
    Value:=StrToCurr(S, AFormatSettings);
  except
    on E: Exception do
      Result:=False;
  end;
end;

class function TCurrencyHelper.TryParse(const S: string; out Value: Currency): Boolean;
begin
  Result:=True;
  try
    Value:=StrToCurr(S);
  except
    on E: Exception do
      Result:=False;
  end;
end;

{ ---------------------------------------------------------------------
  TSingleHelper
  ---------------------------------------------------------------------}

{$MACRO ON}

{$IFDEF FPC_HAS_TYPE_SINGLE}
{$define TFLOATHELPER:=TSingleHelper}
{$define FLOATTYPE:=Single}
{$define TFloatRec:=TSingleRec}
{$i syshelpf.inc}
{$UNDEF TFloatRec}
{$ENDIF FPC_HAS_TYPE_SINGLE}

{ ---------------------------------------------------------------------
  TDoubleHelper
  ---------------------------------------------------------------------}
{$IFDEF FPC_HAS_TYPE_DOUBLE}
{$define TFLOATHELPER:=TDoubleHelper}
{$define FLOATTYPE:=Double}
{$define TFloatRec:=TDoubleRec}
{$i syshelpf.inc}
{$UNDEF TFloatRec}
{$ENDIF FPC_HAS_TYPE_DOUBLE}

{ ---------------------------------------------------------------------
  TExtendedHelper
  ---------------------------------------------------------------------}

{$ifdef FPC_HAS_TYPE_EXTENDED}
{$define TFLOATHELPER:=TExtendedHelper}
{$define FLOATTYPE:=Extended}
{$define TFloatRec:=TExtended80Rec}
{$i syshelpf.inc}
{$UNDEF TFloatRec}
{$ENDIF FPC_HAS_TYPE_EXTENDED}

{ ---------------------------------------------------------------------
  TByteHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TByteHelper}
{$define TORDINALTYPE:=Byte}
{$define TORDINALBITINDEX:=TByteBitIndex}
{$define TORDINALNIBBLEINDEX:=TByteNibbleIndex}
{$define TORDINALOVERLAY:=TByteOverlay}
{$define TORDINALTYPESIZE1}
{$i syshelpo.inc}
{$undef TORDINALTYPESIZE1}

{ ---------------------------------------------------------------------
  TShortintHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TShortIntHelper}
{$define TORDINALTYPE:=ShortInt}
{$define TORDINALBITINDEX:=TShortIntBitIndex}
{$define TORDINALNIBBLEINDEX:=TShortIntNibbleIndex}
{$define TORDINALOVERLAY:=TShortIntOverlay}
{$define TORDINALTYPESIZE1}
{$i syshelpo.inc}
{$undef TORDINALTYPESIZE1}

{ ---------------------------------------------------------------------
  TSmallintHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TSmallIntHelper}
{$define TORDINALTYPE:=SmallInt}
{$define TORDINALBITINDEX:=TSmallIntBitIndex}
{$define TORDINALNIBBLEINDEX:=TSmallIntNibbleIndex}
{$define TORDINALBYTEINDEX:=TWordByteIndex}
{$define TORDINALOVERLAY:=TWordOverlay}
{$define TORDINALTYPESIZE2}
{$i syshelpo.inc}
{$undef TORDINALTYPESIZE2}

{ ---------------------------------------------------------------------
  TWordHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TWordHelper}
{$define TORDINALTYPE:=Word}
{$define TORDINALBITINDEX:=TWordBitIndex}
{$define TORDINALNIBBLEINDEX:=TWordNibbleIndex}
{$define TORDINALBYTEINDEX:=TWordByteIndex}
{$define TORDINALOVERLAY:=TWordOverlay}
{$define TORDINALTYPESIZE2}
{$i syshelpo.inc}
{$undef TORDINALTYPESIZE2}

{ ---------------------------------------------------------------------
  TCardinalHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TCardinalHelper}
{$define TORDINALTYPE:=Cardinal}
{$define TORDINALBITINDEX:=TCardinalBitIndex}
{$define TORDINALNIBBLEINDEX:=TCardinalNibbleIndex}
{$define TORDINALBYTEINDEX:=TCardinalByteIndex}
{$define TORDINALWORDINDEX:=TCardinalWordIndex}
{$define TORDINALOVERLAY:=TDwordOverlay}
{$define TORDINALTYPESIZE4}
{$i syshelpo.inc}
{$undef TORDINALTYPESIZE4}

{ ---------------------------------------------------------------------
  TIntegerHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TIntegerHelper}
{$define TORDINALTYPE:=Integer}
{$define TORDINALBITINDEX:=TIntegerBitIndex}
{$define TORDINALNIBBLEINDEX:=TIntegerNibbleIndex}
{$define TORDINALBYTEINDEX:=TIntegerByteIndex}
{$define TORDINALWORDINDEX:=TIntegerWordIndex}
{$if sizeof(Integer)=2}
  {$define TORDINALOVERLAY:=TWordOverlay}
  {$define TORDINALTYPESIZE2}
{$elseif sizeof(Integer)=4}
  {$define TORDINALOVERLAY:=TDwordOverlay}
  {$define TORDINALTYPESIZE4}
{$else}
  {$fatal Unsupported Integer type size}
{$endif}
{$i syshelpo.inc}
{$undef TORDINALTYPESIZE2}
{$undef TORDINALTYPESIZE4}

{ ---------------------------------------------------------------------
  TLongIntHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TLongIntHelper}
{$define TORDINALTYPE:=LongInt}
{$define TORDINALBITINDEX:=TLongIntBitIndex}
{$define TORDINALNIBBLEINDEX:=TLongIntNibbleIndex}
{$define TORDINALBYTEINDEX:=TLongIntByteIndex}
{$define TORDINALWORDINDEX:=TLongIntWordIndex}
{$define TORDINALOVERLAY:=TDwordOverlay}
{$define TORDINALTYPESIZE4}
{$i syshelpo.inc}
{$undef TORDINALTYPESIZE4}

{ ---------------------------------------------------------------------
  TInt64Helper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TInt64Helper}
{$define TORDINALTYPE:=Int64}
{$define TORDINALBITINDEX:=TInt64BitIndex}
{$define TORDINALNIBBLEINDEX:=TInt64NibbleIndex}
{$define TORDINALBYTEINDEX:=TInt64ByteIndex}
{$define TORDINALWORDINDEX:=TInt64WordIndex}
{$define TORDINALDWORDINDEX:=TInt64DWordIndex}
{$define TORDINALOVERLAY:=TQwordOverlay}
{$define TORDINALTYPESIZE8}
{$i syshelpo.inc}
{$undef TORDINALTYPESIZE8}

{ ---------------------------------------------------------------------
  TQWordHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TQWordHelper}
{$define TORDINALTYPE:=QWord}
{$define TORDINALBITINDEX:=TQwordBitIndex}
{$define TORDINALNIBBLEINDEX:=TQwordNibbleIndex}
{$define TORDINALBYTEINDEX:=TQwordByteIndex}
{$define TORDINALWORDINDEX:=TQWordWordIndex}
{$define TORDINALDWORDINDEX:=TQWordDWordIndex}
{$define TORDINALOVERLAY:=TQwordOverlay}
{$define TORDINALTYPESIZE8}
{$i syshelpo.inc}
{$undef TORDINALTYPESIZE8}

{ ---------------------------------------------------------------------
  TNativeIntHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TNativeIntHelper}
{$define TORDINALTYPE:=NativeInt}
{$define TORDINALBITINDEX:=TNativeIntBitIndex}
{$if sizeof(NativeInt)=2}
  {$define TORDINALNIBBLEINDEX:=TSmallIntNibbleIndex}
  {$define TORDINALBYTEINDEX:=TSmallIntByteIndex}
  {$define TORDINALOVERLAY:=TSmallIntOverlay}
  {$define TORDINALTYPESIZE2}
{$elseif sizeof(NativeInt)=4}
  {$define TORDINALNIBBLEINDEX:=TLongIntNibbleIndex}
  {$define TORDINALBYTEINDEX:=TLongIntByteIndex}
  {$define TORDINALWORDINDEX:=TLongIntWordIndex}
  {$define TORDINALOVERLAY:=TLongIntOverlay}
  {$define TORDINALTYPESIZE4}
{$elseif sizeof(NativeInt)=8}
  {$define TORDINALNIBBLEINDEX:=TInt64NibbleIndex}
  {$define TORDINALBYTEINDEX:=TInt64ByteIndex}
  {$define TORDINALWORDINDEX:=TInt64WordIndex}
  {$define TORDINALDWORDINDEX:=TInt64DWordIndex}
  {$define TORDINALOVERLAY:=TInt64Overlay}
  {$define TORDINALTYPESIZE8}
{$else}
  {$fatal Unsupported NativeInt type size}
{$endif}
{$i syshelpo.inc}
{$undef TORDINALTYPESIZE2}
{$undef TORDINALTYPESIZE4}
{$undef TORDINALTYPESIZE8}

{ ---------------------------------------------------------------------
  TNativeUIntHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TNativeUIntHelper}
{$define TORDINALTYPE:=NativeUInt}
{$define TORDINALBITINDEX:=TNativeUIntBitIndex}
{$if sizeof(NativeUInt)=2}
  {$define TORDINALNIBBLEINDEX:=TWordNibbleIndex}
  {$define TORDINALBYTEINDEX:=TWordByteIndex}
  {$define TORDINALOVERLAY:=TWordOverlay}
  {$define TORDINALTYPESIZE2}
{$elseif sizeof(NativeUInt)=4}
  {$define TORDINALNIBBLEINDEX:=TDwordNibbleIndex}
  {$define TORDINALBYTEINDEX:=TDwordByteIndex}
  {$define TORDINALWORDINDEX:=TDwordWordIndex}
  {$define TORDINALOVERLAY:=TDwordOverlay}
  {$define TORDINALTYPESIZE4}
{$elseif sizeof(NativeUInt)=8}
  {$define TORDINALNIBBLEINDEX:=TQwordNibbleIndex}
  {$define TORDINALBYTEINDEX:=TQwordByteIndex}
  {$define TORDINALWORDINDEX:=TQwordWordIndex}
  {$define TORDINALDWORDINDEX:=TQwordDWordIndex}
  {$define TORDINALOVERLAY:=TQwordOverlay}
  {$define TORDINALTYPESIZE8}
{$else}
  {$fatal Unsupported NativeUInt type size}
{$endif}
{$i syshelpo.inc}
{$undef TORDINALTYPESIZE2}
{$undef TORDINALTYPESIZE4}
{$undef TORDINALTYPESIZE8}

{ ---------------------------------------------------------------------
  TBooleanHelper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TBooleanHelper}
{$define TBOOLTYPE:=Boolean}
{$i syshelpb.inc}

{ ---------------------------------------------------------------------
  TBoolean8Helper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TBoolean8Helper}
{$define TBOOLTYPE:=Boolean8}
{$i syshelpb.inc}

{ ---------------------------------------------------------------------
  TBoolean16Helper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TBoolean16Helper}
{$define TBOOLTYPE:=Boolean16}
{$i syshelpb.inc}

{ ---------------------------------------------------------------------
  TBoolean32Helper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TBoolean32Helper}
{$define TBOOLTYPE:=Boolean32}
{$i syshelpb.inc}

{ ---------------------------------------------------------------------
  TBoolean64Helper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TBoolean64Helper}
{$define TBOOLTYPE:=Boolean64}
{$i syshelpb.inc}

{ ---------------------------------------------------------------------
  TByteBoolHelper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TByteBoolHelper}
{$define TBOOLTYPE:=ByteBool}
{$i syshelpb.inc}

{ ---------------------------------------------------------------------
  TWordBoolHelper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TWordBoolHelper}
{$define TBOOLTYPE:=WordBool}
{$i syshelpb.inc}

{ ---------------------------------------------------------------------
  TLongBoolHelper
  ---------------------------------------------------------------------}


{$define TBOOLHELPER:=TLongBoolHelper}
{$define TBOOLTYPE:=LongBool}
{$i syshelpb.inc}

{ ---------------------------------------------------------------------
  TQWordBoolHelper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TQWordBoolHelper}
{$define TBOOLTYPE:=QWordBool}
{$i syshelpb.inc}

