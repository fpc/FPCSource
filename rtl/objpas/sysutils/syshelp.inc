{%mainunit syshelpers.pp}

{ ---------------------------------------------------------------------
  TGUIDHelper
  ---------------------------------------------------------------------}

Procedure NotImplemented(S : String);

begin
  Raise Exception.Create('Not yet implemented : '+S);
end;

Class function TGUIDHelper.Create(const Data; BigEndian: Boolean): TGUID; overload; static;

Const
  GUIDSize = SizeOf(TGUID);

Var
  B : Array[1..GUIDSize] of Byte;

begin
  Move(Data,B,GUIDSize);
  Result:=Create(B,0,BigEndian);
end;

class function TGUIDHelper.Create(const Data: array of Byte; AStartIndex: Cardinal; BigEndian: Boolean): TGUID; overload; static;

Var
  A : Cardinal;
  B,C : Word;

begin
  if ((System.Length(Data)-AStartIndex)<16) then
    raise EArgumentException.CreateFmt('The length of a GUID array must be at least %d',[]);
  Move(Data[AStartIndex],A,SizeOf(Cardinal));
  Move(Data[AStartIndex+4],B,SizeOf(Word));
  Move(Data[AStartIndex+6],C,SizeOf(Word));
//  Writeln('BigEndian : ',BigEndian,', CPU bigendian : ',(CPUendian=TEndian.Big));
  if BigEndian<>(CPUendian=TEndian.Big) then
    begin
//    Writeln('Swapping');
    A:=SwapEndian(A);
    B:=SwapEndian(B);
    C:=SwapEndian(C);
    end;
  Result:=Create(A,B,C,Data[AStartIndex+8],Data[AStartIndex+9],Data[AStartIndex+10],Data[AStartIndex+11],Data[AStartIndex+12],Data[AStartIndex+13],Data[AStartIndex+14],Data[AStartIndex+15]);
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

{ ---------------------------------------------------------------------
  TStringHelper
  ---------------------------------------------------------------------}

Function HaveChar(AChar : Char; const AList: array of Char) : Boolean;

Var
  I : Integer;

begin
  I:=0;
  Result:=False;
  While (Not Result) and (I<Length(AList)) do
    begin
    Result:=(AList[i]=AChar);
    Inc(I);
    end;
end;

function TStringHelper.GetChar(AIndex: Integer): Char;
begin
  Result:=Self[AIndex+1];
end;


function TStringHelper.GetLength: Integer;

begin
  Result:=System.Length(Self);
end;


class function TStringHelper.Compare(const A: string; const B: string): Integer;
begin
  Result:=Compare(A,0,B,0,System.Length(B),[]);
end;


class function TStringHelper.Compare(const A: string; const B: string;
  IgnoreCase: Boolean): Integer; //deprecated 'Use same with TCompareOptions';
begin
  if IgnoreCase then
    Result:=Compare(A,B,[coIgnoreCase])
  else
    Result:=Compare(A,B,[]);
end;


class function TStringHelper.Compare(const A: string; const B: string;
  Options: TCompareOptions): Integer;
begin
  Result:=Compare(A,0,B,0,System.Length(B),Options);
end;


class function TStringHelper.Compare(const A: string; IndexA: Integer;
  const B: string; IndexB: Integer; ALen: Integer): Integer;
begin
  Result:=Compare(A,IndexA,B,IndexB,ALen,[]);
end;


class function TStringHelper.Compare(const A: string; IndexA: Integer;
  const B: string; IndexB: Integer; ALen: Integer; IgnoreCase: Boolean
  ): Integer; //deprecated 'Use same with TCompareOptions';
begin
  if IgnoreCase then
    Result:=Compare(A,IndexA,B,IndexB,ALen,[coIgnoreCase])
  else
    Result:=Compare(A,IndexA,B,IndexB,ALen,[])
end;


class function TStringHelper.Compare(const A: string; IndexA: Integer;
  const B: string; IndexB: Integer; ALen: Integer; Options: TCompareOptions
  ): Integer;

Var
  l : Integer;

begin
  L:=ALen;
  If (L>system.Length(A)-IndexA) then
    L:=system.Length(A)-IndexA;
  If (L>system.Length(B)-IndexB) then
    L:=system.Length(B)-IndexB;
  if (coIgnoreCase in Options) then
    begin
    Result:=strlicomp(PAnsiChar(@A[IndexA+1]),PAnsiChar(@B[IndexB+1]),L)
    end
  else
    Result:=strlcomp(PAnsiChar(@A[IndexA+1]),PAnsiChar(@B[IndexB+1]),L);
end;


class function TStringHelper.CompareOrdinal(const A: string; const B: string
  ): Integer;

Var
  L : Integer;

begin
  L:=System.Length(B);
  if L>System.Length(A) then
    L:=System.Length(A);
  Result:=CompareOrdinal(A,0,B,0,L);
end;


class function TStringHelper.CompareOrdinal(const A: string; IndexA: Integer;
  const B: string; IndexB: Integer; ALen: Integer): Integer;

begin
  Result:=StrLComp(PAnsiChar(@A[IndexA+1]), PAnsiChar(@B[IndexB+1]), ALen);
end;


class function TStringHelper.CompareText(const A: string; const B: string
  ): Integer;
begin
  Result:=Sysutils.CompareText(A,B);
end;


class function TStringHelper.Copy(const Str: string): string;
begin
  Result:=Str;
  UniqueString(Result);
end;


class function TStringHelper.Create(AChar: Char; ACount: Integer): string;
begin
   Result:=StringOfChar(AChar,ACount);
end;


class function TStringHelper.Create(const AValue: array of Char): string;

begin
  Result:=Create(AValue,0,System.Length(AValue));
end;


class function TStringHelper.Create(const AValue: array of Char;
  StartIndex: Integer; ALen: Integer): string;
begin
  SetLength(Result,ALen);
  if ALen>0 then
    Move(AValue[StartIndex],Result[1],ALen);
end;


class function TStringHelper.EndsText(const ASubText, AText: string): Boolean;
begin
  Result:=(ASubText<>'') and (sysutils.CompareText(System.Copy(AText,System.Length(AText)-System.Length(ASubText)+1,System.Length(ASubText)),ASubText)=0);
end;


class function TStringHelper.Equals(const a: string; const b: string): Boolean;
begin
  Result:=A=B;
end;


class function TStringHelper.Format(const AFormat: string;
  const args: array of const): string;
begin
  Result:=Sysutils.Format(AFormat,Args);
end;


class function TStringHelper.IsNullOrEmpty(const AValue: string): Boolean;
begin
  Result:=system.Length(AValue)=0;
end;


class function TStringHelper.IsNullOrWhiteSpace(const AValue: string): Boolean;
begin
  Result:=system.Length(SysUtils.Trim(AValue))=0;
end;


class function TStringHelper.Join(const Separator: string;
  const Values: array of const): string;

Var
  SValues : Array of string;
  I,L : Integer;
  S : String;
  P : ^TVarRec;

begin
  L:=System.Length(Values);
  SetLength(SValues,L);
  Dec(L);
  for I:=0 to L do
    begin
    S:='';
    P:=@Values[I];
    Case P^.VType of
      vtInteger  : S:=IntToStr(P^.VInteger);
      vtBoolean  : S:=BoolToStr(P^.VBoolean, True);
      vtChar     : S:=P^.VChar;
      vtPChar    : S:= string(P^.VPChar);
      {$ifndef FPUNONE}
      vtExtended : S:=FloatToStr(P^.VExtended^);
      {$endif}
      vtObject   : S:=TObject(P^.VObject).Classname;
      vtClass    : S:=P^.VClass.Classname;
      vtCurrency : S:=CurrToStr(P^.VCurrency^);
      vtVariant  : S:=(P^.VVariant^);
      vtInt64    : S:=IntToStr(PInt64(P^.VInt64)^);
      vtQword    : S:=IntToStr(PQWord(P^.VQword)^);
      vtWideChar     : S:=WideString(P^.VWideChar);
      vtPWideChar     : S:=WideString(P^.VPWideChar);
      vtUnicodeString : S:=UnicodeString(P^.VUnicodeString);
      vtAnsiString    : S:=Ansistring(P^.VAnsiString);
    else
      S:=Format('Unknown type: %d',[P^.VType]);
    end;
    SValues[I]:=S;
    end;
  Result:=Join(Separator,SValues);
end;


class function TStringHelper.Join(const Separator: string;
  const Values: array of string): string;
begin
  Result:=Join(Separator,Values,0,System.Length(Values));
end;


class function TStringHelper.Join(const Separator: string;
  const Values: array of string; StartIndex: Integer; ACount: Integer): string;

Var
  I,L,VLen : integer;

begin
  VLen:=High(Values);
  If (ACount<0) or ((StartIndex>0) and (StartIndex>VLen)) then
    raise ERangeError.Create(SRangeError);
  If (ACount=0) or (VLen<0) then
    Result:=''
  else
    begin
    L:=StartIndex+ACount-1;
    if L>Vlen then
      L:=VLen;
    Result:=Values[StartIndex];
    For I:=StartIndex+1 to L do
      Result:=Result+Separator+Values[I];
    end;
end;


class function TStringHelper.LowerCase(const S: string): string;
begin
  Result:=sysutils.Lowercase(S);
end;


class function TStringHelper.Parse(const AValue: Boolean): string;
begin
  Result:=BoolToStr(AValue);
end;


class function TStringHelper.Parse(const AValue: Extended): string;
begin
  Result:=FloatToStr(AValue);
end;


class function TStringHelper.Parse(const AValue: Int64): string;
begin
  Result:=IntToStr(AValue);
end;


class function TStringHelper.Parse(const AValue: Integer): string;
begin
  Result:=IntToStr(AValue);
end;


class function TStringHelper.ToBoolean(const S: string): Boolean;
begin
  Result:=StrToBool(S);
end;


class function TStringHelper.ToDouble(const S: string): Double;
begin
  Result:=StrToFloat(S);
end;


class function TStringHelper.ToExtended(const S: string): Extended;
begin
  Result:=StrToFloat(S);
end;


class function TStringHelper.ToInt64(const S: string): Int64;
begin
  Result:=StrToInt64(S);
end;


class function TStringHelper.ToInteger(const S: string): Integer;
begin
  Result:=StrToInt(S);
end;


class function TStringHelper.ToSingle(const S: string): Single;
begin
  Result:=StrToFloat(S);
end;


class function TStringHelper.UpperCase(const S: string): string;
begin
  Result:=sysutils.Uppercase(S);
end;


function TStringHelper.CompareTo(const B: string): Integer;
begin
  // Order is important
  Result:=sysUtils.StrComp(PChar(Self),PChar(B));
end;

procedure TStringHelper.CopyTo(SourceIndex: Integer; var destination: array of Char; DestinationIndex: Integer; ACount: Integer);

Var
  P1,P2 : PChar;
begin
//  Writeln('((',DestinationIndex,'+',ACount,')<',System.Length(Destination),')  : ', ((DestinationIndex+ACount)<System.Length(Destination)));
  if ((DestinationIndex+ACount)<=System.Length(Destination)) then
    begin
//    Writeln('AHA');
    P1:=@Self[SourceIndex+1];
    P2:=@Destination[DestinationIndex];
    Move(P1^,P2^,ACount*SizeOf(Char));
    end;
end;

function TStringHelper.Contains(const AValue: string): Boolean;
begin
  Result:=Pos(AValue,Self)>0;
end;


function TStringHelper.CountChar(const C: Char): Integer;

Var
  S : Char;
begin
  Result:=0;
  For S in Self do
    if (S=C) then
      Inc(Result);
end;


function TStringHelper.DeQuotedString: string;
begin
  Result:=DeQuotedString('''');
end;


function TStringHelper.DeQuotedString(const AQuoteChar: Char): string;

var
  L,I : Integer;
  Res : Array of Char;
  PS,PD : PChar;
  IsQuote : Boolean;

begin
  L:=System.Length(Self);
  if (L<2) or Not ((Self[1]=AQuoteChar) and (Self[L]=AQuoteChar)) then
    Exit(Self);
  SetLength(Res,L);
  IsQuote:=False;
  PS:=@Self[2];
  PD:=@Res[0];
  For I:=2 to L-1 do
    begin
    if (PS^=AQuoteChar) then
      begin
      IsQuote:=Not IsQuote;
      if Not IsQuote then
        begin
        PD^:=PS^;
        Inc(PD);
        end;
      end
    else
      begin
      if IsQuote then
        IsQuote:=false;
      PD^:=PS^;
      Inc(PD);
      end;
    Inc(PS);
    end;
  SetString(Result,@Res[0],PD-@Res[0]);
end;


function TStringHelper.EndsWith(const AValue: string): Boolean;
begin
  Result:=EndsWith(AValue,False);
end;


function TStringHelper.EndsWith(const AValue: string; IgnoreCase: Boolean): Boolean;

Var
  L : integer;
  S : String;
begin
  L:=system.Length(AVAlue);
  Result:=L>0;
  if Result then
    begin
    S:=system.Copy(Self,Length-L+1,L);
    Result:=system.Length(S)=L;
    if Result then
      if IgnoreCase then
        Result:=CompareText(S,AValue)=0
      else
        Result:=S=AValue;
    end;
end;

function TStringHelper.Equals(const AValue: string): Boolean;
begin
  Result:=(Self=AValue);
end;

function TStringHelper.Format(const args: array of const): string;
begin
  Result:=Format(Self,Args);
end;


function TStringHelper.GetHashCode: Integer;

// Taken from contnrs, fphash
var
  P,pmax : PChar;
begin
{$push}
{$Q-}
  Result:=0;
  P:=Pchar(Self);
  pmax:=p+length;
  while (p<pmax) do
    begin
    Result:=LongWord(LongInt(Result shl 5) - LongInt(Result)) xor LongWord(P^);
    Inc(p);
    end;
{$pop}
end;


function TStringHelper.IndexOf(AValue: Char): Integer;
begin
  Result:=IndexOf(AValue,0,Length);
end;


function TStringHelper.IndexOf(const AValue: string): Integer;
begin
  Result:=IndexOf(AValue,0,Length);
end;


function TStringHelper.IndexOf(AValue: Char; StartIndex: Integer): Integer;
begin
  Result:=IndexOf(AValue,StartIndex,Length);
end;


function TStringHelper.IndexOf(const AValue: string; StartIndex: Integer
  ): Integer;
begin
  Result:=IndexOf(AValue,StartIndex,Length);
end;


function TStringHelper.IndexOf(AValue: Char; StartIndex: Integer;
  ACount: Integer): Integer;

Var
  S : String;

begin
  S:=System.Copy(Self,StartIndex+1,ACount);
  Result:=Pos(AValue,S)-1;
  if Result<>-1 then
    Result:=Result+StartIndex;
end;


function TStringHelper.IndexOf(const AValue: string; StartIndex: Integer;
  ACount: Integer): Integer;

Var
  S : String;

begin
  S:=System.Copy(Self,StartIndex+1,ACount);
  Result:=Pos(AValue,S)-1;
  if Result<>-1 then
    Result:=Result+StartIndex;
end;

function TStringHelper.IndexOfUnQuoted(const AValue: string; StartQuote,
  EndQuote: Char; StartIndex: Integer = 0): Integer;

Var
  LV : Integer;

  Function MatchAt(I : Integer) : Boolean ; Inline;

  Var
    J : integer;

  begin
    J:=1;
    Repeat
      Result:=(Self[I+J-1]=AValue[j]);
      Inc(J);
    Until (Not Result) or (J>LV);
  end;

Var
  I,L: Integer;
  Q : Integer;

begin
  Result:=-1;
  LV:=system.Length(AValue);
  L:=Length-LV+1;
  if L<0 then
    L:=0;
  I:=StartIndex+1;
  Q:=0;
  if StartQuote=EndQuote then
    begin
    While (Result=-1) and (I<=L) do
      begin
      if (Self[I]=StartQuote) then
        Q:=1-Q;
      if (Q=0) and MatchAt(i) then
        Result:=I-1;
      Inc(I);
      end;
    end
  else
    begin
    While (Result=-1) and (I<=L) do
      begin
      if Self[I]=StartQuote then
        Inc(Q)
      else if (Self[I]=EndQuote) and (Q>0) then
        Dec(Q);
      if (Q=0) and MatchAt(i) then
        Result:=I-1;
      Inc(I);
      end;
    end;
end;


function TStringHelper.IndexOfAny(const AnyOf: array of Char): Integer;
begin
  Result:=IndexOfAny(AnyOf,0,Length);
end;


function TStringHelper.IndexOfAny(const AnyOf: array of Char;
  StartIndex: Integer): Integer;
begin
  Result:=IndexOfAny(AnyOf,StartIndex,Length);
end;


function TStringHelper.IndexOfAny(const AnyOf: array of Char;
  StartIndex: Integer; ACount: Integer): Integer;

Var
  i,L : Integer;

begin
  I:=StartIndex+1;
  L:=I+ACount-1;
  If L>Length then
    L:=Length;
  Result:=-1;
  While (Result=-1) and (I<=L) do
    begin
    if HaveChar(Self[i],AnyOf) then
      Result:=I-1;
    Inc(I);
    end;
end;

function TStringHelper.IndexOfAny(const AnyOf: array of String): Integer;
begin
  Result:=IndexOfAny(AnyOf,0,Length);
end;

function TStringHelper.IndexOfAny(const AnyOf: array of String;
  StartIndex: Integer): Integer;
begin
  Result:=IndexOfAny(AnyOf,StartIndex,Length-StartIndex);
end;

function TStringHelper.IndexOfAny(const AnyOf: array of String;
  StartIndex: Integer; ACount: Integer): Integer;

Var
  M : Integer;

begin
  Result:=IndexOfAny(AnyOf,StartIndex,ACount,M);
end;

function TStringHelper.IndexOfAny(const AnyOf: array of String;
  StartIndex: Integer; ACount: Integer; out AMatch: Integer): Integer;

Var
  L,I : Integer;

begin
  Result:=-1;
  For I:=0 to System.Length(AnyOf)-1 do
    begin
    L:=IndexOf(AnyOf[i],StartIndex,ACount);
    If (L>=0) and ((Result=-1) or (L<Result)) then
      begin
      Result:=L;
      AMatch:=I;
      end;
    end;
end;


function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of Char;
  StartQuote, EndQuote: Char): Integer;
begin
  Result:=IndexOfAnyUnquoted(AnyOf,StartQuote,EndQuote,0,Length);
end;


function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of Char;
  StartQuote, EndQuote: Char; StartIndex: Integer): Integer;
begin
  Result:=IndexOfAnyUnquoted(AnyOf,StartQuote,EndQuote,StartIndex,Length);
end;


function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of Char;
  StartQuote, EndQuote: Char; StartIndex: Integer; ACount: Integer): Integer;

Var
  I,L : Integer;
  Q : Integer;

begin
  Result:=-1;
  L:=StartIndex+ACount-1;
  if L>Length then
    L:=Length;
  I:=StartIndex+1;
  Q:=0;
  if StartQuote=EndQuote then
    begin
    While (Result=-1) and (I<=L) do
      begin
      if (Self[I]=StartQuote) then
        Q:=1-Q;
      if (Q=0) and HaveChar(Self[i],AnyOf) then
        Result:=I-1;
      Inc(I);
      end;
    end
  else
  begin
    While (Result=-1) and (I<=L) do
      begin
      if Self[I]=StartQuote then
        Inc(Q)
      else if (Self[I]=EndQuote) and (Q>0) then
        Dec(Q);
      if (Q=0) and HaveChar(Self[i],AnyOf) then
        Result:=I-1;
      Inc(I);
      end;
    end;

end;

function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of string;
  StartQuote, EndQuote: Char; StartIndex: Integer; out Matched: Integer
  ): Integer;

Var
  L,I : Integer;

begin
  Result:=-1;
  For I:=0 to System.Length(AnyOf)-1 do
    begin
    L:=IndexOfUnquoted(AnyOf[i],StartQuote,EndQuote,StartIndex);
    If (L>=0) and ((Result=-1) or (L<Result)) then
      begin
      Result:=L;
      Matched:=I;
      end;
    end;
end;


function TStringHelper.Insert(StartIndex: Integer; const AValue: string
  ): string;
begin
  system.Insert(AValue,Self,StartIndex+1);
  Result:=Self;
end;


function TStringHelper.IsDelimiter(const Delimiters: string; Index: Integer
  ): Boolean;
begin
  Result:=sysutils.IsDelimiter(Delimiters,Self,Index+1);
end;


function TStringHelper.IsEmpty: Boolean;
begin
  Result:=(Length=0)
end;


function TStringHelper.LastDelimiter(const Delims: string): Integer;
begin
  Result:=sysutils.LastDelimiter(Delims,Self)-1;
end;


function TStringHelper.LastIndexOf(AValue: Char): Integer;
begin
  Result:=LastIndexOf(AValue,Length-1,Length);
end;


function TStringHelper.LastIndexOf(const AValue: string): Integer;
begin
  Result:=LastIndexOf(AValue,Length-1,Length);
end;


function TStringHelper.LastIndexOf(AValue: Char; AStartIndex: Integer): Integer;
begin
  Result:=LastIndexOf(AValue,AStartIndex,Length);
end;


function TStringHelper.LastIndexOf(const AValue: string; AStartIndex: Integer
  ): Integer;
begin
  Result:=LastIndexOf(AValue,AStartIndex,Length);
end;


function TStringHelper.LastIndexOf(AValue: Char; AStartIndex: Integer;
  ACount: Integer): Integer;

Var
  Min : Integer;

begin
  Result:=AStartIndex+1;
  Min:=Result-ACount+1;
  If Min<1 then
    Min:=1;
  While (Result>=Min) and (Self[Result]<>AValue) do
    Dec(Result);
  if Result<Min then
    Result:=-1
  else
    Result:=Result-1;
end;


function TStringHelper.LastIndexOf(const AValue: string; AStartIndex: Integer;
  ACount: Integer): Integer;
begin
  Result:=LastIndexOf(AValue,AStartIndex,AStartIndex+1);
end;


function TStringHelper.LastIndexOfAny(const AnyOf: array of Char): Integer;
begin
  Result:=LastIndexOfAny(AnyOf,Length-1,Length);
end;


function TStringHelper.LastIndexOfAny(const AnyOf: array of Char;
  AStartIndex: Integer): Integer;
begin
  Result:=LastIndexOfAny(AnyOf,AStartIndex,Length);
end;


function TStringHelper.LastIndexOfAny(const AnyOf: array of Char;
  AStartIndex: Integer; ACount: Integer): Integer;

Var
  Min : Integer;

begin
  Result:=AStartIndex+1;
  Min:=Result-ACount+1;
  If Min<1 then
    Min:=1;
  While (Result>=Min) and Not HaveChar(Self[Result],AnyOf) do
    Dec(Result);
  if Result<Min then
    Result:=-1
  else
    Result:=Result-1;
end;


function TStringHelper.PadLeft(ATotalWidth: Integer): string;
begin
  Result:=PadLeft(ATotalWidth,' ');
end;


function TStringHelper.PadLeft(ATotalWidth: Integer; PaddingChar: Char): string;
Var
  L : Integer;

begin
  Result:=Self;
  L:=ATotalWidth-Length;
  If L>0 then
    Result:=StringOfChar(PaddingChar,L)+Result;
end;


function TStringHelper.PadRight(ATotalWidth: Integer): string;
begin
  Result:=PadRight(ATotalWidth,' ');
end;


function TStringHelper.PadRight(ATotalWidth: Integer; PaddingChar: Char
  ): string;

Var
  L : Integer;

begin
  Result:=Self;
  L:=ATotalWidth-Length;
  If L>0 then
    Result:=Result+StringOfChar(PaddingChar,L);
end;


function TStringHelper.QuotedString: string;
begin
  Result:=QuotedStr(Self);
end;


function TStringHelper.QuotedString(const AQuoteChar: Char): string;
begin
  Result:=AnsiQuotedStr(Self,AQuoteChar);
end;


function TStringHelper.Remove(StartIndex: Integer): string;
begin
  Result:=Remove(StartIndex,Self.Length-StartIndex);
end;


function TStringHelper.Remove(StartIndex: Integer; ACount: Integer): string;
begin
  Result:=Self;
  System.Delete(Result,StartIndex+1,ACount);
end;


function TStringHelper.Replace(OldChar: Char; NewChar: Char): string;
begin
  Result:=Replace(OldChar,NewChar,[rfReplaceAll]);
end;


function TStringHelper.Replace(OldChar: Char; NewChar: Char;
  ReplaceFlags: TReplaceFlags): string;
begin
  Result:=StringReplace(Self,OldChar,NewChar,ReplaceFlags);
end;


function TStringHelper.Replace(const OldValue: string; const NewValue: string
  ): string;
begin
  Result:=Replace(OldValue,NewValue,[rfReplaceAll]);
end;


function TStringHelper.Replace(const OldValue: string; const NewValue: string;
  ReplaceFlags: TReplaceFlags): string;
begin
  Result:=StringReplace(Self,OldValue,NewValue,ReplaceFlags);
end;


function TStringHelper.Split(const Separators: array of Char): TStringArray;
begin
  Result:=SPlit(Separators,#0,#0,Length,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of Char; ACount: Integer
  ): TStringArray;
begin
  Result:=SPlit(Separators,#0,#0,ACount,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of Char;
  Options: TStringSplitOptions): TStringArray;
begin
  Result:=SPlit(Separators,Length,Options);
end;


function TStringHelper.Split(const Separators: array of Char; ACount: Integer;
  Options: TStringSplitOptions): TStringArray;
begin
  Result:=SPlit(Separators,#0,#0,ACount,Options);
end;


function TStringHelper.Split(const Separators: array of string): TStringArray;
begin
  Result:=Split(Separators,Length);
end;


function TStringHelper.Split(const Separators: array of string; ACount: Integer
  ): TStringArray;
begin
  Result:=Split(Separators,ACount,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of string;
  Options: TStringSplitOptions): TStringArray;
begin
  Result:=Split(Separators,Length,Options);
end;


function TStringHelper.Split(const Separators: array of string;
  ACount: Integer; Options: TStringSplitOptions): TStringArray;
begin
  Result:=Split(Separators,#0,#0,ACount,Options);
end;


function TStringHelper.Split(const Separators: array of Char; AQuote: Char
  ): TStringArray;
begin
  Result:=Split(Separators,AQuote,AQuote);
end;


function TStringHelper.Split(const Separators: array of Char; AQuoteStart,
  AQuoteEnd: Char): TStringArray;
begin
  Result:=Split(Separators,AQuoteStart,AQuoteEnd,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of Char; AQuoteStart,
  AQuoteEnd: Char; Options: TStringSplitOptions): TStringArray;
begin
  Result:=Split(Separators,AQuoteStart,AQuoteEnd,Length,Options);
end;


function TStringHelper.Split(const Separators: array of Char; AQuoteStart,
  AQuoteEnd: Char; ACount: Integer): TStringArray;
begin
  Result:=Split(Separators,AQuoteStart,AQuoteEnd,ACount,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of Char; AQuoteStart,
  AQuoteEnd: Char; ACount: Integer; Options: TStringSplitOptions): TStringArray;

Const
  BlockSize = 10;

  Function NextSep(StartIndex : integer) : Integer;

  begin
    if (AQuoteStart<>#0) then
      Result:=Self.IndexOfAnyUnQuoted(Separators,AQuoteStart,AQuoteEnd,StartIndex)
    else
      Result:=Self.IndexOfAny(Separators,StartIndex);
  end;

  Procedure MaybeGrow(Curlen : Integer);

  begin
    if System.Length(Result)<=CurLen then
      SetLength(Result,System.Length(Result)+BlockSize);
  end;

Var
  Sep,LastSep,Len : integer;
  T : String;

begin
  SetLength(Result,BlockSize);
  Len:=0;
  LastSep:=0;
  Sep:=NextSep(0);
  While (Sep<>-1) and ((ACount=0) or (Len<ACount)) do
    begin
    T:=SubString(LastSep,Sep-LastSep);
//    Writeln('Examining >',T,'< at pos,',LastSep,' till pos ',Sep);
    If (T<>'') or (not (TStringSplitOptions.ExcludeEmpty=Options)) then
      begin
      MaybeGrow(Len);
      Result[Len]:=T;
      Inc(Len);
      end;
    LastSep:=Sep+1;
    Sep:=NextSep(LastSep);
    end;
  if (LastSep<Length) and ((ACount=0) or (Len<ACount)) then
    begin
    T:=SubString(LastSep);
//    Writeln('Examining >',T,'< at pos,',LastSep,' till pos ',Sep);
    MaybeGrow(Len);
    Result[Len]:=T;
    Inc(Len);
    end;
  SetLength(Result,Len);
end;


function TStringHelper.Split(const Separators: array of string; AQuote: Char
  ): TStringArray;
begin
  Result:=SPlit(Separators,AQuote,AQuote);
end;


function TStringHelper.Split(const Separators: array of string; AQuoteStart,
  AQuoteEnd: Char): TStringArray;
begin
  Result:=SPlit(Separators,AQuoteStart,AQuoteEnd,Length,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of string; AQuoteStart,
  AQuoteEnd: Char; Options: TStringSplitOptions): TStringArray;
begin
  Result:=SPlit(Separators,AQuoteStart,AQuoteEnd,Length,Options);
end;


function TStringHelper.Split(const Separators: array of string; AQuoteStart,
  AQuoteEnd: Char; ACount: Integer): TStringArray;
begin
  Result:=SPlit(Separators,AQuoteStart,AQuoteEnd,ACount,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of string; AQuoteStart,
  AQuoteEnd: Char; ACount: Integer; Options: TStringSplitOptions): TStringArray;
Const
  BlockSize = 10;

  Function NextSep(StartIndex : integer; out Match : Integer) : Integer;

  begin
    if (AQuoteStart<>#0) then
      Result:=Self.IndexOfAnyUnQuoted(Separators,AQuoteStart,AQuoteEnd,StartIndex,Match)
    else
      Result:=Self.IndexOfAny(Separators,StartIndex,Length,Match);
    if Result<>-1 then
  end;

  Procedure MaybeGrow(Curlen : Integer);

  begin
    if System.Length(Result)<=CurLen then
      SetLength(Result,System.Length(Result)+BlockSize);
  end;

Var
  Sep,LastSep,Len,Match : integer;
  T : String;

begin
  SetLength(Result,BlockSize);
  Len:=0;
  LastSep:=0;
  Sep:=NextSep(0,Match);
  While (Sep<>-1) and ((ACount=0) or (Len<ACount)) do
    begin
    T:=SubString(LastSep,Sep-LastSep);
    If (T<>'') or (not (TStringSplitOptions.ExcludeEmpty=Options)) then
      begin
      MaybeGrow(Len);
      Result[Len]:=T;
      Inc(Len);
      end;
    LastSep:=Sep+System.Length(Separators[Match]);
    Sep:=NextSep(LastSep,Match);
    end;
  if (LastSep<Length-1) and ((ACount=0) or (Len<ACount)) then
    begin
    T:=SubString(LastSep);
//    Writeln('Examining >',T,'< at pos,',LastSep,' till pos ',Sep);
    MaybeGrow(Len);
    Result[Len]:=T;
    Inc(Len);
    end;
  SetLength(Result,Len);
end;


function TStringHelper.StartsWith(const AValue: string): Boolean;
begin
  Result:=StartsWith(AValue,False);
end;


function TStringHelper.StartsWith(const AValue: string; IgnoreCase: Boolean
  ): Boolean;
Var
  L : Integer;
begin
  L:=System.Length(AValue);
  Result:=L>0;
  if Result then
    if IgnoreCase then
      Result:=StrLiComp(PChar(AValue),PChar(Self),L)=0
    else
      Result:=StrLComp(PChar(AValue),PChar(Self),L)=0
end;


function TStringHelper.Substring(AStartIndex: Integer): string;
begin
  Result:=Self.SubString(AStartIndex,Self.Length-AStartIndex);
end;


function TStringHelper.Substring(AStartIndex: Integer; ALen: Integer): string;
begin
  Result:=system.Copy(Self,AStartIndex+1,ALen);
end;


function TStringHelper.ToBoolean: Boolean;
begin
  Result:=StrToBool(Self);
end;


function TStringHelper.ToInteger: Integer;
begin
  Result:=StrToInt(Self);
end;


function TStringHelper.ToInt64: Int64;
begin
  Result:=StrToInt64(Self);
end;


function TStringHelper.ToSingle: Single;
begin
  Result:=StrToFLoat(Self);
end;


function TStringHelper.ToDouble: Double;
begin
  Result:=StrToFLoat(Self);
end;


function TStringHelper.ToExtended: Extended;
begin
  Result:=StrToFLoat(Self);
end;


function TStringHelper.ToCharArray: TCharArray;

begin
  Result:=ToCharArray(0,Self.Length);
end;


function TStringHelper.ToCharArray(AStartIndex: Integer; ALen: Integer
  ): TCharArray;

Var
  I : Integer;

begin
  SetLength(Result,ALen);
  For I:=0 to ALen-1 do
    Result[I]:=Self[AStartIndex+I+1];
end;


function TStringHelper.ToLower: string;
begin
  Result:=LowerCase(Self);
end;


function TStringHelper.ToLowerInvariant: string;
begin
  Result:=LowerCase(Self);
end;


function TStringHelper.ToUpper: string;
begin
  Result:=UpperCase(Self);
end;


function TStringHelper.ToUpperInvariant: string;
begin
  Result:=UpperCase(Self);
end;


function TStringHelper.Trim: string;
begin
  Result:=SysUtils.Trim(Self);
end;


function TStringHelper.TrimLeft: string;
begin
  Result:=SysUtils.TrimLeft(Self);
end;


function TStringHelper.TrimRight: string;
begin
  Result:=SysUtils.TrimRight(Self);
end;


function TStringHelper.Trim(const ATrimChars: array of Char): string;
begin
  Result:=Self.TrimLeft(ATrimChars).TrimRight(ATrimChars);
end;


function TStringHelper.TrimLeft(const ATrimChars: array of Char): string;

Var
  I,Len : Integer;

begin
  I:=1;
  Len:=Self.Length;
  While (I<=Len) and HaveChar(Self[i],ATrimChars) do Inc(I);
  if I=1 then
    Result:=Self
  else if I>Len then
    Result:=''
  else
    Result:=system.Copy(Self,I,Len-I+1);
end;


function TStringHelper.TrimRight(const ATrimChars: array of Char): string;

Var
  I,Len : Integer;

begin
  Len:=Self.Length;
  I:=Len;
  While (I>=1) and HaveChar(Self[i],ATrimChars) do Dec(I);
  if I<1 then
    Result:=''
  else if I=Len then
    Result:=Self
  else
    Result:=system.Copy(Self,1,I);
end;


function TStringHelper.TrimEnd(const ATrimChars: array of Char): string;
begin
  Result:=TrimRight(ATrimChars);
end;


function TStringHelper.TrimStart(const ATrimChars: array of Char): string;
begin
  Result:=TrimLeft(ATrimChars);
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
{$ENDIF FPC_HAS_TYPE_SINGLE}

{ ---------------------------------------------------------------------
  TDoubleHelper
  ---------------------------------------------------------------------}
{$IFDEF FPC_HAS_TYPE_DOUBLE}
{$define TFLOATHELPER:=TDoubleHelper}
{$define FLOATTYPE:=Double}
{$define TFloatRec:=TDoubleRec}
{$i syshelpf.inc}
{$ENDIF FPC_HAS_TYPE_DOUBLE}

{ ---------------------------------------------------------------------
  TExtendedHelper
  ---------------------------------------------------------------------}

{$ifdef FPC_HAS_TYPE_EXTENDED}
{$define TFLOATHELPER:=TExtendedHelper}
{$define FLOATTYPE:=Extended}
{$define TFloatRec:=TExtended80Rec}
{$i syshelpf.inc}
{$ENDIF FPC_HAS_TYPE_EXTENDED}

{ ---------------------------------------------------------------------
  TByteHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TByteHelper}
{$define TORDINALTYPE:=Byte}
{$i syshelpo.inc}

{ ---------------------------------------------------------------------
  TShortintHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TShortIntHelper}
{$define TORDINALTYPE:=ShortInt}
{$i syshelpo.inc}

{ ---------------------------------------------------------------------
  TSmallintHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TSmallIntHelper}
{$define TORDINALTYPE:=SmallInt}
{$i syshelpo.inc}

{ ---------------------------------------------------------------------
  TWordHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TWordHelper}
{$define TORDINALTYPE:=Word}
{$i syshelpo.inc}

{ ---------------------------------------------------------------------
  TCardinalHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TCardinalHelper}
{$define TORDINALTYPE:=Cardinal}
{$i syshelpo.inc}

{ ---------------------------------------------------------------------
  TIntegerHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TIntegerHelper}
{$define TORDINALTYPE:=Integer}
{$i syshelpo.inc}

{ ---------------------------------------------------------------------
  TInt64Helper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TInt64Helper}
{$define TORDINALTYPE:=Int64}
{$i syshelpo.inc}

{ ---------------------------------------------------------------------
  TQWordHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TQWordHelper}
{$define TORDINALTYPE:=QWord}
{$i syshelpo.inc}

{ ---------------------------------------------------------------------
  TNativeIntHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TNativeIntHelper}
{$define TORDINALTYPE:=NativeInt}
{$i syshelpo.inc}

{ ---------------------------------------------------------------------
  TNativeUIntHelper
  ---------------------------------------------------------------------}

{$define TORDINALHELPER:=TNativeUIntHelper}
{$define TORDINALTYPE:=NativeUInt}
{$i syshelpo.inc}

{ ---------------------------------------------------------------------
  TBooleanHelper
  ---------------------------------------------------------------------}

{$define TBOOLHELPER:=TBooleanHelper}
{$define TBOOLTYPE:=Boolean}
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

