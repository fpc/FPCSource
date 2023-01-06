{%MainUnit sysutils.pp}
{ ---------------------------------------------------------------------
  TStringHelper
  ---------------------------------------------------------------------}

{$IFNDEF IS_SHORTSTRINGHELPER}
{$IFNDEF IS_UNICODESTRINGHELPER}
// Doubles with (wide/ansi)string...
Function HaveChar(AChar : TStringChar; const AList: array of TStringChar) : Boolean;

Var
  I : SizeInt;

begin
  I:=0;
  Result:=False;
  While (Not Result) and (I<Length(AList)) do
    begin
    Result:=(AList[i]=AChar);
    Inc(I);
    end;
end;
{$ENDIF}
{$ENDIF}

function TStringHelper.GetChar(AIndex: SizeInt): TStringChar;
begin
  Result:=Self[AIndex+1];
end;


function TStringHelper.GetLength: SizeInt;

begin
  Result:=System.Length(Self);
end;


class function TStringHelper.Compare(const A: TStringType; const B: TStringType): Integer;
begin
  Result:=Compare(A,0,B,0,System.Length(B),[]);
end;


class function TStringHelper.Compare(const A: TStringType; const B: TStringType;
  IgnoreCase: Boolean): Integer; //deprecated 'Use same with TCompareOptions';
begin
  if IgnoreCase then
    Result:=Compare(A,B,[coIgnoreCase])
  else
    Result:=Compare(A,B,[]);
end;


class function TStringHelper.Compare(const A: TStringType; const B: TStringType;
  Options: TCompareOptions): Integer;
begin
  Result:=Compare(A,0,B,0,System.Length(B),Options);
end;


class function TStringHelper.Compare(const A: TStringType; IndexA: SizeInt;
  const B: TStringType; IndexB: SizeInt; ALen: SizeInt): Integer;
begin
  Result:=Compare(A,IndexA,B,IndexB,ALen,[]);
end;


class function TStringHelper.Compare(const A: TStringType; IndexA: SizeInt;
  const B: TStringType; IndexB: SizeInt; ALen: SizeInt; IgnoreCase: Boolean
  ): Integer; //deprecated 'Use same with TCompareOptions';
begin
  if IgnoreCase then
    Result:=Compare(A,IndexA,B,IndexB,ALen,[coIgnoreCase])
  else
    Result:=Compare(A,IndexA,B,IndexB,ALen,[])
end;


class function TStringHelper.Compare(const A: TStringType; IndexA: SizeInt;
  const B: TStringType; IndexB: SizeInt; ALen: SizeInt; Options: TCompareOptions
  ): Integer;

Var
  L : SizeInt;

begin
  L:=ALen;
  If (L>system.Length(A)-IndexA) then
    L:=system.Length(A)-IndexA;
  If (L>system.Length(B)-IndexB) then
    L:=system.Length(B)-IndexB;
  if (coIgnoreCase in Options) then
    begin
    Result:=strlicomp(PTStringChar(@A[IndexA+1]),PTStringChar(@B[IndexB+1]),L)
    end
  else
    Result:=strlcomp(PTStringChar(@A[IndexA+1]),PTStringChar(@B[IndexB+1]),L);
end;


class function TStringHelper.CompareOrdinal(const A: TStringType; const B: TStringType
  ): Integer;

Var
  L : SizeInt;

begin
  L:=System.Length(B);
  if L>System.Length(A) then
    L:=System.Length(A);
  Result:=CompareOrdinal(A,0,B,0,L);
end;


class function TStringHelper.CompareOrdinal(const A: TStringType; IndexA: SizeInt;
  const B: TStringType; IndexB: SizeInt; ALen: SizeInt): Integer;

begin
  Result:=StrLComp(PTStringChar(@A[IndexA+1]), PTStringChar(@B[IndexB+1]), ALen);
end;


class function TStringHelper.CompareText(const A: TStringType; const B: TStringType
  ): Integer;
begin
  Result:=Sysutils.CompareText(A,B);
end;


class function TStringHelper.Copy(const Str: TStringType): TStringType;
begin
  Result:=Str;
{$IFNDEF IS_SHORTSTRINGHELPER}
  UniqueString(Result);
{$ENDIF}
end;


class function TStringHelper.Create(AChar: TStringChar; ACount: SizeInt): TStringType;
begin
   Result:=StringOfChar(AChar,ACount);
end;


class function TStringHelper.Create(const AValue: array of TStringChar): TStringType;

begin
  Result:=Create(AValue,0,System.Length(AValue));
end;


class function TStringHelper.Create(const AValue: array of TStringChar;
  StartIndex: SizeInt; ALen: SizeInt): TStringType;
begin
  SetLength(Result,ALen);
  if ALen>0 then
    Move(AValue[StartIndex],Result[1],ALen);
end;


class function TStringHelper.EndsText(const ASubText, AText: TStringType): Boolean;
begin
  Result:=(ASubText<>'') and (sysutils.CompareText(System.Copy(AText,System.Length(AText)-System.Length(ASubText)+1,System.Length(ASubText)),ASubText)=0);
end;


class function TStringHelper.Equals(const a: TStringType; const b: TStringType): Boolean;
begin
  Result:=A=B;
end;


class function TStringHelper.Format(const AFormat: TStringType;
  const args: array of const): TStringType;
begin
  Result:=Sysutils.Format(AFormat,Args);
end;


class function TStringHelper.IsNullOrEmpty(const AValue: TStringType): Boolean;
begin
  Result:=system.Length(AValue)=0;
end;


class function TStringHelper.IsNullOrWhiteSpace(const AValue: TStringType): Boolean;
const
  LWhiteSpace = [#0..' '];
var
  I: SizeInt;
begin
  for I:=1 to System.Length(AValue) do
    if not (AValue[I] in LWhiteSpace) then
      exit(False);
  Result:=True;
end;


class function TStringHelper.Join(const Separator: TStringType;
  const Values: array of const): TStringType;

Var
  SValues : Array of TStringType;
  I,L : SizeInt;
  S : TStringType;
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
      vtPChar    : S:= TStringType(P^.VPChar);
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


class function TStringHelper.Join(const Separator: TStringType;
  const Values: array of TStringType): TStringType;
begin
  Result:=Join(Separator,Values,0,System.Length(Values));
end;


class function TStringHelper.Join(const Separator: TStringType;
  const Values: array of TStringType; StartIndex: SizeInt; ACount: SizeInt): TStringType;
Var
  VLen,I,CountLim,NR,NSep,N : SizeInt;
  Rp: PTStringChar;
begin
  VLen:=System.Length(Values);
  If (ACount=0)  then
    Exit('');
  CountLim:=VLen-StartIndex;
  if ACount>CountLim then
    ACount:=CountLim;
  If (ACount<0) or (StartIndex>VLen) or (StartIndex<0) then
    raise ERangeError.Create(SRangeError);
  if ACount=1 then
    exit(Values[StartIndex]);
  NSep:=System.Length(Separator);
  NR:=(ACount-1)*NSep;
  for I:=StartIndex to StartIndex+ACount-1 do
    NR:=NR+System.Length(Values[I]);
  SetLength(Result,NR);
  Rp:=@Result[1];
  for I:=StartIndex to StartIndex+ACount-1 do
     begin
        if I>StartIndex then
          begin
            Move(separator[1],Rp^,NSep*sizeof(TStringChar));
            Rp:=Rp+NSep;
          end;
        N:=System.Length(Values[I]);
        Move(Values[I][1],Rp^,N*sizeof(TStringChar));
        Rp:=Rp+N;
     end;
end;


class function TStringHelper.LowerCase(const S: TStringType): TStringType;
begin
  Result:=sysutils.Lowercase(S);
end;


class function TStringHelper.Parse(const AValue: Boolean): TStringType;
begin
  Result:=BoolToStr(AValue);
end;


class function TStringHelper.Parse(const AValue: Extended): TStringType;
begin
  Result:=FloatToStr(AValue);
end;


class function TStringHelper.Parse(const AValue: Int64): TStringType;
begin
  Result:=IntToStr(AValue);
end;


class function TStringHelper.Parse(const AValue: Integer): TStringType;
begin
  Result:=IntToStr(AValue);
end;


class function TStringHelper.ToBoolean(const S: TStringType): Boolean;
begin
  Result:=StrToBool(S);
end;


class function TStringHelper.ToDouble(const S: TStringType): Double;
begin
  Result:=StrToFloat(S);
end;


class function TStringHelper.ToExtended(const S: TStringType): Extended;
begin
  Result:=StrToFloat(S);
end;


class function TStringHelper.ToInt64(const S: TStringType): Int64;
begin
  Result:=StrToInt64(S);
end;


class function TStringHelper.ToInteger(const S: TStringType): Integer;
begin
  Result:=StrToInt(S);
end;


class function TStringHelper.ToSingle(const S: TStringType): Single;
begin
  Result:=StrToFloat(S);
end;


class function TStringHelper.UpperCase(const S: TStringType): TStringType;
begin
  Result:=sysutils.Uppercase(S);
end;


function TStringHelper.CompareTo(const B: TStringType): Integer;
begin
  // Order is important
{$IFDEF IS_SHORTSTRINGHELPER}
  Result:=sysUtils.StrComp(PTStringChar(@Self[1]),PTStringChar(@B[1]));
{$ELSE}
  Result:=sysUtils.StrComp(PTStringChar(Self),PTStringChar(B));
{$ENDIF}
end;

procedure TStringHelper.CopyTo(SourceIndex: SizeInt; var destination: array of TStringChar; DestinationIndex: SizeInt; ACount: SizeInt);

Var
  P1,P2 : PTStringChar;
begin
//  Writeln('((',DestinationIndex,'+',ACount,')<',System.Length(Destination),')  : ', ((DestinationIndex+ACount)<System.Length(Destination)));
  if ((DestinationIndex+ACount)<=System.Length(Destination)) then
    begin
//    Writeln('AHA');
    P1:=@Self[SourceIndex+1];
    P2:=@Destination[DestinationIndex];
    Move(P1^,P2^,ACount*SizeOf(TStringChar));
    end;
end;

function TStringHelper.Contains(const AValue: TStringType; IgnoreCase: Boolean): Boolean;
begin
  if IgnoreCase then
    Result:=Pos(LowerCase(AValue),LowerCase(Self))>0
  else
    Result:=Pos(AValue,Self)>0;
end;


function TStringHelper.CountChar(const C: TStringChar): SizeInt;

Var
  S : TStringChar;
begin
  Result:=0;
  For S in Self do
    if (S=C) then
      Inc(Result);
end;


function TStringHelper.DeQuotedString: TStringType;
begin
  Result:=DeQuotedString('''');
end;


function TStringHelper.DeQuotedString(const AQuoteChar: TStringChar): TStringType;

var
  L,I : SizeInt;
  Res : Array of TStringChar;
  PS,PD : PTStringChar;
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


function TStringHelper.EndsWith(const AValue: TStringType): Boolean;
begin
  Result:=EndsWith(AValue,False);
end;


function TStringHelper.EndsWith(const AValue: TStringType; IgnoreCase: Boolean): Boolean;

Var
  L,NS : SizeInt;

begin
  L:=system.Length(AVAlue);
  NS:=System.Length(Self);
  Result:=L<=NS;
  if Result then
    if IgnoreCase then
      Result:=SameText(System.Copy(Self,NS-L+1,L),AValue)
    else
{$IFDEF IS_SHORTSTRINGHELPER}
      Result:=CompareChar(PTStringChar(@Self[1])[NS-L],PTStringChar(@AValue[1])^,L)=0;
{$ELSE}
      Result:=CompareChar(PTStringChar(Pointer(Self))[NS-L],PTStringChar(Pointer(AValue))^,L)=0;
{$ENDIF}
end;


function TStringHelper.Equals(const AValue: TStringType; IgnoreCase: Boolean = False): Boolean;

begin
  if IgnoreCase then
    Result:=SameText(Self,aValue)
  else
    Result:=(Self=AValue);
end;


function TStringHelper.Format(const args: array of const): TStringType;

begin
  Result:=Format(Self,Args);
end;


function TStringHelper.GetHashCode: Integer;

// Taken from contnrs, fphash
var
  P,pmax : PTStringChar;
begin
{$push}
{$Q-}
  Result:=0;
  {$IFDEF IS_SHORTSTRINGHELPER}
  P:=PTStringChar(@Self[1]);
  {$ELSE}
  P:=PTStringChar(Self);
  {$ENDIF}
  pmax:=p+length;
  while (p<pmax) do
    begin
    Result:=LongWord(LongInt(Result shl 5) - LongInt(Result)) xor LongWord(P^);
    Inc(p);
    end;
{$pop}
end;


function TStringHelper.IndexOf(AValue: TStringChar): SizeInt;
begin
  Result:=IndexOf(AValue,0,Length);
end;


function TStringHelper.IndexOf(const AValue: TStringType): SizeInt;
begin
  Result:=IndexOf(AValue,0,Length);
end;


function TStringHelper.IndexOf(AValue: TStringChar; StartIndex: SizeInt): SizeInt;
begin
  Result:=IndexOf(AValue,StartIndex,Length);
end;


function TStringHelper.IndexOf(const AValue: TStringType; StartIndex: SizeInt
  ): SizeInt;
begin
  Result:=IndexOf(AValue,StartIndex,Length);
end;


function TStringHelper.IndexOf(AValue: TStringChar; StartIndex: SizeInt;
  ACount: SizeInt): SizeInt;

Var
  CountLim : SizeInt;

begin
  if StartIndex<0 then
    StartIndex:=0;
  CountLim:=System.Length(Self)-StartIndex;
  if ACount>CountLim then
    ACount:=CountLim;
  if ACount<=0 then
    Exit(-1);
  // pointer casts are to access self as 0 based index!
  {$IFDEF IS_SHORTSTRINGHELPER}
  Result:=IndexChar(PTStringChar(@self[1])[StartIndex],ACount,AValue);
  {$ELSE}
  Result:=IndexChar(PTStringChar(Pointer(self))[StartIndex],ACount,AValue);
  {$ENDIF}
  if Result>=0 then
    Result:=Result+StartIndex;
end;


function TStringHelper.IndexOf(const AValue: TStringType; StartIndex: SizeInt;
  ACount: SizeInt): SizeInt;

Var
  CountLim,NV,Ofs : SizeInt;
  SP,SE : PTStringChar;

begin
  if StartIndex<0 then
    StartIndex:=0;
  CountLim:=System.Length(Self)-StartIndex;
  if ACount>CountLim then
    ACount:=CountLim;
  NV:=System.Length(AValue);
  if (NV>0) and (ACount>=NV) then
    begin
{$IFDEF IS_SHORTSTRINGHELPER}
      SP:=PTStringChar(@Self[1])+StartIndex;
{$ELSE}
      SP:=PTStringChar(Pointer(Self))+StartIndex;
{$ENDIF}
      SE:=SP+ACount-NV+1;
      repeat
        {$IFDEF IS_SHORTSTRINGHELPER}
        Ofs:=IndexChar(SP^,SE-SP,PTStringChar(@AValue[1])[0]);
        {$ELSE}
        Ofs:=IndexChar(SP^,SE-SP,PTStringChar(Pointer(AValue))[0]);
        {$ENDIF}
        if Ofs<0 then
          Break;
        SP:=SP+Ofs+1;
        {$IFDEF IS_SHORTSTRINGHELPER}
        if CompareChar(SP^,PTStringChar(@AValue[1])[1],NV-1)=0 then
          Exit(SP-PTStringChar(@Self[1])-1);
        {$ELSE}
        if CompareChar(SP^,PTStringChar(Pointer(AValue))[1],NV-1)=0 then
          Exit(SP-PTStringChar(Pointer(Self))-1);
        {$ENDIF}
      until false;
    end;
  Result:=-1;
end;

function TStringHelper.IndexOfUnQuoted(const AValue: TStringType; StartQuote,
  EndQuote: TStringChar; StartIndex: SizeInt = 0): SizeInt;

Var
  LV : SizeInt;

  Function MatchAt(I : SizeInt) : Boolean ; Inline;

  Var
    J : SizeInt;

  begin
    J:=1;
    Repeat
      Result:=(Self[I+J-1]=AValue[j]);
      Inc(J);
    Until (Not Result) or (J>LV);
  end;

Var
  I,L,Q: SizeInt;

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


function TStringHelper.IndexOfAny(const AnyOf: array of TStringChar): SizeInt;
begin
  Result:=IndexOfAny(AnyOf,0,Length);
end;


function TStringHelper.IndexOfAny(const AnyOf: array of TStringChar;
  StartIndex: SizeInt): SizeInt;
begin
  Result:=IndexOfAny(AnyOf,StartIndex,Length);
end;


function TStringHelper.IndexOfAny(const AnyOf: array of TStringChar;
  StartIndex: SizeInt; ACount: SizeInt): SizeInt;

Var
  i,L : SizeInt;

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

function TStringHelper.IndexOfAny(const AnyOf: array of TStringType): SizeInt;
begin
  Result:=IndexOfAny(AnyOf,0,Length);
end;

function TStringHelper.IndexOfAny(const AnyOf: array of TStringType;
  StartIndex: SizeInt): SizeInt;
begin
  Result:=IndexOfAny(AnyOf,StartIndex,Length-StartIndex);
end;

function TStringHelper.IndexOfAny(const AnyOf: array of TStringType;
  StartIndex: SizeInt; ACount: SizeInt): SizeInt;

Var
  M : SizeInt;

begin
  Result:=IndexOfAny(AnyOf,StartIndex,ACount,M);
end;

function TStringHelper.IndexOfAny(const AnyOf: array of TStringType;
  StartIndex: SizeInt; ACount: SizeInt; out AMatch: SizeInt): SizeInt;

Var
  L,I : SizeInt;

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


function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of TStringChar;
  StartQuote, EndQuote: TStringChar): SizeInt;
begin
  Result:=IndexOfAnyUnquoted(AnyOf,StartQuote,EndQuote,0,Length);
end;


function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of TStringChar;
  StartQuote, EndQuote: TStringChar; StartIndex: SizeInt): SizeInt;
begin
  Result:=IndexOfAnyUnquoted(AnyOf,StartQuote,EndQuote,StartIndex,Length);
end;


function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of TStringChar;
  StartQuote, EndQuote: TStringChar; StartIndex: SizeInt; ACount: SizeInt): SizeInt;

Var
  I,L : SizeInt;
  Q : SizeInt;

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

function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of TStringType;
  StartQuote, EndQuote: TStringChar; StartIndex: SizeInt; out Matched: SizeInt
  ): SizeInt;

Var
  L,I : SizeInt;

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


function TStringHelper.Insert(StartIndex: SizeInt; const AValue: TStringType
  ): TStringType;
begin
  system.Insert(AValue,Self,StartIndex+1);
  Result:=Self;
end;


function TStringHelper.IsDelimiter(const Delimiters: TStringType; Index: SizeInt
  ): Boolean;
begin
  Result:=sysutils.IsDelimiter(Delimiters,Self,Index+1);
end;


function TStringHelper.IsEmpty: Boolean;
begin
  Result:=(Length=0)
end;


function TStringHelper.LastDelimiter(const Delims: TStringType): SizeInt;
begin
  Result:=sysutils.LastDelimiter(Delims,Self)-1;
end;


function TStringHelper.LastIndexOf(AValue: TStringChar): SizeInt;
begin
  Result:=LastIndexOf(AValue,Length-1,Length);
end;


function TStringHelper.LastIndexOf(const AValue: TStringType): SizeInt;
begin
  Result:=LastIndexOf(AValue,Length-1,Length);
end;


function TStringHelper.LastIndexOf(AValue: TStringChar; AStartIndex: SizeInt): SizeInt;
begin
  Result:=LastIndexOf(AValue,AStartIndex,Length);
end;


function TStringHelper.LastIndexOf(const AValue: TStringType; AStartIndex: SizeInt
  ): SizeInt;
begin
  Result:=LastIndexOf(AValue,AStartIndex,Length);
end;


function TStringHelper.LastIndexOf(AValue: TStringChar; AStartIndex: SizeInt;
  ACount: SizeInt): SizeInt;

Var
  Min : SizeInt;

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


function TStringHelper.LastIndexOf(const AValue: TStringType; AStartIndex: SizeInt; ACount: SizeInt): SizeInt;
  
var
  I,L,LS,M : SizeInt;
  S : TStringType;
  P : PTStringChar;
    
begin
  Result:=-1;
  LS:=system.Length(Self);
  L:=system.Length(AValue);
  if (L=0) or (L>LS) then
    Exit;
{$IFDEF IS_SHORTSTRINGHELPER}
  P:=PTStringChar(@AValue[1]);
{$ELSE}
  P:=PTStringChar(AValue);
{$ENDIF}
  S:=Self;
  I:=AStartIndex+1; // 1 based
  if (I>LS) then
    I:=LS;
  I:=I-L+1;
  M:=AStartIndex-ACount+2; // 1 based
  if M<1 then
    M:=1;
  while (Result=-1) and (I>=M) do
    begin
    if (0=StrLComp(PTStringChar(@S[I]),P,L)) then
      Result:=I-1;
    Dec(I);
    end;
end;


function TStringHelper.LastIndexOfAny(const AnyOf: array of TStringChar): SizeInt;
begin
  Result:=LastIndexOfAny(AnyOf,Length-1,Length);
end;


function TStringHelper.LastIndexOfAny(const AnyOf: array of TStringChar;
  AStartIndex: SizeInt): SizeInt;
begin
  Result:=LastIndexOfAny(AnyOf,AStartIndex,Length);
end;


function TStringHelper.LastIndexOfAny(const AnyOf: array of TStringChar;
  AStartIndex: SizeInt; ACount: SizeInt): SizeInt;

Var
  Min : SizeInt;

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


function TStringHelper.PadLeft(ATotalWidth: SizeInt): TStringType;
begin
  Result:=PadLeft(ATotalWidth,' ');
end;


function TStringHelper.PadLeft(ATotalWidth: SizeInt; PaddingChar: TStringChar): TStringType;
Var
  L : SizeInt;

begin
  Result:=Self;
  L:=ATotalWidth-Length;
  If L>0 then
    Result:=StringOfChar(PaddingChar,L)+Result;
end;


function TStringHelper.PadRight(ATotalWidth: SizeInt): TStringType;
begin
  Result:=PadRight(ATotalWidth,' ');
end;


function TStringHelper.PadRight(ATotalWidth: SizeInt; PaddingChar: TStringChar
  ): TStringType;

Var
  L : SizeInt;

begin
  Result:=Self;
  L:=ATotalWidth-Length;
  If L>0 then
    Result:=Result+StringOfChar(PaddingChar,L);
end;


function TStringHelper.QuotedString: TStringType;
begin
  Result:=QuotedStr(Self);
end;


function TStringHelper.QuotedString(const AQuoteChar: TStringChar): TStringType;
begin
  Result:=AnsiQuotedStr(Self,AQuoteChar);
end;


function TStringHelper.Remove(StartIndex: SizeInt): TStringType;
begin
  Result:=Remove(StartIndex,Self.Length-StartIndex);
end;


function TStringHelper.Remove(StartIndex: SizeInt; ACount: SizeInt): TStringType;
begin
  Result:=Self;
  System.Delete(Result,StartIndex+1,ACount);
end;


function TStringHelper.Replace(OldChar: TStringChar; NewChar: TStringChar): TStringType;
begin
  Result:=Replace(OldChar,NewChar,[rfReplaceAll]);
end;


function TStringHelper.Replace(OldChar: TStringChar; NewChar: TStringChar;
  ReplaceFlags: TReplaceFlags): TStringType;
var
  Sp,Se,Rp : PTStringChar;
  Ofs : SizeInt;
begin
  if rfIgnoreCase in ReplaceFlags then
    exit(StringReplace(Self,OldChar,NewChar,ReplaceFlags));
{$IFDEF IS_SHORTSTRINGHELPER}
  Sp:=PTStringChar(@Self[1]);
{$ELSE}
  Sp:=PTStringChar(Pointer(Self));
{$ENDIF}
  Se:=Sp+System.Length(Self);
  Ofs:=IndexChar(Sp^,Se-Sp,OldChar);
  if Ofs<0 then
    exit(Self);
  SetLength(Result,Se-Sp);
{$IFDEF IS_SHORTSTRINGHELPER}
  Rp:=PTStringChar(@Result[1]);
{$ELSE}
  Rp:=PTStringChar(Pointer(Result));
{$ENDIF}
  repeat
    Move(Sp^,Rp^,Ofs*sizeof(TStringChar));
    Sp:=Sp+Ofs+1;
    Rp[Ofs]:=NewChar;
    Rp:=Rp+Ofs+1;
    if not (rfReplaceAll in ReplaceFlags) then
      break;
    { This loop can be removed entirely, but greatly speeds up replacing streaks of characters. }
    while (Sp<Se) and (Sp^=OldChar) do
      begin
        Rp^:=NewChar;
        Sp:=Sp+1;
        Rp:=Rp+1;
      end;
    Ofs:=IndexChar(Sp^,Se-Sp,OldChar);
  until Ofs<0;
  Move(Sp^,Rp^,(Se-Sp)*sizeof(TStringChar));
end;


function TStringHelper.Replace(const OldValue: TStringType; const NewValue: TStringType
  ): TStringType;
begin
  Result:=Replace(OldValue,NewValue,[rfReplaceAll]);
end;


function TStringHelper.Replace(const OldValue: TStringType; const NewValue: TStringType;
  ReplaceFlags: TReplaceFlags): TStringType;
begin
  Result:=StringReplace(Self,OldValue,NewValue,ReplaceFlags);
end;


function TStringHelper.Split(const Separators: array of TStringChar): TSHStringArray;
begin
  Result:=Split(Separators,#0,#0,Length+1,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of TStringChar; ACount: SizeInt
  ): TSHStringArray;
begin
  Result:=Split(Separators,#0,#0,ACount,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of TStringChar;
  Options: TStringSplitOptions): TSHStringArray;
begin
  Result:=Split(Separators,Length+1,Options);
end;


function TStringHelper.Split(const Separators: array of TStringChar; ACount: SizeInt;
  Options: TStringSplitOptions): TSHStringArray;
begin
  Result:=Split(Separators,#0,#0,ACount,Options);
end;


function TStringHelper.Split(const Separators: array of TStringType): TSHStringArray;
begin
  Result:=Split(Separators,Length+1);
end;


function TStringHelper.Split(const Separators: array of TStringType; ACount: SizeInt
  ): TSHStringArray;
begin
  Result:=Split(Separators,ACount,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of TStringType;
  Options: TStringSplitOptions): TSHStringArray;
begin
  Result:=Split(Separators,Length+1,Options);
end;


function TStringHelper.Split(const Separators: array of TStringType;
  ACount: SizeInt; Options: TStringSplitOptions): TSHStringArray;
begin
  Result:=Split(Separators,#0,#0,ACount,Options);
end;


function TStringHelper.Split(const Separators: array of TStringChar; AQuote: TStringChar
  ): TSHStringArray;
begin
  Result:=Split(Separators,AQuote,AQuote);
end;


function TStringHelper.Split(const Separators: array of TStringChar; AQuoteStart,
  AQuoteEnd: TStringChar): TSHStringArray;
begin
  Result:=Split(Separators,AQuoteStart,AQuoteEnd,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of TStringChar; AQuoteStart,
  AQuoteEnd: TStringChar; Options: TStringSplitOptions): TSHStringArray;
begin
  Result:=Split(Separators,AQuoteStart,AQuoteEnd,Length+1,Options);
end;


function TStringHelper.Split(const Separators: array of TStringChar; AQuoteStart,
  AQuoteEnd: TStringChar; ACount: SizeInt): TSHStringArray;
begin
  Result:=Split(Separators,AQuoteStart,AQuoteEnd,ACount,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of TStringChar; AQuoteStart,
  AQuoteEnd: TStringChar; ACount: SizeInt; Options: TStringSplitOptions): TSHStringArray;

  Function NextSep(StartIndex : SizeInt) : SizeInt;

  begin
    if (AQuoteStart<>#0) then
      Result:=Self.IndexOfAnyUnQuoted(Separators,AQuoteStart,AQuoteEnd,StartIndex)
    else
      Result:=Self.IndexOfAny(Separators,StartIndex);
  end;

  Procedure MaybeGrow(Curlen : SizeInt);

  begin
    if System.Length(Result)<=CurLen then
      SetLength(Result,System.Length(Result)+4+SizeInt(SizeUint(System.Length(Result)) div 4));
  end;

Var
  Sep,LastSep,Len : SizeInt;

begin
  Result:=nil;
  Len:=0;
  LastSep:=0;
  While ((ACount=0) or (Len<ACount)) and (LastSep<=System.Length(Self)) do
    begin
    Sep:=NextSep(LastSep);
    if Sep<0 then
      Sep:=System.Length(Self);
//    Writeln('Examining >',T,'< at pos ',LastSep,', till pos ',Sep);
    If (Sep>LastSep) or (not (TStringSplitOptions.ExcludeEmpty=Options)) then
      begin
      MaybeGrow(Len);
      Result[Len]:=SubString(LastSep,Sep-LastSep);
      Inc(Len);
      end;
    LastSep:=Sep+1;
    end;

  if (TStringSplitOptions.ExcludeLastEmpty=Options) then
    if (Len > 0) and (Result[Len-1] = '') then
      dec(Len);

  SetLength(Result,Len);
end;


function TStringHelper.Split(const Separators: array of TStringType; AQuote: TStringChar
  ): TSHStringArray;
begin
  Result:=SPlit(Separators,AQuote,AQuote);
end;


function TStringHelper.Split(const Separators: array of TStringType; AQuoteStart,
  AQuoteEnd: TStringChar): TSHStringArray;
begin
  Result:=SPlit(Separators,AQuoteStart,AQuoteEnd,Length+1,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of TStringType; AQuoteStart,
  AQuoteEnd: TStringChar; Options: TStringSplitOptions): TSHStringArray;
begin
  Result:=SPlit(Separators,AQuoteStart,AQuoteEnd,Length+1,Options);
end;


function TStringHelper.Split(const Separators: array of TStringType; AQuoteStart,
  AQuoteEnd: TStringChar; ACount: SizeInt): TSHStringArray;
begin
  Result:=SPlit(Separators,AQuoteStart,AQuoteEnd,ACount,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of TStringType; AQuoteStart,
  AQuoteEnd: TStringChar; ACount: SizeInt; Options: TStringSplitOptions): TSHStringArray;
Const
  BlockSize = 10;

  Function NextSep(StartIndex : SizeInt; out Match : SizeInt) : SizeInt;

  begin
    if (AQuoteStart<>#0) then
      Result:=Self.IndexOfAnyUnQuoted(Separators,AQuoteStart,AQuoteEnd,StartIndex,Match)
    else
      Result:=Self.IndexOfAny(Separators,StartIndex,Length,Match);
    if Result<>-1 then
  end;

  Procedure MaybeGrow(Curlen : SizeInt);

  begin
    if System.Length(Result)<=CurLen then
      SetLength(Result,System.Length(Result)+BlockSize);
  end;

Var
  Sep,LastSep,Len,Match : SizeInt;
  T : TStringType;

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
  if (LastSep<=Length) and ((ACount=0) or (Len<ACount)) then
    begin
    T:=SubString(LastSep);
//    Writeln('Examining >',T,'< at pos,',LastSep,' till pos ',Sep);
    If (T<>'') or (not (TStringSplitOptions.ExcludeEmpty=Options)) then
      begin
      MaybeGrow(Len);
      Result[Len]:=T;
      Inc(Len);
      end;
    end;

  If (TStringSplitOptions.ExcludeLastEmpty=Options) then
    if (Len > 0) and (Result[Len-1] = '') then
      dec(Len);

  SetLength(Result,Len);
end;


function TStringHelper.StartsWith(const AValue: TStringType): Boolean;
begin
  Result:=StartsWith(AValue,False);
end;


function TStringHelper.StartsWith(const AValue: TStringType; IgnoreCase: Boolean
  ): Boolean;
Var
  L : SizeInt;

begin
  L:=System.Length(AValue);
  Result:=L<=System.Length(Self);
  if Result then
    if IgnoreCase then
      Result:=SameText(System.Copy(Self,1,L),AValue)
    else
{$IFDEF IS_SHORTSTRINGHELPER}
      Result:=CompareChar(PTStringChar(@Self[1])^,PTStringChar(@AValue[1])^,L)=0;
{$ELSE}
      Result:=CompareChar(PTStringChar(Pointer(Self))^,PTStringChar(Pointer(AValue))^,L)=0;
{$ENDIF}
end;


function TStringHelper.Substring(AStartIndex: SizeInt): TStringType;
begin
  Result:=Self.SubString(AStartIndex,Self.Length-AStartIndex);
end;


function TStringHelper.Substring(AStartIndex: SizeInt; ALen: SizeInt): TStringType;
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


function TStringHelper.ToCharArray(AStartIndex: SizeInt; ALen: SizeInt
  ): TCharArray;

Var
  I : SizeInt;

begin
  SetLength(Result,ALen);
  For I:=0 to ALen-1 do
    Result[I]:=Self[AStartIndex+I+1];
end;


function TStringHelper.ToLower: TStringType;
begin
  Result:=LowerCase(Self);
end;


function TStringHelper.ToLowerInvariant: TStringType;
begin
  Result:=LowerCase(Self);
end;


function TStringHelper.ToUpper: TStringType;
begin
  Result:=UpperCase(Self);
end;


function TStringHelper.ToUpperInvariant: TStringType;
begin
  Result:=UpperCase(Self);
end;


function TStringHelper.Trim: TStringType;
begin
  Result:=SysUtils.Trim(Self);
end;


function TStringHelper.TrimLeft: TStringType;
begin
  Result:=SysUtils.TrimLeft(Self);
end;


function TStringHelper.TrimRight: TStringType;
begin
  Result:=SysUtils.TrimRight(Self);
end;


function TStringHelper.Trim(const ATrimChars: array of TStringChar): TStringType;
begin
  Result:=Self.TrimLeft(ATrimChars).TrimRight(ATrimChars);
end;


function TStringHelper.TrimLeft(const ATrimChars: array of TStringChar): TStringType;

Var
  I,Len : SizeInt;

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


function TStringHelper.TrimRight(const ATrimChars: array of TStringChar): TStringType;

Var
  I,Len : SizeInt;

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


function TStringHelper.TrimEnd(const ATrimChars: array of TStringChar): TStringType;
begin
  Result:=TrimRight(ATrimChars);
end;


function TStringHelper.TrimStart(const ATrimChars: array of TStringChar): TStringType;
begin
  Result:=TrimLeft(ATrimChars);
end;
