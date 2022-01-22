
Class Function TORDINALHELPER.Parse(const AString: string): TORDINALTYPE; inline; static;

var
  Error: Integer;
begin
  Val(AString,Result,Error);
  if Error<>0 then
    raise EConvertError.CreateFmt(SInvalidInteger,[AString]);
end;

Class Function TORDINALHELPER.Size: Integer; inline; static;

begin
  Result:=SizeOf(TORDINALTYPE);
end;

Class Function TORDINALHELPER.ToString(const AValue: TORDINALTYPE): string; overload; inline; static;

begin
  Str(AValue,Result);
end;

Class Function TORDINALHELPER.TryParse(const AString: string; out AValue: TORDINALTYPE): Boolean; inline; static;

Var
  C : Integer;

begin
  Val(AString,AValue,C);
  Result:=(C=0);
end;

Function TORDINALHELPER.ToBoolean: Boolean; inline;

begin
  Result:=(Self<>0);
end;

Function TORDINALHELPER.ToDouble: Double; inline;

begin
  Result:=Self;
end;

Function TORDINALHELPER.ToExtended: Extended; inline;

begin
  Result:=Self;
end;

Function TORDINALHELPER.ToBinString: string; inline;

begin
  Result:=BinStr(Self,SizeOf(TORDINALTYPE)*8);
end;

Function TORDINALHELPER.ToHexString(const AMinDigits: Integer): string;
overload; inline;

begin
  Result:=IntToHex(Self,AMinDigits);
end;

Function TORDINALHELPER.ToHexString: string; overload; inline;

begin
  Result:=IntToHex(Self);
end;

Function TORDINALHELPER.ToSingle: Single; inline;

begin
  Result:=Self;
end;

Function TORDINALHELPER.ToString: string; overload; inline;

begin
  Str(Self,Result);
end;

Function TORDINALHELPER.SetBit(const index: TORDINALBITINDEX) : TORDINALTYPE; inline;

begin
  Self := Self or (TORDINALTYPE(1) shl index);
  Result:=Self;
end;

Function TORDINALHELPER.ClearBit(const index: TORDINALBITINDEX) : TORDINALTYPE; inline;

begin
  Self:=Self and not TORDINALTYPE((TORDINALTYPE(1) shl index));
  Result:=Self;
end;

Function TORDINALHELPER.ToggleBit(const index: TORDINALBITINDEX) : TORDINALTYPE; inline;

begin
  Self := Self xor TORDINALTYPE((TORDINALTYPE(1) shl index));
  Result:=Self;
end;

Function TORDINALHELPER.TestBit(const Index: TORDINALBITINDEX):Boolean; inline;

begin
  Result := (Self and TORDINALTYPE((TORDINALTYPE(1) shl index)))<>0;
end;

procedure TORDINALHELPER.Clear;
begin
  Self := 0;
end;

function TORDINALHELPER.HighestSetBitPos: int8;
begin
  {$ifdef TORDINALTYPESIZE1}
        Result := int8(BsrByte(byte(Self)));
  {$else}
    {$ifdef TORDINALTYPESIZE2}
        Result := int8(BsrWord(word(Self)));
    {$else}
      {$ifdef TORDINALTYPESIZE4}
        Result := int8(BsrDWord(dword(Self)));
      {$else} // TORDINALTYPESIZE8
        Result := int8(BsrQWord(qword(Self)));
      {$endif}
    {$endif}
  {$endif}
end;

function TORDINALHELPER.LowestSetBitPos: int8;
begin
  {$ifdef TORDINALTYPESIZE1}
        Result := int8(BsfByte(byte(Self)));
  {$else}
    {$ifdef TORDINALTYPESIZE2}
        Result := int8(BsfWord(word(Self)));
    {$else}
      {$ifdef TORDINALTYPESIZE4}
        Result := int8(BsfDWord(dword(Self)));
      {$else} // TORDINALTYPESIZE8
        Result := int8(BsfQWord(qword(Self)));
      {$endif}
    {$endif}
  {$endif}
end;

function TORDINALHELPER.SetBitsCount: byte;
begin
  {$ifdef TORDINALTYPESIZE1}
{$ifdef VER3_2_2}
        Result := PopCnt(Word(byte(Self)));
{$else VER3_2_2}
        Result := PopCnt(byte(Self));
{$endif VER3_2_2}
  {$else}
    {$ifdef TORDINALTYPESIZE2}
        Result := PopCnt(word(Self));
    {$else}
      {$ifdef TORDINALTYPESIZE4}
        Result := PopCnt(dword(Self));
      {$else} // TORDINALTYPESIZE8
        Result := PopCnt(qword(Self));
      {$endif}
    {$endif}
  {$endif}
end;

function TORDINALHELPER.GetBit(const aIndex: TORDINALBITINDEX): boolean;
begin
  Result := ((Self shr aIndex) and TORDINALTYPE(1)) = TORDINALTYPE(1);
end;

procedure TORDINALHELPER.PutBit(const aIndex: TORDINALBITINDEX; const aNewValue: boolean);
begin
  Self := Self or (TORDINALTYPE(1) shl aIndex) xor (TORDINALTYPE(not aNewValue) shl aIndex);
end;

function TORDINALHELPER.GetNibble(const aIndex: TORDINALNIBBLEINDEX): nibble;
begin
  Result := TORDINALOVERLAY(Self).AsNibble[aIndex];
end;

procedure TORDINALHELPER.PutNibble(const aIndex: TORDINALNIBBLEINDEX; const aNewValue: nibble);
begin
  TORDINALOVERLAY(Self).AsNibble[aIndex] := aNewValue;
end;

{$ifndef TORDINALTYPESIZE1} // TWordHelper, TDWordHelper, TQWordHelper jump in here (and others with 2, 4 and 8 bytes)
  function TORDINALHELPER.GetByte(const aIndex: TORDINALBYTEINDEX): byte;
  begin
    Result := TORDINALOVERLAY(Self).AsByte[aIndex];
  end;

  procedure TORDINALHELPER.PutByte(const aIndex: TORDINALBYTEINDEX; const aNewValue: byte);
  begin
    TORDINALOVERLAY(Self).AsByte[aIndex] := aNewValue;
  end;

  {$ifndef TORDINALTYPESIZE2} // TDWordHelper, TQWordHelper jump in here (and others with 4 and 8 bytes)
    function TORDINALHELPER.GetWord(const aIndex: TORDINALWORDINDEX): word;
    begin
      Result := TORDINALOVERLAY(Self).AsWord[aIndex];
    end;

    procedure TORDINALHELPER.PutWord(const aIndex: TORDINALWORDINDEX; const aNewValue: word);
    begin
      TORDINALOVERLAY(Self).AsWord[aIndex] := aNewValue;
    end;

    {$ifndef TORDINALTYPESIZE4} // TQWordHelper jumps in here (and others with 8 bytes)
      function TORDINALHELPER.GetDword(const aIndex: TORDINALDWORDINDEX): dword;
      begin
        Result := TORDINALOVERLAY(Self).AsDword[aIndex];
      end;

      procedure TORDINALHELPER.PutDword(const aIndex: TORDINALDWORDINDEX; const aNewValue: dword);
      begin
        TORDINALOVERLAY(Self).AsDword[aIndex] := aNewValue;
      end;
    {$endif}

  {$endif}

{$endif}
