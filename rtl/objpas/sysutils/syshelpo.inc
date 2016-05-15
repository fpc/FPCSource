
Class Function TORDINALHELPER.Parse(const AString: string): TORDINALTYPE; inline; static;

begin
  Result:=StrToInt(AString);
end;

Class Function TORDINALHELPER.Size: Integer; inline; static;

begin
  Result:=SizeOf(TORDINALTYPE);
end;

Class Function TORDINALHELPER.ToString(const AValue: TORDINALTYPE): string; overload; inline; static;

begin
  Result:=IntToStr(AValue);
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

Function TORDINALHELPER.ToHexString(const AMinDigits: Integer): string;
overload; inline;

begin
  Result:=HexStr(Self,AMinDigits);
end;

Function TORDINALHELPER.ToHexString: string; overload; inline;

begin
  Result:=HexStr(Self,SizeOf(TORDINALTYPE)*2);
end;

Function TORDINALHELPER.ToSingle: Single; inline;

begin
  Result:=Self;
end;

Function TORDINALHELPER.ToString: string; overload; inline;

begin
  Result:=IntToStr(Self);
end;
