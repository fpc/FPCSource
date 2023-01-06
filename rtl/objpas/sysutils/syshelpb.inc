{%MainUnit sysutils.pp}

Class Function TBOOLHELPER.Parse(const S: string): Boolean; inline; static;

begin
  Result:=StrToBool(S);
end;

Class Function TBOOLHELPER.Size: Integer; inline; static;

begin
  Result:=SizeOf(TBOOLTYPE);
end;

Class Function TBOOLHELPER.ToString(const AValue: Boolean; UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline; static;

begin
  Result:=BoolToStr(AValue,UseBoolStrs=TUseBoolStrs.True);
end;

Class Function TBOOLHELPER.TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;

begin
  Result:=TryStrToBool(S,AValue);
end;

Function TBOOLHELPER.ToInteger: Integer; inline;

begin
  Result:=Integer(Self);
end;

Function TBOOLHELPER.ToString(UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline;

begin
  Result:=BoolToStr(Self,UseBoolStrs=TUseBoolStrs.True);
end;
