procedure pw(const S : RawByteString); overload;
  begin
    WriteLn('fail');
  end;

procedure pw(const S : WideString); overload;
  begin
    WriteLn('ok');
  end;

procedure pu(const S : RawByteString); overload;
  begin
    WriteLn('fail');
  end;

procedure pu(const S : UnicodeString); overload;
  begin
    WriteLn('ok');
  end;

var
  u : UnicodeString;
  w : WideString;
begin
  pw(u);
  pu(w);
end.