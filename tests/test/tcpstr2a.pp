{ %fail }
procedure p(const S : RawByteString); overload;
  begin
  end;

procedure p(const S : UTF8String); overload;
  begin
  end;
var
  s1 : Ansistring;
begin
  p(s1);
end.
