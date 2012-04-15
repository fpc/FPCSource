{ %fail }
procedure p(var S : RawByteString);
  begin
  end;

var
  s1 : UTF8String;

begin
  p(s1);
end.
