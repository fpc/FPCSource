procedure p(const S : RawByteString); overload;
  begin
    writeln(s,'AnsiString prefers RawByteString');
  end;

procedure p(const S : UTF8String); overload;
  begin
    writeln(s,'AnsiString prefers UTF8String');
    { This is not expected, so generate error }
    writeln('Unexpected, changed behavior!');
    halt(1);
  end;
var
  s1 : Ansistring;
begin
  s1:='Test: ';
  p(s1);
end.
