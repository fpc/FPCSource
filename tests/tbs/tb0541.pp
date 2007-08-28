const
  TestStr: widestring = 'Test';

var
  buf: array[0..10] of widechar;
  s: widestring;

begin
  Move(TestStr[1], buf[0], (Length(TestStr) + 1)*SizeOf(widechar));
  s:=buf;
  writeln(s);
  buf[0]:=#0;
  s:=buf;
end.
