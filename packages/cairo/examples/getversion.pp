uses
  cairo;

var
  major, minor, micro: LongInt;

begin
  cairo_version(major, minor, micro);
  writeln(major,'.',minor,'.',micro);
end.
