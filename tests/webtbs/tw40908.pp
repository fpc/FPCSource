{$minenumsize 1}
type
	MyEnum = (a, b, c);
var
	e: MyEnum;
begin
	e := MyEnum(random(0) = 0);
    if ord(e)<>1 then
      halt(1);
	writeln('sizeof(e) = ', sizeof(e), ', ord(e) = ', ord(e));
end.
