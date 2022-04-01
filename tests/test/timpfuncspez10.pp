{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
	Test generic parameter order
}

program timpfuncspez10;
uses
  typinfo;

generic function DoThis<S, T>(p1: T; p2: S; t1, t2: TTypeKind): boolean;
begin
	result := (GetTypeKind(S) = t1) and (GetTypeKind(T) = t2);
	writeln('S:',GetTypeKind(S), ' T:',GetTypeKind(T), ' = ', result);
end;

begin
	if not DoThis(1, 'a', tkChar, tkInteger) then
		Halt(-1);
	if not DoThis('a', 1, tkInteger, tkChar) then
		Halt(-1);
end.