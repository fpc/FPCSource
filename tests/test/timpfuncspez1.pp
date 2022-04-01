{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
	Test precedence
}

program timpfuncspez1;

function Test(const aStr: String): LongInt;
begin
  Result := 1;
end;

generic function Test<T>(aT: T): LongInt;
begin
  Result := 2;
end;

operator := (aArg: LongInt): String;
begin
  Result := '';
end;

begin
	if Test('Hello World')<>1 then
		Halt(-1);
	if Test(42)<>2 then
		Halt(-1);
	if Test(String(42))<>1 then
		Halt(-1);
end.