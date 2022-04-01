{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
	Test string literals (array of char) is compatible with open arrays
}

program timpfuncspez4;
uses
	typinfo;

generic function DoThis<T>(a: array of T): TTypeKind;
begin
  result := GetTypeKind(T);
end;

begin
	if DoThis('string') <> tkChar then
		Halt(-1);
end.