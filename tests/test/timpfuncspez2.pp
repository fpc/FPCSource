{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
	Test open arrays and array constructors
}

program timpfuncspez2;
uses
	typinfo;

generic function DoThis<T>(a: array of T): TTypeKind;
begin
	writeln(GetTypeKind(T), ': ', Length(a));
  result := GetTypeKind(T);
end;

begin
	if DoThis(['a','b','c']) <> tkChar then
		Halt(-1);
	if DoThis(['aaa','bbb','ccc']) <> tkString then
		Halt(-1);
	if DoThis([1,2,3]) <> tkInteger then
		Halt(-1);
end.