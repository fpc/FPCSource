{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
	Test specialized arrays and array constructors
}

program timpfuncspez12;
uses
	typinfo;

type
	generic TMyArray<T> = array of T;

generic function DoThis<T>(a: specialize TMyArray<T>): TTypeKind;
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