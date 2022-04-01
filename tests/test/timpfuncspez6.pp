{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
	Test "array of enum" is compatible with open arrays
}

program timpfuncspez6;
uses
	typinfo;

type
	TMyEnum = (_A, _B, _C);

generic procedure DoThis<T>(value: array of T);
begin
	if GetTypeKind(value[0]) <> tkEnumeration then
	  Halt(-1);
end;

begin
	DoThis([_A, _B]);
end.