{ %opt=-vw -Sew }
{ %norun }
{$mode macpas}
program nowarnings;
var
    p: Pointer;
    offs: PtrInt;
    a: array[ 1..100] of integer;
begin
{$warnings on}
    p:= @a;
    offs:= SizeOf(integer);
{$warnings off}
    PtrUInt(p):=PtrInt(p) + offs;
{$warnings on}
end.
