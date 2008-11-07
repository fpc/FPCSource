{ %fail }
{ %opt=-Sew -vw-}

{$mode macpas}
program popwarnings;
var
    p: Pointer;
    offs: PtrInt;
    a: array[ 1..100] of integer;
begin
{$warnings on}
    p:= @a;
{$push}
{$warnings off}
{$pop}
    PtrUInt(p):=PtrUInt(p) + offs;
end.
