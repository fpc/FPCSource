{ %norun }
program tw18152;
{$TypedAddress on}
var
    p: ^integer;
    c: char;
begin
    // test that addr return untyped pointer in spite of $TypedAddress directive
    p := addr(c)
end.