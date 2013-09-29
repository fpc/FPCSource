{ %fail }

var
  astr: bitpacked array[1..5] of #0..#127;
  str: string;
  i: integer;

begin
    writeln('Assigning a bitpacked array to a string variable:');
    for i := 1 to 5 do
        astr[i] := 'a';
    str := astr;
end.
