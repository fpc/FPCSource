{ %fail }

var
  astr: bitpacked array[1..5] of #0..#127;

begin
    writeln('Assigning a literal string to astr:');
    astr := 'aaaaa';
end.
