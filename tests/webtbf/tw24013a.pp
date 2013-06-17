{ %fail }

var
  astr: bitpacked array[1..5] of #0..#127;
  str: string;

begin
    writeln('Assigning a variable string to astr:');
    str := 'aaaaa';
    astr := str;
end.
