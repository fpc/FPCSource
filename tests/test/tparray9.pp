{ %fail }

program PCErrorB;
{$bitpacking on}
{$r+}

var
chs :packed array [1..10] of char;
ch1 :array[1..10] of char;

begin
unpack(chs,ch1,2);  { WRONG }
end.
