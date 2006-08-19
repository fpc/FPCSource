{ %fail }

{ from gpc test suite }
program PCErrorA;

{$r+}
var
chs :bitpacked array [1..10] of char;
ch1 :array[1..10] of char;

begin
pack(ch1,2,chs);  { WRONG }
end.

