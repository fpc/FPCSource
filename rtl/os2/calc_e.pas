program calc_e;

{Calculate the number e.}

const   fac:array[0..7] of word=(1,1,2,6,24,120,720,5040);

var e:fixed;
    i:byte;

begin
    e:=0;
    for i:=0 to 7 do
        e:=e+fixed(1)/fac[i];
    writeln(e);
end.
