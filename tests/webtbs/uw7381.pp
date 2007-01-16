{$mode objfpc}
unit uw7381;


interface


type
    c1 = class
        procedure p1;
    end;
    c1array = array [0..1] of c1;


implementation


procedure c1.p1;
begin
end;


procedure test(const a: c1array);inline;
begin
    a[0].p1;
end;


end.