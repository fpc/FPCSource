program testgoto;

{$goto on}

function test : longint;

label l;


var
   a,b : longint;

begin
   a:=1;
   b:=1;
   l:
     if a>b then
       begin
          exit(0);
       end;
   a:=2;
   goto l;
end;

begin
   test;
end.
