{$MODE ISO}

{$r+}

program range ( output );

const  ttlow = 0;   tthigh  =  800;

type   ttx = ttlow .. tthigh;

var    ttop : ttx;

procedure p ( low : ttx );
var  high : ttx;
begin
   for high := low to ttop - 1 do
     halt(1);
end;

begin
   writeln(sizeof(ttop));
   ttop := 0;
   p( 1 );
end.
