uses
   graph,crt;
        
var
   gd,gm : integer;

begin
   gd:=detect;
   gm:=$103;
   initgraph(gd,gm,'');
   line(1,1,100,100);
   readkey;
   closegraph;
   initgraph(gd,gm,'');
   line(100,100,1,100);
   readkey;
   closegraph;
end.
