uses
   graph,crt;

var
   gd,gm : integer;

begin
   gd:=detect;
   initgraph(gd,gm,'');
   line(1,1,100,100);
   readkey;
   setgraphmode($107);
   line(100,100,1024,800);
   readkey;
   closegraph;
end.
   
