{$ifdef go32v2}
{$define OK}
{$endif}
{$ifdef linux}
{$define OK}
{$endif}
{$ifdef win32}
{$define OK}
{$endif}

{$ifdef OK}
uses
   graph,
   crt;

var
   gd,gm : integer;
{$endif OK}

begin
{$ifdef OK}
   gd:=detect;
   initgraph(gd,gm,'');
   line(1,1,100,100);
   {readkey;}
   setgraphmode($107);
   line(100,100,1024,800);
   {readkey;}
   delay(1000);
   closegraph;
{$endif OK}
end.

