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
   graph,crt;

var
   gd,gm : integer;

{$endif OK}
begin
{$ifdef OK}
   gd:=detect;
   gm:=$103;
   initgraph(gd,gm,'');
   setcolor(white);
   line(1,1,100,100);
   {readkey;}delay(1000);
   closegraph;
   initgraph(gd,gm,'');
   line(100,100,1,100);
   {readkey;}delay(1000);
   closegraph;
{$endif OK}
   writeln('OK');
end.
