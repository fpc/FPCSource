{ %GRAPH }

uses graph;
var
 gd,gm:integer;
 testimage:array[1..50000] of byte; {this is plenty big}
begin
 gd:=VESA;
 gm:=$100; { 640 x 400 x 256 }
 initgraph(gd,gm,'');
 if graphresult<>grOk then
   begin
     Writeln('Unable to open driver ',gd,' in mode ',gm);
     Halt(1);
   end;
 line(0,0,639,399);
 getimage(190,49,257,125,testimage);
 { a simple statement, and yet
   it throws a General Protection fault, but only with certain
   numbers for getimage.  The numbers i have here do not produce
   too big an image for the array testimage, and yet it faults.
   Is this a bug in getimage, or is there something i am
   missing here?
  }
 closegraph;
end.
