{ Old file: tbs0076.pp }
{  bugs in intel asm generator. was already fixed        OK 0.99.1 (FK) }

program bug0076;

{Generates wrong code when compiled with output set to intel asm.

 Reported from mailinglist by Vtech Kavan.

 15 Januari 1998, Daniel Mantione}

type  TVtx2D = record x,y:longint end;

var  Vtx2d:array[0..2] of TVtx2D;

function SetupScanLines(va,vb,vc:word):single;
var dx3d,dx2d,dy2d,dz,ex3d,ex2d,ez:longint;
    r:single;
begin
 dy2d := Vtx2d[vb].y;
 r    := (dy2d-Vtx2d[va].y);     {this line causes error!!!!!!!!!!!!!!!!!!!}
end;

begin
 SetupScanLines(1,2,3);
end.
