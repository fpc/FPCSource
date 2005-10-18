{ %opt=-O2r }

{$mode objfpc}

type
  TFPColor = record
    red,green,blue,alpha : word;
  end;
  TColorData = qword;

  tcl=class
    function ColorColorAlpha16 (CD:TColorData) : TFPColor;
  end;

function tcl.ColorColorAlpha16 (CD:TColorData) : TFPColor;
var c : qword;
begin
with result do
begin
red := CD and $FFFF;
c := qword($FFFF0000);
green := (CD and c) shr 16;
c := c shl 16;
blue := (CD and c) shr 32;
c := c shl 16;
alpha := (CD and c) shr 48;
end;
end;

var
  cd  : tcolordata;
  c : tcl;
begin
  cd:=$1234;
  c:=tcl.create;
  c.colorcoloralpha16(cd);
end.
