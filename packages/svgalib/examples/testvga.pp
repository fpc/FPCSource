program testvga;

uses svgalib;

var i : longint;
    mode : longint;

begin
  vga_init;
  repeat
    Write ('Mode (0 exits) : ');
    Readln (Mode);
    if (mode<>0) and vga_hasmode(Mode) then
      begin
      vga_setmode(Mode);
      //  vga_screenon;
      vga_setcolor(vga_white);
      for i:=1 to 100 do
        vga_drawline (100+2*i,100,100+2*i,200);
      readln;
      vga_setmode(0);
      end;
  until mode=0;
end.
