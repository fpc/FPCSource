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
end.  $Log$
end.  Revision 1.1  2002-01-29 17:55:22  peter
end.    * splitted to base and extra
end.
end.  Revision 1.2  2000/07/13 11:33:31  michael
end.  + removed logs
end. 
}
