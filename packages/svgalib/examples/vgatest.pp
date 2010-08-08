{ From VGAlib, changed for svgalib }
{ partially copyrighted (C) 1993 by Hartmut Schirmer }
Program vgatest;

uses svgalib,strings;

Var
  line : array [0..2048 * 3-1] of byte;

Procedure testmode (Mode : Longint);

var
  xmax, ymax, i, x, y, yw, ys, c : longint;
  modeinfo : Pvga_modeinfo;

begin
   vga_setmode(mode);

   modeinfo := vga_getmodeinfo(mode);

   Writeln ('Width: ',modeinfo^.width,' Height: ',modeinfo^.height,
     ' Colors: ',modeinfo^.colors);
   Write ('DisplayStartRange: ',modeinfo^.startaddressrange,
          ' Maxpixels: ',modeinfo^.maxpixels,' Blit: ');
   if modeinfo^.haveblit<>0 then
      writeln ('YES') else writeln ('NO');

   vga_screenoff();

   xmax := vga_getxdim - 1;
   ymax := vga_getydim - 1;

   vga_setcolor(vga_white);
   vga_drawline(0, 0, xmax, 0);
   vga_drawline(xmax, 0, xmax, ymax);
   vga_drawline(xmax, ymax, 0, ymax);
   vga_drawline(0, ymax, 0, 0);

   for i := 0 to 25 do
       begin
       vga_setegacolor(i);
       vga_drawline(10 + i * 5, 10, 90 + i * 5, 90);
       end;
   for i := 0 to 25 do
       begin
       vga_setegacolor(i);
       vga_drawline(90 + i * 5, 10, 10 + i * 5, 90);
       end;

   vga_screenon();

   ys := 100;
   yw := (ymax - 100) div 4;
   Case vga_getcolors of
     256:
     begin
     for i := 0 to 60 do
       begin
       c := (i * 64) div 60;
       vga_setpalette(i + 16, c, c, c);
       vga_setpalette(i + 16 + 60, c, 0, 0);
       vga_setpalette(i + 16 + (2 * 60), 0, c, 0);
       vga_setpalette(i + 16 + (3 * 60), 0, 0, c);
       end;
     line[0] := 15;
     line[xmax] := 15;
     line[1] := 0;
     line[xmax - 1] := 0;
     for x := 2 to  xmax - 1 do
         line[x] := (((x - 2) * 60) div (xmax - 3)) + 16;
     for y := ys to (ys + yw-1) do        { gray }
         vga_drawscanline(y, line);
     for x := 2 to xmax - 1 do
         inc(line[x],60);
     inc(ys,yw);
     for y := ys to ys + yw-1 do
         vga_drawscanline(y, line);
     for x := 2 to  xmax - 1 do
         inc(line[x],60);
     inc(ys,yw);
     for y := ys to ys + yw-1 do
         vga_drawscanline(y, line);
     for x := 2 to xmax - 1 do
         inc(line[x],60);
     inc(ys,yw);
     for y := ys to ys + yw-1 do
         vga_drawscanline(y, line);
     end;

   1 shl 15,
   1 shl 16,
   1 shl 24:
     begin
       for x := 2 to xmax - 1 do
         begin
         c := ((x - 2) * 256) div (xmax - 3);
         y := ys;
         vga_setrgbcolor(c, c, c);
         vga_drawline(x, y, x, y + yw - 1);
         inc (y,yw);
         vga_setrgbcolor(c, 0, 0);
         vga_drawline(x, y, x, y + yw - 1);
         inc(y, yw);
         vga_setrgbcolor(0, c, 0);
         vga_drawline(x, y, x, y + yw - 1);
         inc (y,yw);
         vga_setrgbcolor(0, 0, c);
         vga_drawline(x, y, x, y + yw - 1);
         end;

       for x := 0 to 63 do
         for y := 0 to 63 do
             begin
             vga_setrgbcolor(x * 4 + 3, y * 4 + 3, 0);
             vga_drawpixel(xmax div 2 - 160 + x, y + ymax div 2 - 80);
             vga_setrgbcolor(x * 4 + 3, 0, y * 4 + 3);
             vga_drawpixel(xmax div 2 - 32 + x, y + ymax div 2 - 80);
             vga_setrgbcolor(0, x * 4 + 3, y * 4 + 3);
             vga_drawpixel(xmax div 2 + 160 - 64 + x, y + ymax div 2 - 80);

             vga_setrgbcolor(x * 4 + 3, y * 4 + 3, 255);
             vga_drawpixel(xmax div 2 - 160 + x, y + ymax div 2 + 16);
             vga_setrgbcolor(x * 4 + 3, 255, y * 4 + 3);
             vga_drawpixel(xmax div 2 - 32 + x, y + ymax div 2 + 16);
             vga_setrgbcolor(255, x * 4 + 3, y * 4 + 3);
             vga_drawpixel(xmax div 2 + 160 - 64 + x, y + ymax div 2 + 16);
             end;
     end;
   else
     begin
     if (vga_getcolors = 16) then
       begin
         for i := 0 to xmax - 2 do
             line[i] := (i + 2) mod 16;
         line[0] := 15;
         line[xmax] := 15;
         line[1] := 0;
         line[xmax - 1] := 0;
       end;
     if (vga_getcolors = 2) then
         begin
         for i := 0 to xmax do
             line[i] := $11;
         line[0] := $91;
         end;
     for i := 100 to ymax - 1 do
         vga_drawscanline(i, line);
     end;
   end;
   readln;
   vga_setmode(GTEXT);
end;

Var

    i,highest,mode : longint;
    info : pvga_modeinfo;
    expl : pchar;

Const  cols : pchar = NiL;

begin
    getmem (expl,100);
    vga_init();                        { Initialize. }

    mode := vga_getdefaultmode();

    if (mode=-1) then
      begin
        Writeln ('Choose one of the following video modes:');

        highest := 0;
        for i := 1 to GLASTMODE do
          if vga_hasmode(i) then
            begin
            expl[0]:=#0;
            info := vga_getmodeinfo(i);
            Case info^.colors of
            2:  begin
                cols := '2';
                strcopy(expl, '1 bitplane, monochrome');
                end;
            16: begin
                cols := '16';
                strcopy(expl, '4 bitplanes');
                end;
            256: begin
                if i=G320x200x256 then
                    strcopy(pchar(@expl[0]), 'packed-pixel')
                else if (i=G320x240x256) or
                        ((i=G320x400x256) or
                         (i=G360x480x256)) then
                    strcopy(expl, 'Mode X')
                else
                    strcopy(expl,'packed-pixel,banked');
                end;
            1 shl 15:
                begin
                cols := '32K';
                strcopy(expl, '5-5-5 RGB,blue at LSB, banked');
                end;
            1 shl 16:
                begin
                cols := '64K';
                strcopy(expl, '5-6-5 RGB,blue at LSB,banked');
                end;
            1 shl 24:
                begin
                cols := '16M';
                if info^.bytesperpixel = 3 then
                   if (info^.flags and RGB_MISORDERED)=0 then
                        strcopy(expl, '8-8-8 BGR, red byte first, banked')
                    else
                        strcopy(expl, '8-8-8 RGB, blue byte first, banked')
                else if (info^.flags and RGB_MISORDERED)=0 then
                    strcopy(expl, '8-8-8 RGBX, 32-bit pixels, X byte first, banked')
                else
                    strcopy(expl, '8-8-8 XRGB, 32-bit pixels, blue byte first, banked')
                end;
            end;
            if (info^.flags and IS_INTERLACED)=0 then
               begin
               if (expl[0] <> #0) then
                    strcat(expl, ',');
                strcat(expl, 'interlaced');
               end;
            if (info^.flags and IS_DYNAMICMODE)=0 then
                begin
                if (expl[0] <> #0) then
                    strcat(expl, ',');
                strcat(expl, 'dynamically loaded');
                end;
            highest := i;
            Write (i,': ', info^.width,'x', info^.height,' ');
            if (cols = NiL) then
                write (info^.colors)
            else
                write (cols);
            write (' colors ');
            if (expl[0]<>#0) then
                write('(', expl,')');
            writeln;
            end;
        Write ('Enter mode number (1-', highest,') : ');
        Readln (mode);

        if (mode < 1) or (mode > GLASTMODE) then
            begin
            Writeln ('Error: Mode number out of range');
            halt(1);
            end;
       end;
    if (vga_hasmode(mode)) then
        testmode(mode)
    else
        begin
        Writeln ('Error: Video mode not supported by driver.');
        Halt(1);
        end;
end.
