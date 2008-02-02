// (c) 1999 Sebastian Guenther

{$MODE objfpc}
{$H-}

program GGI1;

uses GGI;

const

  WhiteColor: TGGIColor = (r: $ffff; g: $ffff; b: $ffff; a: 0);

  StarCount = 500;

  Frame: Integer = 0;

type

  TStar = record
    x, y, z: Integer;
  end;

var

  Visual: TGGIVisual;
  mode: TGGIMode;
  ScreenW, ScreenH: Integer;

  i, rx, ry: Integer;
  angle: Single;
  White: TGGIPixel;
  Stars: array[1..StarCount] of TStar;

begin

  if ggiInit <> 0 then
  begin
    WriteLn(StdErr, 'Initialization of GGI failed');
    Halt(2);
  end;


  Visual := ggiOpen(nil, []);   // Open default visual
  if not Assigned(Visual) then
  begin
    WriteLn(StdErr, 'Could not get default visual');
    Halt(3);
  end;

  ggiSetFlags(Visual, GGIFLAG_ASYNC);

  ggiParseMode({'S640x480[GT_8BIT]'}'', mode);
  ggiSetMode(Visual, mode);
  ggiGetMode(Visual, mode);
  ScreenW := mode.Virt.x;
  ScreenH := mode.Virt.y;

  WriteLn('Screen size: ', ScreenW, ' x ', ScreenH);

  White := ggiMapColor(Visual, WhiteColor);

  for i := 1 to StarCount do
  begin
    Stars[i].x := Random(ScreenW) - ScreenW div 2;
    Stars[i].y := Random(ScreenH) - ScreenH div 2;
    Stars[i].z := Random(99) + 1;
  end;

  angle := 0.0;

  while ggiKbhit(Visual) = 0 do
  begin

    ggiSetWriteFrame(Visual, Frame);
    ggiFillscreen(Visual);

    for i := 1 to StarCount do
    begin
      // the following is not as optimized as it could be...
      rx := Trunc(Sin(angle) * Stars[i].x + Cos(angle) * Stars[i].y) * 50 div Stars[i].z + (ScreenW div 2);
      ry := Trunc(Cos(angle) * Stars[i].x - Sin(angle) * Stars[i].y) * 50 div Stars[i].z + (ScreenH div 2);
      ggiPutPixel(Visual, rx, ry, White);
      if Stars[i].z = 1 then
        Stars[i].z := Random(99) + 1
      else
        Dec(Stars[i].z);
    end;
    angle := angle + 0.01;

    ggiFlush(Visual);
    Frame := (Frame + 1) mod mode.Frames;
    ggiSetDisplayFrame(Visual, Frame);
  end;

  ggiClose(Visual);

  ggiExit;

end.
