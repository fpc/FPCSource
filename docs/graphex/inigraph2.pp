Program inigraph2;

{ Program to demonstrate dynamic graphics mode selection }

uses graph;

const
  TheLine = 'We are now in 640 x 480 x 256 colors!'+
            ' (press <Return> to continue)';

var
  th,tw,gd, gm, lo, hi, error: integer;
  found: boolean;

begin
  { We want an 8 bit mode }
  gd := D8bit;
  { Get all available resolutions for this bitdepth }
  getmoderange(gd,lo,hi);
  { If the highest available mode number is -1,
    no resolutions are supported for this bitdepth  }
  if hi = -1 then
    begin
    writeln('no 8 bit modes supported!');
    halt
    end;
  found := false;
  { Search all resolutions for 640x480 }
  for gm := lo to hi do
    begin
    initgraph(gd,gm,'');
    { Make sure you always check graphresult! }
    error := graphResult;
    if (error = grOk) and
       (getmaxx = 639) and (getmaxy = 479) then
      begin
      found := true;
      break;
      end;
    end;
  if not found then
      CloseGraph();
    begin
    writeln('640x480x256 is not supported!');
    halt(1)
    end;
  { We are now in 640x480x256 }
  setColor(cyan);
  rectangle(0,0,getmaxx,getmaxy);
  { Write a nice message in the center of the screen }
  setTextStyle(defaultFont,horizDir,1);
  TW:=TextWidth(TheLine);
  TH:=TextHeight(TheLine);
  outTextXY((getMaxX - TW) div 2,
            (getMaxY - TH) div 2,TheLine);
  { Wait for return }
  readln;
  { Back to text mode }
  closegraph;
end.
