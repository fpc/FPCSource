Program GetModeRange_Example;

{ This program demonstrates how to find all available graph modes }

uses graph;


const
  { Currently, only 4, 8, 15 and 16 bit modes are supported
    but this may  change in the future }
  gdnames: array[D4bit..D16bit] of string[6] =
    ('4 bit','6 bit','8 bit','12 bit','15 bit','16 bit');

procedure WriteRes(const depth : integer);
var
	tw, th : integer;
	v, text : String;
begin
  text := 'Current resolution is '; str(getmaxx+1, v);
  text := text + v + 'x'; str(getmaxy+1, v);
  text := text + v + 'x' + gdnames[depth];
  setTextStyle(defaultFont,horizDir,1);
  TW:=TextWidth(text);
  TH:=TextHeight(text);
  outTextXY((getMaxX - TW) div 2,
            (getMaxY - TH) div 2,text);
end;

var
  t: text;
  line : string;
  gd, c, low, high, res: integer;
begin
  assign(t,'modes.txt');
  rewrite(t);
  close(t);
  for gd := D4bit to D16bit do
    begin
    { Get the available mode numbers for this driver }
    getModeRange(gd,low,high);
    append(t);
    write(t,gdnames[gd]);
    Writeln(t,': low modenr = ',low,', high modenr = ',high);
    close(t);
    { If high is -1,
       no resolutions are supported for this bitdepth }
    if high = -1 then
      begin
      append(t);
      writeln(t,'  No modes supported!');
      writeln(t);
      close(t);
      end
    else
      { Enter all supported resolutions for this bitdepth
        and write their characteristics to the file }
      for c := low to high do
        begin
        append(t);
        writeln(t,'  testing mode nr ',c);
        close(t);
        initgraph(gd,c,'');
        res := graphresult;
        append(t);
        { An error occurred when entering the mode? }
        if res <> grok then
          writeln(t,grapherrormsg(res))
        else
          begin
          write(t,'maxx: ',getmaxx,', maxy: ',getmaxy);
          Writeln(t,', maxcolor: ',getmaxcolor);
          closegraph;
          end;
        writeln(t);
          WriteRes(gd);
        close(t);
        end;
    append(t);
    writeln(t);
    close(t);
    end;
  Writeln('All supported modes are listed in modes.txt files');
end.
