{ %GRAPH }
{ %TARGET=go32v2,win32,linux }

{ Old file: tbs0051.pp }
{  Graph, shows a problem with putpixel                 OK 0.99.9 (PM) }

{$ifdef go32v2}
  {define has_colors_equal}
{$endif go32v2}

uses  crt,graph;

{$ifndef has_colors_equal}
  function ColorsEqual(c1, c2 : longint) : boolean;
    begin
       ColorsEqual:=((GetMaxColor=$FF) and ((c1 and $FF)=(c2 and $FF))) or
         ((GetMaxColor=$7FFF) and ((c1 and $F8F8F8)=(c2 and $F8F8F8))) or
         ((GetMaxColor=$FFFF) and ((c1 and $F8FCF8)=(c2 and $F8FCF8))) or
         ((GetMaxColor>$10000) and ((c1 and $FFFFFF)=(c2 and $FFFFFF)));
    end;

{$endif not has_colors_equal}

var   gd,gm,gError,yi,i : integer;
      col: longint;
      error : word;

BEGIN
  if paramcount=0 then
    gm:=$111   {640x480/64K  HiColor}
  else
    begin
       val(paramstr(1),gm,error);
       if error<>0 then
         gm:=$111;
    end;
  gd:=detect;

  InitGraph(gd,gm,'');
  gError := graphResult;
  IF gError <> grOk
  THEN begin
    writeln ('graphDriver=',gd,'  graphMode=',gm,
    #13#10'Graphics error: ',gError);
    halt(1);
  end;

  for i := 0 to 255
  do begin
    { new grpah unit used word type for colors }
    col := {i shl 16 + }(i) shl 8 + (i div 2);
    for yi := 0 to 20 do
      PutPixel (i,yi,col);
    SetColor (col);
    Line (i,22,i,42);
  end;

  for i:=0 to 255 do
   if not ColorsEqual(getpixel(i,15),getpixel(i,30)) then
     Halt(1);
  {readkey;}delay(1000);

  closegraph;
END.
