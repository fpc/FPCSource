program TestPutP;

uses  crt,graph;

var   gd,gm,gError,yi,i : integer;
      col: longint;
BEGIN
  gm:=$111;   {640x480/64K  HiColor}
  gd:=$FF;

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
    col := i shl 16 + (i div 2) shl 8 + (i div 3);
    for yi := 0 to 20 do
      PutPixel (i,yi,col);
    SetColor (col);
    Line (i,22,i,42);
  end;
  readkey;

  closegraph;
END.
