program TestPutP;

uses  crt,graph;

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

  for i:=0 to 255 do
   if not ColorsEqual(getpixel(i,15),getpixel(i,30)) then
     Halt(1); 
  {readkey;}delay(1000);

  closegraph;
END.
