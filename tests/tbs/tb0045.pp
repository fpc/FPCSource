{ %GRAPH }

{ Old file: tbs0051.pp }
{  Graph, shows a problem with putpixel                 OK 0.99.9 (PM) }

{$ifdef go32v2}
  {define has_colors_equal}
{$endif go32v2}

{$ifdef go32v2}
{$define OK}
{$endif}
{$ifdef Unix}
{$define OK}
{$endif}

{$ifdef OK}
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

{$endif OK}
BEGIN
{$ifdef OK}
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
{$endif OK}
END.

{
  $Log$
  Revision 1.2  2002-06-01 19:08:52  marco
   * Renamefest

  Revision 1.1  2000/11/30 22:38:17  peter
    * renamed test suite

  Revision 1.1  2000/11/29 23:14:15  peter
    * new testsuite setup

  Revision 1.1  2000/07/13 09:21:54  michael
  + Initial import

  Revision 1.2  2000/04/14 05:44:22  pierre
   * adapted to new graph unit

  Revision 1.1  1999/12/02 17:37:38  peter
    * moved *.pp into subdirs
    * fpcmaked

  Revision 1.5  1999/11/28 12:17:14  jonas
    * changed the requested graphdriver from $FF to VESA (= 10), so the
      test program works again with the new graph unit
    * undefined has_colors_equal for go32v2, because it is not anymore
      in the new graph unit


}
