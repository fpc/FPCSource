{ %GRAPH }
{ Source provided for Free Pascal Bug Report 1050 }
{ Submitted by "Jonathan Ball" on  2000-07-17 }
{ e-mail: j.ball@rgu.ac.uk }
PROGRAM test;
USES Crt, Graph;
VAR
  bpoint        : pointer;
  bsize, actual : longint;
  f             : file;
  s             : string;
  i             : BYTE;

{------------------PROCEDURES-------------------}
PROCEDURE GraphInit;
VAR gd,gm : INTEGER;
BEGIN
  gd:=VGA; {gd:=DETECT;} gm:=VGAHi;
  InitGraph (gd,gm, '.\bgi');
  gd:=graphresult;
  IF gd<>grok THEN
  BEGIN
    WRITELN('Error initialising graphic card!');
    WRITELN(grapherrormsg(gd));HALT;
  END
END;

{---------------MAIN PROGRAM BODY----------------}
BEGIN
  GraphInit;
  i := 0;
  s := 'test';                    {set file name}
  REPEAT
    i := i + 1;                  {increment size}
    BSize := ImageSize(0,0,i,i); {buffer size}
    GETMEM(bpoint,bsize);        {reserve buffer}
    GetImage(0,0,i,i,bpoint^);   {store in buffer}
    writeln(i,' ',bsize);
    ASSIGN(f,s);
    REWRITE(f,1);
    BLOCKWRITE(f,bpoint^,bsize,actual);
    CLOSE(f);
    FREEMEM(bpoint,bsize);        {release memory}
  UNTIL (i=255){FALSE};                    {until error}
  CloseGraph;
END.
{OUTPUT: program runs OK until i=31 and   }
{bsize=2060 bytes. When i increments to 32}
{(bsize=2190), runtime error is generated }
