program screen_demo;
{---------------------------------------------------------------------------
                                 CncWare
                           (c) Copyright 2000
 ---------------------------------------------------------------------------
  Filename..: screen_demo.pp
  Programmer: Ken J. Wright
  Date......: 08/24/2000

  Purpose - Demonstrate Linux screen saving/restoring with oCrt.

-------------------------------<< REVISIONS >>--------------------------------
  Ver  |   Date   | Prog| Description
-------+----------+-----+----------------------------------------------------
  1.00 | 08/24/00 | kjw | Initial Release.
------------------------------------------------------------------------------
}
uses ocrt;
var
   i,j : integer;
   pb : pnScreenBuf;
begin
   For i := 1 to 24 Do Begin
      TextColor(i);
      For j := 1 to 79 Do Write(chr(j+32));
      writeln;
   End;
   nGrabScreen(pb);
   Write('screen stored, press a key to clear');readkey;
   NormVideo;
   ClrScr;
   Write('press a key to restore previous screen');readkey;
   nPopScreen(pb);
   GotoXY(1,nMaxRows);
   Write('press a key to restore to a smaller window');readkey;
   ClrScr;
   Window(10,5,70,20);
   nPopScreen(pb);
   Window(1,1,nMaxCols,nMaxRows);
   GotoXY(1,nMaxRows);
   Write('press a key to offset stored screen');readkey;
   ClrScr;
   nPopScreen(pb,5,3);
   GotoXY(1,nMaxRows);
   Write('press a key to restore a portion of this screen in multiple ');readkey;
   nGrabScreen(pb,5,3,8,10);
   ClrScr;
   For i := 0 to 7 Do For j := 0 to 1 Do
      nPopScreen(pb,i*10+1,j*12+1);
   GotoXY(1,nMaxRows);
   { make sure to clean up! }
   nReleaseScreen(pb);
end.
