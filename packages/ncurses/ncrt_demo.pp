Program ncrt_demo;
{---------------------------------------------------------------------------
                                 CncWare
                           (c) Copyright 1999
 ---------------------------------------------------------------------------
  Filename..: ncrt_demo.pp
  Programmer: Ken J. Wright
  Date......: 11/22/99

  Purpose - Demonstrate the use of nCrt.

-------------------------------<< REVISIONS >>--------------------------------
  Ver  |   Date   | Prog | Description
-------+----------+------+----------------------------------------------------
  1.00 | 11/22/99 | kjw  | Initial Release.
------------------------------------------------------------------------------
}
uses ncrt;
var
   win,win1,
   stdscr : pwin;
   s : string;
   c : char;
   i,x,y : integer;
Begin
   {---------------------------------------
     Initialize ncurses screen & keyboard.
        **** This MUST be called ****
    ---------------------------------------}
   if Not StartCurses(stdscr) then Begin
      writeln('ncurses failed to initialize');
      halt;
   End;
   nClrScr(stdscr,7);
   nDrawBox(btSingle,1,1,80,3,31);
   FWrite(27,2,30,0,'nCrt Demomstration Program');
   nNewWindow(win1,9,9,71,16);
   nWinColor(win1,95);
   nWriteScr(win1,3,2,95,'This is a background window.');
   nWriteScr(win1,10,3,95,'It was built first, then displayed later.');
   FWrite(1,24,15,80,'Enter some text, press [Enter]');
   nWindow(win,10,10,70,15);
   nClrScr(win,31);
   nGotoXY(win,1,1);
   nEcho(true);
   s := nReadln(win);
   FWrite(1,24,15,80,'Enter some more text, press [Enter]');
   nGotoXY(win,nWhereX(win),nWhereY(win));
   s := nReadln(win);
   FWrite(1,24,79,80,'Please wait...');
   nGotoXY(win,1,1);
   Delay(500);
   nDelLine(win);
   Delay(500);
   nInsLine(win);
   Delay(500);
   nFrame(win1);
   nRefresh(win1);
   Delay(4000);
   nRefresh(win);
   Delay(2000);
   ClrScr;
   FWrite(1,24,14,80,'Enter even more text, press [Enter]');
   s := nReadln(stdscr);
   nClrScr(win,47);
   FWrite(1,24,11,80,'Press some keys, followed by [Esc]');
   nGotoXY(win,5,1);
   x := nWhereX(win);
   y := nWhereY(win);
   i := 0;
   nEcho(false);
   repeat
      c := nReadkey(win);
      DelLine;
      inc(i);
   until (c = #27) or (i >= 8);
   While i > 0 Do Begin
      InsLine;
      dec(i);
   End;
   str(x:0,s);
   nWrite(win,'x = '+s+', ');
   str(y:0,s);
   nWrite(win,'y = '+s);
   nWriteln(stdscr,'press a key...');
   readkey;
   nDrawBox(btSingle,11,11,69,14,63);
   FWrite(30,11,79,49,' nCrt Demo Program');
   nDelWindow(win);
   nDelWindow(win1);
   nWindow(win,2,2,79,24);
   nFrame(stdscr);
   nFrame(win);
   nDelWindow(win);
   { close ncurses & release all data structures }
   EndCurses;
End.
