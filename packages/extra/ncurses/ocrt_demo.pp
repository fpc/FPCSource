Program ocrt_demo;
{---------------------------------------------------------------------------
                                 CncWare
                         (c) Copyright 1999-2000
 ---------------------------------------------------------------------------
  Filename..: ocrt_demo.pp
  Programmer: Ken J. Wright
  Date......: 11/22/99

  Purpose - Demonstrate the use of nCrt.

-------------------------------<< REVISIONS >>--------------------------------
  Ver  |   Date   | Prog| Description
-------+----------+-----+----------------------------------------------------
  1.00 | 11/22/99 | kjw | Initial Release.
  1.01 | 12/10/99 | kjw | Added OOP stuff.
  1.02 | 12/13/99 | kjw | 1) Changed from nCrt to oCrt.
                        | 2) Renamed from ncrt_demo to ocrt_demo.
                        | 3) Added some standard crt code at beginning.
  1.03 | 01/06/00 | kjw | Some minor changes for ncrt mods.
  1.04 | 06/27/00 | kjw | Changes for ncrt mods.
------------------------------------------------------------------------------
}
uses oCrt;
var
   win,win1,
   stdscr : pwin;
   s : string;
   c : char;
   i,j,k,x,y : integer;

var
   win11,win22 : pnWindow;
   win33,msgbox : TnWindow;

Begin
   { some nCrt standard in/out stuff, like crt }
   TextColor(15);
   TextBackground(1);
   TextAttr := TextAttr + blink;
   ClrScr;
   GotoXY(2,35);
   Writeln(1.0:0:4,' This should be blinking text');
   Window(10,10,70,15);
   TextAttr := TextAttr - blink;
   TextBackground(2);
   ClrScr;
   s := ' : ';
   for i := 1 to 6 do
   writeln(i:0,s,'No blinking here');
   writeln('Press Enter');
   readln(s);
   TextBackground(3);
   Write('input a number [i]: ');
   Readln(i);
   Write('input two numbers [j k]: ');
   Readln(j,k);
   Window(20,11,60,16);
   TextBackground(0);
   TextColor(15);
   ClrScr;
   writeln('i: ',i);
   writeln('j: ',j);
   writeln('k: ',k);
   Write('Press a key: ');
   readkey;
   TextMode(LastMode);
   write('Press a key: ');
   repeat until keypressed;
   while keypressed do readkey;

   { now some oCrt basics }
   stdscr := nscreen;
   nClrScr(stdscr,7);
   nDrawBox(stdscr,btSingle,1,1,80,3,31);
   nFWrite(27,2,30,0,'nCrt Demonstration Program');
   nNewWindow(win1,9,9,71,16);
   nClrScr(win1,95);
   nWriteScr(win1,3,2,95,'This is a background window.');
   nWriteScr(win1,10,3,95,'It was built first, then displayed later.');
   nFWrite(stdscr,1,24,15,80,'Enter some text, press [Enter]');
   nWindow(win,10,10,70,15);
   nClrScr(win,31);
   nGotoXY(win,1,1);
   s := nReadln(win);
   If s <> 'oop' Then Begin { skip right to OOP section? }
      nFWrite(stdscr,1,24,15,80,'Enter some more text, press [Enter]');
      nGotoXY(win,nWhereX(win),nWhereY(win));
      s := nReadln(win);
      nFWrite(stdscr,1,24,79,80,'Please wait...');
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
      { force nCrt to use full screen }
      nSetActiveWin(stdscr);
      ClrScr;
      nFWrite(1,24,14,80,'Enter even more text, press [Enter]');
      s := nReadln(stdscr);
      nClrScr(win,47);
      nFWrite(1,24,11,80,'Press some keys, followed by [Esc]');
      nGotoXY(win,5,1);
      x := nWhereX(win);
      y := nWhereY(win);
      i := 0;
      { turn off oCrt keyboard echo }
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
      { turn on oCrt keyboard echo }
      nEcho(true);
      str(x:0,s);
      nWrite(win,'x = '+s+', ');
      str(y:0,s);
      nWrite(win,'y = '+s);
      nWriteln(stdscr,'press a key...');
      readkey;
      nDrawBox(stdscr,btSingle,11,11,69,14,63);
      nFWrite(30,11,79,49,' nCrt Demo Program');
      nDelWindow(win);
      nDelWindow(win1);
      nWindow(win,2,2,79,24);
      nFrame(stdscr);
      nFrame(win);
      nDelWindow(win);
   End;
   { and now for some object oCrt }
   win := nscreen;
   New(win11,Init(1,1,nStdScr.Cols,nStdScr.Rows,31,true,30));
   win11^.PutHeader(' Now for some OOP with nCrt! ',79,center);
   win11^.DrawBox(1,1,1,78,3,62);
   New(win22,Init(20,7,60,17,47,false,0));
   win33.Init(30,15,50,20,79,true,78);
   win33.PutHeader(' Little Window ',15,right);
   Writeln('And here is window #3');
   win11^.Show;
   GotoXY(2,2);
   Write('Please press a key...');
   ReadKey;
   msgbox.init(25,11,55,13,47,true,47);
   s := 'Please enter a string';
   msgbox.FWrite((msgbox.cols-length(s)) div 2,1,46,0,s);
   msgbox.Show;
   win11^.Active;
   GotoXY(1,10);
   msgbox.Show;
   win11^.Active;
   Readln(s);
   msgbox.Hide;
   win22^.Show;
   Writeln(s);
   Delay(2000);
   win11^.Hide;
   win22^.Active;
   Writeln('Hiding window 1...');
   Delay(2000);
   win33.Show;
   Delay(2000);
   win11^.Show;
   Writeln('Showing window 1');
   win22^.Show;
   Writeln('Showing window 2');
   win33.Show;
   Write('Showing window 3');
   nKeypressed(2000);
   While Keypressed Do Readkey;
   win11^.Hide;
   win33.Active;
   Write('Hiding window 1');
   win22^.PutFrame(62);
   win22^.PutHeader(' New frame color ',63,center);
   win22^.Show;
   win33.Show;
   nKeypressed(3000);
   While Keypressed Do Readkey;
   win22^.Hide;
   win33.Active;
   Write('Hiding window 2');
   nKeypressed(2000);
   While Keypressed Do Readkey;
   win33.SetColor(47);
   nKeypressed(2000);
   While Keypressed Do Readkey;
   x := 30;
   y := 15;
   win33.ClrScr;
   for i := 1 to 11 do Begin
      TextAttr := win33.GetColor;
      dec(x);
      dec(y);
      str(i:0,s);
      win33.Move(x,y);
      Writeln('Moved by '+s);
      nFWrite(stdscr,1,nStdScr.Rows,63,80,'Moved by '+s);
      Delay(250);
   End;
   win33.Align(center,none);
   win33.PutHeader('Left Header',14,left);
   win33.Show;
   Delay(1000);
   win33.PutHeader('Right Header',14,right);
   win33.Show;
   Delay(1000);
   win33.PutHeader('Center Header',15,center);
   win33.Show;
   Delay(2000);
   Dispose(win11,Done);
   Dispose(win22,Done);
   win33.Done;
   msgbox.Done;
   NormVideo;
   ClrScr;
End.
