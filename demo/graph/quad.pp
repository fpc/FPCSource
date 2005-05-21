PROGRAM Quad;
{A demo which loads some graphics etc. Nice. Don't forget to distribute
quaddata.inc!

The sources for this game was found on a site that claims to only have
PD stuff with the below header(which was only reindented), and the webmaster
said that everything he published was sent to him with that purpose. We tried
to contact the authors mentioned below via mail over internet, but that
failed. If there is somebody that claims authorship of these programs,
please mail marco@freepascal.org, and the sources will be removed from our
websites.

------------------------------------------------------------------------

ORIGINAL Header:

Programmed by: Justin Pierce
Graphics by: Whitney Pierce
Inspired by: Jos Dickman''s triple memory!
-----

Old version requires egavga.bgi. FPC doesn't require BGI's (VGA and VESA
support are built in the Graph, others are ignored).}

{$Define UseGraphics}

{$ifdef UseGraphics}
 {$ifdef Win32}
   {$define Win32Graph}         // Needed for GameUnit.
   {$APPTYPE GUI}
 {$endif}
{$endif}

Uses
 {$ifdef Win32}
  WinCrt,Windows,
 {$else}
  Crt,
 {$endif}
  Dos,Graph,GameUnit;         {Supplied with FPC demoes package. Wrapper for
                          mousesupport (via msmouse or api), and contains
                          highscore routines}

Const nox             = 10;
      noy             = 8;
      card_border     = red;
      PicBufferSize   = 64000;  {Buffersize for deRLE'ed picture data}
      ComprBufferSize = 20000;  {Buffer for diskread- RLE'ed data}
      PicsFilename    = 'quaddata.dat';  {Name of picturesfile}
      ScoreFileName   = 'quad.scr';
      {$IFDEF UseGraphics}
       DisplGrX=110;
       DisplGrY=90;
       DisplGrScale=16;
       HelpY=130;
      {$ENDIF}


Type
    pByte           = ^Byte;                  {BufferTypes}
    Card            = Record
                       exposed: boolean;
                       pic: byte;
                      End;

            {Assigns an enumeration to each picture}
    PictureEnum= (zero,one,two,three,four,five,six,seven,eight,nine,colon,
                  back,score,exit_b,score_b,chunk,p1,p2,p3,p4,p5,p6,p7,p8,
                  p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20);

            {A pictures definition;
              x and y dimensions and offset in PicData buffer}

    Picture = packed Record
                start: longint;
                x,y: byte;
                End;

    {All pictures. This array, and the data in PicData is all pic info.}
    PictureArray= ARRAY[zero..p20] OF Picture;

    selected = Record
                 x,y: byte;
                 pic: byte;
                End;
    time_record = Record
                    o_hr,o_min,o_sec,o_sec100: word;
                    hr,min,sec,sec100: word;
                    a_sec,a_min: word;
    End;

Var b           : array[1..nox,1..noy] Of card;
    Pics        : PictureArray;
    PicData     : PByte;
    s           : array[1..4] Of selected;
    os          : byte;
    turns       : integer;
    off,ok,exit1: boolean;
    opened      : byte;
    bgidirec    : string;
    time        : time_record;


{
Procedure fatal(fcall:String);
Begin
  textmode(CO80);
  clrscr;
  Writeln('A fatal error has occured');
  Writeln('Error: ',fcall);
  Writeln;
  Write('Hit enter to halt program--');
  readln;
  halt;
End;
}

Procedure ginit640x480x16(direc:String);

Var grd,grmode: integer;
Begin
  {$ifdef Win32}
   {$ifndef Debug}
     ShowWindow(GetActiveWindow,0);
   {$endif}
  grmode:=vgaHI;
  grd:=vga;
  Direc:='';
  {$else}
  closegraph;
  grd := 9;{ detect;}
  grmode := 2;{ m800x600x16;}
  {$endif}
  initgraph(grd,grmode,direc);
  {$ifndef Win32}
  setgraphmode(2);
  {$endif}
End;

procedure WaitForMouse;

var    ms_mx,ms_my,ms_but : LONGINT;

begin
  Repeat
    GetMouseState(ms_mx,ms_my,ms_but);
   Until ms_but=0;
  Repeat
    GetMouseState(ms_mx,ms_my,ms_but);
   Until ms_but<>0;
end;


Procedure clean_board;

Var x,y: byte;
Begin
  y := 1;
  Repeat
    x := 1;
    Repeat
      b[x,y].pic := 0;
      b[x,y].exposed := false;
      inc(x);
    Until x>nox;
    inc(y);
  Until y>noy
End;

Procedure showpic(xp,yp:integer; tp:pictureenum);

Var x,y,x1,y1: byte;
    tx: integer;
Begin
  x := pics[tp].x; {mb[tp.start];}
  y := pics[tp].y; {mb[tp.start+1];}
  y1 := 1;
  tx := 0;
  Repeat
    x1 := 1;
    Repeat
      putpixel(xp+(x1-1),yp+(y1-1),picdata[pics[tp].start-1+tx]);
      inc(x1);
      inc(tx);
    Until x1>x;
    inc(y1);
  Until y1>y;
End;

Procedure NumberOutput(X,Y,Number:LONGINT;RightY:BOOLEAN);

Var num: string;
    plc: byte;

Begin
  str(number,num);
  If length(num)=1 Then
   insert('0',num,0);
  IF RightY THEN
   dec (x,length(num)*11);
  plc := 1;
  Repeat
   IF (Num[plc]>CHR(47)) AND (Num[plc]<CHR(58)) THEN
    showpic(((plc-1)*11)+X,Y,pictureenum(ORD(Zero)+ORD(Num[plc])-48));
   inc(plc);
  Until plc>length(num);
End;

Procedure update_secs;

Begin
 showpic(605,453,colon);
 NumberOutput(615,453,time.a_sec,FALSE);
End;

Procedure showturn(x,y:integer);

Begin
  hidemouse;
  If (x=0) And (y=0) Then
   NumberOutput(4,453,Turns,FALSE)
  ELSE
   NumberOutput(x,y,Turns,FALSE);
  showmouse;
End;

Procedure get_original_time;
Begin
  With time Do
    Begin
      a_sec := 0;
      a_min := 0;
      gettime(o_hr,o_min,o_sec,o_sec100);
      gettime(hr,min,sec,sec100);
    End;
End;

Procedure update_time(ForcedUpdate:BOOLEAN);
Begin
  With time Do
    Begin
      gettime(hr,min,sec,sec100);

      If sec<>o_sec Then
        Begin
          inc(a_sec);
          If a_sec<=60 Then update_secs;
        End;
      If a_sec>60 Then
        Begin
          a_sec := 0;
          inc(a_min);
          ForcedUpdate:=TRUE;
        End;
      IF ForcedUpdate THEN
       BEGIN
        Update_secs;
        showpic(606,453,colon);
        NumberOutput(606,453,time.a_min,TRUE);
       END;
      o_hr := hr;
      o_min := min;
      o_sec := sec;
      o_sec100 := sec;
    End;
End;


Procedure makecard(x,y:byte);

Var xp,yp: integer;
Begin
  hidemouse;
  xp := ((x-1)*63);
  yp := ((y-1)*56);
  setcolor(card_border);
  setfillstyle(1,0);
  bar(xp+1,yp+1,xp+62,yp+55);
  rectangle(xp,yp,xp+63,yp+56);
  If b[x,y].exposed=false Then
    Begin
      showpic(xp+1,yp+1,back);
    End;
  showmouse;
  If b[x,y].exposed=true Then
    Begin
      hidemouse;
      showpic(xp+7,yp+4,pictureenum(ORD(b[x,y].pic)+ORD(p1)-1));
      showmouse;
    End;
End;

Function used(pic:byte): byte;

Var cx,cy,u: byte;
Begin
  used := 0;
  u := 0;
  cy := 1;
  Repeat
    cx := 1;
    Repeat
      If b[cx,cy].pic=pic Then inc(u);
      inc(cx);
    Until cx>nox;
    inc(cy);
  Until cy>noy;
  used := u;
End;

Procedure set_board;

CONST Outstr=#219+#219+#219+#219+#219+#219+#219+#219+#219+#219+#219+#219+#219+
             #219+#219+#219+#219+#219+#219+#219+#219+#219+#219+#219+#219+
             #219+#219+#219+#219;

Var cx,cy,pic: byte;
Begin
  setcolor(0);
  outtextxy(0,470,OutStr);
  setcolor(green);
  outtextxy(0,470,'Dealing board, please wait...');
  Delay(1000);
  cy := 1;
  Repeat
    cx := 1;
    Repeat
      Repeat
        pic := random(20)+1;
      Until used(pic)<4;
      b[cx,cy].pic := pic;
      makecard(cx,cy);
      inc(cx);
    Until cx>nox;
    inc(cy);
  Until cy>noy;
  setcolor(0);
  outtextxy(0,470,OutStr);
End;

Procedure fire_works;

Const
  nof = 30;

Type
  fires = Record
            x,y: Longint;
            direct: longint;
            speed: Longint;
            explode: boolean;
            color: byte;
            oex: longint;
End;

Var fire: array[1..nof] Of fires;

Procedure clean_fires;

Var c: longint;
Begin
  c := 1;
  Repeat
    fire[c].direct := random(2)+1;
    fire[c].color := random(15)+1;
    fire[c].x := random(639);
    fire[c].y := 479;
    fire[c].explode := false;
    fire[c].speed := random(20)+15;
    fire[c].oex := 1;
    inc(c);
  Until c>nof;
End;

Procedure inact;

Var c: longint;
Begin
  c := 1;
  Repeat
    If fire[c].explode=false Then
      Begin
        setcolor(fire[c].color);
        circle(fire[c].x,fire[c].y,1);
      End;

    If (fire[c].explode=true) And (fire[c].oex<10) Then
      Begin
        setcolor(fire[c].color);
        circle(fire[c].x,fire[c].y,fire[c].oex);
        setcolor(random(15)+1);
        circle(fire[c].x,fire[c].y,fire[c].oex-1);
      End;

    inc(c);
  Until c>nof;

  delay(75);
  {$ifndef Win32}
   gotoxy(1,1);
  {$endif}

  c := 1;
  Repeat
    setcolor(0);
    circle(fire[c].x,fire[c].y,1);

    If (fire[c].explode=true) And (fire[c].oex<10) Then
      Begin
        circle(fire[c].x,fire[c].y,fire[c].oex);
        circle(fire[c].x,fire[c].y,fire[c].oex-1);
        inc(fire[c].oex);
      End;

    If fire[c].explode=false Then
      Begin
        dec(fire[c].speed,1);
        dec(fire[c].y,fire[c].speed);
        If fire[c].direct=1 Then inc(fire[c].x,2);
        If fire[c].direct=2 Then dec(fire[c].x,2);
        If fire[c].speed<=(-1*LONGINT(random(11))) Then
         fire[c].explode := true;
      End;

    inc(c);
  Until c>nof;
  c := 1;
End;

Function exploded: boolean;

Var c: longint;
    m: boolean;
Begin
  c := 1;
  m := true;
  Repeat
    If fire[c].oex<6 Then m := false;
    inc(c);
  Until (c>nof);
  exploded := m;
End;

Begin
  cleardevice;
  Repeat
    clean_fires;
    Repeat
      inact;
    Until (exploded=true) Or (keypressed);
  Until keypressed;
End;

{$ifdef Win32Graph}
Procedure ClearTextCoord(x1,y1,x2,y2:Longint);

Begin
 SetFillStyle(SolidFill,0);            {Clear part of playfield}
 Bar ((x1+1) * DisplGrScale, (Y1+1)*DisplGrScale,(x2+1) * DisplGrScale, (Y2+1)*DisplGrScale);
End;
{$endif}

Procedure win;

Var m,s: string;
    I,J   : LONGINT;

Const GameOver='Game Over, turns needed = ';

Begin
  hidemouse;
  // fire_works;
  cleardevice;

  {$ifndef Win32}
  closegraph;
  textmode(co80+font8x8);
  clrscr;
  {$endif}
  I:=SlipInScore(Turns);
  {$ifndef Win32}
  GotoXY(1,23);
  Writeln(GameOver,Turns);
  FOR J:=9 TO 22 DO
   BEGIN
    GotoXY(20,J);
    Write(' ':38);
   END;
  {$else}
   SetColor(White);
   ClearTextCoord(20,9,58,22);
   Str(Turns,S);
   S:=GameOver+S;
   OutTextXY(5,40+9*LineDistY,S);
   OutTextXY(5,40+23*LineDistY,'Press mouse to continue');
   WaitForMouse;
  {$endif}
 IF I<>0 THEN
  BEGIN
   ShowHighScore;
{$IFDEF USEGRAPHICS}
   SetColor(Black);
   Bar(5,40+23*LineDisty,5+8*26,40+23*LineDisty+8);
   SetColor(White);
   OutTextXY(5,40+23*LineDistY,'Please enter your name');
   GrInputStr(S,300,HelpY+20+(17-I+1)*LineDistY,16,12,10,FALSE,AlfaBeta);
{$ELSE}
   InputStr(S,20,21-I,10,FALSE,AlfaBeta);
{$ENDIF}
   IF Length(S)<12 THEN
    BEGIN
     str(time.a_min,m);
     S:=S+'['+m+':';
     str(time.a_sec,m);
     S:=S+'m'+']';
    END;
   HighScore[I-1].Name:=S;
  END;
  ShowHighScore;
  {$ifdef Win32}
   Bar(5,40+23*LineDisty,5+8*26,40+23*LineDisty+8);
   OutTextXY(5,40+23*LineDistY,'Press mouse to continue');
   WaitForMouse;
  {$else}
  ginit640x480x16(bgidirec);
  {$endif}
  off := false;
  clean_board;
  set_board;
  turns := 0;
  showpic(0,450,score);
  showpic(80,450,score_b);
  showpic(150,450,exit_b);
  showpic(569,450,score);
  showturn(0,0);
  exit1 := false;
  get_original_time;
  update_time(True);
  SetMousePosition(0,0);
  showmouse;
End;

Procedure show_scores;

Var x,y,c: byte;
Begin
  hidemouse;

  y := 1;
  Repeat
    x := 1;
    showpic(x+135,(y-1)*21,score);
    showpic(x,(y-1)*21,score);
    showpic(x+204,(y-1)*21,score);
    Repeat
      showpic(((x-1)*10)+3,(y-1)*21,chunk);
      inc(x);
    Until x>20;
    inc(y);
  Until y>10;

  c := 0;
  Repeat
    If HighScore[c].name<>'' Then
      Begin
        setcolor(white);
        outtextxy(4,7+(c*21),HighScore[c].name);
        turns:= HighScore[c].Score;
        showturn(211,3+(c*21));
      End;
    inc(c);
  Until c>9;
  turns := 0;
  {$ifndef Win32}
  gotoxy(1,1);
  {$endif}
  readln;

  off := false;
  clean_board;
  set_board;
  turns := 0;
  showpic(0,450,score);
  showpic(80,450,score_b);
  showpic(150,450,exit_b);
  showpic(569,450,score);
  showturn(0,0);
  exit1 := false;
  get_original_time;
  update_time(True);
  SetMousePosition(0,0);
  showmouse;
End;

Procedure interpret;

Var mpx,mpy: byte;
    ms_mx,ms_my,ms_but : LONGINT;
Begin
  GetMouseState(ms_mx,ms_my,ms_but);
  ms_mx:=ms_mx shr 1;;

  If ms_but=0 Then off := false;

  If ((ms_but AND 1)=1) And (off=false) Then
    Begin
      off := true;
      mpx := ms_mx*2 Div 63;
      mpy := (ms_my) Div 56;

      If (ms_mx*2>=80) And (ms_mx*2<=129) And (ms_my>=450) And (ms_my<=466)
         And (ok=true) Then show_scores;
      If (ms_mx*2>=150) And (ms_mx*2<=199) And (ms_my>=450) And (ms_my<=466)
        Then
        Begin
          exit1 := true;
        End;

      inc(mpx);
      inc(mpy);
      If (b[mpx,mpy].exposed=false) And (mpx>=1) And (mpy>=1) And (mpx<=10) And (mpy<=8)
        Then
        Begin
          setfillstyle(1,0);
          bar(80,450,130,466);
          ok := false;
          b[mpx,mpy].exposed := true;
          makecard(mpx,mpy);
          inc(os);
          s[os].x := mpx;
          s[os].y := mpy;
          s[os].pic := b[mpx,mpy].pic;
        End;
    End;

  If os=4 Then
    Begin
      inc(turns);
      showturn(0,0);
      os := 0;
      delay(700);
      inc(opened);
      If Not((s[1].pic=s[2].pic) And (s[1].pic=s[3].pic) And (s[1].pic=s[4].pic)) Then
        Begin
          dec(opened);
          b[s[1].x,s[1].y].exposed := false;
          b[s[2].x,s[2].y].exposed := false;
          b[s[3].x,s[3].y].exposed := false;
          b[s[4].x,s[4].y].exposed := false;
          makecard(s[1].x,s[1].y);
          makecard(s[2].x,s[2].y);
          makecard(s[3].x,s[3].y);
          makecard(s[4].x,s[4].y);
        End;
      If opened=20 Then win;
    End;

  If NOT ok Then
   update_time(FALSE);
End;

Procedure load_pics(PicBuf:PByte);
{loads picture structures from disc}

VAR  F           : File;
     Buf1Ind,
     I,J,K       : LONGINT;
     TData       : PByte;

Begin
  GetMem(TData,ComprBufferSize);        { allocate buffer}
  Assign(F,Picsfilename);             { Open file}
  {$I-}
  Reset(F,1);
  {$I+}
  If ioresult<>0 Then
   BEGIN
    {$ifdef Win32}
     MessageBox(GetActiveWindow,'Error','Fatal error, couldn''t find graphics data file quaddata.dat',WM_QUIT);
    {$else}
     TextMode(CO80);
     Writeln('Fatal error, couldn''t find graphics data file quaddata.dat');
    {$endif}
    HALT;
   END;

  {Read the array with picture information; (X,Y dimensions and offset in
          binary data)}
  BlockRead(F,pics,SIZEOF(Picture)*(ORD(p20)-ORD(zero)+1),I);

  {Read some slackspace which shouldn't be in the file ;-)}
  blockread(F,TData[0],6,Buf1ind);

  {Read the real, RLE'ed graphics data}
  BlockRead(F,TData[0],ComprBufferSize,Buf1Ind);
  Close(F);

  {Expand the RLE data. Of each byte, the high nibble is the count-1, low
    nibble is the value}

  I:=0; J:=0;
  REPEAT
   K:=(TData[I] SHR 4) +1;
   FillChar(PicBuf[J],K,TData [I] AND 15);
   INC(J,K);
   INC(I);
  UNTIL I>=Buf1Ind;

  {Release the temporary buffer (the compressed data isn't necesary anymore)}
  Freemem(TData,ComprBufferSize);
End;

Procedure clean;

VAR I : LONGINT;

Begin
  Randomize;                                    {Initialize random generator}
  Negative:=TRUE;                               {Higher highscore is worse}
  {$ifdef Win32}
   HighX     :=300;   {Coordinates of highscores}
   HighY     :=130+20+8*LineDistY;  {y coordinate relative to other options}
  {$else}
   HighX:=20;   HighY:=9;                        {coordinates for highscores}
  {$endif}

  GetMem(PicData,PicBufferSize);                {Allocate room for pictures}
  load_pics(PicData);                           {Load picture data from file}
  FOR I:=0 TO 9 DO                              {Create default scores}
  begin
   HighScore[I].Score:=-100*I;                  {Negative, because then the
                                                  "highest" score is best}
   If HighScore[I].Score>0 Then
    Writeln('ohoh');
   End;

  LoadHighScore(ScoreFileName);                 {Try to load highscore file}
//  closegraph;
  {$ifNdef FPC}
  bgidirec := 'd:\prog\bp\bgi';
  {$ENDIF}
  ginit640x480x16(bgidirec);
  setcolor(card_border);
  ok := true;
  opened := 0;
  os := 0;
  s[1].x := 0;
  s[2].x := 0;
  s[3].x := 0;
  off := false;
  clean_board;
  set_board;
  turns := 0;
  showpic(0,450,score);        showpic(80,450,score_b);
  showpic(150,450,exit_b);     showpic(569,450,score);
  showturn(0,0);
  exit1 := false;
  SetMousePosition(0,0);
  get_original_time;
  update_time(True);
  showmouse;
End;


Begin
  Initmouse;
  Negative:=True;
  clean;
  Repeat
    interpret;
  Until (exit1=true) {$ifdef Debug} or (turns=1) {$endif};
  {$ifndef Win32}
  closegraph;
  {$endif}

  Freemem(PicData,PicBufferSize);
  SaveHighScore;
  {$ifndef Win32}
   Textmode(co80);
   clrscr;
   HideMouse;
   Writeln('Thanks for playing Quadruple Memory');
   Writeln('Feel free to distribute this software.');
   Writeln;
   Writeln('Programmed by: Justin Pierce');
   Writeln('Graphics by: Whitney Pierce');
   Writeln('Inspired by: Jos Dickman''s triple memory!');
   Writeln('FPC conversion and cleanup by Marco van de Voort');
   Writeln;
   ShowMouse;
  {$else}
   SetbkColor(black);
   SetColor(White);
   SetViewPort(0,0,getmaxx,getmaxy,clipoff);
   ClearViewPort;
   SetTextStyle(0,Horizdir,2);
   SetTextStyle(0,Horizdir,1);
   OutTextXY(220,10,'QUAD');
   OutTextXY(5,40+1*LineDistY,'Thanks for playing Quadruple Memory');
   OutTextXY(5,40+2*LineDistY,'Feel free to distribute this software.');
   OutTextXY(5,40+4*LineDistY,'Programmed by: Justin Pierce');
   OutTextXY(5,40+5*LineDistY,'Graphics by: Whitney Pierce');
   OutTextXY(5,40+6*LineDistY,'Inspired by: Jos Dickman''s triple memory!');
   OutTextXY(5,40+7*LineDistY,'FPC conversion and cleanup by Marco van de Voort');
   OutTextXY(5,40+9*LineDistY,'Press mouse to continue');
   WaitForMouse;
  {$endif}
  {$ifdef Win32}
   CloseGraph;
  {$endif}
   DoneMouse;

End.
