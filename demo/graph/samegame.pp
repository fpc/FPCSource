{
    $Id: samegame.pp,v 1.10 2005/02/14 17:13:10 peter Exp $

    This program is both available in XTDFPC as in the FPC demoes.
    Copyright (C) 1999 by Marco van de Voort

    SameGame is a standard game in GNOME and KDE. I liked it, and I
    automatically brainstormed how I would implement it.
    It turned out to be really easy, and is basically only 100 lines or so,
    the rest is scorekeeping, helptext, menu etc.

    The game demonstrates some features of the MSMOUSE unit, and some of
    the Crt and Graph units. (depending whether it is compiled with
    UseGraphics or not)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
PROGRAM SameGame;


{$ifdef UseGraphics}
 {$ifdef Win32}
   {$define Win32Graph}
    {$apptype GUI}
 {$endif}
{$endif}


Uses
{$ifdef Win32}
  Windows,
{$endif}
{$ifdef Win32Graph}
  WinCrt,
 {$else}
  Crt,
{$endif}
  Dos,
{$IFDEF UseGraphics}
  Graph,
  {$INFO GRAPH}
{$ENDIF}
  GameUnit;

CONST
   {$IFDEF UseGraphics}
        GrFieldX                          = 10; {X topleft of playfield}
        GrFieldY                          = 70; {Y topleft of playfield}
        ScalerX                           = 22; {ScalerX x Scaler y dots
                                                  must be approx a square}
        ScalerY                           = 20;
   {$ENDIF}
        FieldX                            = 10; {Top left playfield
                                                 coordinates in squares(textmode)}
        FieldY                            =  3; {Top left playfield coordinates}
        PlayFieldXDimension               = 20; {Dimensions of playfield}
        PlayFieldYDimension               = 15;
   {$IFDEF UseGraphics}
        RowDispl                          = 15;
        MenuX                             = 480;
        MenuY                             = 120;
        grNewGameLine                     = 'NEW GAME';
        grHelpLine                        = 'HELP';
        grEndGame                         = 'END GAME';
   {$ENDIF}


       {Used colors. Colors[0..2] are the colors used on the playfield, Colors[3]
          is the background and Colors[4] is the color used to mark the pieces}
        Colors : ARRAY [0..4] OF LONGINT  = (White,Blue,Red,Black,LightMagenta);


TYPE PlayFieldType=ARRAY[0..PlayFieldXDimension-1,0..PlayFieldYDimension-1] OF BYTE;

{$IFDEF UseGraphics}
PROCEDURE DisplayPlayField(CONST PlayField:PlayFieldType);
{Screen routine, simply puts the array Playfield on screen.
Both used for displaying the normal grid as the grid with a certain area marked}

VAR X,Y : LONGINT;
    LastOne,
    NumbLast : LONGINT;

BEGIN
 HideMouse;
 FOR Y:=0 TO PlayFieldYDimension-1 DO
  BEGIN
   X:=0;
   REPEAT
    LastOne:=PlayField[X,Y];
    NumbLast:=X;
    WHILE (PlayField[X,Y]=LastOne) AND (X<(PlayFieldXDimension-1))DO
     INC(X);
    SetFillStyle(SolidFill,Colors[LastOne]);
    Bar(GrFieldX+NumbLast*ScalerX,GrFieldY+Y*ScalerY,GrFieldX+X*ScalerX-1,GrFieldY+(Y+1)*ScalerY-1);
   UNTIL X>=(PlayFieldXDimension-1);
  END;
 ShowMouse;
END;
{$ELSE}

PROCEDURE DisplayPlayField(CONST PlayField:PlayFieldType);
{Screen routine, simply puts the array Playfield on screen.
Both used for displaying the normal grid as the grid with a certain area marked}

VAR X,Y : LONGINT;

BEGIN
 FOR Y:=0 TO PlayFieldYDimension-1 DO
  BEGIN
   GotoXY(FieldX,Y+FieldY);
   FOR X:=0 TO PlayFieldXDimension-1 DO
    BEGIN
     TextColor(Colors[PlayField[X,Y]]);
     Write(#219#219);
    END;
   END;
END;
{$ENDIF}

PROCEDURE ShowHelp;
{Shows some explanation of the game and waits for a key}

{$ifndef UseGraphics}
VAR I : LONGINT;
{$endif}

BEGIN
 {$IFDEF UseGraphics}
  HideMouse;
  SetbkColor(black);
  SetViewPort(0,0,getmaxx,getmaxy,clipoff);
  ClearViewPort;
  SetTextStyle(0,Horizdir,2);
  OutTextXY(220,10,'SAMEGAME');
  SetTextStyle(0,Horizdir,1);
  OutTextXY(5,40+1*LineDistY,' is a small game, with a principle copied from some KDE game');
  OutTextXY(5,40+3*LineDistY,'I tried to implement it under Go32V2 to demonstrate the MsMouse unit');
  OutTextXY(5,40+4*LineDistY,'When it worked, I tried to get it running under Linux. I succeeded,');
  OutTextXY(5,40+5*LineDistY,'but the mouse unit of the API doesn'#39't work with GPM 1.17');
  OutTextXY(5,40+7*LineDistY,'If you move over the playfield, aggregates of one color will be marked');
  OutTextXY(5,40+8*LineDistY,'in purple. If you then press the left mouse button, that aggregate will');
  OutTextXY(5,40+9*LineDistY,'disappear, and the playfield will collapse to the bottom-left. Please');
  OutTextXY(5,40+10*LineDistY,'keep in mind that only an aggregate of two blocks or more will disappear.');
  OutTextXY(5,40+12*LineDistY,'For every aggregate you let disappear you get points, but the score is');
  OutTextXY(5,40+13*LineDistY,'quadratic proportional to the number of blocks killed. So 4 times killing');
  OutTextXY(5,40+14*LineDistY,' a 4 block aggregate scores 4 points, and one time 16 blocks will score 64');
  OutTextXY(5,40+15*LineDistY,'blocks. The purpose of the game is obtaining the highscore');
  OutTextXY(5,40+17*LineDistY,'If you manage to empty the entire playfield, you'#39'll get a bonus');
  OutTextXY(5,40+19*LineDistY,'Press any key to get back to the game');
  ShowMouse;
 {$ELSE}
  FOR I:=2 TO 24 DO
   BEGIN
    GotoXY(1,I);
    ClrEol;
   END;
  GotoXY(1,3); TextColor(White);
  Write('SAMEGAME');
  SetDefaultColor;
  WriteLn(' is a small game, with a principle copied from some KDE game');
  WriteLn;
  WriteLn('I tried to implement it under Go32V2 to demonstrate the MsMouse unit');
  Writeln('When it worked, I tried to get it running under Linux. I succeeded,');
  Writeln('but the mouse unit of the API doesn'#39't work with GPM 1.17');
  Writeln;
  WriteLn('If you move over the playfield, aggregates of one color will be marked');
  Writeln('in purple. If you then press the left mouse button, that aggregate will');
  Writeln('disappear, and the playfield will collapse to the bottom-left. Please');
  Writeln('keep in mind that only an aggregate of two blocks or more will disappear.');
  Writeln;
  Writeln('For every aggregate you let disappear you get points, but the score is');
  Writeln('quadratic proportional to the number of blocks killed. So 4 times killing');
  Writeln(' a 4 block aggregate scores 4 points, and one time 16 blocks will score 64');
  Writeln('blocks. The purpose of the game is obtaining the highscore');
  Writeln;
  Writeln('If you manage to empty the entire playfield, you'#39'll get a bonus');
  Writeln;
  WriteLn('Press any key to get back to the game');
 {$ENDIF}
  GetKey;
END;

VAR MarkField,PlayField : PlayFieldType; {The playfield, and the marked form}
    CubesMarked         : LONGINT;       {Cubes currently marked}
    Score               : LONGINT;       {The current score}
    LastScore           : LONGINT;

PROCEDURE ShowButtons;
{Shows the clickable buttons}

BEGIN
 {$IFNDEF UseGraphics}
 TextColor(Yellow); TextBackGround(Blue);
 GotoXY(60,5);   Write('NEW game');
 GotoXY(60,6);   Write('HELP');
 GotoXY(60,7);   Write('END game');
 {$IFDEF Unix}
  GotoXY(60,8);   Write('Force IBM charset');
 {$ENDIF}
  SetDefaultColor;
 {$ELSE}
 SetTextStyle(0,Horizdir,1);
 OutTextXY(MenuX,MenuY,grNewGameLine);
 OutTextXY(MenuX,MenuY+RowDispl,grHelpLine);
 OutTextXY(MenuX,MenuY+2*RowDispl,grEndGame);
  {$ENDIF}

END;

FUNCTION PlayFieldPiecesLeft:LONGINT;
{Counts pieces/cubes/blocks left on the playfield}

VAR I,J,K : LONGINT;

BEGIN
 K:=0;
 FOR I:=0 TO PlayFieldXDimension-1 DO
  FOR J:=0 TO PlayFieldYDimension-1 DO
   IF PlayField[I,J]<>3 THEN
    INC(K);
 PlayFieldPiecesLeft:=K;
END;

PROCEDURE ShowScore;
{Simply procedure to update the score}

{$IFDEF UseGraphics}
VAR S : String;
{$ENDIF}
BEGIN
 {$IFDEF UseGraphics}
  Str(Score:5,S);
  SetFillStyle(SolidFill,0);
  Bar(300,440,450,458);
  OutTextXY(300,440,'Score :'+S);
 {$ELSE}
 TextColor(White);
 GotoXY(20,23);   Write(' ':20);
 GotoXY(20,23);   Write('Score : ',Score);
 SetDefaultColor;
 {$ENDIF}
END;

FUNCTION CubesToScore : LONGINT;
{Function to calculate score from the number of cubes. Should have a higher
order than linear, or the purpose of the game disappears}

BEGIN
 CubesToScore:=(CubesMarked*CubesMarked) DIV 4;
END;

PROCEDURE MarkAfield(X,Y:LONGINT);
{Recursively marks the area adjacent to (X,Y);}

VAR TargetColor : LONGINT;

PROCEDURE MarkRecur(X1,Y1:LONGINT);
{Marks X1,Y1, checks if neighbours (horizontally or vertically) are the
same color}

BEGIN
 IF (PlayField[X1,Y1]=TargetColor) AND (MarkField[X1,Y1]<>4) THEN
  BEGIN
   MarkField[X1,Y1]:=4;
   INC(CubesMarked);
  IF X1>0 THEN
   MarkRecur(X1-1,Y1);
  IF Y1>0 THEN
   MarkRecur(X1,Y1-1);
  IF X1<(PlayFieldXDimension-1) THEN
   MarkRecur(X1+1,Y1);
  IF Y1<(PlayFieldYDimension-1) THEN
   MarkRecur(X1,Y1+1);
  END;
END;

BEGIN
 CubesMarked:=0;
 TargetColor:=PlayField[X,Y];
 IF TargetColor<>3 THEN         {Can't mark black space}
  MarkRecur(X,Y);
END;

PROCEDURE FillPlayfield;
{Initial version, probably not nice to play with.
Some Life'ish algoritm would be better I think. (so that more aggregates exist)}

VAR X,Y,Last,Now : LONGINT;

BEGIN
 Last:=0;
 FOR X:=0 TO PlayFieldXDimension-1 DO
  FOR Y:=0 TO PlayFieldYDimension-1 DO
   BEGIN
    Now:=RANDOM(4);
    IF Now=3 THEN
     Now:=Last;
    PlayField[X,Y]:=Now;
    Last:=Now;
   END;
  MarkField:=PlayField;
END;

PROCEDURE Colapse;
{Processes the playfield if the mouse button is used.

  First the procedure deletes the marked area, and let gravity do its work
  Second the procedure uses as if some gravity existed on the left of the
  playfield }

VAR X, Y,J :LONGINT;

BEGIN
 {Vertical colapse: All marked pieces are deleted, and let gravity do it's work}
 IF CubesMarked>1 THEN
  BEGIN
   FOR X:=0 TO PlayFieldXDimension-1 DO
    BEGIN
     Y:=PlayFieldYDimension-1; J:=Y;
     while y>=0 do
      begin
       IF MarkField[X,Y]<>4 THEN
        BEGIN
         PlayField[X,J]:=PlayField[X,Y];
         DEC(J);
        END;
       DEC(Y);
      end;
    FOR Y:=0 TO J  DO
     PlayField[X,Y]:=3;
    END;
   J:=0;
   FOR X:=PlayFieldXDimension-2 DOWNTO 0  DO
    BEGIN
     IF PlayfIeld[X,PlayFieldYDimension-1]=3 THEN
      BEGIN
       Move(PlayfIeld[X+1,0],PlayField[X,0],PlayFieldYDimension*(PlayFieldXDimension-X-1));
       INC(J);
      END;
    END;
   IF J<>0 THEN
    FillChar(PlayField[PlayFieldXDimension-J,0],J*PlayFieldYDimension,#3);
   INC(Score,CubesToScore);
   ShowScore;
  END;
END;

PROCEDURE BuildScreen;
{Some procedures that build the screen}

Var S:String; // do not remove. Depends on usegraphics.

BEGIN
  {$IFDEF UseGraphics}
   setbkcolor(black);
   setviewport(0,0,getmaxx,getmaxy,clipoff);
   clearviewport;
  {$ELSE}
   ClrScr;
  {$ENDIF}
  Score:=0;
  ShowScore;
  ShowButtons;
  ShowHighScore;
  ShowMouse;
  {$IFDEF UseGraphics}
   SetTextStyle(0,Horizdir,2);
   OuttextXY(10,10,'SameGame v0.03, (C) by Marco v/d Voort. ');
   SetTextStyle(0,Horizdir,1);
   OuttextXY(50,40,'A demo for the FPC RTL and API units Crt,(MS)Mouse and Graph');
  {$ELSE}
  GotoXY(1,1);
  TextColor(Yellow);
  Write('SameGame v0.02');
  TextColor(White);
  Write('   A demo for the ');
  TextColor(Yellow); Write('FPC');
  TextColor(White); Write(' API or MsMouse unit. By Marco van de Voort');
  SetDefaultColor;
  {$ENDIF}
  IF LastScore<>0 THEN
   BEGIN
    {$Ifdef UseGraphics}
     SetTextStyle(0,Horizdir,1);
     Str(LastScore,S);
     OuttextXY(50,40,'The Score in the last game was :'+S);
    {$else}
     GotoXY(10,20);
     Write('The score in the last game was :',LastScore);
    {$endif}
   END;
  DisplayPlayField(PlayField);
 MarkField:=PlayField;
END;

PROCEDURE DoMainLoopMouse;
{The main game loop. The entire game runs in this procedure, the rest is
    initialisation/finalisation (like loading and saving highscores etc etc)}

VAR X,Y,
    MX,MY,MState,Dummy : LONGINT;
    EndOfGame          : LONGINT;
    S                  : String;

BEGIN
 RANDOMIZE;
 REPEAT
  FillPlayField;
  BuildScreen;
  EndOfGame:=0;
  REPEAT
   GetMouseState(MX,MY,MState);
   {$IFDEF UseGraphics}
    X:=2*((MX-GrFieldX) DIV ScalerX) +FieldX;
    Y:=((MY-GrFieldY) DIV ScalerY) +FieldY-1;
   {$ELSE}
    X:=MX SHR 3;
    Y:=MY SHR 3;
   {$ENDIF}
   IF PlayFieldPiecesLeft=0 THEN
    BEGIN
     INC(Score,1000);
     EndOfGame:=1;
    END
   ELSE
    BEGIN
     {$IFDEF UseGraphics}
      IF (MX>=MenuX) AND (MX<(MenuX+16*Length(GrNewGameLine))) THEN
       BEGIN {X in clickable area}
        IF (MY>=MenuY) AND (MY<(MenuY+RowDispl*3+2)) THEN
         BEGIN
          X:=65; {X doesn't matter as long as it is 60..69}
          Y:=((MY-MenuY) DIV RowDispl)+4;
         END;
       END;
     {$ENDIF}
     IF (X>=60) AND (X<=69) THEN
      BEGIN
         IF (MState AND LButton) <>0 THEN {If leftbutton pressed,}
          BEGIN
           IF Y=4 THEN
            EndOfGame:=1;
           IF Y=6 THEN
            EndOfGame:=2;
           IF (EndOfGame>0) AND (PlayFieldPiecesLeft=0) THEN
            INC(Score,1000);
           IF Y=5 THEN
            BEGIN
             ShowHelp;
             BuildScreen;
            END;
           {$IFDEF Unix}
           IF Y=7 THEN
            BEGIN
             write(#27+'(K');
             BuildScreen;
            END;
           {$ENDIF}
        END;
      END;
    IF (X>=(FieldX-2)) AND (Y>=(FieldY-2)) THEN
     BEGIN

      DEC(X,FieldX-1);
      DEC(Y,FieldY-1);
      X:=X SHR 1;
      IF (x>=0) and (y>=0) and (X<PlayFieldXDimension) AND (Y<PlayFieldYDimension) THEN
       BEGIN
        IF MarkField[X,Y]<>4 THEN
         BEGIN
          MarkField:=PlayField;
          MarkAfield(X,Y);
          DisplayPlayField(MarkField);
          {$ifdef UseGraphics}
           SetFillStyle(SolidFill,black);
           Bar(420,440,540,460);
           SetTextStyle(0,Horizdir,1);
           Str(CubesToScore,S);
           OuttextXY(420,440,'Marked : '+S);
          {$else}
           TextColor(White);
           GotoXY(20,22);
           Write(' ':20);
           GotoXY(20,22);
           Write('Marked :',CubesToScore);
          {$endif}
         END;
        IF (MarkField[X,Y]=4) AND ((MState AND LButton) <>0) THEN
                                   {If leftbutton pressed,}
         BEGIN
          REPEAT                            {wait untill it's released.
                                           The moment of pressing counts}
           GetMouseState(X,Y,Dummy);
          UNTIL (Dummy AND LButton)=0;
          Colapse;
          MarkField:=PlayField;
          DisplayPlayField(MarkField);
        END
      END
    END;
   IF KeyPressed THEN
    BEGIN
     X:=GetKey;
     IF (CHR(X) IN ['X','x','Q','q']) OR (X=27) THEN
      EndOfGame:=2;
    END;
   END;
  UNTIL EndOfGame>0;
  ShowScore;
  X:=SlipInScore(Score);
  IF X<>0 THEN
   BEGIN
    HideMouse;
    ShowHighScore;
    {$IFDEF UseGraphics}
     Str(Score:5,S);
     OutTextXY(HighX+150,HighY+LineDistY*(10-X),S);
     GrInputStr(S,HighX,HighY+LineDistY*(10-X),16,12,10,FALSE,AlfaBeta);
    {$ELSE}
     InputStr(S,HighX,HighY+12-X,10,FALSE,AlfaBeta);
    {$ENDIF}
    HighScore[X-1].Name:=S;
    ShowMouse;
   END;
  LastScore:=Score;
  UNTIL EndOFGame=2;
END;

CONST FileName='samegame.scr';

VAR I : LONGINT;
    {$IFDEF UseGraphics}
    gd,gm : INTEGER;
    Pal   : PaletteType;
    {$ENDIF}

BEGIN
 {$IFDEF UseGraphics}
  {$ifdef Win32}
   ShowWindow(GetActiveWindow,0);
  {$endif}
  gm:=vgahi;
  gd:=vga;
  InitGraph(gd,gm,'');
  if GraphResult <> grOk then
    begin
      Writeln('Graph driver ',gd,' graph mode ',gm,' not supported');
      Halt(1);
    end;
  SetFillStyle(SolidFill,1);
  GetDefaultPalette(Pal);
  SetAllPalette(Pal);
  {$ifdef win32}
   {$ifdef win32}
    Windows.SetWindowText(GraphWindow,'Samegame, a demonstration of Free Pascal');
   {$endif}

  {$endif}
 {$ENDIF}
  IF NOT MousePresent THEN
   BEGIN
    Writeln('No mouse found. A mouse is required!');
    HALT;
   END;
  FOR I:=0 TO 9 DO
   HighScore[I].Score:=I*1500;
  LoadHighScore(FileName);
  InitMouse;
  {$ifndef Win32Graph}
   CursorOff;
  {$endif}
 {$IFDEF UseGraphics}
    HighX:=450;   HighY:=220; {the position of the highscore table}
 {$else}
    HighX:=52;   HighY:=10; {the position of the highscore table}
  {$endif}

  DoMainLoopMouse;

  HideMouse;
  DoneMouse;
  {$ifndef Win32Graph}
   CursorOn;
  {$endif}
  SaveHighScore;
  {$IFDEF UseGraphics}
   CloseGraph;
  {$ENDIF}
  {$ifndef Win32Graph}
   ClrScr;
  Writeln;
  Writeln('Last games'#39' score was : ',Score);
  {$endif}
END.
{
  $Log: samegame.pp,v $
  Revision 1.10  2005/02/14 17:13:10  peter
    * truncate log

  Revision 1.9  2004/06/21 07:03:36  marco
   * 2nd recommendation 3177

  Revision 1.8  2004/06/21 07:01:34  marco
   * 1st and 3rd recommendation of bug 3177

  Revision 1.7  2004/02/18 16:43:29  marco
   *  added an API call to avoid the "Graph Window" window title, and readded previously removed variable
        It was used in usegraph

  Revision 1.6  2003/09/06 14:14:12  marco
   * removed unused var reported in bug 2170

  Revision 1.5  2002/09/07 15:06:35  peter
    * old logs removed and tabs fixed

  Revision 1.4  2002/06/02 09:49:17  marco
   * Renamefest

  Revision 1.3  2002/02/22 21:41:22  carl
  * range check error fix

}
