{
    $Id$

    This program is both available in XTDFPC as in the FPC demoes.
    Copyright (C) 1999 by Marco van de Voort

    SameGame is a standard game in GNOME and KDE. I liked it, and I
    automatically brainstormed how I would implement it.
    It turned out to be really easy, and is basically only 100 lines or so.

    The game demonstrates some features of the MSMOUSE unit, and some of
    the Crt unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
PROGRAM SameGame;
Uses Crt,GameUnit;

CONST   FieldX                            = 10; {Top left playfield coordinates}
        FieldY                            =  3; {Top left playfield coordinates}
        PlayFieldXDimension               = 20; {Dimensions of playfield}
        PlayFieldYDimension               = 15;

       {Used colors. Colors[0..2] are the colors used on the playfield, Colors[3]
          is the background and Colors[4] is the color used to mark the pieces}
        Colors : ARRAY [0..4] OF LONGINT  = (White,Blue,Red,Black,LightMagenta);

TYPE PlayFieldType=ARRAY[0..PlayFieldXDimension-1,0..PlayFieldYDimension-1] OF BYTE;

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

PROCEDURE ShowHelp;
{Shows some explanation of the game and waits for a key}

VAR I : LONGINT;

BEGIN
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
 Writeln('If you manage to kill the entire playfield, you'#39'll get a bonus');
 Writeln;
 WriteLn('Press any key to get back to the game');
 GetKey;
END;

VAR MarkField,PlayField : PlayFieldType; {The playfield, and the marked form}
    CubesMarked         : LONGINT;       {Cubes currently marked}
    Score               : LONGINT;       {The current score}
    LastScore           : LONGINT;

PROCEDURE ShowButtons;
{Shows the clickable buttons}

BEGIN
 TextColor(Yellow); TextBackGround(Blue);
 GotoXY(60,5);   Write('NEW game');
 GotoXY(60,6);   Write('HELP');
 GotoXY(60,7);   Write('END game');
 {$IFDEF Linux}
  GotoXY(60,8);   Write('Force IBM charset');
 {$ENDIF}
 SetDefaultColor;
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

BEGIN
 TextColor(White);
 GotoXY(20,23);   Write(' ':20);
 GotoXY(20,23);   Write('Score : ',Score);
 SetDefaultColor;
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
     REPEAT
       IF MarkField[X,Y]<>4 THEN
        BEGIN
         PlayField[X,J]:=PlayField[X,Y];
         DEC(J);
        END;
       DEC(Y);
      UNTIL Y<0;
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

BEGIN
  ClrScr; Score:=0;
  ShowScore;
  ShowButtons;
  ShowHighScore;
  ShowMouse;
  GotoXY(1,1);
  TextColor(Yellow);
  Write('SameGame v0.02');
  TextColor(White);
  Write('   A demo for the ');
  TextColor(Yellow); Write('FPC');
  TextColor(White); Write(' API or MsMouse unit. By Marco van de Voort');
  SetDefaultColor;
  IF LastScore<>0 THEN
   BEGIN
    GotoXY(10,20);
    Write('The score in the last game was :',LastScore);
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
   X:=MX SHR 3;
   Y:=MY SHR 3;
   IF PlayFieldPiecesLeft=0 THEN
    BEGIN
     INC(Score,1000);
     EndOfGame:=1;
    END
   ELSE
    BEGIN
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
           {$IFDEF Linux}
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
      DEC(X,FieldX-1); DEC(Y,FieldY-1);
      X:=X SHR 1;
      IF (X<PlayFieldXDimension) AND (Y<PlayFieldYDimension) THEN
       BEGIN
        IF MarkField[X,Y]<>4 THEN
         BEGIN
          MarkField:=PlayField;
          MarkAfield(X,Y);
          DisplayPlayField(MarkField);
          TextColor(White);
          GotoXY(20,22);
          Write(' ':20);
          GotoXY(20,22);
          Write('Marked :',CubesToScore);
         END;
        IF (MState AND LButton) <>0 THEN {If leftbutton pressed,}
         BEGIN
          REPEAT                            {wait untill it's released.
                                           The moment of pressing counts}
           GetMouseState(X,Y,Dummy);
          UNTIL (Dummy AND LButton)=0;
          Colapse;
          MarkField:=PlayField;
          DisplayPlayField(MarkField);
        END;
      END;
    END;
   IF KeyPressed THEN
    BEGIN
     X:=GetKey;
     IF (X=ORD('X')) OR (X=ORD('x'))  THEN
      EndOfGame:=2;
    END;
   END;
  UNTIL EndOfGame>0;
  ShowScore;
  X:=SlipInScore(Score);
  IF X<>0 THEN
   BEGIN
    ShowHighScore;
    InputStr(S,HighX,HighY+12-X,10,FALSE,AlfaBeta);
    HighScore[X-1].Name:=S;
   END;
  LastScore:=Score;
  UNTIL EndOFGame=2;
END;

CONST FileName='samegame.scr';

VAR I : LONGINT;

BEGIN
  IF NOT MousePresent THEN
   BEGIN
    Writeln('No mouse found. A mouse is required!');
    HALT;
   END;
  FOR I:=1 TO 10 DO
   HighScore[I].Score:=I*1500;
  LoadHighScore(FileName);
  InitMouse;
  CursorOff;
  HighX:=52;   HighY:=10; {the position of the highscore table}

  DoMainLoopMouse;

  HideMouse;
  DoneMouse;
  CursorOn;
  SaveHighScore;
  ClrScr;
  Writeln;
  Writeln('Last games'#39' score was : ',Score);
END.
{
  $Log$
  Revision 1.2  1999-06-01 19:24:33  peter
    * updates from marco

  Revision 1.1  1999/05/27 21:36:34  peter
    * new demo's
    * fixed mandel for linux

}
