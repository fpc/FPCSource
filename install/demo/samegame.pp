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

Uses Crt,MsMouse;

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

VAR MarkField,PlayField : PlayFieldType;
    CubesMarked         : LONGINT;
    Score               : LONGINT;

FUNCTION CubesToScore : LONGINT;
{Function to calculate score from the number of cubes. Should have a higher
order than linear, or the purpose of the game disappears}

BEGIN
 CubesToScore:=(CubesMarked*CubesMarked) DIV 2;
END;

PROCEDURE MarkAfield(X,Y:LONGINT);
{Recursively marks the area adjacent to (X,Y);

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

VAR X,Y : LONGINT;

BEGIN
 FOR Y:=0 TO PlayFieldYDimension-1 DO
  FOR X:=0 TO PlayFieldXDimension-1 DO
   PlayField[X,Y]:=RANDOM(3);
  MarkField:=PlayField;
END;

PROCEDURE ShowScore;
{Simply procedure to update the score}

BEGIN
 TextColor(White);
 GotoXY(20,23);
 Write(' ':20);
 GotoXY(20,23);
 Write('Score : ',Score);
END;

PROCEDURE Colapse;
{Processes the playfield if the mouse button is used}

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

PROCEDURE DoMainLoopMouse;

VAR X,Y,
    MX,MY,MState,Dummy : LONGINT;

BEGIN
 MarkField:=PlayField;
 REPEAT
  GetMouseState(MX,MY,MState);
  X:=MX SHR 3;
  Y:= MY SHR 3;
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
       END;
     END;
   END;
 UNTIL (MState AND RButton) =RButton;
END;

BEGIN
  IF NOT MouseFound THEN
   BEGIN
    Writeln('No mouse found. A mouse is required!');
    HALT;
   END;
  ShowMouse;

  RANDOMIZE;
  ClrScr; Score:=0;
  ShowScore;
  GotoXY(1,1);
  TextColor(Yellow);
  Write('SameGame v0.01');
  TextColor(White);
  Write('   A demo for the FPC MsMouse unit. By Marco van de Voort');
  FillPlayField;
  DisplayPlayField(PlayField);
  DoMainLoopMouse;
  HideMouse;
END.
{
  $Log$
  Revision 1.1  1999-05-27 21:36:34  peter
    * new demo's
    * fixed mandel for linux

}
