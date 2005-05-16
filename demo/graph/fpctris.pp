{
    $Id: fpctris.pp,v 1.6 2004/02/18 16:43:04 marco Exp $

    This program is both available in XTDFPC as in the FPC demoes.
    Copyright (C) 1999 by Marco van de Voort

    FPCTris implements a simple Crt driven Tetrisish game to demonstrate the
    Crt unit. (KeyPressed, ReadKey, GotoXY, Delay,TextColor,TextBackground)
    Quality games cost money, so that's why this one is free.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

PROGRAM FPCTris;
{ Trying to make a tetris from zero as a demo for FPC.
  Problems: - Colorsupport is a hack which handicaps creating a better
               update mechanism. (is done now)
            - Graph version input command has no cursor.
            - Graph or text isn't decided runtime, but compilertime.
            - Linux status graph version unknown at this moment.
            - Graphic and textmode speed of the game is not the same.
               The delay is fixed, and the time required to update is
               not constant due to optimisations.

  Coordinate system:

   0  ->   TheWidth-1            A figure is coded in a LONGINT like this:
   ---------
0 |   *     |                    ..*.            00100000    MSB
| |   **    |                    ..*.            00100000
V |   *     |                    .**.            01100000
  |         |                    ....            00000000    LSB
  |+   ++ ++|
  |++ ++++++|                  so  00100000001000000110000000000000b
  |+++++++++|
   ---------
TheHeight-1

}

{$ifdef UseGraphics}
 {$ifdef Win32}
   {$define Win32Graph}
   {$APPTYPE GUI}
 {$endif}
{$endif}

Uses
{$ifdef Win32Graph}
 WinCrt, Windows,
{$else}
 Crt,
{$endif}
 Dos,
{$IFDEF UseGraphics}
 Graph,
{$ENDIF}
 GameUnit;

{$DEFINE DoubleCache}

CONST TheWidth  = 11; {Watch out, also correct RowMask!}
      TheHeight = 20;
{$IFNDEF UseGraphics}
      PosXField = 10; { Upper X,Y coordinates of playfield}
      PosYField = 3;
{$ENDIF}
      MaxFigures= 16; {Maximum # figures place is reserved for.}
      NrLevels  = 12; {Number of levels currenty defined}
{      FieldSpace= 177;}

{$IFDEF UseGraphics}
      DisplGrX=110;
      DisplGrY=90;
      DisplGrScale=16;
      HelpY=130;
{$ENDIF}

      {$IFDEF UseGraphics}
       BaseX     =300;   {Coordinates of highscores}
       BaseY     =HelpY+20+8*LineDistY;  {y coordinate relative to other options}
      {$ELSE}
       BaseX     =40;
       BaseY     =9;
      {$ENDIF}

TYPE TetrisFieldType = ARRAY [0..25] OF LONGINT;
     LevelInfoType   = ARRAY [0..NrLevels-1] OF LONGINT;
     FigureType      = LONGINT;    { actually array[0..4][0..4] of bit rounded up to a longint}
{     CHARSET         = SET OF CHAR;}

{The figures, are converted to binary bitmaps on startup.}

CONST GraphFigures : ARRAY[0..4] OF String[80] =(
'.*... .*... .*... ..*.. .*... .*... **... **... ..**. .**.. ..*.. *....',
'.*... .*... .**.. .**.. .*... .**.. **... .*... ..*.. .**.. ..*.. **...',
'**... .**.. ..*.. .*... .*... .*... ..... .*... ..*.. .**.. **.** .**..',
'..... ..... ..... ..... .*... ..... ..... .***. ***.. .**.. ..*.. ..**.',
'..... ..... ..... ..... ..... ..... ..... ..... ..... .**.. ..*.. .....');

{Their relative occurance : }

      FigureChance : ARRAY[0..MaxFigures-1] OF LONGINT =(
  8,     8,    8,    8,     8,   8,   10,    1,   1,     1,    1,    1,0,0,0,0 );

{Scores per figure. Not necessarily used. Just for future use}

      FigureScore  : ARRAY[0..MaxFigures-1] OF LONGINT =(
  2,     2,    4,    4,     1,   2,    2,   10,  10,    10,   20,   10,0,0,0,0 );

{Diverse AND/OR masks to manipulate graphics}

{general table to mask out a bit 31=msb 0=lsb}
 AndTable : ARRAY[0..31] OF LONGINT=($80000000,$40000000,$20000000,$10000000,
    $8000000,$4000000,$2000000,$1000000,$800000,$400000,$200000,$100000,
    $80000,$40000,$20000,$10000,$8000,$4000,$2000,$1000,$800,$400,$200,$100,
    $80,$40,$20,$10,8,4,2,1);

{Mask to isolate a row of a (FigureType)}

 MagicMasks : ARRAY[0..4] OF LONGINT = ($F8000000,$07C00000,$003E0000,$0001F000,$00000F80);

{Mask to check if a line is full; a bit for every column aligned to left.}
 RowMask    = $FFE00000;

{Masks to calculate if the left or rightside is partially empty, write them
in binary, and put 5 bits on a row. }

 LeftMask : ARRAY[0..4] OF LONGINT = ($84210800,$C6318C00,$E739CE00,$F7BDEF00,$FFFFFFE0);
 RightMask: ARRAY[0..4] OF LONGINT = ($08421080,$18C63180,$39CE7380,$7BDEF780,$FFFFFF80);

{Allowed characters entering highscores}

{This constant/parameter is used to detect a certain bug. The bug was fixed, but
I use the constant to remind where the bug was, and what is related to eachother.}

   Tune=-1;

{First array is a table to find the level for a given number of dissappeared lines
 the second and third are the delaytime and iterationlevel per level.  }

  LevelBorders  : LevelInfoType = ( 10, 20, 30, 45, 60, 80,100,130,160,200,240,280);
  DelayLevel    : LevelInfoType = (100, 90, 80, 70, 60, 60, 50, 40, 40, 20, 20,10);
  IterationLevel: LevelInfoType = (  5,  5,  5,  5,  5,  4,  4,  4,  3,  3,  2, 2);

{Some frequently used chars in high-ascii and low-ascii. UseColor selects between
them}
  ColorString = #196#179#192#217#219;
  DumbTermStr = '-|..*';

{ A multiplication factor to reward killing more then one line with one figure}

  ProgressiveFactor :  ARRAY[1..5] OF LONGINT = (10,12,16,22,30);

VAR
    TopX,TopY   : LONGINT;                      {Coordinates figure relative
                                                  to left top of playfield}
    FigureNr    : LONGINT;                      {Nr in Figure cache, second
                                                  index in Figures}
    {$IFDEF DoubleCache}
    BackField,                                  {Copy of the screen for faster matching}
    {$ENDIF}
    MainField   : TetrisFieldType;              {The screen grid}
    ColorField  : ARRAY[0..TheHeight-1,0..TheWidth-1] OF LONGINT; {The color info}
    DelayTime   : LONGINT;                      {Delay time, can be used for
                                                  implementing levels}
    IterationPerDelay : LONGINT;                {Iterations of mainloop (incl delay)
                                                 before the piece falls down a row}
    TotalChance : LONGINT;                      {Sum of FigureChange array}
    Lines       : LONGINT;                      {Completed lines}
    NrFigures   : LONGINT;                      {# Figures currently used}
    RightSizeArray,                             {Nunber of empty columns to the left }
    LeftSizeArray,                              {or right of the figure/piece}
    Figures     : ARRAY[0..MaxFigures-1,0..3] OF LONGINT; {All bitmap info of figures}

    NrFiguresLoaded : LONGINT;                  {Total figures available in GraphFigures}
    CurrentCol  : LONGINT;                      {Color of current falling piece}
    UseColor    : BOOLEAN;                      {Color/Mono mode}
    Level       : LONGINT;                      {The current level number}
{$IFNDEF UseGraphics}
    Style       : String;                       {Contains all chars to create the field}
{$ENDIF}
    nonupdatemode  : BOOLEAN;                   {Helpmode/highscore screen or game mode}
{$IFNDEF UseGraphics}
    HelpMode    : BOOLEAN;
{$ENDIF}
    NextFigure  : LONGINT;                      {Next figure to fall}
    Score       : LONGINT;                      {The score}


FUNCTION RRotate(Figure:FigureType;ColumnsToDo:LONGINT):FigureType;
{Rotate a figure to the right (=clockwise).

This new (v0.06) routine performs a ColumnsTodo x ColumnsToDo rotation,
instead of always a 4x4 (v0.04) or 5x5 (v0.05) rotation.

This avoids weird, jumpy behaviour when rotating small pieces.}

VAR I,J, NewFig:LONGINT;

BEGIN
 NewFig:=0;
 FOR I:=0 TO ColumnsToDo-1 DO
  FOR J:=0 TO ColumnsToDo-1 DO
   IF Figure AND AndTable[I*5+J]<>0 THEN
    NewFig:=NewFig OR AndTable[(ColumnsToDo-1-I)+5*(J)]; {}
 RRotate:=NewFig;
END;

{ LeftSize and RightSize count the number of empty lines to the left and
right of the character. On the below character LeftSize will return 2 and
RightSize will return 1.

        ..*.
        ..*.
        ..*.
        ..*.
}
FUNCTION RightSize(Fig:FigureType):LONGINT;

VAR I : LONGINT;

BEGIN
 I:=0;
 WHILE ((Fig AND RightMask[I])=0) AND (I<5) DO
  INC(I);
  IF I>4 THEN
   HALT;
 Rightsize:=I;
END;

FUNCTION Leftsize(Fig:FigureType):LONGINT;

VAR I : LONGINT;

BEGIN
 I:=0;
 WHILE ((Fig AND LeftMask[I])=0)  AND (I<5) DO
  INC(I);
  IF I>4 THEN
   HALT;
 Leftsize:=I;
END;

FUNCTION FigSym(Figure:LONGINT;RightSizeFig:LONGINT):LONGINT;
 {Try to find the "symmetry" of a figure, the smallest square (1x1,2x2,3x3 etc)
 in which the figure fits. This requires all figures designed to be aligned to
 topleft.}

VAR ColumnsToDo : LONGINT;

BEGIN
 {Determine which bottom rows aren't used}

 ColumnsToDo:=5;
 WHILE ((Figure AND MagicMasks[ColumnsToDo-1])=0) AND (ColumnsToDo>1) DO
  DEC(ColumnsToDo);

 {Compare with columns used, already calculated, and take the biggest}
 IF ColumnsToDo<(5-RightSizeFig) THEN
  ColumnsToDo:=5-RightSizeFig;
 FigSym:=ColumnsToDo;
END;


PROCEDURE CreateFiguresArray;
{Reads figures from ASCII representation into binary form, and creates the
 rotated representations, and the number of empty columns to the right and
 left per figure. }

VAR I,J,K,L,Symmetry : LONGINT;

BEGIN
 NrFigures:=0; K:=1;
 WHILE K<Length(GraphFigures[0]) DO
  BEGIN
   IF GraphFigures[0][K]=' ' THEN
    INC(K);
   L:=0;
   FOR I:=0 TO 4 DO   {Rows}
    FOR J:=0 TO 4 DO {Columns}
     IF GraphFigures[I][K+J]='*' THEN
      L:=L OR AndTable[I*5+J];
    Figures[NrFigures][0]:=L;
    INC(NrFigures);
    INC(K,5);
  END;
 NrFiguresLoaded:=NrFigures;
 FOR I:= 0 TO NrFigures-1 DO
  BEGIN
   RightSizeArray[I][0]:=RightSize(Figures[I][0]);
   LeftSizeArray[I][0]:=LeftSize(Figures[I][0]);
   Symmetry:=FigSym(Figures[I][0],RightSizeArray[I][0]);
   FOR J:=0 TO 2 DO                              {Create the other 3 by rotating}
    BEGIN
     Figures[I][J+1]:=RRotate(Figures[I][J],Symmetry);
     RightSizeArray[I][J+1]:=RightSize(Figures[I][J+1]);
     LeftSizeArray[I][J+1]:=LeftSize(Figures[I][J+1]);
    END;
   END;
{Clear main grid}
 FillChar(MainField,SIZEOF(TetrisFieldType),0);
END;

PROCEDURE CalculateTotalChance;
{Called after a change in the the number of figures, normally 7 (standard)
or NrFiguresLoaded (10 right now) to recalculate the total of the chance table}

VAR Temp:LONGINT;

BEGIN
 TotalChance:=0;
 FOR Temp:=0 TO NrFigures-1 DO INC(TotalChance,FigureChance[Temp]);
END;

FUNCTION MatchPosition(Fig:FigureType;X,Y:LONGINT): BOOLEAN;
{Most important routine. Tries to position the figure on the position
IF it returns FALSE then the piece overlaps something on the background,
or the lower limit of the playfield
}

VAR I,J,K  : LONGINT;
    Match: BOOLEAN;

BEGIN
 Match:=TRUE;
 FOR I:=0 TO 4 DO
  BEGIN
   K:=Fig;
   K:=K AND MagicMasks[I];
   IF K<>0 THEN
    BEGIN
     J:=5*(I)-X+Tune;
     IF J>0 THEN
      K:=K SHL J
     ELSE
      IF J<0 THEN
       K:=K SHR -J;
     IF (MainField[Y+I] AND K)<>0 THEN
      Match:=FALSE;
   END;
  END;
 I:=4;
 IF (Fig AND MagicMasks[4])=0 THEN
  DEC(I);
 IF (Fig AND MagicMasks[3])=0 THEN
  DEC(I);
 IF (Fig AND MagicMasks[2])=0 THEN
  DEC(I);
 IF (Y+I)>=TheHeight THEN
  Match:=FALSE;
 MatchPosition:=Match;
END;

PROCEDURE FixFigureInField(Fig:FigureType;X,Y:LONGINT;Clear:BOOLEAN);
{Blends the figure into the background, or erases the figure from the
background}

VAR I,J,K  : LONGINT;

BEGIN
 FOR I:=0 TO 4 DO
  BEGIN
   K:=Fig;
    K:=K AND MagicMasks[I];
   IF K<>0 THEN
    BEGIN
     J:=5*I-X+Tune;
     IF J>0 THEN
      K:=K SHL J
     ELSE
      IF J<0 THEN
       K:=K SHR (-J);
     IF Clear THEN
      BEGIN
       K:=K XOR -1;
       MainField[Y+I]:= MainField[Y+I] AND K;
      END
     ELSE
      MainField[Y+I]:= MainField[Y+I] OR K;
    END;
 END;
END;

PROCEDURE FixColField(ThisFig:LONGINT);
{Puts color info of a figure into the colorgrid, simplified
FixFigureInField on byte instead of bit manipulation basis.}

VAR I,J,K  : LONGINT;

BEGIN
 FOR I:=0 TO 4 DO
  BEGIN
   K:=Figures[ThisFig][FigureNr];
   IF (I+TopY)<=TheHeight THEN
    FOR J:=0 TO 4 DO
     BEGIN
      IF (K AND AndTable[J+5*I])<>0 THEN
       ColorField[TopY+I,TopX-Tune+J]:=CurrentCol
     END;
  END;
END;

PROCEDURE RedrawScreen;
{Frustrates the caching system so that the entire screen is redrawn}

VAR I : LONGINT;

BEGIN
 FOR I:=0 TO TheHeight-1 DO
  BackField[I]:=MainField[I] XOR -1;    {backup copy is opposite of MainField}
END;

FUNCTION GetNextFigure:LONGINT;

VAR IndTotal,Temp,TheFigure : LONGINT;

BEGIN
Temp:=RANDOM(TotalChance);
 IndTotal:=0;
 TheFigure:=0;
 WHILE Temp>=IndTotal DO
  BEGIN
   INC(IndTotal,FigureChance[TheFigure]);
   INC(TheFigure);
  END;
 dec(thefigure);
 GetNextFigure:=TheFigure;
END;

{$IFDEF UseGraphics}
 {$I ftrisgr.inc}
{$ELSE}
 {$I ftristxt.inc}
{$ENDIF}


FUNCTION InitAFigure(VAR TheFigure:LONGINT) : BOOLEAN;
{A new figure appears in the top of the screen. If return value=FALSE then
the piece couldn't be created (when it is overlapping with the background.
That's the game-over condition)}

VAR Temp : LONGINT;

BEGIN
 TopX:=(TheWidth-4) DIV 2;             { Middle of Screen}
 TopY:=0;
 FigureNr:=1;
 IF TheFigure<>-1 THEN
  INC(Score,FigureScore[TheFigure]);
 IF NOT NonUpdateMode THEN
  FixScores;
 Temp:=GetNextFigure;                   {Determine next char (after the one this
                                      initafigure created has got down)}
 TheFigure:=NextFigure;                 {Previous NextFigure becomes active now.}
 NextFigure:=Temp;
 InitAFigure:=MatchPosition(Figures[TheFigure][0],TopX,TopY);
 ShowNextFigure(NextFigure);
 CurrentCol:=RANDOM(14)+1;
END;

PROCEDURE FixLevel(Lines:LONGINT);


BEGIN
 Level:=0;
 WHILE (Lines>LevelBorders[Level]) AND (Level<HIGH(LevelBorders)) DO
  INC(Level);
 DelayTime:=DelayLevel[Level];
 IterationPerDelay:=IterationLevel[Level];
END;

PROCEDURE FixMainFieldLines;
{Deletes full horizontal lines from the playfield will also get some
score-keeping code in the future.}

VAR I,LocalLines : LONGINT;

BEGIN
 I:=TheHeight-1;
 LocalLines:=0;
 WHILE I>=0 DO
  BEGIN
   IF (MainField[I] XOR RowMask)=0 THEN
    BEGIN
     Move(MainField[0],MainField[1],I*4);
     Move(ColorField[0,0],ColorField[1,0],4*I*TheWidth);
     MainField[0]:=0;
     FillChar(ColorField[0,0],0,TheWidth);
     INC(LocalLines);
    END
   ELSE
    DEC(I);
  END;

 INC(Lines,LocalLines);

 I:=Level;
 FixLevel(Lines);
 IF LocalLines<>0 THEN
  BEGIN
   INC(Score,ProgressiveFactor[LocalLines]*LocalLines);
   ShowLines;
  END;
 {$IFDEF DoubleCache}
  IF UseColor THEN
   RedrawScreen;
 {$ENDIF}
END;

PROCEDURE DoFPCTris;
{The main routine. Initialisation, keyboard loop}

VAR EndGame   : BOOLEAN;
    FixHickup : LONGINT;
    Counter   : LONGINT;
    Temp,Key  : LONGINT;
    TheFigure : LONGINT;                      {Current first index in Figures}

PROCEDURE TurnFigure;
{Erases a figure from the grid, turns it if possible, and puts it back on
again}

BEGIN
  FixFigureInField(Figures[TheFigure][FigureNr],TopX,TopY,TRUE);
  IF MatchPosition(Figures[TheFigure][Temp],TopX,TopY) THEN
   FigureNr:=Temp;
  FixFigureInField(Figures[TheFigure][FigureNr],TopX,TopY,FALSE);
END;

PROCEDURE FixHighScores;

VAR I : LONGINT;
{$IFNDEF UseGraphics}
    J : LONGINT;
{$ENDIF}
    S : String;

BEGIN
{$IFDEF UseGraphics}
  Str(Score:5,S);
  SetFillStyle(SolidFill,0);            {Clear part of playfield}
  Bar(DisplGrX+DisplGrScale,DisplGrY + ((TheHeight DIV 2)-2)*DisplGrScale,
      DisplGrX+(TheWidth-1)*(DisplGrScale), DisplGrY + DisplGrScale*((TheHeight DIV 2)+5));
  SetTextStyle(0,Horizdir,2);
  OuttextXY(DisplGrX+DisplGrScale,DisplGrY+ DisplGrScale*((TheHeight DIV 2)-1),'GAME OVER');
  SetTextStyle(0,Horizdir,1);
  OutTextXY(DisplGrX+DisplGrScale,DisplGrY+ DisplGrScale*((TheHeight DIV 2)+3),'Score= '+S);
{$ELSE}
 FOR J:=9 TO 22 DO
    BEGIN
     GotoXY(40,J);
     Write(' ':38);
    END;
 IF UseColor THEN
  TextColor(White);
 GotoXY(40,23);
 Writeln('Game Over, score = ',Score);
{$ENDIF}
 I:=SlipInScore(Score);
 IF I<>0 THEN
  BEGIN
   NonUpdateMode:=TRUE;
{$IFNDEF UseGraphics}
   HelpMode:=FALSE;
{$ENDIF}
   ShowHighScore;
   {$IFDEF UseGraphics}
    OutTextXY(450,HelpY+20+(17-I+1)*LineDistY,S);
    GrInputStr(S,300,HelpY+20+(17-I+1)*LineDistY,16,12,10,FALSE,AlfaBeta);
   {$ELSE}
    InputStr(S,40,21-I,10,FALSE,AlfaBeta);
   {$ENDIF}
   HighScore[I-1].Name:=S;
  END;
 ShowHighScore;
END;

{$IFDEF UseGraphics}
VAR
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
    Windows.SetWindowText(GraphWindow,'FPCTris, a demonstration of Free Pascal');
  {$endif}
{$ENDIF}

 {Here should be some terminal-detection for Linux}
 nonupdatemode:=FALSE;
{$IFNDEF UseGraphics}
 HelpMode :=TRUE;
{$ENDIF}
 {$IFDEF Unix}
  UseColor:=FALSE;
 {$ELSE}
  UseColor:=TRUE;
 {$ENDIF}
 {$ifndef Win32Graph}
 ClrScr;
 CursorOff;
 {$endif}
 RANDOMIZE;
 HighX:=BaseX;
 HighY:=BaseY;
 CreateFiguresArray;                  { Load and precalculate a lot of stuff}
{$IFNDEF UseGraphics}
 IF UseColor THEN
  Style:= ColorString
 ELSE
  Style:=DumbTermStr;
{$ENDIF}

 NrFigures:=7;                        {Default standard tetris mode, only use
                                        the first 7 standard figures}
 CalculateTotalChance;                {Calculated the total of all weightfactors}
 EndGame:=FALSE;                      {When TRUE, end of game has been detected}
 FixHickup:=0;                        {Used to avoid unnecessary pauses with the "down key"}
 CreateFrame;                         {Draws all background garbadge}

 TheFigure:=-1;
 NextFigure:=GetNextFigure;              {Two figures have to be inited. The first
                                        figure starts dropping, and that is this
                                        one}
 InitAFigure(TheFigure);              {The second figure is the figure to be
                                       displayed as NEXT. That's this char :-)}
 DisplMainField;                  {Display/update the grid}
 Counter:=0;                          {counts up to IterationPerDelay}
 DelayTime:=200;                      {Time of delay}
 IterationPerDelay:=4;                {= # Delays per shift down of figure}
 Lines:=0;                            {Lines that have disappeared}
 Score:=0;
 ShowLines;
 REPEAT
  IF KeyPressed THEN                  {The function name says it all}
   BEGIN
    Key:=ORD(READKEY);
    IF Key=0 THEN                     {Function key?}
     Key:=ORD(READKEY) SHL 8;
    CASE Key OF                       {Check for all keys}
     ArrU : BEGIN
             Temp:=(FigureNr+3) AND 3;
             IF ((TopX+LeftSizeArray[TheFigure][FigureNr])<0) THEN
              BEGIN
              IF  (LeftSizeArray[TheFigure][FigureNr]<=LeftSizeArray[TheFigure][Temp]) THEN
               TurnFigure;
              END
             ELSE
             IF (TopX+7-RightSizeArray[TheFigure][FigureNr])>TheWidth THEN
              BEGIN
              IF  (RightSizeArray[TheFigure][FigureNr]<=RightSizeArray[TheFigure][Temp]) THEN
               TurnFigure;
              END
             ELSE
              TurnFigure;
           END;

    ArrL  : BEGIN
             IF (TopX+LeftSizeArray[TheFigure][FigureNr])>=0 THEN
              BEGIN
               Temp:=TopX+1-LeftSizeArray[TheFigure][FigureNr];
               FixFigureInField(Figures[TheFigure][FigureNr],TopX,TopY,TRUE);
               IF MatchPosition(Figures[TheFigure][FigureNr],TopX-1,TopY) THEN
                DEC(TopX);
               FixFigureInField(Figures[TheFigure][FigureNr],TopX,TopY,FALSE);
              END;
             END;

    ArrR  : BEGIN
             IF (TopX+7-RightSizeArray[TheFigure][FigureNr])<=TheWidth THEN
              BEGIN
               FixFigureInField(Figures[TheFigure][FigureNr],TopX,TopY,TRUE);
               IF MatchPosition(Figures[TheFigure][FigureNr],TopX+1,TopY) THEN
                INC(TopX);
               FixFigureInField(Figures[TheFigure][FigureNr],TopX,TopY,FALSE);
              END;
             END;

    ArrD  : BEGIN
             IF FixHickup=0 THEN
              BEGIN
             FixFigureInField(Figures[TheFigure][FigureNr],TopX,TopY,TRUE);
             Temp:=TopY;
             WHILE MatchPosition(Figures[TheFigure][FigureNr],TopX,TopY+1) DO
              INC(TopY);
             Temp:=TopY-Temp;
             INC(Score,Temp DIV 2);
             FixFigureInField(Figures[TheFigure][FigureNr],TopX,TopY,FALSE);
             FixHickUp:=4;
             END;
            END;

ORD('q'),
   ESC     : BEGIN
             SetDefaultColor;
             {$ifndef Win32Graph}
             GotoXY(1,25);
             {$endif}
             EndGame:=TRUE;
            END;

{$IFNDEF UseGraphics}
ORD('C'),
 ORD('c') : BEGIN
             UseColor:=NOT UseColor;
             IF UseColor THEN
              Style:= ColorString
             ELSE
              BEGIN
               SetDefaultColor;
               Style:=DumbTermStr;
              END;
             CreateFrame;
             RedrawScreen;
             DisplMainField;
            END;
 ORD('S'),
  ORD('s') : BEGIN
              IF NOT nonupdatemode THEN
               BEGIN
                NonUpdateMode:=TRUE;
                helpmode:=NOT helpmode;
               END
              ELSE
                HelpMode:=NOT helpmode;
               CreateFrame;
               ShowLines;
               ShowNextFigure(NextFigure);
              END;
{$ENDIF}
ORD('H'),
 ORD('h') : BEGIN
             nonupdatemode:=NOT nonupdatemode;
             CreateFrame;
             ShowLines;
             ShowNextFigure(NextFigure);
            END;
ORD('E'),
 ORD('e'): BEGIN                            {Extra figures on/off}
            IF NrFigures<>NrFiguresLoaded THEN
              NrFigures:=NrFiguresLoaded     {Extra figures}
            ELSE
              NrFigures:=7;                   {Standard Tetris figures}
            CalculateTotalChance;             {Recalculate weight-totals}
            IF UseColor THEN
             SetDefaultColor;
            ShowGameMode;
           END;

ORD('p') : BEGIN                             {"p" : Pause}
             Key:=ORD(ReadKey);
            IF Key=0 THEN
             Key:=ORD(ReadKey);
           END;
{$IFNDEF UseGraphics}
{$IFDEF Unix}
 ORD('i')  : write(#27+'(K');
{$ENDIF}
{$ENDIF}
        END; {END OF Key CASE}
      END { OF If KeyPressed}

  ELSE
   BEGIN
    {$IFDEF Unix}
     GotoXY(50,10);      {Get cursor out of the way, CursorOn/Off
                            doesn't work on telnet-terminals}
    {$ENDIF}
    Delay(DelayTime);
   END;

  INC(Counter);
  IF (Counter=IterationPerDelay) OR (FixHickup=1) THEN
   BEGIN
    IF FixHickup=1 THEN
      Counter:=IterationPerDelay-1
    ELSE
     Counter:=0;
    FixFigureInField(Figures[TheFigure][FigureNr],TopX,TopY,TRUE);
    FixHickup:=0;
    IF MatchPosition(Figures[TheFigure][FigureNr],TopX,TopY+1) THEN
     BEGIN
      INC(TopY);
      FixFigureInField(Figures[TheFigure][FigureNr],TopX,TopY,FALSE);
     END
    ELSE
    BEGIN
      FixFigureInField(Figures[TheFigure][FigureNr],TopX,TopY,FALSE);
      FixColField(TheFigure);
      IF InitAFigure(TheFigure) THEN
        BEGIN
         FixMainFieldLines;
         FixFigureInField(Figures[TheFigure][FigureNr],TopX,TopY,FALSE);
         DisplMainField;
         Delay(DelayTime*IterationPerDelay);
        END
      ELSE
       BEGIN
        FixFigureInField(Figures[TheFigure][FigureNr],TopX,TopY,FALSE);
        EndGame:=TRUE;
       END;
    END;
   END
  ELSE
   IF FixHickup>1 THEN
    DEC(FixHickup);
 DisplMainField;
 UNTIL EndGame;
 FixHighScores;
 {$ifndef Win32Graph}
 CursorOn;
 SetDefaultColor;
 GotoXY(1,25);
 {$endif}
 {$IFDEF UseGraphics}
  {$ifndef Win32}
  TextMode(CO80);
  {$endif}
 {$ENDIF}
END;

CONST FileName='fpctris.scr';

VAR I : LONGINT;

BEGIN
 FOR I:=0 TO 9 DO
  HighScore[I].Score:=(I+1)*750;
 LoadHighScore(FileName);
 DoFpcTris;
 SaveHighScore;
END.

{
  $Log: fpctris.pp,v $
  Revision 1.6  2004/02/18 16:43:04  marco
   * added an API call to avoid the "Graph Window" window title

  Revision 1.5  2002/09/07 15:06:34  peter
    * old logs removed and tabs fixed

  Revision 1.4  2002/06/02 09:49:17  marco
   * Renamefest

}
