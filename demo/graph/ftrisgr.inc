PROCEDURE ShowLines;

VAR S,S2 : String;

BEGIN
  SetFillStyle(SolidFill,0);
  Bar(300,460,550,478);
  Str(Lines:4,S2);
  S:='Lines : ';
  S:=S+S2+'   Level: ';
  Str(Level:4,S2);
  S:=S+S2;
  OutTextXY(300,460,S);
END;

PROCEDURE ShowGameMode;

BEGIN
   SetFillStyle(SolidFill,0);
   Bar(20,440,154,458);
   IF NrFigures<>7 THEN
    OutTextXY(20,440,'GameMode: Extended')
   ELSE
    OutTextXY(20,440,'GameMode: Standard')
END;

PROCEDURE CreateFrame;
{Used once to print the "background" of the screen (not the background grid,
but the text, and the cadre around the playfield)}

BEGIN
 setbkcolor(black);
 setviewport(0,0,getmaxx,getmaxy,clipoff);
 clearviewport;
 SetTextStyle(0,Horizdir,2);
 OuttextXY(30,50,'FPCTris v0.08, (C) by Marco v/d Voort.');
 SetTextStyle(0,Horizdir,1);
 OutTextXY(300,HelpY-30,'A demo of the FPC Graph unit');

 VLine(DisplGrX-1,DisplGrY,DisplGrY+DisplGrScale*TheHeight);
 VLine(DisplGrX+TheWidth*DisplGrScale,DisplGrY,DisplGrY+DisplGrScale*TheHeight);
 HLine(DisplGrX-1,DisplGrX+TheWidth*DisplGrScale,DisplGrY+DisplGrScale*TheHeight);

 {Clean below area}
 ShowGameMode;
 OutTextXY(300,HelpY,'Arrow left/right to move, down to drop');
 OutTextXY(300,HelpY+LineDistY,'arrow-up to rotate the piece');
 OutTextXY(300,HelpY+2*LineDistY,'"P" to pause');
 OutTextXY(300,HelpY+3*LineDistY,'"E" Mode (standard or extended)');
 OutTextXY(300,HelpY+5*LineDistY,'Escape to quit');
 OutTextXY(300,HelpY+20+6*LineDistY,'The Highscores');
 ShowHighScore;
END;

PROCEDURE DisplMainField;
{Graph mode version. Always caches.}


VAR Row,Column,Difference,StartRow,EndRow,
    L : LONGINT;
{    LastCol : LONGINT; }

BEGIN
 FOR Row:=0 TO TheHeight-1 DO
  BEGIN
   IF BackField[Row]<>MainField[Row] THEN
    BEGIN
     StartRow:=0;
     EndRow:=TheWidth-1;
     Difference:=MainField[Row] XOR BackField[Row];     {Calc differences in line}
     WHILE ((Difference AND AndTable[StartRow])=0) AND
        (StartRow<(TheWidth-1)) DO
      INC(StartRow);
     WHILE ((Difference AND AndTable[EndRow])=0) AND (EndRow>0) DO
      DEC(EndRow);
     FOR Column:=StartRow To EndRow DO
      BEGIN
       IF (MainField[Row] AND AndTable[Column])<>0 THEN
       BEGIN
         L:=ColorField[Row,Column];
         IF L=0 THEN
          L:=CurrentCol;
         IF L<>255 THEN
          BEGIN
           L:=L AND 15;
           SetFillStyle(SolidFill,L);
           Bar((Column)*DisplGrScale+DisplGrX,DisplGrY+DisplGrScale*Row,(Column+1)*DisplGrScale-1+DisplGrX,DisplGrY+DisplGrScale*(Row)+DisplGrScale-1);
          END;
        END
       ELSE
        BEGIN
           SetFillStyle(SolidFill,0);
           Bar((Column)*DisplGrScale+DisplGrX,DisplGrY+DisplGrScale*Row,(Column+1)*DisplGrScale-1+DisplGrX,DisplGrY+DisplGrScale*(Row)+DisplGrScale-1);
        END
      END;
   END;
  END;
  BackField:=MainField;     {Keep a copy of the screen for faster updates
                              of terminals, for next DisplMainField.}
END;

PROCEDURE ShowNextFigure(ThisFig:LONGINT);

CONST NextFigX=10;
      NextFigY=120;
      NextFigDim=16;

VAR I,J,K  : LONGINT;

BEGIN
 IF NOT nonupdatemode THEN
  BEGIN
   FOR I:=0 TO 4 DO
    BEGIN
     K:=Figures[ThisFig][FigureNr] AND MagicMasks[I];
     IF K=0 THEN
      BEGIN
       SetFillStyle(SolidFill,0);
       Bar(NextFigX,NextFigY+I*NextFigDim,NextFigX+5*NextFigDim-1,NextFigY+(I+1)*NextFigDim);
      END
     ELSE
      BEGIN
       FOR J:=0 TO 5 DO
         IF (K And AndTable[J+5*I])=0 THEN
          BEGIN
           SetFillStyle(SolidFill,0);
           Bar(NextFigX+J*NextFigDim,NextFigY+I*NextFigDim,NextFigX++(J+1)*NextFigDim,NextFigY+(I+1)*NextFigDim);
          END
         ELSE
          BEGIN
           SetFillStyle(SolidFill,1);
           Bar(NextFigX+J*NextFigDim,NextFigY+I*NextFigDim,NextFigX++(J+1)*NextFigDim,NextFigY+(I+1)*NextFigDim);
          END;
       END;
    END;
  END;
END;

PROCEDURE FixScores;

VAR S : String;

BEGIN
   Str(Score:5,S);
   SetFillStyle(SolidFill,0);
   Bar(300,440,450,458);
   OutTextXY(300,440,'Score :'+S);
END;

{
  $Log: ftrisgr.inc,v $
  Revision 1.3  2002/09/07 15:06:35  peter
    * old logs removed and tabs fixed

}
