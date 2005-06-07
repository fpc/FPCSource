PROCEDURE ShowLines;

BEGIN
 IF NOT nonupdatemode THEN
  BEGIN
   IF UseColor THEN
    TextColor(Yellow);
   GotoXY(40,16); Write('Lines: ',Lines:4,'    Level: ',Level);
  END;
END;

PROCEDURE ShowGameMode;

BEGIN
 IF NOT nonupdatemode THEN
  BEGIN
   GotoXY(61,13);
   IF NrFigures<>7 THEN
    write('Extended')
   ELSE
    write('Standard');
  END;
END;


PROCEDURE CreateFrame;
{Used once to print the "background" of the screen (not the background grid,
but the text, and the cadre around the playfield}

VAR I : LONGINT;

BEGIN
 SetDefaultColor;
 GotoXY(40,4);
 Write('FPCTris v0.08, (C) by Marco van de Voort');
 GotoXY(40,6);
 Write('A demo of the FPC Crt unit, and');
 GotoXY(40,7);
 Write(' its portability');
 FOR I:=9 TO 24 DO
  BEGIN
   GotoXY(40,I);
   Write(' ':38);
  END;
 ShowGameMode;
 IF nonupdatemode THEN
  BEGIN
   IF HelpMode THEN
    BEGIN
   GotoXY(40,9);
   Write('Arrow left/right to move, down to drop');
   GotoXY(40,10);
   Write('arrow-up to rotate the piece');
   GotoXY(40,11);
   Write('"P" to pause');
   GotoXY(40,12);
   Write('"E" Mode (standard or extended)');
   GotoXY(40,13);
   Write('"C" switches between color/mono mode');
   GotoXY(40,14);
   Write('Escape to quit');
   GotoXY(40,15);
   Write('"S" to show the highscores');
   {$IFDEF Unix}
   GotoXY(40,16);
   Write('"i" try to switch to IBM character set');
   {$ENDIF}
   END
   ELSE
    ShowHighScore;
  END
 ELSE
  BEGIN
   GotoXY(40,9);
   Write('"h" to display the helpscreen');
  END;

 FOR I :=0 TO TheHeight-1 DO
  BEGIN
   GotoXY(PosXField-1 ,PosYField+I); Write(Style[2]);
   GotoXY(PosXField+2*TheWidth ,PosYField+I); Write(Style[2]);
  END;
 GotoXY(PosXField-1,PosYField+TheHeight);
 Write(Style[3]);
 FOR I:=0 TO (2*TheWidth)-1 DO
  Write(Style[1]);
 Write(Style[4]);
END;
PROCEDURE DisplMainFieldMono;
{Displays the grid with a simple buffering algoritm, depending on
conditional DoubleBuffer}

VAR Row,Column,Difference,StartRow,EndRow : LONGINT;
    S : String;

BEGIN
 FOR Row:=0 TO TheHeight-1 DO
  BEGIN
   {$IFDEF DoubleCache}
    IF BackField[Row]<>MainField[Row] THEN
     BEGIN
    {$ENDIF}
   FillChar(S[1],2*TheWidth,#32);
   StartRow:=0;
   EndRow:=TheWidth-1;
   {$IFDEF DoubleCache}
    Difference:=MainField[Row] XOR BackField[Row];     {Calc differences in line}
    {Search for first and last bit changed}
    WHILE ((Difference AND AndTable[StartRow])=0) AND (StartRow<(TheWidth-1)) DO
     INC(StartRow);
    WHILE ((Difference AND AndTable[EndRow])=0) AND (EndRow>0) DO
     DEC(EndRow);
   {$ENDIF}
   {Prepare a string}
   GotoXY(PosXField+2*StartRow,PosYField+Row);
   S[0]:=CHR(2*(EndRow-StartRow+1));
   FOR Column:=0 TO EndRow-StartRow DO
    BEGIN
     IF (MainField[Row] AND AndTable[StartRow+Column])<>0 THEN
      BEGIN
       S[Column*2+1]:=Style[5];
       S[Column*2+2]:=Style[5];
      END;
    END;
   {Write the string}
   Write(S);
   {$IFDEF DoubleCache}
    END;
   {$ENDIF}
  END;
 {$IFDEF DoubleCache}
  BackField:=MainField;     {Keep a copy of the screen for faster updates
                              of terminals, for next DisplMainField.}
 {$ENDIF}
END;

PROCEDURE DisplMainFieldColor;
{Same as above, but also use ColorField to output colors,
 the buffering is the same, but the colors make it less efficient.}

VAR Row,Column,Difference,StartRow,EndRow,
    L : LONGINT;
    S   : String;
    LastCol : LONGINT;

BEGIN
 LastCol:=255;
 FOR Row:=0 TO TheHeight-1 DO
  BEGIN
   {$IFDEF DoubleCache}
    IF BackField[Row]<>MainField[Row] THEN
     BEGIN
    {$ENDIF}
   FillChar(S[1],2*TheWidth,#32);
   StartRow:=0;
   EndRow:=TheWidth-1;
   {$IFDEF DoubleCache}
    Difference:=MainField[Row] XOR BackField[Row];     {Calc differences in line}
    WHILE ((Difference AND AndTable[StartRow])=0) AND (StartRow<(TheWidth-1)) DO
     INC(StartRow);
    WHILE ((Difference AND AndTable[EndRow])=0) AND (EndRow>0) DO
     DEC(EndRow);
   {$ENDIF}
   GotoXY(PosXField+2*StartRow,PosYField+Row);
   FOR Column:=0 TO EndRow-StartRow DO
    BEGIN
     IF (MainField[Row] AND AndTable[StartRow+Column])<>0 THEN
      BEGIN
       L:=ColorField[Row,StartRow+Column];
       IF L=0 THEN
        L:=CurrentCol;
       IF L<>LastCol THEN
        BEGIN
         TextColor(L);
         Write(Style[5],Style[5]);
        END;
      END
     ELSE
      Write('  ');
    END;
   {$IFDEF DoubleCache}
    END;
   {$ENDIF}
  END;
 {$IFDEF DoubleCache}
  BackField:=MainField;     {Keep a copy of the screen for faster updates
                              of terminals, for next DisplMainField.}
 {$ENDIF}
END;

PROCEDURE DisplMainField;
{Main redraw routine; Check in what mode we are and call appropriate routine}

BEGIN
   IF UseColor THEN
    DisplMainFieldColor
   ELSE
    DisplMainFieldMono;
END;


PROCEDURE ShowNextFigure(ThisFig:LONGINT);

VAR I,J,K  : LONGINT;
    S      : String[8];

BEGIN
 IF UseColor THEN
  TextColor(White);
 IF NOT nonupdatemode THEN
  BEGIN
   FOR I:=0 TO 4 DO
    BEGIN
     FillChar(S,9,' ');
     S[0]:=#8;
     K:=Figures[ThisFig][FigureNr];
     IF (I+TopY)<=TheHeight THEN
      FOR J:=0 TO 4 DO
       BEGIN
        IF (K AND AndTable[J+5*I])<>0 THEN
         BEGIN
          S[J*2+1]:=Style[5];
          S[J*2+2]:=Style[5];
         END
       END;
     GotoXY(50,11+I); Write(S);
    END;
  END;
END;

PROCEDURE FixScores;

BEGIN
   IF UseColor THEN
    SetDefaultColor;
   GotoXY(40,18);
   Write('Score :',Score);
END;

