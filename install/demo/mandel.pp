{ Mandelbrot 2 (C)opyright 1994 by Gernot Tenchio }
{ dieses Programm kann modifiziert, geloescht, verschenkt, kopiert, validiert, }
{ bewegt, komprimiert, ausgelacht usw. werden. Allerdings bittscheen immer mit }
{ meinem (G)obbirait }

USES GRAPH;

const shift:byte=12;

VAR SerchPoint,ActualPoint,NextPoint       : PointType ;
    LastColor                              : longint;
    Gd,Gm,Max_Color,Max_X_Width,
    Max_Y_Width,Y_Width                    : INTEGER ;
    Y1,Y2,X1,X2,Dy,Dx                      : Real ;
    Zm                                     : Integer ;
    Flag                                   : BOOLEAN ;
    LineY                                  : ARRAY [0..600] OF BYTE;
    LineX                                  : ARRAY [0..100,0..600] OF INTEGER;
CONST
    SX : ARRAY [0..7] OF SHORTINT=(-1, 0, 1, 1, 1, 0,-1,-1);
    SY : ARRAY [0..7] OF SHORTINT=(-1,-1,-1, 0, 1, 1, 1, 0);
TYPE
    ArrayType = array[1..50] of integer;
{------------------------------------------------------------------------------}

FUNCTION CalcMandel(Point:PointType; z:integer) : Longint ;
var x,y,xq,yq,Cx,Cy : real ;

BEGIN
Cy:=y2 + dy*Point.y ;
Cx:=x2 + dx*Point.x ;
X:=-Cx ; Y:=-Cy ;
REPEAT
xq:=x * x;
yq:=y * y  ;
y :=x * y;
y :=y + y - cy;
x :=xq - yq - cx ;
z :=z -1;
UNTIL (Z=0) OR (Xq + Yq > 4 );
IF Z=0 Then CalcMandel:=1 else CalcMandel:=(z mod Max_Color) + 1 ;
END ;
{-----------------------------------------------------------------------------}
PROCEDURE Partition(VAR A : ArrayType; First, Last : Byte);
{ ist nicht auf meinem Mist gewachsen. Weiss aber auch nicht mehr so richtig 
  wo es herkommt. Allseits bekannter Sortieralgo }
VAR
    Right,Left : BYTE ;
    V,Temp : integer;
BEGIN
    V := A[(First + Last) SHR 1];
    Right := First;
    Left := Last;
    REPEAT
      WHILE (A[Right] < V) DO
        Right:=Right+1;
      WHILE (A[Left] > V) DO
        Left:=Left-1;
      IF (Right <= Left) THEN
        BEGIN
          Temp:=A[Left];
          A[Left]:=A[Right];
          A[Right]:=Temp;
          Right:=Right+1;
          Left:=Left-1;
        END;
    UNTIL Right > Left;
    IF (First < Left) THEN
      Partition(A, First, Left);
    IF (Right < Last) THEN
      Partition(A, Right, Last)
END;

FUNCTION BlackScan(var NextPoint:PointType) : BOOLEAN ;
BEGIN
BlackScan:=TRUE;
   REPEAT
          IF NextPoint.X=Max_X_Width THEN
             BEGIN
             IF NextPoint.Y < Y_Width THEN
                BEGIN
                NextPoint.X:=0 ;
                NextPoint.Y:=NextPoint.Y+1;
                END
                ELSE
                BEGIN
                BlackScan:=FALSE;
                EXIT;
             END ; { IF }
          END ; { IF }
          NextPoint.X:=NextPoint.X+1;
   UNTIL GetPixel(NextPoint.X,NextPoint.Y)=0;
END ;
{------------------------------------------------------------------------------}
PROCEDURE Fill(Ymin,Ymax,LastColor:integer);
VAR P1,P3,P4,P    : INTEGER ;
    Len,P2        : BYTE ;
    Darray        : ARRAYTYPE;

BEGIN
SetColor(LastColor);
FOR P1:=Ymin+1 TO Ymax-1 DO
BEGIN
  Len:=LineY[P1] ;
  IF Len >= 2 THEN
  BEGIN
       FOR P2:=1 TO Len DO
       BEGIN
         Darray[P2]:=LineX[P2,P1] ;
       END; { FOR }
       IF Len > 2 THEN Partition(Darray,1,len);
       P2:=1;
       REPEAT
          P3:= Darray[P2] ; P4:= Darray[P2 + 1];
          IF P3 <> P4 THEN
          BEGIN
             LINE ( P3 , P1 , P4 , P1) ;
             IF Flag THEN
             BEGIN
               P:=Max_Y_Width-P1;
               LINE ( P3 , P , P4 , P ) ;
             END;
          END; { IF }
          P2:=P2+2;
       UNTIL P2 >= Len ;
  END; { IF }
END; { FOR }
END;

{-----------------------------------------------------------------------------}

Function NewPosition(Last:Byte):Byte;
begin
  newposition:=(((last+1) and 254)+6) and 7;
END;

{-----------------------------------------------------------------------------}

PROCEDURE CalcBounds;
VAR LastOperation,KK,
    Position                     : Byte ;
    foundcolor                   : longint;
    Start,Found,NotFound         : BOOLEAN ;
    MerkY,Ymax                   : Integer ;
LABEL L;
BEGIN
REPEAT
   FillChar(LineY,SizeOf(LineY),0) ;
   ActualPoint:=NextPoint;
   LastColor:=CalcMandel(NextPoint,Zm) ;
   PUTPIXEL (ActualPoint.X,ActualPoint.Y,LastColor);
   IF Flag THEN PUTPIXEL (ActualPoint.X,
                          Max_Y_Width-ActualPoint.Y,LastColor) ;
   Ymax:=NextPoint.Y ;
   MerkY:=NextPoint.Y ;
   NotFound:=FALSE ;
   Start:=FALSE ;
   LastOperation:=4 ;
REPEAT
   Found:=FALSE ;
   KK:=0 ;
   Position:=NewPosition(LastOperation);
REPEAT
  LastOperation:=(Position+KK) AND 7 ;
  SerchPoint.X:=ActualPoint.X+Sx[LastOperation];
  SerchPoint.Y:=ActualPoint.Y+Sy[LastOperation];
  IF ( (SerchPoint.X < 0)
     OR (SerchPoint.X > Max_X_Width)
     OR (SerchPoint.Y < NextPoint.Y)
     OR (SerchPoint.Y > Y_Width) ) THEN GOTO L;
  IF (SerchPoint.X=NextPoint.X) AND (SerchPoint.Y=NextPoint.Y) THEN 
  BEGIN
    Start:=TRUE ;
    Found:=TRUE ;
  END
  ELSE
  BEGIN
    FoundColor:=GetPixel(SerchPoint.X,SerchPoint.Y) ;
    IF FoundColor = 0 THEN
    BEGIN
        FoundColor:= CalcMandel (SerchPoint,Zm) ;
        Putpixel (SerchPoint.X,SerchPoint.Y,FoundColor) ;
        IF Flag THEN PutPixel (SerchPoint.X,Max_Y_Width-SerchPoint.Y,
                              FoundColor) ;
    END ;
    IF FoundColor=LastColor THEN
    BEGIN
        IF ActualPoint.Y <> SerchPoint.Y THEN
        BEGIN
        IF SerchPoint.Y = MerkY THEN LineY[ActualPoint.Y]:=LineY[ActualPoint.Y]-1;
        MerkY:= ActualPoint.Y ;
        LineY[SerchPoint.Y]:=LineY[SerchPoint.Y]+1;
        END ;
        LineX[LineY[SerchPoint.Y],SerchPoint.Y]:=SerchPoint.X ;
        IF SerchPoint.Y > Ymax THEN Ymax:= SerchPoint.Y ;
        Found:=TRUE ;
        ActualPoint:=SerchPoint ;
    END;
    L:
    KK:=KK+1;
    IF KK > 8 THEN
    BEGIN
        Start:=TRUE ;
        NotFound:=TRUE ;
    END;
  END;
UNTIL Found OR (KK > 8);
UNTIL Start ;

IF not NotFound THEN Fill(NextPoint.Y,Ymax,LastColor) ;
UNTIL NOT BlackScan(NextPoint);
END ;
{------------------------------------------------------------------------------}
                {-----------------------}
                {      MAINROUTINE      }
                {-----------------------}

BEGIN
gm:=$103;
gd:=$ff;
{$IFDEF TURBO}
gd:=detect;
{$ENDIF}
InitGraph(gd,gm,'D:\bp\bgi');
IF GraphResult <> grOk THEN Halt(1);
Max_X_Width:=GetMaxX;
Max_y_Width:=GetMaxY;
Max_Color:=GetMaxColor-1;
ClearViewPort;

x1:=-0.9;
x2:= 2.2;
y1:= 1.25;
y2:=-1.25;
zm:=90;
dx:=(x1 - x2) / Max_X_Width ;
dy:=(y1 - y2) / Max_Y_Width ;

IF ABS(y1) = ABS(y2) THEN
BEGIN
flag:=TRUE ; Y_Width:=Max_Y_Width shr 1;
END
ELSE
BEGIN
flag:=FALSE ; Y_Width:=Max_Y_Width;
END;
NextPoint.X:=0; NextPoint.Y:=0;
LastColor:=CalcMandel(SerchPoint,zm);
CalcBounds ;
readln;
CloseGraph;
END.
