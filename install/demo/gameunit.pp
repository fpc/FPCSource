{
    $Id$

    A simple unit with some common used routines for FPCGames (FpcTris and
      SameGame)

    Contains
     - Highscore routines "developped" for FPCTris, but now also used by SameGame
     - "Dummy" mouse routines which either shell to API units or to MSMouse.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
UNIT GameUnit;

INTERFACE

{MouseAPI defined : unit unes API mouse units, which requires that package,
                    but also works under Linux
 MouseAPI undef   : RTL unit MsMouse. API not required, but doesn't work under
                    Linux }

{$UNDEF MouseAPI}

TYPE CHARSET=SET OF CHAR;

{----   Unified Mouse procedures. ---- }

FUNCTION MousePresent : BOOLEAN;

PROCEDURE HideMouse;
PROCEDURE ShowMouse;
PROCEDURE GetMouseState(VAR MX,MY,MState : LONGINT);
PROCEDURE DoneMouse;
PROCEDURE InitMouse;


Const LButton = 1; {left button}
      RButton = 2; {right button}
      MButton = 4; {middle button}


{---- Standard Highscore procedures ----}

TYPE  HighScoreType   = Packed RECORD
                        Name : String[12];
                        Score: LONGINT;
                       END;
     HighScoreArr    = ARRAY[0..9] OF HighScoreType;

VAR HighScore   : HighScoreArr;
    ScorePath   : String;
    HighX,HighY : LONGINT;

PROCEDURE LoadHighScore(FileName:STRING);
PROCEDURE SaveHighScore;
PROCEDURE ShowHighScore;
FUNCTION  SlipInScore(Score:LONGINT):LONGINT;

{---- Keyboard routines ----}

CONST {Constants for GetKey}
   ArrU   = $04800;    ArrL   = $04B00;    ArrR   = $04D00;   BS  = $08;  (* Backspace *)
   ArrD   = $05000;    CR     = $0D;       ESC    = $1B;      KDelete= $05300;
   KInsert= $05200;    Home   = $04700;    KEnd   = $04F00;   CtrlY = $19;
   CtrlT = $14;

CONST FieldSpace : CHAR = #177;
      AlfaBeta : CHARSET= [' '..'z'];

FUNCTION GetKey:LONGINT;

{Generic string input routine}
FUNCTION  InputStr(VAR S:String;X,Y,Len:LONGINT;TextIn:BOOLEAN;CharAllow:CHARSET):BOOLEAN;

{---- Misc ----}

PROCEDURE SetDefaultColor; {Restore the attribs saved on startup}

IMPLEMENTATION

{$IFDEF MouseAPI}
 Uses Mouse,Dos,Crt;
{$ELSE}
 Uses MsMouse,Dos,Crt;
{$ENDIF}

VAR  DefColor    : BYTE;                         {Backup of startup colors}


CONST

{The initial names. If people feel they are missing, I first checked the Alias,
  and then filled with names of the FPC-Devel list, and arranged them alfabetically}
  InitNames : ARRAY[0..9] OF String[12] = ('Carl','Daniel','Florian','Jonas','Lee','Marco','Michael (3x)',
                                           'Peter','Pierre','Thomas' );

{$IFDEF MouseAPI}

VAR MouseBuffer : LONGINT;
{$ENDIF}

FUNCTION MousePresent : BOOLEAN;

BEGIN
 {$IFDEF MouseAPI}
  MousePresent:=DetectMouse<>0;
 {$ELSE}
  MousePresent:=MouseFound;
 {$ENDIF}
END;

PROCEDURE ShowMouse;

BEGIN
  {$IFDEF MouseAPI}
  Mouse.ShowMouse;
 {$ELSE}
  MsMouse.ShowMouse;
 {$ENDIF}
END;

PROCEDURE HideMouse;

BEGIN
 {$IFDEF MouseAPI}
  Mouse.HideMouse;
 {$ELSE}
  MsMouse.HideMouse;
 {$ENDIF}
END;

PROCEDURE InitMouse;

BEGIN
 {$IFDEF MouseAPI}
  Mouse.InitMouse;
 {$ELSE}
  MsMouse.InitMouse;
 {$ENDIF}
END;

PROCEDURE DoneMouse;

BEGIN
 {$IFDEF MouseAPI}
  Mouse.DoneMouse;
 {$ENDIF}
END;

PROCEDURE GetMouseState(VAR MX,MY,MState : LONGINT);

  {$IFDEF MouseAPI}
   VAR MouseEvent : TMouseEvent;
  {$ENDIF}

BEGIN
  {$IFDEF MouseAPI}
   GetMouseEvent(MouseEvent);
   MX:=MouseEvent.X SHL 3;
   MY:=MouseEvent.Y SHL 3;
   MState:=MouseEvent.Buttons;
 {$ELSE}
  MsMouse.GetMouseState(MX,MY,MState);
 {$ENDIF}
END;


Procedure LoadHighScore(FileName:STRING);

var
 F: File;
 I : LONGINT;

BEGIN
 {$I-}
 Assign(F, FileName);
 FileMode := 0;  {Set file access to read only }
 Reset(F);
 Close(F);
 {$I+}
 IF IOResult=0 THEN
  ScorePath:=FileName
 ELSE
  ScorePath:=FSearch(FileName,GetEnv('PATH'));
 IF ScorePath='' THEN
  BEGIN
   FOR I:=0 TO 9 DO
    BEGIN
     HighScore[I].Name:=InitNames[I];
     HighScore[I].Score:=(I+1)*750;
    END;
   ScorePath:=FileName;
  END
 ELSE
  BEGIN
   Assign(F,ScorePath);
   Reset(F,1);
   BlockRead(F,HighScore,SIZEOF(HighScoreArr));
   Close(F);
  END;
END;

Procedure SaveHighScore;

var
 F: File;

BEGIN
 Assign(F,ScorePath);
 Rewrite(F,1);
 BlockWrite(F,HighScore,SIZEOF(HighScoreArr));
 Close(F);
END;

FUNCTION  SlipInScore(Score:LONGINT):LONGINT;

VAR I,J : LONGINT;

BEGIN
 I:=0;
 WHILE (Score>HighScore[I].Score) AND (I<10) DO
  INC(I);
 IF I<>0 THEN
  BEGIN
   IF I>1 THEN
    FOR J:=0 TO I-2 DO
     HighScore[J]:=HighScore[J+1];
    HighScore[I-1].Score:=Score;
    HighScore[I-1].Name:='';
  END;
 SlipInScore:=I;
END;

PROCEDURE ShowHighScore;

VAR I : LONGINT;

{HighX=40 HighY=9}

BEGIN
 GotoXY(HighX+5,9); Write('The Highscores');
 FOR I:=0 TO 9 DO
  BEGIN
   GotoXY(HighX,HighY+11-I);
   Write(HighScore[I].Name,' ':(13-Length(HighScore[I].Name)),' ',HighScore[I].Score:5);
  END;
END;

FUNCTION GetKey:LONGINT;

VAR InKey: LONGINT;

BEGIN
 InKey:=ORD(ReadKey);
 IF InKey=0 THEN InKey:=ORD(ReadKey) SHL 8;
 GetKey:=InKey;
END;

FUNCTION  InputStr(VAR S:String;X,Y,Len:LONGINT;TextIn:BOOLEAN;CharAllow:CHARSET):BOOLEAN;

{
  Input a string from keyboard, in a nice way,
   allowed characters are in CHARSET CharAllow, but several editting
   keys are always allowed, see CASE loop.

Parameters:

   X,Y       Coordinates field
   Len       Length field
   TextIn    S already filled?}

VAR
    InGev                     : LONGINT; { No. of chars inputted }
    Posi                      : LONGINT; { Cursorposition}
    Ins                       : BOOLEAN;  { Insert yes/no}
    Key                       : LONGINT; { Last key as ELib.GetKey
                                            code <255 if normal key,
                                            >256 if special/function
                                            key. See keys.inc}
    Uitg                      : String;    {The inputted string}
    Full                      : BOOLEAN;   { Is the string full? }
    EndVal                    : WORD;

PROCEDURE ReWr; { Rewrite the field, using Uitg}

VAR    I                         : LONGINT;  { Temporary variabele }

BEGIN
 IF Length(Uitg)>Len THEN
  Uitg[0]:=CHR(Len);
 IF Length(Uitg)>0 THEN
  FOR I:= 1 TO Length(Uitg) DO
   BEGIN
    GotoXY(X+I-1,Y);
    IF Uitg[I]=CHR(32) THEN
     Write(FieldSpace)
    ELSE
     Write(Uitg[I]);
   END;
 IF Len<>Length(Uitg) THEN
  BEGIN
   GotoXY(X+Length(Uitg),Y);
   FOR I:= Length(Uitg) TO Len-1 DO
    Write(FieldSpace);
  END;
END;

PROCEDURE DoCursor; { Put Cursor in/out insert-mode }

BEGIN
 {$IFNDEF Linux}
{ IF Ins THEN
  SetCursorSize($11E)
 ELSE
  SetCursorSize($71E); }
 {$ENDIF}

END;

BEGIN
    { Init }

  InGev :=0;              { 0 chars untill now }
  Posi  :=1;               { Cursorposition 0 }
  Ins   :=TRUE;            { Insert according to parameters }
  DoCursor;        { Set cursor accordingly }
  Key   :=0;

       { put ±±± padded field on screen }

  FillChar(Uitg,Len+1,FieldSpace);
  Uitg[0]:=CHR(Len);
  ReWr;
  GotoXY(X,Y);

  FillChar(Uitg,Len,32);
  UitG[0]:=#0;

  IF TextIn THEN
   BEGIN
    Uitg:=S;
    Posi:=Length(Uitg)+1;                        { Put a predefined }
    ReWr;                                   {  String on screen if specified }
   END;

  EndVal:=0;
  WHILE EndVal=0 DO
   BEGIN
    Full:=FALSE;
    IF ((Posi)>=Len) THEN
     BEGIN
      Full:=TRUE;
      Posi:=Len;
     END;
    GotoXY(X+Posi-1,Y);
    {$IFNDEF Linux}
     {$IFDEF FPC}
      CursorOn;
     {$ENDIF}
    DoCursor;
    {$ENDIF}
    Key:=GetKey;
   {$IFNDEF Linux}
    {$IFDEF FPC}
    CursorOff;
    {$ENDIF}
   {$ENDIF}
    CASE Key OF
          CR              : BEGIN
                             EndVal:=1;
                             S:=UitG;
                            END;
          ESC             : EndVal:=2;
          BS              : IF Posi>1 THEN       { BackSpace }
                              BEGIN
                               DEC(Posi);
                               Delete(Uitg,Posi,1);
                               DEC(InGev);
                               ReWr;
                              END;
          KDelete          : BEGIN
                              Delete(Uitg,Posi,1);
                              DEC(InGev);
                              ReWr;
                             END;
          ArrR            : IF (NOT Full) AND ((Posi-1)<InGev) THEN
                              BEGIN
                               INC (Posi);
                               GotoXY(X+Posi-1,Y);
                               END;
          KInsert          : BEGIN
                               Ins:= NOT Ins;
                               DoCursor;
                              END;
          ArrL            : IF (NOT (Posi=1)) THEN
                              BEGIN
                               DEC (Posi);
                               GotoXY(X+Posi-1,Y);
                              END;
          Home            : Posi:=1;
          KEnd            : Posi:=InGev-1;
          CtrlY           : BEGIN
                             Delete(Uitg,Posi,Length(Uitg)-Posi);
                             ReWr;
                            END;
          CtrlT           : BEGIN
                             Uitg[0]:=#0; Posi:=1; ReWr;
                            END;
    END; {Case}
   IF EndVal=0 THEN
    BEGIN
     IF (CHR(Key) IN CharAllow) THEN
      BEGIN
       IF Posi>Len THEN
        Posi:=Len;
       IF (Ins=FALSE) OR Full THEN
        BEGIN
         IF (ORD(Uitg[0])<Posi) THEN
           Uitg[0]:=CHR(Posi);
         Uitg[Posi]:=CHR(Key);
        END
       ELSE
        BEGIN
         Insert(CHR(Key),Uitg,Posi);
{         InsertC(uitg,CHR(Key),Posi);}
        END;
       ReWr;
       INC(Posi);
      END;
     END;
    InGev:=Length(Uitg);
   END;
  InputStr:=Endval=1;
END;

PROCEDURE SetDefaultColor;

BEGIN
 TextColor(DefColor AND 15);
 TextBackground(DefColor SHR 4);
END;

BEGIN
 {$IFDEF MouseAPI}
  MouseBuffer:=0;
 {$ENDIF}
  DefColor:=TextAttr;                { Save the current attributes, to restore}
END.
{
  $Log$
  Revision 1.1  1999-06-01 19:24:33  peter
    * updates from marco

}
