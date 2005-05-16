{
    $Id: gameunit.pp,v 1.9 2004/06/21 07:01:34 marco Exp $

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


{$ifdef Unix}
  {$define MouseAPI}
{$endif}
{$ifdef win32}
  {$define MouseAPI}
  {$define UseGraphics} {Mandatory}
{$endif}
{$IFDEF Ver70}
  {$define MouseAPI}
  {$G+}
{$endif}
{$IFDEF Ver60}
  {$define MouseAPI}
  {$G+}
{$endif}
{$IFDEF Ver55}
  {$define MouseAPI}
  {$G+}
{$endif}
{$ifdef UseGraphics}
 {$ifdef Win32}
   {$define Win32Graph}
 {$endif}
{$endif}

CONST  LineDistY=13;


TYPE CHARSET=SET OF CHAR;

{----   Unified Mouse procedures. ---- }

FUNCTION MousePresent : BOOLEAN;

PROCEDURE HideMouse;
PROCEDURE ShowMouse;
PROCEDURE GetMouseState(VAR MX,MY,MState : LONGINT);
PROCEDURE DoneMouse;
PROCEDURE InitMouse;
PROCEDURE SetMousePosition(X,Y:LONGINT);


Const LButton = 1; {left button}
      RButton = 2; {right button}
      MButton = 4; {middle button}


{---- Standard Highscore procedures ----}

TYPE  HighScoreType   = Packed RECORD
                        Name : String[15];
                        Score: LONGINT;
                       END;
     HighScoreArr    = ARRAY[0..9] OF HighScoreType;

VAR HighScore   : HighScoreArr;
    ScorePath   : String;
    HighX,HighY : LONGINT;
    Negative    : BOOLEAN;      { Negative=true-> better scores are lower}

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
{$IFDEF UseGraphics}
FUNCTION  GrInputStr(VAR S:String;X,Y,Len,dX,dY:LONGINT;TextIn:BOOLEAN;CharAllow:CHARSET):BOOLEAN;
{$ELSE}
FUNCTION  InputStr(VAR S:String;X,Y,Len:LONGINT;TextIn:BOOLEAN;CharAllow:CHARSET):BOOLEAN;
{$ENDIF}

{---- Misc ----}

PROCEDURE SetDefaultColor; {Restore the attribs saved on startup}

{BP compability}

{$IFNDEF FPC}
PROCEDURE SetCursorSize(CurDat:WORD);
FUNCTION  GetCursorSize:WORD;
PROCEDURE CursorOn;
PROCEDURE CursorOff;

{Non Go32 but not existant in BP}
PROCEDURE FillWord(VAR Data;Count,Value:WORD);

PROCEDURE dosmemfillword(Segx,xofs,Count,Value:WORD);
PROCEDURE dosmemput(Segx,xofs:WORD;VAR Data;Count:WORD);
PROCEDURE dosmemget(Segx,xofs:WORD;VAR Data;Count:WORD);

FUNCTION  inportb(portx : word) : byte;
PROCEDURE outportb(portx : word;data : byte);

FUNCTION  inportw(portx : word) : word;
PROCEDURE outportw(portx : word;data : word);

FUNCTION  inportl(portx : word) : longint;
PROCEDURE outportl(portx : word;data : longint);
{$ENDIF}

IMPLEMENTATION

Uses

{$ifdef Win32Graph}
   WinMouse,
   {$undef MouseApi}
{$else}
 {$IFDEF MouseAPI}
   Mouse,
 {$ELSE}
   MSMouse,
 {$ENDIF}
{$endif}

{$ifdef UseGraphics}
  Graph,
{$endif}
{$ifdef Win32Graph}
  WinCrt,
{$else}
  Crt,
{$endif}
  Dos;

VAR  DefColor    : BYTE;                         {Backup of startup colors}

CONST

{The initial names. If people feel they are missing, I first checked the Alias,
  and then filled with names of the FPC-Devel list, and arranged them alfabetically}
  InitNames : ARRAY[0..9] OF String[12] = ('Carl','Daniel','Florian','Jonas','John','Marco','Michael (3x)',
                                           'Peter','Pierre','Thomas' );

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
 {$ifdef Win32Graph}
  WinMouse.ShowMouse;
 {$else}
  {$IFDEF MouseAPI}
   Mouse.ShowMouse;
  {$ELSE}
   MsMouse.ShowMouse;
  {$ENDIF}
 {$endif}
END;

PROCEDURE HideMouse;

BEGIN
 {$ifdef Win32Graph}
  WinMouse.HideMouse;
 {$else}
  {$IFDEF MouseAPI}
   Mouse.HideMouse;
  {$ELSE}
   MsMouse.HideMouse;
  {$ENDIF}
 {$endif}
END;

PROCEDURE InitMouse;

BEGIN
 {$ifdef Win32Graph}
  WinMouse.InitMouse;
 {$else}
  {$IFDEF MouseAPI}
   Mouse.InitMouse;
  {$ELSE}
   MsMouse.InitMouse;
  {$ENDIF}
 {$endif}
END;

PROCEDURE DoneMouse;

BEGIN
 {$ifdef Win32Graph}
 {$else}
  {$IFDEF MouseAPI}
   Mouse.DoneMouse;
  {$ENDIF}
 {$endif}
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
  {$ifdef Win32Graph}
   WinMouse.GetMouseState(MX,MY,MState);
  {$else}
   MsMouse.GetMouseState(MX,MY,MState);
  {$endif}
 {$ENDIF}
END;

PROCEDURE SetMousePosition(X,Y:LONGINT);

BEGIN
 {$ifndef Win32Graph}
 {$IFDEF MouseAPI}
  SetMouseXY(x,y);
 {$ELSE}
  SetMousePos(X,Y);
  {$endif}
 {$ENDIF}
END;

Procedure LoadHighScore(FileName:STRING);

var
 F: File;
 I : LONGINT;
 OFileMode : LONGINT;

BEGIN
 {$I-}
 Assign(F, FileName);
 OFileMode:=FileMode;
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
     If Negative Then
      HighScore[I].Score:=-100*(10-I)
     Else
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
 FileMode:=OFileMode;
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
 IF Negative THEN
  Score:=-Score;
 I:=0;
 WHILE (i<10) and (Score>HighScore[I].Score) DO
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

{$IFDEF UseGraphics}

PROCEDURE ShowHighScore;

VAR I : LONGINT;
    S : String;

BEGIN
 SetFillStyle(SolidFill,0);            {Clear part of playfield}
 Bar(HighX,HighY, 638, HighY+20+18*LineDistY);
 FOR I:=0 TO 9 DO
  BEGIN
   OutTextXY(HighX,HighY+(9-I)*LineDistY,HighScore[I].Name);
   IF Negative THEN
    Str((-HighScore[I].Score):5,S)
   ELSE
    Str(HighScore[I].Score:5,S);
   OutTextXY(HighX+150,HighY+(9-I)*LineDistY,S);
  END;
END;

{$ELSE}
PROCEDURE ShowHighScore;

VAR I : LONGINT;

{HighX=40 HighY=9}

BEGIN
 GotoXY(HighX+5,9); Write('The Highscores');
 FOR I:=0 TO 9 DO
  BEGIN
   GotoXY(HighX,HighY+11-I);
   Write(HighScore[I].Name,' ':(13-Length(HighScore[I].Name)),' ');
   IF NOT Negative THEN     { Negative=true-> better scores are lower}
    Write(HighScore[I].Score:5)
   ELSE
    Write(-HighScore[I].Score:5)
  END;
END;
{$ENDIF}

FUNCTION GetKey:LONGINT;

VAR InKey: LONGINT;

BEGIN
 InKey:=ORD(ReadKey);
 IF InKey=0 THEN InKey:=ORD(ReadKey) SHL 8;
 GetKey:=InKey;
END;

{$IFNDEF UseGraphics}
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
 {$IFNDEF Unix}
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
    {$IFNDEF Unix}
     {$IFDEF FPC}
       {$ifndef Win32Graph}
        CursorOn;
       {$endif}
     {$ENDIF}
    DoCursor;
    {$ENDIF}
    Key:=GetKey;
   {$IFNDEF Unix}
    {$IFDEF FPC}
     {$ifndef Win32Graph}
      CursorOff;
     {$endif}
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
        END;
       ReWr;
       INC(Posi);
      END;
     END;
    InGev:=Length(Uitg);
   END;
  InputStr:=Endval=1;
END;
{$ENDIF}

{$IFDEF UseGraphics}
FUNCTION  GrInputStr(VAR S:String;X,Y,Len,dX,dY:LONGINT;TextIn:BOOLEAN;CharAllow:CHARSET):BOOLEAN;
{As the (older) textversion except:
    -  oX,oY are in pixels.
    -  dX,dY are the dimensions of the font.
    -  Len is still characters ( length in pixels/dX)
}


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
       S                         : String;

BEGIN
 FillChar(S[1],Len,FieldSpace);
 S:=Uitg;
 IF Length(Uitg)>Len THEN
  SetLength(Uitg,Len);
 SetLength(S,Len);
 IF Length(S)>0 THEN
  BEGIN
   FOR I:= 1 TO Length(S) DO
    IF S[I]=CHR(32) THEN
     S[I]:=FieldSpace;
   SetFillStyle(SolidFill,0);
   Bar(X,Y,X+Len*Dx+5,Y+Dy+1);
   OutTextXY(X,Y,S);
  END;
END;

PROCEDURE DoCursor; { Put Cursor in/out insert-mode }

BEGIN
 {$IFNDEF Unix}
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
//  SetFillStyle(SolidFill,0);
//  Bar(X,Y,X+Len*Dx+5,Y+Dy+1);
       { put ±±± padded field on screen }

  FillChar(Uitg,Len+1,FieldSpace);
  Uitg[0]:=CHR(Len);
  ReWr;
//  GotoXY(X,Y);
  FillChar(Uitg,Len,32);
  SetLength(UitG,0);

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
    {$IFNDEF Unix}
     {$IFDEF FPC}
      {$ifndef Win32Graph}
       CursorOn;
      {$endif}
     {$ENDIF}
    DoCursor;
    {$ENDIF}
    Key:=GetKey;
   {$IFNDEF Unix}
    {$IFDEF FPC}
     {$ifndef Win32Graph}
      CursorOff;
     {$endif}
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
  //                             GotoXY(X+Posi-1,Y);
                               END;
          KInsert          : BEGIN
                               Ins:= NOT Ins;
                               DoCursor;
                              END;
          ArrL            : IF (NOT (Posi=1)) THEN
                              BEGIN
                               DEC (Posi);
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
         IF (Length(Uitg)<Posi) THEN
          SetLength(UitG,Posi);
         Uitg[Posi]:=CHR(Key);
        END
       ELSE
         Insert(CHR(Key),Uitg,Posi);
       ReWr;
       INC(Posi);
      END;
     END;
    InGev:=Length(Uitg);
   END;
  GrInputStr:=Endval=1;
END;
{$ENDIF}

PROCEDURE SetDefaultColor;

BEGIN
 {$ifndef UseGraphics}
  TextColor(DefColor AND 15);
  TextBackground(DefColor SHR 4);
 {$endif}
END;


{$IFNDEF FPC}
PROCEDURE SetCursorSize(CurDat:WORD);ASSEMBLER;
ASM
  mov ah,1
  mov cx,CurDat
  int $10
END;

{The two procedures below are standard (and os-independant) in FPC's Crt}
PROCEDURE CursorOn;
BEGIN
  SetCursorSize($090A);
END;

PROCEDURE CursorOff;
BEGIN
  SetCursorSize($FFFF);
END;

PROCEDURE dosmemfillword(Segx,xofs,Count,Value:WORD); ASSEMBLER;
{VAR A:WORD;
BEGIN
  FOR A :=0 TO Count-1 DO
    MemW[Seg:xofs+2*A]:=Value;
END;
}
ASM
  mov  ax,segx
  mov  es,ax
  mov  di,xofs
  mov  cx,count
  mov  ax,value
  rep
    stosw
end;

{TYPE VetteArray=ARRAY[0..9999] OF BYTE;}

PROCEDURE dosmemput(Segx,xofs:WORD;VAR Data;Count:WORD); assembler;
{VAR A:WORD;
    L:^VetteArray;
BEGIN
  L:=@Data;
  FOR A :=0 TO Count-1 DO
    Mem[Segx:xofs+A]:=L^[A];
END;
}
asm
  lds si,Data
  mov ax,segx
  mov es,ax
  mov di,xofs
  mov cx,count
  rep
    movsw
end;

PROCEDURE dosmemget(Segx,xofs:WORD;VAR Data;Count:WORD); ASSEMBLER;
{VAR A:WORD;
    L:^VetteArray;
BEGIN
  L:=@Data;
  FOR A :=0 TO Count-1 DO
    L^[A]:=Mem[Segx:xofs+A];
END;
}
asm
  les di,Data
  mov ax,segx
  mov ds,ax
  mov si,xofs
  mov cx,count
  rep
    movsw
end;

PROCEDURE FillWord(VAR Data;Count,Value:WORD); ASSEMBLER;
{VAR A :WORD;
    L:^VetteArray;
BEGIN
  L:=@Data;
  FOR A:=0 TO Count-1 DO
  Begin
    L^[2*A]:=Value AND 255;
    L^[2*A+1]:=Value shr 8;
  END;
END;}

asm
  les di,Data
  mov cx,count
  mov ax,Value
  rep
    movsw
end;

FUNCTION GetCursorSize:WORD;ASSEMBLER;
ASM
  mov ah,3
  xor bh,bh
  int $10
  mov ax,cx
END;

FUNCTION  inportb(portx : word) : byte;
BEGIN
  Inportb:=Port[PortX];
END;

PROCEDURE outportb(portx : word;data : byte);
BEGIN
  Port[portx]:=Data;
END;

FUNCTION  inportw(portx : word) : word;
BEGIN
  Inportw:=Portw[PortX];
END;

PROCEDURE outportw(portx : word;data : word);
BEGIN
  PortW[portx]:=Data;
END;

 FUNCTION  inportl(portx : word) : longint; ASSEMBLER;
 ASM
   mov dx,portx                   { load port address }
   db $66; in  ax,dx              { in  eax,dx }
   db $66; mov dx,ax              { mov edx, eax }
   db $66; shr dx,16              { shr edx, 16 }
   { return: ax=low word, dx=hi word }
 END;

 PROCEDURE  outportl(portx : word;data : longint); ASSEMBLER;
 ASM
   { we cant use the 32 bit operand prefix for loading the longint -
     therefore we have to do that in two chunks }
     mov dx, portx
     db $66; mov ax, Word(Data)  { mov eax, Data }
   db $66; out dx,ax              { out dx, eax }
 END;

{$ENDIF}

BEGIN
 {$ifndef Win32Graph}
  DefColor:=TextAttr;                { Save the current attributes, to restore}
 {$endif}
  Negative:=FALSE;                    { Negative=true-> better scores are lower}
END.
{
  $Log: gameunit.pp,v $
  Revision 1.9  2004/06/21 07:01:34  marco
   * 1st and 3rd recommendation of bug 3177

  Revision 1.8  2002/09/07 15:06:35  peter
    * old logs removed and tabs fixed

  Revision 1.7  2002/06/02 17:34:21  marco
   * Renamefest

  Revision 1.6  2002/06/02 09:49:17  marco
   * Renamefest

  Revision 1.5  2002/02/25 12:23:05  marco
   * Fixes for Quad Win32 GUI mode

  Revision 1.4  2002/02/22 21:40:09  carl
  * fix compilation problem

}
