{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1996-2000 by Berczi Gabor

    ANSI support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{.$DEFINE DEBUG}
unit WANSI;

interface

uses Objects,Drivers,
{$ifdef WITH_CRT}
     Crt,
{$endif WITH_CRT}
     Dos,Views,App;

const
{$ifndef WITH_CRT}
{ Foreground and background color constants }
  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

{ Foreground color constants }
  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;

{ Add-in for blinking }
  Blink         = 128;
{$endif not WITH_CRT}

      ANSIMaxParamLen     = 30; { max ANSI escape sequence length }
      ANSICurPosStackSize = 20; { max number of cursor positions stored at the same time }

      Esc = #27;

      { BoundCheck constants }
      bc_MinX    = 1;
      bc_MinY    = 2;
      bc_MaxX    = 4;
      bc_MaxY    = 8;
      bc_X       = bc_MinX or bc_MaxX;
      bc_Y       = bc_MinY or bc_MaxY;
      bc_Min     = bc_MinX or bc_MinY;
      bc_Max     = bc_MaxX or bc_MaxY;
      bc_All     = bc_X or bc_Y;

type
     TANSIParam = string[ANSIMaxParamLen];

     PHookProc = ^THookProc;
     THookProc = procedure (S: string);

     PConsoleObject = ^TConsoleObject;
     TConsoleObject = object(TObject)
       CurPos   : TPoint;
       Size     : TPoint;
       TextAttr : byte;
       BoldOn   : boolean;
       BlinkOn  : boolean;
       BoundChecks: byte;
       LineWrapping: boolean;
       ReplyHook   : PHookProc;
       KeyHook     : PHookProc;
       WriteHook   : PHookProc;
       constructor Init(AReplyHook, AKeyHook, AWriteHook: PHookProc);
       procedure   Home; virtual;
       procedure   ClrScr; virtual;
       procedure   FillScreen(B: byte); virtual;
       procedure   ClrEol; virtual;
       procedure   GotoXY(X,Y: integer); virtual;
       procedure   Write(Const S: string); virtual;
       procedure   WriteLn(Const S: string); virtual;
       procedure   WriteChar(C: char); virtual;
       procedure   WriteCharRaw(C: char); virtual;
       procedure   DelLine(LineCount: integer); virtual;
       procedure   InsLine(LineCount: integer); virtual;
       procedure   HighVideo; virtual;
       procedure   BlinkVideo; virtual;
       procedure   NoBlinkVideo; virtual;
       procedure   NormVideo; virtual;
       procedure   LowVideo; virtual;
       procedure   TextBackground(Color: byte); virtual;
       procedure   TextColor(Color: byte); virtual;
       function    WhereX: integer; virtual;
       function    WhereY: integer; virtual;
       procedure   CursorOn; virtual;
       procedure   CursorOff; virtual;
       procedure   UpdateCursor; virtual;
       { --- Hook procedures --- }
       procedure   Reply(S: string); virtual;
       procedure   PutKey(S: string); virtual;
       destructor  Done; virtual;
       private
       procedure   ProcessChar(C: char); virtual;
     end;

     PANSIConsole = ^TANSIConsole;
     TANSIConsole = object(TConsoleObject)
       ANSIParam          : TANSIParam;
       ANSILevel          : byte;
       ANSICurPosStack    : array[1..ANSICurPosStackSize] of TPoint;
       ANSICurPosStackPtr : byte;
       constructor Init(AReplyHook, AKeyHook, AWriteHook: PHookProc);
       procedure   ProcessChar(C: char); virtual;
       function    GetANSIParam: integer; virtual;
       { --- ANSI functions --- }
       procedure   PushCurPos; virtual;
       procedure   PopCurPos; virtual;
       procedure   CursorUp(LineCount: integer); virtual;
       procedure   CursorDown(LineCount: integer); virtual;
       procedure   CursorForward(CharCount: integer); virtual;
       procedure   CursorBack(CharCount: integer); virtual;
       procedure   SetAttr(Color: integer); virtual;
     end;

{$ifdef WITH_CRT}
     PCrtConsole = ^TCrtConsole;
     TCrtConsole = object(TANSIConsole)
       constructor Init(AReplyHook, AKeyHook, AWriteHook: PHookProc);
       procedure   CursorOn; virtual;
       procedure   CursorOff; virtual;
       procedure   ClrScr; virtual;
       procedure   ClrEol; virtual;
       procedure   WriteChar(C: char); virtual;
       procedure   DelLine(LineCount: integer); virtual;
       procedure   InsLine(LineCount: integer); virtual;
       procedure   UpdateCursor; virtual;
       procedure   TextBackground(Color: byte); virtual;
       procedure   TextColor(Color: byte); virtual;
     end;
{$endif WITH_CRT}

const
      MaxVideoLine = 65520 div (2*MaxViewWidth); { maximum number of lines that fit in 64K }

type
     TAnsiBuffer = array[0..MaxViewWidth*MaxVideoLine] of word;
     PAnsiBuffer = ^TAnsiBuffer;

     PANSIView = ^TANSIView;

     PANSIViewConsole = ^TANSIViewConsole;
     TANSIViewConsole = object(TANSIConsole)
       Owner : PANSIView;
       constructor Init(AOwner: PANSIView);
       procedure   CursorOn; virtual;
       procedure   CursorOff; virtual;
       procedure   ClrScr; virtual;
       procedure   ClrEol; virtual;
       procedure   WriteChar(C: char); virtual;
       procedure   WriteCharRaw(C: char); virtual;
       procedure   DelLine(LineCount: integer); virtual;
       procedure   InsLine(LineCount: integer); virtual;
       procedure   UpdateCursor; virtual;
       procedure   GotoXY(X,Y: integer); virtual;
     end;

     TANSIView = object(TScroller)
       Console : PANSIViewConsole;
       Buffer  : PAnsiBuffer;
       LockCount : word;
       constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:PScrollBar);
       function    LoadFile(const FileName: string): boolean;
       procedure   Draw; virtual;
       destructor  Done; virtual;
       procedure   Write(Const S: string); virtual;
       procedure   WriteLn(Const S: string); virtual;
       procedure   Lock; virtual;
       procedure   UnLock; virtual;
       procedure   ChangeBounds(var Bounds: TRect); virtual;
       procedure   HandleEvent(var Event: TEvent); virtual;
       private
     end;

     PANSIBackground = ^TANSIBackground;

     PANSIBackgroundConsole = ^TANSIBackgroundConsole;
     TANSIBackgroundConsole = object(TANSIConsole)
       Owner : PANSIBackground;
       constructor Init(AOwner: PANSIBackground);
       procedure   CursorOn; virtual;
       procedure   CursorOff; virtual;
       procedure   ClrScr; virtual;
       procedure   ClrEol; virtual;
       procedure   WriteChar(C: char); virtual;
       procedure   DelLine(LineCount: integer); virtual;
       procedure   InsLine(LineCount: integer); virtual;
       procedure   UpdateCursor; virtual;
       procedure   GotoXY(X,Y: integer); virtual;
     end;

     TANSIBackground = object(TBackground)
       Console : PANSIBackgroundConsole;
       Buffer  : TAnsiBuffer;
       LockCount : word;
       constructor Init(var Bounds: TRect);
       function    LoadFile(const FileName: string): boolean;
       procedure   Draw; virtual;
       destructor  Done; virtual;
       procedure   Write(Const S: string); virtual;
       procedure   WriteLn(Const S: string); virtual;
       procedure   Lock; virtual;
       procedure   UnLock; virtual;
       procedure   ChangeBounds(var Bounds: TRect); virtual;
       procedure   HandleEvent(var Event: TEvent); virtual;
       private
     end;

implementation

uses WUtils;

constructor TConsoleObject.Init(AReplyHook, AKeyHook, AWriteHook: PHookProc);
begin
  inherited Init;
  ReplyHook:=AReplyHook; KeyHook:=AKeyHook; WriteHook:=AWriteHook;
  BoundChecks:=bc_All; LineWrapping:=true;
  TextColor(LightGray); TextBackground(Black);
  NormVideo;
  ClrScr;
end;

procedure TConsoleObject.Home;
begin
  GotoXY(1,1);
end;

procedure TConsoleObject.ClrScr;
begin
  Abstract;
end;

procedure TConsoleObject.FillScreen(B: byte);
var X,Y: integer;
    S  : string;
begin
  GotoXY(1,1);
  for Y:=1 to Size.Y do
      begin
        S:='';
        for X:=1 to Size.X do S:=S+chr(B);
        WriteLn(S);
      end;
end;

procedure TConsoleObject.ClrEol;
begin
  Abstract;
end;

procedure TConsoleObject.GotoXY(X,Y: integer);
begin
  if (BoundChecks and bc_MinX)<>0 then X:=Max(X,1);
  if (BoundChecks and bc_MaxX)<>0 then
     if LineWrapping then while (X>Size.X) and (Size.X<>0)
                                do begin
                                     Inc(Y);
                                     X:=X-Size.X;
                                   end
                     else X:=Min(X,Size.X);
  if (BoundChecks and bc_MinY)<>0 then Y:=Max(Y,1);
  if (BoundChecks and bc_MaxY)<>0 then Y:=Min(Y,Size.Y);
  CurPos.X:=X; CurPos.Y:=Y;
  UpdateCursor;
end;

procedure TConsoleObject.ProcessChar(C: char);
begin
  WriteChar(C);
end;

procedure TConsoleObject.WriteChar(C: char);
begin
  Abstract;
end;

procedure TConsoleObject.WriteCharRaw(C: char);
begin
  Abstract;
end;

procedure TConsoleObject.Write(Const S: string); {assembler;
asm
  push   ds
  lds    si, S
  lodsb
  xor    ah, ah
  mov    cx, ax
@loop:
  or     cx, cx
  je     @exitloop
  lodsb
  pop    ds
  push   ax
  call   ProcessChar
  push   ds
  dec    cx
  jmp    @loop
@exitloop:
  pop    ds
end;}
var Len: byte;
    I  : byte;
begin
  Len:=length(S);
  for I:=1 to Len do ProcessChar(S[I]);
end;

procedure TConsoleObject.WriteLn(Const S: string);
begin
  Write(S);Write(#10);
end;

procedure TConsoleObject.DelLine(LineCount: integer);
begin
  Abstract;
end;

procedure TConsoleObject.InsLine(LineCount: integer);
begin
  Abstract;
end;

procedure TConsoleObject.NormVideo;
begin
  BoldOn:=false; BlinkOn:=false;
  TextColor(LightGray);
  TextBackground(Black);
end;

procedure TConsoleObject.BlinkVideo;
begin
  BlinkOn:=true;
  TextBackground(TextAttr shr 4);
end;

procedure TConsoleObject.NoBlinkVideo;
begin
  BlinkOn:=false;
  TextAttr:=TextAttr and $7f;
  TextBackground(TextAttr shr 4);
end;

procedure TConsoleObject.HighVideo;
begin
  BoldOn:=true;
  TextColor(TextAttr);
end;

procedure TConsoleObject.LowVideo;
begin
  BoldOn:=false;
  TextAttr:=TextAttr and not $08;
  TextColor(TextAttr);
end;

procedure TConsoleObject.TextBackground(Color: byte);
begin
  TextAttr:=(TextAttr and $0f) or (Color shl 4) or byte(BlinkOn)*$80;
end;

procedure TConsoleObject.TextColor(Color: byte);
begin
  TextAttr:=((TextAttr and $f0) or (Color and $0f) or byte(BoldOn)*$08);
end;

function TConsoleObject.WhereX: integer;
begin
  WhereX:=CurPos.X;
end;

function TConsoleObject.WhereY: integer;
begin
  WhereY:=CurPos.Y;
end;

procedure TConsoleObject.CursorOn;
begin
  Abstract;
end;

procedure TConsoleObject.CursorOff;
begin
  Abstract;
end;

procedure TConsoleObject.UpdateCursor;
begin
  Abstract;
end;

procedure TConsoleObject.Reply(S: string);
begin
  if ReplyHook<>nil then ReplyHook^(S);
end;

procedure TConsoleObject.PutKey(S: string);
begin
  if KeyHook<>nil then KeyHook^(S);
end;

destructor TConsoleObject.Done;
begin
  inherited Done;
end;

{$ifdef WITH_CRT}
constructor TCrtConsole.Init(AReplyHook, AKeyHook, AWriteHook: PHookProc);
begin
  inherited Init(AReplyHook, AKeyHook, AWriteHook);
  Size.X:=Lo(Crt.WindMax); Size.Y:=Hi(Crt.WindMax);
end;

procedure TCrtConsole.CursorOn;
begin
end;

procedure TCrtConsole.CursorOff;
begin
end;

procedure TCrtConsole.ClrScr;
begin
  Crt.ClrScr;
  GotoXY(Crt.WhereX,Crt.WhereY);
end;

procedure TCrtConsole.ClrEol;
begin
  Crt.ClrEol;
  GotoXY(Crt.WhereX,Crt.WhereY);
end;

procedure TCrtConsole.WriteChar(C: char);
{var OK: boolean;}
begin
{  OK:=((C>=#32) and (WhereX<Size.X)) or (C<#32);
  if OK then
  begin}
  System.Write(C);
  GotoXY(Crt.WhereX,Crt.WhereY);
{  end
  else Inc(CurPos.X);}
end;

procedure TCrtConsole.DelLine(LineCount: integer);
var I: integer;
begin
  for I:=1 to LineCount do Crt.DelLine;
end;

procedure TCrtConsole.InsLine(LineCount: integer);
var I: integer;
begin
  for I:=1 to LineCount do Crt.InsLine;
end;

procedure TCrtConsole.UpdateCursor;
begin
  Crt.GotoXY(CurPos.X,CurPos.Y);
end;

procedure TCrtConsole.TextBackground(Color: byte);
begin
  inherited TextBackground(Color);
  Crt.TextAttr:=TextAttr;
end;

procedure TCrtConsole.TextColor(Color: byte);
begin
  inherited TextColor(Color);
  Crt.TextAttr:=TextAttr;
end;
{$endif WITH_CRT}

constructor TANSIConsole.Init(AReplyHook, AKeyHook, AWriteHook: PHookProc);
begin
  inherited Init(AReplyHook, AKeyHook, AWriteHook);
  BoundChecks:=bc_MaxX;
  ANSIParam:=''; ANSILevel:=0; ANSICurPosStackPtr:=0;
end;

procedure TANSIConsole.ProcessChar(C: char);
var SkipThis : boolean;
    ANSIDone : boolean;
    X,Y,Z    : integer;
begin
  SkipThis:=false;
  if C=Esc then
    begin
       { Treat EscEsc as a request to print a single Escape #27 char PM }
      if AnsiLevel=0 then
        begin
          ANSILevel:=1;
          SkipThis:=true;
        end
      else
        begin
          AnsiLevel:=0;
          WriteCharRaw(c);
          SkipThis:=true;
        end;
    end
  else if (ANSILevel=1) then
     begin
       ANSILevel:=0;
       case C of
            '[' : begin
                    ANSILevel:=2;
                    SkipThis:=true;
                  end;
       else
       { Treat Esc+ AnyChar as a request to print that single char raw PM }
         begin
           WriteCharRaw(c);
           SkipThis:=true;
         end;
       end;
     end;

  if SkipThis=false then
  if (ANSILevel=2)
     then begin
            ANSIDone:=true;
            case C of
                 'H','f' : if ANSIParam='' then GotoXY(1,1) else
                           begin
                             X:=WhereX; Y:=WhereY;
                             Z:=Pos(';',ANSIParam);
                             if Z=0
                                then Y:=GetANSIParam
                                else if Z=1 then X:=GetANSIParam
                                            else begin Y:=GetANSIParam; X:=GetANSIParam; end;
                             GotoXY(X,Y);
                           end;
                 'A'     : if ANSIParam='' then CursorUp(1)
                                           else CursorUp(GetANSIParam);
                 'B'     : if ANSIParam='' then CursorDown(1)
                                           else CursorDown(GetANSIParam);
                 'C'     : if ANSIParam='' then CursorForward(1)
                                           else CursorForward(GetANSIParam);
                 'D'     : if ANSIParam='' then CursorBack(1)
                                           else CursorBack(GetANSIParam);
                 's'     : if ANSIParam='' then PushCurPos;
                 'u'     : if ANSIParam='' then PopCurPos;
                 'J'     : if ANSIParam='2' then begin ANSIParam:=''; ClrScr; end
                                            else FillScreen(GetANSIParam);
                 'K'     : if ANSIParam='' then ClrEol;
                 'L'     : if ANSIParam='' then InsLine(1)
                                           else InsLine(GetANSIParam);
                 'M'     : if ANSIParam='' then DelLine(1)
                                           else DelLine(GetANSIParam);
                 'm'     : while ANSIParam<>'' do SetAttr(GetANSIParam);
            else
              begin
                {ANSIParam:=ANSIParam+C;}
                System.Insert(C,AnsiParam,Length(AnsiParam)+1);
                ANSIDone:=false;
              end;
            end;
            if ANSIDone then
               begin
{$IFDEF DEBUG}
                 if ANSIParam<>'' then RunError(240);
{$ENDIF}
                 ANSIParam:=''; ANSILevel:=0;
               end;
          end
     else begin
            WriteChar(C);
            if C=#10 then WriteChar(#13);
          end;
end;

function TANSIConsole.GetANSIParam: integer;
var P: byte;
    I,C: integer;
begin
  P:=Pos(';',ANSIParam);
  if P=0 then P:=length(ANSIParam)+1;
  Val(copy(ANSIParam,1,P-1),I,C);
  if C<>0 then I:=0;
  Delete(ANSIParam,1,P);
  GetANSIParam:=I;
end;
procedure TANSIConsole.CursorUp(LineCount: integer);
begin
  GotoXY(WhereX,WhereY-LineCount);
end;

procedure TANSIConsole.CursorDown(LineCount: integer);
begin
  GotoXY(WhereX,WhereY+LineCount);
end;

procedure TANSIConsole.CursorForward(CharCount: integer);
var X, Y: integer;
begin
  X:=WhereX; Y:=WhereY;
  X:=X+CharCount;
  while (X>Size.X) do
        begin Inc(Y); Dec(X,Size.X); end;
  GotoXY(X,Y);
end;

procedure TANSIConsole.CursorBack(CharCount: integer);
var X, Y: integer;
begin
  X:=WhereX; Y:=WhereY;
  X:=X-CharCount;
  while (X<1) do begin Dec(Y); Inc(X,Size.X); end;
  GotoXY(X,Y);
end;

procedure TANSIConsole.PushCurPos;
begin
  if ANSICurPosStackPtr=ANSICurPosStackSize then Exit;
  Inc(ANSICurPosStackPtr);
  ANSICurPosStack[ANSICurPosStackPtr].X:=WhereX;
  ANSICurPosStack[ANSICurPosStackPtr].Y:=WhereY;
end;

procedure TANSIConsole.PopCurPos;
begin
  if ANSICurPosStackPtr=0 then Exit;
  GotoXY(ANSICurPosStack[ANSICurPosStackPtr].X,ANSICurPosStack[ANSICurPosStackPtr].Y);
  Dec(ANSICurPosStackPtr);
end;

procedure TANSIConsole.SetAttr(Color: integer);
const ColorTab : array[0..7] of byte =
      (Black,Red,Green,Brown,Blue,Magenta,Cyan,LightGray);
begin
  case Color of
    0      : NormVideo;
    1      : HighVideo;
    5      : BlinkVideo;
    7,27   : TextAttr:=(TextAttr shl 4) or (TextAttr shr 4);
    8      : TextColor(TextAttr shr 4);
    21,22  : LowVideo;
    25     : NoBlinkVideo;
    30..37 : TextColor(ColorTab[Color-30]);
    40..47 : TextBackground(ColorTab[Color-40]);
(*  else {$IFDEF DEBUG}begin system.writeln('Unknown attr : ',Color); Halt; end{$ENDIF};*)
  end;
end;

constructor TANSIViewConsole.Init(AOwner: PANSIView);
begin
  if AOwner=nil then Fail;
  inherited Init(nil,nil,nil);
  Owner:=AOwner;
  Size:=Owner^.Size;
end;

procedure TANSIViewConsole.CursorOn;
begin
  Owner^.ShowCursor;
end;

procedure TANSIViewConsole.CursorOff;
begin
  Owner^.HideCursor;
end;

procedure TANSIViewConsole.ClrScr;
var X,Y: word;
    Pos: longint;
begin
  GotoXY(1,1);
  if Owner<>nil then
  for X:=0 to MaxViewWidth-1 do for Y:=0 to Size.Y-1 do
      begin
        Pos:=(Owner^.Delta.Y+Y)*MaxViewWidth+X;
        Owner^.Buffer^[Pos]:=32+256*word(TextAttr);
      end;
end;

procedure TANSIViewConsole.ClrEol;
var X,Y: word;
    Pos: longint;
begin
  if Owner<>nil then
  begin
    Y:=CurPos.Y;
    for X:=CurPos.X to MaxViewWidth-1 do
        begin
          Pos:=(Owner^.Delta.Y*MaxViewWidth)+X+Y*MaxViewWidth;
          Owner^.Buffer^[Pos]:=32+256*word(TextAttr);
        end;
  end;
end;

procedure TANSIViewConsole.WriteChar(C: char);
var Pos: longint;
begin
  case C of
       #8 : begin
              CursorBack(1);
              Pos:=(CurPos.Y-1)*MaxViewWidth+(WhereX-1);
              Owner^.Buffer^[Pos]:=ord(' ')+256*word(TextAttr);
            end;
       #0..#7,#9,
       #11..#12,
       #14..#31,
       #32..#255
            : begin
                Pos:=(CurPos.Y-1)*MaxViewWidth+(WhereX-1);
                Owner^.Buffer^[Pos]:=ord(C)+256*word(TextAttr);
                GotoXY(WhereX+1,WhereY);
              end;
       #10  :
              GotoXY(WhereX,WhereY+1);
       #13  :
              GotoXY(1,WhereY);
  else {$IFDEF DEBUG}RunError(241){$ENDIF};
  end;
end;

procedure TANSIViewConsole.WriteCharRaw(C: char);
var Pos: longint;
begin
  Pos:=(CurPos.Y-1)*MaxViewWidth+(WhereX-1);
  Owner^.Buffer^[Pos]:=ord(C)+256*word(TextAttr);
  GotoXY(WhereX+1,WhereY);
end;

procedure TANSIViewConsole.DelLine(LineCount: integer);
begin
  Abstract;
end;

procedure TANSIViewConsole.InsLine(LineCount: integer);
begin
  Abstract;
end;

procedure TANSIViewConsole.UpdateCursor;
begin
  if Owner<>nil then
  if Owner^.LockCount=0 then Owner^.SetCursor(WhereX-1,WhereY-1);
end;

procedure TANSIViewConsole.GotoXY(X,Y: integer);
var W: word;
begin
  if Owner<>nil then
  while Y>MaxVideoLine do
  begin
    Move(Owner^.Buffer^[MaxViewWidth],Owner^.Buffer,SizeOf(Owner^.Buffer^)-(MaxViewWidth*2));
    W:=(MaxViewWidth*MaxVideoLine)-1-(MaxViewWidth);
    FillChar(Owner^.Buffer^[W],MaxViewWidth*2,0);
    Dec(Y);
  end;
  inherited GotoXY(X,Y);
end;

constructor TANSIView.Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
    PScrollBar);
begin
  inherited Init(Bounds,AHScrollBar,AVScrollBar);
  LockCount:=0; Options:=Options or ofTopSelect;
  GrowMode:=gfGrowHiX or gfGrowHiY;
  New(Buffer);
  SetLimit({MaxViewWidth}80,MaxVideoLine);
  New(Console, Init(@Self));
  Console^.Size.X:=80; Console^.Size.Y:=25;
  Console^.ClrScr;
  Console^.CursorOn;
end;

function TANSIView.LoadFile(const FileName: string): boolean;
var S: PBufStream;
    OK: boolean;
    B: array[0..1023] of char;
    I,FragSize: integer;
begin
{$I-}
  New(S, Init(FileName, stOpenRead, 4096));
  OK:=Assigned(S);
  Lock;
  while OK and (S^.Status=stOK) do
  begin
    FragSize:=Min(Sizeof(B),S^.GetSize-S^.GetPos);
    if FragSize=0 then Break;
    S^.Read(B,FragSize);
    OK:=(S^.Status=stOK);
    if OK then
      for I:=0 to FragSize-1 do
        self.Write(B[I]);
  end;
  Unlock;
  if Assigned(S) then Dispose(S, Done); S:=nil;
{$I+}
  LoadFile:=OK;
end;

procedure TANSIView.Draw;
var I: integer;
    Pos: longint;
    X,Y: integer;
begin
  if LockCount<>0 then Exit;
  for I:=0 to Size.Y-1 do
  begin
    Pos:=Delta.X+(Delta.Y+I)*MaxViewWidth;
    WriteLine(0,I,Size.X,1,Buffer^[Pos]);
  end;
  if Console=nil then Exit;
  X:=Console^.WhereX-Delta.X; Y:=Console^.WhereY-Delta.Y;
  if (X<0) or (Y<0) or (X>Size.X-1) or (Y>Size.X-1)
     then HideCursor
     else begin
            ShowCursor;
            SetCursor(X-1,Y-1);
          end;
end;

procedure TANSIView.Write(Const S: string);
begin
  Console^.Write(S);
  DrawView;
end;

procedure TANSIView.WriteLn(Const S: string);
begin
  Console^.WriteLn(S);
  DrawView;
end;

procedure TANSIView.Lock;
begin
  Inc(LockCount);
end;

procedure TANSIView.UnLock;
begin
  Dec(LockCount);
  if LockCount=0 then DrawView;
end;

procedure TANSIView.ChangeBounds(var Bounds: TRect);
begin
  inherited ChangeBounds(Bounds);
{  Console^.Size.X:=Size.X; Console^.Size.Y:=Size.Y;}
end;

procedure TANSIView.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
{  if Event.What=evKeyDown then
     begin
       if VScrollBar<>nil then VScrollBar^.HandleEvent(Event);
       if HScrollBar<>nil then HScrollBar^.HandleEvent(Event);
     end;}
end;

destructor TANSIView.Done;
begin
  Dispose(Console, Done);
  Dispose(Buffer);
  inherited Done;
end;

constructor TANSIBackgroundConsole.Init(AOwner: PANSIBackground);
begin
  if AOwner=nil then Fail;
  inherited Init(nil,nil,nil);
  Owner:=AOwner;
  Size:=Owner^.Size;
end;

procedure TANSIBackgroundConsole.CursorOn;
begin
  Owner^.ShowCursor;
end;

procedure TANSIBackgroundConsole.CursorOff;
begin
  Owner^.HideCursor;
end;

procedure TANSIBackgroundConsole.ClrScr;
var X,Y: word;
    Pos: longint;
begin
  GotoXY(1,1);
  if Owner<>nil then
  for X:=0 to MaxViewWidth-1 do
    for Y:=0 to Size.Y-1 do
      begin
        Pos:=X+Y*MaxViewWidth;
        Owner^.Buffer[Pos]:=32+256*word(TextAttr);
      end;
end;

procedure TANSIBackgroundConsole.ClrEol;
var X,Y: word;
    Pos: longint;
begin
  if Owner<>nil then
  begin
    Y:=CurPos.Y;
    for X:=CurPos.X to MaxViewWidth-1 do
        begin
          Pos:=X+Y*MaxViewWidth;
          Owner^.Buffer[Pos]:=32+256*word(TextAttr);
        end;
  end;
end;

procedure TANSIBackgroundConsole.WriteChar(C: char);
var Pos: longint;
begin
  case C of
       #8 : begin
              CursorBack(1);
              Pos:=(CurPos.Y-1)*MaxViewWidth+(WhereX-1);
              Owner^.Buffer[Pos]:=ord(' ')+256*word(TextAttr);
            end;
       #0..#7,#9,
       #11..#12,
       #14..#31,
       #32..#255
            : begin
                Pos:=(CurPos.Y-1)*MaxViewWidth+(WhereX-1);
                Owner^.Buffer[Pos]:=ord(C)+256*word(TextAttr);
                GotoXY(WhereX+1,WhereY);
              end;
       #10  :
              GotoXY(WhereX,WhereY+1);
       #13  :
              GotoXY(1,WhereY);
  else {$IFDEF DEBUG}RunError(241){$ENDIF};
  end;
end;

procedure TANSIBackgroundConsole.DelLine(LineCount: integer);
begin
  Abstract;
end;

procedure TANSIBackgroundConsole.InsLine(LineCount: integer);
begin
  Abstract;
end;

procedure TANSIBackgroundConsole.UpdateCursor;
begin
  if Owner<>nil then
  if Owner^.LockCount=0 then Owner^.SetCursor(WhereX-1,WhereY-1);
end;

procedure TANSIBackgroundConsole.GotoXY(X,Y: integer);
var W: word;
begin
  if Owner<>nil then
  while Y>MaxVideoLine do
  begin
    Move(Owner^.Buffer[MaxViewWidth],Owner^.Buffer,SizeOf(Owner^.Buffer)-(MaxViewWidth*2));
    W:=(MaxViewWidth*MaxVideoLine)-1-(MaxViewWidth);
    FillChar(Owner^.Buffer[W],MaxViewWidth*2,0);
    Dec(Y);
  end;
  inherited GotoXY(X,Y);
end;

constructor TANSIBackground.Init(var Bounds: TRect);
begin
  inherited Init(Bounds,' ');
  LockCount:=0;
  GrowMode:=gfGrowHiX or gfGrowHiY;
  New(Console, Init(@Self));
  Console^.Size.X:=Bounds.B.X+1; Console^.Size.Y:=Bounds.B.Y+1;
  Console^.ClrScr;
  Console^.CursorOn;
end;

function TANSIBackground.LoadFile(const FileName: string): boolean;
var S: PBufStream;
    OK: boolean;
    B: array[0..1023] of char;
    I,FragSize: integer;
begin
{$I-}
  New(S, Init(FileName, stOpenRead, 4096));
  OK:=Assigned(S);
  while OK and (S^.Status=stOK) do
  begin
    FragSize:=Min(Sizeof(B),S^.GetSize-S^.GetPos);
    if FragSize=0 then Break;
    S^.Read(B,FragSize);
    OK:=(S^.Status=stOK);
    if OK then
      for I:=0 to FragSize-1 do
        self.Write(B[I]);
  end;
  if Assigned(S) then Dispose(S, Done); S:=nil;
{$I+}
  LoadFile:=OK;
end;

procedure TANSIBackground.Draw;
var I: integer;
    Pos: longint;
    X,Y: integer;
begin
  if LockCount<>0 then Exit;
  for I:=0 to Size.Y-1 do
  begin
    Pos:=I*MaxViewWidth;
    WriteLine(0,I,Size.X,1,Buffer[Pos]);
  end;
  if Console=nil then Exit;
  X:=Console^.WhereX; Y:=Console^.WhereY;
  if (X<0) or (Y<0) or (X>Size.X-1) or (Y>Size.X-1)
     then HideCursor
     else begin
            ShowCursor;
            SetCursor(X-1,Y-1);
          end;
end;

procedure TANSIBackground.Write(Const S: string);
begin
  Console^.Write(S);
  DrawView;
end;

procedure TANSIBackground.WriteLn(Const S: string);
begin
  Console^.WriteLn(S);
  DrawView;
end;

procedure TANSIBackground.Lock;
begin
  Inc(LockCount);
end;

procedure TANSIBackground.UnLock;
begin
  Dec(LockCount);
  if LockCount=0 then DrawView;
end;

procedure TANSIBackground.ChangeBounds(var Bounds: TRect);
begin
  inherited ChangeBounds(Bounds);
{  Console^.Size.X:=Size.X; Console^.Size.Y:=Size.Y;}
end;

procedure TANSIBackground.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
{  if Event.What=evKeyDown then
     begin
       if VScrollBar<>nil then VScrollBar^.HandleEvent(Event);
       if HScrollBar<>nil then HScrollBar^.HandleEvent(Event);
     end;}
end;

destructor TANSIBackground.Done;
begin
  Dispose(Console, Done);
  inherited Done;
end;

END.
