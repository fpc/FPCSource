program example2;

uses video,keyboard;

{$ifndef cpu86}
{$error This example only works on intel 80x86 machines}
{$endif}

Var
  P,PP,D : Integer;
  K: TKeyEvent;

  Procedure PutSquare (P : INteger; C : Char);

  begin
    VideoBuf^[P]:=Ord(C)+($07 shl 8);
    VideoBuf^[P+ScreenWidth]:=Ord(c)+($07 shl 8);
    VideoBuf^[P+1]:=Ord(c)+($07 shl 8);
    VideoBuf^[P+ScreenWidth+1]:=Ord(c)+($07 shl 8);
  end;

begin
  InitVideo;
  InitKeyBoard;
  P:=0;
  PP:=-1;
  Repeat
    If PP<>-1 then
      PutSquare(PP,' ');
    PutSquare(P,'#');
    SetCursorPos(P Mod ScreenWidth,P div ScreenWidth);
    UpdateScreen(False);
    PP:=P;
    Repeat
      D:=0;
      K:=TranslateKeyEvent(GetKeyEvent);
      Case GetKeyEventCode(K) of
        kbdLeft : If (P Mod ScreenWidth)<>0 then
                   D:=-1;
        kbdUp : If P>=ScreenWidth then
                 D:=-ScreenWidth;
        kbdRight : If ((P+2) Mod ScreenWidth)<>0 then
                   D:=1;
        kbdDown : if (P<(VideoBufSize div 2)-(ScreenWidth*2)) then
                   D:=ScreenWidth;
      end;
    Until (D<>0) or (GetKeyEventChar(K)='q');
    P:=P+D;
  until GetKeyEventChar(K)='q';
  DoneKeyBoard;
  DoneVideo;
end.