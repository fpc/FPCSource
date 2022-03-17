{ test for the enhanced video attributes support }
program video4;

{$mode objfpc}{$H+}

uses
  video, keyboard;

procedure TextOut(X, Y: Integer; const S: string; Attr: TEnhancedVideoAttributes);
var
  W, P, I, M: Integer;
begin
  P := ((X-1)+(Y-1)*ScreenWidth);
  M := Length(S);
  if (P+M) > ScreenWidth*ScreenHeight then
    M := ScreenWidth*ScreenHeight-P;
  for I := 1 to M do
    with EnhancedVideoBuf[P+I-1] do
    begin
      ExtendedGraphemeCluster := S[I];
      EnhancedVideoAttributes := Attr;
    end;
end;

var
  k: TKeyEvent;
  X, Y: Integer;
begin
  InitKeyboard;
  InitEnhancedVideo;
  repeat
    TextOut( 1,  4, 'vanilla', []);
    TextOut( 6,  6, 'underline', [evaUnderlined]);
    TextOut( 1,  8, 'blink', [evaBlinkSlow]);
    TextOut( 6, 10, 'underline blink', [evaUnderlined, evaBlinkSlow]);
    TextOut( 1, 12, 'negative', [evaInverse]);
    TextOut( 6, 14, 'underline negative', [evaUnderlined, evaInverse]);
    TextOut( 1, 16, 'blink negative', [evaBlinkSlow, evaInverse]);
    TextOut( 6, 18, 'underline blink negative', [evaUnderlined, evaBlinkSlow, evaInverse]);
    TextOut(40,  4, 'bold', [evaBold]);
    TextOut(46,  6, 'bold underline', [evaBold, evaUnderlined]);
    TextOut(40,  8, 'bold blink', [evaBold, evaBlinkSlow]);
    TextOut(46, 10, 'bold underline blink', [evaBold, evaUnderlined, evaBlinkSlow]);
    TextOut(40, 12, 'bold negative', [evaBold, evaInverse]);
    TextOut(46, 14, 'bold underline negative', [evaBold, evaUnderlined, evaInverse]);
    TextOut(40, 16, 'bold blink negative', [evaBold, evaBlinkSlow, evaInverse]);
    TextOut(46, 18, 'bold underline blink negative', [evaBold, evaUnderlined, evaBlinkSlow, evaInverse]);

    TextOut(10, 24, 'Press space to continue', []);
    UpdateScreen(False);

    k := GetKeyEvent;
    k := TranslateKeyEvent(k);
  until GetKeyEventChar(k) = ' ';
  ClearScreen;
  repeat
    TextOut( 1,  4, 'vanilla', []);
    TextOut( 6,  6, 'bold', [evaBold]);
    TextOut( 1,  8, 'faint', [evaFaint]);
    TextOut( 6, 10, 'italicized', [evaItalicized]);
    TextOut( 1, 12, 'underlined', [evaUnderlined]);
    TextOut( 6, 14, 'slowly blinking', [evaBlinkSlow]);
    TextOut( 1, 16, 'rapidly blinking', [evaBlinkFast]);
    TextOut( 6, 18, 'inverse', [evaInverse]);
    TextOut(40,  4, 'invisible', [evaInvisible]);
    TextOut(46,  6, 'crossed out', [evaCrossedOut]);
    TextOut(40,  8, 'doubly underlined', [evaDoublyUnderlined]);

    TextOut(10, 24, 'Press space to continue', []);
    UpdateScreen(False);

    k := GetKeyEvent;
    k := TranslateKeyEvent(k);
  until GetKeyEventChar(k) = ' ';
  DoneEnhancedVideo;
  DoneKeyboard;
end.

