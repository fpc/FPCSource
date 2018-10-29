program kbd1;

uses
  keyboard;

procedure ShowKeyEvent(K: TKeyEvent);
begin
  case GetKeyEventFlags(K) of
    kbASCII    : Writeln('ASCII key');
    kbUniCode  : Writeln('Unicode key');
    kbFnKey    : Writeln('Function key');
    kbPhys     : Writeln('Physical key');
    kbReleased : Writeln('Released key event');
  end;
  Writeln('Got key : ', KeyEventToString(K));
end;

var
  K: TKeyEvent;
begin
  InitKeyboard;
  Writeln('Press keys, press "q" to end.');
  repeat
    K:=GetKeyEvent;
    Write('Before translation: ');
    ShowKeyEvent(K);
    K:=TranslateKeyEvent(K);
    Write('After translation: ');
    ShowKeyEvent(K);
  until (GetKeyEventChar(K)='q');
  DoneKeyboard;
end.
