program kbd1;

uses
  keyboard;

procedure ShowKeyEvent(K: TKeyEvent);
begin
  case GetKeyEventFlags(K) and 3 of
    kbASCII:
      Writeln('ASCII key #', Ord(GetKeyEventChar(K)));
    kbUniCode:
      Writeln('Unicode key');
    kbFnKey:
      Writeln('Function key');
    kbPhys:
      Writeln('Physical key $' + HexStr(K and $FFFF, 4));
  end;
  if (GetKeyEventFlags(K) and kbReleased) <> 0 then
    Writeln('Released key event');
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
