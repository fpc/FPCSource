Program Example2;

{ Program to demonstrate the GetKeyEventCode function. }

Uses keyboard;

Var
  K : TKeyEvent;

begin
  InitKeyBoard;
  Writeln('Press function keys, or press "q" to end.');
  Repeat
    K:=GetKeyEvent;
    K:=TranslateKeyEvent(K);
    If (GetKeyEventFlags(K)<>KbfnKey) then
      Writeln('Not a function key')
    else
      begin
      Write('Got key (',GetKeyEventCode(K));
      Writeln(') : ',KeyEventToString(K));
      end;
  Until (GetKeyEventChar(K)='q');
  DoneKeyboard;
end.
