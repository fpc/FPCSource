Program Example8;

{ Program to demonstrate the FunctionKeyName function. }

Uses keyboard;

Var
  K : TkeyEvent;

begin
  InitKeyboard;
  Writeln('Press function keys, press "q" to end.');
  Repeat
    K:=GetKeyEvent;
    K:=TranslateKeyEvent(K);
    If IsFunctionKey(k) then
      begin
      Write('Got function key : ');
      Writeln(FunctionKeyName(TkeyRecord(K).KeyCode));
      end;
  Until (GetKeyEventChar(K)='q');
  DoneKeyboard;
end.
