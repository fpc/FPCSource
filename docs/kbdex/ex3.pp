Program Example3;

{ Program to demonstrate the GetKeyEventShiftState function. }

Uses keyboard;

Var
  K : TKeyEvent;
  S : Byte;

begin
  InitKeyBoard;
  Write('Press keys combined with CTRL/SHIFT/ALT');
  Writeln(', or press "q" to end.');
  Repeat
    K:=GetKeyEvent;
    K:=TranslateKeyEvent(K);
    S:=GetKeyEventShiftState(K);
    If (S=0) then
      Writeln('No special keys pressed')
    else
      begin
      Writeln('Detected special keys : ',ShiftStateToString(K,False));
      Writeln('Got key : ',KeyEventToString(K));
      end;
  Until (GetKeyEventChar(K)='q');
  DoneKeyboard;
end.
