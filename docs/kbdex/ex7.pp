program example1;

{ This program demonstrates the GetKeyEvent function }

uses keyboard;

Var
  K : TKeyEvent;

begin
  InitKeyBoard;
  Writeln('Press keys, press "q" to end.');
  Repeat
    K:=GetKeyEvent;
    K:=TranslateKeyEvent(K);
    If IsFunctionKey(K) then
      Writeln('Got function key : ',KeyEventToString(K))
    else
      Writeln('not a function key.');
  Until (GetKeyEventChar(K)='q');
  DoneKeyBoard;
end.
