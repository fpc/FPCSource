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
    Write('Got key event with ');
    Case GetKeyEventFlags(K) of
      kbASCII    : Writeln('ASCII key');
      kbUniCode  : Writeln('Unicode key');
      kbFnKey    : Writeln('Function key');
      kbPhys     : Writeln('Physical key');
      kbReleased : Writeln('Released key event');
    end;
    K:=TranslateKeyEvent(K);
    Writeln('Got key : ',KeyEventToString(K));
  Until (GetKeyEventChar(K)='q');
  DoneKeyBoard;
end.
