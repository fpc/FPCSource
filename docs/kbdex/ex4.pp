program example4;

{ This program demonstrates the PollKeyEvent function }

uses keyboard;

Var
  K : TKeyEvent;

begin
  InitKeyBoard;
  Writeln('Press keys, press "q" to end.');
  Repeat
    K:=PollKeyEvent;
    If k<>0 then
      begin
      K:=GetKeyEvent;
      K:=TranslateKeyEvent(K);
      writeln;
      Writeln('Got key : ',KeyEventToString(K));
      end
    else
      write('.');
  Until (GetKeyEventChar(K)='q');
  DoneKeyBoard;
end.
