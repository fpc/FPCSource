program example6;

{ This program demonstrates the PollShiftStateEvent function }

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
      K:=PollShiftStateEvent;
      Writeln('Got shift state : ',ShiftStateToString(K,False));
      // Consume the key.
      K:=GetKeyEvent;
      K:=TranslateKeyEvent(K);
      end
{    else
      write('.')};
  Until (GetKeyEventChar(K)='q');
  DoneKeyBoard;
end.
