program example9;

{ This program demonstrates the logkeys unit }

uses keyboard,logkeys;

Var
  K : TKeyEvent;

begin
  InitKeyBoard;
  Writeln('Press keys, press "q" to end, "s" toggles logging.');
  Repeat
    K:=GetKeyEvent;
    K:=TranslateKeyEvent(K);
    Writeln('Got key : ',KeyEventToString(K));
    if GetKeyEventChar(K)='s' then
      if IsKeyLogging then
        StopKeyLogging
      else
        StartKeyLogging;
  Until (GetKeyEventChar(K)='q');
  DoneKeyBoard;
end.
