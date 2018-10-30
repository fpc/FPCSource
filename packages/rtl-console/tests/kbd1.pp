program kbd1;

uses
  keyboard;

procedure ShowASCIIKey(C: Char);
begin
  Write('ASCII key #', Ord(C), ' - #$', HexStr(Ord(C), 2));
  if C = '''' then
    Write(' - ''''''''')
  else if (C >= #32) and (C <= #126) then
    Write(' - ''', C, '''')
  else if C < #32 then
    Write(' - ^', Chr(Ord(C) + Ord('@')));
  Writeln;
end;

procedure ShowUnicodeKey(WC: WideChar);
begin
  Writeln('Unicode key #', Ord(WC));
end;

procedure ShowKeyEvent(K: TKeyEvent);
begin
  case GetKeyEventFlags(K) and 3 of
    kbASCII:
      ShowASCIIKey(GetKeyEventChar(K));
    kbUniCode:
      ShowUnicodeKey(WideChar(GetKeyEventUniCode(K)));
    kbFnKey:
      Writeln('Function key ', FunctionKeyName(GetKeyEventCode(K)));
    kbPhys:
      Writeln('Physical key ', K and $FFFF, ' - $' + HexStr(K and $FFFF, 4));
  end;
  Writeln('Shift state: ', ShiftStateToString(K, True));
  if (GetKeyEventFlags(K) and kbReleased) <> 0 then
    Writeln('Released key event');
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
