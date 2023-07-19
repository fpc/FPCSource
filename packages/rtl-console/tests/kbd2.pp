program kbd2;

uses
  keyboard;

procedure ShowASCIIKey(C: AnsiChar);
begin
  Write('ASCII key #', Ord(C), ' - #$', HexStr(Ord(C), 2));
  if C = '''' then
    Write(' - ''''''''')
  else if (C >= #32) and (C <= #126) then
    Write(' - ''', C, '''')
  else if C < #32 then
    Write(' - ^', Chr(Ord(C) + Ord('@')));
end;

procedure ShowUnicodeKey(WC: WideChar);
begin
  Write('Unicode key #', Ord(WC));
end;

function EnhancedShiftStateToString(const ShiftState: TEnhancedShiftState): shortstring;
var
  S: TEnhancedShiftStateElement;
  FirstElement: Boolean = True;
begin
  EnhancedShiftStateToString := '[';
  for S in TEnhancedShiftStateElement do
    if S in ShiftState then
    begin
      if FirstElement then
        WriteStr(EnhancedShiftStateToString, EnhancedShiftStateToString, S)
      else
        WriteStr(EnhancedShiftStateToString, EnhancedShiftStateToString, ',', S);
      FirstElement := False;
    end;
  EnhancedShiftStateToString := EnhancedShiftStateToString + ']';
end;

procedure ShowKeyEvent(const K: TEnhancedKeyEvent);
begin
  ShowASCIIKey(K.AsciiChar);
  Write(', ');
  ShowUnicodeKey(K.UnicodeChar);
  Write(', Virtual Scan Code ', K.VirtualScanCode, ' - $' + HexStr(K.VirtualScanCode, 4),
    ', Function key ', FunctionKeyName(K.VirtualKeyCode),
    ', Shift state: ', EnhancedShiftStateToString(K.ShiftState));
  Writeln;
end;

var
  K: TEnhancedKeyEvent;
begin
  InitKeyboard;
  Writeln('Press keys, press "q" to end.');
  repeat
    K:=GetEnhancedKeyEvent;
    ShowKeyEvent(K);
  until K.AsciiChar='q';
  DoneKeyboard;
end.
