{$ifndef macos}{$APPTYPE CONSOLE}{$endif}
{$ifdef fpc}{$mode delphi}{$endif}

procedure wideansi;
var
  S: AnsiString = 'abcd';
  pc: PChar = 'abcd';
  ca: array [0..4] of Char = ('a', 'b', 'c', 'd', #0);
  W: WideString = 'abcd';
  pw: PWideChar = 'abcd';
  wa: array [0..4] of WideChar = ('a', 'b', 'c', 'd', #0);
begin
{$IFDEF FPC}
  ca := S;
  ca := W;
  ca := pc;
  wa := pw;
  wa := S;
  wa := W;
{$ENDIF}

  W := S;
  W := pc;
  W := ca;
  W := pw;
  W := wa;

  S := W;
  S := pc;
  S := ca;
  S := pw;
  S := wa;

  if S = pc then WriteLn('(2.1) strings are equal');
  if S = ca then WriteLn('(2.2) strings are equal');
  WriteLn;

  if W = pw then WriteLn('(3.1) wide pass');
  if W = wa then WriteLn('(3.2) wide pass');
  WriteLn;

  if S = W  then WriteLn('(4.1) wide vs ansi');
  if pc = W then WriteLn('(4.2) wide vs ansi');
  if ca = W then WriteLn('(4.3) wide vs ansi');
  if pw = S then WriteLn('(4.4) wide vs ansi');
  if wa = S then WriteLn('(4.5) wide vs ansi');
  WriteLn;
end;

procedure wideshort;
var
  S: ShortString = 'abcd';
  pc: PChar = 'abcd';
  ca: array [0..4] of Char = ('a', 'b', 'c', 'd', #0);
  W: WideString = 'abcd';
  pw: PWideChar = 'abcd';
  wa: array [0..4] of WideChar = ('a', 'b', 'c', 'd', #0);
begin
{$IFDEF FPC}
  ca := S;
  ca := W;
  ca := pc;
  wa := pw;
  wa := S;
  wa := W;
{$ENDIF}

  W := S;
  W := pc;
  W := ca;
  W := pw;
  W := wa;

  S := W;
  S := pc;
  S := ca;
  S := pw;
  S := wa;

  if S = pc then WriteLn('(2.1) strings are equal');
  if S = ca then WriteLn('(2.2) strings are equal');
  WriteLn;

  if W = pw then WriteLn('(3.1) wide pass');
  if W = wa then WriteLn('(3.2) wide pass');
  WriteLn;

  if S = W  then WriteLn('(4.1) wide vs ansi');
  if pc = W then WriteLn('(4.2) wide vs ansi');
  if ca = W then WriteLn('(4.3) wide vs ansi');
  if pw = S then WriteLn('(4.4) wide vs ansi');
  if wa = S then WriteLn('(4.5) wide vs ansi');
  WriteLn;
end;

begin
  wideshort;
  wideansi;
end.
