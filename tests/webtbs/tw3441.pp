{ Source provided for Free Pascal Bug Report 3441 }
{ Submitted by "Alexey Barkovoy" on  2004-12-07 }
{ e-mail: clootie@ixbt.com }
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}
procedure ArrayOfCharsTest(const a: PChar; b: PWideChar);
begin
  Writeln(a, b^); // just do something
end;

procedure ArrayOfConstTest(const Args: array of const);
begin
  Writeln(High(Args)); // just do something
end;

var
  a: array[0..5] of Char;
  b: array[0..5] of WideChar;
begin
  a[0]:= 'a'; a[1]:= #0;
  b[0]:= 'b'; b[1]:= #0;
  ArrayOfCharsTest(a, b); // This compiles
  ArrayOfConstTest(['a', a, b]); //_2.pas(19,29) Error: Incompatible types: got "Array[0..5] Of WideChar" expected "^Char"
end.
