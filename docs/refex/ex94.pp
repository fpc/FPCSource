Program Example94;

{ Program to demonstrate the SetResourceStringValue function. }
{$Mode Delphi}

ResourceString

  First  = 'First string';
  Second = 'Second String';

Var I,J : Longint;
    S : AnsiString;

begin
  { Print current values of all resourcestrings }
  For I:=0 to ResourceStringTableCount-1 do
    For J:=0 to ResourceStringCount(i)-1 do
      begin
      Writeln ('Translate => ',GetResourceStringDefaultValue(I,J));
      Write   ('->');
      Readln(S);
      SetResourceStringValue(I,J,S);
      end;
  Writeln ('Translated strings : ');
  For I:=0 to ResourceStringTableCount-1 do
    For J:=0 to ResourceStringCount(i)-1 do
      begin
      Writeln (GetResourceStringDefaultValue(I,J));
      Writeln ('Translates to : ');
      Writeln (GetResourceStringCurrentValue(I,J));
      end;
end.
