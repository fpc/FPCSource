Program Example95;

{ Program to demonstrate the SetResourceStrings function. }
{$Mode objfpc}

ResourceString

  First  = 'First string';
  Second = 'Second String';

Var I,J : Longint;
    S : AnsiString;

Function Translate (Name,Value : AnsiString; Hash : longint): AnsiString;

begin
  Writeln ('Translate (',Name,') => ',Value);
  Write   ('->');
  Readln  (Result);
end;

begin
  SetResourceStrings(@Translate);
  Writeln ('Translated strings : ');
  For I:=0 to ResourceStringTableCount-1 do
    For J:=0 to ResourceStringCount(i)-1 do
      begin
      Writeln (GetResourceStringDefaultValue(I,J));
      Writeln ('Translates to : ');
      Writeln (GetResourceStringCurrentValue(I,J));
      end;
end.
