Program Example93;

{ Program to demonstrate the Hash function. }
{$Mode Delphi}

ResourceString

  First  = 'First string';
  Second = 'Second String';

Var I,J : Longint;

begin
  For I:=0 to ResourceStringTableCount-1 do
    For J:=0 to ResourceStringCount(i)-1 do
      If Hash(GetResourceStringDefaultValue(I,J))
        <>GetResourceStringHash(I,J) then
        Writeln ('Hash mismatch at ',I,',',J)
      else
        Writeln ('Hash (',I,',',J,') matches.');
end.
