Program Example17;

{ Program to demonstrate the GetUid and GetEUid functions. }

Uses BaseUnix;

begin
  writeln ('User Id = ',fpgetuid,' Effective user Id = ',fpgeteuid);
end.
