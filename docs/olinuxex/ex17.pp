Program Example17;

{ Program to demonstrate the GetUid and GetEUid functions. }

Uses oldlinux;

begin
  writeln ('User Id = ',getuid,' Effective user Id = ',geteuid);
end.
