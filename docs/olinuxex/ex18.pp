Program Example18;

{ Program to demonstrate the GetGid and GetEGid functions. }

Uses oldlinux;

begin
 writeln ('Group Id = ',getgid,' Effective group Id = ',getegid);
end.
