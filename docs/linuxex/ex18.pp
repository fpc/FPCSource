Program Example18;

{ Program to demonstrate the GetGid and GetEGid functions. }

Uses linux;

begin
 writeln ('Group Id = ',getgid,' Effective group Id = ',getegid);
end.
