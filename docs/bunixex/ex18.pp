Program Example18;

{ Program to demonstrate the GetGid and GetEGid functions. }

Uses BaseUnix;

begin
 writeln ('Group Id = ',fpgetgid,' Effective group Id = ',fpgetegid);
end.
