{ %fail }

{ Source provided for Free Pascal Bug Report 4144 }
{ Submitted by "Wiktor Sywula" on  2005-06-30 }
{ e-mail: wswiktor@poczta.fm }
procedure foo;
type
   bar = object
     constructor init;
     procedure zzyzzy; virtual;
   end;
begin
end;

begin
end.
