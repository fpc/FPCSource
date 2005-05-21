{ Source provided for Free Pascal Bug Report 3967 }
{ Submitted by "luoyonggang" on  2005-05-14 }
{ e-mail: luoyonggang@gmail.com }
var
  i,j,n,m,k,l:longint;
  o,p,q,s:int64;
begin
  s:=1; p:=1;
  for i:=3 downto 1 do begin
    if s*(i+2) mod i=0 then s:=s*(i+2) div i else begin
      s:=s*(i+2);
      p:=p*i;
      {The error is here!If we swap the variable i and the variable p then the value of the variable p is right}
      {But now the value of the vairable p is zero,you see,the value of the vairable p must be 3}
      {The compiler was 1.98}
      if p<>3 then
        halt(1);
    end
  end;
  if p<>3 then
    halt(1);
  writeln('ok');
end.
