{ Source provided for Free Pascal Bug Report 1699 }
{ Submitted by "SHADOW" on  2001-11-25 }
{ e-mail: mdikm@hotmail.com }
const
  i : longint = 0;


procedure nset(var d:byte;p,c:byte;v:byte);
begin
  i:=i or 1;
end;

procedure nset(var d:word;p,c:byte;v:word);
begin
  i:=i or 2;
end;

procedure nset(var d:cardinal;p,c:byte;v:cardinal);
begin
  i:=i or 4;
end;

procedure nset(var d:qword;p,c:byte;v:qword);
begin
  i:=i or 8;
end;

var
  b : byte;
  w : word;
  c : cardinal;
  q : qword;
begin
  nset(b,0,0,0);
  nset(w,0,0,0);
  nset(c,0,0,0);
  nset(q,0,0,0);
  if i<>15 then
   begin
     Writeln('Error');
     halt(1);
   end;
end.
