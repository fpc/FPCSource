{ Source provided for Free Pascal Bug Report 1699 }
{ Submitted by "SHADOW" on  2001-11-25 }
{ e-mail: mdikm@hotmail.com }
function bget(d:byte;p:byte):boolean;
begin
end;
procedure bset(var d:byte;p:byte;v:boolean);
begin
end;
function nget(d:byte;p,c:byte):byte;
begin
end;
procedure nset(var d:byte;p,c:byte;v:byte);
begin
end;

function bget(d:word;p:byte):boolean;
begin
end;
procedure bset(var d:word;p:byte;v:boolean);
begin
end;
function nget(d:word;p,c:byte):word;
begin
end;
procedure nset(var d:word;p,c:byte;v:word);
begin
end;

function bget(d:cardinal;p:byte):boolean;
begin
end;
procedure bset(var d:cardinal;p:byte;v:boolean);
begin
end;
function nget(d:cardinal;p,c:byte):cardinal;
begin
end;
procedure nset(var d:cardinal;p,c:byte;v:cardinal);
begin
end;

function bget(d:qword;p:byte):boolean;
begin
end;
procedure bset(var d:qword;p:byte;v:boolean);
begin
end;
function nget(d:qword;p,c:byte):qword;
begin
end;
procedure nset(var d:qword;p,c:byte;v:qword);
begin
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
end.
