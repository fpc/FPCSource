{ %fail }

{ Source provided for Free Pascal Bug Report 3145 }
{ Submitted by "Christian Iversen" on  2004-06-08 }
{ e-mail: chrivers@iversen-net.dk }
program envhost;

{$mode objfpc}

Procedure Foo(A : Integer = Integer(Nil^));
begin
end;

begin
end.
