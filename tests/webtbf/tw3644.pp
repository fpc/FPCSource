{ %fail }

{ Source provided for Free Pascal Bug Report 3644 }
{ Submitted by "Nicola Lugato" on  2005-02-10 }
{ e-mail:  }

{$mode delphi}

procedure callme(y:array of const);
begin
end;

procedure callme2(x:array of const);
begin
callme([x]);
end;

begin
callme([1,2,'ciao']);
end.
