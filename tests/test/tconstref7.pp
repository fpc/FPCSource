{ %FAIL }
{ [Ref] is the only attribute a parameter takes }
program tconstref7;

{$mode delphi}

procedure P(const [Weak] a: TObject);
begin
end;

begin
end.
