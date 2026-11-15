{ %FAIL }
{ a parameter's attribute has to be a known attribute class }
program tconstref7;

{$mode delphi}

procedure P(const [NoSuchAttribute] a: TObject);
begin
end;

begin
end.
