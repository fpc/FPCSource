{ Source provided for Free Pascal Bug Report 2040 }
{ Submitted by "Dimitry Sibiryakov" on  2002-07-16 }
{ e-mail: SD@topol.udm.net }
Unit uw2040;
{$H+}
interface

Var
 a: String; // make this string short and all is Ok

implementation

Procedure b(s: string); // Remove this proc and all is Ok too
begin
end;

end.
