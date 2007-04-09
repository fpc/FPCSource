{ Source provided for Free Pascal Bug Report 4624 }
{ Submitted by "benoit sanchez" on  2005-12-20 }
{ e-mail: sanchez@clipper.ens.fr }
type Number=record
  value:boolean;
end;

operator div (a,b:number) c:number;
begin
  c.value:=true;
end;

var a:number;
begin
  a:=a div a;
end.
