{ Old file: tbs0120.pp }
{ inc/dec(enumeration) doesn't work                     OK 0.99.6 (MVC) }

type
   te = (enum1,enum2,enum3);

var
   e,f : te;

begin
   e:=enum1;
   inc(e);
   f:=enum3;
   dec(f);
   if e<>f then
    halt(1);
end.
