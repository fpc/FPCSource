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
