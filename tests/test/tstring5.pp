uses
   erroru;

var
   a1,a2 : ansistring;

function f1 : ansistring;

  begin
     f1:='';
  end;

function f2 : ansistring;

  begin
     f2:='Hello';
  end;

begin
   a1:='';
   a2:='Hello';
   if a1<>'' then
     do_error(1000);
   if a2='' then
     do_error(1001);
   if ''<>a1 then
     do_error(1002);
   if ''=a2 then
     do_error(1003);

   if f1<>'' then
     do_error(1004);
   if f2='' then
     do_error(1005);
   if ''<>f1 then
     do_error(1006);
   if ''=f2 then
     do_error(1007);
end.
