{ %OPT=-Or}
{$maxfpuregisters 3}
uses
   erroru;

var
   t : text;

procedure p;

  var
     d : double;
     e : extended;
     s : single;

  begin
     readln(t,d);
     if d<>1 then
       do_error(1000);
     readln(t,d);
     if d<>2 then
       do_error(1001);
     readln(t,e);
     if e<>3 then
       do_error(1002);
     readln(t,e);
     if e<>4 then
       do_error(1003);
     readln(t,s);
     if s<>5 then
       do_error(1004);
     readln(t,s);
     if s<>6 then
       do_error(1005);
  end;

begin
   assign(t,'treg2.dat');
   reset(t);
   p;
   close(t);
end.
