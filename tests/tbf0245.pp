const
 r = 3.5;
 s = 'test idiot';
type
  preal = ^real;
  pstring = ^string;

  procedure ss;
   begin
   end;
   
var
  p : pointer;
  pr : preal;
  ps : pstring;

  begin
   p:=@ss;
   p:=@s;
   pr:=@r;
   ps:=@s;
   pr^:=7.8;
   ps^:='test3';
   Writeln('r=',r,' s=',s);
  end.
   
