type
  days = (sun,mon,tue,wed,thu,fri,sat);
  workdays = mon..fri;

procedure t(d: workdays);
  begin
     case d of
        mon: writeln('monday');
        thu: writeln('thursday');
     else
        writeln('error');
     end;
  end;

var
   d: workdays;

begin
   d := thu;
   t(d);
end.
