
uses
  Terminfo, Linux;

var
   Error, J : Longint;
   I: Integer;

begin
   setupterm(nil, 1, Error);
   if Error = 1 then begin
     Write(cur_term^.TType.Strings[clear_screen]);
     for I := 1 to 15 do begin
       Write(cur_term^.TType.Strings[cursor_right]);
       Write(cur_term^.TType.Strings[cursor_down]);
       for J := 1 to 1000000 do ;
     end;
     for I := 1 to 15 do begin
       Write(cur_term^.TType.Strings[cursor_up]);
       Write(cur_term^.TType.Strings[cursor_right]);
       for J := 1 to 1000000 do ;
     end;
   end;
end.
