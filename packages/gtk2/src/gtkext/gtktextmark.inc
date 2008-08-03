
procedure Freegtktextmark;
begin
  gtk_text_mark_new:=nil
end;


procedure Loadgtktextmark;
begin
  Freegtktextmark;

  pointer(gtk_text_mark_new):=GetProcAddress(gtkhandle,'gtk_text_mark_new');
end;

function Available_GtkTextMark: Boolean;
begin
  Result := Assigned(gtk_text_mark_new);
end;

