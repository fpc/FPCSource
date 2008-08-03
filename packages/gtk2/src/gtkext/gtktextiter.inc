
procedure Freegtkiter;
begin
  gtk_text_iter_forward_visible_line:=nil;
  gtk_text_iter_backward_visible_line:=nil;
  gtk_text_iter_forward_visible_lines:=nil;
  gtk_text_iter_backward_visible_lines:=nil;
end;


procedure Loadgtkiter;
begin
  Freegtkiter;

  pointer(gtk_text_iter_forward_visible_line):=GetProcAddress(gtkhandle,'gtk_text_iter_forward_visible_line');
  pointer(gtk_text_iter_backward_visible_line):=GetProcAddress(gtkhandle,'gtk_text_iter_backward_visible_line');
  pointer(gtk_text_iter_forward_visible_lines):=GetProcAddress(gtkhandle,'gtk_text_iter_forward_visible_lines');
  pointer(gtk_text_iter_backward_visible_lines):=GetProcAddress(gtkhandle,'gtk_text_iter_backward_visible_lines');
end;

function Available_GtkTextIter: Boolean;
begin
  Result := Assigned(gtk_text_iter_forward_visible_line);
end;

