
var
  gtk_text_iter_forward_visible_line: function(iter: PGtkTextIter): gboolean;cdecl;
  gtk_text_iter_backward_visible_line: function(iter: PGtkTextIter): gboolean;cdecl;
  gtk_text_iter_forward_visible_lines: function(iter: PGtkTextIter; count: gint): gboolean;cdecl;
  gtk_text_iter_backward_visible_lines: function(iter: PGtkTextIter; count: gint): gboolean;cdecl;

  function Available_GtkTextIter: Boolean;
