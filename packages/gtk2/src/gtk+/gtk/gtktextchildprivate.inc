// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkTextPixbuf = ^TGtkTextPixbuf;
   TGtkTextPixbuf = record
        pixbuf : PGdkPixbuf;
     end;

   PGtkTextChildBody = ^TGtkTextChildBody;
   TGtkTextChildBody = record
        obj : PGtkTextChildAnchor;
        widgets : PGSList;
        tree : PGtkTextBTree;
        line : PGtkTextLine;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

function _gtk_pixbuf_segment_new(pixbuf:PGdkPixbuf):PGtkTextLineSegment; cdecl; external gtklib;

function _gtk_widget_segment_new(anchor:PGtkTextChildAnchor):PGtkTextLineSegment; cdecl; external gtklib;
procedure _gtk_widget_segment_add(widget_segment:PGtkTextLineSegment; child:PGtkWidget); cdecl; external gtklib;
procedure _gtk_widget_segment_remove(widget_segment:PGtkTextLineSegment; child:PGtkWidget); cdecl; external gtklib;
procedure _gtk_widget_segment_ref(widget_segment:PGtkTextLineSegment); cdecl; external gtklib;
procedure _gtk_widget_segment_unref(widget_segment:PGtkTextLineSegment); cdecl; external gtklib;
function _gtk_anchored_child_get_layout(child:PGtkWidget):PGtkTextLayout; cdecl; external gtklib;
{$ENDIF read_interface_rest}

