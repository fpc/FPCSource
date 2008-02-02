{$IfDef read_interface}

type
   PGtkDial = ^TGtkDial;
   TGtkDial = record
        widget : TGtkWidget;
        offscreen_pixmap : PGdkPixmap;
        adjustment : PGtkAdjustment;
        flag0 : word;
        button : guint8;
        radius : gint;
        pointer_width : gint;
        timer : guint32;
        angle : gfloat;
        percentage : gfloat;
        old_value : gfloat;
        old_lower : gfloat;
        old_upper : gfloat;
     end;
   GTK_DIAL = PGtkDial;

const
   bm__GtkDial_policy = $3;
   bp__GtkDial_policy = 0;
   bm__GtkDial_view_only = $4;
   bp__GtkDial_view_only = 2;
function policy(var a : TGtkDial) : guint;
procedure set_policy(var a : TGtkDial; __policy : guint);
function view_only(var a : TGtkDial) : guint;
procedure set_view_only(var a : TGtkDial; __view_only : guint);

type
   PGtkDialClass = ^TGtkDialClass;
   TGtkDialClass = record
        parent_class : TGtkWidgetClass;
     end;
   GTK_DIAL_CLASS = PGtkDialClass;

function GTK_TYPE_DIAL : TGTKType;
function GTK_IS_DIAL(obj : Pointer) : gboolean;
function GTK_IS_DIAL_CLASS(klass : Pointer) : gboolean;

function gtk_dial_new(adjustment:PGtkAdjustment):PGtkWidget;cdecl;external libgnomeuidll name 'gtk_dial_new';
function gtk_dial_get_type:TGTKType;cdecl;external libgnomeuidll name 'gtk_dial_get_type';
function gtk_dial_get_adjustment(dial:PGtkDial):PGtkAdjustment;cdecl;external libgnomeuidll name 'gtk_dial_get_adjustment';
procedure gtk_dial_set_update_policy(dial:PGtkDial; policy:TGtkUpdateType);cdecl;external libgnomeuidll name 'gtk_dial_set_update_policy';
procedure gtk_dial_set_adjustment(dial:PGtkDial; adjustment:PGtkAdjustment);cdecl;external libgnomeuidll name 'gtk_dial_set_adjustment';
function gtk_dial_set_percentage(dial:PGtkDial; percent:gfloat):gfloat;cdecl;external libgnomeuidll name 'gtk_dial_set_percentage';
function gtk_dial_get_percentage(dial:PGtkDial):gfloat;cdecl;external libgnomeuidll name 'gtk_dial_get_percentage';
function gtk_dial_set_value(dial:PGtkDial; value:gfloat):gfloat;cdecl;external libgnomeuidll name 'gtk_dial_set_value';
function gtk_dial_get_value(dial:PGtkDial):gfloat;cdecl;external libgnomeuidll name 'gtk_dial_get_value';
procedure gtk_dial_set_view_only(dial:PGtkDial; view_only:gboolean);cdecl;external libgnomeuidll name 'gtk_dial_set_view_only';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GTK_TYPE_DIAL : TGTKType;
begin
  GTK_TYPE_DIAL:=gtk_dial_get_type;
end;

function GTK_IS_DIAL(obj : Pointer) : gboolean;
begin
   GTK_IS_DIAL:=(obj<>nil) and GTK_IS_DIAL_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GTK_IS_DIAL_CLASS(klass : Pointer) : gboolean;
begin
   GTK_IS_DIAL_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_TYPE_DIAL);
end;

function policy(var a : TGtkDial) : guint;
begin
   policy:=(a.flag0 and bm__GtkDial_policy) shr bp__GtkDial_policy;
end;

procedure set_policy(var a : TGtkDial; __policy : guint);
begin
   a.flag0:=a.flag0 or ((__policy shl bp__GtkDial_policy) and bm__GtkDial_policy);
end;

function view_only(var a : TGtkDial) : guint;
begin
   view_only:=(a.flag0 and bm__GtkDial_view_only) shr bp__GtkDial_view_only;
end;

procedure set_view_only(var a : TGtkDial; __view_only : guint);
begin
   a.flag0:=a.flag0 or ((__view_only shl bp__GtkDial_view_only) and bm__GtkDial_view_only);
end;

{$Endif read_implementation}
