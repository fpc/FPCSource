{$IfDef read_interface}

type
   TGnomeProcbarCallback = function (_para1:gpointer):gint; cdecl;

   PGnomeProcBar = ^TGnomeProcBar;
   TGnomeProcBar = record
        hbox : TGtkHBox;
        bar : PGtkWidget;
        thelabel : PGtkWidget;
        frame : PGtkWidget;
        flag0 : word;
        bs : PGdkPixmap;
        colors : PGdkColor;
        colors_allocated : gint;
        first_request : gint;
        n : gint;
        tag : gint;
        last : Pdword;
        cb : TGnomeProcbarCallback;
        cb_data : gpointer;
     end;
   GNOME_PROC_BAR = PGnomeProcBar;

const
   bm__GnomeProcBar_vertical = $1;
   bp__GnomeProcBar_vertical = 0;
function vertical(var a : TGnomeProcBar) : gboolean;
procedure set_vertical(var a : TGnomeProcBar; __vertical : gboolean);
type
   PGnomeProcBarClass = ^TGnomeProcBarClass;
   TGnomeProcBarClass = record
        parent_class : TGtkHBoxClass;
     end;
   GNOME_PROC_BAR_CLASS = PGnomeProcBarClass;

function GNOME_TYPE_PROC_BAR : TGTKType;
function GNOME_IS_PROC_BAR(obj : Pointer) : Boolean;
function GNOME_IS_PROC_BAR_CLASS(klass : Pointer) : Boolean;

function gnome_proc_bar_get_type:TGTKTYPE;cdecl;external libgnomeuidll name 'gnome_proc_bar_get_type';
function gnome_proc_bar_new(thelabel:PGtkWidget; n:gint; colors:PGdkColor; cb : TGnomeProcbarCallback):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_proc_bar_new';
procedure gnome_proc_bar_construct(pb:PGnomeProcBar; thelabel:PGtkWidget; n:gint; colors:PGdkColor; cb:TGnomeProcbarCallback);cdecl;external libgnomeuidll name 'gnome_proc_bar_construct';
procedure gnome_proc_bar_set_values(pb:PGnomeProcBar; val:Pguint);cdecl;external libgnomeuidll name 'gnome_proc_bar_set_values';
procedure gnome_proc_bar_set_orient(pb:PGnomeProcBar; vertical:gboolean);cdecl;external libgnomeuidll name 'gnome_proc_bar_set_orient';
procedure gnome_proc_bar_start(pb:PGnomeProcBar; gtime:gint; data:gpointer);cdecl;external libgnomeuidll name 'gnome_proc_bar_start';
procedure gnome_proc_bar_stop(pb:PGnomeProcBar);cdecl;external libgnomeuidll name 'gnome_proc_bar_stop';
procedure gnome_proc_bar_update(pb:PGnomeProcBar; colors:PGdkColor);cdecl;external libgnomeuidll name 'gnome_proc_bar_update';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_PROC_BAR : TGTKType;
begin
  GNOME_TYPE_PROC_BAR:=gnome_proc_bar_get_type;
end;

function GNOME_IS_PROC_BAR(obj : Pointer) : Boolean;
begin
   GNOME_IS_PROC_BAR:=(obj<>nil) and GNOME_IS_PROC_BAR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_PROC_BAR_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_PROC_BAR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_PROC_BAR);
end;

function vertical(var a : TGnomeProcBar) : gboolean;
begin
   vertical:=gboolean((a.flag0 and bm__GnomeProcBar_vertical) shr bp__GnomeProcBar_vertical);
end;

procedure set_vertical(var a : TGnomeProcBar; __vertical : gboolean);
begin
   a.flag0:=a.flag0 or ((gint(__vertical) shl bp__GnomeProcBar_vertical) and bm__GnomeProcBar_vertical);
end;

{$Endif read_implementation}
