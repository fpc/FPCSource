{$IfDef read_interface}

type
   PGnomeScores = ^TGnomeScores;
   TGnomeScores = record
        dialog : TGnomeDialog;
        but_clear : PGtkWidget;
        n_scores : guint;
        logo : PGtkWidget;
        label_names : PPGtkWidget;
        label_scores : PPGtkWidget;
        label_times : PPGtkWidget;
     end;
   GNOMES_SCORES = PGnomeScores;

   PGnomeScoresClass = ^TGnomeScoresClass;
   TGnomeScoresClass = record
        parent_class : TGnomeDialogClass;
     end;
   GNOMES_SCORES_CLASS = PGnomeScoresClass;

function GNOME_TYPE_SCORES : TGTKType;
function GNOME_IS_SCORES(obj : Pointer) : Boolean;
function GNOME_IS_SCORES_CLASS(klass : Pointer) : Boolean;

function gnome_scores_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_scores_get_type';
procedure gnome_scores_display(title:Pgchar; app_name:Pgchar; level:Pgchar; pos:longint);cdecl;external libgnomeuidll name 'gnome_scores_display';
function gnome_scores_new(n_scores:guint; names:PPgchar; scores:Pgfloat; times:Ptime_t; clear:guint):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_scores_new';
procedure gnome_scores_set_logo_label(gs:PGnomeScores; txt:Pgchar; font:Pgchar; color:PGdkColor);cdecl;external libgnomeuidll name 'gnome_scores_set_logo_label';
procedure gnome_scores_set_logo_pixmap(gs:PGnomeScores; logo:Pgchar);cdecl;external libgnomeuidll name 'gnome_scores_set_logo_pixmap';
procedure gnome_scores_set_logo_widget(gs:PGnomeScores; w:PGtkWidget);cdecl;external libgnomeuidll name 'gnome_scores_set_logo_widget';
procedure gnome_scores_set_color(gs:PGnomeScores; pos:guint; col:PGdkColor);cdecl;external libgnomeuidll name 'gnome_scores_set_color';
procedure gnome_scores_set_def_color(gs:PGnomeScores; col:PGdkColor);cdecl;external libgnomeuidll name 'gnome_scores_set_def_color';
procedure gnome_scores_set_colors(gs:PGnomeScores; col:PGdkColor);cdecl;external libgnomeuidll name 'gnome_scores_set_colors';
procedure gnome_scores_set_logo_label_title(gs:PGnomeScores; txt:Pgchar);cdecl;external libgnomeuidll name 'gnome_scores_set_logo_label_title';
procedure gnome_scores_set_current_player(gs:PGnomeScores; i:gint);cdecl;external libgnomeuidll name 'gnome_scores_set_current_player';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_SCORES : TGTKType;
begin
  GNOME_TYPE_SCORES:=gnome_scores_get_type;
end;

function GNOME_IS_SCORES(obj : Pointer) : Boolean;
begin
   GNOME_IS_SCORES:=(obj<>nil) and GNOME_IS_SCORES_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_SCORES_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_SCORES_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_SCORES);
end;

{$Endif read_implementation}
