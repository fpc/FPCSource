{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkToggleButton = ^TGtkToggleButton;
       TGtkToggleButton = record
          button : TGtkButton;
          flag0 : longint;
          event_window : PGdkWindow;
        end;

  const
     bm_TGtkToggleButton_active = $1;
     bp_TGtkToggleButton_active = 0;
     bm_TGtkToggleButton_draw_indicator = $2;
     bp_TGtkToggleButton_draw_indicator = 1;
function  active(var a : TGtkToggleButton) : guint;
procedure set_active(var a : TGtkToggleButton; __active : guint);
function  draw_indicator(var a : TGtkToggleButton) : guint;
procedure set_draw_indicator(var a : TGtkToggleButton; __draw_indicator : guint);

    type
       PGtkToggleButtonClass = ^TGtkToggleButtonClass;
       TGtkToggleButtonClass = record
            parent_class : TGtkButtonClass;
            toggled : procedure (toggle_button:PGtkToggleButton); cdecl;
         end;

Type
  GTK_TOGGLE_BUTTON=PGtkToggleButton;
  GTK_TOGGLE_BUTTON_CLASS=PGtkToggleButtonClass;

function  GTK_TOGGLE_BUTTON_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_toggle_button_get_type';
function  GTK_IS_TOGGLE_BUTTON(obj:pointer):boolean;
function  GTK_IS_TOGGLE_BUTTON_CLASS(klass:pointer):boolean;

function  gtk_toggle_button_get_type:TGtkType;cdecl;external gtkdll name 'gtk_toggle_button_get_type';
function  gtk_toggle_button_new : PGtkWidget;cdecl;external gtkdll name 'gtk_toggle_button_new';
function  gtk_toggle_button_new_with_label (thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_toggle_button_new_with_label';
procedure gtk_toggle_button_set_mode(toggle_button:PGtkToggleButton; draw_indicator:gint);cdecl;external gtkdll name 'gtk_toggle_button_set_mode';
procedure gtk_toggle_button_set_active(toggle_button:PGtkToggleButton; is_active:gboolean);cdecl;external gtkdll name 'gtk_toggle_button_set_active';
function  gtk_toggle_button_get_active(toggle_button:PGtkToggleButton):gboolean;cdecl;external gtkdll name 'gtk_toggle_button_get_active';
procedure gtk_toggle_button_toggled(toggle_button:PGtkToggleButton);cdecl;external gtkdll name 'gtk_toggle_button_toggled';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  active(var a : TGtkToggleButton) : guint;
    begin
       active:=(a.flag0 and bm_TGtkToggleButton_active) shr bp_TGtkToggleButton_active;
    end;

procedure set_active(var a : TGtkToggleButton; __active : guint);
    begin
       a.flag0:=a.flag0 or ((__active shl bp_TGtkToggleButton_active) and bm_TGtkToggleButton_active);
    end;

function  draw_indicator(var a : TGtkToggleButton) : guint;
    begin
       draw_indicator:=(a.flag0 and bm_TGtkToggleButton_draw_indicator) shr bp_TGtkToggleButton_draw_indicator;
    end;

procedure set_draw_indicator(var a : TGtkToggleButton; __draw_indicator : guint);
    begin
       a.flag0:=a.flag0 or ((__draw_indicator shl bp_TGtkToggleButton_draw_indicator) and bm_TGtkToggleButton_draw_indicator);
    end;

function  GTK_IS_TOGGLE_BUTTON(obj:pointer):boolean;
begin
  GTK_IS_TOGGLE_BUTTON:=(obj<>nil) and GTK_IS_TOGGLE_BUTTON_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_TOGGLE_BUTTON_CLASS(klass:pointer):boolean;
begin
  GTK_IS_TOGGLE_BUTTON_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_TOGGLE_BUTTON_TYPE);
end;

{$endif read_implementation}


