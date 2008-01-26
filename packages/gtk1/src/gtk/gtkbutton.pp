{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkButton = ^TGtkButton;
       TGtkButton = record
            bin : TGtkBin;
            child : PGtkWidget;
            flag0 : {$ifdef win32}longint{$else}word{$endif};
         end;

    const
       bm_in_button = 1;
       bp_in_button = 0;
       bm_button_down = 2;
       bp_button_down = 1;
       bm_relief = 4;
       bp_relief = 2;
function  in_button(var a : TGtkButton) : guint;
procedure set_in_button(var a : TGtkButton; __in_button : guint);
function  button_down(var a : TGtkButton) : guint;
procedure set_button_down(var a : TGtkButton; __button_down : guint);
function  relief(var a : TGtkButton) : guint;
procedure set_relief(var a : TGtkButton; __button_down : guint);

    type
       PGtkButtonClass = ^TGtkButtonClass;
       TGtkButtonClass = record
            parent_class : TGtkBinClass;
            pressed : procedure (button:PGtkButton); cdecl;
            released : procedure (button:PGtkButton); cdecl;
            clicked : procedure (button:PGtkButton); cdecl;
            enter : procedure (button:PGtkButton); cdecl;
            leave : procedure (button:PGtkButton); cdecl;
         end;

Type
  GTK_BUTTON=PGtkButton;
  GTK_BUTTON_CLASS=PGtkButtonClass;

function  GTK_BUTTON_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_button_get_type';
function  GTK_IS_BUTTON(obj:pointer):boolean;
function  GTK_IS_BUTTON_CLASS(klass:pointer):boolean;

function  gtk_button_get_type:TGtkType;cdecl;external gtkdll name 'gtk_button_get_type';
function  gtk_button_new : PGtkWidget;cdecl;external gtkdll name 'gtk_button_new';
function  gtk_button_new_with_label (thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_button_new_with_label';
procedure gtk_button_pressed(button:PGtkButton);cdecl;external gtkdll name 'gtk_button_pressed';
procedure gtk_button_released(button:PGtkButton);cdecl;external gtkdll name 'gtk_button_released';
procedure gtk_button_clicked(button:PGtkButton);cdecl;external gtkdll name 'gtk_button_clicked';
procedure gtk_button_enter(button:PGtkButton);cdecl;external gtkdll name 'gtk_button_enter';
procedure gtk_button_leave(button:PGtkButton);cdecl;external gtkdll name 'gtk_button_leave';
procedure gtk_button_set_relief(button:PGtkButton; newstyle:TGtkReliefStyle);cdecl;external gtkdll name 'gtk_button_set_relief';
function  gtk_button_get_relief(button:PGtkButton):TGtkReliefStyle;cdecl;external gtkdll name 'gtk_button_get_relief';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  in_button(var a : TGtkButton) : guint;
      begin
         in_button:=(a.flag0 and bm_in_button) shr bp_in_button;
      end;

procedure set_in_button(var a : TGtkButton; __in_button : guint);
      begin
         a.flag0:=a.flag0 or ((__in_button shl bp_in_button) and bm_in_button);
      end;

function  button_down(var a : TGtkButton) : guint;
      begin
         button_down:=(a.flag0 and bm_button_down) shr bp_button_down;
      end;

procedure set_button_down(var a : TGtkButton; __button_down : guint);
      begin
         a.flag0:=a.flag0 or ((__button_down shl bp_button_down) and bm_button_down);
      end;

function  relief(var a : TGtkButton) : guint;
      begin
         relief:=(a.flag0 and bm_relief) shr bp_relief;
      end;

procedure set_relief(var a : TGtkButton; __button_down : guint);
      begin
         a.flag0:=a.flag0 or ((__button_down shl bp_relief) and bm_relief);
      end;

function  GTK_IS_BUTTON(obj:pointer):boolean;
begin
  GTK_IS_BUTTON:=(obj<>nil) and GTK_IS_BUTTON_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_BUTTON_CLASS(klass:pointer):boolean;
begin
  GTK_IS_BUTTON_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_BUTTON_TYPE);
end;

{$endif read_implementation}


