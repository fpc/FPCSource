{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkTooltipsData = ^TGtkTooltipsData;
     PGtkTooltips = ^TGtkTooltips;
     TGtkTooltipsData = record
          tooltips : PGtkTooltips;
          widget : PGtkWidget;
          tip_text : Pgchar;
          tip_private : Pgchar;
          font : PGdkFont;
          width : gint;
          row : PGList;
       end;

     TGtkTooltips = record
          data : TGtkData;
          tip_window : PGtkWidget;
          active_tips_data : PGtkTooltipsData;
          tips_data_list : PGList;
          gc : PGdkGC;
          foreground : PGdkColor;
          background : PGdkColor;
          flag0 : longint;
          timer_tag : gint;
       end;

  const
     bm_TGtkTooltips_delay = $3FFFFFFF;
     bp_TGtkTooltips_delay = 0;
     bm_TGtkTooltips_enabled = $40000000;
     bp_TGtkTooltips_enabled = 30;
function  delay(var a : TGtkTooltips) : guint;
procedure set_delay(var a : TGtkTooltips; __delay : guint);
function  enabled(var a : TGtkTooltips) : guint;
procedure set_enabled(var a : TGtkTooltips; __enabled : guint);

  type
     PGtkTooltipsClass = ^TGtkTooltipsClass;
     TGtkTooltipsClass = record
          parent_class : TGtkDataClass;
       end;

Type
  GTK_TOOLTIPS=PGtkTooltips;
  GTK_TOOLTIPS_CLASS=PGtkTooltipsClass;

function  GTK_TOOLTIPS_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_tooltips_get_type';
function  GTK_IS_TOOLTIPS(obj:pointer):boolean;
function  GTK_IS_TOOLTIPS_CLASS(klass:pointer):boolean;

function  gtk_tooltips_get_type:TGtkType;cdecl;external gtkdll name 'gtk_tooltips_get_type';
function  gtk_tooltips_new:PGtkTooltips;cdecl;external gtkdll name 'gtk_tooltips_new';
procedure gtk_tooltips_enable(tooltips:PGtkTooltips);cdecl;external gtkdll name 'gtk_tooltips_enable';
procedure gtk_tooltips_disable(tooltips:PGtkTooltips);cdecl;external gtkdll name 'gtk_tooltips_disable';
procedure gtk_tooltips_set_delay(tooltips:PGtkTooltips; delay:guint);cdecl;external gtkdll name 'gtk_tooltips_set_delay';
procedure gtk_tooltips_set_tip(tooltips:PGtkTooltips; widget:PGtkWidget; tip_text:Pgchar; tip_private:Pgchar);cdecl;external gtkdll name 'gtk_tooltips_set_tip';
procedure gtk_tooltips_set_colors(tooltips:PGtkTooltips; background:PGdkColor; foreground:PGdkColor);cdecl;external gtkdll name 'gtk_tooltips_set_colors';
function  gtk_tooltips_data_get(widget:PGtkWidget):PGtkTooltipsData;cdecl;external gtkdll name 'gtk_tooltips_data_get';
procedure gtk_tooltips_force_window(tooltips:PGtkTooltips);cdecl;external gtkdll name 'gtk_tooltips_force_window';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  delay(var a : TGtkTooltips) : guint;
    begin
       delay:=(a.flag0 and bm_TGtkTooltips_delay) shr bp_TGtkTooltips_delay;
    end;

procedure set_delay(var a : TGtkTooltips; __delay : guint);
    begin
       a.flag0:=a.flag0 or ((__delay shl bp_TGtkTooltips_delay) and bm_TGtkTooltips_delay);
    end;

function  enabled(var a : TGtkTooltips) : guint;
    begin
       enabled:=(a.flag0 and bm_TGtkTooltips_enabled) shr bp_TGtkTooltips_enabled;
    end;

procedure set_enabled(var a : TGtkTooltips; __enabled : guint);
    begin
       a.flag0:=a.flag0 or ((__enabled shl bp_TGtkTooltips_enabled) and bm_TGtkTooltips_enabled);
    end;

function  GTK_IS_TOOLTIPS(obj:pointer):boolean;
begin
  GTK_IS_TOOLTIPS:=(obj<>nil) and GTK_IS_TOOLTIPS_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_TOOLTIPS_CLASS(klass:pointer):boolean;
begin
  GTK_IS_TOOLTIPS_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_TOOLTIPS_TYPE);
end;

{$endif read_implementation}


