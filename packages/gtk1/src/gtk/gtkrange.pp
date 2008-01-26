{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkRange = ^TGtkRange;
       TGtkRange = record
            widget : TGtkWidget;
            trough : PGdkWindow;
            slider : PGdkWindow;
            step_forw : PGdkWindow;
            step_back : PGdkWindow;
            x_click_point : gint16;
            y_click_point : gint16;
            button : guint8;
            digits : gint8;
            flag0 : {$ifdef win32}longint{$else}word{$endif};
            timer : guint32;
            old_value : gfloat;
            old_lower : gfloat;
            old_upper : gfloat;
            old_page_size : gfloat;
            adjustment : PGtkAdjustment;
         end;

    const
       bm_policy = 3;
       bp_policy = 0;
       bm_scroll_type = 28;
       bp_scroll_type = 2;
       bm_in_child = 224;
       bp_in_child = 5;
       bm_click_child = 1792;
       bp_click_child = 8;
       bm_need_timer = 2048;
       bp_need_timer = 11;
function  policy(var a : TGtkRange) : guint;
procedure set_policy(var a : TGtkRange; __policy : guint);
function  scroll_type(var a : TGtkRange) : guint;
procedure set_scroll_type(var a : TGtkRange; __scroll_type : guint);
function  in_child(var a : TGtkRange) : guint;
procedure set_in_child(var a : TGtkRange; __in_child : guint);
function  click_child(var a : TGtkRange) : guint;
procedure set_click_child(var a : TGtkRange; __click_child : guint);
function  need_timer(var a : TGtkRange) : guint;
procedure set_need_timer(var a : TGtkRange; __need_timer : guint);

    type
       PGtkRangeClass = ^TGtkRangeClass;
       TGtkRangeClass = record
            parent_class : TGtkWidgetClass;
            slider_width : gint;
            stepper_size : gint;
            stepper_slider_spacing : gint;
            min_slider_size : gint;
            trough : guint8;
            slider : guint8;
            step_forw : guint8;
            step_back : guint8;
            draw_background : procedure (range:PGtkRange); cdecl;
            clear_background : procedure (range:PGtkRange);cdecl;
            draw_trough : procedure (range:PGtkRange); cdecl;
            draw_slider : procedure (range:PGtkRange); cdecl;
            draw_step_forw : procedure (range:PGtkRange); cdecl;
            draw_step_back : procedure (range:PGtkRange); cdecl;
            slider_update : procedure (range:PGtkRange); cdecl;
            trough_click : function (range:PGtkRange; x:gint; y:gint; jump_perc:Pgfloat):gint; cdecl;
            trough_keys : function (range:PGtkRange; key:PGdkEventKey; scroll:PGtkScrollType; trough:PGtkTroughType):gint; cdecl;
            motion : procedure (range:PGtkRange; xdelta:gint; ydelta:gint); cdecl;
            timer : function (range:PGtkRange):gint; cdecl;
         end;

Type
  GTK_RANGE=PGtkRange;
  GTK_RANGE_CLASS=PGtkRangeClass;

function  GTK_RANGE_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_range_get_type';
function  GTK_IS_RANGE(obj:pointer):boolean;
function  GTK_IS_RANGE_CLASS(klass:pointer):boolean;

function  gtk_range_get_type:TGtkType;cdecl;external gtkdll name 'gtk_range_get_type';
function  gtk_range_get_adjustment (range:PGtkRange):PGtkAdjustment;cdecl;external gtkdll name 'gtk_range_get_adjustment';
procedure gtk_range_set_update_policy(range:PGtkRange; policy:TGtkUpdateType);cdecl;external gtkdll name 'gtk_range_set_update_policy';
procedure gtk_range_set_adjustment(range:PGtkRange; adjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_range_set_adjustment';
procedure gtk_range_draw_background(range:PGtkRange);cdecl;external gtkdll name 'gtk_range_draw_background';
procedure gtk_range_clear_background(range:PGtkRange);cdecl;external gtkdll name 'gtk_range_clear_background';
procedure gtk_range_draw_trough(range:PGtkRange);cdecl;external gtkdll name 'gtk_range_draw_trough';
procedure gtk_range_draw_slider(range:PGtkRange);cdecl;external gtkdll name 'gtk_range_draw_slider';
procedure gtk_range_draw_step_forw(range:PGtkRange);cdecl;external gtkdll name 'gtk_range_draw_step_forw';
procedure gtk_range_draw_step_back(range:PGtkRange);cdecl;external gtkdll name 'gtk_range_draw_step_back';
procedure gtk_range_slider_update(range:PGtkRange);cdecl;external gtkdll name 'gtk_range_slider_update';
function  gtk_range_trough_click(range:PGtkRange; x:gint; y:gint; jump_perc:Pgfloat):gint;cdecl;external gtkdll name 'gtk_range_trough_click';
procedure gtk_range_default_hslider_update(range:PGtkRange);cdecl;external gtkdll name 'gtk_range_default_hslider_update';
procedure gtk_range_default_vslider_update(range:PGtkRange);cdecl;external gtkdll name 'gtk_range_default_vslider_update';
function  gtk_range_default_htrough_click(range:PGtkRange; x:gint; y:gint; jump_perc:Pgfloat):gint;cdecl;external gtkdll name 'gtk_range_default_htrough_click';
function  gtk_range_default_vtrough_click(range:PGtkRange; x:gint; y:gint; jump_perc:Pgfloat):gint;cdecl;external gtkdll name 'gtk_range_default_vtrough_click';
procedure gtk_range_default_hmotion(range:PGtkRange; xdelta:gint; ydelta:gint);cdecl;external gtkdll name 'gtk_range_default_hmotion';
procedure gtk_range_default_vmotion(range:PGtkRange; xdelta:gint; ydelta:gint);cdecl;external gtkdll name 'gtk_range_default_vmotion';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  policy(var a : TGtkRange) : guint;
      begin
         policy:=(a.flag0 and bm_policy) shr bp_policy;
      end;

procedure set_policy(var a : TGtkRange; __policy : guint);
      begin
         a.flag0:=a.flag0 or ((__policy shl bp_policy) and bm_policy);
      end;

function  scroll_type(var a : TGtkRange) : guint;
      begin
         scroll_type:=(a.flag0 and bm_scroll_type) shr bp_scroll_type;
      end;

procedure set_scroll_type(var a : TGtkRange; __scroll_type : guint);
      begin
         a.flag0:=a.flag0 or ((__scroll_type shl bp_scroll_type) and bm_scroll_type);
      end;

function  in_child(var a : TGtkRange) : guint;
      begin
         in_child:=(a.flag0 and bm_in_child) shr bp_in_child;
      end;

procedure set_in_child(var a : TGtkRange; __in_child : guint);
      begin
         a.flag0:=a.flag0 or ((__in_child shl bp_in_child) and bm_in_child);
      end;

function  click_child(var a : TGtkRange) : guint;
      begin
         click_child:=(a.flag0 and bm_click_child) shr bp_click_child;
      end;

procedure set_click_child(var a : TGtkRange; __click_child : guint);
      begin
         a.flag0:=a.flag0 or ((__click_child shl bp_click_child) and bm_click_child);
      end;

function  need_timer(var a : TGtkRange) : guint;
      begin
         need_timer:=(a.flag0 and bm_need_timer) shr bp_need_timer;
      end;

procedure set_need_timer(var a : TGtkRange; __need_timer : guint);
      begin
         a.flag0:=a.flag0 or ((__need_timer shl bp_need_timer) and bm_need_timer);
      end;

function  GTK_IS_RANGE(obj:pointer):boolean;
begin
  GTK_IS_RANGE:=(obj<>nil) and GTK_IS_RANGE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_RANGE_CLASS(klass:pointer):boolean;
begin
  GTK_IS_RANGE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_RANGE_TYPE);
end;

{$endif read_implementation}


