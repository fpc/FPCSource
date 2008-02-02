// included by gtk2.pas

{$IFDEF read_forward_definitions}
   PGtkStyle = ^TGtkStyle;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  // internal type
  PGtkIconSource = pointer;


{ Some forward declarations needed to rationalize the header
   files.
  }
   TGtkRcPropertyParser = function (pspec:PGParamSpec; rc_string:PGString; property_value:PGValue):gboolean; cdecl;
{ We make this forward declaration here, since we pass
   GtkWidget's to the draw functions.
  }


{< public > }
{ Halfway between text/base  }
{< private > }
{ Font description for style->private_font or %NULL  }
{ the RcStyle from which this style was created  }
{ of type GtkStyle   }
{ of type GtkIconFactory   }
   TGtkStyle = record
        parent_instance : TGObject;
        fg : array[0..4] of TGdkColor;
        bg : array[0..4] of TGdkColor;
        light : array[0..4] of TGdkColor;
        dark : array[0..4] of TGdkColor;
        mid : array[0..4] of TGdkColor;
        text : array[0..4] of TGdkColor;
        base : array[0..4] of TGdkColor;
        text_aa : array[0..4] of TGdkColor;
        black : TGdkColor;
        white : TGdkColor;
        font_desc : PPangoFontDescription;
        xthickness : gint;
        ythickness : gint;
        fg_gc : array[0..4] of PGdkGC;
        bg_gc : array[0..4] of PGdkGC;
        light_gc : array[0..4] of PGdkGC;
        dark_gc : array[0..4] of PGdkGC;
        mid_gc : array[0..4] of PGdkGC;
        text_gc : array[0..4] of PGdkGC;
        base_gc : array[0..4] of PGdkGC;
        text_aa_gc : array[0..4] of PGdkGC;
        black_gc : PGdkGC;
        white_gc : PGdkGC;
        bg_pixmap : array[0..4] of PGdkPixmap;
        attach_count : gint;
        depth : gint;
        colormap : PGdkColormap;
        private_font : PGdkFont;
        private_font_desc : PPangoFontDescription;
        rc_style : PGtkRcStyle;
        styles : PGSList;
        property_cache : PGArray;
        icon_factories : PGSList;
     end;

{ Initialize for a particular colormap/depth
     combination. style->colormap/style->depth will have
     been set at this point. Will typically chain to parent.
    }
{ Clean up for a particular colormap/depth combination. Will
     typically chain to parent.
    }
{ Make style an exact duplicate of src.
    }
{ Create an empty style of the same type as this style.
     The default implementation, which does
     g_object_new (G_OBJECT_TYPE (style), NULL);
     should work in most cases.
    }
{ Initialize the GtkStyle with the values in the GtkRcStyle.
     should chain to the parent implementation.
    }
{ Drawing functions
    }
{ Padding for future expansion  }
   PGtkStyleClass = ^TGtkStyleClass;
   TGtkStyleClass = record
        parent_class : TGObjectClass;
        realize : procedure (style:PGtkStyle); cdecl;
        unrealize : procedure (style:PGtkStyle); cdecl;
        copy : procedure (style:PGtkStyle; src:PGtkStyle); cdecl;
        clone : function (style:PGtkStyle):PGtkStyle; cdecl;
        init_from_rc : procedure (style:PGtkStyle; rc_style:PGtkRcStyle); cdecl;
        set_background : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType); cdecl;
        render_icon : function (style:PGtkStyle; source:PGtkIconSource; direction:TGtkTextDirection; state:TGtkStateType; size:TGtkIconSize;
                     widget:PGtkWidget; detail:Pgchar):PGdkPixbuf; cdecl;
        draw_hline : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget;
                      detail:Pgchar; x1:gint; x2:gint; y:gint); cdecl;
        draw_vline : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget;
                      detail:Pgchar; y1:gint; y2:gint; x:gint); cdecl;
        draw_shadow : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
                      height:gint); cdecl;
        draw_polygon : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; point:PGdkPoint; npoints:gint; fill:gboolean); cdecl;
        draw_arrow : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; arrow_type:TGtkArrowType; fill:gboolean; x:gint;
                      y:gint; width:gint; height:gint); cdecl;
        draw_diamond : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
                      height:gint); cdecl;
        draw_string : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget;
                      detail:Pgchar; x:gint; y:gint; _string:Pgchar); cdecl;
        draw_box : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
                      height:gint); cdecl;
        draw_flat_box : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
                      height:gint); cdecl;
        draw_check : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
                      height:gint); cdecl;
        draw_option : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
                      height:gint); cdecl;
        draw_tab : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
                      height:gint); cdecl;
        draw_shadow_gap : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
                      height:gint; gap_side:TGtkPositionType; gap_x:gint; gap_width:gint); cdecl;
        draw_box_gap : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
                      height:gint; gap_side:TGtkPositionType; gap_x:gint; gap_width:gint); cdecl;
        draw_extension : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
                      height:gint; gap_side:TGtkPositionType); cdecl;
        draw_focus : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget;
                      detail:Pgchar; x:gint; y:gint; width:gint; height:gint); cdecl;
        draw_slider : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
                      height:gint; orientation:TGtkOrientation); cdecl;
        draw_handle : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
                      height:gint; orientation:TGtkOrientation); cdecl;
        draw_expander : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget;
                      detail:Pgchar; x:gint; y:gint; expander_style:TGtkExpanderStyle); cdecl;
        draw_layout : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; use_text:gboolean; area:PGdkRectangle;
                      widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; layout:PPangoLayout); cdecl;
        draw_resize_grip : procedure (style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget;
                      detail:Pgchar; edge:TGdkWindowEdge; x:gint; y:gint; width:gint;
                      height:gint); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
        _gtk_reserved5 : procedure ; cdecl;
        _gtk_reserved6 : procedure ; cdecl;
        _gtk_reserved7 : procedure ; cdecl;
        _gtk_reserved8 : procedure ; cdecl;
        _gtk_reserved9 : procedure ; cdecl;
        _gtk_reserved10 : procedure ; cdecl;
        _gtk_reserved11 : procedure ; cdecl;
        _gtk_reserved12 : procedure ; cdecl;
     end;

   PGtkBorder = ^TGtkBorder;
   TGtkBorder = record
        left : gint;
        right : gint;
        top : gint;
        bottom : gint;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_STYLE : GType;
function GTK_STYLE(anObject: pointer) : PGtkStyle;
function GTK_STYLE_CLASS(klass: pointer) : PGtkStyleClass;
function GTK_IS_STYLE(anObject: pointer) : boolean;
function GTK_IS_STYLE_CLASS(klass: pointer) : boolean;
function GTK_STYLE_GET_CLASS(obj: pointer) : PGtkStyleClass;
function GTK_TYPE_BORDER : GType;
function GTK_STYLE_ATTACHED(style : pointer) : boolean;


function gtk_style_get_type:GType; cdecl; external gtklib;
function gtk_style_new:PGtkStyle; cdecl; external gtklib;
function gtk_style_copy(style:PGtkStyle):PGtkStyle; cdecl; external gtklib;
function gtk_style_attach(style:PGtkStyle; window:PGdkWindow):PGtkStyle; cdecl; external gtklib;
procedure gtk_style_detach(style:PGtkStyle); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_style_ref(style:PGtkStyle):PGtkStyle; cdecl; external gtklib;
procedure gtk_style_unref(style:PGtkStyle); cdecl; external gtklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gtk_style_get_font(style:PGtkStyle):PGdkFont; cdecl; external gtklib;
{$endif}

function gtk_style_get_font_for_display(display:PGdkDisplay; style:PGtkStyle):PGdkFont; cdecl; external gtklib;
procedure gtk_style_set_font(style:PGtkStyle; font:PGdkFont); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

procedure gtk_style_set_background(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType); cdecl; external gtklib;
procedure gtk_style_apply_default_background(style:PGtkStyle; window:PGdkWindow; set_bg:gboolean; state_type:TGtkStateType; area:PGdkRectangle;
            x:gint; y:gint; width:gint; height:gint); cdecl; external gtklib;
function gtk_style_lookup_icon_set(style:PGtkStyle; stock_id:Pgchar):PGtkIconSet; cdecl; external gtklib;
function gtk_style_render_icon(style:PGtkStyle; source:PGtkIconSource; direction:TGtkTextDirection; state:TGtkStateType; size:TGtkIconSize;
           widget:PGtkWidget; detail:Pgchar):PGdkPixbuf; cdecl; external gtklib;
{$ifndef GTK_DISABLE_DEPRECATED}

procedure gtk_draw_hline(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; x1:gint; x2:gint;
            y:gint); cdecl; external gtklib;
procedure gtk_draw_vline(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; y1:gint; y2:gint;
            x:gint); cdecl; external gtklib;
procedure gtk_draw_shadow(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint;
            y:gint; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_draw_polygon(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; points:PGdkPoint;
            npoints:gint; fill:gboolean); cdecl; external gtklib;
procedure gtk_draw_arrow(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; arrow_type:TGtkArrowType;
            fill:gboolean; x:gint; y:gint; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_draw_diamond(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint;
            y:gint; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_draw_box(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint;
            y:gint; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_draw_flat_box(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint;
            y:gint; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_draw_check(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint;
            y:gint; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_draw_option(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint;
            y:gint; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_draw_tab(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint;
            y:gint; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_draw_shadow_gap(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint;
            y:gint; width:gint; height:gint; gap_side:TGtkPositionType; gap_x:gint;
            gap_width:gint); cdecl; external gtklib;
procedure gtk_draw_box_gap(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint;
            y:gint; width:gint; height:gint; gap_side:TGtkPositionType; gap_x:gint;
            gap_width:gint); cdecl; external gtklib;
procedure gtk_draw_extension(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint;
            y:gint; width:gint; height:gint; gap_side:TGtkPositionType); cdecl; external gtklib;
procedure gtk_draw_focus(style:PGtkStyle; window:PGdkWindow; x:gint; y:gint; width:gint;
            height:gint); cdecl; external gtklib;
procedure gtk_draw_slider(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint;
            y:gint; width:gint; height:gint; orientation:TGtkOrientation); cdecl; external gtklib;
procedure gtk_draw_handle(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint;
            y:gint; width:gint; height:gint; orientation:TGtkOrientation); cdecl; external gtklib;
procedure gtk_draw_expander(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; x:gint; y:gint;
            expander_style:TGtkExpanderStyle); cdecl; external gtklib;
procedure gtk_draw_layout(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; use_text:gboolean; x:gint;
            y:gint; layout:PPangoLayout); cdecl; external gtklib;
procedure gtk_draw_resize_grip(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; edge:TGdkWindowEdge; x:gint;
            y:gint; width:gint; height:gint); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

procedure gtk_paint_hline(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget;
            detail:Pgchar; x1:gint; x2:gint; y:gint); cdecl; external gtklib;
procedure gtk_paint_vline(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget;
            detail:Pgchar; y1:gint; y2:gint; x:gint); cdecl; external gtklib;
procedure gtk_paint_shadow(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
            height:gint); cdecl; external gtklib;
procedure gtk_paint_polygon(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; points:PGdkPoint; npoints:gint; fill:gboolean); cdecl; external gtklib;
procedure gtk_paint_arrow(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; arrow_type:TGtkArrowType; fill:gboolean; x:gint;
            y:gint; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_paint_diamond(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
            height:gint); cdecl; external gtklib;
procedure gtk_paint_box(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
            height:gint); cdecl; external gtklib;
procedure gtk_paint_flat_box(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
            height:gint); cdecl; external gtklib;
procedure gtk_paint_check(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
            height:gint); cdecl; external gtklib;
procedure gtk_paint_option(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
            height:gint); cdecl; external gtklib;
procedure gtk_paint_tab(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
            height:gint); cdecl; external gtklib;
procedure gtk_paint_shadow_gap(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
            height:gint; gap_side:TGtkPositionType; gap_x:gint; gap_width:gint); cdecl; external gtklib;
procedure gtk_paint_box_gap(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
            height:gint; gap_side:TGtkPositionType; gap_x:gint; gap_width:gint); cdecl; external gtklib;
procedure gtk_paint_extension(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
            height:gint; gap_side:TGtkPositionType); cdecl; external gtklib;
procedure gtk_paint_focus(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget;
            detail:Pgchar; x:gint; y:gint; width:gint; height:gint); cdecl; external gtklib;
procedure gtk_paint_slider(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
            height:gint; orientation:TGtkOrientation); cdecl; external gtklib;
procedure gtk_paint_handle(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint;
            height:gint; orientation:TGtkOrientation); cdecl; external gtklib;
procedure gtk_paint_expander(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget;
            detail:Pgchar; x:gint; y:gint; expander_style:TGtkExpanderStyle); cdecl; external gtklib;
procedure gtk_paint_layout(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; use_text:gboolean; area:PGdkRectangle;
            widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; layout:PPangoLayout); cdecl; external gtklib;
procedure gtk_paint_resize_grip(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget;
            detail:Pgchar; edge:TGdkWindowEdge; x:gint; y:gint; width:gint;
            height:gint); cdecl; external gtklib;
function gtk_border_get_type:GType; cdecl; external gtklib;
function gtk_border_copy(border:PGtkBorder):PGtkBorder; cdecl; external gtklib;
procedure gtk_border_free(border:PGtkBorder); cdecl; external gtklib;
{ --- private API ---  }
function _gtk_style_peek_property_value(style:PGtkStyle; widget_type:GType; pspec:PGParamSpec; parser:TGtkRcPropertyParser):PGValue; cdecl; external gtklib;

{ deprecated  }
{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_style_apply_default_pixmap(style:PGtkStyle; window:PGdkWindow;
  state_type:TGtkStateType; area:PGdkRectangle;
  x:gint; y:gint; width:gint; height:gint);

procedure gtk_draw_string(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; x:gint; y:gint;
            _string:Pgchar); cdecl; external gtklib;
procedure gtk_paint_string(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget;
            detail:Pgchar; x:gint; y:gint; _string:Pgchar); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

function _gtk_get_insertion_cursor_gc(widget:PGtkWidget; is_primary:gboolean):PGdkGC; cdecl; external gtklib;
procedure _gtk_draw_insertion_cursor(widget:PGtkWidget; drawable:PGdkDrawable; gc:PGdkGC; location:PGdkRectangle; direction:TGtkTextDirection;
            draw_arrow:gboolean); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_STYLE : GType;
begin
   GTK_TYPE_STYLE:=gtk_style_get_type;
end;

function GTK_STYLE(anObject: pointer) : PGtkStyle;
begin
   GTK_STYLE:=PGtkStyle(G_TYPE_CHECK_INSTANCE_CAST(anObject,GTK_TYPE_STYLE));
end;

function GTK_STYLE_CLASS(klass: pointer) : PGtkStyleClass;
begin
   GTK_STYLE_CLASS:=PGtkStyleClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_STYLE));
end;

function GTK_IS_STYLE(anObject: pointer) : boolean;
begin
   GTK_IS_STYLE:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GTK_TYPE_STYLE);
end;

function GTK_IS_STYLE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_STYLE_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_STYLE);
end;

function GTK_STYLE_GET_CLASS(obj: pointer) : PGtkStyleClass;
begin
   GTK_STYLE_GET_CLASS:=PGtkStyleClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_STYLE));
end;

function GTK_TYPE_BORDER : GType;
begin
   GTK_TYPE_BORDER:=gtk_border_get_type;
end;

function GTK_STYLE_ATTACHED(style : pointer) : boolean;
begin
   GTK_STYLE_ATTACHED:=((GTK_STYLE(style))^.attach_count) > 0;
end;

procedure gtk_style_apply_default_pixmap(style:PGtkStyle; window:PGdkWindow;
  state_type:TGtkStateType; area:PGdkRectangle;
  x:gint; y:gint; width:gint; height:gint);
begin
   gtk_style_apply_default_background(style,window,true,state_type,area,x,y,width,height);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

