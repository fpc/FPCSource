{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

{********************************
   Types inserted in gtkwidget
********************************}

    const
       GTK_STYLE_NUM_STYLECOLORS = 35;

function  GTK_STYLE_ATTACHED(style : PGtkstyle) : boolean;

function  gtk_style_new : PGtkStyle;cdecl;external gtkdll name 'gtk_style_new';
function  gtk_style_copy (style:PGtkStyle):PGtkStyle;cdecl;external gtkdll name 'gtk_style_copy';
function  gtk_style_attach (style:PGtkStyle; window:PGdkWindow):PGtkStyle;cdecl;external gtkdll name 'gtk_style_attach';
procedure gtk_style_detach(style:PGtkStyle);cdecl;external gtkdll name 'gtk_style_detach';
function  gtk_style_ref (style:PGtkStyle):PGtkStyle;cdecl;external gtkdll name 'gtk_style_ref';
procedure gtk_style_unref(style:PGtkStyle);cdecl;external gtkdll name 'gtk_style_unref';
procedure gtk_style_set_background(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType);cdecl;external gtkdll name 'gtk_style_set_background';
procedure gtk_style_apply_default_background(style:PGtkStyle; window:PGdkWindow; set_bg:gboolean; state_type:TGtkStateType; area:PGdkRectangle; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_style_apply_default_background';

procedure gtk_draw_hline(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; x1:gint; x2:gint; y:gint);cdecl;external gtkdll name 'gtk_draw_hline';
procedure gtk_draw_vline(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; y1:gint; y2:gint; x:gint);cdecl;external gtkdll name 'gtk_draw_vline';
procedure gtk_draw_shadow(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_draw_shadow';
procedure gtk_draw_polygon(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; points:PGdkPoint; npoints:gint; fill:gboolean);cdecl;external gtkdll name 'gtk_draw_polygon';
procedure gtk_draw_arrow(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; arrow_type:TGtkArrowType; fill:gboolean; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_draw_arrow';
procedure gtk_draw_diamond(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_draw_diamond';
procedure gtk_draw_oval(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_draw_oval';
procedure gtk_draw_string(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; x:gint; y:gint; thestring:Pgchar);cdecl;external gtkdll name 'gtk_draw_string';
procedure gtk_draw_box(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_draw_box';
{$ifndef gtkwin}
procedure gtk_draw_flat_box(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_draw_flat_box';
procedure gtk_draw_check(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_draw_check';
procedure gtk_draw_option(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_draw_option';
procedure gtk_draw_cross(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_draw_cross';
procedure gtk_draw_ramp(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; arrow_type:TGtkArrowType; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_draw_ramp';
procedure gtk_draw_tab(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_draw_tab';
procedure gtk_draw_shadow_gap(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint; gap_side:TGtkPositionType; gap_x:gint; gap_width:gint);cdecl;external gtkdll name 'gtk_draw_shadow_gap';
procedure gtk_draw_box_gap(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint; gap_side:TGtkPositionType; gap_x:gint; gap_width:gint);cdecl;external gtkdll name 'gtk_draw_box_gap';
procedure gtk_draw_extension(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint; gap_side:TGtkPositionType);cdecl;external gtkdll name 'gtk_draw_extension';
procedure gtk_draw_focus(style:PGtkStyle; window:PGdkWindow; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_draw_focus';
procedure gtk_draw_slider(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint; orientation:TGtkOrientation);cdecl;external gtkdll name 'gtk_draw_slider';
procedure gtk_draw_handle(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; x:gint; y:gint; width:gint; height:gint; orientation:TGtkOrientation);cdecl;external gtkdll name 'gtk_draw_handle';
{$endif}

procedure gtk_paint_hline(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x1:gint; x2:gint; y:gint);cdecl;external gtkdll name 'gtk_paint_hline';
procedure gtk_paint_vline(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; y1:gint; y2:gint; x:gint);cdecl;external gtkdll name 'gtk_paint_vline';
procedure gtk_paint_shadow(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_paint_shadow';
procedure gtk_paint_polygon(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; points:PGdkPoint; npoints:gint; fill:gboolean);cdecl;external gtkdll name 'gtk_paint_polygon';
procedure gtk_paint_arrow(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; arrow_type:TGtkArrowType; fill:gboolean; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_paint_arrow';
procedure gtk_paint_diamond(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_paint_diamond';
procedure gtk_paint_oval(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_paint_oval';
procedure gtk_paint_string(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; thestring:Pgchar);cdecl;external gtkdll name 'gtk_paint_string';
procedure gtk_paint_box(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_paint_box';
procedure gtk_paint_flat_box(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_paint_flat_box';
procedure gtk_paint_check(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_paint_check';
procedure gtk_paint_option(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_paint_option';
procedure gtk_paint_cross(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_paint_cross';
procedure gtk_paint_ramp(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; arrow_type:TGtkArrowType; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_paint_ramp';
procedure gtk_paint_tab(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_paint_tab';
procedure gtk_paint_shadow_gap(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint; gap_side:TGtkPositionType; gap_x:gint; gap_width:gint);cdecl;external gtkdll name 'gtk_paint_shadow_gap';
procedure gtk_paint_box_gap(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint; gap_side:TGtkPositionType; gap_x:gint; gap_width:gint);cdecl;external gtkdll name 'gtk_paint_box_gap';
procedure gtk_paint_extension(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint; gap_side:TGtkPositionType);cdecl;external gtkdll name 'gtk_paint_extension';
procedure gtk_paint_focus(style:PGtkStyle; window:PGdkWindow; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint);cdecl;external gtkdll name 'gtk_paint_focus';
procedure gtk_paint_slider(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint; orientation:TGtkOrientation);cdecl;external gtkdll name 'gtk_paint_slider';
procedure gtk_paint_handle(style:PGtkStyle; window:PGdkWindow; state_type:TGtkStateType; shadow_type:TGtkShadowType; area:PGdkRectangle; widget:PGtkWidget; detail:Pgchar; x:gint; y:gint; width:gint; height:gint; orientation:TGtkOrientation);cdecl;external gtkdll name 'gtk_paint_handle';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_STYLE_ATTACHED(style : PGtkstyle) : boolean;
      begin
         GTK_STYLE_ATTACHED:=(style^.attach_count) > 0;
      end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:06  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.11  1999/10/21 08:42:01  florian
    * some changes to get it work with gtk 1.3 under Windows 98:
      - removed some trailing space after the import name
      - In gtkbindings.h is
        #define  gtk_binding_entry_add          gtk_binding_entry_clear
        so in the pascal headers the import name of gtk_bindings_entry_add should be
        gtk_binding_entry_clear!
      - removed the declaration of
        gtk_drag_source_unset in gtkdnd.pp it isn't in gtk-1.3.dll!
      - in gdk.pp glibdll must be set to gdk-1.3:
        const
           gdkdll='gdk-1.3';
           glibdll='gdk-1.3';
        else the whole gdk_* calls are imported from glib-1.3.dll which is wrong!

  Revision 1.10  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.9  1999/05/11 00:39:29  peter
    * win32 fixes

  Revision 1.8  1999/05/10 15:20:27  peter
    * cdecl fixes

  Revision 1.7  1999/05/10 09:04:00  peter
    * gtk 1.2 port working

  Revision 1.6  1999/05/07 15:10:14  peter
    * more fixes

  Revision 1.5  1998/11/09 10:10:33  peter
    + C type casts are now correctly handled

  Revision 1.4  1998/10/21 22:25:18  peter
    * fixed some wrong cdecls

  Revision 1.3  1998/10/21 20:23:15  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

