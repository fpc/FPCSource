unit libgnomeui;

{off $DEFINE GNOME_EXCLUDE_EXPERIMENTAL}

{$PACKRECORDS C}
{$mode objfpc}

interface

Uses glib, gdk, gdk_imlib, gtk, libgnome, libart;

const
 libgnomeuidll='gnomeui';

Type
   va_list = pchar;

{$define read_interface}
{$undef read_implementation}

(* GNOME initialization *)

{$include gnomeinit.inc}


(* GTK Helper Widgets *)

{$include gtkpixmapmenuitem.inc}(* Menu item diplaying pixmap *)
{$include gtkclock.inc}(* clock widget for counting or realtime *)
{$include gtkdial.inc}(* Analog dial widget for number selection *)


(* GNOME MISC. Types/Routines/Consts *)
type
  TGnomeStringCallback = procedure (_string:Pgchar; data:gpointer);cdecl;
  TGnomeReplyCallback = procedure (reply:gint; data:gpointer);cdecl;
  TGnomePreferencesType = (GNOME_PREFERENCES_NEVER,GNOME_PREFERENCES_USER,
    GNOME_PREFERENCES_ALWAYS);

{$include gnomeuidefs.inc} (* GNOME KEY CONST DEFINES *)
{$include gnomegeometry.inc} (* WINDOW Geometry <-> String *)
{$include gnometypebuiltins.inc} (* GTK type macros *)
{$include gnomeicontext.inc}(* object & routines for painting word-wrapped text *)
{$include gnomewinhints.inc} (* Convenience functions for working with XA_WIN_* hints *)
{$include gnomedentryedit.inc} (* object(not widget) for handling the editing of Desktop entries *)

(* GNOME MISC. Required Widgets *)

{$include gnomepixmap.inc} (* widget for diplaying pixmaps *)
{$include gnomedialog.inc}  (* base dialog class *)
{$include gnomemessagebox.inc}(* message/error dialog class *)
{$include gnomeclient.inc} (*routines to add session-management to apps *)


(* GNOME APP. Core widgets *)

{$include gnomeabout.inc} (* Dialog for creating app. about box *)
{$include gnomedock.inc} (* Docking Widget/Items & Layout  *)
{$include gnomeapp.inc} (* Main App Window, simplifies toolbars, menus etc. *)
{$include gnomeappbar.inc} (* Main App bar, Status, progress, etc *)


(* GNOME STOCK WIDGETS/ROUTINES *)

{$include gnomestock.inc} (* Stock images/icons *)
{$include gnomeapphelper.inc} (* Simplify stock menu/toolbar creation *)
{$include gnomedialogutil.inc}(* Simplify message/error dialog creation *)

{$ifndef GNOME_EXCLUDE_EXPERIMENTAL}
  {$include gnomeapputil.inc} (* App. Message Dialogs and Progress modifications *)
{$endif}

{$include gnomepopupmenu.inc} (* routines for handling popup menus and attaching to widgets *)
{$include gnomepopuphelp.inc} (* add help, Cut, Copy & Paste menu to Widgets *)


(* GNOME ENTRY(aka Edit) Widgets *)

{$include gnomedateedit.inc}(* Entry For Date & Time *)
{$include gnomeentry.inc} (* Entry with History *)
{$include gnomefileentry.inc} (* Entry For File Names *)
{$include gnomeiconentry.inc}(* Entry for selecting icons *)
{$include gnomenumberentry.inc}(* Entry for number input *)
{$include gnomepixmapentry.inc} (* Entry For large images *)


(* GNOME "Select" Widgets *)

{$include gnomecolorpicker.inc} (* button tied to color dialog *)
{$include gnomefontpicker.inc} (* button tied to font dialog *)
{$include gnomepaperselector.inc} (* Widget used to select paper type *)
{$include gnomeiconsel.inc}(* Widget used to select an icon *)


(* GNOME Muliple Document Interface *)

{$include gnomemdichild.inc}
{$include gnomemdigenericchild.inc}
{$include gnomemdi.inc}
{$include gnomemdisession.inc}


(* GNOME Canvas & Types *)

{$include gnomecanvas.inc}
{$include gnomecanvasline.inc}
{$include gnomecanvasimage.inc}
{$include gnomecanvasload.inc}
{$include gnomecanvasrectellipse.inc}
{$include gnomecanvaspolygon.inc}
{$include gnomecanvastext.inc}
{$include gnomecanvaswidget.inc}
{$include gnomeiconitem.inc}

{$include gnomecanvasutil.inc}

(* GNOME Misc. Widgets *)

{$include gnomecalculator.inc} (* fully functional embedable dialog widget *)
{$include gnomeiconlist.inc} (* Icon list widget *)
{$include gnomehref.inc} (* icon for displaying clickable url *)
{$include gnomeprocbar.inc} (* Gnome Process Bar *)

{$ifndef GNOME_EXCLUDE_EXPERIMENTAL}
  {$include gnomeanimator.inc}
{$endif}

{$include gnomescores.inc} (* Game Dialog  for displaying High scores *)
{$include gnomepropertybox.inc} (* Dialog Box for handling property configuration *)

(* GNOME Druid Wizard System *)

{$include gnomedruidpage.inc}
{$include gnomedruidpagestart.inc}
{$include gnomedruidpagestandard.inc}
{$include gnomedruidpagefinish.inc}
{$include gnomedruid.inc}

implementation

{$undef read_interface}
{$define read_implementation}

(* GTK Helper Widgets *)

{$include gtkpixmapmenuitem.inc}(* Menu item diplaying pixmap *)
{$include gtkclock.inc}(* clock widget for counting or realtime *)
{$include gtkdial.inc}(* Analog dial widget for number selection *)


(* GNOME MISC. Types/Routines/Consts *)

{$include gnomeuidefs.inc} (* GNOME KEY CONST DEFINES *)
{$include gnomegeometry.inc} (* WINDOW Geometry <-> String *)
{$include gnomeicontext.inc}(* object & routines for painting word-wrapped text *)
{$include gnometypebuiltins.inc} (* GTK type macros *)
{$include gnomewinhints.inc} (* Convenience functions for working with XA_WIN_* hints *)
{$include gnomedentryedit.inc} (* object(not widget) for handling the editing of Desktop entries *)


(* GNOME MISC. Required Widgets *)

{$include gnomepixmap.inc} (* widget for diplaying pixmaps *)
{$include gnomedialog.inc}  (* base dialog class *)
{$include gnomemessagebox.inc}(* message/error dialog class *)
{$include gnomeclient.inc} (*routines to add session-management to apps *)


(* GNOME APP. Core widgets *)

{$include gnomeabout.inc} (* Dialog for creating app. about box *)
{$include gnomedock.inc} (* Docking Widget/Items & Layout  *)
{$include gnomeapp.inc} (* Main App Window, simplifies toolbars, menus etc. *)
{$include gnomeappbar.inc} (* Main App bar, Status, progress, etc *)


(* GNOME STOCK WIDGETS/ROUTINES *)

{$include gnomestock.inc} (* Stock images/icons *)
{$include gnomeapphelper.inc} (* Simplify stock menu/toolbar creation *)
{$include gnomedialogutil.inc}(* Simplify message/error dialog creation *)

{$ifndef GNOME_EXCLUDE_EXPERIMENTAL}
  {$include gnomeapputil.inc} (* App. Message Dialogs and Progress modifications *)
{$endif}

{$include gnomepopupmenu.inc} (* routines for handling popup menus and attaching to widgets *)
{$include gnomepopuphelp.inc} (* add help, Cut, Copy & Paste menu to Widgets *)


(* GNOME ENTRY(aka Edit) Widgets *)

{$include gnomedateedit.inc}(* Entry For Date & Time *)
{$include gnomeentry.inc} (* Entry with History *)
{$include gnomefileentry.inc} (* Entry For File Names *)
{$include gnomeiconentry.inc}(* Entry for selecting icons *)
{$include gnomenumberentry.inc}(* Entry for number input *)
{$include gnomepixmapentry.inc} (* Entry For large images *)


(* GNOME "Select" Widgets *)

{$include gnomecolorpicker.inc} (* button tied to color dialog *)
{$include gnomefontpicker.inc} (* button tied to font dialog *)
{$include gnomepaperselector.inc} (* Widget used to select paper type *)
{$include gnomeiconsel.inc}(* Widget used to select an icon *)


(* GNOME Muliple Document Interface *)

{$include gnomemdichild.inc}
{$include gnomemdigenericchild.inc}
{$include gnomemdi.inc}
{$include gnomemdisession.inc}


(* GNOME Canvas & Types *)

{$include gnomecanvas.inc}
{$include gnomecanvasline.inc}
{$include gnomecanvasimage.inc}
{$include gnomecanvasload.inc}
{$include gnomecanvasrectellipse.inc}
{$include gnomecanvaspolygon.inc}
{$include gnomecanvastext.inc}
{$include gnomecanvaswidget.inc}
{$include gnomeiconitem.inc}

{$include gnomecanvasutil.inc}

(* GNOME Misc. Widgets *)

{$include gnomecalculator.inc} (* fully functional embedable dialog widget *)
{$include gnomeiconlist.inc} (* Icon list widget *)
{$include gnomehref.inc} (* icon for displaying clickable url *)
{$include gnomeprocbar.inc} (* Gnome Process Bar *)

{$ifndef GNOME_EXCLUDE_EXPERIMENTAL}
  {$include gnomeanimator.inc}
{$endif}

{$include gnomescores.inc} (* Game Dialog  for displaying High scores *)
{$include gnomepropertybox.inc} (* Dialog Box for handling property configuration *)


(* GNOME Druid Wizard System *)

{$include gnomedruidpage.inc}
{$include gnomedruidpagestart.inc}
{$include gnomedruidpagestandard.inc}
{$include gnomedruidpagefinish.inc}
{$include gnomedruid.inc}

end.
