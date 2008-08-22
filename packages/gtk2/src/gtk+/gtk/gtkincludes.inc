{%MainUnit gtk2.pas}
{$include gtkobject.inc}
{$include gtkdebug.inc}
{$include gtktypeutils.inc}
{$include gtkwidget.inc}
{$include gtkmisc.inc}
{$include gtkaccelgroup.inc}
{$include gtkcontainer.inc}
{$include gtkbin.inc}
{$include gtkwindow.inc}
{$include gtklabel.inc}
{$include gtkaccellabel.inc}
{$include gtkaccelmap.inc}
{$include gtkaccessible.inc}
{$include gtkadjustment.inc}
{$include gtkalignment.inc}
{$include gtkframe.inc}
{$include gtkaspectframe.inc}
{$include gtkarrow.inc}
{$include gtkbindings.inc}
{$include gtkbox.inc}
{$include gtkbbox.inc}
{$include gtkbutton.inc}
{$include gtkcalendar.inc}
{$include gtkcelleditable.inc}
{$include gtkcellrenderer.inc}
{$include gtkcellrenderertext.inc}
{$include gtkcellrenderertoggle.inc}
{$include gtkcellrendererpixbuf.inc}
{$include gtkitem.inc}
{$include gtkmenuitem.inc}
{$include gtktogglebutton.inc}
{$include gtkcheckbutton.inc}
{$include gtkcheckmenuitem.inc}
{$include gtkclipboard.inc}
{$include gtkclist.inc}
{$include gtkdialog.inc}
{$include gtkvbox.inc}
{$include gtkcolorsel.inc}
{$include gtkcolorseldialog.inc}
{$include gtkhbox.inc}
{$include gtkcombo.inc}
{$include gtkctree.inc}
{$include gtkdrawingarea.inc}
{$include gtkcurve.inc}
{$include gtkdnd.inc}
{$include gtkeditable.inc}
{$include gtkimcontext.inc}
{$include gtkmenushell.inc}
{$include gtkmenu.inc}
{$include gtkentry.inc}
{$include gtkenums.inc}
{$include gtkeventbox.inc}
{$include fnmatch.inc}
{$include gtkfilesel.inc}
{$include gtkfixed.inc}
{$include gtkfontsel.inc}
{$include gtkgamma.inc}
{$include gtkgc.inc}
{$include gtkhandlebox.inc}
{$include gtkpaned.inc}
{$include gtkhbbox.inc}
{$include gtkhpaned.inc}
{$include gtkruler.inc}
{$include gtkhruler.inc}
{$include gtksettings.inc}
{$include gtkrc.inc}
{$include gtkstyle.inc}
{$include gtkrange.inc}
{$include gtkscale.inc}
{$include gtkhscale.inc}
{$include gtkscrollbar.inc}
{$include gtkhscrollbar.inc}
{$include gtkseparator.inc}
{$include gtkhseparator.inc}
{$include gtkiconfactory.inc}
{$include gtkimage.inc}
{$include gtkimagemenuitem.inc}
{$include gtkimcontextsimple.inc}
{$include gtkimmulticontext.inc}
{$include gtkinputdialog.inc}
{$include gtkinvisible.inc}
{$include gtkitemfactory.inc}
{$include gtklayout.inc}
{$include gtklist.inc}
{$include gtklistitem.inc}
{$include gtktreemodel.inc}
{$include gtktreesortable.inc}
{$include gtktreemodelsort.inc}
{$include gtkliststore.inc}
{$include gtkmain.inc}
{$include gtkmenubar.inc}
{$include gtkmessagedialog.inc}
{$include gtknotebook.inc}
{$include gtkoldeditable.inc}
{$include gtkoptionmenu.inc}
{$include gtkpixmap.inc}
{$include gtkplug.inc}
{$include gtkpreview.inc}
{$include gtkprogress.inc}
{$include gtkprogressbar.inc}
{$include gtkradiobutton.inc}
{$include gtkradiomenuitem.inc}
{$include gtkscrolledwindow.inc}
{$include gtkselection.inc}
{$include gtkseparatormenuitem.inc}
{$include gtksignal.inc}
{$include gtksizegroup.inc}
{$include gtksocket.inc}
{$include gtkspinbutton.inc}
{$include gtkstock.inc}
{$include gtkstatusbar.inc}
{$include gtktable.inc}
{$include gtktearoffmenuitem.inc}
{$include gtktext.inc}
{$include gtktextiter.inc}
{$include gtktexttag.inc}
{$include gtktexttagtable.inc}
{$include gtktextmark.inc}
{$include gtktextmarkprivate.inc}
{$include gtktextchild.inc}
{$include gtktextchildprivate.inc}
{$include gtktextsegment.inc}
{$include gtktextbtree.inc}
{$include gtktextbuffer.inc}
{$include gtktextlayout.inc}
{$include gtktextview.inc}
{$include gtktipsquery.inc}
{$include gtktooltips.inc}
{$include gtktoolbar.inc}
{$include gtktree.inc}
{$include gtktreednd.inc}
{$include gtktreeitem.inc}
{$include gtktreeselection.inc}
{$include gtktreestore.inc}
{$include gtktreeviewcolumn.inc}
{$include gtkrbtree.inc}
{$include gtktreeprivate.inc}
{$include gtktreeview.inc}
{$include gtkvbbox.inc}
{$include gtkviewport.inc}
{$include gtkvpaned.inc}
{$include gtkvruler.inc}
{$include gtkvscale.inc}
{$include gtkvscrollbar.inc}
{$include gtkvseparator.inc}


{$ifdef HasGTK2_4}
// new GTK 2.4 + 2.6 + 2.8 code

// GtkFileSystem Code for FileChooser
{$include gtkfilefilter.inc}
{$include gtkfilesystem.inc}

// new GtkFileChooser Widget
{$include gtkfilechooser.inc}
//{$include gtkfilechooserdefault.inc} - no exports
{$include gtkfilechooserdialog.inc}
//{$include gtkfilechooserembed.inc} - no exports
//{$include gtkfilechooserentry.inc} - no exports
{$include gtkfilechooserprivate.inc}
{$include gtkfilechooserutils.inc}
{$include gtkfilechooserwidget.inc}

// GtkExpander
{$include gtkexpander.inc}

// GtkActions
{$include gtkaction.inc}
{$include gtkactiongroup.inc}
{$include gtktoggleaction.inc}
{$include gtkradioaction.inc}

// GtkComboBox
{$include gtkcombobox.inc}
{$include gtkcomboboxentry.inc}

// Gtk~ToolButtons and -Item
{$include gtktoolitem.inc}
{$include gtktoolbutton.inc}
{$include gtktoggletoolbutton.inc}
{$include gtkradiotoolbutton.inc}

// even more
{$include gtkfontbutton.inc}
{$include gtkicontheme.inc}
{$include gtkcolorbutton.inc}
{$include gtkcelllayout.inc}
{$include gtkentrycompletion.inc}
{$include gtkuimanager.inc}
{$include gtkseparatortoolitem.inc}
{$include gtktreemodelfilter.inc}

{$IFDEF HasGTK2_6}
{$include gtkaboutdialog.inc}
{$include gtkcellrenderercombo.inc} // new GtkCellRendererCombo widget
{$include gtkcellrendererprogress.inc}
{$include gtkcellview.inc}
{$include gtkfilechooserbutton.inc}
{$include gtkiconview.inc}
{$include gtkmenutoolbutton.inc}

{$IFDEF HasGTK2_8}
// Put new GTK-2.8 functions here
{$ENDIF HasGTK2_8}

{$ENDIF HasGTK2_6}

{$endif HasGTK2_4}

// included by gtk2.pas
