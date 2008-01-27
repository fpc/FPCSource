{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('gnome1');
{$ifdef ALLPACKAGES}
    P.Directory:='gnome1';
{$endif ALLPACKAGES}
    P.Version:='2.0.0';
    P.SourcePath.Add('src');
//    P.Dependencies.Add('x11');

    T:=P.Targets.AddUnit('gconfclient.pp');
      with T.Dependencies do
        begin
          AddInclude('gconflisteners.inc');
          AddInclude('gconfchangeset.inc');
          AddInclude('gconflisteners.inc');
          AddInclude('gconfchangeset.inc');
          AddUnit('glib');
          AddUnit('gtk');
          AddUnit('gconf');
        end;
    T:=P.Targets.AddUnit('gconf.pp');
      with T.Dependencies do
        begin
          AddInclude('gconfglibpublic.inc');
          AddInclude('gconferror.inc');
          AddInclude('gconfvalue.inc');
          AddInclude('gconfschema.inc');
          AddInclude('gconfengine.inc');
          AddInclude('gconfglibpublic.inc');
          AddInclude('gconferror.inc');
          AddInclude('gconfvalue.inc');
          AddInclude('gconfschema.inc');
          AddInclude('gconfengine.inc');
          AddUnit('glib');
        end;
    T:=P.Targets.AddUnit('libart.pp');
    T:=P.Targets.AddUnit('libgnome.pp');
      with T.Dependencies do
        begin
          AddInclude('gnomeutil.inc');
          AddInclude('gnomeconfig.inc');
          AddInclude('gnomedentry.inc');
          AddInclude('gnomeexec.inc');
          AddInclude('gnomehelp.inc');
          AddInclude('gnomei18n.inc');
          AddInclude('gnomemetadata.inc');
          AddInclude('gnomemime.inc');
          AddInclude('gnomemimeinfo.inc');
          AddInclude('gnomepaper.inc');
          AddInclude('gnomeremote.inc');
          AddInclude('gnomescore.inc');
          AddInclude('gnomesound.inc');
          AddInclude('gnometriggers.inc');
          AddInclude('gnomeurl.inc');
          AddInclude('gnomeutil.inc');
          AddInclude('gnomeconfig.inc');
          AddInclude('gnomedentry.inc');
          AddInclude('gnomeexec.inc');
          AddInclude('gnomehelp.inc');
          AddInclude('gnomei18n.inc');
          AddInclude('gnomemetadata.inc');
          AddInclude('gnomemime.inc');
          AddInclude('gnomemimeinfo.inc');
          AddInclude('gnomepaper.inc');
          AddInclude('gnomeremote.inc');
          AddInclude('gnomescore.inc');
          AddInclude('gnomesound.inc');
          AddInclude('gnometriggers.inc');
          AddInclude('gnomeurl.inc');
          AddUnit('glib');
        end;
    T:=P.Targets.AddUnit('libgnomeui.pp');
      with T.Dependencies do
        begin
          AddInclude('gnomeinit.inc');
          AddInclude('gtkpixmapmenuitem.inc');
          AddInclude('gtkclock.inc');
          AddInclude('gtkdial.inc');
          AddInclude('gnomeuidefs.inc');
          AddInclude('gnomegeometry.inc');
          AddInclude('gnometypebuiltins.inc');
          AddInclude('gnomeicontext.inc');
          AddInclude('gnomewinhints.inc');
          AddInclude('gnomedentryedit.inc');
          AddInclude('gnomepixmap.inc');
          AddInclude('gnomedialog.inc');
          AddInclude('gnomemessagebox.inc');
          AddInclude('gnomeclient.inc');
          AddInclude('gnomeabout.inc');
          AddInclude('gnomedock.inc');
          AddInclude('gnomeapp.inc');
          AddInclude('gnomeappbar.inc');
          AddInclude('gnomestock.inc');
          AddInclude('gnomeapphelper.inc');
          AddInclude('gnomedialogutil.inc');
          AddInclude('gnomeapputil.inc');
          AddInclude('gnomepopupmenu.inc');
          AddInclude('gnomepopuphelp.inc');
          AddInclude('gnomedateedit.inc');
          AddInclude('gnomeentry.inc');
          AddInclude('gnomefileentry.inc');
          AddInclude('gnomeiconentry.inc');
          AddInclude('gnomenumberentry.inc');
          AddInclude('gnomepixmapentry.inc');
          AddInclude('gnomecolorpicker.inc');
          AddInclude('gnomefontpicker.inc');
          AddInclude('gnomepaperselector.inc');
          AddInclude('gnomeiconsel.inc');
          AddInclude('gnomemdichild.inc');
          AddInclude('gnomemdigenericchild.inc');
          AddInclude('gnomemdi.inc');
          AddInclude('gnomemdisession.inc');
          AddInclude('gnomecanvas.inc');
          AddInclude('gnomecanvasline.inc');
          AddInclude('gnomecanvasimage.inc');
          AddInclude('gnomecanvasload.inc');
          AddInclude('gnomecanvasrectellipse.inc');
          AddInclude('gnomecanvaspolygon.inc');
          AddInclude('gnomecanvastext.inc');
          AddInclude('gnomecanvaswidget.inc');
          AddInclude('gnomeiconitem.inc');
          AddInclude('gnomecanvasutil.inc');
          AddInclude('gnomecalculator.inc');
          AddInclude('gnomeiconlist.inc');
          AddInclude('gnomehref.inc');
          AddInclude('gnomeprocbar.inc');
          AddInclude('gnomeanimator.inc');
          AddInclude('gnomescores.inc');
          AddInclude('gnomepropertybox.inc');
          AddInclude('gnomedruidpage.inc');
          AddInclude('gnomedruidpagestart.inc');
          AddInclude('gnomedruidpagestandard.inc');
          AddInclude('gnomedruidpagefinish.inc');
          AddInclude('gnomedruid.inc');
          AddInclude('gtkpixmapmenuitem.inc');
          AddInclude('gtkclock.inc');
          AddInclude('gtkdial.inc');
          AddInclude('gnomeuidefs.inc');
          AddInclude('gnomegeometry.inc');
          AddInclude('gnomeicontext.inc');
          AddInclude('gnometypebuiltins.inc');
          AddInclude('gnomewinhints.inc');
          AddInclude('gnomedentryedit.inc');
          AddInclude('gnomepixmap.inc');
          AddInclude('gnomedialog.inc');
          AddInclude('gnomemessagebox.inc');
          AddInclude('gnomeclient.inc');
          AddInclude('gnomeabout.inc');
          AddInclude('gnomedock.inc');
          AddInclude('gnomeapp.inc');
          AddInclude('gnomeappbar.inc');
          AddInclude('gnomestock.inc');
          AddInclude('gnomeapphelper.inc');
          AddInclude('gnomedialogutil.inc');
          AddInclude('gnomeapputil.inc');
          AddInclude('gnomepopupmenu.inc');
          AddInclude('gnomepopuphelp.inc');
          AddInclude('gnomedateedit.inc');
          AddInclude('gnomeentry.inc');
          AddInclude('gnomefileentry.inc');
          AddInclude('gnomeiconentry.inc');
          AddInclude('gnomenumberentry.inc');
          AddInclude('gnomepixmapentry.inc');
          AddInclude('gnomecolorpicker.inc');
          AddInclude('gnomefontpicker.inc');
          AddInclude('gnomepaperselector.inc');
          AddInclude('gnomeiconsel.inc');
          AddInclude('gnomemdichild.inc');
          AddInclude('gnomemdigenericchild.inc');
          AddInclude('gnomemdi.inc');
          AddInclude('gnomemdisession.inc');
          AddInclude('gnomecanvas.inc');
          AddInclude('gnomecanvasline.inc');
          AddInclude('gnomecanvasimage.inc');
          AddInclude('gnomecanvasload.inc');
          AddInclude('gnomecanvasrectellipse.inc');
          AddInclude('gnomecanvaspolygon.inc');
          AddInclude('gnomecanvastext.inc');
          AddInclude('gnomecanvaswidget.inc');
          AddInclude('gnomeiconitem.inc');
          AddInclude('gnomecanvasutil.inc');
          AddInclude('gnomecalculator.inc');
          AddInclude('gnomeiconlist.inc');
          AddInclude('gnomehref.inc');
          AddInclude('gnomeprocbar.inc');
          AddInclude('gnomeanimator.inc');
          AddInclude('gnomescores.inc');
          AddInclude('gnomepropertybox.inc');
          AddInclude('gnomedruidpage.inc');
          AddInclude('gnomedruidpagestart.inc');
          AddInclude('gnomedruidpagestandard.inc');
          AddInclude('gnomedruidpagefinish.inc');
          AddInclude('gnomedruid.inc');
          AddUnit('glib');
          AddUnit('gdk');
          AddUnit('gdk_imlib');
          AddUnit('gtk');
          AddUnit('libgnome');
          AddUnit('libart');
        end;
    T:=P.Targets.AddUnit('libzvt.pp');
      with T.Dependencies do
        begin
          AddInclude('lists.inc');
          AddInclude('vt.inc');
          AddInclude('vtx.inc');
          AddInclude('lists.inc');
          AddInclude('vt.inc');
          AddInclude('vtx.inc');
          AddUnit('glib');
          AddUnit('gdk');
          AddUnit('gtk');
          AddUnit('gdk_imlib');
        end;


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
