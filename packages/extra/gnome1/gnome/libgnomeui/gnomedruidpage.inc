{$IfDef read_interface}
type
   PGnomeDruidPage = ^TGnomeDruidPage;
   TGnomeDruidPage = record
        parent : TGtkBin;
     end;
   GNOME_DRUID_PAGE = PGnomeDruidPage;

   PGnomeDruidPageClass = ^TGnomeDruidPageClass;
   TGnomeDruidPageClass = record
        parent_class : TGtkBinClass;
        next : function (druid_page:PGnomeDruidPage; druid:PGtkWidget):gboolean;cdecl;
        prepare : procedure (druid_page:PGnomeDruidPage; druid:PGtkWidget);cdecl;
        back : function (druid_page:PGnomeDruidPage; druid:PGtkWidget):gboolean;cdecl;
        finish : procedure (druid_page:PGnomeDruidPage; druid:PGtkWidget);cdecl;
        cancel : function (druid_page:PGnomeDruidPage; druid:PGtkWidget):gboolean;cdecl;
     end;
   GNOME_DRUID_PAGE_CLASS = PGnomeDruidPageClass;

function GNOME_TYPE_DRUID_PAGE : TGTKType;
function GNOME_IS_DRUID_PAGE(obj : Pointer) : Boolean;
function GNOME_IS_DRUID_PAGE_CLASS(klass : Pointer) : Boolean;

function gnome_druid_page_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_druid_page_get_type';
function gnome_druid_page_next(druid_page:PGnomeDruidPage):gboolean;cdecl;external libgnomeuidll name 'gnome_druid_page_next';
procedure gnome_druid_page_prepare(druid_page:PGnomeDruidPage);cdecl;external libgnomeuidll name 'gnome_druid_page_prepare';
function gnome_druid_page_back(druid_page:PGnomeDruidPage):gboolean;cdecl;external libgnomeuidll name 'gnome_druid_page_back';
function gnome_druid_page_cancel(druid_page:PGnomeDruidPage):gboolean;cdecl;external libgnomeuidll name 'gnome_druid_page_cancel';
procedure gnome_druid_page_finish(druid_page:PGnomeDruidPage);cdecl;external libgnomeuidll name 'gnome_druid_page_finish';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_DRUID_PAGE : TGTKType;
begin
  GNOME_TYPE_DRUID_PAGE:=gnome_druid_page_get_type;
end;

function GNOME_IS_DRUID_PAGE(obj : Pointer) : Boolean;
begin
   GNOME_IS_DRUID_PAGE:=(obj<>nil) and GNOME_IS_DRUID_PAGE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_DRUID_PAGE_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_DRUID_PAGE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_DRUID_PAGE);
end;

{$Endif read_implementation}
