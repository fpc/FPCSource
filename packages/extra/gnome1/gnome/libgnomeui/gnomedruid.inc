{$IfDef read_interface}

type
   PGnomeDruid = ^TGnomeDruid;
   TGnomeDruid = record
        parent : TGtkContainer;
        back : PGtkWidget;
        next : PGtkWidget;
        cancel : PGtkWidget;
        finish : PGtkWidget;
        current : PGnomeDruidPage;
        children : PGList;
        show_finish : gboolean;
     end;
   GNOME_DRUID = PGnomeDruid;

   PGnomeDruidClass = ^TGnomeDruidClass;
   TGnomeDruidClass = record
        parent_class : TGtkContainerClass;
        cancel : procedure (druid:PGnomeDruid);cdecl;
     end;
   GNOME_DRUID_CLASS = PGnomeDruidClass;

function GNOME_TYPE_DRUID : TGTKType;
function GNOME_IS_DRUID(obj : Pointer) : Boolean;
function GNOME_IS_DRUID_CLASS(klass : Pointer) : Boolean;

function gnome_druid_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_druid_get_type';
function gnome_druid_new:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_druid_new';
procedure gnome_druid_set_buttons_sensitive(druid:PGnomeDruid; back_sensitive:gboolean; next_sensitive:gboolean; cancel_sensitive:gboolean);cdecl;external libgnomeuidll name 'gnome_druid_set_buttons_sensitive';
procedure gnome_druid_set_show_finish(druid:PGnomeDruid; show_finish:gboolean);cdecl;external libgnomeuidll name 'gnome_druid_set_show_finish';
procedure gnome_druid_prepend_page(druid:PGnomeDruid; page:PGnomeDruidPage);cdecl;external libgnomeuidll name 'gnome_druid_prepend_page';
procedure gnome_druid_insert_page(druid:PGnomeDruid; back_page:PGnomeDruidPage; page:PGnomeDruidPage);cdecl;external libgnomeuidll name 'gnome_druid_insert_page';
procedure gnome_druid_append_page(druid:PGnomeDruid; page:PGnomeDruidPage);cdecl;external libgnomeuidll name 'gnome_druid_append_page';
procedure gnome_druid_set_page(druid:PGnomeDruid; page:PGnomeDruidPage);cdecl;external libgnomeuidll name 'gnome_druid_set_page';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_DRUID : TGTKType;
begin
  GNOME_TYPE_DRUID:=gnome_druid_get_type;
end;

function GNOME_IS_DRUID(obj : Pointer) : Boolean;
begin
   GNOME_IS_DRUID:=(obj<>nil) and GNOME_IS_DRUID_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_DRUID_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_DRUID_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_DRUID);
end;

{$Endif read_implementation}
