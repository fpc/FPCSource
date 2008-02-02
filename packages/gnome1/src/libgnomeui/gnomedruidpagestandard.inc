{$IfDef read_interface}

type
   PGnomeDruidPageStandard = ^TGnomeDruidPageStandard;
   TGnomeDruidPageStandard = record
        parent : TGnomeDruidPage;
        vbox : PGtkWidget;
        background_color : TGdkColor;
        logo_background_color : TGdkColor;
        title_color : TGdkColor;
        logo_image : PGdkImlibImage;
        title : Pgchar;
        canvas : PGtkWidget;
        side_bar : PGtkWidget;
        logoframe_item : PGnomeCanvasItem;
        logo_item : PGnomeCanvasItem;
        title_item : PGnomeCanvasItem;
        background_item : PGnomeCanvasItem;
        bottom_bar : PGtkWidget;
        right_bar : PGtkWidget;
     end;
   GNOME_DRUID_PAGE_STANDARD = PGnomeDruidPageStandard;

   PGnomeDruidPageStandardClass = ^TGnomeDruidPageStandardClass;
   TGnomeDruidPageStandardClass = record
        parent_class : TGnomeDruidPageClass;
     end;
   GNOME_DRUID_PAGE_STANDARD_CLASS = PGnomeDruidPageStandardClass;

function GNOME_TYPE_DRUID_PAGE_STANDARD : TGTKType;
function GNOME_IS_DRUID_PAGE_STANDARD(obj : Pointer) : Boolean;
function GNOME_IS_DRUID_PAGE_STANDARD_CLASS(klass : Pointer) : Boolean;

function gnome_druid_page_standard_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_druid_page_standard_get_type';
function gnome_druid_page_standard_new:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_druid_page_standard_new';
function gnome_druid_page_standard_new_with_vals(title:Pgchar; logo:PGdkImlibImage):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_druid_page_standard_new_with_vals';
procedure gnome_druid_page_standard_set_bg_color(druid_page_standard:PGnomeDruidPageStandard; color:PGdkColor);cdecl;external libgnomeuidll name 'gnome_druid_page_standard_set_bg_color';
procedure gnome_druid_page_standard_set_logo_bg_color(druid_page_standard:PGnomeDruidPageStandard; color:PGdkColor);cdecl;external libgnomeuidll name 'gnome_druid_page_standard_set_logo_bg_color';
procedure gnome_druid_page_standard_set_title_color(druid_page_standard:PGnomeDruidPageStandard; color:PGdkColor);cdecl;external libgnomeuidll name 'gnome_druid_page_standard_set_title_color';
procedure gnome_druid_page_standard_set_title(druid_page_standard:PGnomeDruidPageStandard; title:Pgchar);cdecl;external libgnomeuidll name 'gnome_druid_page_standard_set_title';
procedure gnome_druid_page_standard_set_logo(druid_page_standard:PGnomeDruidPageStandard; logo_image:PGdkImlibImage);cdecl;external libgnomeuidll name 'gnome_druid_page_standard_set_logo';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_DRUID_PAGE_STANDARD : TGTKType;
begin
  GNOME_TYPE_DRUID_PAGE_STANDARD:=gnome_druid_page_standard_get_type;
end;

function GNOME_IS_DRUID_PAGE_STANDARD(obj : Pointer) : Boolean;
begin
   GNOME_IS_DRUID_PAGE_STANDARD:=(obj<>nil) and GNOME_IS_DRUID_PAGE_STANDARD_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_DRUID_PAGE_STANDARD_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_DRUID_PAGE_STANDARD_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_DRUID_PAGE_STANDARD);
end;

{$Endif read_implementation}
