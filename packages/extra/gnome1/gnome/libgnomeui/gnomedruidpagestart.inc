{$IfDef read_interface}

type
   PGnomeDruidPageStart = ^TGnomeDruidPageStart;
   TGnomeDruidPageStart = record
        parent : TGnomeDruidPage;
        background_color : TGdkColor;
        textbox_color : TGdkColor;
        logo_background_color : TGdkColor;
        title_color : TGdkColor;
        text_color : TGdkColor;
        title : Pgchar;
        thetext : Pgchar;
        logo_image : PGdkImlibImage;
        watermark_image : PGdkImlibImage;
        canvas : PGtkWidget;
        background_item : PGnomeCanvasItem;
        textbox_item : PGnomeCanvasItem;
        text_item : PGnomeCanvasItem;
        logo_item : PGnomeCanvasItem;
        logoframe_item : PGnomeCanvasItem;
        watermark_item : PGnomeCanvasItem;
        title_item : PGnomeCanvasItem;
     end;
   GNOME_DRUID_PAGE_START = PGnomeDruidPageStart;

   PGnomeDruidPageStartClass = ^TGnomeDruidPageStartClass;
   TGnomeDruidPageStartClass = record
        parent_class : TGnomeDruidPageClass;
     end;
   GNOME_DRUID_PAGE_START_CLASS = PGnomeDruidPageStartClass;


function GNOME_TYPE_DRUID_PAGE_START : TGTKType;
function GNOME_IS_DRUID_PAGE_START(obj : Pointer) : Boolean;
function GNOME_IS_DRUID_PAGE_START_CLASS(klass : Pointer) : Boolean;

function gnome_druid_page_start_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_druid_page_start_get_type';
function gnome_druid_page_start_new:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_druid_page_start_new';
function gnome_druid_page_start_new_with_vals(title:Pgchar; thetext:Pgchar; logo:PGdkImlibImage; watermark:PGdkImlibImage):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_druid_page_start_new_with_vals';
procedure gnome_druid_page_start_set_bg_color(druid_page_start:PGnomeDruidPageStart; color:PGdkColor);cdecl;external libgnomeuidll name 'gnome_druid_page_start_set_bg_color';
procedure gnome_druid_page_start_set_textbox_color(druid_page_start:PGnomeDruidPageStart; color:PGdkColor);cdecl;external libgnomeuidll name 'gnome_druid_page_start_set_textbox_color';
procedure gnome_druid_page_start_set_logo_bg_color(druid_page_start:PGnomeDruidPageStart; color:PGdkColor);cdecl;external libgnomeuidll name 'gnome_druid_page_start_set_logo_bg_color';
procedure gnome_druid_page_start_set_title_color(druid_page_start:PGnomeDruidPageStart; color:PGdkColor);cdecl;external libgnomeuidll name 'gnome_druid_page_start_set_title_color';
procedure gnome_druid_page_start_set_text_color(druid_page_start:PGnomeDruidPageStart; color:PGdkColor);cdecl;external libgnomeuidll name 'gnome_druid_page_start_set_text_color';
procedure gnome_druid_page_start_set_text(druid_page_start:PGnomeDruidPageStart; thetext:Pgchar);cdecl;external libgnomeuidll name 'gnome_druid_page_start_set_text';
procedure gnome_druid_page_start_set_title(druid_page_start:PGnomeDruidPageStart; title:Pgchar);cdecl;external libgnomeuidll name 'gnome_druid_page_start_set_title';
procedure gnome_druid_page_start_set_logo(druid_page_start:PGnomeDruidPageStart; logo_image:PGdkImlibImage);cdecl;external libgnomeuidll name 'gnome_druid_page_start_set_logo';
procedure gnome_druid_page_start_set_watermark(druid_page_start:PGnomeDruidPageStart; watermark:PGdkImlibImage);cdecl;external libgnomeuidll name 'gnome_druid_page_start_set_watermark';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_DRUID_PAGE_START : TGTKType;
begin
  GNOME_TYPE_DRUID_PAGE_START:=gnome_druid_page_start_get_type;
end;

function GNOME_IS_DRUID_PAGE_START(obj : Pointer) : Boolean;
begin
   GNOME_IS_DRUID_PAGE_START:=(obj<>nil) and GNOME_IS_DRUID_PAGE_START_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_DRUID_PAGE_START_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_DRUID_PAGE_START_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_DRUID_PAGE_START);
end;

{$Endif read_implementation}
