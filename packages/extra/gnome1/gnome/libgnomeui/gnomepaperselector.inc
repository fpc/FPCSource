{$IfDef read_interface}

type
   PGnomePaperSelector = ^TGnomePaperSelector;
   TGnomePaperSelector = record
        vbox : TGtkVBox;
        paper : PGtkWidget;
        width : PGtkWidget;
        height : PGtkWidget;
        theunit : PGtkWidget;
        unit_label : PGtkWidget;
        paper_id : gint;
        width_id : gint;
        height_id : gint;
     end;
   GNOME_PAPER_SELECTOR = PGnomePaperSelector;

   PGnomePaperSelectorClass = ^TGnomePaperSelectorClass;
   TGnomePaperSelectorClass = record
        parent_class : TGtkVBoxClass;
     end;
   GNOME_PAPER_SELECTOR_CLASS = PGnomePaperSelectorClass;

function GNOME_TYPE_PAPER_SELECTOR : TGTKType;
function GNOME_IS_PAPER_SELECTOR(obj : Pointer) : Boolean;
function GNOME_IS_PAPER_SELECTOR_CLASS(klass : Pointer) : Boolean;

function gnome_paper_selector_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_paper_selector_get_type';
function gnome_paper_selector_new:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_paper_selector_new';
function gnome_paper_selector_get_name(gspaper:PGnomePaperSelector):Pgchar;cdecl;external libgnomeuidll name 'gnome_paper_selector_get_name';
function gnome_paper_selector_get_width(gspaper:PGnomePaperSelector):gfloat;cdecl;external libgnomeuidll name 'gnome_paper_selector_get_width';
function gnome_paper_selector_get_height(gspaper:PGnomePaperSelector):gfloat;cdecl;external libgnomeuidll name 'gnome_paper_selector_get_height';
function gnome_paper_selector_get_left_margin(gspaper:PGnomePaperSelector):gfloat;cdecl;external libgnomeuidll name 'gnome_paper_selector_get_left_margin';
function gnome_paper_selector_get_right_margin(gspaper:PGnomePaperSelector):gfloat;cdecl;external libgnomeuidll name 'gnome_paper_selector_get_right_margin';
function gnome_paper_selector_get_top_margin(gspaper:PGnomePaperSelector):gfloat;cdecl;external libgnomeuidll name 'gnome_paper_selector_get_top_margin';
function gnome_paper_selector_get_bottom_margin(gspaper:PGnomePaperSelector):gfloat;cdecl;external libgnomeuidll name 'gnome_paper_selector_get_bottom_margin';
procedure gnome_paper_selector_set_name(gspaper:PGnomePaperSelector; name:Pgchar);cdecl;external libgnomeuidll name 'gnome_paper_selector_set_name';
procedure gnome_paper_selector_set_width(gspaper:PGnomePaperSelector; width:gfloat);cdecl;external libgnomeuidll name 'gnome_paper_selector_set_width';
procedure gnome_paper_selector_set_height(gspaper:PGnomePaperSelector; height:gfloat);cdecl;external libgnomeuidll name 'gnome_paper_selector_set_height';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_PAPER_SELECTOR : TGTKType;
begin
  GNOME_TYPE_PAPER_SELECTOR:=gnome_paper_selector_get_type;
end;

function GNOME_IS_PAPER_SELECTOR(obj : Pointer) : Boolean;
begin
   GNOME_IS_PAPER_SELECTOR:=(obj<>nil) and GNOME_IS_PAPER_SELECTOR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_PAPER_SELECTOR_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_PAPER_SELECTOR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_PAPER_SELECTOR);
end;

{$Endif read_implementation}
