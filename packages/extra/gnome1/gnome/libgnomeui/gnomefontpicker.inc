{$IfDef read_interface}

type
   TGnomeFontPickerMode = (GNOME_FONT_PICKER_MODE_PIXMAP,GNOME_FONT_PICKER_MODE_FONT_INFO,
     GNOME_FONT_PICKER_MODE_USER_WIDGET,GNOME_FONT_PICKER_MODE_UNKNOWN
     );

   PGnomeFontPicker = ^TGnomeFontPicker;
   TGnomeFontPicker = record
        button : TGtkButton;
        font_dialog : PGtkWidget;
        inside : PGtkWidget;
        font_label : PGtkWidget;
        vsep : PGtkWidget;
        size_label : PGtkWidget;
        mode : TGnomeFontPickerMode;
        font_name : Pgchar;
        preview_text : Pgchar;
        use_font_in_label : gboolean;
        use_font_in_label_size : gboolean;
        show_size : gboolean;
        title : Pgchar;
     end;
   GNOME_FONT_PICKER = PGnomeFontPicker;

   PGnomeFontPickerClass = ^TGnomeFontPickerClass;
   TGnomeFontPickerClass = record
        parent_class : TGtkButtonClass;
        font_set : procedure (gfp:PGnomeFontPicker; font_name:Pgchar);cdecl;
     end;
   GNOME_FONT_PICKER_CLASS = PGnomeFontPickerClass;

function GNOME_TYPE_FONT_PICKER : TGTKType;
function GNOME_IS_FONT_PICKER(obj : Pointer) : Boolean;
function GNOME_IS_FONT_PICKER_CLASS(klass : Pointer) : Boolean;

function gnome_font_picker_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_font_picker_get_type';
function gnome_font_picker_new:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_font_picker_new';
procedure gnome_font_picker_set_title(gfp:PGnomeFontPicker; title:Pgchar);cdecl;external libgnomeuidll name 'gnome_font_picker_set_title';
function gnome_font_picker_get_mode(gfp:PGnomeFontPicker):TGnomeFontPickerMode;cdecl;external libgnomeuidll name 'gnome_font_picker_get_mode';
procedure gnome_font_picker_set_mode(gfp:PGnomeFontPicker; mode:TGnomeFontPickerMode);cdecl;external libgnomeuidll name 'gnome_font_picker_set_mode';
procedure gnome_font_picker_fi_set_use_font_in_label(gfp:PGnomeFontPicker; use_font_in_label:gboolean; size:gint);cdecl;external libgnomeuidll name 'gnome_font_picker_fi_set_use_font_in_label';
procedure gnome_font_picker_fi_set_show_size(gfp:PGnomeFontPicker; show_size:gboolean);cdecl;external libgnomeuidll name 'gnome_font_picker_fi_set_show_size';
procedure gnome_font_picker_uw_set_widget(gfp:PGnomeFontPicker; widget:PGtkWidget);cdecl;external libgnomeuidll name 'gnome_font_picker_uw_set_widget';
function gnome_font_picker_get_font_name(gfp:PGnomeFontPicker):Pgchar;cdecl;external libgnomeuidll name 'gnome_font_picker_get_font_name';
function gnome_font_picker_get_font(gfp:PGnomeFontPicker):PGdkFont;cdecl;external libgnomeuidll name 'gnome_font_picker_get_font';
function gnome_font_picker_set_font_name(gfp:PGnomeFontPicker; fontname:Pgchar):gboolean;cdecl;external libgnomeuidll name 'gnome_font_picker_set_font_name';
function gnome_font_picker_get_preview_text(gfp:PGnomeFontPicker):Pgchar;cdecl;external libgnomeuidll name 'gnome_font_picker_get_preview_text';
procedure gnome_font_picker_set_preview_text(gfp:PGnomeFontPicker; thetext:Pgchar);cdecl;external libgnomeuidll name 'gnome_font_picker_set_preview_text';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_FONT_PICKER : TGTKType;
begin
  GNOME_TYPE_FONT_PICKER:=gnome_font_picker_get_type;
end;

function GNOME_IS_FONT_PICKER(obj : Pointer) : Boolean;
begin
   GNOME_IS_FONT_PICKER:=(obj<>nil) and GNOME_IS_FONT_PICKER_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_FONT_PICKER_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_FONT_PICKER_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_FONT_PICKER);
end;

{$Endif read_implementation}
