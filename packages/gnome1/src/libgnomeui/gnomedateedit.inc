{$IfDef read_interface}
type
   TGnomeDateEditFlags = (GNOME_DATE_EDIT_SHOW_TIME := 1 shl 0,GNOME_DATE_EDIT_24_HR := 1 shl 1,
     GNOME_DATE_EDIT_WEEK_STARTS_ON_MONDAY := 1 shl 2
     );

type
   PGnomeDateEdit = ^TGnomeDateEdit;
   TGnomeDateEdit = record
        hbox : TGtkHBox;
        date_entry : PGtkWidget;
        date_button : PGtkWidget;
        time_entry : PGtkWidget;
        time_popup : PGtkWidget;
        cal_label : PGtkWidget;
        cal_popup : PGtkWidget;
        calendar : PGtkWidget;
        lower_hour : longint;
        upper_hour : longint;
        initial_time : time_t;
        flags : longint;
     end;
   GNOME_DATE_EDIT = PGnomeDateEdit;

   PGnomeDateEditClass = ^TGnomeDateEditClass;
   TGnomeDateEditClass = record
        parent_class : TGtkHBoxClass;
        date_changed : procedure (gde:PGnomeDateEdit);cdecl;
        time_changed : procedure (gde:PGnomeDateEdit);cdecl;
     end;
   GNOME_DATE_EDIT_CLASS = PGnomeDateEditClass;

function GNOME_TYPE_DATE_EDIT : TGTKType;
function GNOME_IS_DATE_EDIT(obj : Pointer) : Boolean;
function GNOME_IS_DATE_EDIT_CLASS(klass : Pointer) : Boolean;

function gnome_date_edit_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_date_edit_get_type';
function gnome_date_edit_new(the_time:time_t; show_time:longint; use_24_format:longint):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_date_edit_new';
function gnome_date_edit_new_flags(the_time:time_t; flags:TGnomeDateEditFlags):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_date_edit_new_flags';
procedure gnome_date_edit_set_time(gde:PGnomeDateEdit; the_time:time_t);cdecl;external libgnomeuidll name 'gnome_date_edit_set_time';
procedure gnome_date_edit_set_popup_range(gde:PGnomeDateEdit; low_hour:longint; up_hour:longint);cdecl;external libgnomeuidll name 'gnome_date_edit_set_popup_range';
function gnome_date_edit_get_date(gde:PGnomeDateEdit):time_t;cdecl;external libgnomeuidll name 'gnome_date_edit_get_date';
procedure gnome_date_edit_set_flags(gde:PGnomeDateEdit; flags:TGnomeDateEditFlags);cdecl;external libgnomeuidll name 'gnome_date_edit_set_flags';
function gnome_date_edit_get_flags(gde:PGnomeDateEdit):longint;cdecl;external libgnomeuidll name 'gnome_date_edit_get_flags';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_DATE_EDIT : TGTKType;
begin
  GNOME_TYPE_DATE_EDIT:=gnome_date_edit_get_type;
end;

function GNOME_IS_DATE_EDIT(obj : Pointer) : Boolean;
begin
   GNOME_IS_DATE_EDIT:=(obj<>nil) and GNOME_IS_DATE_EDIT_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_DATE_EDIT_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_DATE_EDIT_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_DATE_EDIT);
end;

{$Endif read_implementation}
