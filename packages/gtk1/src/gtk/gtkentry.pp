{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkEntry = ^TGtkEntry;
     TGtkEntry = record
          editable : TGtkEditable;
          text_area : PGdkWindow;
          backing_pixmap : PGdkPixmap;
          cursor : PGdkCursor;
          text : PGdkWChar;
          text_size : guint16;
          text_length : guint16;
          text_max_length : guint16;
          scroll_offset : gint;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          timer : guint32;
          button : guint;
          char_offset : Pgint;
          text_mb : Pgchar;
          flag1 : {$ifdef win32}longint{$else}word{$endif};
       end;

  const
     bm_TGtkEntry_visible = $1;
     bp_TGtkEntry_visible = 0;
     bm_TGtkEntry_text_mb_dirty = $1;
     bp_TGtkEntry_text_mb_dirty = 0;
     bm_TGtkEntry_use_wchar = $2;
     bp_TGtkEntry_use_wchar = 1;
function  visible(var a : TGtkEntry) : guint;
procedure set_visible(var a : TGtkEntry; __visible : guint);
function  text_mb_dirty(var a : TGtkEntry) : guint;
procedure set_text_mb_dirty(var a : TGtkEntry; __text_mb_dirty : guint);
function  use_wchar(var a : TGtkEntry) : guint;
procedure set_use_wchar(var a : TGtkEntry; __use_wchar : guint);

  type
     PGtkEntryClass = ^TGtkEntryClass;
     TGtkEntryClass = record
          parent_class : TGtkEditableClass;
       end;

Type
  GTK_ENTRY=PGtkEntry;
  GTK_ENTRY_CLASS=PGtkEntryClass;

function  GTK_ENTRY_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_entry_get_type';
function  GTK_IS_ENTRY(obj:pointer):boolean;
function  GTK_IS_ENTRY_CLASS(klass:pointer):boolean;

function  gtk_entry_get_type:TGtkType;cdecl;external gtkdll name 'gtk_entry_get_type';
function  gtk_entry_new :PGtkWidget;cdecl;external gtkdll name 'gtk_entry_new';
function  gtk_entry_new_with_max_length (max:guint16):PGtkWidget;cdecl;external gtkdll name 'gtk_entry_new_with_max_length';
procedure gtk_entry_set_text(entry:PGtkEntry; text:Pgchar);cdecl;external gtkdll name 'gtk_entry_set_text';
procedure gtk_entry_append_text(entry:PGtkEntry; text:Pgchar);cdecl;external gtkdll name 'gtk_entry_append_text';
procedure gtk_entry_prepend_text(entry:PGtkEntry; text:Pgchar);cdecl;external gtkdll name 'gtk_entry_prepend_text';
procedure gtk_entry_set_position(entry:PGtkEntry; position:gint);cdecl;external gtkdll name 'gtk_entry_set_position';
function  gtk_entry_get_text (entry:PGtkEntry):Pgchar;cdecl;external gtkdll name 'gtk_entry_get_text';
procedure gtk_entry_select_region(entry:PGtkEntry; start:gint; theend:gint);cdecl;external gtkdll name 'gtk_entry_select_region';
procedure gtk_entry_set_visibility(entry:PGtkEntry; visible:gboolean);cdecl;external gtkdll name 'gtk_entry_set_visibility';
procedure gtk_entry_set_editable(entry:PGtkEntry; editable:gboolean);cdecl;external gtkdll name 'gtk_entry_set_editable';
procedure gtk_entry_set_max_length(entry:PGtkEntry; max:guint16);cdecl;external gtkdll name 'gtk_entry_set_max_length';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  visible(var a : TGtkEntry) : guint;
    begin
       visible:=(a.flag0 and bm_TGtkEntry_visible) shr bp_TGtkEntry_visible;
    end;

procedure set_visible(var a : TGtkEntry; __visible : guint);
    begin
       a.flag0:=a.flag0 or ((__visible shl bp_TGtkEntry_visible) and bm_TGtkEntry_visible);
    end;

function  text_mb_dirty(var a : TGtkEntry) : guint;
    begin
       text_mb_dirty:=(a.flag1 and bm_TGtkEntry_text_mb_dirty) shr bp_TGtkEntry_text_mb_dirty;
    end;

procedure set_text_mb_dirty(var a : TGtkEntry; __text_mb_dirty : guint);
    begin
       a.flag1:=a.flag1 or ((__text_mb_dirty shl bp_TGtkEntry_text_mb_dirty) and bm_TGtkEntry_text_mb_dirty);
    end;

function  use_wchar(var a : TGtkEntry) : guint;
    begin
       use_wchar:=(a.flag1 and bm_TGtkEntry_use_wchar) shr bp_TGtkEntry_use_wchar;
    end;

procedure set_use_wchar(var a : TGtkEntry; __use_wchar : guint);
    begin
       a.flag1:=a.flag1 or ((__use_wchar shl bp_TGtkEntry_use_wchar) and bm_TGtkEntry_use_wchar);
    end;

function  GTK_IS_ENTRY(obj:pointer):boolean;
begin
  GTK_IS_ENTRY:=(obj<>nil) and GTK_IS_ENTRY_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_ENTRY_CLASS(klass:pointer):boolean;
begin
  GTK_IS_ENTRY_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_ENTRY_TYPE);
end;

{$endif read_implementation}


