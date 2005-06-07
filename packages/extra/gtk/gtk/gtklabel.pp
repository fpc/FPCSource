{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkLabelWord = pointer;

       PGtkLabel = ^TGtkLabel;
       TGtkLabel = record
          misc : TGtkMisc;
          thelabel : Pgchar;
          label_wc : PGdkWChar;
          pattern : Pgchar;
          words : PGtkLabelWord;
          flag0 : longint;
          wrap : gboolean;
        end;

  const
     bm_TGtkLabel_max_width = $FFFF;
     bp_TGtkLabel_max_width = 0;
     bm_TGtkLabel_jtype = $30000;
     bp_TGtkLabel_jtype = 16;
function  max_width(var a : TGtkLabel) : guint;
procedure set_max_width(var a : TGtkLabel; __max_width : guint);
function  jtype(var a : TGtkLabel) : guint;
procedure set_jtype(var a : TGtkLabel; __jtype : guint);

    type
       PGtkLabelClass = ^TGtkLabelClass;
       TGtkLabelClass = record
            parent_class : TGtkMiscClass;
         end;

Type
  GTK_LABEL=PGtkLabel;
  GTK_LABEL_CLASS=PGtkLabelClass;

function  GTK_LABEL_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_label_get_type';
function  GTK_IS_LABEL(obj:pointer):boolean;
function  GTK_IS_LABEL_CLASS(klass:pointer):boolean;

function  gtk_label_get_type:TGtkType;cdecl;external gtkdll name 'gtk_label_get_type';
function  gtk_label_new (str:pchar):PGtkWidget;cdecl;external gtkdll name 'gtk_label_new';
procedure gtk_label_set_text(theLabel: PGtkLabel; str:pchar);cdecl;external gtkdll name 'gtk_label_set_text';
procedure gtk_label_set_justify(thelabel:PGtkLabel; jtype:TGtkJustification);cdecl;external gtkdll name 'gtk_label_set_justify';
procedure gtk_label_set_pattern(thelabel:PGtkLabel; pattern:Pgchar);cdecl;external gtkdll name 'gtk_label_set_pattern';
procedure gtk_label_set_line_wrap(thelabel:PGtkLabel; wrap:gboolean);cdecl;external gtkdll name 'gtk_label_set_line_wrap';
procedure gtk_label_get(thelabel:PGtkLabel; str:ppchar);cdecl;external gtkdll name 'gtk_label_get';
function  gtk_label_parse_uline(thelabel:PGtkLabel;thestring:Pgchar):guint;cdecl;external gtkdll name 'gtk_label_parse_uline';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  max_width(var a : TGtkLabel) : guint;
    begin
       max_width:=(a.flag0 and bm_TGtkLabel_max_width) shr bp_TGtkLabel_max_width;
    end;

procedure set_max_width(var a : TGtkLabel; __max_width : guint);
    begin
       a.flag0:=a.flag0 or ((__max_width shl bp_TGtkLabel_max_width) and bm_TGtkLabel_max_width);
    end;

function  jtype(var a : TGtkLabel) : guint;
    begin
       jtype:=(a.flag0 and bm_TGtkLabel_jtype) shr bp_TGtkLabel_jtype;
    end;

procedure set_jtype(var a : TGtkLabel; __jtype : guint);
    begin
       a.flag0:=a.flag0 or ((__jtype shl bp_TGtkLabel_jtype) and bm_TGtkLabel_jtype);
    end;

function  GTK_IS_LABEL(obj:pointer):boolean;
begin
  GTK_IS_LABEL:=(obj<>nil) and GTK_IS_LABEL_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_LABEL_CLASS(klass:pointer):boolean;
begin
  GTK_IS_LABEL_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_LABEL_TYPE);
end;

{$endif read_implementation}


