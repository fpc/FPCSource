// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkTableRowCol = ^TGtkTableRowCol;

   PGtkTable = ^TGtkTable;
   TGtkTable = record
        container : TGtkContainer;
        children : PGList;
        rows : PGtkTableRowCol;
        cols : PGtkTableRowCol;
        nrows : guint16;
        ncols : guint16;
        column_spacing : guint16;
        row_spacing : guint16;
        flag0 : word;
     end;

   PGtkTableClass = ^TGtkTableClass;
   TGtkTableClass = record
        parent_class : TGtkContainerClass;
     end;

   PGtkTableChild = ^TGtkTableChild;
   TGtkTableChild = record
        widget : PGtkWidget;
        left_attach : guint16;
        right_attach : guint16;
        top_attach : guint16;
        bottom_attach : guint16;
        xpadding : guint16;
        ypadding : guint16;
        flag0 : word;
     end;

   TGtkTableRowCol = record
        requisition : guint16;
        allocation : guint16;
        spacing : guint16;
        flag0 : word;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkTable_homogeneous = $1;
   bp_TGtkTable_homogeneous = 0;

   bm_TGtkTableChild_xexpand = $1;
   bp_TGtkTableChild_xexpand = 0;
   bm_TGtkTableChild_yexpand = $2;
   bp_TGtkTableChild_yexpand = 1;
   bm_TGtkTableChild_xshrink = $4;
   bp_TGtkTableChild_xshrink = 2;
   bm_TGtkTableChild_yshrink = $8;
   bp_TGtkTableChild_yshrink = 3;
   bm_TGtkTableChild_xfill = $10;
   bp_TGtkTableChild_xfill = 4;
   bm_TGtkTableChild_yfill = $20;
   bp_TGtkTableChild_yfill = 5;

   bm_TGtkTableRowCol_need_expand = $1;
   bp_TGtkTableRowCol_need_expand = 0;
   bm_TGtkTableRowCol_need_shrink = $2;
   bp_TGtkTableRowCol_need_shrink = 1;
   bm_TGtkTableRowCol_expand = $4;
   bp_TGtkTableRowCol_expand = 2;
   bm_TGtkTableRowCol_shrink = $8;
   bp_TGtkTableRowCol_shrink = 3;
   bm_TGtkTableRowCol_empty = $10;
   bp_TGtkTableRowCol_empty = 4;


function GTK_TYPE_TABLE : GType;
function GTK_TABLE(obj: pointer) : PGtkTable;
function GTK_TABLE_CLASS(klass: pointer) : PGtkTableClass;
function GTK_IS_TABLE(obj: pointer) : boolean;
function GTK_IS_TABLE_CLASS(klass: pointer) : boolean;
function GTK_TABLE_GET_CLASS(obj: pointer) : PGtkTableClass;

function homogeneous(var a : TGtkTable) : guint; overload;
procedure set_homogeneous(var a : TGtkTable; __homogeneous : guint); overload;

function xexpand(var a : TGtkTableChild) : guint;
procedure set_xexpand(var a : TGtkTableChild; __xexpand : guint);
function yexpand(var a : TGtkTableChild) : guint;
procedure set_yexpand(var a : TGtkTableChild; __yexpand : guint);
function xshrink(var a : TGtkTableChild) : guint;
procedure set_xshrink(var a : TGtkTableChild; __xshrink : guint);
function yshrink(var a : TGtkTableChild) : guint;
procedure set_yshrink(var a : TGtkTableChild; __yshrink : guint);
function xfill(var a : TGtkTableChild) : guint;
procedure set_xfill(var a : TGtkTableChild; __xfill : guint);
function yfill(var a : TGtkTableChild) : guint;
procedure set_yfill(var a : TGtkTableChild; __yfill : guint);

function need_expand(var a : TGtkTableRowCol) : guint;
procedure set_need_expand(var a : TGtkTableRowCol; __need_expand : guint);
function need_shrink(var a : TGtkTableRowCol) : guint;
procedure set_need_shrink(var a : TGtkTableRowCol; __need_shrink : guint);
function expand(var a : TGtkTableRowCol) : guint; overload;
procedure set_expand(var a : TGtkTableRowCol; __expand : guint); overload;
function shrink(var a : TGtkTableRowCol) : guint;
procedure set_shrink(var a : TGtkTableRowCol; __shrink : guint);
function empty(var a : TGtkTableRowCol) : guint;
procedure set_empty(var a : TGtkTableRowCol; __empty : guint);

function gtk_table_get_type:TGtkType; cdecl; external gtklib;
function gtk_table_new(rows:guint; columns:guint; homogeneous:gboolean):PGtkWidget; cdecl; external gtklib;
procedure gtk_table_resize(table:PGtkTable; rows:guint; columns:guint); cdecl; external gtklib;
procedure gtk_table_attach(table:PGtkTable; child:PGtkWidget; left_attach:guint; right_attach:guint; top_attach:guint;
            bottom_attach:guint; xoptions:TGtkAttachOptions; yoptions:TGtkAttachOptions; xpadding:guint; ypadding:guint); cdecl; external gtklib;
procedure gtk_table_attach_defaults(table:PGtkTable; widget:PGtkWidget; left_attach:guint; right_attach:guint; top_attach:guint;
            bottom_attach:guint); cdecl; external gtklib;
procedure gtk_table_set_row_spacing(table:PGtkTable; row:guint; spacing:guint); cdecl; external gtklib;
function gtk_table_get_row_spacing(table:PGtkTable; row:guint):guint; cdecl; external gtklib;
procedure gtk_table_set_col_spacing(table:PGtkTable; column:guint; spacing:guint); cdecl; external gtklib;
function gtk_table_get_col_spacing(table:PGtkTable; column:guint):guint; cdecl; external gtklib;
procedure gtk_table_set_row_spacings(table:PGtkTable; spacing:guint); cdecl; external gtklib;
function gtk_table_get_default_row_spacing(table:PGtkTable):guint; cdecl; external gtklib;
procedure gtk_table_set_col_spacings(table:PGtkTable; spacing:guint); cdecl; external gtklib;
function gtk_table_get_default_col_spacing(table:PGtkTable):guint; cdecl; external gtklib;
procedure gtk_table_set_homogeneous(table:PGtkTable; homogeneous:gboolean); cdecl; external gtklib;
function gtk_table_get_homogeneous(table:PGtkTable):gboolean; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TABLE : GType;
begin
   GTK_TYPE_TABLE:=gtk_table_get_type;
end;

function GTK_TABLE(obj: pointer) : PGtkTable;
begin
   GTK_TABLE:=PGtkTable(GTK_CHECK_CAST(obj,GTK_TYPE_TABLE));
end;

function GTK_TABLE_CLASS(klass: pointer) : PGtkTableClass;
begin
   GTK_TABLE_CLASS:=PGtkTableClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TABLE));
end;

function GTK_IS_TABLE(obj: pointer) : boolean;
begin
   GTK_IS_TABLE:=GTK_CHECK_TYPE(obj,GTK_TYPE_TABLE);
end;

function GTK_IS_TABLE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TABLE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TABLE);
end;

function GTK_TABLE_GET_CLASS(obj: pointer) : PGtkTableClass;
begin
   GTK_TABLE_GET_CLASS:=PGtkTableClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TABLE));
end;

function homogeneous(var a : TGtkTable) : guint;
begin
   homogeneous:=(a.flag0 and bm_TGtkTable_homogeneous) shr bp_TGtkTable_homogeneous;
end;

procedure set_homogeneous(var a : TGtkTable; __homogeneous : guint);
begin
   a.flag0:=a.flag0 or ((__homogeneous shl bp_TGtkTable_homogeneous) and bm_TGtkTable_homogeneous);
end;

function xexpand(var a : TGtkTableChild) : guint;
begin
   xexpand:=(a.flag0 and bm_TGtkTableChild_xexpand) shr bp_TGtkTableChild_xexpand;
end;

procedure set_xexpand(var a : TGtkTableChild; __xexpand : guint);
begin
   a.flag0:=a.flag0 or ((__xexpand shl bp_TGtkTableChild_xexpand) and bm_TGtkTableChild_xexpand);
end;

function yexpand(var a : TGtkTableChild) : guint;
begin
   yexpand:=(a.flag0 and bm_TGtkTableChild_yexpand) shr bp_TGtkTableChild_yexpand;
end;

procedure set_yexpand(var a : TGtkTableChild; __yexpand : guint);
begin
   a.flag0:=a.flag0 or ((__yexpand shl bp_TGtkTableChild_yexpand) and bm_TGtkTableChild_yexpand);
end;

function xshrink(var a : TGtkTableChild) : guint;
begin
   xshrink:=(a.flag0 and bm_TGtkTableChild_xshrink) shr bp_TGtkTableChild_xshrink;
end;

procedure set_xshrink(var a : TGtkTableChild; __xshrink : guint);
begin
   a.flag0:=a.flag0 or ((__xshrink shl bp_TGtkTableChild_xshrink) and bm_TGtkTableChild_xshrink);
end;

function yshrink(var a : TGtkTableChild) : guint;
begin
   yshrink:=(a.flag0 and bm_TGtkTableChild_yshrink) shr bp_TGtkTableChild_yshrink;
end;

procedure set_yshrink(var a : TGtkTableChild; __yshrink : guint);
begin
   a.flag0:=a.flag0 or ((__yshrink shl bp_TGtkTableChild_yshrink) and bm_TGtkTableChild_yshrink);
end;

function xfill(var a : TGtkTableChild) : guint;
begin
   xfill:=(a.flag0 and bm_TGtkTableChild_xfill) shr bp_TGtkTableChild_xfill;
end;

procedure set_xfill(var a : TGtkTableChild; __xfill : guint);
begin
   a.flag0:=a.flag0 or ((__xfill shl bp_TGtkTableChild_xfill) and bm_TGtkTableChild_xfill);
end;

function yfill(var a : TGtkTableChild) : guint;
begin
   yfill:=(a.flag0 and bm_TGtkTableChild_yfill) shr bp_TGtkTableChild_yfill;
end;

procedure set_yfill(var a : TGtkTableChild; __yfill : guint);
begin
   a.flag0:=a.flag0 or ((__yfill shl bp_TGtkTableChild_yfill) and bm_TGtkTableChild_yfill);
end;

function need_expand(var a : TGtkTableRowCol) : guint;
begin
   need_expand:=(a.flag0 and bm_TGtkTableRowCol_need_expand) shr bp_TGtkTableRowCol_need_expand;
end;

procedure set_need_expand(var a : TGtkTableRowCol; __need_expand : guint);
begin
   a.flag0:=a.flag0 or ((__need_expand shl bp_TGtkTableRowCol_need_expand) and bm_TGtkTableRowCol_need_expand);
end;

function need_shrink(var a : TGtkTableRowCol) : guint;
begin
   need_shrink:=(a.flag0 and bm_TGtkTableRowCol_need_shrink) shr bp_TGtkTableRowCol_need_shrink;
end;

procedure set_need_shrink(var a : TGtkTableRowCol; __need_shrink : guint);
begin
   a.flag0:=a.flag0 or ((__need_shrink shl bp_TGtkTableRowCol_need_shrink) and bm_TGtkTableRowCol_need_shrink);
end;

function expand(var a : TGtkTableRowCol) : guint;
begin
   expand:=(a.flag0 and bm_TGtkTableRowCol_expand) shr bp_TGtkTableRowCol_expand;
end;

procedure set_expand(var a : TGtkTableRowCol; __expand : guint);
begin
   a.flag0:=a.flag0 or ((__expand shl bp_TGtkTableRowCol_expand) and bm_TGtkTableRowCol_expand);
end;

function shrink(var a : TGtkTableRowCol) : guint;
begin
   shrink:=(a.flag0 and bm_TGtkTableRowCol_shrink) shr bp_TGtkTableRowCol_shrink;
end;

procedure set_shrink(var a : TGtkTableRowCol; __shrink : guint);
begin
   a.flag0:=a.flag0 or ((__shrink shl bp_TGtkTableRowCol_shrink) and bm_TGtkTableRowCol_shrink);
end;

function empty(var a : TGtkTableRowCol) : guint;
begin
   empty:=(a.flag0 and bm_TGtkTableRowCol_empty) shr bp_TGtkTableRowCol_empty;
end;

procedure set_empty(var a : TGtkTableRowCol; __empty : guint);
begin
   a.flag0:=a.flag0 or ((__empty shl bp_TGtkTableRowCol_empty) and bm_TGtkTableRowCol_empty);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

