// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGTuples = ^TGTuples;
  TGTuples = record
    len : guint;
  end;
  PGRelation  = pointer;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function g_relation_new(fields:gint):PGRelation;cdecl;external gliblib name 'g_relation_new';
procedure g_relation_destroy(relation:PGRelation);cdecl;external gliblib name 'g_relation_destroy';
procedure g_relation_index(relation:PGRelation; field:gint; hash_func:TGHashFunc; key_equal_func:TGEqualFunc);cdecl;external gliblib name 'g_relation_index';
{$IFNDEF KYLIX}
procedure g_relation_insert(relation:PGRelation; args:array of const);cdecl;overload;external gliblib name 'g_relation_insert';
procedure g_relation_insert(relation:PGRelation);cdecl;overload;varargs;external gliblib name 'g_relation_insert';
{$ELSE}
procedure g_relation_insert(relation:PGRelation);varargs;cdecl;external gliblib name 'g_relation_insert';
{$ENDIF}
function g_relation_delete(relation:PGRelation; key:gconstpointer; field:gint):gint;cdecl;external gliblib name 'g_relation_delete';
function g_relation_select(relation:PGRelation; key:gconstpointer; field:gint):PGTuples;cdecl;external gliblib name 'g_relation_select';
function g_relation_count(relation:PGRelation; key:gconstpointer; field:gint):gint;cdecl;external gliblib name 'g_relation_count';
{$IFNDEF KYLIX}
function g_relation_exists(relation:PGRelation; args:array of const):gboolean;cdecl;overload;external gliblib name 'g_relation_exists';
function g_relation_exists(relation:PGRelation):gboolean;cdecl;overload;varargs;external gliblib name 'g_relation_exists';
{$ELSE}
function g_relation_exists(relation:PGRelation):gboolean;varargs;cdecl;external gliblib name 'g_relation_exists';
{$ENDIF}
procedure g_relation_print(relation:PGRelation);cdecl;external gliblib name 'g_relation_print';
procedure g_tuples_destroy(tuples:PGTuples);cdecl;external gliblib name 'g_tuples_destroy';
function g_tuples_index(tuples:PGTuples; index:gint; field:gint):gpointer;cdecl;external gliblib name 'g_tuples_index';
{$ENDIF read_interface_rest}
// included by glib2.pas


