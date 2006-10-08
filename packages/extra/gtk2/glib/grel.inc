{*
 * grel.inc
 *
 * depends on gtypes.inc
 *}

  type
     PGTuples = ^TGTuples;
     TGTuples = record
          len : guint;
       end;
     PGRelation  = pointer;

  { GRelation

     Indexed Relations.  Imagine a really simple table in a
     database.  Relations are not ordered.  This data type is meant for
     maintaining a N-way mapping.

     g_relation_new() creates a relation with FIELDS fields

     g_relation_destroy() frees all resources
     g_tuples_destroy() frees the result of g_relation_select()

     g_relation_index() indexes relation FIELD with the provided
       equality and hash functions.  this must be done before any
       calls to insert are made.

     g_relation_insert() inserts a new tuple.  you are expected to
       provide the right number of fields.

     g_relation_delete() deletes all relations with KEY in FIELD
     g_relation_select() returns ...
     g_relation_count() counts ...
    }

  function g_relation_new(fields:gint):PGRelation;cdecl;external gliblib name 'g_relation_new';

  procedure g_relation_destroy(relation:PGRelation);cdecl;external gliblib name 'g_relation_destroy';

  procedure g_relation_index(relation:PGRelation; field:gint; hash_func:TGHashFunc; key_equal_func:TGEqualFunc);cdecl;external gliblib name 'g_relation_index';

  procedure g_relation_insert(relation:PGRelation);varargs;cdecl;external gliblib name 'g_relation_insert';

  function g_relation_delete(relation:PGRelation; key:gconstpointer; field:gint):gint;cdecl;external gliblib name 'g_relation_delete';

  function g_relation_select(relation:PGRelation; key:gconstpointer; field:gint):PGTuples;cdecl;external gliblib name 'g_relation_select';

  function g_relation_count(relation:PGRelation; key:gconstpointer; field:gint):gint;cdecl;external gliblib name 'g_relation_count';

  function g_relation_exists(relation:PGRelation):gboolean;varargs;cdecl;external gliblib name 'g_relation_exists';

  procedure g_relation_print(relation:PGRelation);cdecl;external gliblib name 'g_relation_print';

  procedure g_tuples_destroy(tuples:PGTuples);cdecl;external gliblib name 'g_tuples_destroy';

  function g_tuples_index(tuples:PGTuples; index:gint; field:gint):gpointer;cdecl;external gliblib name 'g_tuples_index';


