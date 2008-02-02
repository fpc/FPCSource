// included by pango.pp
{$IFDEF read_forward_definitions}
   PPangoAnalysis = ^TPangoAnalysis;
   PPangoItem = ^TPangoItem;
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
   TPangoAnalysis = record
        shape_engine : PPangoEngineShape;
        lang_engine : PPangoEngineLang;
        font : PPangoFont;
        level : guint8;
        language : PPangoLanguage;
        extra_attrs : PGSList;
     end;

   TPangoItem = record
        offset : gint;
        length : gint;
        num_chars : gint;
        analysis : TPangoAnalysis;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_functions}
function pango_item_new:PPangoItem; cdecl; external pangolib;
function pango_item_copy(item:PPangoItem):PPangoItem; cdecl; external pangolib;
procedure pango_item_free(item:PPangoItem); cdecl; external pangolib;
function pango_item_split(orig:PPangoItem; split_index:longint; split_offset:longint):PPangoItem; cdecl; external pangolib;
{$endif read_interface_functions}


