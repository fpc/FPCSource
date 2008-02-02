// included by pango.pp
{$IFDEF read_forward_definitions}
{$ENDIF}

{$IFDEF read_interface_types}
   PPangoEngineRange = ^TPangoEngineRange;
   TPangoEngineRange = record
        start : guint32;
        theEnd : guint32;
        langs : Pgchar;
     end;

   PPangoEngineInfo = ^TPangoEngineInfo;
   TPangoEngineInfo = record
        id : Pgchar;
        engine_type : Pgchar;
        render_type : Pgchar;
        ranges : PPangoEngineRange;
        n_ranges : gint;
     end;

   PPangoEngine = ^TPangoEngine;
   TPangoEngine = record
        id : Pgchar;
        _type : Pgchar;
        length : gint;
     end;

   TPangoEngineLangScriptBreak =
     procedure (text:Pchar; len:longint; analysis:PPangoAnalysis;
                attrs:PPangoLogAttr; attrs_len:longint); cdecl;

   PPangoEngineLang = ^TPangoEngineLang;
   TPangoEngineLang = record
        engine : TPangoEngine;
        script_break : TPangoEngineLangScriptBreak;
     end;

   TPangoEngineShapeScript =
     procedure (font:PPangoFont; text:Pchar; length:longint;
                analysis:PPangoAnalysis; glyphs:PPangoGlyphString); cdecl;
   TPangoEngineShapeGetCoverage =
     function (font:PPangoFont; language:PPangoLanguage):PPangoCoverage; cdecl;

   PPangoEngineShape = ^TPangoEngineShape;
   TPangoEngineShape = record
        engine : TPangoEngine;
        script_shape : TPangoEngineShapeScript;
        get_coverage : TPangoEngineShapeGetCoverage
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_functions}

{$ifdef PANGO_ENABLE_ENGINE}
{ Module API  }

const
   PANGO_ENGINE_TYPE_LANG = 'PangoEngineLang';
   PANGO_ENGINE_TYPE_SHAPE = 'PangoEngineShape';
   PANGO_RENDER_TYPE_NONE = 'PangoRenderNone';

{ A module should export the following functions  }
procedure script_engine_list(var engines:PPangoEngineInfo; n_engines:Plongint); cdecl; external pangolib;
function script_engine_load(id:Pchar):PPangoEngine; cdecl; external pangolib;
procedure script_engine_unload(engine:PPangoEngine); cdecl; external pangolib;
{$endif}
{ PANGO_ENABLE_ENGINE  }
{$endif read_interface_functions}



