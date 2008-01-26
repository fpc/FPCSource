// included by pango.pp
{$IFDEF read_interface_types}

{$IFDEF PANGO_ENABLE_BACKEND}
   PPangoFontMap = ^TPangoFontMap;
   TPangoFontMap = record
        parent_instance : TGObject;
     end;

{ Padding for future expansion  }
   TPangoFontMapClassLoadFont =
     function (fontmap:PPangoFontMap; context:PPangoContext;
               desc:PPangoFontDescription):PPangoFont; cdecl;
   TPangoFontMapClassListFamilies =
     procedure (fontmap:PPangoFontMap; families:PPPPangoFontFamily;
                n_families:Plongint); cdecl;
   TPangoFontMapClassFontSet =
     function (fontmap:PPangoFontMap; context:PPangoContext;
               desc:PPangoFontDescription;
               language:PPangoLanguage):PPangoFontset; cdecl;

   PPangoFontMapClass = ^TPangoFontMapClass;
   TPangoFontMapClass = record
        parent_class : TGObjectClass;
        load_font : TPangoFontMapClassLoadFont;
        list_families : TPangoFontMapClassListFamilies;
        load_fontset : TPangoFontMapClassFontSet;
        _pango_reserved1 : procedure ; cdecl;
        _pango_reserved2 : procedure ; cdecl;
        _pango_reserved3 : procedure ; cdecl;
        _pango_reserved4 : procedure ; cdecl;
     end;
{$ENDIF}


{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_functions}
function PANGO_TYPE_FONT_MAP : GType;
function PANGO_FONT_MAP(anObject : pointer) : PPangoFontMap;
function PANGO_IS_FONT_MAP(anObject : pointer) : boolean;

function pango_font_map_get_type:GType; cdecl; external pangolib;
function pango_font_map_load_font(fontmap:PPangoFontMap; context:PPangoContext; desc:PPangoFontDescription):PPangoFont; cdecl; external pangolib;
function pango_font_map_load_fontset(fontmap:PPangoFontMap; context:PPangoContext; desc:PPangoFontDescription; language:PPangoLanguage):PPangoFontset; cdecl; external pangolib;
procedure pango_font_map_list_families(fontmap:PPangoFontMap; families:PPPPangoFontFamily; n_families:Plongint); cdecl; external pangolib;

{$ifdef PANGO_ENABLE_BACKEND}
function PANGO_FONT_MAP_CLASS(klass : pointer) : PPangoFontMapClass;
function PANGO_IS_FONT_MAP_CLASS(klass : pointer) : boolean;
function PANGO_FONT_MAP_GET_CLASS(obj : PPangoFontMap) : PPangoFontMapClass;
{$endif}
{ PANGO_ENABLE_BACKEND  }
{$endif read_interface_functions}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function PANGO_TYPE_FONT_MAP : GType;
begin
  PANGO_TYPE_FONT_MAP:=pango_font_map_get_type;
end;

function PANGO_FONT_MAP(anObject : pointer) : PPangoFontmap;
begin
  PANGO_FONT_MAP:=PPangoFontmap(G_TYPE_CHECK_INSTANCE_CAST(anObject,
                                                          PANGO_TYPE_FONT_MAP));
end;

function PANGO_IS_FONT_MAP(anObject : pointer) : boolean;
begin
  PANGO_IS_FONT_MAP:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,PANGO_TYPE_FONT_MAP);
end;

function PANGO_FONT_MAP_CLASS(klass : pointer) : PPangoFontMapClass;
begin
  PANGO_FONT_MAP_CLASS:=PPangoFontMapClass(G_TYPE_CHECK_CLASS_CAST(klass,
                                                          PANGO_TYPE_FONT_MAP));
end;

function PANGO_IS_FONT_MAP_CLASS(klass : pointer) : boolean;
begin
  PANGO_IS_FONT_MAP_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,PANGO_TYPE_FONT_MAP);
end;

function PANGO_FONT_MAP_GET_CLASS(obj : PPangoFontMap) : PPangoFontMapClass;
begin
   PANGO_FONT_MAP_GET_CLASS:=PPangoFontMapClass(G_TYPE_INSTANCE_GET_CLASS(obj,
                                                          PANGO_TYPE_FONT_MAP));
end;
{$ENDIF}

