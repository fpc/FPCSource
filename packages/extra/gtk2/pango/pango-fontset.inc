// included by pango.pp
{$IFDEF read_interface_types}

{$IFDEF PANGO_ENABLE_BACKEND}
   PPangoFontset = ^TPangoFontset;
   TPangoFontset = record
        parent_instance : TGObject;
     end;

{ Padding for future expansion  }
   PPangoFontsetClass = ^TPangoFontsetClass;
   TPangoFontsetClass = record
        parent_class : TGObjectClass;
        get_font : function (fontset:PPangoFontset; wc:guint):PPangoFont; cdecl;
        get_metrics : function (fontset:PPangoFontset):PPangoFontMetrics; cdecl;
        get_language : function (fontset:PPangoFontset):PPangoLanguage; cdecl;
        _pango_reserved1 : procedure ; cdecl;
        _pango_reserved2 : procedure ; cdecl;
        _pango_reserved3 : procedure ; cdecl;
        _pango_reserved4 : procedure ; cdecl;
     end;
{$ENDIF}

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_functions}
{
   PangoFontset
}
function PANGO_TYPE_FONTSET : GType;
function PANGO_FONTSET(anObject : pointer) : PPangoFontset;
function PANGO_IS_FONTSET(anObject : pointer) : boolean;
function pango_fontset_get_type:GType; cdecl; external pangolib;

function pango_fontset_get_font(fontset:PPangoFontset; wc:guint):PPangoFont; cdecl; external pangolib;
function pango_fontset_get_metrics(fontset:PPangoFontset):PPangoFontMetrics; cdecl; external pangolib;

{$ifdef PANGO_ENABLE_BACKEND}
function PANGO_FONTSET_CLASS(klass : pointer) : PPangoFontsetClass;
function PANGO_IS_FONTSET_CLASS(klass : pointer) : boolean;
function PANGO_FONTSET_GET_CLASS(obj : PPangoFontset) : PPangoFontsetClass;

{
   PangoFontsetSimple
}

function PANGO_TYPE_FONTSET_SIMPLE : GType;
function PANGO_FONTSET_SIMPLE(anObject : pointer) : PPangoFontsetSimple;
function PANGO_IS_FONTSET_SIMPLE(anObject : pointer) : boolean;

function pango_fontset_simple_get_type:GType; cdecl; external pangolib;
function pango_fontset_simple_new(language:PPangoLanguage):PPangoFontsetSimple; cdecl; external pangolib;
procedure pango_fontset_simple_append(fontset:PPangoFontsetSimple; font:PPangoFont); cdecl; external pangolib;
function pango_fontset_simple_size(fontset:PPangoFontsetSimple):longint; cdecl; external pangolib;
{$endif}
{ PANGO_ENABLE_BACKEND  }
{$endif read_interface_functions}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function PANGO_TYPE_FONTSET : GType;
begin
  PANGO_TYPE_FONTSET:=pango_fontset_get_type;
end;

function PANGO_FONTSET(anObject : pointer) : PPangoFontset;
begin
  PANGO_FONTSET:=PPangoFontset(G_TYPE_CHECK_INSTANCE_CAST(anObject,
                                                          PANGO_TYPE_FONTSET));
end;

function PANGO_IS_FONTSET(anObject : pointer) : boolean;
begin
  PANGO_IS_FONTSET:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,PANGO_TYPE_FONTSET);
end;

function PANGO_FONTSET_CLASS(klass : pointer) : PPangoFontsetClass;
begin
  PANGO_FONTSET_CLASS:=PPangoFontsetClass(G_TYPE_CHECK_CLASS_CAST(klass,
                                                           PANGO_TYPE_FONTSET));
end;

function PANGO_IS_FONTSET_CLASS(klass : pointer) : boolean;
begin
  PANGO_IS_FONTSET_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,PANGO_TYPE_FONTSET);
end;

function PANGO_FONTSET_GET_CLASS(obj : PPangoFontset) : PPangoFontsetClass;
begin
  PANGO_FONTSET_GET_CLASS:=PPangoFontsetClass(G_TYPE_INSTANCE_GET_CLASS(obj,
                                              PANGO_TYPE_FONTSET));
end;

function PANGO_TYPE_FONTSET_SIMPLE : GType;
begin
  PANGO_TYPE_FONTSET_SIMPLE:=pango_fontset_simple_get_type;
end;

function PANGO_FONTSET_SIMPLE(anObject : pointer) : PPangoFontsetSimple;
begin
  PANGO_FONTSET_SIMPLE:=PPangoFontsetSimple(G_TYPE_CHECK_INSTANCE_CAST(anObject,
                                                    PANGO_TYPE_FONTSET_SIMPLE));
end;

function PANGO_IS_FONTSET_SIMPLE(anObject : pointer) : boolean;
begin
  PANGO_IS_FONTSET_SIMPLE:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,
                                                     PANGO_TYPE_FONTSET_SIMPLE);
end;
{$ENDIF}

