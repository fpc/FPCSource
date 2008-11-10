// included by pango.pp

{$IFDEF read_forward_definitions}
  PPangoMatrix = ^TPangoMatrix;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   TPangoMatrix = record
        xx : double;
        xy : double;
        yx : double;
        yy : double;
        x0 : double;
        y0 : double;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_functions}
function PANGO_TYPE_MATRIX : GType;

// define PANGO_MATRIX_INIT { 1., 0., 0., 1., 0., 0. }

function pango_matrix_get_type: GType;cdecl; external pangolib;
function pango_matrix_copy(matrix:PPangoMatrix):PPangoMatrix;cdecl; external pangolib;
procedure pango_matrix_free(matrix:PPangoMatrix);cdecl; external pangolib;
procedure pango_matrix_translate(matrix:PPangoMatrix; tx:double; ty:double);cdecl; external pangolib;
procedure pango_matrix_scale(matrix:PPangoMatrix; scale_x:double; scale_y:double);cdecl; external pangolib;
procedure pango_matrix_rotate(matrix:PPangoMatrix; degrees:double);cdecl; external pangolib;
procedure pango_matrix_concat(matrix:PPangoMatrix; new_matrix:PPangoMatrix);cdecl; external pangolib;
{ pango 1.16
procedure pango_matrix_transform_point(matrix:PPangoMatrix; x:Pdouble; y:Pdouble);cdecl; external pangolib;
procedure pango_matrix_transform_distance(matrix:PPangoMatrix; dx:Pdouble; dy:Pdouble);cdecl; external pangolib;
procedure pango_matrix_transform_rectangle(matrix:PPangoMatrix; rect:PPangoRectangle);cdecl; external pangolib;
procedure pango_matrix_transform_pixel_rectangle(matrix:PPangoMatrix; rect:PPangoRectangle);cdecl; external pangolib;
}
{ pango 1.12
function pango_matrix_get_font_scale_factor(matrix:PPangoMatrix):double; cdecl; external pangolib;
}
{$endif read_interface_functions}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function PANGO_TYPE_MATRIX : GType;
begin
  PANGO_TYPE_MATRIX:=pango_matrix_get_type;
end;
{$ENDIF}
