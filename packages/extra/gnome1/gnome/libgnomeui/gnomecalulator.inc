unit gnomecalulator;
interface

{
  Automatically converted by H2Pas 0.99.15 from gnome-calculator.h
  The following command line parameters were used:
    -d
    -c
    -s
    -o
    gnomecalulator.inc
    gnome-calculator.h
}

{$PACKRECORDS C}

{$ifndef GNOME_CALCULATOR_H}
{$define GNOME_CALCULATOR_H}
{$include <gdk/gdk.h>}
{$include <gtk/gtkvbox.h>}
{$include <libgnome/gnome-defs.h>}
{ was #define dname def_expr }
function GNOME_TYPE_CALCULATOR : longint;
    { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function GNOME_CALCULATOR(obj : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function GNOME_CALCULATOR_CLASS(klass : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function GNOME_IS_CALCULATOR(obj : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function GNOME_IS_CALCULATOR_CLASS(klass : longint) : longint;

type
   _GnomeCalculator = GnomeCalculator;
   _GnomeCalculatorClass = GnomeCalculatorClass;

   GnomeCalculatorMode = (GNOME_CALCULATOR_DEG,GNOME_CALCULATOR_RAD,
     GNOME_CALCULATOR_GRAD);
   _GnomeCalculator = record
        vbox : GtkVBox;
        result : gdouble;
        result_string : array[0..12] of gchar;
        memory : gdouble;
        display : ^GtkWidget;
        mode : GnomeCalculatorMode;
        flag0 : word;
        invert_button : ^GtkWidget;
        stack : ^GList;
        accel : ^GtkAccelGroup;
     end;

const
   bm__GnomeCalculator_add_digit = $1;
   bp__GnomeCalculator_add_digit = 0;
   bm__GnomeCalculator_error = $2;
   bp__GnomeCalculator_error = 1;
   bm__GnomeCalculator_invert = $4;
   bp__GnomeCalculator_invert = 2;
function add_digit(var a : _GnomeCalculator) : guint;
procedure set_add_digit(var a : _GnomeCalculator; __add_digit : guint);
function error(var a : _GnomeCalculator) : guint;
procedure set_error(var a : _GnomeCalculator; __error : guint);
function invert(var a : _GnomeCalculator) : guint;
procedure set_invert(var a : _GnomeCalculator; __invert : guint);
type
   _GnomeCalculatorClass = record
        parent_class : GtkVBoxClass;
        result_changed : procedure (gc:PGnomeCalculator; result:gdouble);cdecl;
     end;


function gnome_calculator_get_type:guint;cdecl;external;
function gnome_calculator_new:^GtkWidget;cdecl;external;
(* Const before type ignored *)
procedure gnome_calculator_clear(gc:PGnomeCalculator; reset:gboolean);cdecl;external;
procedure gnome_calculator_set(gc:PGnomeCalculator; result:gdouble);cdecl;external;
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function gnome_calculator_get_result(gc : longint) : longint;

{$endif}

implementation

{ was #define dname def_expr }
function GNOME_TYPE_CALCULATOR : longint;
    { return type might be wrong }
  begin
     GNOME_TYPE_CALCULATOR:=gnome_calculator_get_type;
  end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function GNOME_CALCULATOR(obj : longint) : longint;
begin
   GNOME_CALCULATOR:=GTK_CHECK_CAST(obj,GNOME_TYPE_CALCULATOR,GnomeCalculator);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function GNOME_CALCULATOR_CLASS(klass : longint) : longint;
begin
   GNOME_CALCULATOR_CLASS:=GTK_CHECK_CLASS_CAST(klass,GNOME_TYPE_CALCULATOR,GnomeCalculatorClass);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function GNOME_IS_CALCULATOR(obj : longint) : longint;
begin
   GNOME_IS_CALCULATOR:=GTK_CHECK_TYPE(obj,GNOME_TYPE_CALCULATOR);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function GNOME_IS_CALCULATOR_CLASS(klass : longint) : longint;
begin
   GNOME_IS_CALCULATOR_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GNOME_TYPE_CALCULATOR);
end;

function add_digit(var a : _GnomeCalculator) : guint;
begin
   add_digit:=(a.flag0 and bm__GnomeCalculator_add_digit) shr bp__GnomeCalculator_add_digit;
end;

procedure set_add_digit(var a : _GnomeCalculator; __add_digit : guint);
begin
   a.flag0:=a.flag0 or ((__add_digit shl bp__GnomeCalculator_add_digit) and bm__GnomeCalculator_add_digit);
end;

function error(var a : _GnomeCalculator) : guint;
begin
   error:=(a.flag0 and bm__GnomeCalculator_error) shr bp__GnomeCalculator_error;
end;

procedure set_error(var a : _GnomeCalculator; __error : guint);
begin
   a.flag0:=a.flag0 or ((__error shl bp__GnomeCalculator_error) and bm__GnomeCalculator_error);
end;

function invert(var a : _GnomeCalculator) : guint;
begin
   invert:=(a.flag0 and bm__GnomeCalculator_invert) shr bp__GnomeCalculator_invert;
end;

procedure set_invert(var a : _GnomeCalculator; __invert : guint);
begin
   a.flag0:=a.flag0 or ((__invert shl bp__GnomeCalculator_invert) and bm__GnomeCalculator_invert);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
function gnome_calculator_get_result(gc : longint) : longint;
begin
   gnome_calculator_get_result:=(GNOME_CALCULATOR(gc))^.result;
end;


end.
