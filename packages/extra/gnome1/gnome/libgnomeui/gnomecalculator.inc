{$IfDef read_interface}

type
   TGnomeCalculatorMode = (GNOME_CALCULATOR_DEG,GNOME_CALCULATOR_RAD,
     GNOME_CALCULATOR_GRAD);

   PGnomeCalculator = ^TGnomeCalculator;
   TGnomeCalculator = record
        vbox : TGtkVBox;
        result : gdouble;
        result_string : array[0..12] of gchar;
        memory : gdouble;
        display : PGtkWidget;
        mode : TGnomeCalculatorMode;
        flag0 : word;
        invert_button : PGtkWidget;
        stack : PGList;
        accel : PGtkAccelGroup;
     end;
   GNOME_CALCULATOR = PGnomeCalculator;

const
   bm__GnomeCalculator_add_digit = $1;
   bp__GnomeCalculator_add_digit = 0;
   bm__GnomeCalculator_error = $2;
   bp__GnomeCalculator_error = 1;
   bm__GnomeCalculator_invert = $4;
   bp__GnomeCalculator_invert = 2;

function add_digit(var a : TGnomeCalculator) : guint;
procedure set_add_digit(var a : TGnomeCalculator; __add_digit : guint);
function error(var a : TGnomeCalculator) : guint;
procedure set_error(var a : TGnomeCalculator; __error : guint);
function invert(var a : TGnomeCalculator) : guint;
procedure set_invert(var a : TGnomeCalculator; __invert : guint);

type
   PGnomeCalculatorClass = ^TGnomeCalculatorClass;
   TGnomeCalculatorClass = record
        parent_class : TGtkVBoxClass;
        result_changed : procedure (gc:PGnomeCalculator; result:gdouble);cdecl;
     end;
   GNOME_CALCULATOR_CLASS = PGnomeCalculatorClass;

function GNOME_TYPE_CALCULATOR : TGTKType;
function GNOME_IS_CALCULATOR(obj : Pointer) : Boolean;
function GNOME_IS_CALCULATOR_CLASS(klass : Pointer) : Boolean;

function gnome_calculator_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_calculator_get_type';
function gnome_calculator_new:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_calculator_new';
procedure gnome_calculator_clear(gc:PGnomeCalculator; doreset:gboolean);cdecl;external libgnomeuidll name 'gnome_calculator_clear';
procedure gnome_calculator_set(gc:PGnomeCalculator; result:gdouble);cdecl;external libgnomeuidll name 'gnome_calculator_set';

function gnome_calculator_get_result(gc : PGnomeCalculator) : gdouble;

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_CALCULATOR : TGTKType;
begin
  GNOME_TYPE_CALCULATOR:=gnome_calculator_get_type;
end;

function GNOME_IS_CALCULATOR(obj : Pointer) : Boolean;
begin
   GNOME_IS_CALCULATOR:=(obj<>nil) and GNOME_IS_CALCULATOR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_CALCULATOR_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_CALCULATOR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_CALCULATOR);
end;

function add_digit(var a : TGnomeCalculator) : guint;
begin
   add_digit:=(a.flag0 and bm__GnomeCalculator_add_digit) shr bp__GnomeCalculator_add_digit;
end;

procedure set_add_digit(var a : TGnomeCalculator; __add_digit : guint);
begin
   a.flag0:=a.flag0 or ((__add_digit shl bp__GnomeCalculator_add_digit) and bm__GnomeCalculator_add_digit);
end;

function error(var a : TGnomeCalculator) : guint;
begin
   error:=(a.flag0 and bm__GnomeCalculator_error) shr bp__GnomeCalculator_error;
end;

procedure set_error(var a : TGnomeCalculator; __error : guint);
begin
   a.flag0:=a.flag0 or ((__error shl bp__GnomeCalculator_error) and bm__GnomeCalculator_error);
end;

function invert(var a : TGnomeCalculator) : guint;
begin
   invert:=(a.flag0 and bm__GnomeCalculator_invert) shr bp__GnomeCalculator_invert;
end;

procedure set_invert(var a : TGnomeCalculator; __invert : guint);
begin
   a.flag0:=a.flag0 or ((__invert shl bp__GnomeCalculator_invert) and bm__GnomeCalculator_invert);
end;

function gnome_calculator_get_result(gc : PGnomeCalculator) : gdouble;
begin
   gnome_calculator_get_result:=gc^.result;
end;

{$Endif read_implementation}
