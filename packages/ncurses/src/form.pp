{$MODE OBJFPC}
unit form;
interface

uses
  ncurses, ctypes;

{$LINKLIB formw}
const
   libform = 'formw';

{$PACKRECORDS C}
{$INCLUDE eti.inc}


type
   //Pva_list = ^va_list;
   //va_list = char;
  Pva_list  = Pointer;
  FIELD_CELL = Pointer;
  Form_Options = Longint;
  Field_Options = Longint;


(* _PAGE  *)

  _PAGE = record
    pmin : Smallint;  { index of first field on page       }
    pmax : Smallint;  { index of last field on page      }
    smin : Smallint;  { index of top leftmost field on page    }
    smax : Smallint;  { index of bottom rightmost field on page  }
  end;



(*  FIELD  *)

  PPFIELD = ^PFIELD;
  PFIELDTYPE  = ^TFIELDTYPE;
  PFIELD  = ^TFIELD;
  PFORM  = ^TFORM;

  TFIELD = record
     status : Word;        { flags      }
     rows : Smallint;      { size in rows       }
     cols : Smallint;      { size in cols       }
     frow : Smallint;      { first row      }
     fcol : Smallint;      { first col      }
     drows : Longint;      { dynamic rows       }
     dcols : Longint;      { dynamic cols       }
     maxgrow : Longint;    { maximum field growth     }
     nrow : Longint;       { off-screen rows    }
     nbuf : Smallint;      { additional buffers     }
     just : Smallint;      { justification    }
     page : Smallint;      { page on form       }
     index : Smallint;     { into form -> field     }
     pad : Longint;        { pad character    }
     fore : chtype;        { foreground attribute     }
     back : chtype;        { background attribute     }
     opts : Field_Options; { options      }
     snext : PFIELD;       { sorted order pointer     }
     sprev : PFIELD;       { sorted order pointer     }
     link : PFIELD;        { linked field chain     }
     form : PFORM;         { containing form    }
     _type : PFIELDTYPE;   { field type       }
     arg : Pointer;        { argument for type    }
     buf : ^FIELD_CELL;    { field buffers    }
     usrptr : Pointer;     { user pointer       }
  end;
  //fieldnode = TFIELD;
{
  The wide-character configuration requires extra information.  Because
  there are existing applications that manipulate the members of FIELD
  directly, we cannot make the struct opaque.  Offsets of members up to
  this point are the same in the narrow- and wide-character configuration.
  But note that the type of buf depends on the configuration, and is made
  opaque for that reason.
}



(*  FIELDTYPE  *)

  TFieldCheck = function (_para1:PFIELD; _para2:Pointer):Bool; cdecl;
  TCharCheck = function (_para1:Longint; _para2:Pointer):Bool; cdecl;
  TMakearg = function (_para1:Pva_list):Pointer; cdecl;
  TCopy_arg = function (_para1:Pointer):Pointer; cdecl;
  TFree_arg = procedure (_para1:Pointer); cdecl;

  TFIELDTYPE = record
    status : Word;                                      { flags     }
    ref : clong;                                        { reference count  }
    left : PFIELDTYPE;                                  { ptr to operand for |  }
    right : PFIELDTYPE;                                 { ptr to operand for |  }
    //makearg : function (_para1:Pva_list):Pointer;cdecl;
    makearg : TMakearg;                                 { make fieldtype arg  }
    //copyarg : function (_para1:Pointer):Pointer;
    copyarg : TCopy_arg;                                { copy fieldtype arg   }
    //freearg : procedure (_para1:Pointer);
    freearg : TFree_arg;                                { field validation  }
    //fcheck : function (_para1:PFIELD; _para2:Pointer):bool;
    fcheck : TFieldCheck;                               { free fieldtype arg  }
    //ccheck : function (_para1:Longint; _para2:Pointer):bool;
    ccheck : TCharCheck;                                { character validation  }
    //next : function (_para1:PFIELD; _para2:Pointer):bool;
    next : TFieldCheck;                                 { enumerate next value  }
    //prev : function (_para1:PFIELD;  _para2:Pointer):bool;
    prev : TFieldCheck;                                 { enumerate prev value  }
  end;
  //typenode = FIELDTYPE;


(* FORM  *)

  Form_Hook = procedure (_para1:PFORM); cdecl;
  TFORM = record
    status : Word;          { flags       }
    rows : Smallint;        { size in rows       }
    cols : Smallint;        { size in cols       }
    currow : Longint;       { current row in field window  }
    curcol : Longint;       { current col in field window  }
    toprow : Longint;       { in scrollable field window   }
    begincol : Longint;     { in horiz. scrollable field   }
    maxfield : Smallint;    { number of fields     }
    maxpage : Smallint;     { number of pages     }
    curpage : Smallint;     { index into page     }
    opts : Form_Options;    { options      }
    win : PWINDOW;          { window       }
    sub : PWINDOW;          { subwindow      }
    w : PWINDOW;            { window for current field   }
    field : PPFIELD;        { field [maxfield]     }
    current : PFIELD;       { current field     }
    page : ^_PAGE;          { page [maxpage]     }
    usrptr : Pointer;       { user pointer      }
    //forminit : procedure (_para1:PFORM); cdecl;
    forminit : Form_Hook;
    //formterm : procedure (_para1:PFORM); cdecl;
    formterm : Form_Hook;
    //fieldinit : procedure (_para1:PFORM); cdecl;
    fieldinit : Form_Hook;
    //fieldterm : procedure (_para1:PFORM); cdecl;
    fieldterm : Form_Hook;
  end;
  //formnode = TFORM;



(* field justification *)
const
  NO_JUSTIFICATION = 0;
  JUSTIFY_LEFT = 1;
  JUSTIFY_CENTER = 2;
  JUSTIFY_RIGHT = 3;


(* field options *)
  O_VISIBLE = $0001;
  O_ACTIVE = $0002;
  O_PUBLIC = $0004;
  O_EDIT = $0008;
  O_WRAP = $0010;
  O_BLANK = $0020;
  O_AUTOSKIP = $0040;
  O_NULLOK = $0080;
  O_PASSOK = $0100;
  O_STATIC = $0200;
  O_NL_OVERLOAD = $0001;
  O_BS_OVERLOAD = $0002;


(* form driver commands *)
   REQ_NEXT_PAGE = KEY_MAX + 1;     { move to next page    }
   REQ_PREV_PAGE = KEY_MAX + 2;     { move to previous page  }
   REQ_FIRST_PAGE = KEY_MAX + 3;    { move to first page    }
   REQ_LAST_PAGE = KEY_MAX + 4;     { move to last page    }
   REQ_NEXT_FIELD = KEY_MAX + 5;    { move to next field    }
   REQ_PREV_FIELD = KEY_MAX + 6;    { move to previous field  }
   REQ_FIRST_FIELD = KEY_MAX + 7;   { move to first field    }
   REQ_LAST_FIELD = KEY_MAX + 8;    { move to last field    }
   REQ_SNEXT_FIELD = KEY_MAX + 9;   { move to sorted next field  }
   REQ_SPREV_FIELD = KEY_MAX + 10;  { move to sorted prev field   }
   REQ_SFIRST_FIELD = KEY_MAX + 11; { move to sorted first field   }
   REQ_SLAST_FIELD = KEY_MAX + 12;  { move to sorted last field   }
   REQ_LEFT_FIELD = KEY_MAX + 13;   { move to left to field  }
   REQ_RIGHT_FIELD = KEY_MAX + 14;  { move to right to field  }
   REQ_UP_FIELD = KEY_MAX + 15;     { move to up to field    }
   REQ_DOWN_FIELD = KEY_MAX + 16;   { move to down to field  }
   REQ_NEXT_CHAR = KEY_MAX + 17;    { move to next char in field  }
   REQ_PREV_CHAR = KEY_MAX + 18;    { move to prev char in field  }
   REQ_NEXT_LINE = KEY_MAX + 19;    { move to next line in field  }
   REQ_PREV_LINE = KEY_MAX + 20;    { move to prev line in field  }
   REQ_NEXT_WORD = KEY_MAX + 21;    { move to next word in field  }
   REQ_PREV_WORD = KEY_MAX + 22;    { move to prev word in field  }
   REQ_BEG_FIELD = KEY_MAX + 23;    { move to first char in field   }
   REQ_END_FIELD = KEY_MAX + 24;    { move after last char in fld   }
   REQ_BEG_LINE = KEY_MAX + 25;     { move to beginning of line  }
   REQ_END_LINE = KEY_MAX + 26;     { move after last char in line   }
   REQ_LEFT_CHAR = KEY_MAX + 27;    { move left in field    }
   REQ_RIGHT_CHAR = KEY_MAX + 28;   { move right in field    }
   REQ_UP_CHAR = KEY_MAX + 29;      { move up in field    }
   REQ_DOWN_CHAR = KEY_MAX + 30;    { move down in field    }
   REQ_NEW_LINE = KEY_MAX + 31;     { insert/overlay new line  }
   REQ_INS_CHAR = KEY_MAX + 32;     { insert blank char at cursor  }
   REQ_INS_LINE = KEY_MAX + 33;     { insert blank line at cursor  }
   REQ_DEL_CHAR = KEY_MAX + 34;     { delete char at cursor  }
   REQ_DEL_PREV = KEY_MAX + 35;     { delete char before cursor  }
   REQ_DEL_LINE = KEY_MAX + 36;     { delete line at cursor  }
   REQ_DEL_WORD = KEY_MAX + 37;     { delete word at cursor  }
   REQ_CLR_EOL = KEY_MAX + 38;      { clear to end of line    }
   REQ_CLR_EOF = KEY_MAX + 39;      { clear to end of field   }
   REQ_CLR_FIELD = KEY_MAX + 40;    { clear entire field    }
   REQ_OVL_MODE = KEY_MAX + 41;     { begin overlay mode     }
   REQ_INS_MODE = KEY_MAX + 42;     { begin insert mode    }
   REQ_SCR_FLINE = KEY_MAX + 43;    { scroll field forward a line   }
   REQ_SCR_BLINE = KEY_MAX + 44;    { scroll field backward a line  }
   REQ_SCR_FPAGE = KEY_MAX + 45;    { scroll field forward a page   }
   REQ_SCR_BPAGE = KEY_MAX + 46;    { scroll field backward a page  }
   REQ_SCR_FHPAGE = KEY_MAX + 47;   { scroll field forward   half page  }
   REQ_SCR_BHPAGE = KEY_MAX + 48;   { scroll field backward half page  }
   REQ_SCR_FCHAR = KEY_MAX + 49;    { horizontal scroll char  }
   REQ_SCR_BCHAR = KEY_MAX + 50;    { horizontal scroll char  }
   REQ_SCR_HFLINE = KEY_MAX + 51;   { horizontal scroll line   }
   REQ_SCR_HBLINE = KEY_MAX + 52;   { horizontal scroll line   }
   REQ_SCR_HFHALF = KEY_MAX + 53;   { horizontal scroll half line  }
   REQ_SCR_HBHALF = KEY_MAX + 54;   { horizontal scroll half line  }
   REQ_VALIDATION = KEY_MAX + 55;   { validate field     }
   REQ_NEXT_CHOICE = KEY_MAX + 56;  { display next field choice   }
   REQ_PREV_CHOICE = KEY_MAX + 57;  { display prev field choice   }
   MIN_FORM_COMMAND = KEY_MAX + 1;  { used by form_driver     }
   MAX_FORM_COMMAND = KEY_MAX + 57; { used by form_driver    }


(* standard field types *)

var
{$ifndef darwin}
  TYPE_ALPHA : PFIELDTYPE;cvar;external;
  TYPE_ALNUM : PFIELDTYPE;cvar;external;
  TYPE_ENUM : PFIELDTYPE;cvar;external;
  TYPE_INTEGER : PFIELDTYPE;cvar;external;
  TYPE_NUMERIC : PFIELDTYPE;cvar;external;
  TYPE_REGEXP : PFIELDTYPE;cvar;external;
{$else darwin}
  TYPE_ALPHA : PFIELDTYPE external libform name 'TYPE_ALPHA';
  TYPE_ALNUM : PFIELDTYPE external libform name 'TYPE_ALNUM';
  TYPE_ENUM : PFIELDTYPE external libform name 'TYPE_ENUM';
  TYPE_INTEGER : PFIELDTYPE external libform name 'TYPE_INTEGER';
  TYPE_NUMERIC : PFIELDTYPE external libform name 'TYPE_NUMERIC';
  TYPE_REGEXP : PFIELDTYPE external libform name 'TYPE_REGEXP';
{$endif darwin}
(***********************************
	*  built-in additional field types *
	*  They are not defined in SVr4    *
	***********************************)

{$ifndef darwin}
  TYPE_IPV4 : PFIELDTYPE;cvar;external; { Internet IP Version 4 address  }
{$else darwin}
  TYPE_IPV4 : PFIELDTYPE external libform name 'TYPE_IPV4';
{$endif darwin}

(* Default objects  *)
{$ifndef darwin}
  _nc_Default_Form : PFORM;cvar;external;
  _nc_Default_Field : PFIELD;cvar;external;
{$else darwin}
  _nc_Default_Form : PFORM external libform name '_nc_Default_Form';
  _nc_Default_Field : PFIELD external libform name '_nc_Default_Field';
{$endif darwin}

(* FIELDTYPE routines *)
(* Const before declarator ignored *)
(* Const before type ignored *)
{
function new_fieldtype(field_check:function (_para1:PFIELD;_para2:Pointer):bool; char_check:function (_para1:Longint;_para2:Pointer):bool):PFIELDTYPE;cdecl;external;
}

function new_fieldtype(field_check: TFieldCheck; char_check:TCharCheck):PFIELDTYPE; cdecl;external libform;

function link_fieldtype(_para1:PFIELDTYPE; _para2:PFIELDTYPE):PFIELDTYPE; cdecl;external libform;
function free_fieldtype(_para1:PFIELDTYPE):Longint; cdecl;external libform;

{TMakearg   TCopy_arg TFree_arg
extern int set_fieldtype_arg (FIELDTYPE *,
      void * (* const make_arg)(va_list *),
      void * (* const copy_arg)(const void *),
      void (* const free_arg)(void *));

function set_fieldtype_arg(_para1:PFIELDTYPE; make_arg:function (_para1:Pva_list):Pointer; copy_arg:function (_para1:Pointer):Pointer; free_arg:procedure (_para1:Pointer)):Longint;cdecl;external;
}
function set_fieldtype_arg(fieldtype: PFIELDTYPE; make_arg: TMakearg; copy_arg: TCopy_arg; free_arg: TFree_arg): Longint;cdecl;external libform;

{
extern int set_fieldtype_choice (FIELDTYPE *,
      bool (* const next_choice)(FIELD *,const void *),
            bool (* const prev_choice)(FIELD *,const void *));

(* Const before declarator ignored *)
(* Const before type ignored *)
(* Const before declarator ignored *)
(* Const before type ignored *)
function set_fieldtype_choice(_para1:PFIELDTYPE; next_choice:function (_para1:PFIELD; _para2:Pointer):bool; prev_choice:function (_para1:PFIELD; _para2:Pointer):bool):Longint;cdecl;external;
}

function set_fieldtype_choice(_para1:PFIELDTYPE; next_choice:TFieldCheck; prev_choice:TFieldCheck):Longint; cdecl;external libform;

(* FIELD routines *)

function new_field(_pa1,_pa2,_pa3,_pa4,_pa5,_pa6:Longint):PFIELD; cdecl;external libform;
function dup_field(_para1:PFIELD; _para2:Longint; _para3:Longint):PFIELD; cdecl;external libform;
function link_field(_para1:PFIELD; _para2:Longint; _para3:Longint):PFIELD; cdecl;external libform;
function free_field(_para1:PFIELD):Longint; cdecl;external libform;
function field_info(_para1:PFIELD; _para2:Pcint; _para3:Pcint; _para4:Pcint; _para5:Pcint; 
           _para6:Pcint; _para7:Pcint):Longint; cdecl;external libform;
function dynamic_field_info(_para1:PFIELD; _para2:Pcint; _para3:Pcint; _para4:Pcint):Longint; cdecl;external libform;
function set_max_field(_para1:PFIELD; _para2:Longint):Longint; cdecl;external libform;
function move_field(_para1:PFIELD; _para2:Longint; _para3:Longint):Longint; cdecl;external libform;
//function set_field_type(_para1:PFIELD; _para2:PFIELDTYPE; args:array of const):Longint; cdecl;external libform;
function set_field_type(_field:PFIELD; _type:PFIELDTYPE):Longint;cdecl;varargs;external libform;

function set_new_page(_para1:PFIELD; _para2:Bool):Longint; cdecl;external libform;
function set_field_just(_para1:PFIELD; _para2:Longint):Longint; cdecl;external libform;
function field_just(_para1:PFIELD):Longint; cdecl;external libform;
function set_field_fore(_para1:PFIELD; _para2:chtype):Longint; cdecl;external libform;
function set_field_back(_para1:PFIELD; _para2:chtype):Longint; cdecl;external libform;
function set_field_pad(_para1:PFIELD; _para2:Longint):Longint; cdecl;external libform;
function field_pad(_para1:PFIELD):Longint; cdecl;external libform;
function set_field_buffer(_para1:PFIELD; _para2:Longint;_para3:PChar):Longint; cdecl;external libform;
function set_field_status(_para1:PFIELD; _para2:Bool):Longint; cdecl;external libform;
function set_field_userptr(_para1:PFIELD; _para2:Pointer):Longint; cdecl;external libform;
function set_field_opts(_para1:PFIELD; _para2:Field_Options):Longint; cdecl;external libform;
function field_opts_on(_para1:PFIELD; _para2:Field_Options):Longint; cdecl;external libform;
function field_opts_off(_para1:PFIELD; _para2:Field_Options):Longint; cdecl;external libform;
function field_fore(_para1:PFIELD):chtype; cdecl;external libform;
function field_back(_para1:PFIELD):chtype; cdecl;external libform;
function new_page(_para1:PFIELD):Bool; cdecl;external libform;
function field_status(_para1:PFIELD):Bool; cdecl;external libform;
function field_arg(_para1:PFIELD):Pointer; cdecl;external libform;
function field_userptr(_para1:PFIELD):Pointer; cdecl;external libform;
function field_type(_para1:PFIELD):PFIELDTYPE; cdecl;external libform;
function field_buffer(_para1:PFIELD; _para2:Longint):PChar; cdecl;external libform;
function field_opts(_para1:PFIELD):Field_Options; cdecl;external libform;

(*  FORM routines  *)
function new_form(_para1:PPFIELD):PFORM; cdecl;external libform;
function form_fields(_para1:PFORM):PPFIELD; cdecl;external libform;
function current_field(_para1:PFORM):PFIELD; cdecl;external libform;
function form_win(_para1:PFORM):PWINDOW; cdecl;external libform;
function form_sub(_para1:PFORM):PWINDOW; cdecl;external libform;
function form_init(_para1:PFORM):Form_Hook; cdecl;external libform;
function form_term(_para1:PFORM):Form_Hook; cdecl;external libform;
function field_init(_para1:PFORM):Form_Hook; cdecl;external libform;
function field_term(_para1:PFORM):Form_Hook; cdecl;external libform;
function free_form(_para1:PFORM):Longint; cdecl;external libform;
function set_form_fields(_para1:PFORM; _para2:PPFIELD):Longint; cdecl;external libform;
function field_count(_para1:PFORM):Longint; cdecl;external libform;
function set_form_win(_para1:PFORM; _para2:PWINDOW):Longint; cdecl;external libform;
function set_form_sub(_para1:PFORM; _para2:PWINDOW):Longint; cdecl;external libform;
function set_current_field(_para1:PFORM; _para2:PFIELD):Longint; cdecl;external libform;
function field_index(_para1:PFIELD):Longint; cdecl;external libform;
function set_form_page(_para1:PFORM; _para2:Longint):Longint; cdecl;external libform;
function form_page(_para1:PFORM):Longint; cdecl;external libform;
function scale_form(_para1:PFORM; _para2:Pcint; _para3:Pcint):Longint; cdecl;external libform;
function set_form_init(_para1:PFORM; _para2:Form_Hook):Longint; cdecl;external libform;
function set_form_term(_para1:PFORM; _para2:Form_Hook):Longint; cdecl;external libform;
function set_field_init(_para1:PFORM; _para2:Form_Hook):Longint; cdecl;external libform;
function set_field_term(_para1:PFORM; _para2:Form_Hook):Longint; cdecl;external libform;
function post_form(_para1:PFORM):Longint; cdecl;external libform;
function unpost_form(_para1:PFORM):Longint; cdecl;external libform;
function pos_form_cursor(_para1:PFORM):Longint; cdecl;external libform;
function form_driver(_para1:PFORM; _para2:Longint):Longint; cdecl;external libform;
function set_form_userptr(_para1:PFORM; _para2:Pointer):Longint; cdecl;external libform;
function set_form_opts(_para1:PFORM; _para2:Form_Options):Longint; cdecl;external libform;
function form_opts_on(_para1:PFORM; _para2:Form_Options):Longint; cdecl;external libform;
function form_opts_off(_para1:PFORM; _para2:Form_Options):Longint; cdecl;external libform;
function form_request_by_name(_para1:PChar):Longint; cdecl;external libform;
function form_request_name(_para1:Longint):PChar; cdecl;external libform;
function form_userptr(_para1:PFORM):Pointer; cdecl;external libform;
function form_opts(_para1:PFORM):Form_Options; cdecl;external libform;
function data_ahead(_para1:PFORM):Bool; cdecl;external libform;
function data_behind(_para1:PFORM):Bool; cdecl;external libform;

implementation


end.
