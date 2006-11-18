{ This demo shows the use of a browser and a file selector.
}
program fbrowse;

uses xforms,strings;

var
form : PFL_FORM;
br : PFL_OBJECT;

procedure load_file(ob : PFL_OBJECT; arg : longint);cdecl;
var
  fname : pchar;

begin
  fname := fl_show_fselector ('File To Load','','*.*','');
  if (fname = nil) then exit;
  if (fl_load_browser(br,fname)<>0) then
     fl_add_browser_line(br,'NO SUCH FILE!');
end;

procedure set_size(ob : PFL_OBJECT; arg : longint);cdecl;
begin
   fl_set_browser_fontsize(br,arg);
end;

procedure exit_program(ob : PFL_OBJECT; data : longint);cdecl;
begin
   halt(0);
end;

procedure create_form;
var
  obj : PFL_OBJECT;
  x,dx : TFL_Coord;

begin
  x  := 20;
  dx := 85;

  form := fl_bgn_form(FL_NO_BOX,590,610);
  obj := fl_add_box(FL_UP_BOX,0,0,590,610,'');
  obj := fl_add_browser(FL_NORMAL_BROWSER,20,20,550,530,'');
  br := obj ;
  obj := fl_add_button(FL_NORMAL_BUTTON,x,560,dx,30,'Load');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@load_file),0);
    x :=x+ dx + 10;
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,x,560,dx,30,'Tiny');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_size),FL_TINY_SIZE);
    x := x+dx;
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,x ,560,dx,30,'Small');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_size),FL_SMALL_SIZE);
{ Compiler crashes on the next one. Probably the comparing of the
  constants..
    if (longint(FL_SMALL_SIZE)=longint(FL_BROWSER_FONTSIZE)) then
    fl_set_button(obj, 1);
}    x := x+dx;
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,x ,560,dx,30,'Normal');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_size),FL_NORMAL_SIZE);
{ idem here.
   if FL_NORMAL_SIZE = FL_BROWSER_FONTSIZE then
    fl_set_button(obj,1 );
}    x := x+dx;
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,x ,560,dx,30,'Large');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_size),FL_LARGE_SIZE);

  obj := fl_add_button(FL_NORMAL_BUTTON,470,560,100,30,'Exit');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@exit_program), 0);
  fl_end_form();
end;

begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  create_form();

  fl_clear_browser(br);
  fl_add_browser_line(br,'LOAD A FILE.');
  fl_set_browser_fontstyle(br,FL_FIXED_STYLE);

  fl_show_form(form,FL_PLACE_FREE,FL_FULLBORDER,'Browser');
  fl_do_forms();
  fl_hide_form(form);
  fl_free_form(form);
end.
