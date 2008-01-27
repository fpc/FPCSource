{ This demo shows the different routines on browsers }
program browserop;

uses xforms;

var
form : PFL_FORM;
browserobj, inputobj, exitobj : PFL_OBJECT;

procedure addit(ob : PFL_OBJECT; arg : longint);cdecl;
begin
  { append and show the last line. Don't use this if you just want
   * to add some lines. use fl_add_browser_line
   }
  fl_addto_browser(browserobj,fl_get_input(inputobj));
end;

procedure insertit(ob : PFL_OBJECT ; arg : longint);cdecl;
var
  n :  integer;
begin
  n:=fl_get_browser(browserobj);
  if (n=0) then exit;
  fl_insert_browser_line(browserobj,n,fl_get_input(inputobj));
end;

procedure replaceit(obj : PFL_OBJECT; arg : longint);cdecl;
var
  n :  integer;
begin
  n:=fl_get_browser(browserobj);
  if (n=0) then exit;
  fl_replace_browser_line(browserobj,n,fl_get_input(inputobj));
end;


procedure deleteit(ob : PFL_OBJECT; arg : Longint);cdecl;
var
  n :  integer;
begin
  n:=fl_get_browser(browserobj);
  if (n=0) then exit;
  fl_delete_browser_line(browserobj,n);
end;

procedure clearit(ob : PFL_OBJECT; arg : longint);cdecl;
begin
  fl_clear_browser(browserobj);
end;


procedure create_form;
var
  obj : PFL_OBJECT;

begin
  form := fl_bgn_form(FL_UP_BOX,390,420);
  browserobj := fl_add_browser(FL_HOLD_BROWSER,20,20,210,330,'');
    fl_set_object_dblbuffer(browserobj, 1);
  obj:= fl_add_input(FL_NORMAL_INPUT,20,370,210,30,'');
  inputobj := obj ;
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@addit),0);
  obj := fl_add_button(FL_NORMAL_BUTTON,250,20,120,30,'Add');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@addit),0);
  obj := fl_add_button(FL_NORMAL_BUTTON,250,60,120,30,'Insert');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@insertit),0);
  obj := fl_add_button(FL_NORMAL_BUTTON,250,100,120,30,'Replace');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@replaceit),0);
  obj := fl_add_button(FL_NORMAL_BUTTON,250,160,120,30,'Delete');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@deleteit),0);
  obj := fl_add_button(FL_NORMAL_BUTTON,250,200,120,30,'Clear');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@clearit),0);
  exitobj := fl_add_button(FL_NORMAL_BUTTON,250,370,120,30,'Exit');
  fl_end_form();
end;

{---------------------------------------}

var obj : PFL_OBJECT;

begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  create_form;
  fl_show_form(form,FL_PLACE_CENTER,FL_TRANSIENT,'Browser Op');
  repeat
    obj := fl_do_forms
  until obj = exitobj;
  fl_hide_form(form);
end.
