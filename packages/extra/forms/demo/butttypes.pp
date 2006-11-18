program butttypes;

uses strings,xforms;

type TFD_form0 = record
        form0 : PFL_FORM;
        vdata : pointer;
        ldata : longint;
        br : PFL_OBJECT;
end;
PFD_form0 = ^TFD_form0;

{ callbacks for form form0 }
procedure button_cb(ob : PFL_OBJECT; data : longint);cdecl;
var
   ui : PFD_form0;
  buf : string [128];

begin
   ui:=ob^.form^.fdui;
   if (ob^.thetype = FL_HIDDEN_BUTTON ) then
     begin
      if (fl_show_question('Want to Quit ?', 1) = 1) then
        halt(0);
     end
   else
     begin
      str (fl_get_button(ob),buf);
      buf:=strpas(ob^ .thelabel)+'callback called from '+ buf+#0;
      fl_addto_browser(ui^.br, @buf[1]);
    end;
end;

{ Form definition file generated with fdesign. }


function create_form_form0 : PFD_form0;
var
  obj : PFL_OBJECT;
  fdui : PFD_form0;

begin
  new(fdui);
  fdui^.form0 := fl_bgn_form(FL_NO_BOX, 360, 300);
  obj := fl_add_box(FL_UP_BOX,0,0,360,300,'');
  obj := fl_add_button(FL_HIDDEN_BUTTON,0,0,360,300,'');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@button_cb),0);
  obj := fl_add_button(FL_INOUT_BUTTON,20,168,105,30,'InOutButton');
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@button_cb),0);
  obj := fl_add_button(FL_NORMAL_BUTTON,20,15,105,30,'NormalButton');
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@button_cb),0);
  obj := fl_add_button(FL_PUSH_BUTTON,20,53,105,30,'PushButton');
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@button_cb),0);
  obj := fl_add_button(FL_TOUCH_BUTTON,20,130,105,30,'TouchButton');
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@button_cb),0);
  obj := fl_add_button(FL_MENU_BUTTON,20,206,105,30,'MenuButton');
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@button_cb),0);
  obj := fl_add_button(FL_RETURN_BUTTON,20,244,105,30,'ReturnButton');
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@button_cb),0);
  obj := fl_add_button(FL_RADIO_BUTTON,20,91,105,30,'RadioButton');
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@button_cb),0);
    obj := fl_add_browser(FL_NORMAL_BROWSER,135,15,210,260,'');
    fdui^.br :=obj;
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@button_cb),0);
  fl_end_form();

  fl_adjust_form_size(fdui^.form0);
  fdui^.form0^.fdui := fdui;

  create_form_form0:= fdui;
end;




var
   fd_form0 : PFD_form0;

begin
   fl_initialize(@argc, argv, '', nil, 0);
   fd_form0 := create_form_form0;

   { fill-in form initialization code }

   { show the first form }
   fl_show_form(fd_form0^.form0,FL_PLACE_CENTER,FL_FULLBORDER,'form0');
   fl_do_forms();
end.
