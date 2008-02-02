{ Demo showing activating and deactivating objects
}
program objin;

uses xforms;

var
form : PFL_FORM;

        button1,
        button2,
        button3,
        button4,
        group,
        firstbut : PFL_OBJECT;

procedure  exit_cb(obj : PFL_OBJECT; arg : longint); cdecl;
begin
 halt;
end;



Procedure setit(obj : PFL_OBJECT; val : longint);
begin
  if (val<>0) then
    begin
    fl_set_object_lcol(obj,FL_BLACK);
    fl_activate_object(obj);
    end
  else
    begin
    fl_set_object_lcol(obj,FL_INACTIVE);
    fl_deactivate_object(obj);
    end
end;

Procedure setit_cb(obj : PFL_OBJECT; val : longint); cdecl;

begin
  setit (obj,val)
end;

Procedure doit(b1,b2,b3,b4 : longint);

begin
  setit(button1,b1);
  setit(button2,b2);
  setit(button3,b3);
  setit(button4,b4);
end;

Procedure set_active(obj : PFL_OBJECT; arg : longint); cdecl;

begin
  case arg of
    0: doit(1,1,1,1);
    1: doit(0,0,0,0);
    2: doit(0,1,0,1);
    3: doit(1,0,1,0);
  end;
end;

Procedure create_form;

var obj : PFL_OBJECT;

begin
  form := fl_bgn_form(FL_NO_BOX,420,230);
  obj := fl_add_box(FL_UP_BOX,0,0,420,230,'');
    fl_set_object_color(obj,FL_SLATEBLUE,FL_COL1);
  obj := fl_add_button(FL_NORMAL_BUTTON,20,170,150,40,'Button 1');
  button1 := obj;
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_button_shortcut(obj, '1 ', 1);
  obj := fl_add_button(FL_NORMAL_BUTTON,20,120,150,40,'Button 2');
  button2 := obj;
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_button_shortcut(obj, '2 ', 1);
  obj := fl_add_button(FL_NORMAL_BUTTON,20,70,150,40,'Button 3');
  button3 := obj;
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_button_shortcut(obj, '3 ', 1);
  obj := fl_add_button(FL_NORMAL_BUTTON,20,20,150,40,'Button 4');
  button4 := obj;
    fl_set_button_shortcut(obj, '4 ', 1);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
  group := fl_bgn_group();
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,260,180,140,30,'All active');
  firstbut := obj;
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_active),0);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,260,150,140,30,'Non active');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_active),1);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,260,120,140,30,'Even active');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_active),2);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,260,90,140,30,'Odd active');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_active),3);
  fl_end_group();
  obj := fl_add_button(FL_NORMAL_BUTTON,270,20,130,30,'Quit');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@exit_cb),0);
  fl_end_form();
end;

begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  create_form;

  fl_set_button(firstbut,1);
  fl_show_form(form,FL_PLACE_CENTER,FL_NOBORDER,NiL);
  while (fl_do_forms()<>nil) do
    begin end;
end.
