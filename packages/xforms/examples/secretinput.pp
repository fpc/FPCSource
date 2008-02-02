program secretinput;
{ Demo showing secret input fields }

uses xforms,strings;

var form : PFL_FORM;
   but, password1, password2, info, ret : PFL_OBJECT;
   str : string;

begin

  fl_initialize(@argc, argv, 'FormDemo', nil, 0);

  form := fl_bgn_form( FL_FLAT_BOX,400,300);
    password1 := fl_add_input(FL_SECRET_INPUT,140,40,160,40,'Password 1:');
    password2 := fl_add_input(FL_SECRET_INPUT,140,100,160,40,'Password 2:');
    info := fl_add_box(FL_SHADOW_BOX,20,160,360,40,'');
    but := fl_add_button( FL_NORMAL_BUTTON,280,240,100,40,'Quit');
  fl_end_form;

  fl_show_form(form, FL_PLACE_MOUSE,FL_NOBORDER,nil);
  repeat
    ret := fl_do_forms;
    str:='Password 1 is: '+strpas(fl_get_input(password1));
    str:=str+' , Password 2 is: '+strpas(fl_get_input(password2))+#0;
    fl_set_object_label(info,@str[1]);
  until (ret = but);
  fl_hide_form(form);
end.
