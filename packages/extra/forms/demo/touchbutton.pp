{  This demo shows the use of a touch buttons. }
program touchbutton;

uses strings,xforms;

var form : PFL_FORM;
    valobj,obj : PFL_OBJECT;
    value : LongInt;


Procedure show_val(ob : PFL_OBJECT;delta : Longint);cdecl;

var     st : string[32];

begin
   value:=value+delta;
   str(value,st);
   st:=st+#0;
   fl_set_object_label(valobj, @st[1]);
end;


begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  form := fl_bgn_form(FL_UP_BOX,360,140);
    obj := fl_add_button(FL_TOUCH_BUTTON,50,30,40,30,'@<<');
     fl_set_object_boxtype(obj,FL_FRAME_BOX);
     fl_set_object_color(obj, FL_COL1, FL_INDIANRED);
     fl_set_object_callback(obj, PFL_CALLBACKPTR(@show_val),-5);
     fl_set_button_shortcut(obj,'1'#0, 0);
    obj := fl_add_button(FL_TOUCH_BUTTON,90,30,40,30,'@<');
     fl_set_object_boxtype(obj,FL_FRAME_BOX);
     fl_set_object_color(obj, FL_COL1, FL_INDIANRED);
     fl_set_object_callback(obj, PFL_CALLBACKPTR(@show_val),-1);
     fl_set_button_shortcut(obj,'2'#0, 0);
    obj := fl_add_box(FL_BORDER_BOX,130,30,100,30,'');
    valobj:=obj;
     fl_set_object_color(obj,FL_LEFT_BCOL,FL_LEFT_BCOL);
    obj := fl_add_button(FL_TOUCH_BUTTON,230,30,40,30,'@>');
     fl_set_object_boxtype(obj,FL_FRAME_BOX);
     fl_set_object_color(obj, FL_COL1, FL_INDIANRED);
     fl_set_object_callback(obj, PFL_CALLBACKPTR(@show_val),1);
     fl_set_button_shortcut(obj,'3'#0, 0);
    obj := fl_add_button(FL_TOUCH_BUTTON,270,30,40,30,'@>>');
     fl_set_object_boxtype(obj,FL_FRAME_BOX);
     fl_set_object_callback(obj, PFL_CALLBACKPTR(@show_val),5);
     fl_set_object_color(obj, FL_COL1, FL_INDIANRED);
     fl_set_button_shortcut(obj,'4'#0, 0);
    obj := fl_add_button(FL_NORMAL_BUTTON,220,90,100,30,'Exit');
  fl_end_form;
  fl_show_form(form,FL_PLACE_CENTER,FL_NOBORDER,'Touch Buttons');
  fl_do_forms();
end.
