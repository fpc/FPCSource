{ Multiline labels. }
program multilabel;

uses xforms;

var
form : PFL_FORM;
readyobj : PFL_OBJECT;

Procedure create_form;

var obj : PFL_OBJECT;

begin
  form := fl_bgn_form(FL_NO_BOX,400,470);
  obj := fl_add_box(FL_UP_BOX,0,0,400,470,'');
    fl_set_object_color(obj,FL_SLATEBLUE,FL_COL1);
  obj := fl_add_text(FL_NORMAL_TEXT,140,40,120,120,'This is'#10'a multi-line'#10'labelT');
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_lalign(obj,FL_ALIGN_TOP);
  obj := fl_add_text(FL_NORMAL_TEXT,140,160,120,120,'This is'#10'a multi-line'#10'labelC');
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_color(obj,FL_PALEGREEN,FL_COL1);
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
  obj := fl_add_button(FL_NORMAL_BUTTON,280,400,100,50,'I am sure'#10'that I am'#10'Ready');
  readyobj :=obj;
    fl_set_object_lsize(obj,FL_SMALL_SIZE);
  obj := fl_add_text(FL_NORMAL_TEXT,260,160,120,120,'This is'#10'a multi-line'#10'labelR');
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_lalign(obj,FL_ALIGN_RIGHT);
  obj := fl_add_text(FL_NORMAL_TEXT,140,280,120,120,'This is'#10'a multi-line'#10'labelB');
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_lalign(obj,FL_ALIGN_BOTTOM);
  obj := fl_add_text(FL_NORMAL_TEXT,20,160,120,120,'This is'#10'a multi-line'#10'label');
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
  fl_end_form();
end;

var
  obj : PFL_OBJECT;

begin

  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  create_form;
  fl_show_form(form,FL_PLACE_CENTER,FL_NOBORDER,'Labels');
  repeat
    obj := fl_do_forms;
  until (obj = readyobj);
  fl_hide_form(form);
end.
