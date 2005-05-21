{ This demo shows the use of special symbol labels }
program arrowbutton;

uses xforms;

const border : longint = FL_TRANSIENT;

var
  form : PFL_FORM;
  obj,but : PFL_OBJECT;

begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);

  form := fl_bgn_form(FL_UP_BOX,400,400);
    obj := fl_add_button(FL_NORMAL_BUTTON, 50, 250,100,100,'@1');
    fl_set_object_lcol(obj,FL_BLUE);
    obj := fl_add_button(FL_NORMAL_BUTTON,150, 250,100,100,'@2');
    fl_set_object_lcol(obj,FL_BLUE);
    obj := fl_add_button(FL_NORMAL_BUTTON,250, 250,100,100,'@3');
    fl_set_object_lcol(obj,FL_BLUE);
    obj := fl_add_button(FL_NORMAL_BUTTON, 50,150,100,100,'@4');
    fl_set_object_lcol(obj,FL_BLUE);
    but := fl_add_button(FL_NORMAL_BUTTON,150,150,100,100,'@square');
    fl_set_object_lcol(but,FL_GREEN);
    fl_set_object_color(but,FL_MAGENTA,FL_RED);
    obj := fl_add_button(FL_NORMAL_BUTTON,250,150,100,100,'@6');
    fl_set_object_lcol(obj,FL_BLUE);
    obj := fl_add_button(FL_NORMAL_BUTTON, 50, 50,100,100,'@7');
    fl_set_object_lcol(obj,FL_BLUE);
    obj := fl_add_button(FL_NORMAL_BUTTON,150, 50,100,100,'@8');
    fl_set_object_lcol(obj,FL_BLUE);
    obj := fl_add_button(FL_NORMAL_BUTTON,250, 50,100,100,'@9');
    fl_set_object_lcol(obj,FL_BLUE);
  fl_end_form();

  fl_show_form(form,FL_PLACE_ASPECT,border,'Buttons');
  while (fl_do_forms() <> but) do;
  fl_hide_form(form);
end.
