program minput;

uses xforms;

procedure input_cb( ob : PFL_OBJECT; data : Longint);cdecl;

var
    x, y : Longint;
begin
    fl_get_input_cursorpos(ob, @x , @y);
    writeln ('x= ',x,' y= ',y);
end;


var
  form : PFL_FORM;
  but, obj : PFL_OBJECT;

begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  form := fl_bgn_form(FL_UP_BOX,400,450);
    obj := fl_add_input(FL_MULTILINE_INPUT,30,270,340,150,'');
    obj := fl_add_input(FL_MULTILINE_INPUT,30,90,340,150,'');
    fl_set_object_lsize(obj, FL_NORMAL_SIZE);
  but := fl_add_button(FL_NORMAL_BUTTON,160,30,80,30,'Exit');
  fl_end_form();
  fl_show_form(form,FL_PLACE_CENTERFREE,FL_FULLBORDER,'MultiLineInput');
  while (obj <> but) do obj:=fl_do_forms;
end.
