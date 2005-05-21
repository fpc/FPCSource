Program pushme;

uses xforms;

var simpleform : PFL_FORM;
{    formname,buttonname : string;
}
begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  simpleform := fl_bgn_form(FL_UP_BOX,230,160);
   fl_add_button(FL_NORMAL_BUTTON,40,50,150,60,'Pushme');
  fl_end_form;
  fl_show_form(simpleform, FL_PLACE_MOUSE, FL_NOBORDER, 'Push me');
  fl_do_forms;
  fl_hide_form(simpleform);
end.
