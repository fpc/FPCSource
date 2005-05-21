Program iconify;

Uses x,forms;

{$i crab.xpm}


function create_form_form : PFL_FORM;

var
  obj : PFL_OBJECT;
  form : PFL_FORM;

begin
  new(form);
  form := fl_bgn_form(FL_NO_BOX,151,111);
  obj := fl_add_pixmapbutton(FL_NORMAL_BUTTON,0,0,151,111,
                             'Iconify Me\nvia Window Manager');
  fl_set_object_lalign(obj, FL_ALIGN_BOTTOM or FL_ALIGN_INSIDE);
  fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  fl_set_pixmapbutton_data(obj, crab45);
  fl_end_form;
  create_form_form:=form;
end;

Var form0 : PFL_FORM;
    mask,p : TPixmap;
    H,W : word;

begin
   fl_initialize(@argc, argv, 'FormDemo', nil, 0);
   form0 := create_form_form;
   p := fl_read_pixmapfile(fl_root, 'crab.xpm', Pword(@w), pword(@h), PPixmap(@mask), nil, nil, 0);
   fl_set_form_icon(form0, p, mask);
   fl_show_form(form0,FL_PLACE_CENTER,FL_FULLBORDER, 'IconTest');
   fl_do_forms();
end.
