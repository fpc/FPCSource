{ Form definition file generated with fdesign. }
program flclock;

uses xforms;

var
 fclock : PFL_FORM;

procedure exit_cb (ob : PFL_OBJECT; q : longint);cdecl;
begin
   halt(0);
end;

procedure create_form_clock;
var
  obj : PFL_OBJECT;

begin
  if (fclock<>nil) then exit;

  fclock := fl_bgn_form(FL_NO_BOX,500,350);
  obj := fl_add_box(FL_UP_BOX,0,0,500,350,'');

  obj := fl_add_clock(FL_DIGITAL_CLOCK,190,20,140,30,'');
  fl_set_object_boxtype(obj,FL_ROUNDED_BOX);
  fl_set_object_color(obj,FL_COL1,FL_BLACK);
  fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
  fl_set_object_lstyle(obj,FL_BOLD_STYLE);

  obj := fl_add_clock(FL_ANALOG_CLOCK,30,70,220,200,'');
  fl_set_object_boxtype(obj,FL_UP_BOX);

  obj := fl_add_clock(FL_ANALOG_CLOCK,260,70,220,200,'');
  fl_set_object_boxtype(obj,FL_OVAL_BOX);
  obj := fl_add_button(FL_NORMAL_BUTTON,380,300,100,30,'Exit');
  fl_set_object_callback(obj, PFL_CALLBACKPTR(@exit_cb), 0);
  fl_end_form;
end;

begin
    fl_initialize(@argc, argv, 'FormDemo', nil, 0);

    create_form_clock();
    fl_set_form_dblbuffer(fclock, 1);
    fl_show_form(fclock, FL_PLACE_CENTER,FL_TRANSIENT,'clocks');
    fl_do_forms();
end.
