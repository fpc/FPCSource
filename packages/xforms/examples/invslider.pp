program invslider;

uses xforms;

type
TFD_inv = record
        inv : PFL_FORM;
        sl : array[0..2] of PFL_OBJECT;
        done : PFL_OBJECT;
        vdata : pointer;
        ldata : longint;
        end;
PFD_inv=^TFD_inv;

var ui : PFD_INV;

procedure invert_it(ob : PFL_OBJECT; data : longint);cdecl;

begin
   if (fl_get_button(ob)<>0) then
     begin
      fl_set_slider_bounds(ui^.sl[0], 1.0, 0.0);
      fl_set_slider_bounds(ui^.sl[1], 1.0, 0.0);
      fl_set_slider_bounds(ui^.sl[2], 1.0, 0.0);
     end
   else
     begin
      fl_set_slider_bounds(ui^.sl[0], 0.0, 1.0);
      fl_set_slider_bounds(ui^.sl[1], 0.0, 1.0);
      fl_set_slider_bounds(ui^.sl[2], 0.0, 1.0);
    end;
end;

function create_form_inv : PFD_inv;
var
  obj : PFL_OBJECT ;
  fdui : PFD_inv;

begin
  new(fdui);
  fdui^.inv := fl_bgn_form(FL_NO_BOX, 245, 280);
  obj := fl_add_box(FL_UP_BOX,0,0,245,280,'');
  obj := fl_add_valslider(FL_VERT_SLIDER,20,30,35,230,'');
  fdui^.sl[0] := obj;
  obj := fl_add_valslider(FL_VERT_FILL_SLIDER,65,30,35,230,'');
  fdui^.sl[1] := obj;
  obj := fl_add_valslider(FL_VERT_NICE_SLIDER,115,30,35,230,'');
  fdui^.sl[2] := obj;
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
  obj := fl_add_button(FL_RETURN_BUTTON,160,235,75,30,'Exit');
  fdui^.done := obj ;
  obj := fl_add_checkbutton(FL_PUSH_BUTTON,165,30,75,35,'Invert');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@invert_it),0);
  fl_end_form;

  create_form_inv:=fdui;
end;


begin
   fl_initialize(@argc, argv, 'FormDemo', nil, 0);
   ui := create_form_inv;

   fl_show_form(ui^.inv,FL_PLACE_CENTER,FL_TRANSIENT,'inv');
   while fl_do_forms <> ui^.done do;
end.
