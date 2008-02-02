{ This is an example of the use of counters.
}
program counter;

uses xforms;

var
form : PFL_FORM;
result : PFL_OBJECT;
co : array[0..2] of PFL_OBJECT;

procedure color_change(ob : PFL_OBJECT; data : longint);cdecl;

var r,g,b : longint;

begin
    r :=  round(fl_get_counter_value(co[0]));
    g :=  round(fl_get_counter_value(co[1]));
    b :=  round(fl_get_counter_value(co[2]));

    fl_mapcolor(FL_FREE_COL1,r,g,b);
    fl_redraw_object(result);
end;

procedure create_form_form;
var
  obj : PFL_OBJECT;

begin
  form := fl_bgn_form(FL_NO_BOX,480,200);
  obj := fl_add_box(FL_UP_BOX,0,0,480,200,'');
  obj := fl_add_box(FL_DOWN_BOX,310,20,150,160,'');
  result := obj ;
    fl_set_object_dblbuffer(result, 1);
  obj := fl_add_counter(FL_NORMAL_COUNTER,20,20,270,30,'');
  co[0] := obj;
    fl_set_object_color(obj,FL_INDIANRED,FL_RED);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@color_change),0);
  obj:= fl_add_counter(FL_NORMAL_COUNTER,20,60,270,30,'');
  co[1] := obj;
    fl_set_object_color(obj,FL_PALEGREEN,FL_GREEN);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@color_change),0);
  obj := fl_add_counter(FL_NORMAL_COUNTER,20,100,270,30,'');
  co[2] := obj ;
    fl_set_object_color(obj,FL_SLATEBLUE,FL_BLUE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@color_change),0);
  obj := fl_add_button(FL_NORMAL_BUTTON,100,150,110,30,'Exit');
  fl_end_form();
end;

var i : longint;

begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  create_form_form();
  fl_set_object_color(result,FL_FREE_COL1,FL_FREE_COL1);

  for i:=0 to 2 do
    begin
    fl_set_counter_bounds(co[i],0.0,255.0);
    fl_set_counter_step(co[i],1.0,10.0);
    fl_set_counter_precision(co[i],0);
    fl_set_counter_return(co[i],1);
    end;

  fl_call_object_callback(co[0]);

  fl_show_form(form,FL_PLACE_CENTER,FL_NOBORDER,'Counter');
  fl_do_forms();

end.
