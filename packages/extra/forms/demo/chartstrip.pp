{ A demo of a moving chart }
program chartstrip;

uses xforms,xlib;

var
 func : longint;
 xx    : double;
 step : double;

 form : PFL_FORM;

 chartobj,sinobj,exitbut,stepobj : PFL_OBJECT;

procedure set_function(obj : PFL_OBJECT; arg : longint);cdecl;
begin
   func := arg;
   fl_clear_chart(chartobj);
   xx := 0.0;
end;

procedure set_step(obj : PFL_OBJECT; arg : longint);cdecl;

begin
    step := fl_get_slider_value(stepobj);
end;

{***********************************************}

procedure create_form_form;

var obj : PFL_OBJECT;

begin
  form := fl_bgn_form(FL_NO_BOX,490,320);
  obj := fl_add_box(FL_BORDER_BOX,0,0,490,320,'');
  obj := fl_add_chart(FL_LINE_CHART,20,160,390,140,'');
  chartobj := obj;
  fl_set_object_dblbuffer(obj,1);

  fl_bgn_group();
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,30,120,170,30,'sin(x)');
  sinobj := obj ;
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_function),1);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,30,90,170,30,'sin(2x)*cos(x)');
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_function),2);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,30,60,170,30,'sin(2x)+cos(x)');
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_function),3);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,240,120,160,30,'sin(3x)+cos(x)');
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_function),4);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,240,90,160,30,'sin(x)^2 + cos(x)');
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_function),5);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,240,60,160,30,'sin(x)^3');
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_function),6);
  fl_end_group();

  obj := fl_add_button(FL_NORMAL_BUTTON,150,20,140,30,'Exit');
  exitbut := obj;
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
  obj:= fl_add_valslider(FL_VERT_SLIDER,430,20,40,280,'');
  stepobj := obj ;
    fl_set_object_boxtype(obj,FL_BORDER_BOX);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_step),0);
  fl_end_form;
end;

{**********************************}

function next_step : double;

var res : double;

begin
  res := 0.0;
  case func of
     1: res := sin(xx);
     2: res := sin(2*xx)*cos(xx);
     3: res := sin(2*xx)+cos(xx);
     4: res := sin(3*xx)+cos(xx);
     5: res := sin(xx)*sin(xx) + cos(xx);
     6: res := sin(xx)*sin(xx)*sin(xx);
  end;
  xx := xx+step;
  next_step:=res;
end;

function idle_cb (ex : PXEvent; d : pointer) : longint;
begin
    fl_insert_chart_value(chartobj,1,next_step(),'',1);
end;

var obj : PFL_OBJECT;

begin
  func:= 1;
  xx:= 0.0;
  step:= 0.15;
  fl_flip_yorigin();
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  create_form_form();
  fl_set_chart_bounds(chartobj,-1.5,1.5);
  fl_set_chart_maxnumb(chartobj,80);
  fl_set_chart_autosize(chartobj,0);
  fl_set_button(sinobj,1);
  fl_set_slider_value(stepobj,0.15);
  fl_set_slider_bounds(stepobj,0.0,0.4);
{$ifdef nevertrue}
  fl_set_idle_delta(15);
{$endif }
  fl_show_form(form,FL_PLACE_CENTER,FL_NOBORDER,'StripChart');
  repeat
    fl_insert_chart_value(chartobj,1,next_step(),'',1);
    obj := fl_check_forms();
  until obj = exitbut;
end.
