{ demo showing the choices when to return object. Note this program,
 * strictly speaking, is illegal in the usage of user data parameter
 * in the callback function.
 }
program objreturn;

uses xforms;


{*** Forms and Objects ***}
type
TFD_FORM = record
    form0 : PFL_FORM;
    obj : array [0..3] of PFL_OBJECT;
    br : PFL_OBJECT;
    when : PFL_OBJECT;
    vdata : pointer;
    ldata : longint;
end;
PFD_FORM = ^TFD_FORM;

var
fd_form0 : PFD_form;

{ callbacks for form form0 }
procedure return_cb(ob : PFL_OBJECT; data : longint); cdecl;

begin
    fl_addto_browser(fd_form0^.br, pchar(data));
end;

procedure set_when( n : longint);

begin
    fl_set_object_return(fd_form0^.obj[0], n);
    fl_set_object_return(fd_form0^.obj[1], n);
    fl_set_object_return(fd_form0^.obj[2], n);
    fl_set_object_return(fd_form0^.obj[3], n);
end;

procedure when_cb(ob : PFL_OBJECT; data : longint); cdecl;

var n : longint;

begin
    n := fl_get_choice(ob) - 1;
    if (n >= 0) then
        set_when(n);
end;

procedure resetlog_cb(ob : PFL_OBJECT; data : longint);cdecl;

begin
    fl_clear_browser(fd_form0^.br);
end;

Function create_form_form0 : PFD_FORM;

Const Preturn : pchar = 'slider returned';
      Pcounter : pchar = 'counter returned';
      Pinput : pchar = 'input2 returned';
      Pinput1 : Pchar =  'input1 returned';

var
  obj : PFL_OBJECT;
  fdui : PFD_form;
  old_bw : longint;

begin
  new(fdui);
  old_bw := fl_get_border_width();

  fl_set_border_width(-2);
  fdui^.form0 := fl_bgn_form(FL_NO_BOX, 321, 276);
  obj := fl_add_box(FL_UP_BOX,0,0,321,276,'');
  obj := fl_add_valslider(FL_HOR_SLIDER,12,55,138,22,'');
  fdui^.obj[0] := obj;
    fl_set_object_lalign(obj,FL_ALIGN_BOTTOM or FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@return_cb),longint(Preturn));
     fl_set_slider_return(obj, FL_RETURN_CHANGED);
  obj := fl_add_counter(FL_NORMAL_COUNTER,12,85,138,22,'');
  fdui^.obj[1] := obj;
    fl_set_object_lalign(obj,FL_ALIGN_BOTTOM or FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@return_cb),longint(PCounter));
  obj := fl_add_input(FL_NORMAL_INPUT,12,187,138,25,'');
  fdui^.obj[3] := obj;
    fl_set_object_lalign(obj,FL_ALIGN_LEFT or FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@return_cb),longint(Pinput));
  obj := fl_add_input(FL_NORMAL_INPUT,12,150,138,25,'');
  fdui^.obj[2] := obj;
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@return_cb),longint(Pinput1));
  obj := fl_add_browser(FL_NORMAL_BROWSER,170,55,140,160,'');
  fdui^.br := obj;
  obj := fl_add_choice(FL_NORMAL_CHOICE,80,12,168,27,'');
  fdui^.when := obj;
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@when_cb),0);
  obj := fl_add_button(FL_NORMAL_BUTTON,170,239,80,25,'Done');
  obj := fl_add_button(FL_NORMAL_BUTTON,70,239,80,25,'ResetLog');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@resetlog_cb),0);
  fl_end_form();
  fl_set_border_width(old_bw);

  create_form_form0:= fdui;
end;


begin
    fl_initialize(@argc, argv, 'FormDemo', nil, 0);
    fd_form0 := create_form_form0;

    { fill-in form initialization code }
    set_when(0);
    fl_set_object_dblbuffer(fd_form0^.br, 1);
    fl_addto_choice(fd_form0^.when,
              'RETURN_END_CHANGED|RETURN_CHANGED|RETURN_END|RETURN_ALWAYS');

    { show the first form }
    fl_show_form(fd_form0^.form0, FL_PLACE_CENTER, FL_FULLBORDER, 'form0');
    fl_do_forms();
end.
