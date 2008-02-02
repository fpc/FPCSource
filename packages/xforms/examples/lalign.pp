program lalign;

{ different label alignments }
uses xforms;

type
TFD_Form0 = record
        form0 : PFL_FORM;
        box,inside,center : PFL_Object;
        vdata : pointer;
        ldata  : longint;
end;
PFD_Form0 = ^TFD_Form0;

var fd_form0 : PFD_form0;

    align :integer;


Procedure align_cb(ob : PFL_OBJECT; n : longint);cdecl;
begin
    if fl_get_button(fd_form0^.inside)<>0 then
       n:=n or FL_ALIGN_INSIDE;
    fl_set_object_lalign(fd_form0^.box, n);
end;

Procedure inside_cb(Ob : PFL_OBJECT; data : longint);cdecl;

begin
   if fl_get_button(ob)<>0 then
      fd_form0^.box^.align := fd_form0^.box^.align or FL_ALIGN_INSIDE
   else
      fd_form0^.box^.align := fd_form0^.box^.align and not FL_ALIGN_INSIDE;
   fl_redraw_form(fd_form0^.form0);
end;

function create_form_form0 : PFD_form0;

Var
  obj : PFL_OBJECT;
  fdui : PFD_form0;
begin
  new(fdui);
  fdui^.form0 := fl_bgn_form(FL_NO_BOX, 351, 170);
  obj := fl_add_box(FL_UP_BOX,0,0,351,170,'');
  obj := fl_add_box(FL_UP_BOX,190,45,90,45,'abcdefg'#10'hijklmno');
  fdui^.box := obj ;
  {  fl_set_object_clip(obj, 1); }
  obj := fl_add_lightbutton(FL_PUSH_BUTTON,20,120,90,30,'Inside');
  fdui^.inside := obj;
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@inside_cb),0);
 fl_bgn_group();
  obj := fl_add_button(FL_RADIO_BUTTON,20,20,30,30,'@#7->');
    fl_set_object_lcol(obj,FL_BLUE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@align_cb),FL_ALIGN_LEFT_TOP);
  obj := fl_add_button(FL_RADIO_BUTTON,50,20,30,30,'@#8->');
    fl_set_object_lcol(obj,FL_BLUE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@align_cb),FL_ALIGN_TOP);
  obj := fl_add_button(FL_RADIO_BUTTON,80,20,30,30,'@#9->');
    fl_set_object_lcol(obj,FL_BLUE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@align_cb),FL_ALIGN_RIGHT_TOP);
  obj := fl_add_button(FL_RADIO_BUTTON,80,50,30,30,'@#->');
    fl_set_object_lcol(obj,FL_BLUE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@align_cb),FL_ALIGN_RIGHT);
  obj := fl_add_button(FL_RADIO_BUTTON,50,50,30,30,'@circle');
  fdui^.center := obj;
    fl_set_object_lcol(obj,FL_RED);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@align_cb),FL_ALIGN_CENTER);
  obj := fl_add_button(FL_RADIO_BUTTON,20,50,30,30,'@#<-');
    fl_set_object_lcol(obj,FL_BLUE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@align_cb),FL_ALIGN_LEFT);
  obj := fl_add_button(FL_RADIO_BUTTON,20,80,30,30,'@#1->');
    fl_set_object_lcol(obj,FL_BLUE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@align_cb),FL_ALIGN_LEFT_BOTTOM);
  obj := fl_add_button(FL_RADIO_BUTTON,50,80,30,30,'@#2->');
    fl_set_object_lcol(obj,FL_BLUE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@align_cb),FL_ALIGN_BOTTOM);
  obj := fl_add_button(FL_RADIO_BUTTON,80,80,30,30,'@#3->');
    fl_set_object_lcol(obj,FL_BLUE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@align_cb),FL_ALIGN_RIGHT_BOTTOM);
  fl_end_group();

  obj := fl_add_button(FL_NORMAL_BUTTON,140,120,70,30,'Done');
  fl_end_form();

  create_form_form0 := fdui;
end;

begin
   fl_initialize(@argc, argv, 'FormDemo', nil, 0);
   fd_form0 := create_form_form0();

   { fill-in form initialization code }
   fl_set_form_dblbuffer(fd_form0^.form0, 1);
   align := fd_form0^.box^.align or FL_ALIGN_INSIDE;
   if align <> fd_form0^.box^.align then
     fl_set_button(fd_form0^.inside, 1);

   { show the first form }
   fl_show_form(fd_form0^.form0,FL_PLACE_FREE,FL_FULLBORDER,'form0');
   fl_do_forms();
end.
