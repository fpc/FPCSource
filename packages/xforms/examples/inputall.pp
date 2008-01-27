program inputall;

uses xforms;

type TFD_INPUT = record
        input : PFL_FORM;
        vdata : pointer;
        norminput,multiinput,report : PFL_OBJECT;
   end;
PFD_INPUT = ^TFD_input;

procedure done_cb( ob : PFL_OBJECT; data : longint);cdecl;

begin
   halt(0);
end;

procedure input_cb( ob : PFL_OBJECT; data : longint);cdecl;

var
   cx, cy, pos : Longint;
   buf,temp : string[128];

begin
   pos := fl_get_input_cursorpos(ob, @cx,@cy);
   str (pos,temp); buf:='P = '+temp+' ';
   str (cx,temp); buf:=buf+'x= '+temp+' ';
   str (cy,temp); buf:=buf+'y= '+temp+#0;
   fl_set_object_label( PFD_input(ob^.form^.fdui)^.report,@buf[1]);
end;

procedure hide_show_cb(ob : PFL_OBJECT; data : longint);cdecl;

var  fd: PFD_input;

begin
    fd := ob^.form^.fdui;
    if (fd^.multiinput^.visible<>0) then
       fl_hide_object(fd^.multiinput)
    else
       fl_show_object(fd^.multiinput);
end;


function create_form_input : PFD_input;

var
  obj : PFL_OBJECT;
  fdui : PFD_input;

begin
  new(fdui);
  fdui^.input := fl_bgn_form(FL_NO_BOX, 441, 441);
  obj := fl_add_box(FL_UP_BOX,0,0,441,441,'');
  obj := fl_add_input(FL_NORMAL_INPUT,40,40,340,30,'NormalInput');
  fdui^.norminput := obj;
  fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@input_cb),0);
  obj := fl_add_input(FL_INT_INPUT,40,100,160,30,'IntInput');
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
  obj := fl_add_input(FL_FLOAT_INPUT,230,100,160,30,'FloatInput');
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
  obj := fl_add_input(FL_DATE_INPUT,40,150,160,30,'DateInput');
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
  obj := fl_add_input(FL_SECRET_INPUT,230,150,160,30,'Secretinput');
    fl_set_object_lalign(obj,FL_ALIGN_TOP_LEFT);
  obj:= fl_add_input(FL_MULTILINE_INPUT,40,210,360,180,'MMM');
  fdui^.multiinput := obj;
  fl_set_object_callback(obj,PFL_CALLBACKPTR(@input_cb),0);
  obj := fl_add_text(FL_NORMAL_TEXT,30,400,210,30,'');
  fdui^.report := obj;
    fl_set_object_lalign(obj,FL_ALIGN_LEFT or FL_ALIGN_INSIDE);
  obj := fl_add_button(FL_NORMAL_BUTTON,330,400,70,30,'Done');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@done_cb),0);
  obj := fl_add_button(FL_NORMAL_BUTTON,250,400,70,30,'Hide/Show');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@hide_show_cb),0);
  fl_end_form();
  fdui^.input^.fdui := fdui;
  create_form_input:=fdui;
end;


var fd_input : PFD_input;

begin
   fl_initialize(@argc, argv, '', nil, 0);
   fd_input := create_form_input();

   { fill-in form initialization code }
   fl_set_object_dblbuffer(fd_input^.report,1);
   fl_set_object_return(fd_input^.multiinput,FL_RETURN_ALWAYS);
   fl_set_object_return(fd_input^.norminput,FL_RETURN_ALWAYS);

   { show the first form }
   fl_show_form(fd_input^.input,FL_PLACE_CENTERFREE,FL_FULLBORDER,'input');
   while (fl_do_forms<>nil) do;
end.
