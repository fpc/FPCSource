program buttonall;

uses xforms;

type TFD_buttform = record
        buttform : PFL_FORM;
        vdata : pointer;
        ldata : Longint;
        backface,done,objsgroup,bbutt,pbutt,bw_obj : PFL_OBJECT;
end;
PFD_buttform = ^TFD_buttform;

Procedure done_cb(ob : PFL_OBJECT; data :  longint);cdecl;
begin
   halt(0);
end;

procedure bw_cb(ob : PFL_OBJECT; data : longint);cdecl;

const
    bws : array [0..7] of longint = (-4,-3,-2,-1,1,2,3,4);
var
  n : longint;
  fdui : PFD_BUTTform;

begin
    n:=fl_get_choice(ob)-1;
    fdui:=ob^.form^.fdui;
    fl_freeze_form(ob^.form);
    fl_set_object_bw(fdui^.backface, bws[n]);
    fl_set_object_bw(fdui^.objsgroup, bws[n]);
    { redrawing the backface wipes out the done button. Redraw it }
    fl_redraw_object(fdui^.done);
    fl_unfreeze_form(ob^.form);
end;


function create_form_buttform : PFD_buttform;

var
  obj : PFL_OBJECT;
  fdui : PFD_buttform;

begin
  new(fdui);
  fdui^.buttform := fl_bgn_form(FL_NO_BOX, 290, 260);
  obj := fl_add_box(FL_UP_BOX,0,0,290,260,'');
  fdui^.backface := obj;
  obj := fl_add_button(FL_NORMAL_BUTTON,185,215,90,30,'Done');
  fdui^.done := obj;
  fl_set_object_callback(obj,PFL_CALLBACKPTR(@done_cb),0);

  fdui^.objsgroup := fl_bgn_group();
  obj := fl_add_frame(FL_ENGRAVED_FRAME,175,170,100,30,'');
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
  obj := fl_add_round3dbutton(FL_PUSH_BUTTON,210,170,30,30,'');
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
  obj := fl_add_bitmapbutton(FL_NORMAL_BUTTON,25,85,40,40,'bitmapbutton');
  fdui^.bbutt := obj ;
    fl_set_object_color(obj,FL_COL1,FL_BLACK);
  obj := fl_add_pixmapbutton(FL_NORMAL_BUTTON,25,25,40,40,'pixmapbutton');
  fdui^.pbutt := obj;
  obj := fl_add_checkbutton(FL_RADIO_BUTTON,100,31,70,32,'Red');
    fl_set_object_color(obj,FL_COL1,FL_RED);
  obj := fl_add_checkbutton(FL_RADIO_BUTTON,100,60,70,32,'Green');
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
  obj := fl_add_checkbutton(FL_RADIO_BUTTON,100,90,70,32,'Blue');
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
  obj := fl_add_lightbutton(FL_PUSH_BUTTON,20,170,92,30,'LightButton');
    fl_set_button(obj, 1);
  obj := fl_add_roundbutton(FL_PUSH_BUTTON,200,35,75,25,'Red');
    fl_set_object_color(obj,FL_COL1,FL_RED);
  obj := fl_add_roundbutton(FL_PUSH_BUTTON,200,64,75,25,'Green');
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
  obj := fl_add_roundbutton(FL_PUSH_BUTTON,200,93,75,25,'Blue');
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
  obj := fl_add_round3dbutton(FL_PUSH_BUTTON,180,170,30,30,'');
    fl_set_object_color(obj,FL_COL1,FL_RED);
  obj := fl_add_round3dbutton(FL_PUSH_BUTTON,240,170,30,30,'');
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
  obj := fl_add_button(FL_PUSH_BUTTON,130,210,30,30,'go');
    fl_set_object_boxtype(obj,FL_OVAL3D_UPBOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj := fl_add_button(FL_NORMAL_BUTTON,20,210,90,30,'Button');
    fl_set_object_boxtype(obj,FL_ROUNDED3D_UPBOX);
  obj := fl_add_choice(FL_NORMAL_CHOICE2,105,135,80,30,'BW');
  fdui^.bw_obj := obj;
  fl_set_object_callback(obj,PFL_CALLBACKPTR(@bw_cb),0);
  obj := fl_add_labelframe(FL_ENGRAVED_FRAME,190,25,85,100,'RoundButton');
  obj := fl_add_labelframe(FL_ENGRAVED_FRAME,90,25,90,100,'CheckButton');
  fl_end_group();

  fl_end_form();

  fdui^.buttform^.fdui := fdui;
  create_form_buttform:=fdui;
end;

var   fd_buttform : PFD_buttform;

begin
   fl_initialize(@argc, argv, '', nil, 0);
   fd_buttform := create_form_buttform;

   fl_set_pixmapbutton_file(fd_buttform^.pbutt,'crab45.xpm');
   fl_set_bitmapbutton_file(fd_buttform^.bbutt,'bm1.xbm');
   fl_addto_choice(fd_buttform^.bw_obj,' -4 | -3 | -2 | -1 |  1|  2|  3|  4');
   fl_set_choice(fd_buttform^.bw_obj,7);

   fl_show_form(fd_buttform^.buttform,FL_PLACE_CENTER,FL_FULLBORDER,'buttform');
   while (fl_do_forms<>nil) do ;
end.
