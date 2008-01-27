Program cursor;

{ Cursor routines demo. }

uses xforms;

{$i bm1.xbm}
{$i bm2.xbm}

type
TFD_cursor = record
        cursor : PFL_FORM;
        vdata : Pointer;
        ldata : Longint;
        end;
PFD_Cursor = ^TFD_Cursor;

var
  bitmapcur : Longint;


function create_form_cursor : PFD_cursor; Forward;

{ callbacks for form cursor }
procedure setcursor_cb(ob : PFL_Object; data : Longint);
begin
   fl_set_cursor(FL_ObjWin(ob), data);
end;

procedure setbitmapcursor_cb(ob : PFL_OBJECT ; data : Longint);cdecl;

begin
  if bitmapcur<>0 then
     bitmapcur := longint(fl_create_bitmap_cursor(Pchar(@bm1_bits), Pchar(@bm2_bits),
                  16, 16, 8 ,8 ));
                  {bm1_width, bm1_height,  bm1_width/2, bm1_height/2);}
  fl_set_cursor(FL_ObjWin(ob), bitmapcur);
end;


Procedure done_cb (ob : PFL_OBJECT; data : Longint);cdecl;
begin
    Halt(0);
end;


Function create_form_cursor : PFD_cursor;

var
  obj : PFL_OBJECT;
  fdui : PFD_cursor;

begin
  new(fdui);

  fdui^.cursor := fl_bgn_form(FL_NO_BOX, 325, 175);
  obj := fl_add_box(FL_UP_BOX,0,0,325,175,'');
  obj := fl_add_frame(FL_EMBOSSED_FRAME,10,10,305,120,'');
  obj := fl_add_button(FL_NORMAL_BUTTON,20,20,65,30,'Hand');
    fl_set_object_callback(obj,PFL_CALLBACkPTR(@setcursor_cb),XC_hand2);
  obj := fl_add_button(FL_NORMAL_BUTTON,250,140,60,25,'Done');
    fl_set_object_callback(obj,PFL_CALLBACkPTR(@done_cb),0);
  obj := fl_add_button(FL_NORMAL_BUTTON,95,20,65,30,'Watch');
    fl_set_object_callback(obj,PFL_CALLBACkPTR(@setcursor_cb),XC_watch);
  obj := fl_add_button(FL_NORMAL_BUTTON,170,20,65,30,'Invisible');
    fl_set_object_callback(obj,PFL_CALLBACkPTR(@setcursor_cb),FL_INVISIBLE_CURSOR);
  obj := fl_add_button(FL_NORMAL_BUTTON,90,70,140,50,'DefaultCursor');
    fl_set_button_shortcut(obj,'Dd#d',1);
    fl_set_object_callback(obj,PFL_CALLBACkPTR(@setcursor_cb),FL_DEFAULT_CURSOR);
  obj := fl_add_button(FL_NORMAL_BUTTON,245,20,65,30,'BitmapCur');
    fl_set_object_callback(obj,PFL_CALLBACkPTR(@setbitmapcursor_cb),0);
  fl_end_form;

  create_form_cursor:= fdui;
end;

var fd_cursor : PFD_cursor ;

Begin

   fl_set_border_width(-2);
   fl_initialize(@argc, argv, 'FormDemo', nil, 0);
   fd_cursor := create_form_cursor;

   { fill-in form initialization code }

   fl_show_form(fd_cursor^.cursor,FL_PLACE_CENTER,FL_FULLBORDER,'cursor');
   fl_do_forms;
end.
