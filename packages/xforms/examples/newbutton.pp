{
 * Demo showing the use of user defined object class: CROSSBUTTON
 *
 * Form definition file generated with fdesign.
 * changed stub button to crossbutton
 }

uses xforms;

Const FL_CROSSBUTTON=2000;
      FL_CROSSBUTTON_BW = 2;

{ How to draw it }
type TSPEC = TFL_BUTTON_STRUCT;
     PSPEC = ^TSPEC;

Procedure draw_crossbutton(ob : PFL_OBJECT);cdecl;

var temp : longint;
    xx, yy, ww, hh ,tt : TFL_Coord;

begin
   temp:=0;
   { if redraw is demanded by FL_ENTER, ignore it }

   if PSPEC(ob^.spec)^.event = FL_ENTER then exit;

   { draw the bounding box first }
   fl_drw_box(ob^.boxtype, ob^.x, ob^.y, ob^.w, ob^.h, ob^.col1, ob^.bw);

   { draw the box that contains the cross }
   if ob^.w>ob^.h then tt:=ob^.h else tt:=ob^.w;

    ww := tt div 2 - 1;
    hh:=ww;
    xx := ob^.x + FL_BOUND_WIDTH;
    yy := ob^.y + (ob^.h - hh) div 2;


   { if pushed, draw a down box with the cross }
   if PSPEC(ob^.spec)^.val<>0 then
      begin
      fl_drw_box(FL_DOWN_BOX, xx, yy, ww, hh, ob^.col1, ob^.bw);
      fl_drw_text(FL_ALIGN_CENTER, xx-2, yy-2, ww+4, hh+4, ob^.col2, 0, 0, '@9plus');
      end
   else
     begin
      fl_drw_box(FL_UP_BOX, xx, yy, ww, hh, ob^.col1, ob^.bw);
     end;

    { label }
    if (ob^.align = FL_ALIGN_CENTER) then
        fl_drw_text(FL_ALIGN_LEFT, xx + ww + 3, ob^.y, 0, ob^.h,
                    ob^.lcol, ob^.lstyle, ob^.lsize, ob^.thelabel)
    else
        fl_draw_object_label_outside(ob);

    if (ob^.thetype = FL_RETURN_BUTTON) then
        fl_drw_text(FL_ALIGN_CENTER,
                    TFL_Coord (ob^.x + ob^.w - round(0.8 * ob^.h)),
                    TFL_Coord (ob^.y + round(0.2 * ob^.h)),
                    TFL_Coord (round(0.6 * ob^.h)),
                    TFL_Coord (round(0.6 * ob^.h)), ob^.lcol, 0, 0, '@returnarrow');

end;


{ creation }
function fl_create_crossbutton(Thetype : Longint; x,y,w,h : TFL_Coord; thelabel : Pchar) : PFL_OBJECT;

var ob : PFL_OBJECT;

begin
     fl_add_button_class(FL_CROSSBUTTON, PFL_Drawbutton(@draw_crossbutton), nil);
     ob := fl_create_generic_button(FL_CROSSBUTTON, thetype, x, y,w, h, thelabel);
     ob^.boxtype := FL_NO_BOX;
     ob^.col2 := FL_BLACK;   { cross color }
     { ob^.bw := FL_CROSSBUTTON_BW; }
     fl_create_crossbutton:=ob;
end;

function fl_add_crossbutton(Thetype : longint; x,y,w,h : TFL_COORD; thelabel : pchar): PFL_OBJECT;

var ob : PFL_OBJECT;
    theform : PFL_FORM;

begin
    ob := fl_create_crossbutton(thetype, x, y, w, h, thelabel);
    theform:=fl_current_form;
    fl_add_object(theform, ob);
    fl_add_crossbutton:=ob;
end;

{*** Forms and Objects ***}
type TFD_NEwBut = record
        newbut : PFL_FORM;
        bexit : PFL_object;
        vdata : pointer;
        ldata : Longint;
end;
PFD_NewBut= ^TFD_NewBut;

function create_form_newbut : PFD_Newbut;

var
  obj : PFL_OBJECT;
  fdui  : PFD_newbut;
  oldbw : longint;

begin
  new (fdui);
  oldbw := fl_get_border_width;

  fl_set_border_width(-2);
  fdui^.newbut := fl_bgn_form(FL_NO_BOX, 310, 190);
  obj := fl_add_box(FL_UP_BOX,0,0,310,190,'');
  obj := fl_add_labelframe(FL_ENGRAVED_FRAME,40,45,100,120,'CrossB');
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj := fl_add_crossbutton(FL_RADIO_BUTTON,50,115,80,30,'cross1');
    fl_set_object_color(obj,FL_COL1,FL_RED);
  obj := fl_add_crossbutton(FL_RADIO_BUTTON,50,85,80,30,'Button');
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
  obj := fl_add_crossbutton(FL_RADIO_BUTTON,50,55,80,30,'Button');
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
  obj := fl_add_labelframe(FL_ENGRAVED_FRAME,180,45,100,120,'CrossB');
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj := fl_add_crossbutton(FL_PUSH_BUTTON,190,115,80,30,'Button');
    fl_set_object_color(obj,FL_COL1,FL_RED);
  obj := fl_add_crossbutton(FL_PUSH_BUTTON,190,85,90,30,'Button');
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
  obj := fl_add_crossbutton(FL_PUSH_BUTTON,190,55,80,30,'Button');
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
  obj := fl_add_button(FL_NORMAL_BUTTON,125,10,65,25,'Exit');
  fdui^.bexit :=obj;
  fl_end_form;
  fl_set_border_width(oldbw);

  create_form_newbut:=fdui;
end;


var cbform : PFD_newbut;

begin
      fl_initialize(@argc, argv, 'FormDemo', nil, 0);
      cbform := create_form_newbut();
      fl_show_form(cbform^.newbut, FL_PLACE_CENTER, 0, nil);
      while(fl_do_forms <> cbform^.bexit) do ;
end.
