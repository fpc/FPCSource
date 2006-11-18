{ This demo show the different boxtypes. Note that some
 * boxtypes are not appropriate for some objects
 }
program boxtype;

uses xforms;

const border = FL_TRANSIENT;


type TVN_Struct = record
     theval : longint;
     name : pchar;
     end;

const gmode : array[0..5] of TVN_Struct =
 ((theval : FL_StaticGray;  name : 'StaticGray'),
 (theval : FL_GrayScale;   name : 'GrayScale'),
 (Theval : FL_StaticColor; name : 'StaticColor'),
 (Theval : FL_Pseudocolor; name : 'PseudoColor'),
 (theval : FL_Truecolor;   name : 'TrueColor'),
 (Theval : FL_DirectColor; name : 'DirectColor'));

Const btypes : array [0..15] of TVN_struct=
(
   (theval : FL_NO_BOX; name : 'no box'),
   (theval : FL_UP_BOX; name : 'up box'),
   (theval : FL_DOWN_BOX; name : 'down box'),
   (theval : FL_BORDER_BOX; name : 'border box'),
   (theval : FL_SHADOW_BOX; name : 'shadow box'),
   (theval : FL_FLAT_BOX; name : 'flat box'),
   (theval : FL_FRAME_BOX; name : 'frame box'),
   (theval : FL_EMBOSSED_BOX; name : 'embossed box'),
   (theval : FL_ROUNDED_BOX; name : 'rounded box'),
   (theval : FL_RFLAT_BOX; name : 'rflat box'),
   (theval : FL_RSHADOW_BOX; name : 'shadow box'),
   (theval : FL_OVAL_BOX; name : 'oval box'),
   (theval : FL_ROUNDED3D_UPBOX; name : 'rounded3d upbox'),
   (theval : FL_ROUNDED3D_DOWNBOX; name : 'rounded3d downbox'),
   (theval : FL_OVAL3D_UPBOX; name : 'oval3d upbox'),
   (theval : FL_OVAL3D_DOWNBOX; name : 'oval3d downbox')
   );

{$i srs.xbm}

{************** Callback *********************}

var
  form : PFL_FORM;
  tobj : array [0..17] of PFL_OBJECT;
  exitob, btypeob, modeob : PFL_OBJECT;
Const
lastbt : Longint = -1;

procedure boxtype_cb (ob : PFL_OBJECT; arg : longint);cdecl;

var
  i, req_bt  : longint;

begin
  req_bt:= fl_get_choice(ob) - 1;
  if (lastbt <> req_bt) then
    begin
     fl_freeze_form (form);
     fl_redraw_form (form);
     i:=0;
     while i<18 do
       begin
       fl_set_object_boxtype (tobj[i], btypes[req_bt].theval);
       inc(i);
       end;
     fl_unfreeze_form (form);
     lastbt := req_bt;
    end;
end;

Const lval : longint = -1;

procedure  mode_cb (ob : PFL_OBJECT; arg : longint);cdecl;

var
  db,theval : longint;

begin
  db:=0;
  theval := fl_get_choice (ob) -1;
  if (theval = lval) or ( theval < 0) then exit;

  fl_hide_form (form);
  if not (fl_mode_capable(gmode[theval].theval, 0)<>0) then
    begin
      fl_set_choice(ob, lval);
      theval := lval;
    end;

  fl_set_graphics_mode (gmode[theval].theval, db);
  fl_show_form (form, FL_PLACE_GEOMETRY, border, 'Box types');

  lval := theval;
end;

{************** Creation Routines ********************}

procedure create_form_form;
var
  obj : PFL_OBJECT;

begin
  form := fl_bgn_form(FL_NO_BOX, 720, 520);
  obj := fl_add_box(FL_UP_BOX, 0, 0, 720, 520, '');
  fl_set_object_color(obj, FL_BLUE, FL_COL1);
  obj := fl_add_box(FL_DOWN_BOX, 10, 90, 700, 420, '');
  fl_set_object_color(obj, FL_COL1, FL_COL1);
  obj := fl_add_box(FL_DOWN_BOX, 10, 10, 700, 70, '');
  fl_set_object_color(obj, FL_SLATEBLUE, FL_COL1);
  obj:= fl_add_box(FL_UP_BOX, 30, 110, 110, 110, 'Box');
  tobj[0] := obj ;
  obj:= fl_add_text(FL_NORMAL_TEXT, 30, 240, 110, 30, 'Text');
  tobj[1] := obj ;
  obj:= fl_add_bitmap(FL_NORMAL_BITMAP, 40, 280, 90, 80, 'Bitmap');
  tobj[2] := obj ;
  fl_set_object_lcol(obj, FL_BLUE);
  obj:= fl_add_chart(FL_BAR_CHART, 160, 110, 160, 110, 'Chart');
  tobj[3] := obj ;
  obj:= fl_add_clock(FL_ANALOG_CLOCK, 40, 390, 90, 90, 'Clock');
  tobj[4] := obj ;
  fl_set_object_dblbuffer(tobj[4],1);
  obj:=fl_add_button(FL_NORMAL_BUTTON, 340, 110, 120, 30, 'Button');
  tobj[5]:=obj;
  obj:=fl_add_lightbutton(FL_PUSH_BUTTON,340,150,120,30,'Lightbutton');
  tobj[6]:=obj;
  obj:=fl_add_roundbutton(FL_PUSH_BUTTON,340,190,120,30,'Roundbutton');
  tobj[7]:=obj;
  obj:=fl_add_slider(FL_VERT_SLIDER, 160, 250, 40, 230, 'Slider');
  tobj[8]:=obj;
  obj:=fl_add_valslider(FL_VERT_SLIDER, 220, 250, 40, 230, 'Valslider');
  tobj[9]:=obj;
  obj:=fl_add_dial (FL_LINE_DIAL, 280, 250, 100, 100, 'Dial');
  tobj[10]:=obj;
  obj:=fl_add_positioner(FL_NORMAL_POSITIONER,280,380,150,100, 'Positioner');
  tobj[11]:=obj;
  obj:=fl_add_counter (FL_NORMAL_COUNTER,480,110,210,30, 'Counter');
  tobj[12]:=obj;
  obj:=fl_add_input (FL_NORMAL_INPUT, 520,170,170,30, 'Input');
  tobj[13]:=obj;
  obj:=fl_add_menu (FL_PUSH_MENU, 400, 240, 100, 30, 'Menu');
  tobj[14]:=obj;
  obj:=fl_add_choice (FL_NORMAL_CHOICE, 580, 250, 110, 30, 'Choice');
  tobj[15]:=obj;
  obj:=fl_add_timer (FL_VALUE_TIMER, 580, 210, 110, 30, 'Timer');
  tobj[16]:=obj;
  fl_set_object_dblbuffer(tobj[16], 1);
  obj:=fl_add_browser (FL_NORMAL_BROWSER,450,300,240, 180, 'Browser');
  tobj[17]:=obj;
  obj:= fl_add_button (FL_NORMAL_BUTTON, 590, 30, 100, 30, 'Exit');
  exitob:=obj;
  obj:= fl_add_choice (FL_NORMAL_CHOICE,110,30, 130, 30, 'Boxtype');
  btypeob:=obj;
  fl_set_object_callback (obj, PFL_CALLBACKPTR(@boxtype_cb), 0);
  obj  :=fl_add_choice(FL_NORMAL_CHOICE,370,30,130,30,'Graphics mode');
  modeob := obj;
  fl_set_object_callback (obj, PFL_CALLBACKPTR(@mode_cb), 0);
  fl_end_form ;
end;

procedure create_the_forms;
begin
  create_form_form;
end;

{************** Main Routine **********************}

const browserlines : array [0..21] of pchar = (
   ' ', '@C1@c@l@bObjects Demo',   ' ',
   'This demo shows you all',      'objects that currently',
   'exist in the Forms Library.',  ' ',
   'You can change the boxtype',   'of the different objects',
   'using the buttons at the',     'top of the form. Note that',
   'some combinations might not',  'look too good. Also realize',
   'that for all object classes',  'many different types are',
   'available with different',     'behaviour.', ' ',
   'With this demo you can also',  'see the effect of the drawing',
   'mode on the appearance of the','objects.');


var
  ob : PFL_OBJECT;
  c : TFL_COLOR;
  p : ppchar;
  vn : ^TVN_struct;
  i : longint;

begin
  c := FL_BLACK;
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  create_the_forms ();
  fl_set_bitmap_data (tobj[2], sorceress_width, sorceress_height, pchar(@sorceress_bits));
  fl_add_chart_value (tobj[3], 15, 'item 1', c);inc(c);
  fl_add_chart_value (tobj[3], 5, 'item 2', c);inc(c);
  fl_add_chart_value (tobj[3], -10, 'item 3', c);inc(c);
  fl_add_chart_value (tobj[3], 25, 'item 4', c);inc(c);
  fl_set_menu (tobj[14], 'item 1|item 2|item 3|item 4|item 5');
  fl_addto_choice (tobj[15], 'item 1');
  fl_addto_choice (tobj[15], 'item 2');
  fl_addto_choice (tobj[15], 'item 3');
  fl_addto_choice (tobj[15], 'item 4');
  fl_addto_choice (tobj[15], 'item 5');
  fl_set_timer (tobj[16], 1000.0);
  for i:=0 to 21 do
     fl_add_browser_line (tobj[17], browserlines[i]);
  for i:= 0 to 15 do
     fl_addto_choice(btypeob, btypes[i].name);
  for i:=0 to 5 do
      begin
      fl_addto_choice (modeob, gmode[i].name);
      if (fl_mode_capable(gmode[i].theval, 0)=0) then
        fl_set_choice_item_mode(modeob, i, FL_PUP_GRAY);
      end;
  fl_set_choice (modeob, fl_vmode+1);
   fl_show_form (form, FL_PLACE_MOUSE, border, 'Box types');
  while (fl_do_forms <> exitob) do;
end.
