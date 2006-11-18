program group;

uses xforms;

{$i porsche.xpm}
{$i nomail.xbm}
{$i bm1.xbm}

Const NGROUP = 4;

type TFD_objsform = record
        objsform : PFL_Form;
        vdata : Pointer;
        ldata : longint;
        bitbutton,
        pixbutton,
        bit,
        pix,
        chart,
        quit,
        menu,
        choice,
        browser,
        xyplot : PFL_Object;
        button,group :  array [0..4] of PFL_OBJECT;
end;
PFD_objsform = ^TFD_objsform;

var fd_objsform : PFD_objsform;

{ callbacks for form objsform }
procedure show_group(ob : PFL_OBJECT;data:  longint);cdecl;

var  i : longint;

begin
  for i := 0 to NGROUP do
    if i=data then
      fl_show_object(fd_objsform^.group[i])
    else
      fl_hide_object(fd_objsform^.group[i]);
end;

procedure init_gui(fd : PFD_objsform);

const
    x : array[0..5] of real = ( 0.0, 1.0, 2.0, 3.0, 4.0, 5.0 );
    y : array[0..5] of real = ( 5.5, 4.0, 4.5, 3.8, 4.0, 5.0 );
    labels: array[0..5] of pchar = ('Mon','Tue','Wed', 'Thur','Fri','Sat');

var
    i : integer;

begin
    fl_addto_menu(fd^.menu,'One'#8'F1|Two'#8'F2|Three'#8'F3|Four'#8'F4');
    fl_addto_choice(fd^.choice,'Option1|Option2|Option3');

    fl_set_pixmapbutton_data(fd^.pixbutton,porsche);
    fl_set_pixmap_data(fd^.pix, porsche);
    fl_set_bitmapbutton_data(fd^.bitbutton, bm1_width, bm1_height,pchar(@bm1_bits));
    fl_set_bitmap_data(fd^.bit, nomail_width, nomail_height, pchar(@nomail_bits));

    fl_set_browser_fontsize(fd^.browser,FL_NORMAL_SIZE);
    fl_addto_browser(fd^.browser,'browser line 1'#10'browser line 2');
    fl_addto_browser(fd^.browser,'browser line 3'#10'browser line 4');
    fl_addto_browser(fd^.browser,'browser line 5'#10'browser line 6');
    fl_addto_browser(fd^.browser,'browser line 7'#10'browser line 8');
    fl_addto_browser(fd^.browser,'browser line 9'#10'browser line 10');
    fl_addto_browser(fd^.browser,'browser line 11'#10'browser line 12');
    fl_addto_browser(fd^.browser,'browser line 13'#10'browser line 14');
    fl_addto_browser(fd^.browser,'browser line 15'#10'browser line 16');
    fl_addto_browser(fd^.browser,'browser line 17'#10'browser line 18');

    for i := 0 to 6 do
      fl_add_chart_value(fd^.chart,y[i],labels[i],i+1);

    fl_set_xyplot_data(fd^.xyplot,x,y,6,'','','');
    fl_add_xyplot_overlay(fd^.xyplot,1, x, y, 6, FL_RED);
    fl_add_xyplot_text(fd^.xyplot,2.5,5.2,'Weekly Summary',
                       FL_ALIGN_CENTER,FL_BLUE);
    fl_add_xyplot_text(fd^.xyplot, 3, 3.85, '@-22^.', FL_ALIGN_TOP, FL_RED);

    fl_set_xyplot_overlay_type(fd^.xyplot,1,FL_NORMAL_XYPLOT);
    fl_set_xyplot_alphaxtics(fd^.xyplot,'Mon|Tue|Wed|Thu|Fri|Sat', nil);
    fl_set_xyplot_ytics(fd^.xyplot,-1,-1);
    fl_set_xyplot_linewidth(fd^.xyplot,0, 3);
end;

function create_form_objsform : PFD_objsform;
var
  obj : PFL_OBJECT;
  fdui : PFD_objsform;
  old_bw : Longint;

begin
  new(fdui);
  old_bw := fl_get_border_width;

  fl_set_border_width(-3);
  fdui^.objsform := fl_bgn_form(FL_NO_BOX, 456, 361);
  obj := fl_add_box(FL_FLAT_BOX,0,0,456,361,'');
  obj := fl_add_box(FL_UP_BOX,0,0,455,360,'');
  obj := fl_add_box(FL_UP_BOX,0,0,456,70,'');
  obj := fl_add_button(FL_RADIO_BUTTON,15,20,75,30,'Static');
    fdui^.button[0] := obj;
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_TIMESBOLD_STYLE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@show_group),0);
  obj := fl_add_button(FL_RADIO_BUTTON,90,20,75,30,'Button');
    fdui^.button[1] := obj;
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_TIMESBOLD_STYLE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@show_group),1);
  obj := fl_add_button(FL_RADIO_BUTTON,165,20,70,30,'Valuator');
    fdui^.button[2] := obj;
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_TIMESBOLD_STYLE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@show_group),2);
  obj := fl_add_button(FL_RADIO_BUTTON,235,20,70,30,'Input');
    fdui^.button[3] := obj;
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_TIMESBOLD_STYLE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@show_group),3);
  obj := fl_add_button(FL_RADIO_BUTTON,305,20,70,30,'Other');
    fdui^.button[4] := obj;
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_TIMESBOLD_STYLE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@show_group),4);
  obj := fl_add_box(FL_UP_BOX,0,70,456,291,'');
  obj := fl_add_box(FL_DOWN_BOX,9,90,435,260,'');

  fdui^.group[2] := fl_bgn_group();
  obj := fl_add_text(FL_NORMAL_TEXT,260,140,80,30,'Text');
    fl_set_object_lalign(obj,FL_ALIGN_LEFT or FL_ALIGN_INSIDE);
  obj := fl_add_slider(FL_HOR_BROWSER_SLIDER,60,120,170,25,'');
  obj := fl_add_slider(FL_HOR_FILL_SLIDER,60,160,170,30,'');
    fl_set_slider_value(obj, 0.54);
  obj := fl_add_slider(FL_VERT_SLIDER,390,110,30,170,'');
    fl_set_slider_value(obj, 0.48);
  obj := fl_add_valslider(FL_VERT_SLIDER,350,110,30,170,'');
  obj := fl_add_dial(FL_FILL_DIAL,50,220,90,70,'');
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
  obj := fl_add_positioner(FL_NORMAL_POSITIONER,150,210,120,100,'');
  obj := fl_add_counter(FL_NORMAL_COUNTER,300,300,130,30,'');
  fl_end_group();


  fdui^.group[1] := fl_bgn_group();
  obj := fl_add_bitmapbutton(FL_NORMAL_BUTTON,60,250,50,40,'');
    fdui^.bitbutton := obj;
  obj := fl_add_pixmapbutton(FL_NORMAL_BUTTON,85,120,80,80,'');
    fdui^.pixbutton := obj;
  obj := fl_add_button(FL_NORMAL_BUTTON,290,270,90,35,'Button');
  obj := fl_add_round3dbutton(FL_PUSH_BUTTON,260,95,60,40,'Round3DButton');
    fl_set_object_color(obj, FL_COL1, FL_MAGENTA);
  obj := fl_add_roundbutton(FL_PUSH_BUTTON,220,140,60,40,'RoundButton');
  obj := fl_add_checkbutton(FL_PUSH_BUTTON,190,230,50,40,'CheckButton');
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
  obj := fl_add_lightbutton(FL_PUSH_BUTTON,290,200,100,30,'LightButton');
  obj := fl_add_button(FL_PUSH_BUTTON, 120, 290, 100, 35 ,'Button');
   fl_set_object_boxtype(obj, FL_ROUNDED3D_UPBOX);
  fl_end_group();


  fdui^.group[0] := fl_bgn_group();
  obj := fl_add_box(FL_UP_BOX,40,120,70,70,'A Box');
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_TIMESBOLD_STYLE);
  obj := fl_add_bitmap(FL_NORMAL_BITMAP,30,220,80,70,'bitmap');
    fdui^.bit := obj;
  obj := fl_add_clock(FL_ANALOG_CLOCK,330,240,90,80,'');
  obj := fl_add_frame(FL_ENGRAVED_FRAME,130,120,80,70,'A Frame');
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_lstyle(obj,FL_TIMESBOLD_STYLE);
  obj := fl_add_pixmap(FL_NORMAL_PIXMAP,340,110,90,70,'pixmap');
    fdui^.pix := obj;
  obj := fl_add_chart(FL_PIE_CHART,160,210,130,110,'chart');
    fdui^.chart := obj;
  obj := fl_add_text(FL_NORMAL_TEXT,240,130,100,30,'Text stuff\nand more stuff');
    fl_set_object_lalign(obj,FL_ALIGN_CENTER or FL_ALIGN_INSIDE);
  fl_end_group();


  fdui^.group[3] := fl_bgn_group();
  obj := fl_add_input(FL_NORMAL_INPUT,140,120,150,30,'Input');
  obj := fl_add_input(FL_MULTILINE_INPUT,60,170,320,130,'');
  fl_end_group();

  obj := fl_add_button(FL_NORMAL_BUTTON,385,20,60,30,'Quit');
    fdui^.quit := obj;

  fdui^.group[4] := fl_bgn_group();
  obj := fl_add_menu(FL_PULLDOWN_MENU,190,110,40,19,'Menu');
    fdui^.menu := obj;
    fl_set_object_boxtype(obj,FL_FLAT_BOX);
  obj := fl_add_choice(FL_NORMAL_CHOICE,290,110,120,30,'');
    fdui^.choice := obj;
  obj := fl_add_browser(FL_NORMAL_BROWSER,30,140,140,150,'');
    fdui^.browser := obj;
  obj := fl_add_xyplot(FL_IMPULSE_XYPLOT,190,150,240,180,'');
    fdui^.xyplot := obj;
    fl_set_object_lsize(obj,FL_DEFAULT_SIZE);
  fl_end_group();

  fl_end_form();
  fdui^.objsform^.fdui:=fdui;
  fl_set_border_width(old_bw);

  create_form_objsform := fdui;
end;

begin
   fl_initialize(@argc, argv, '', nil, 0);
   fd_objsform := create_form_objsform;
   init_gui(fd_objsform);

   { fill-in form initialization code }
   fl_set_button(fd_objsform^.button[0], 1);
   show_group(nil, 0);

   { show the first form }
   fl_show_form(fd_objsform^.objsform,FL_PLACE_CENTER,FL_FULLBORDER,'objsform');
   while (fl_do_forms() <> fd_objsform^.quit) do
     begin
     end;
end.
