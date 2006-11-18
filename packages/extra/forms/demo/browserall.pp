{ This is a demo that shows the different types of browsers.  }


uses xforms,strings;

var
form : PFL_FORM;
br : array[0..3] of PFL_OBJECT;
exitobj, readout : PFL_OBJECT;


const
bnames : array[0..3] of pchar =
(
    'NORMAL_BROWSER', 'SELECT_BROWSER', 'HOLD_BROWSER', 'MULTI_BROWSER'
);


procedure deselect(obj  : PFL_OBJECT ; arg : longint);cdecl;
var
  i : longint ;
begin
  for i:=0 to 3 do
     fl_deselect_browser(br[i]);
end;

procedure set_size(obj : PFL_OBJECT; arg : longint);cdecl;
var
  i : longint;
begin
  for i:=0 to 3 do
     fl_set_browser_fontsize(br[i],arg);
end;

procedure set_style(obj : PFL_OBJECT; arg :  longint);cdecl;
var
  i : longint;

begin
  for i:=0 to 3 do
     fl_set_browser_fontstyle(br[i], arg);
end;

procedure br_callback(ob : PFL_OBJECT; arg : longint);cdecl;
var
    buf : string[255];
const
    mb : array[0..3] of pchar = ('','left','middle','right');

var i,b : longint;

begin
    b:=fl_mouse_button;
    if (b < FL_SHORTCUT) then
      buf :='In '+strpas(bnames[arg]) +'['+strpas(mb[b])+']'#0
    else
      buf:='In '+strpas(bnames[arg])+#0;
    i := fl_get_browser(ob);
    if i>0 then
      begin
      buf:=buf+strpas(fl_get_browser_line(ob,i));
      buf:=buf+' was selected'#0
      end
    else
      begin
      buf:=buf+strpas(fl_get_browser_line(ob,-i));
      buf:=buf+' was deselected'#0
      end;
    fl_set_object_label(readout,@buf[1]);
end;


procedure create_form;
var
  obj : PFL_OBJECT;

begin
  form := fl_bgn_form(FL_UP_BOX,700,570);
  readout := fl_add_text(FL_NORMAL_TEXT,50,30,600,50,'');
  fl_set_object_lsize(readout,FL_LARGE_SIZE);
  fl_set_object_lalign(readout,FL_ALIGN_CENTER);
  fl_set_object_lstyle(readout,FL_BOLD_STYLE);
  fl_set_object_boxtype(readout,FL_UP_BOX);

  fl_set_object_color(readout,FL_MAGENTA,FL_MAGENTA);

  obj:= fl_add_browser(FL_NORMAL_BROWSER,20,120,150,290,bnames[0]);
  br[0] := obj ;
    fl_set_object_callback(obj, PFL_CALLBACKPTR(@br_callback), 0);
  obj:= fl_add_browser(FL_SELECT_BROWSER,190,120,150,290,bnames[1]);
  br[1] := obj ;
    fl_set_object_callback(obj, PFL_CALLBACKPTR(@br_callback), 1);
  obj := fl_add_browser(FL_HOLD_BROWSER,360,120,150,290,bnames[2]);
  br[2] := obj ;
    fl_set_object_color(obj,FL_COL1,FL_GREEN);
    fl_set_object_callback(obj, PFL_CALLBACKPTR(@br_callback), 2);
  obj := fl_add_browser(FL_MULTI_BROWSER,530,120,150,290,bnames[3]);
  br[3] := obj;
    fl_set_object_color(br[3],FL_COL1,FL_CYAN);
    fl_set_object_callback(obj, PFL_CALLBACKPTR(@br_callback), 3);

  obj  := fl_add_button(FL_NORMAL_BUTTON,560,510,120,30,'Exit');
  exitobj := obj;
     obj := fl_add_button(FL_NORMAL_BUTTON,560,460,120,30,'Deselect');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@deselect),0);

  fl_bgn_group();
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,20,500,100,30,'Tiny');
    fl_set_object_lsize(obj,FL_TINY_SIZE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_size),obj^.lsize);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,130,500,100,30,'Small');
    fl_set_object_lsize(obj,FL_SMALL_SIZE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_size),obj^.lsize);
    fl_set_button(obj,1);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,240,500,100,30,'Normal');
    fl_set_object_lsize(obj,FL_NORMAL_SIZE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_size),obj^.lsize);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,350,500,100,30,'Large');
    fl_set_object_lsize(obj,FL_LARGE_SIZE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_size),obj^.lsize);
  fl_end_group;

  fl_bgn_group;
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,20,450,100,30,'Normal');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_style),FL_NORMAL_STYLE);
    fl_set_button(obj,1);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,120,450,100,30,'Bold');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_style),FL_BOLD_STYLE);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,220,450,100,30,'Italic');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_style),FL_ITALIC_STYLE);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,320,450,100,30,'BoldItalic');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_style),FL_BOLDITALIC_STYLE);
  obj := fl_add_lightbutton(FL_RADIO_BUTTON,420,450,100,30,'Fixed');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_style),FL_FIXED_STYLE);
  fl_end_group;
  fl_end_form;
end;


procedure fill_browsers;
var
  i, j : longint;
  buf : string[128];

begin
  for i:=0 to 3 do
    for j:=1 to 100 do
      begin
      if ( j = 5) then
        begin
        str(j,buf);
        buf:='@NLine with qb '+buf+#0;
        end
      else if ( j = 10) then
        buf:='@-'#0
      else if ( j = 40 ) then
        begin
        str(j,buf);
        buf:='@mLine with qb '+buf+#0;
        end
      else
        begin
        str(j,buf);
        buf:='Line with qb '+buf+#0;
        end;
      fl_add_browser_line(br[i],@buf[1]);
      end
end;


begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  create_form;
  fill_browsers;
  fl_show_form(form,FL_PLACE_CENTER,FL_TRANSIENT,'All Browsers');
  fl_do_forms;
  fl_hide_form(form);
end.
