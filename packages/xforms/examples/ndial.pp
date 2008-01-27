{ This is an example of the use of dials.  }

uses xforms;

var
form : PFL_FORM;
button, red, green, blue, redtext, greentext, bluetext, theresult : PFL_OBJECT;

procedure  makeform;

begin
  form := fl_bgn_form(FL_UP_BOX,300,330);
    button := fl_add_button(FL_NORMAL_BUTTON,45,15,210,45,'A Color Editor');
    fl_set_object_lsize(button,FL_LARGE_SIZE);

    red := fl_add_dial(FL_NORMAL_DIAL,30,240,60,60,'Red');
    fl_set_dial_bounds(red,0.0,255.0);
    fl_set_dial_value(red,128.0);
    fl_set_object_color(red,FL_RED,FL_DIAL_COL2);
    redtext := fl_add_box(FL_DOWN_BOX,105,255,50,25,'');

    green := fl_add_dial(FL_NORMAL_DIAL,30,155,60,60,'Green');
    fl_set_dial_bounds(green,0.0,255.0);
    fl_set_dial_value(green,128.0);
    fl_set_object_color(green,FL_GREEN,FL_DIAL_COL2);
    greentext := fl_add_box(FL_DOWN_BOX,105,170,50,25,'');

    blue := fl_add_dial(FL_NORMAL_DIAL,30,70,60,60,'Blue');
    fl_set_dial_bounds(blue,0.0,255.0);
    fl_set_dial_value(blue,128);
    fl_set_object_color(blue,FL_BLUE,FL_DIAL_COL2);
    bluetext := fl_add_box(FL_DOWN_BOX,105,90,50,25,'');

    theresult := fl_add_box(FL_DOWN_BOX,180,70,90,245,'');
    fl_set_object_color(theresult,FL_FREE_COL1,FL_FREE_COL1);
    fl_set_object_dblbuffer(theresult, 1);
  fl_end_form;
end;


var
  ret : PFL_OBJECT;
  r,g,b : Longint;
  st : string[100];

begin

  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  makeform;
  fl_show_form(form,FL_PLACE_MOUSE,FL_TRANSIENT,'A Form');
  repeat
    r := round( fl_get_dial_value(red));
    g := round(fl_get_dial_value(green));
    b := round(fl_get_dial_value(blue));
    fl_mapcolor(FL_FREE_COL1,r,g,b);
    fl_redraw_object(theresult);
    str(r,st);st:=st+#0; fl_set_object_label(redtext,@st[1]);
    str(g,st);st:=st+#0; fl_set_object_label(greentext,@st[1]);
    str(b,st);st:=st+#0; fl_set_object_label(bluetext,@st[1]);
    ret := fl_do_forms;
  until (ret = button);
  fl_hide_form(form);
end.
