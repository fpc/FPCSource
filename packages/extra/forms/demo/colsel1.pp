program colsel1;

uses xforms;

var form : PFL_FORM;
    topbox : PFL_OBJECT;

procedure change_color(ob : PFL_OBJECT; col : longint);cdecl;

begin
  fl_set_object_color(topbox, col,  col);
end;

procedure  makeform;

var obj : PFL_OBJECT;
  i,j : Integer;
  st : string[32];

begin
  form := fl_bgn_form(FL_UP_BOX,100,100);
    for i:=0 to 7 do
      for j:=0 to 7 do
        begin
        str (8*j+i,st);
        st:=st+#0;
        obj := fl_add_button(FL_RADIO_BUTTON,11+10*i,15+10*j,8,6,@st[1]);
        fl_set_object_color(obj,8*j+i,8*j+i);
        fl_set_object_lalign(obj,FL_ALIGN_BOTTOM);
        fl_set_object_callback(obj,PFL_CALLBACKPTR(@change_color), (8*j+i));
        end;
    topbox := fl_add_button(FL_NORMAL_BUTTON,30,5,40,8,'The Color Map');
    fl_set_object_lsize(topbox,FL_LARGE_SIZE);
    fl_set_object_lstyle(topbox,FL_BOLD_STYLE);
  fl_end_form;
end;

var ret : PFL_OBJECT;

begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  makeform;
  fl_scale_form(form, 4.0,4.0);
  fl_show_form(form,FL_PLACE_FREE,FL_NOBORDER,NiL);
  while ret<>topbox do ret := fl_do_forms();
  fl_hide_form(form);
end.
