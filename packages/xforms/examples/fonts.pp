{* Demo, showing ther different fonts in the different sizes. *}
program fonts;

uses xforms;

type
TFD_fontsform = record
        fontsform : PFL_FORM;
        fontobj,
        sizeobj,
        textobj : PFL_OBJECT;
        vdata : pointer;
        ldata : longint;
end;
PFD_fontsform=^TFD_fontsform;

var ui : PFD_fontsform;

procedure done_cb(obj : PFL_OBJECT; arg : longint);cdecl;
begin
    halt(0);
end;

procedure style_cb(obj : PFL_OBJECT; arg : Longint);cdecl;
begin
  fl_set_object_lstyle(ui^.textobj, fl_get_browser(obj) - 1);
end;

procedure size_cb(obj : PFL_OBJECT; arg : longint); cdecl;
begin
  case fl_get_browser(obj) of
     1: fl_set_object_lsize(ui^.textobj,8);
     2: fl_set_object_lsize(ui^.textobj,10);
     3: fl_set_object_lsize(ui^.textobj,11);
     4: fl_set_object_lsize(ui^.textobj,12);
     5: fl_set_object_lsize(ui^.textobj,13);
     6: fl_set_object_lsize(ui^.textobj,14);
     7: fl_set_object_lsize(ui^.textobj,18);
     8: fl_set_object_lsize(ui^.textobj,24);
     9: fl_set_object_lsize(ui^.textobj,30);
  end;
end;

procedure addit(st : pchar);
begin
   fl_add_browser_line(ui^.fontobj,st);
end;


function create_form_fontsform : PFD_fontsform;
var
  obj : PFL_OBJECT ;
  fdui : PFD_fontsform;

begin
  new(fdui);

  fdui^.fontsform := fl_bgn_form(FL_NO_BOX, 371, 296);
  obj := fl_add_box(FL_FLAT_BOX,0,0,371,296,'');
    fl_set_object_color(obj,FL_SLATEBLUE,FL_COL1);
  obj := fl_add_browser(FL_HOLD_BROWSER,10,145,195,135,'');
  fdui^.fontobj := obj;
    fl_set_object_lalign(obj,FL_ALIGN_BOTTOM or FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@style_cb),0);
  obj := fl_add_browser(FL_HOLD_BROWSER,215,145,145,135,'');
  fdui^.sizeobj := obj ;
    fl_set_object_lalign(obj,FL_ALIGN_BOTTOM or FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@size_cb),0);
  obj:= fl_add_text(FL_NORMAL_TEXT,10,5,351,125,'The quick brown'#10'fox jumps over'#10'the lazy dog.');
  fdui^.textobj := obj ;
    fl_set_object_boxtype(obj, FL_FRAME_BOX);
    fl_set_object_lalign(obj, FL_ALIGN_CENTER);
  obj := fl_add_button(FL_HIDDEN_BUTTON,0,0,370,140,'Button');
    fl_set_button_shortcut(obj,'^[qQ',1);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@done_cb),0);
  fl_end_form();

  create_form_fontsform:= fdui;
end;

begin
  fl_set_border_width(-3);

  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  ui := create_form_fontsform();
  fl_scale_form(ui^.fontsform, 1.1,1.2);
  fl_set_object_dblbuffer(ui^.textobj,1);
  fl_set_object_bw(ui^.textobj,5);

  fl_enumerate_fonts(PFL_ENUMERATEPTR(@addit), 1);
  fl_select_browser_line(ui^.fontobj,1);
  fl_addto_browser(ui^.sizeobj,'8  (tiny)');
  fl_addto_browser(ui^.sizeobj,'10 (small)');
  fl_addto_browser(ui^.sizeobj,'11 (scaled)');
  fl_addto_browser(ui^.sizeobj,'12 (normal)');
  fl_addto_browser(ui^.sizeobj,'13 (scaled)');
  fl_addto_browser(ui^.sizeobj,'14 (medium)');
  fl_addto_browser(ui^.sizeobj,'18 (large)');
  fl_addto_browser(ui^.sizeobj,'24 (Huge)');
  fl_addto_browser(ui^.sizeobj,'30 (scaled)');
  fl_select_browser_line(ui^.sizeobj,2);
  fl_set_object_lstyle(ui^.textobj,FL_NORMAL_STYLE);
  fl_call_object_callback(ui^.fontobj);
  fl_call_object_callback(ui^.sizeobj);
  fl_show_form(ui^.fontsform,FL_PLACE_CENTER,FL_TRANSIENT,'Fonts');
  fl_do_forms();
end.
