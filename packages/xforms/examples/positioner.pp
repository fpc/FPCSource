{ This demo shows the use of a positioner.  }
program positioner;

uses xforms;

var
 xval, yval : PFL_Object;

{ callback routine }

Procedure Do_position (ob :PFL_OBJECT; q : longint);

var
    st : string [30];

begin

    str(fl_get_positioner_xvalue(ob),st);st:=st+#0;
    fl_set_object_label(xval,@st[1]);
    str(fl_get_positioner_yvalue(ob),st);st:=st+#0;
    fl_set_object_label(yval,@st[1]);
end;

procedure positioner_cb(ob :PFL_OBJECT; q : longint);cdecl;

begin
  Do_Position (ob,q);
end;

var
  form : PFL_FORM;
  thepos, button : PFL_OBJECT;


begin

  fl_initialize(@argc, argv, 'FormDemo', nil, 0);

  form := fl_bgn_form(FL_UP_BOX,400,280);
     thepos := fl_add_positioner(FL_NORMAL_POSITIONER,40,40,200,200,'');
     fl_set_positioner_xbounds(thepos,0,1);
     fl_set_positioner_ybounds(thepos,0,1);
     fl_set_object_callback(thepos,PFL_CALLBACKPTR(@positioner_cb),0);
     xval := fl_add_box(FL_DOWN_BOX,270,40,100,30,'');
     yval := fl_add_box(FL_DOWN_BOX,270,90,100,30,'');
     fl_set_object_color(xval,FL_COL1,FL_COL1);
     fl_set_object_color(yval,FL_COL1,FL_COL1);
     button := fl_add_button(FL_NORMAL_BUTTON,270,210,100,30,'Exit');
  fl_end_form();

  fl_show_form(form,FL_PLACE_CENTER,FL_NOBORDER,NiL);
  Do_position(thepos,0);
  fl_do_forms;
  fl_hide_form(form);
end.
