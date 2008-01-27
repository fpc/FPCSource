{ This demo shows the use of a browser and fl_call_object_callback.  }
program fbrowse1;

uses xforms;

var
  form : PFL_FORM;
  br, but : PFL_OBJECT;

procedure load_file(ob : PFL_OBJECT; arg : longint);cdecl;

begin
  if (fl_load_browser(br,fl_show_input('Filename to load',''))<>0) then
     fl_add_browser_line(br,'NO SUCH FILE!');
end;

procedure set_size(ob : PFL_OBJECT; arg : Longint);cdecl;

begin
  fl_set_browser_fontsize(br, arg);
end;


var obj : PFL_OBJECT;

begin
    fl_initialize(@argc, argv, 'FormDemo', nil, 0);

    form := fl_bgn_form(FL_UP_BOX,130,100);
     br := fl_add_browser(FL_NORMAL_BROWSER,5,5,95,90,'');
     but := fl_add_button(FL_NORMAL_BUTTON,105,5,20,8,'Exit');
     obj := fl_add_button(FL_NORMAL_BUTTON,105,75,20,8,'Load');
     fl_set_object_callback(obj,PFL_CALLBACKPTR(@load_file),0);
     obj := fl_add_lightbutton(FL_RADIO_BUTTON,105,60,20,8,'Small');
     fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_size),FL_SMALL_SIZE);
     fl_call_object_callback(obj);
     fl_set_button(obj,1);
     obj := fl_add_lightbutton(FL_RADIO_BUTTON,105,50,20,8,'Normal');
     fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_size),FL_NORMAL_SIZE);
     obj := fl_add_lightbutton(FL_RADIO_BUTTON,105,40,20,8,'Large');
     fl_set_object_callback(obj,PFL_CALLBACKPTR(@set_size),FL_LARGE_SIZE);
    fl_end_form();

  fl_clear_browser(br);
  fl_add_browser_line(br,'LOAD A FILE.');
  fl_scale_form(form, 4.0, 4.0);

  fl_show_form(form,FL_PLACE_FREE,FL_FULLBORDER,'Browser');

  repeat
    obj := fl_do_forms;
  until obj = but;
  fl_hide_form(form);
end.
