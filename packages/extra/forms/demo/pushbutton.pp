Program pushButtons;
{ A demo that shows the use of push buttons.  }

uses xforms;

var form : PFL_FORM;
    Abox : array[0..8] of PFL_OBJECT;

procedure push_cb( ob : PFL_OBJECT; n : longint);cdecl;

begin
   if (fl_get_button(ob)<>0) then
      fl_show_object(abox[n])
   else
      fl_hide_object(abox[n]);
end;


Procedure makeform;

Var i : Integer;
    obj : PFL_OBJECT;

begin
  form := fl_bgn_form(FL_UP_BOX,400,400);
  for i:=0 to 7 do
    begin
    obj := fl_add_button(FL_PUSH_BUTTON,40,310-40*i,80,30,'');
    fl_set_object_color(obj,FL_BLACK+i+1,FL_BLACK+i+1);
      fl_set_object_callback(obj,PFL_CALLBACKPTR(@push_cb),i);
    abox[i] := fl_add_box(FL_DOWN_BOX,150+30*i,40,25,320,'');
      fl_set_object_color(abox[i],FL_BLACK+i+1,FL_BLACK+i+1);
      fl_hide_object(abox[i]);
    end;
  fl_add_button(FL_NORMAL_BUTTON,40,350,80,30,'Exit');
  fl_end_form;
end;

Begin
  fl_initialize(@argc, argv, 'FormDemo', nil,0);
  makeform;
  fl_show_form(form,FL_PLACE_CENTER,FL_NOBORDER,'Push Buttons');
  { fl_do_forms will return only when Exit is pressed }
  fl_do_forms;
end.
