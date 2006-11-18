program ll;

uses xforms;

{*** Forms and Objects ***}

Type
TFD_axypform = record
        axypform : PFL_FORM;
        xyplot : PFL_OBJECT;
        status : PFL_OBJECT;
        vdata : pointer;
        ldata : longint;
end;
PFD_axypform = ^TFD_axypform;


var
xypui : PFD_axypform;

{ callbacks for form axypform }
Procedure xyplot_cb(ob : PFL_OBJECT; data : longint);cdecl;


var
    x, y : double;
    i : Longint;
    temp,buf : string[64];

begin
    fl_get_xyplot(ob, @x, @y, @i);
    if (i < 0) then exit;
    str (x,buf);
    str (y,temp);
    Buf:='X= '+Buf+' Y= '+temp+#0;
    fl_set_object_label(xypui^.status, @buf[1]);
end;


Procedure alwaysreturn_cb(ob : PFL_OBJECT; data : longint); cdecl;

begin
   fl_set_xyplot_return(xypui^.xyplot, fl_get_button(ob));
end;

procedure interpolate_cb(ob : PFL_OBJECT; data : longint);cdecl;

begin
   if fl_get_button(ob)=0 then
     fl_set_xyplot_interpolate(xypui^.xyplot, 0,2,0.2)
   else
     fl_set_xyplot_interpolate(xypui^.xyplot, 0,0,0.2);
end;

Procedure inspect_cb(ob : PFL_OBJECT; data :longint); cdecl;

begin
   fl_set_xyplot_inspect(xypui^.xyplot, fl_get_button(ob));
end;


procedure notic_cb(ob : PFL_OBJECT; data :longint); cdecl;

var notic : Longint;

begin
   notic := fl_get_button(ob);

   if (notic)<>0 then
     begin
     fl_set_xyplot_xtics(xypui^.xyplot, -1, -1);
     fl_set_xyplot_ytics(xypui^.xyplot, -1, -1);
     end
   else
     begin
     fl_set_xyplot_xtics(xypui^.xyplot, 0, 0);
     fl_set_xyplot_ytics(xypui^.xyplot, 0, 0);
     end;
end;


Function create_form_axypform : PFD_axypform;

var
  obj : PFL_OBJECT;
  fdui : PFD_axypform;

begin
  new(fdui);

  fdui^.axypform := fl_bgn_form(FL_NO_BOX, 431, 301);
  obj := fl_add_box(FL_UP_BOX,0,0,431,301,'');
  obj := fl_add_xyplot(FL_ACTIVE_XYPLOT,20,50,285,235,'');
  fdui^.xyplot :=  obj;
    fl_set_object_boxtype(obj,FL_DOWN_BOX);
    fl_set_object_color(obj, FL_BLACK, FL_GREEN);
    fl_set_object_lalign(obj,FL_ALIGN_BOTTOM or FL_ALIGN_INSIDE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@xyplot_cb),0);
  obj := fl_add_checkbutton(FL_PUSH_BUTTON,315,40,80,25,'AlwaysReturn');
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@alwaysreturn_cb),0);
  obj := fl_add_checkbutton(FL_PUSH_BUTTON,315,65,80,25,'Interpolate');
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@interpolate_cb),0);
  obj := fl_add_checkbutton(FL_PUSH_BUTTON,315,90,85,25,'InspectOnly');
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@inspect_cb),0);
  obj := fl_add_text(FL_NORMAL_TEXT,45,15,170,25,'');
  fdui^.status := obj;
    fl_set_object_boxtype(obj, FL_DOWN_BOX);
  obj := fl_add_button(FL_NORMAL_BUTTON,325,250,90,30,'Done');
  obj := fl_add_checkbutton(FL_PUSH_BUTTON,315,120,85,25,'NoTics');
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@notic_cb),0);
  fl_end_form();

  create_form_axypform:=fdui;
end;


var
   x,y : array[0..24] of double;
   i : longint;

begin
   fl_initialize(@argc, argv, 'FormDemo', nil, 0);
   xypui := create_form_axypform;

   { fill-in form initialization code }
   fl_set_object_dblbuffer(xypui^.status, 1);
   for i  := 0 to 10 do
      begin
      x[i] := i;
      y[i] := i;
      end;
   fl_set_xyplot_data(xypui^.xyplot, x, y, 11, '','','');

   { show the first form }
   fl_show_form(xypui^.axypform,FL_PLACE_MOUSE,FL_TRANSIENT,'axypform');
   fl_do_forms();
end.
