program objpos;

uses forms;

const
  dx : TFL_Coord = 11 ;
  dy : TFL_Coord = 7 ;

procedure move_cb(ob : PFL_OBJECT; data : longint);export;

var
   but : PFL_OBJECT;
   x,y,w,h : TFL_COORD; 

begin
   but := PFL_OBJECT (data);
   fl_get_object_geometry(but,@x,@y, @w, @h);

   if ( (x + dx) < 0) or ( (x+w+dx) >= but^.form^.w) then
       dx := -dx;
   if ( (y + dy) < 0) or ( (y+h+dy) >= but^.form^.h) then
       dy := -dy;
    x := x+dx;
    y := y+dy;

    fl_set_object_position(but,x,y);
end;

var
  form : PFL_FORM;
  but, obj : PFL_OBJECT ;

begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);

  form := fl_bgn_form(FL_DOWN_BOX,400,200);
    but := fl_add_button(FL_NORMAL_BUTTON,140,160,70,35,'Exit');
    obj := fl_add_button(FL_TOUCH_BUTTON,330,150,50,30,'Move');
     fl_set_object_callback(obj,PFL_CALLBACKPTR(@move_cb),longint(but));
  fl_end_form();

  fl_show_form(form,FL_PLACE_MOUSE,FL_NOBORDER,'ObjPos');

  fl_do_forms();

end.  $Log$
end.  Revision 1.1  2002-01-29 17:55:01  peter
end.    * splitted to base and extra
end.
end.  Revision 1.2  2000/07/13 11:33:14  michael
end.  + removed logs
end. 
}
