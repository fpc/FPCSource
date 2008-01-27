{ This demo is meant to demonstrate the use of a free
   object in a form.
}
program free1;

uses xforms;

Const
  onn : boolean = True;
  dcol : longint = 1;

var
  cole : TFL_COLOR;
  form : PFL_FORM;
  obj : PFL_OBJECT;
  i, j, depth, col : Longint;
  dummy : cardinal;


{ The call back routine }
function handle_it(obj : PFL_OBJECT; event : longint;
                   mx,my : TFL_Coord;
                   key : longint; ev : pointer) : longint;cdecl;

begin
  exit;
  case event of
    FL_DRAW:
        fl_rect(obj^.x,obj^.y,obj^.w,obj^.h, obj^.u_ldata);
    FL_RELEASE:
        onn := not(onn);
    FL_STEP:
        if (onn) then
          begin
          if (obj^.u_ldata = cole) then
              dcol := -1;
          if (obj^.u_ldata = FL_FREE_COL1) then
              dcol := 1;
          obj^.u_ldata := dcol;
          fl_redraw_object(obj);
          end;
  end;
  handle_it:=0;
end;

procedure done(ob : PFL_OBJECT; data : longint); cdecl;

begin
 halt(0)
end;

begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  form := fl_bgn_form(FL_UP_BOX,400,400);
  obj := fl_add_button(FL_NORMAL_BUTTON,320,20,40,30,'Exit');
  fl_set_object_callback(obj, PFL_CALLBACKPTR(@done), 0);
  obj := fl_add_free(FL_CONTINUOUS_FREE,40,80,320,280
                     ,'',PFL_HANDLEPTR(@handle_it));
  fl_end_form();
  depth  := fl_get_visual_depth;
  { can't do it if less than 4 bit deep }
  if depth=8 then writeln ('depth of 8');
  if (depth < 4) then
    begin
    writeln ('This Demo requires a depth of at least 4 bits');
    halt(1);
  end;
  cole := ((1 shl depth)-1);
  if (cole > 64) then
     cole := 64;
  col := FL_FREE_COL1;
{  obj^.u_ldata := col;
}  cole := cole+col;
  i:=col;
  while i<=cole do
   begin
     j := round(255 * (i - col) /(cole  - col));
     dummy:=fl_mapcolor(i, j, j, j);
     inc(i);
   end;

  fl_show_form(form,FL_PLACE_CENTER,FL_NOBORDER,'Free Object');
  fl_do_forms();
end.
