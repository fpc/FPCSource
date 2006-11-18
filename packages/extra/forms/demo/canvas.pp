{ Demo showing the use of canvas object.   V0.75
 }

uses xforms,x,xlib;

{*** Forms and Objects ***}

type TFD_canvasform = record
        canvasform : PFL_FORM;
        vdata : pointer;
        ldata : longint;
        canvas,
        br,
        keyboard,
        mouse,
        done,
        misc,
        menu : PFL_OBJECT;
end;

PFD_canvasform = ^TFD_canvasform;


Var canvasGC : TGC;


function canvas_expose(ob : PFL_OBJECT;win : TWindow; w,h : longint; ev : PXEvent;d : Pointer) : longint;cdecl;

var ui : PFD_CANVASFORM;

begin
    ui := d;
    XFillRectangle(PDisplay(fl_display), TDRAWABLE(win), canvasGC, 0, 0,cardinal( w), cardinal(h));
    fl_addto_browser(ui^.br, 'Expose');
end;


function canvas_key(ob : PFL_OBJECT; win : TWindow; w,h : Longint; ev : PXEvent; d: pointer) : longint;cdecl;
var
   ui :PFD_canvasform;
   buf : string[128];


begin
    ui := d;
    str (XKeycodeToKeysym(PDisplay(fl_display),ev^.xkey.keycode, TkeyCode(#0)),buf);
    buf:= 'KeyPress: keysym='+buf+#0;
    fl_addto_browser(ui^.br, @buf[1]);
    return 0;
end;


function canvas_but(ob : PFL_OBJECT; win : TWindow; w,h : longint, ev : PXEvent;d : pointer) : longint;cdecl;

var
    ui : PFD_canvasform;
    buf : string[128];

begin
  ui := d;

  str(ev^.xbutton.button,buf);
  if ev^.eventtype=ButtonPress then
    buf:='Press '+buf
  else
    buf:='Release '+buf;
  buf:='Button '+buf+#0;
    fl_addto_browser(ui^.br, @buf[1]);
end;

function canvas_misc(ob : PFL_OBJECT; win : TWindow; w,h : longint, ev : PXEvent;d : pointer) : longint;cdecl;

var

   ui : PFD_canvasform;
begin
  ui := d;
  if ev^.xcrossing.eventtype=EnterNotify then
  fl_addto_browser(ui^.br, 'Enter Canvas')
  else
  fl_addto_browser(ui^.br, 'Leave Canvas')
end;

procedure init_canvas(FD_canvasform *fdui)

begin
   fl_add_canvas_handler(fdui^.canvas, Expose, PFL_HANDLE_CANVAS(@canvas_expose), fdui);
   fl_add_canvas_handler(fdui^.canvas, KeyPress, PFL_HANDLE_CANVAS(@canvas_key), fdui);
   fl_add_canvas_handler(fdui^.canvas, ButtonPress,PFL_HANDLE_CANVAS(@canvas_but),fdui);
   fl_add_canvas_handler(fdui^.canvas, ButtonRelease,PFL_HANDLE_CANVAS(canvas_but),fdui);
   fl_set_button(fdui^.mouse, 1);
   fl_set_button(fdui^.keyboard, 1);
   canvasGC := XCreateGC(fl_get_display(),fl_state[fl_vmode].trailblazer,0,0);
   XSetForeground(fl_get_display(), canvasGC, fl_get_flcolor(FL_BLACK));
end;

var
fd_canvasform : PFD_canvasform;

procedure sensitive_setting(ob : PFL_OBJECT ; event : longint); cdecl;

var
    hc : PFL_HANDLE_CANVAS;
begin
 if event=Keypress then
   hc := PFL_HANDLE_CANVAS(@canvas_key)
 else
  hc := PFL_HANDLE_CANVAS(@canvas_but);

    if(fl_get_button(ob)<>0) then
       fl_add_canvas_handler(fd_canvasform^.canvas, event, hc, fd_canvasform);
    else
       fl_remove_canvas_handler(fd_canvasform^.canvas, event, hc);
end;

procedure disable_it(ob : PFL_OBJECT; data : longint);cdecl;

begin
    if fl_get_button(ob)<>0 then
      fl_deactivate_object(fd_canvasform^.canvas)
    else
      fl_activate_object (fd_canvasform^.canvas);
end;

procedure hide_it(ob : PFL_OBJECT; all : longint);cdecl;


begin
      if(all<>0) then
        begin
         fl_hide_form(fd_canvasform^.canvasform);
         fl_show_form(fd_canvasform^.canvasform,
                      FL_PLACE_CENTER, FL_TRANSIENT, 'canvas');
{$ifdef nevertrue}
       fl_remove_selected_xevent(fd_canvasform^.canvasform^.window,
            OwnerGrabButtonMask);
{$endif }
        end
      else
       begin
         if(fd_canvasform^.canvas^.visible)<>0 then
           begin
            fl_hide_object(fd_canvasform^.canvas);
            fl_set_object_label(ob,'ShowCanvas');
           end
         else
           begin
            fl_show_object(fd_canvasform^.canvas);
            fl_set_object_label(ob,'HideCanvas');
           end
        end;
end;

procedure misc_cb(ob : PFL_OBJECT; data : longint); cdecl;

begin
    if(fl_get_button(ob)<>0) then
      begin
       fl_add_canvas_handler(fd_canvasform^.canvas, EnterNotify,
                             PFL_HANDLE_CANVAS(@canvas_misc), fd_canvasform);
       fl_add_canvas_handler(fd_canvasform^.canvas, LeaveNotify,
                             PFL_HANDLE_CANVAS(@canvas_misc), fd_canvasform);
      end
    else
      begin
       fl_remove_canvas_handler(fd_canvasform^.canvas,
                                EnterNotify, PFL_HANDLE_CANVAS(@canvas_misc));
       fl_remove_canvas_handler(fd_canvasform^.canvas,
                                LeaveNotify, PFL_HANDLE_CANVAS(@canvas_misc));
      end;
end;

{*****************}
function create_form_canvasform : PFD_canvasform;
var
  obj :=PFL_OBJECT;
  fdui : PFD_canvasform;

begin
  new(fdui);

  fdui^.canvasform := fl_bgn_form(FL_NO_BOX, 450, 280);
  obj := fl_add_box(FL_UP_BOX,0,0,450,280,'');
  obj:= fl_add_canvas(FL_NORMAL_CANVAS,20,40,155,187,'');
  fdui^.canvas := obj ;
  obj := fl_add_browser(FL_NORMAL_BROWSER,188,40,152,187,'');
  fdui^.br := obj ;
    obj := fl_add_button(FL_PUSH_BUTTON,30,236,90,27,'Deactivate');
    fl_set_object_callback(obj,disable_it,0);
  obj := fl_add_button(FL_NORMAL_BUTTON,120,236,90,27,'HideCanvas');
    fl_set_object_callback(obj, hide_it, 0);
  obj := fl_add_button(FL_NORMAL_BUTTON,210,236,90,27,'HideForm');
    fl_set_object_callback(obj, hide_it, 1);
  obj := fl_add_button(FL_NORMAL_BUTTON,300,236,90,27,'Done');
  fdui^.done := obj ;
  obj := fl_add_text(FL_NORMAL_TEXT,130,10,120,20,'Canvas');
    fl_set_object_lsize(obj,FL_MEDIUM_SIZE);
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
    fl_set_object_lstyle(obj,FL_BOLD_STYLE);
  obj:= fl_add_menu(FL_PULLDOWN_MENU, 20,10, 45,22,'Menu');
  fdui^.menu := obj ;
    fl_set_object_shortcut(obj,'#m', 1);
    fl_set_object_lstyle(obj, FL_BOLD_STYLE);
  obj := fl_add_checkbutton(FL_PUSH_BUTTON,345,40,76,26,'Keyboard');
  fdui^.keyboard := obj ;
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
    fl_set_object_callback(obj,sensitive_setting,KeyPress);
  obj := fl_add_checkbutton(FL_PUSH_BUTTON,345,70,76,26,'Mouse');
    fdui^.mouse := obj ;
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
    fl_set_object_callback(obj,sensitive_setting,ButtonPress);
 obj := fl_add_checkbutton(FL_PUSH_BUTTON,345,100,74,26,'Enter/Leave');
    fdui^.misc := obj;
    fl_set_object_color(obj,FL_COL1,FL_BLUE);
    fl_set_object_callback(obj,misc_cb,ButtonPress);
  fl_end_form();

  create_form_canvasform :=fdui;
end;


begin
   fl_set_border_width(-2);

   fl_initialize(@argc, argv, 'FormDemo', nil, 0);
   fd_canvasform := create_form_canvasform;

   { fill-in form initialization code }
   fl_set_object_dblbuffer(fd_canvasform^.br, 1); { looks better }
   init_canvas (fd_canvasform);

   fl_addto_menu(fd_canvasform^.menu,'Item1|Item2|Item3|Item4');

   fl_show_form(fd_canvasform^.canvasform,
                FL_PLACE_FREE,FL_FULLBORDER,'canvasform');
{$ifdef nevertrue}
         fl_remove_selected_xevent(fd_canvasform^.canvasform^.window,
            OwnerGrabButtonMask);
{$endif}

   while (fl_do_forms() <> fd_canvasform^.done) do  ;
end.
