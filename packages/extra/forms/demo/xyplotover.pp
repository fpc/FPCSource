{ Demo showing the use of xyplot overlay. }
program xyplotover;

uses xforms;

{*** Forms and Objects ***}

type
TFD_fff = record
        fff : PFL_FORM;
        xyplot : PFL_OBJECT;
        vdata : pointer;
        ldata : longint;
end;
PFD_FFF = ^TFD_fff;

function create_form_fff : PFD_fff;

var
  obj : PFL_OBJECT;
  fdui : PFD_fff;

begin
  new(fdui);
  fdui^.fff := fl_bgn_form(FL_NO_BOX, 370, 310);
  obj := fl_add_box(FL_UP_BOX,0,0,370,310,'');
  obj  := fl_add_xyplot(FL_IMPULSE_XYPLOT,10,20,350,260,'');
  fdui^.xyplot := obj;
    fl_set_object_lalign(obj,(FL_ALIGN_BOTTOM or FL_ALIGN_INSIDE));
  obj := fl_add_button(FL_HIDDEN_BUTTON,10,10,350,290,'');
  fl_end_form;

  create_form_fff:=fdui;
end;


var
  i : integer;
  xx,yy : array[0..69] of real;
  fd_fff : PFD_fff;

begin
   fl_initialize(@argc, argv, 'FormDemo', nil, 0);

   { fill-in form initialization code }
   for i := 0 to 69 do
     begin
        xx[i] := 3.1415 * i / 8.0;
        yy[i] := sin(2 * xx[i]) + cos(xx[i]);
     end;

   fd_fff := create_form_fff;

   fl_set_xyplot_data(fd_fff^.xyplot, xx, yy, 35, '', '','');
   fl_add_xyplot_overlay(fd_fff^.xyplot, 1, xx, yy, 70, FL_BLUE);
   fl_set_xyplot_overlay_type(fd_fff^.xyplot, 1, FL_NORMAL_XYPLOT);
   fl_set_xyplot_xbounds(fd_fff^.xyplot, 0, 3.142 * 69/8.0);
   fl_set_xyplot_interpolate(fd_fff^.xyplot, 1, 2, 0.1);
   { add inset text }
   fl_add_xyplot_text(fd_fff^.xyplot, 2.2, 1.2, 'Original: Impulse',
                      FL_ALIGN_LEFT , FL_BLACK);
   fl_add_xyplot_text(fd_fff^.xyplot, 2.2, 1.0, 'Overlay: Solid',
                      FL_ALIGN_LEFT, FL_BLUE);


   { show the first form }
   fl_show_form(fd_fff^.fff,FL_PLACE_MOUSE,FL_TRANSIENT,'XYPlot Overlay');
   fl_do_forms();
end.
