program chartall;
{ Showing all different charts }

{ Form definition file generated with fdesign. }
uses xforms;

var
  form : PFL_FORM;

        barchart,
        linechart,
        filledchart,
        piechart,
        specialpiechart,
        exitbut,
        horbarchart,
        spikechart : PFL_OBJECT;

procedure create_form_form;

var
  obj : PFL_OBJECT;

begin
  form := fl_bgn_form(FL_NO_BOX,940,360);
  obj := fl_add_box(FL_UP_BOX,0,0,940,360,'');
  obj := fl_add_chart(FL_BAR_CHART,20,20,210,140,'BAR_CHART');
  barchart := obj;
    fl_set_object_boxtype(obj,FL_RSHADOW_BOX);
  obj := fl_add_chart(FL_LINE_CHART,250,20,210,140,'LINE_CHART');
  linechart := obj ;
    fl_set_object_boxtype(obj,FL_RSHADOW_BOX);
  obj := fl_add_chart(FL_FILL_CHART,250,190,210,140,'FILL_CHART');
  filledchart := obj   ;
  fl_set_object_boxtype(obj,FL_RSHADOW_BOX);
  obj := fl_add_chart(FL_PIE_CHART,480,190,210,140,'PIE_CHART');
  piechart := obj;
    fl_set_object_boxtype(obj,FL_RSHADOW_BOX);
  obj:= fl_add_chart(FL_SPECIALPIE_CHART,710,20,210,140,'SPECIALPIE_CHART');
  specialpiechart := obj ;
    fl_set_object_boxtype(obj,FL_RSHADOW_BOX);
  obj := fl_add_button(FL_NORMAL_BUTTON,750,260,140,30,'Exit');
  exitbut := obj;
  obj:= fl_add_chart(FL_HORBAR_CHART,20,190,210,140,'HORBAR_CHART');
  horbarchart := obj;
     fl_set_object_boxtype(obj,FL_RSHADOW_BOX);
  obj:= fl_add_chart(FL_SPIKE_CHART,480,20,210,140,'SPIKE_CHART');
  spikechart := obj;
     fl_set_object_boxtype(obj,FL_RSHADOW_BOX);
  fl_end_form();
end;

{---------------------------------------}

procedure fill_in(ob : PFL_OBJECT);

var c : longint;

begin
  c := FL_BLACK+1;
  fl_add_chart_value(ob,15.0,'item 1',c); inc(c);
  fl_add_chart_value(ob,5.0,'item 2',c);inc(c);
  fl_add_chart_value(ob,0.0,'item 3',c);inc(c);
  fl_add_chart_value(ob,-10.0,'item 4',c);inc(c);
  fl_add_chart_value(ob,25.0,'item 5',c);inc(c);
  fl_add_chart_value(ob,12.0,'item 6',c);inc(c);
end;

begin

  fl_initialize(@argc, argv, 'FormDemo', nil, 0);
  create_form_form();
  fill_in(barchart);
  fill_in(horbarchart);
  fill_in(linechart);
  fill_in(filledchart);
  fill_in(spikechart);
  fill_in(piechart);
  fill_in(specialpiechart);
  fl_show_form(form,FL_PLACE_CENTER,FL_TRANSIENT,'Charts');
  fl_do_forms();
end.
