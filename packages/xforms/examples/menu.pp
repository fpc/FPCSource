{ This demo shows the use of menu's.
 * The first two are PUSH_MENUs (pop-up).
 * The third one is PULLDOWN_MENU
 * and the last one is TOUCH_MENU
 *
 * a confusing demo, but a good testing program ..
 }
program menudemo;

uses xforms;

Var menu,abox : array[0..3] of PFL_OBJECT;
    isset : array[0..3] of longint;

   form : PFL_FORM;
   i, j : longint;


procedure menu_cb(ob : PFL_OBJECT; m : longint);cdecl;

var i,item : longint;

begin
    item := fl_get_menu(ob);

    if (item <= 0) or (isset[m] = item) then
       exit;

    for i := 0 to 3 do
       if ( i <> m) then
         begin
         { enable the old selected color for other menus}
         fl_set_menu_item_mode(menu[i], isset[m], Cardinal(FL_PUP_RADIO));
         { disable the currently selected color for other menus }
         fl_set_menu_item_mode(menu[i], item, Cardinal(FL_PUP_GRAY or FL_PUP_RADIO));
         end;

    isset[m] := item;
    fl_set_object_color(abox[m], FL_BLACK+item, FL_BLACK);
end;

procedure done_cb(ob : PFL_OBJECT; data : longint) ; cdecl;

begin
    halt;
end;

Function create_form : PFL_Form;

var form : PFL_FORM;
   obj : PFL_OBJECT;

begin
   form := fl_bgn_form(FL_NO_BOX,440,380);
   obj := fl_add_box(FL_BORDER_BOX,0,0,440,380,'');
     fl_set_object_color(obj,FL_SLATEBLUE,FL_COL1);

   obj := fl_add_menu(FL_PUSH_MENU,0,0,110,30,'Color 1');
   menu[0] := obj;
    fl_set_object_shortcut(obj, '1#1', 1);
    fl_set_object_boxtype(obj, FL_UP_BOX);
    fl_set_object_callback(obj, PFL_CALLBACKPTR(@menu_cb), 0);
   obj := fl_add_menu(FL_PUSH_MENU,110,0,110,30,'Color 2');
   menu[1] := obj;
    fl_set_object_shortcut(obj, '2#2', 1);
    fl_set_object_callback(obj, PFL_CALLBACKPTR(@menu_cb), 1);
   obj := fl_add_menu(FL_PULLDOWN_MENU,220,0,110,30,'Color 3');
   menu[2] := obj;
    fl_set_object_shortcut(obj, '3#3', 1);
    fl_set_object_callback(obj, PFL_CALLBACKPTR(@menu_cb), 2);
   obj := fl_add_menu(FL_TOUCH_MENU,330,0,110,30,'Color 4');
   menu[3] := obj;
    fl_set_object_shortcut(obj, '4#4', 1);
    fl_set_object_callback(obj, PFL_CALLBACKPTR(@menu_cb), 3);

   abox[0] := fl_add_box(FL_SHADOW_BOX,20,80,70,230,'');
   abox[1] := fl_add_box(FL_SHADOW_BOX,130,80,70,230,'');
   abox[2] := fl_add_box(FL_SHADOW_BOX,240,80,70,230,'');
   abox[3] := fl_add_box(FL_SHADOW_BOX,350,80,70,230,'');
   obj := fl_add_button(FL_NORMAL_BUTTON,310,330,110,30,'Exit');
     fl_set_object_callback(obj, PFL_CALLBACKPTR(@done_cb), 0);
   fl_end_form();
   {fl_scale_form(form, 0.9, 0.9);}
   create_form:=form;
end;

begin
   fl_initialize(@argc, argv, 'FormDemo', nil, 0);

   form := create_form();

  {fl_setpup_color(FL_SLATEBLUE, FL_BLACK); }

   for i:=0 to 3 do
      begin
      fl_show_menu_symbol(menu[i], 1);
      fl_set_menu(menu[i],
              'Red%r1|Green%r1|Yellow%r1|Blue%r1|Purple%r1|Cyran%r1|White%r1');
      fl_set_menu_item_shortcut(menu[i], 1, 'Rr#R#r');
      fl_set_menu_item_shortcut(menu[i], 2, 'Gg#G#g');
      fl_set_menu_item_shortcut(menu[i], 3, 'Yy#Y#y');
      fl_set_menu_item_shortcut(menu[i], 4, 'Bb#B#b');
      fl_set_menu_item_shortcut(menu[i], 5, 'Pp#P#p');
      fl_set_menu_item_shortcut(menu[i], 6, 'Cc#C#c');
      fl_set_menu_item_shortcut(menu[i], 7, 'Ww#W#w');

      { initially the last three entries are enabled }
      for j:=5 to 7 do
         fl_set_menu_item_mode(menu[i], j, Cardinal(FL_PUP_RADIO));
      { the first four are disabled except the item (i+1) }
      for j:=1 to 4 do
         fl_set_menu_item_mode(menu[i], j, Cardinal(FL_PUP_GREY or FL_PUP_RADIO));
      isset[i] := i + 1;
      fl_set_object_color(abox[i], FL_BLACK+isset[i], FL_BLACK);
      fl_set_menu_item_mode(menu[i], isset[i], Cardinal(FL_PUP_CHECK or FL_PUP_RADIO));
      end;

  fl_show_form(form,FL_PLACE_CENTER,FL_NOBORDER,Nil);
  fl_do_forms();
  fl_hide_form(form);
end.
