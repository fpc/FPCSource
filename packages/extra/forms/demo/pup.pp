{ Demo: complete pop-ups.
 * test font/cursor change
 * test attaching pup to menu
 }
program popup;

uses xforms,strings;

var
pup : PFL_FORM;
done, pret, b1, b2, b3, menu : PFL_OBJECT;

var
    aa : TFL_IOPT;
    mask : cardinal;


const
   subm : longint = -1;
    m : longint = -1;
    ssm : longint = 0;
    n1 : longint = -1;
    n2 : longint = -1;
    n : longint = 0;

{ post-handler }
function  post(ob : PFL_OBJECT; ev : Longint; mx,y : TFL_Coord; key  : longint; xev : pointer) : Longint;

begin
    if (n1 = -1) then
     begin
      n1 := fl_defpup(FL_ObjWin(ob),'line1|line2');
      fl_setpup_shadow(n1,0);
      fl_setpup_bw(n1,0);
      fl_setpup_pad(n1,3,0);

      n2 := fl_defpup(FL_ObjWin(ob),'button1|button2');
      fl_setpup_shadow(n2,0);
      fl_setpup_bw(n2,-1);
      fl_setpup_pad(n2,3,0);
    end;

    if (ev = FL_ENTER) then
      begin
       if (ob=b3) then
         fl_show_oneliner('button3',ob^.form^.x+ob^.x,
                         ob^.form^.y+ob^.y + ob^.h + 5)
       else
        begin
          fl_setpup_position(ob^.form^.x+ob^.x, ob^.form^.y+ob^.y+ob^.h + 5);
          if ob=b1 then
            fl_showpup(n1)
          else
            fl_showpup(n2);
        end
      end
    else if(ev <> FL_MOTION) then
      begin
       if (ob=b3) then
         fl_hide_oneliner
       else
         if ob=b1 then
           fl_hidepup(n1)
         else
           fl_hidepup(n2);
      end;
end;


procedure show_return_val(i :  longint);

var
    buf : string[128];

begin
      str(i,buf);
      buf:= 'Returned '+buf;
     if i>0 then
       buf:=buf+'('++strpas(fl_getpup_text(m,i))+')'#0;

    fl_set_object_label(pret, @buf[1]);
end;

function ssm_cb(a : longint) : longint;
begin
   show_return_val(a);
   ssm_cb:=a;
end;

procedure do_pup(ob : PFL_OBJECT; q : longint);

begin
   if (subm = -1) then
   begin
      ssm  := fl_newpup(FL_ObjWin(ob));
      subm := fl_newpup(FL_ObjWin(ob));
      m    := fl_newpup(FL_ObjWin(ob));

      { Problem, variable nr. of arguments in c code !!!!}
      { fl_addtopup(ssm,'SubSubM%F%t',ssm_cb); }
      fl_addtopup(ssm,'SSMItem20%x20%R1');
      fl_addtopup(ssm,'SSMItem21%x21%r1');
      fl_addtopup(ssm,'SSMItem22%x22%r1%l');
      fl_addtopup(ssm,'SSMitem30%x30%R2');
      fl_addtopup(ssm,'SSMItem31%x31%r2');
      fl_addtopup(ssm,'SSMItem32%x32%r2');

      fl_addtopup(subm,'SubMenu%t');
      fl_addtopup(subm,'SMItemA\tAlt-A%x10%h','#a');
      fl_addtopup(subm,'SMItemB\tAlt-B%x11%h','#b');
      fl_addtopup(subm,'SMItemC\tAlt-C%x12%h','#c');
      fl_addtopup(subm,'SMItemD\tAlt-F5%x13%h%m','#&5',ssm);
      fl_addtopup(subm,'SMItemE\tAlt-E%x14','#E');

      fl_setpup_mode(subm, 14, FL_PUP_GREY);

      fl_addtopup(m,'PopUP%t');
      fl_addtopup(m,'MenuItem1%h','1#1');

      fl_addtopup(m,'MenuItem2%h','2#2');
      fl_setpup_submenu(m, 2, subm);
      fl_addtopup(m,'MenuItem3%h','3#3');
      fl_addtopup(m,'MenuItem4%h','4#4');
   end;


   if (fl_get_button_numb(ob) >= FL_SHORTCUT) then
      fl_setpup_position(ob^.form^.x + ob^.x,
                      ob^.form^.y + ob^.y + ob^.h);

   show_return_val(fl_dopup(m));

   { test if changing size/style ok }
   n := not(n);
   if n<>0 then
    begin
    fl_setpup_fontsize(14);
    fl_setpup_fontstyle(FL_TIMES_STYLE);
    fl_setpup_cursor(m, XC_hand2);
    end
   else
     begin
     fl_setpup_fontsize(12);
     fl_setpup_fontstyle(FL_BOLDITALIC_STYLE);
     fl_setpup_cursor(m,XC_sb_right_arrow);
     end;
end;

procedure init_menu;
var
    mm,smm : longint;

begin
    mm := fl_newpup(fl_default_win);
    fl_setpup_bw(mm, -2);
    fl_setpup_shadow(mm, 0);
    smm := fl_newpup(0);
    fl_setpup_shadow(smm, 0);

    fl_addtopup(mm,'MenuItem1|MenuItem2%m|MenuItem3',smm);
    fl_addtopup(smm,'SubItem1%x11|SubItem2%x12|SubItem3%x13');


    { attach pup to menu }

    fl_set_menu_popup(menu, mm);
end;


procedure do_menu(ob  : PFL_OBJECT; data : longint);
var
    buf : string[128];

begin
    str(fl_get_menu(ob),buf);
    if (fl_get_menu(ob) >= 0) then
       buf:=buf+strpas(fl_get_menu_text(ob));
    buf:=buf+#0;

    fl_set_object_label(pret, @buf[1]);
end;

procdure done_cb(ob : PFL_OBJECT; data : longint);

begin
  halt(0)
end;

procedure create_form_pup;

var
  obj : PFL_OBJECT ;

begin
  if (pup<>nil) then
     exit;
  pup := fl_bgn_form(FL_UP_BOX,260,210);
  obj := fl_add_button(FL_NORMAL_BUTTON,150,150,90,35,'Done');
    fl_set_object_callback(obj,PFL_CALLBACKPTR(@done_cb), 0);
  done := obj;
  obj := fl_add_button(FL_MENU_BUTTON,30,90,100,30,'PopUp');
    fl_set_button_shortcut(obj,'Pp#p',1);
    fl_set_object_callback(obj, PFL_CALLBACKPTR(@do_pup), 0);
  obj := fl_add_menu(FL_PULLDOWN_MENU,160,95,60,25,'Menu');
    fl_set_object_callback(obj, PFL_CALLBACKPTR(@do_menu), 0);
  menu := obj;
  obj := fl_add_text(FL_NORMAL_TEXT,20,60,220,30,'');
    fl_set_object_lalign(obj,FL_ALIGN_CENTER);
  pret := obj;
  b1 := fl_add_button(FL_NORMAL_BUTTON, 20, 10, 60, 30,'Button1');
  b2 := fl_add_button(FL_NORMAL_BUTTON, 90, 10, 60, 30,'Button2');
  b3 := fl_add_button(FL_NORMAL_BUTTON, 160, 10, 60, 30,'Button3');
  fl_end_form();
end;


begin
    mask := FL_PDVisual;
    aa.vclass := FL_DefaultVisual;
    fl_set_defaults(mask, @aa);

    fl_initialize(@argc, argv, 'FormDemo', nil, 0);

    create_form_pup();

    { initialize }

    fl_set_object_posthandler(b1, @post);
    fl_set_object_posthandler(b2, @post);
    fl_set_object_posthandler(b3, @post);

    fl_show_form(pup, FL_PLACE_MOUSE, FL_TRANSIENT,'PupDemo');
    init_menu;

    fl_do_forms;
end.
