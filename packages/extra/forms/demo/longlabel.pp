{ Demo of the Use of a very long label }
program longlabel;

uses xforms;

Const
label1 : pchar = 'This demo shows the use of some very'#10+
              'long labels. The dynamic storage allocation'#10+
              'for such long labels should guarantee that'#10+
              'all of this works without any problem.';
{ This shows a funny bug in FPK 0.9.1 :) MVC.}

label2 : pchar ='This is the second string that should again'#10+
             'be a bit larger such that a new, larger amount'#10+
             'of storage has to be allocated for the label.'#10+
             'This is of course no problem. By the way,'#10+
             'dynamic allocation of storage saves a lot'#10+
             'of memory because for most objects the label'#10+
             'is much shorter than the 64 bytes that were'#10+
             'allocated for it in the previous version of'#10+
             'the Forms Library';

label3 : pchar ='And now back to the first one:'#10#10+
             'This demo shows the use of some very'#10+
             'long labels. The dynamic storage allocation'#10+
             'for such long labels should guarantee that'#10+
             'all of this works without any problem.';

var
   form : PFL_FORM;
   strobj, but : PFL_OBJECT;

begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);

  form := fl_bgn_form(FL_UP_BOX,400,300);
    strobj := fl_add_box(FL_DOWN_BOX,10,10,380,240,'Press Next');
    fl_set_object_lsize(strobj,FL_NORMAL_SIZE);
    but := fl_add_button(FL_NORMAL_BUTTON,160,260,80,30,'Next');
  fl_end_form();

  fl_set_form_hotobject(form, but);
  fl_show_form(form,FL_PLACE_HOTSPOT,FL_TRANSIENT,nil);

  fl_do_forms();
  fl_set_object_label(strobj,label1);

  fl_do_forms();
  fl_set_object_label(strobj,label2);

  fl_do_forms();
  fl_set_object_label(strobj, 'Now we turn to a short label');
  fl_do_forms();
  fl_set_object_label(strobj,label3);

  fl_set_object_label(but,'Quit');
  fl_do_forms();
end.
