Program goodies;

{ This demo program uses the routines in the
   goodies section, that help you create easy
   forms in an even easier way.
}

uses xforms,strings;

var
  choice : Longint;
  str1,str2 : string[100];
  s : pchar;

begin
  fl_initialize(@argc, argv, 'FormDemo', nil, 0);

  fl_set_resource(FLOKLabel,'Go');

  if (fl_show_question('Do you want bold font ?',1)<>0) then
     fl_set_goodies_font(FL_BOLD_STYLE,FL_NORMAL_SIZE);

  fl_show_messages('This is a test program for the goodies of the forms library');

  fl_show_alert('Alert', 'Alert form can be used to inform',
               'recoverable errors', 0);

  if (fl_show_question('Do you want to quit?', 0)<>0) then
      halt(0);

  s:=fl_show_input('Give a string:','');
  if s<>nil then strcopy(@str1[1],s);
  fl_show_message('You typed:','',@str1[1]);
  choice := fl_show_choices('Pick a choice',3,'One','Two','Three',2);
  case choice of
    1: fl_show_message('You typed: One','','');
    2: fl_show_message('You typed: Two','','');
    3: fl_show_message('You typed: Three','','');
  else
    begin
    fl_show_message('An error occured!','','');
    end
  end;
  str1:='<Cancel>'#0;
  s:=fl_show_input('Give another string:',@str1[1]);
  if s<>nil then s:=@str1[1];
  fl_show_message('You typed:','',s);
  fl_show_messages('Good Bye');
end.
