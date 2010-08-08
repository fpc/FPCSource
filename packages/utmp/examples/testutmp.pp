Program testu;

uses utmp,unixutil;

Const UTMPFile = '/var/run/utmp';

var
   EndOfUsers : Boolean;
   m,d,y,hh,mm,ss : Word;

begin
  Set_utmp_file(UTMPFile);
  Writeln ('Number of utmp entries : ',number_of_utmp_entries);
  set_search_parameters (Include ,All_login_types);
  Read_logged_users;
  Writeln ('Number of logged users : ',Number_Of_logged_users);
  While More_USers do
    With Get_Next_user do
      begin
      Writeln ('Logtype    : ',login_type_names[type_of_login]);
      Writeln ('Pid        : ',Pid);
      Writeln ('Device     : ',Device);
      Writeln ('TTYname    : ',TTy_Name);
      EpochToLocal(login_time,y,m,d,hh,mm,ss);
      Writeln ('Login Time : ',y,'/',m,'/',d,' ',hh,':',mm,':',ss);
      Writeln ('User Name  : ',user_name);
      Writeln ('Host name  : ',host_name);
      Write ('IP address : ',Ip_address[1],'.',Ip_address[2]);
      Writeln ('.',ip_address[3],'.',ip_address[4]);
      end;
end.
