unit timer;

  interface

    uses
       sysutils;

    procedure start;
    procedure stop;

  implementation

    var
       stime : longint;

    function gt : longint;

      var
         h,m,s,s1000 : word;

      begin
         decodetime(time,h,m,s,s1000);
         gt:=h*3600000+m*60000+s*1000+s1000;
         {
         gettime(h,m,s,s100);
         gt:=h*360000+m*6000+s*100+s100;
         }
      end;

    procedure start;

      begin
         stime:=gt;
      end;

    procedure stop;

      var
         s : longint;

      begin
         s:=gt-stime;
         write(s div 1000,'.',s mod 1000,' Sekunden');
     end;

end.
