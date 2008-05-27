unit timer;

  interface

    uses
       SysUtils;

    var
       verbosetimer : boolean = true;

    procedure start;
    procedure stop;
    function MSec:cardinal;

  implementation

    var
       stime,etime : cardinal;

    function gt : cardinal;

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
         s : cardinal;

      begin
         etime:=gt;
         s:=etime-stime;
	 if verbosetimer then
           write(stderr,s div 1000,'.',s mod 1000,' Seconds');
     end;

    function MSec:cardinal;
      begin
        Msec:=etime-stime;
      end;

end.
