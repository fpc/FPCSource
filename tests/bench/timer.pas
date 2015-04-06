{ $define USEEPIK}
unit timer;

  interface

    uses
       SysUtils{$ifdef USEEPIK},epiktimer{$endif USEEPIK};

    var
       verbosetimer : boolean = true;

    procedure start;
    procedure stop;
    function MSec:cardinal;

  implementation

    var
{$ifdef USEEPIK}
       et : TEpiktimer;
{$else EPIKTIMER}
       stime,etime : cardinal;
{$endif USEEPIK}

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
{$ifdef USEEPIK}
        et:=TEpikTimer.Create;
        et.Start;
{$else USEEPIK}
        stime:=gt;
{$endif USEEPIK}
      end;

    procedure stop;

      var
{$ifdef USEEPIK}
         e : extended;
{$else USEEPIK}
         s : cardinal;
{$endif USEEPIK}

      begin
{$ifdef USEEPIK}
         e:=et.elapsed;
         et.Free;
{$else USEEPIK}
         etime:=gt;
         s:=etime-stime;
{$endif USEEPIK}
         if verbosetimer then
{$ifdef USEEPIK}
           write(stderr,e:0:6,' Seconds');
{$else USEEPIK}
           write(stderr,s div 1000,'.',format('%03d',[s mod 1000]),' Seconds');
{$endif USEEPIK}
      end;

{$ifdef USEEPIK}
    function MSec:cardinal;
      begin
        Msec:=round(et.elapsed*1000);
      end;
{$else USEEPIK}
    function MSec:cardinal;
      begin
        Msec:=etime-stime;
      end;
{$endif USEEPIK}

end.
