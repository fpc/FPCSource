{$mode objfpc}
{$h+}
unit utstrtotime;

Interface

Function CheckStrToTime : String;

Implementation

uses sysutils, punit;

Function CheckStrToTime : String;

var
  fmt : TFormatSettings;

  Function Check(TestNo : Integer; inputstr : String;shouldfailstrtotime:boolean=false;shouldfailcomparison:boolean=false;resultstr:string='') : Boolean;

  var 
    dt :TDateTime;
    outputstr:ansistring;
    S : String;
    
  begin
    Result:=True;
    S:='Test '+IntToStr(TestNo)+': ';
    if TryStrToTime(inputstr,dt,fmt) then
     begin
       if shouldfailstrtotime then
         begin
         Fail(S+' should fail on strtotime while it didn''t '+timetostr(dt,fmt));
         Exit(False);
         end
       else
         begin
           outputstr:=TimeToStr(dt,fmt); // note because of this bugs can also be in timetostr
           if resultstr<>'' then
              begin
                if outputstr<>resultstr then
                  begin
                    Fail(S+' should be "'+resultstr+'" is "'+outputstr+'"');
                    Exit(False);
                  end;
                exit; // don't do other comparisons
              end;

           if inputstr<>outputstr then
             begin
              if not shouldfailcomparison then
                begin
                  Fail(S+' failed "'+inputstr+'" <> "'+outputstr+'"');
                  Exit(False);
                end;
             end
           else
             begin
              if shouldfailcomparison then
                begin
                  Fail(S+' succeeded "'+inputstr+'" = "'+outputstr+'", while it shouldn''t');
                  exit(False);
                end;
             end;
         end;
     end
    else
      if not shouldfailstrtotime then
       begin
       Fail(S+' failed: '+inputstr);
       Exit(False);
      end;
  end;

  procedure setdecimalsep(c:char);
  begin
    fmt.DecimalSeparator:=c;
    fmt.longtimeformat:='hh:nn:ss'+fmt.DecimalSeparator+'zzz';
  end;

begin
  Result:='';
  fmt:=defaultformatsettings;
  fmt.TimeSeparator:=':';
  fmt.TimeAmstring:='AM';
  fmt.TimePmstring:='PM';

  setdecimalsep('.');
  If not Check( 0,'12:34:45.789',false,false) then exit;
  If not Check( 1,'12:34:45,789',true,false) then exit;

  setdecimalsep(',');
  If not Check( 2,'12:34:45.789',true,false) then exit;
  If not Check( 3,'12:34:45,789',false,false) then exit;

  If not Check( 4,'12 am',false,false,'00:00:00,000') then exit;
  If not Check( 5,'pm 12:34',false,false,'12:34:00,000') then exit;
  If not Check( 6,'12::45',true,false) then exit;
  If not Check( 7,'12:34:56 px',true,false) then exit;
  If not Check( 8,'12:34:5x',true,false) then exit;
  If not Check( 9,'12:34:56:78:90',true,false) then exit;
  If not Check(10,'5 am',false,false,'05:00:00,000') then exit;
  If not Check(11,'5 pm',false,false,'17:00:00,000') then exit;
  If not Check(12,'am 5',false,false,'05:00:00,000') then exit;
  If not Check(13,'pm 5',false,false,'17:00:00,000') then exit;
  fmt.longtimeformat:='hh:nn:ss'+fmt.DecimalSeparator+'zzz am/pm';
  If not Check(14,'5 am',false,false,'05:00:00,000 am') then exit;
  If not Check(15,'5 pm',false,false,'05:00:00,000 pm') then exit;
  If not Check(16,'am 5',false,false,'05:00:00,000 am') then exit;
  If not Check(17,'pm 5',false,false,'05:00:00,000 pm') then exit;
  fmt.TimeAmstring:='AM';
  fmt.TimePmstring:='PM';
  fmt.longtimeformat:='hh:nn:ss'+fmt.DecimalSeparator+'zzz a/p';
  If not Check(18,'am 5',false,false,'05:00:00,000 a') then exit;
  If not Check(19,'pm 5',false,false,'05:00:00,000 p') then exit;

  fmt.TimeAMString:='a'; fmt.TimePMString:='p';

  If not Check(20,'a 5',false,false,'05:00:00,000 a') then exit;
  If not Check(21,'p 5',false,false,'05:00:00,000 p') then exit;
  If not Check(22,'12:',True,false) then exit;
  If not Check(23,'13:14:',True,false) then exit;
  If not Check(24,'a 17:00',True,false) then exit;
  If not Check(25,'p 19:00',True,false) then exit;
  If not Check(26,'1:2:3',false,false,'01:02:03,000 a') then exit;
  If not Check(27,'1:4',false,false,'01:04:00,000 a') then exit;
  If not Check(28,'111:2:3',True,false) then exit;
  If not Check(29,'1:444',True,false) then exit;
  If not Check(30,'1:2:333',True,false) then exit;
  If not Check(31,'1:4:55,4',False,false,'01:04:55,004 a') then exit;
  If not Check(32,'1:4:55,12',False,false,'01:04:55,012 a') then exit;
  If not Check(33,'1:4:55,004',False,false,'01:04:55,004 a') then exit;
  If not Check(34,'1:4:55,0012',False,false,'01:04:55,012 a') then exit;
  If not Check(35,'1:4:55,004'#9'am',true,false,'01:04:55,004'#9'am') then exit;
  If not Check(36,#9'1:4:55,0012',true,false,'01:04:55,012 a') then exit;
  If not Check(37,' 1:4:55,4',False,false,'01:04:55,004 a') then exit;
  If not Check(38,'1: 4:55,12',False,false,'01:04:55,012 a') then exit;
  If not Check(39,'1:4: 55,004',False,false,'01:04:55,004 a') then exit;
  If not Check(40,'1:4:55, 2',False,false,'01:04:55,002 a') then exit;
  If not Check(41,'1:4:55,   4',False,false,'01:04:55,004 a') then exit; // note more padding then needed
  If not Check(42,'1:    4:55,   4',False,false,'01:04:55,004 a') then exit; // note more padding then needed
  If not Check(43,'1:  4:   55,   4',False,false,'01:04:55,004 a') then exit; // note more padding then needed
  If not Check(44,'1:  4:  55,   4',False,false,'01:04:55,004 a') then exit; // note more padding then needed
  If not Check(45,'1 4 55 4',True,false) then exit;
  fmt.timeseparator:=' ';
  If not Check(46,'01 04 55',True,false) then exit;
  If not Check(47,'a 01',false,false,'01 00 00,000 a') then exit;
  If not Check(52,'a01',false,false,'01 00 00,000 a') then exit;
  fmt.TimeSeparator:=':';
  If not Check(48,'1:4:55,0000000000000000000000012',false,false,'01:04:55,012 a') then exit;
  If not Check(49,'1:4:55,0000100012',True,false) then exit;
  If not Check(50,'1:4:55,000001012',True,false) then exit;
  If not Check(51,'12:034:00056',false,false,'12:34:56,000 p') then exit;
end;

begin
  AddSuite('SysUtils');
  AddTest('CheckStrToTime',@CheckStrToTime,'SysUtils');
end.
