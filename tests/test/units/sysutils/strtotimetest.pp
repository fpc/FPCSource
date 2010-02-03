program strtmtest;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$else}
  {$apptype console}
{$endif}

uses sysutils,sysconst{$ifndef fpc},windows{$endif};

{$ifndef fpc}
function defaultformatsettings:TFormatSettings;
begin
  GetLocaleFormatSettings(getsystemdefaultlcid,result);
end;
{$endif}
var exitwitherror:integer =0;
    fmt : TFormatSettings;

Procedure Check(TestNo : Integer; inputstr : String;shouldfailstrtotime:boolean=false;shouldfailcomparison:boolean=false;resultstr:string='');

var dt :TDateTime;
    outputstr:ansistring;

begin
  if TryStrToTime(inputstr,dt,fmt) then
   begin
     if shouldfailstrtotime then
       begin
         writeln('test ',TestNo,' should fail on strtotime while it didn''t ',timetostr(dt,fmt));
         exitwitherror:=1;
       end
     else
       begin
         outputstr:=TimeToStr(dt,fmt); // note because of this bugs can also be in timetostr
         if resultstr<>'' then
            begin
              if outputstr<>resultstr then
                begin
                  writeln('test ',TestNo,' should be "',resultstr,'" is "',outputstr,'"');
                  exitwitherror:=1;
                end;
              exit; // don't do other comparisons
            end;

         if inputstr<>outputstr then
           begin
            if not shouldfailcomparison then
              begin
                writeln('test ',TestNo,' failed "',inputstr,'" <> "',outputstr,'"');
                exitwitherror:=1;
              end;
           end
         else
           begin
            if shouldfailcomparison then
              begin
                writeln('test ',TestNo,' succeeded "',inputstr,'" = "',outputstr,'", while it shouldn''t');
                exitwitherror:=1;
              end;
           end;
       end;
   end
  else
    if not shouldfailstrtotime then
     begin
       Writeln('Test ',TestNo,' failed: ',inputstr);
       exitwitherror:=1;
    end;
end;

procedure setdecimalsep(c:char);
begin
  fmt.DecimalSeparator:=c;
  fmt.longtimeformat:='hh:nn:ss'+fmt.DecimalSeparator+'zzz';
end;

var value: word;
  code : longint;
begin
  fmt:=defaultformatsettings;
  fmt.TimeSeparator:=':';
  fmt.TimeAmstring:='AM';
  fmt.TimePmstring:='PM';

  setdecimalsep('.');
  Check( 0,'12:34:45.789',false,false);
  Check( 1,'12:34:45,789',true,false);

  setdecimalsep(',');
  Check( 2,'12:34:45.789',true,false);
  Check( 3,'12:34:45,789',false,false);

  Check( 4,'12 am',false,false,'00:00:00,000');
  Check( 5,'pm 12:34',false,false,'12:34:00,000');
  Check( 6,'12::45',true,false);
  Check( 7,'12:34:56 px',true,false);
  Check( 8,'12:34:5x',true,false);
  Check( 9,'12:34:56:78:90',true,false);
  Check(10,'5 am',false,false,'05:00:00,000');
  Check(11,'5 pm',false,false,'17:00:00,000');
  Check(12,'am 5',false,false,'05:00:00,000');
  Check(13,'pm 5',false,false,'17:00:00,000');
  fmt.longtimeformat:='hh:nn:ss'+fmt.DecimalSeparator+'zzz am/pm';
  Check(14,'5 am',false,false,'05:00:00,000 am');
  Check(15,'5 pm',false,false,'05:00:00,000 pm');
  Check(16,'am 5',false,false,'05:00:00,000 am');
  Check(17,'pm 5',false,false,'05:00:00,000 pm');
  fmt.TimeAmstring:='AM';
  fmt.TimePmstring:='PM';
  fmt.longtimeformat:='hh:nn:ss'+fmt.DecimalSeparator+'zzz a/p';
  Check(18,'am 5',false,false,'05:00:00,000 a');
  Check(19,'pm 5',false,false,'05:00:00,000 p');

  fmt.TimeAMString:='a'; fmt.TimePMString:='p';

  Check(20,'a 5',false,false,'05:00:00,000 a');
  Check(21,'p 5',false,false,'05:00:00,000 p');
  Check(22,'12:',True,false);
  Check(23,'13:14:',True,false);
  Check(24,'a 17:00',True,false);
  Check(25,'p 19:00',True,false);
  Check(26,'1:2:3',false,false,'01:02:03,000 a');
  Check(27,'1:4',false,false,'01:04:00,000 a');
  Check(28,'111:2:3',True,false);
  Check(29,'1:444',True,false);
  Check(30,'1:2:333',True,false);
  Check(31,'1:4:55,4',False,false,'01:04:55,004 a');
  Check(32,'1:4:55,12',False,false,'01:04:55,012 a');
  Check(33,'1:4:55,004',False,false,'01:04:55,004 a');
  Check(34,'1:4:55,0012',False,false,'01:04:55,012 a');
  Check(35,'1:4:55,004'#9'am',true,false,'01:04:55,004'#9'am');
  Check(36,#9'1:4:55,0012',true,false,'01:04:55,012 a');
  Check(37,' 1:4:55,4',False,false,'01:04:55,004 a');
  Check(38,'1: 4:55,12',False,false,'01:04:55,012 a');
  Check(39,'1:4: 55,004',False,false,'01:04:55,004 a');
  Check(40,'1:4:55, 2',False,false,'01:04:55,002 a');
  Check(41,'1:4:55,   4',False,false,'01:04:55,004 a'); // note more padding then needed
  Check(42,'1:    4:55,   4',False,false,'01:04:55,004 a'); // note more padding then needed
  Check(43,'1:  4:   55,   4',False,false,'01:04:55,004 a'); // note more padding then needed
  Check(44,'1:  4:  55,   4',False,false,'01:04:55,004 a'); // note more padding then needed
  Check(45,'1 4 55 4',True,false);
  fmt.timeseparator:=' ';
  Check(46,'01 04 55',True,false);
  Check(47,'a 01',false,false,'01 00 00,000 a');
  Check(52,'a01',false,false,'01 00 00,000 a');
  fmt.TimeSeparator:=':';
  Check(48,'1:4:55,0000000000000000000000012',false,false,'01:04:55,012 a');
  Check(49,'1:4:55,0000100012',True,false);
  Check(50,'1:4:55,000001012',True,false);
  Check(51,'12:034:00056',false,false,'12:34:56,000 p');

  exitcode:=exitwitherror;

 {$ifndef fpc} // halt in delphi ide  
  readln;
 {$endif}
end.

