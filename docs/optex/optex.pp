program testopt;

{ Program to depmonstrate the getopts function. }

{
  Valid calls to this program are
  optex --verbose --add me --delete you
  optex --append --create child
  optex -ab -c me -d you
  and so on
}
uses getopts;

var c : char;
    optionindex : Longint;
    theopts : array[1..7] of TOption;

begin
  with theopts[1] do
   begin
    name:='add';
    has_arg:=1;
    flag:=nil;
    value:=#0;
  end;
  with theopts[2] do
   begin
    name:='append';
    has_arg:=0;
    flag:=nil;
    value:=#0;
  end;
  with theopts[3] do
   begin
    name:='delete';
    has_arg:=1;
    flag:=nil;
    value:=#0;
  end;
  with theopts[4] do
   begin
    name:='verbose';
    has_arg:=0;
    flag:=nil;
    value:=#0;
  end;
  with theopts[5] do
   begin
    name:='create';
    has_arg:=1;
    flag:=nil;
    value:='c'
  end;
  with theopts[6] do
   begin
    name:='file';
    has_arg:=1;
    flag:=nil;
    value:=#0;
  end;
  with theopts[7] do
   begin
    name:='';
    has_arg:=0;
    flag:=nil;
  end;
  c:=#0;
  repeat
    c:=getlongopts('abc:d:012',@theopts[1],optionindex);
    case c of
      '1','2','3','4','5','6','7','8','9' :
        begin
        writeln ('Got optind : ',c)
        end;
      #0 : begin
           write ('Long option : ',theopts[optionindex].name);
           if theopts[optionindex].has_arg>0 then
             writeln (' With value  : ',optarg)
           else
             writeln
           end;
      'a' : writeln ('Option a.');
      'b' : writeln ('Option b.');
      'c' : writeln ('Option c : ', optarg);
      'd' : writeln ('Option d : ', optarg);
      '?',':' : writeln ('Error with opt : ',optopt);
   end; { case }
 until c=endofoptions;
 if optind<=paramcount then
    begin
    write ('Non options : ');
    while optind<=paramcount do
      begin
      write (paramstr(optind),' ');
      inc(optind)
      end;
    writeln
    end
end.
