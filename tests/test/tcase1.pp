program test_case;
function case1(Val : byte) : char;
begin
  case Val of
    0..25 : case1:=chr(Val + ord('A'));
    26..51: case1:=chr(Val + ord('a') - 26);
    52..61: case1:=chr(Val + ord('0') - 52);
    62    : case1:='+';
    63    : case1:='/';
  else
    case1:='$';
  end;
end;

function case2(Val : integer) : integer;
begin
  case Val of
    -1      : case2:=1;
    32765..
    32767   : case2:=2;
  else
    case2:=-1;
  end;
end;

function case3(Val : integer) : integer;
begin
  case Val of
    -32768..
    -32766 : case3:=1;
    0..10  : case3:=2;
  else
    case3:=-1;
  end;
end;

var
  error: boolean;

begin
  { The correct outputs should be:
    F $
    2 2
    1 2 2
  }
  error := false;
  writeln(case1(5), ' ', case1(255),' (should be: F $)');
  error := (case1(5) <> 'F') or (case1(255) <> '$');
  writeln(case2(32765), ' ', case2(32767),' (should be: 2 2)');
  error := error or (case2(32765) <> 2) or (case2(32767) <> 2);
  writeln(case3(-32768),' ',case3(0), ' ',case3(5),' (should be: 1 2 2)');
  error := error or (case3(-32768) <> 1) or (case3(0) <> 2) or
           (case3(5) <> 2);
  if error then
    halt(1);
end.
