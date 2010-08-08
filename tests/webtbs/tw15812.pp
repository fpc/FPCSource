{$mode macpas}
program test;

type
  Rec1Ptr = ^Rec1;
  Rec1 =
    record
      case boolean of
        False : ( f1: Integer);
        True : ( case boolean of
                    False: ( f2: Integer);
                    True : ( f3: Integer))
    end;
  Rec2 =
    record
      case boolean of
        False : ( p1: Rec1Ptr);
        True : ( p2: Pointer)
    end;

procedure PP( theRec2: Rec2; var theRec1: Rec1);
  begin
    theRec1 := theRec2.p1^
  end;

begin
end.
