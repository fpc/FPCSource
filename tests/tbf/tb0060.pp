{ %FAIL }
{ Old file: tbf0311.pp }
{ No dup id checking in variant records                  OK 0.99.15 (FK) }

type
  tsplitextended = record
    case byte of
      0: (a: array[0..9] of byte);
      { the following "a" should give a duplicate identifier error }
      1: (a: array[0..4] of word);
      2: (a: array[0..1] of cardinal; w: word);
    end;

begin
end.
