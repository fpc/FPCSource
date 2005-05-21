{ Source provided for Free Pascal Bug Report 1720 }
{ Submitted by "Jesse Towner" on  2001-12-09 }
{ e-mail: jesse@gdnmail.net }
var a, b, c : single;
begin
    (* ... other floating point code goes here *)

    a := 5.6;
    b := 3.4;
    b:= b + sqrt(b)- ln(a);

    c := a - (b-(b-1));

    if abs(c-(a-1))>0.01 then
      begin
        Writeln('Error in floating point code');
        halt(1);
      end;
    (* sometimes, depending on the previous code,
       the code generator will chose to swap the
       operands. however, it just calls the
       reverse version of the instruction without
       swapping the operands. this results in
       giving a value of (b-a) or -2.2 for c. the
       same goes for floating point division. *)
end.
