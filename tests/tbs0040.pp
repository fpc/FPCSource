{ xor operator bug                }
{ needs fix in pass_1.pas line    }
{ 706. as well as in the code     }
{ generator - secondadd()         }
var
 b1,b2: boolean;
Begin
  b1:=true;
  b2:=false;
  If (b1 xor b2) Then
  begin
  end
  else
    begin
       writeln('Problem with bool xor');
       halt;
    end;
  b1:=true;
  b2:=true;
  If (b1 xor b2) Then
    begin
       writeln('Problem with bool xor');
       halt;
    end;
  writeln('No problem found');
end.
