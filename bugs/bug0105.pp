{$R+}
{ BOUND check error... I don't think this is a code generator error }
{ but an error because the type casting is not considered at all!   }
{ Must be compiled with -Cr                                         }


Var
 Sel: Word;
 v: longint;
Begin
 v:=$00ffffff;
 Sel:=word(v);
 writeln(sel);
 sel:=v;
end.
