Program Example44;

{ Program to demonstrate the Ofs function. }

Var W : Pointer;


begin
  W:=Pointer(Ofs(W)); { W contains its own offset. }
end.
