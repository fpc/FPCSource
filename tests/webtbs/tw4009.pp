{ Source provided for Free Pascal Bug Report 4009 }
{ Submitted by "Daniël Mantione" on  2005-05-23 }
{ e-mail: daniel@freepascal.org }
program testpointercalcbug;

{$R+}
{$Q+}

type    Pheader=^Theader;
        Theader=record
          x:cardinal
        end;

        Ppayload=^Tpayload;
        Tpayload=record
          s:string;
        end;

        Trecordwithheader=record
          header:Theader;
          payload:Tpayload;
        end;

{$ifndef fpc} ptruint = cardinal; {$endif}

var r:Trecordwithheader;
    p:Ppayload;
    h:Pheader;
    l,l2 : cardinal;
begin
  p:=@r.payload;
  {Calculate address of header from address of payload.}

  h:=Pheader(ptruint(p)-shortint(sizeof(Theader)));
  writeln(ptruint(h));
  l:=$ffffffff;
  l2:=1;
  writeln(l-l2);
end.
