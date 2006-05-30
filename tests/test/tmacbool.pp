program macbool;

{$mode macpas}

{ checks boolean evaluation in macpas mode }

var
  ftruecalled, ffalsecalled: boolean;

function ftrue: boolean;
begin
  ftruecalled := true;
  ftrue := true;
end;

function ffalse: boolean;
begin
  ffalsecalled := true;
  ffalse := false;
end;


begin
  { OR and |, short circuit }
{$b-}
  ffalsecalled := false;
  ftruecalled := false;
  if (ftrue or ffalse) then
    begin
      if not(ftruecalled) then
        halt(1);
      if ffalsecalled then
        halt(2);
    end
  else
    halt(128);

  ffalsecalled := false;
  ftruecalled := false;
  if not(ffalse or ftrue) then
    halt(3);
  if not(ffalsecalled) then
    halt(4);
  if not(ftruecalled) then
    halt(5);

  ffalsecalled := false;
  ftruecalled := false;
  if (ftrue | ffalse) then
    begin
      if not(ftruecalled) then
        halt(6);
      if ffalsecalled then
        halt(7);
    end
  else
    halt(129);

  ffalsecalled := false;
  ftruecalled := false;
  if not(ffalse | ftrue) then
    halt(8);
  if not(ffalsecalled) then
    halt(9);
  if not(ftruecalled) then
    halt(10);


  { OR and |, full evaluation }
{$b+}
  ffalsecalled := false;
  ftruecalled := false;
  if (ftrue or ffalse) then
    begin
      if not(ftruecalled) then
        halt(11);
      if not(ffalsecalled) then
        halt(12);
    end
  else
    halt(130);

  ffalsecalled := false;
  ftruecalled := false;
  if not(ffalse or ftrue) then
    halt(13);
  if not(ffalsecalled) then
    halt(14);
  if not(ftruecalled) then
    halt(15);

  ffalsecalled := false;
  ftruecalled := false;
  if (ftrue | ffalse) then
    begin
      if not(ftruecalled) then
        halt(16);
      if ffalsecalled then
        halt(17);
    end
  else
    halt(131);

  ffalsecalled := false;
  ftruecalled := false;
  if not(ffalse | ftrue) then
    halt(18);
  if not(ffalsecalled) then
    halt(19);
  if not(ftruecalled) then
    halt(20);

  { AND and &, short circuit }
{$b-}
  ffalsecalled := false;
  ftruecalled := false;
  if (ftrue and ffalse) then
    halt(21);
   if not(ftruecalled) then
     halt(211);
   if not(ffalsecalled) then
     halt(22);

  ffalsecalled := false;
  ftruecalled := false;
  if (ffalse and ftrue) then
    halt(23);
  if not(ffalsecalled) then
    halt(24);
  if (ftruecalled) then
    halt(25);

  ffalsecalled := false;
  ftruecalled := false;
  if (ftrue & ffalse) then
    halt(206);
  if not(ftruecalled) then
    halt(26);
  if not(ffalsecalled) then
    halt(27);

  ffalsecalled := false;
  ftruecalled := false;
  if (ffalse & ftrue) then
    halt(28);
  if not(ffalsecalled) then
    halt(29);
  if (ftruecalled) then
    halt(30);


  { AND and &, full evaluation }
{$b+}
  ffalsecalled := false;
  ftruecalled := false;
  if (ftrue and ffalse) then
    halt(31);
   if not(ftruecalled) then
     halt(111);
   if not(ffalsecalled) then
     halt(32);

  ffalsecalled := false;
  ftruecalled := false;
  if (ffalse and ftrue) then
    halt(33);
  if not(ffalsecalled) then
    halt(34);
  if not(ftruecalled) then
    halt(35);

  ffalsecalled := false;
  ftruecalled := false;
  if (ftrue & ffalse) then
    halt(133);
  if not(ftruecalled) then
    halt(36);
  if not(ffalsecalled) then
    halt(37);

  ffalsecalled := false;
  ftruecalled := false;
  if (ffalse & ftrue) then
    halt(38);
  if not(ffalsecalled) then
    halt(39);
  if (ftruecalled) then
    halt(40);
end.
