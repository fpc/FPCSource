{ %OPT=-gh }
{ Source provided for Free Pascal Bug Report 4247 }
{ Submitted by "Martin Schreiber" on  2005-08-02 }
{ e-mail:  }
program project1;
 //compile with -ghl
var
 po1,po2: pointer;
begin
 HaltOnNotReleased := true;
 getmem(po1,500);
 getmem(po2,500);
 reallocmem(po1,400);
 reallocmem(po1,300);
 reallocmem(po1,200);
 reallocmem(po1,400); //crash with error 204
 reallocmem(po1,600);
 freemem(po1,600);
 freemem(po2,500);
end.
