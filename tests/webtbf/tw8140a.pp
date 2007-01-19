{ %fail }
{ %norun }

{$ifdef fpc}
{$mode delphi}
{$endif}

type
 inullinterface = interface
  //no referencecount, only for fpc, not available in delphi
 end;

 locateresultty = (loc_timeout,loc_notfound,loc_ok); 

 tfield = class
 end;

 locateoptionty = (loo_caseinsensitive,loo_partialkey,
                        loo_noforeward,loo_nobackward);
 locateoptionsty  = set of locateoptionty;

 imselocate = interface(inullinterface)['{2680958F-F954-DA11-9015-00C0CA1308FF}']
   function locate(const key: integer; const field: tfield;
                     const options: locateoptionsty = []): locateresultty;
   function locate(const key: string; const field: tfield; 
                 const options: locateoptionsty = []): locateresultty;
 end;

begin
end.
