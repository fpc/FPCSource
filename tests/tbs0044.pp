 { Problem with nested comments -- as you can probably see } 
 { but it does give out kind of a funny error output :)    }

 
 {$UNDEF VP}

 {$IFDEF Windows} ssss {$ENDIF}      {No Syntax Error}

 {$IFDEF VP}
      {$D+}{$R+}
 {$ELSE}
   {$IFDEF Windows} ssss {$ENDIF}    {Syntax Error at: Col 25 }
 {$ENDIF}

 BEGIN
 END.
