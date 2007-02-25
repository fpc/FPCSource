{ Make DB directory and all subdirectories }
{ FULLDB will be defined if included from db directory }
{ FULLFCL will be defined if included from main FCL directory }

    Targets.ResetDefaults;
    Targets.DefaultDir:='db/memds';
    T:=Targets.AddUnit('memds');
    T.ResourceStrings:=True;
    Targets.Addexampleunit('testpop');
    Targets.Addexampleunit('testopen');
    Targets.Addexampleunit('testld');
    Targets.Addexampleunit('testcp');
