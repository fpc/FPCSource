{ Make DB directory and all subdirectories }
{ FULLDB will be defined if included from db directory }
{ FULLFCL will be defined if included from main FCL directory }


Targets.ResetDefaults;
Targets.DefaultDir:='db/sqldb';
T:=Targets.AddUnit('sqldb');
T.ResourceStrings:=True;

{ Include connection drivers }    

{ PosGreSQL } 
{$i postgres/fpmake.inc}

{ MySQL }  
{$i mysql/fpmake.inc}

{ Interbase/Firebird }
{$i interbase/fpmake.inc}

{ MS SQL }
{$i mssql/fpmake.inc}
    