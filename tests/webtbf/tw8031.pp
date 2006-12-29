{ %fail }

program text;

type TSQLDBTypes = (mysql40,mysql41,mysql50,postgresql,interbase,odbc,oracle);
const MySQLdbTypes = [mysql40,mysql41,mysql50];

begin
  if (SQLDbType in TSQLDBTypes) then writeln('strange');
end. 
