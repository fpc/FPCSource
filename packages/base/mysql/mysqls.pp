program mysqls;

uses mysql,mysql_com;


begin
  Writeln('PASCAL');
  Writeln ('MYSQL : ',sizeof(TMYSQL),' options : ',sizeof(st_mysql_options));
  writeln ('MYSQL_DATA : ',sizeof(TMYSQL_DATA));
  writeln ('MYSQL_ROWS : ',sizeof(TMYSQL_ROWS));
  writeln ('MYSQL_ROW : ',sizeof(TMYSQL_ROW));
  writeln ('MYSQL_FIELD : ',sizeof(TMYSQL_FIELD));
  writeln ('MYSQL_RES : ',sizeof(TMYSQL_RES));
  writeln ('MEM_ROOT : ',sizeof(TMEM_ROOT));
  writeln ('my_bool : ',sizeof(my_bool));
  writeln ('TNET : ',sizeof(TNET),' TNET.nettype : ',SizeOf(net_type));
  writeln ('USED_MEM : ',sizeof(TUSED_MEM));
end.
  $Log$
  Revision 1.2  2002-09-07 15:42:53  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:54:54  peter
    * splitted to base and extra

}
