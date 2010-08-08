#include <stdio.h>
#include <mysql/mysql.h>

main () {
  printf("C\n");
  printf("MYSQL : %d  options : %d\n",sizeof(MYSQL),sizeof(struct st_mysql_options));
  printf("MYSQL_DATA : %d\n" ,sizeof(MYSQL_DATA));
  printf("MYSQL_ROWS : %d\n",sizeof(MYSQL_ROWS));
  printf("MYSQL_ROW : %d\n",sizeof(MYSQL_ROW));
  printf("MYSQL_FIELD : %d\n",sizeof(MYSQL_FIELD));
  printf("MYSQL_RES : %d \n",sizeof(MYSQL_RES));
  printf("MEM_ROOT : %d \n",sizeof(MEM_ROOT));
  printf("my_bool : %d \n",sizeof(my_bool));
/*  printf("TNET : %d TNET.nettype : %d \n",sizeof(NET),sizeof(net_type)); */
  printf("USED_MEM : %d \n",sizeof(USED_MEM));
}
