create sequence seqExpenseTypesID;
create table ExpenseTypes (
  etID bigint not null default nextval('seqExpenseTypesID'),
  etName varchar(50) not null,
  etDescription varchar(100) not null,
  etMaxAmount decimal(10,2),
  etCost decimal(10,2) default 1,
  etActive boolean not null default true
);

create sequence seqUsersID;
create table Users (
  uID bigint not null default nextval('seqUsersID'),
  uLogin varchar(50) not null,
  uFullName varchar(100) not null,
  uPassword varchar(100) not null,
  uActive boolean not null default true 
);

create sequence seqProjectsID;
create table Projects (
  pID bigint not null default nextval('seqProjectsID'),
  pName varchar(50) not null,
  pDescription varchar(100) not null,
  pActive boolean not null default true
);

create sequence seqExpenseID;
drop table Expenses;
create table Expenses (
  eID bigint not null default nextval('seqExpenseID'),
  eUserFK bigint not null,
  eProjectFK bigint not null,
  eTypeFK bigint not null,
  eAmount decimal(10,2) not null,
  eDate date not null default 'today',
  eComment varchar(1024)
);

alter table ExpenseTypes add constraint pkExpenseTypes primary key (etID);
alter table Users add constraint pkUsers primary key (uID);
alter table Projects add constraint pkProjects primary key (pID);
alter table Expenses add  constraint pkExpenses primary key (eID);
