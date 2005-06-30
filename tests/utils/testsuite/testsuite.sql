--
-- Table structure for table 'TESTCPU'
--

CREATE TABLE TESTCPU (
  TC_ID int(11) NOT NULL auto_increment,
  TC_NAME varchar(10) default NULL,
  PRIMARY KEY  (TC_ID),
  UNIQUE KEY TC_INAME (TC_NAME)
) TYPE=MyISAM;

--
-- Table structure for table 'TESTOS'
--

CREATE TABLE TESTOS (
  TO_ID int(11) NOT NULL auto_increment,
  TO_NAME varchar(10) default NULL,
  PRIMARY KEY  (TO_ID),
  UNIQUE KEY TR_INAME (TO_NAME)
) TYPE=MyISAM;

--
-- Table structure for table 'TESTRESULTS'
--
CREATE TABLE TESTRUN (
  TU_ID int(11) NOT NULL auto_increment,
  TU_DATE timestamp(14) NOT NULL,
  TU_CPU_FK int(11) NOT NULL,
  TU_OS_FK int(11) NOT NULL,
  TU_VERSION_FK int(11) NOT NULL,
  PRIMARY KEY  (TU_ID),
  KEY TU_IDATE (TU_DATE),
  UNIQUE TU_UNIQUE(TU_DATE,TU_CPU_FK,TU_OS_FK,TU_VERSION_FK)
) TYPE=MyISAM;


--
-- Table structure for table 'TESTRESULTS'
--
CREATE TABLE TESTRESULTS (
  TR_ID int(11) NOT NULL auto_increment,
  TR_TESTRUN_FK int(11) NOT NULL,
  TR_TEST_FK int(11),
  TR_OK char(1) NOT NULL default '-',
  TR_SKIP char(1) NOT NULL default '-',
  TR_RESULT int(11) NOT NULL default '0',
  TR_LOG text,
  PRIMARY KEY  (TR_ID),
  INDEX I_TRTESTRUN (TR_TESTRUN_FK),
  INDEX I_TRTEST (TR_TEST_FK)
) TYPE=MyISAM;

--
-- Table structure for table 'TESTS'
--

CREATE TABLE TESTS (
  T_ID int(11) NOT NULL auto_increment,
  T_NAME varchar(80) NOT NULL default '',
  T_FULLNAME varchar(255) NOT NULL default '',
  T_CPU varchar(20) default NULL,
  T_OS varchar(30) default NULL,
  T_VERSION varchar(10) default NULL,
  T_ADDDATE date NOT NULL default '0000-00-00',
  T_GRAPH char(1) NOT NULL default '-',
  T_INTERACTIVE char(1) NOT NULL default '-',
  T_RESULT int(11) NOT NULL default '0',
  T_FAIL char(1) NOT NULL default '-',
  T_RECOMPILE char(1) NOT NULL default '-',
  T_NORUN char(1) NOT NULL default '-',
  T_NEEDLIBRARY char(1) NOT NULL default '-',
  T_KNOWNRUNERROR int(11) NOT NULL default '0',
  T_KNOWN char(1) NOT NULL default '-',
  T_NOTE varchar(255) default NULL,
  T_DESCRIPTION text,
  T_SOURCE text,
  T_OPTS varchar(255) default NULL,
  PRIMARY KEY  (T_ID),
  UNIQUE KEY TESTNAME (T_NAME)
) TYPE=MyISAM;

--
-- Table structure for table 'TESTVERSION'
--

CREATE TABLE TESTVERSION (
  TV_ID int(11) NOT NULL auto_increment,
  TV_VERSION varchar(10) default NULL,
  TV_RELEASEDATE timestamp(14) NOT NULL,
  PRIMARY KEY  (TV_ID),
  UNIQUE KEY TR_INAME (TV_VERSION)
) TYPE=MyISAM;

