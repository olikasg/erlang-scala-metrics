#-*-makefile-*-   ; force emacs to enter makefile-mode

SUITE_MODULES = \
	snmp_SUITE \
	snmp_app_test \
	snmp_appup_test \
	snmp_compiler_test \
	snmp_conf_test \
	snmp_log_test \
	snmp_note_store_test \
	snmp_pdus_test \
	snmp_agent_mibs_test \
	snmp_agent_nfilter_test \
	snmp_agent_test \
	snmp_agent_test_lib \
	snmp_manager_config_test \
	snmp_manager_user \
	snmp_manager_user_test \
	snmp_manager_user_test_lib \
	snmp_manager_test

TEST_UTIL_MODULES = \
	snmp_test_lib \
	snmp_test_manager \
	snmp_test_mgr \
	snmp_test_mgr_misc \
	sa \
	klas3 \
	test1 \
	test2

TEST_SERVER_MODULES = \
	snmp_test_server \
	snmp_test_suite

MODULES = \
	$(TEST_UTIL_MODULES) \
	$(SUITE_MODULES)

HRL_FILES = snmp_test_lib.hrl

MIB_FILES = \
	OLD-SNMPEA-MIB.mib \
	OLD-SNMPEA-MIB-v2.mib \
	Klas1.mib \
	Klas1-v2.mib \
	Klas2.mib \
	Klas3.mib \
	Klas4.mib \
	SA-MIB.mib \
	EX1-MIB.mib \
	TestTrap.mib \
	TestTrapv2.mib \
	Test1.mib \
	Test2.mib

SPECS = snmp.spec snmp.spec.vxworks 

