#=============================================================================
#
#    File:  lib/typer/src/Makefile
# Authors:  Kostis Sagonas
#
#=============================================================================

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(TYPER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/typer-$(VSN)

# ----------------------------------------------------
# Orientation information -- find dialyzer's dir
# ----------------------------------------------------
DIALYZER_DIR = $(ERL_TOP)/lib/dialyzer

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES =  \
	typer \
	typer_annotator \
	typer_info \
	typer_map \
	typer_options \
	typer_preprocess

HRL_FILES= typer.hrl
ERL_FILES= $(MODULES:%=%.erl)
INSTALL_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)
TARGET_FILES= $(INSTALL_FILES)
 
APP_FILE= typer.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= typer.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -I$(DIALYZER_DIR)/src +warn_obsolete_guard +warn_untyped_record +warn_missing_spec

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

docs:

clean:
	rm -f $(TARGET_FILES)
	rm -f core

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(EBIN)/typer_options.$(EMULATOR): typer_options.erl ../vsn.mk Makefile
	erlc -W  $(ERL_COMPILE_FLAGS) -DVSN="\"v$(VSN)\"" -o$(EBIN) typer_options.erl

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);' $< > $@

# ---------------------------------------------------------------------
# dependencies -- I wish they were somehow automatically generated
# ---------------------------------------------------------------------

$(EBIN)/typer.beam: typer.hrl $(DIALYZER_DIR)/src/dialyzer.hrl
$(EBIN)/typer_annotator.beam: typer.hrl $(DIALYZER_DIR)/src/dialyzer.hrl
$(EBIN)/typer_info.beam: typer.hrl $(DIALYZER_DIR)/src/dialyzer.hrl
$(EBIN)/typer_options.beam: typer.hrl $(DIALYZER_DIR)/src/dialyzer.hrl
$(EBIN)/typer_preprocess.beam: typer.hrl $(DIALYZER_DIR)/src/dialyzer.hrl

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(YRL_FILES) \
		$(RELSYSDIR)/src
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(INSTALL_FILES) $(RELSYSDIR)/ebin

release_docs_spec:
