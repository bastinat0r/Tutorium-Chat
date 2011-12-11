ERL          = erl
ERLC         = erlc

APPLICATION  = uns_chat
SRC_DIR      = $(CURDIR)/src
EBIN_DIR     = $(CURDIR)/ebin

CT_SUITE     = all
TEST_DIR     = $(CURDIR)/test
TEST_LOG_DIR = $(CURDIR)/test-log

.PHONY: all clean shell doc

all:
	$(ERL) -noinput -eval "case make:all() of up_to_date -> halt(0); error -> halt(1) end."

clean:
	rm -f $(EBIN_DIR)/*.beam
	rm -fr ${TEST_DIR}/*.beam
	rm -fr ${TEST_LOG_DIR}/*
	rm -f doc/edoc-info doc/*.html doc/*.css doc/*.png

shell: all
	$(ERL) -pa $(EBIN_DIR)

check: all
	mkdir -p ${TEST_LOG_DIR}
	$(ERL) -pa $(EBIN_DIR) -noinput -eval 'ct:run_test([{logdir, "$(TEST_LOG_DIR)"},{dir, "$(TEST_DIR)"},{suite, ${CT_SUITE}}]), halt(0).'

doc:
	mkdir -p $(CURDIR)/doc
	$(ERL) -noinput -eval 'edoc:application($(APPLICATION), "$(CURDIR)", []), halt(0).'
