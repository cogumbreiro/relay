RELAY_DIR := $(PWD)/..
RELAY := ./relay.sh    # can only execute if working dir is RELAY_DIR
SERVER := ./server.sh
BINS := race_anal.exe server.exe # used as dependencies / freshness check
DUMPER = ./dump-calls.sh
OUT_DIR = out
OLD_OUT = oldout
START_DIR = $(PWD)

SMALL_TESTS = locktest ptr_tests ptr_tests2 thread_tests aget knot esp ide-disk synclink plip mini_linux
LARGE_TESTS = fresh_clam httpd libdb sigtool 

ALL_TESTS = $(SMALL_TESTS) $(LARGE_TESTS)


small: init $(SMALL_TESTS:%=$(OUT_DIR)/%.out) diff_c
large: init $(LARGE_TESTS:%=$(OUT_DIR)/%.out)
all: init $(ALL_TESTS:%=$(OUT_DIR)/%.out)

diff_c:
	$(RELAY_DIR)/scripts/diff_check.sh "SUMS (\|Total Warnings" $(OUT_DIR)/ $(OLD_OUT)/ $(SMALL_TESTS)

#---------------------------------------------------
# Run the tests and make .out / .err logs


init:
	@test -d $(OUT_DIR) || mkdir $(OUT_DIR)


# regenerate when the ciltrees change, or when the binaries change?
$(OUT_DIR)/%.out: %/ciltrees $(BINS:%=$(RELAY_DIR)/%)
	@ echo "testing: " $(PWD)/$@
	@ cd $(RELAY_DIR) ; \
	   $(SERVER) $(START_DIR)/$< > $(START_DIR)/$(basename $@).serv 2>&1 & \
	   sleep 1; $(RELAY) $(START_DIR)/$< \
	   > $(START_DIR)/$@ 2> $(START_DIR)/$(basename $@).err


# regenerate the ciltrees whenever the sources change
# TODO should also do that when the CIL dumper binary changes
%/ciltrees/calls.txt: %/*.c
	cd $(*); $(DUMPER)



#---------------------------------------------------
# Clean up... should it also delete the ciltrees?

.PHONY: clean

clean:
	rm $(OUT_DIR)/*
	$(foreach dir,$(SMALL_TESTS),$(MAKE) --no-print-directory -C $(dir) clean;)
	$(foreach dir,$(LARGE_TESTS),$(MAKE) --no-print-directory -C $(dir) clean;)


renew_small:
	$(foreach dir,$(SMALL_TESTS),rm -rf $(dir)/ciltrees; \
	cd $(dir); $(DUMPER); cd ..)

renew_large:
	$(foreach dir,$(LARGE_TESTS),rm -rf $(dir)/ciltrees; \
	cd $(dir); $(DUMPER); cd ..)

