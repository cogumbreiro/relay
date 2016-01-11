#RUN THE TESTS

RELAY_DIR := $(PWD)/..
RELAY := ./relay.sh    # can only execute if working dir is RELAY_DIR
SERVER := ./server.sh
BINS := race_anal.exe server.exe
CLEANER = make clean
DUMPER = make
OUT_DIR := out
OLD_OUT := oldout
START_DIR = $(PWD)

SMALL_TESTS = simpleone simplelock condlock fplock fp_access ptr_tests ptr_tests2 thread_tests aget knot esp ide-disk synclink plip
LARGE_TESTS = fresh_clam httpd_comb libdb sigtool 

ALL_TESTS = $(SMALL_TESTS) $(LARGE_TESTS)


small: init $(SMALL_TESTS:%=$(OUT_DIR)/%.out) diff_c
large: init $(LARGE_TESTS:%=$(OUT_DIR)/%.out)
all: init $(ALL_TESTS:%=$(OUT_DIR)/%.out)

# force diff_check to run
diff_c:
	@echo "=============================================="
	@echo "CHECKING DIFFS"
	$(RELAY_DIR)/scripts/diff_check.sh "SUMS (\|Total Warnings" $(OUT_DIR)/ $(OLD_OUT)/ $(SMALL_TESTS)


#---------------------------------------------------
# Run the tests and make .out / .err logs


# force output directory to exist
init:
	@test -d $(OUT_DIR) || mkdir $(OUT_DIR)


# regenerate when the ciltrees change, or when the binaries change?
$(OUT_DIR)/%.out: %/ciltrees $(BINS:%=$(RELAY_DIR)/%)
	@ echo "testing: " $(PWD)/$@
	@ cd $(RELAY_DIR) ; \
	   $(SERVER) $(START_DIR)/$< > $(START_DIR)/$(basename $@).serv 2>&1 & \
	   sleep 0.3; $(RELAY) $(START_DIR)/$< \
		 > $(START_DIR)/$@ 2> $(START_DIR)/$(basename $@).err; wait $!

# regenerate the ciltrees whenever the sources change
# TODO should also do that when the CIL binary is recompiled
%/ciltrees/calls.txt: %/*.c
	cd $(*); $(DUMPER)



#---------------------------------------------------
# Clean up individual test dirs (not the output dir)

.PHONY: clean

clean:
	$(foreach dir,$(SMALL_TESTS),cd $(dir); $(CLEANER);cd ..;)

renew_small:
	$(foreach dir,$(SMALL_TESTS),cd $(dir); $(CLEANER); $(DUMPER);cd ..;)

renew_large:
	$(foreach dir,$(LARGE_TESTS),cd $(dir); $(CLEANER); $(DUMPER);cd ..;)

