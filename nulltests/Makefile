RELAY_DIR := $(PWD)/..
NULL_ALL := ./nullset.sh
DUMPER = $(RELAY_DIR)/scripts/dump-with-stats.sh

# re-run whenever the binaries change
BINS := race_temp_anal.exe nullset_seq.exe nullset_race.exe nullset_adj_l.exe nullset_adj_nl.exe nullset_pess.exe

# where to place results
OUT_DIR := out
OLD_OUT := oldout
START_DIR = $(PWD)

# what to test
SMALL_TESTS = caller_proves all_unlocks g i k l
LARGE_TESTS = 

ALL_TESTS = $(SMALL_TESTS) $(LARGE_TESTS)


small: init $(SMALL_TESTS:%=$(OUT_DIR)/%.out) diff_c
large: init $(LARGE_TESTS:%=$(OUT_DIR)/%.out)
all: init $(ALL_TESTS:%=$(OUT_DIR)/%.out)

# force diff_check to run
diff_c:
	@echo "=============================================="
	@echo "CHECKING DIFFS"
	$(RELAY_DIR)/scripts/diff_check.sh "\- unsafe \|\- safe\|Delta\|SUMS (\|Total Warnings" $(OUT_DIR)/ $(OLD_OUT)/ $(SMALL_TESTS)


#---------------------------------------------------
# Run the tests and make .out / .err logs

# force output directory to exist
init:
	@test -d $(OUT_DIR) || mkdir $(OUT_DIR)


# regenerate when the ciltrees change, or when the binaries change?
$(OUT_DIR)/%.out: %/ciltrees $(BINS:%=$(RELAY_DIR)/%)
	@ echo "testing: " $(PWD)/$@
	@ cd $(RELAY_DIR) ; \
	   $(NULL_ALL) $(START_DIR)/$< > /dev/null 2>&1 ; \
	   mv log $(START_DIR)/$@ 


# regenerate the ciltrees whenever the sources change
# TODO should also do that when the CIL binary is recompiled
%/ciltrees/calls.txt: %/*.c
	cd $(*); $(DUMPER)



#---------------------------------------------------
# Clean up... should it also delete the ciltrees?

.PHONY: clean

clean:
	$(foreach dir,$(SMALL_TESTS),cd $(dir); make clean; cd ..;)

renew_small:
	$(foreach dir,$(SMALL_TESTS),rm -rf $(dir)/ciltrees; \
	cd $(dir); $(DUMPER); cd ..)

renew_large:
	$(foreach dir,$(LARGE_TESTS),rm -rf $(dir)/ciltrees; \
	cd $(dir); $(DUMPER); cd ..)

