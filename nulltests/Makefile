RELAY_DIR := $(PWD)/..
NULL_ALL := ./null_check.sh
BINS := race_temp_anal.exe nullset_radar.exe nullset_race.exe
CLEANER = make clean
DUMPER = make
OUT_DIR := out
OLD_OUT := oldout
START_DIR = $(PWD)

# what to test
SMALL_TESTS = simple caller_proves all_unlocks all_unlocks2 g i k pldi_prod_cons pldi_prod_cons1 pldi_prod_cons2
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
	   $(NULL_ALL) $(START_DIR)/$</ > /dev/null 2>&1 ; \
	   cp $(START_DIR)/$</log_nullset_rad $(START_DIR)/$@ 


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

