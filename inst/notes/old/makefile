
# Configuration
# -------------
SRC_DIR := rmd
BIN_DIR ?= .

vpath %.Rmd $(SRC_DIR)


# Targets
# -------
TARGETS := README.md

.PHONY : all
all : $(TARGETS) $(patsubst %.md,%.html,$(TARGETS))


# Generic Rules & Recipes
# -----------------------
%.md %.html : %.Rmd
	@echo Building...
	mdr $<

.PHONY : watch
watch :
	@echo Watching \'$(SRC_DIR)\'...
	while true; do inotifywait --event modify $(SRC_DIR); $(MAKE); done

.PHONY : clean
clean :
	@echo Cleaning...
	rm $(TARGETS)
	rm $(patsubst %.md,%.html,$(TARGETS))


.PHONY : rebuild
rebuild : clean all
