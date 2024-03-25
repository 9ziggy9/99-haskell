# Project settings
EXECUTABLE = hanoi
SOURCE = hanoi.hs
BUILD_DIR = build

# Compiler settings
GHC = ghc
GHC_FLAGS = -outputdir $(BUILD_DIR) -O2
LIBS = -package gloss

# Default target
all: $(EXECUTABLE)

# Rule for building the executable
$(EXECUTABLE): $(SOURCE)
	mkdir -p $(BUILD_DIR)
	$(GHC) $(GHC_FLAGS) --make $< -o $@ $(LIBS)

# Rule for running the executable
run: $(EXECUTABLE)
	./$(EXECUTABLE)

# Rule for cleaning up
clean:
	rm -rf $(BUILD_DIR) $(EXECUTABLE)

.PHONY: all run clean