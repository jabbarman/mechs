# Makefile for mechs CAL System

# Compiler settings
FPC = fpc
FPCFLAGS = -MObjFPC -Sh -O2 -Fi$(UNITS_DIR) -Fu$(UNITS_DIR) -FU$(LIB_DIR)

# Directories
SRC_DIR = src
UNITS_DIR = src/units
LIB_DIR = lib
BIN_DIR = .
DATA_DIR = data

# Target executable
TARGET = $(BIN_DIR)/mechs
MAIN_SOURCE = $(SRC_DIR)/mechs.lpr

# Unit files
UNITS = $(UNITS_DIR)/MechsTypes.pas \
        $(UNITS_DIR)/Terminal.pas \
        $(UNITS_DIR)/Content.pas \
        $(UNITS_DIR)/Diagnostics.pas \
        $(UNITS_DIR)/UI.pas

.PHONY: all clean install uninstall run test dirs

all: dirs $(TARGET)

dirs:
	@mkdir -p $(LIB_DIR)
	@mkdir -p $(DATA_DIR)/chapters

$(TARGET): $(MAIN_SOURCE) $(UNITS)
	$(FPC) $(FPCFLAGS) -o$(TARGET) $(MAIN_SOURCE)

clean:
	rm -f $(TARGET)
	rm -rf $(LIB_DIR)/*
	rm -f $(SRC_DIR)/*.o $(SRC_DIR)/*.ppu
	rm -f $(UNITS_DIR)/*.o $(UNITS_DIR)/*.ppu
	rm -f *.o *.ppu

install: all
	@echo "Installing mechs to /usr/local/bin..."
	@sudo cp $(TARGET) /usr/local/bin/
	@echo "Installation complete!"

uninstall:
	@echo "Removing mechs from /usr/local/bin..."
	@sudo rm -f /usr/local/bin/mechs
	@echo "Uninstallation complete!"

run: all
	@$(TARGET)

test: all
	@echo "Running mechs in test mode..."
	@$(TARGET)

help:
	@echo "mechs CAL System - Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  make          - Build the project"
	@echo "  make clean    - Remove compiled files"
	@echo "  make run      - Build and run the program"
	@echo "  make install  - Install to /usr/local/bin"
	@echo "  make uninstall- Remove from /usr/local/bin"
	@echo "  make test     - Run in test mode"
	@echo "  make help     - Show this help message"
