# Original mechs CAL Program (1987)

This directory contains the original Computer-Assisted Learning (CAL) program for teaching O-level mechanics, preserved as a historical artifact.

## Historical Context

**Written:** February - April 1987  
**Author:** S J Jabbar  
**Institution:** The Polytechnic of Wales (now University of South Wales)  
**Purpose:** Final year project - Interactive tutorial system for O-level mechanics education  
**Platform:** Originally written for CP/M systems with 80x24 character displays

## Directory Structure

```
original/
├── source/          # 1987 Pascal source code
├── data/            # Original runtime data files  
├── docs/            # Project documentation
│   ├── MECHS_merged_canonical.pdf    # Scanned original documentation
│   └── reconstruction/                # Markdown transcription
└── README.md        # This file
```

### source/
Contains the original Pascal source code:
- `mechs_original.pas` - The complete original program
- `mechs_original.lpi/lps` - Lazarus project files (for modern editing)
- `mechs.lpr/lpi/lps` - Early modernization attempts (preserved for history)

### data/
Contains the original runtime data files used by the program:
- `INTRO.TEX` - Introduction screen text displayed on startup
- `CHAPTER#01` - Sample chapter content (Force of Gravity, Weight and Friction)

The program expected these files in specific formats with embedded control flags.

### docs/
Contains the complete project documentation:
- **MECHS_merged_canonical.pdf** - High-resolution scans of the original project report
- **reconstruction/** - Markdown transcription of the documentation for easier navigation

See [`docs/reconstruction/README.md`](docs/reconstruction/README.md) for the complete reading guide.

## Key Features of the Original

### Educational Structure
The program covered 6 mechanics chapters:
1. Force of Gravity, Weight and Friction
2. Speed, Velocity and Acceleration
3. Newton's Laws of Motion
4. Work, Energy and Power
5. Machines
6. Density and Relative Density

### Technical Implementation
- **Terminal Control:** Direct ANSI escape sequences for cursor positioning
- **Screen Layout:** Fixed 80x24 grid with split-screen design
- **Content Format:** External data files with embedded control flags:
  - `{-T}` - Text section
  - `{-A}` - Answer prompt
  - `{-R}` - Reinforcement/feedback
  - `{-E}` - End of chapter
  - `{-I}` - Introduction
- **Diagnostics:** Tracked questions attempted and correct answers per chapter

### Architecture
The original was a monolithic program with all functionality in a single Pascal file. Key procedures included:
- Screen management (`clear_screen`, `gotoxy`, `draw_line`, `split_screen`)
- Menu display and navigation
- File handling for chapter data
- Question/answer processing
- Performance tracking

## Running the Original

To compile and run the original program:

```bash
cd original/source
fpc mechs_original.pas -o mechs_original
./mechs_original
```

**Note:** The program expects `INTRO.TEX` and `CHAPTER#01-06` files to exist in the current directory. Only `INTRO.TEX` and `CHAPTER#01` are included in this repository.

## Preservation Philosophy

**DO NOT MODIFY** files in this directory. This is a museum piece preserving 1987 computing history and educational software design.

The original code demonstrates:
- Pascal programming style of the 1980s
- Terminal-based user interface design
- Educational software patterns from the pre-GUI era
- Character-cell display techniques
- Computer-Assisted Learning (CAL) pedagogical approaches

## For Modern Development

The modern implementation is in the main repository (`src/` directory). It's a complete architectural rewrite using:
- Modular unit-based design
- JSON-based chapter format
- Modern Pascal features
- Extensible architecture

See the main [`README.md`](../README.md) for details about the modern version.

## Documentation

For a deep dive into the original project's design, implementation, and evaluation:
1. Read the [project documentation](docs/reconstruction/README.md)
2. Examine the [original source code](source/mechs_original.pas)
3. Compare with the [modern implementation](../src/)

This provides valuable insights into how educational software has evolved over nearly 40 years.
