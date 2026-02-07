# Original mechs CAL Program (1987)

This directory contains the original Computer-Assisted Learning (CAL) program for teaching O-level mechanics, preserved as a historical artifact.

## Historical Context

**Written:** February - April 1987  
**Author:** S J Jabbar  
**Purpose:** Interactive tutorial system for O-level mechanics education  
**Platform:** Originally written for terminals with 80x24 character displays

## Files

- `mechs_original.pas` - The complete original source code
- `mechs_original.lpi` - Lazarus project file (for reference)
- `mechs_original.lps` - Lazarus session file (for reference)
- `mechs.lpr` / `mechs.lpi` / `mechs.lps` - Early modernization attempts (preserved for history)

## Key Features of the Original

### Educational Structure
- 6 mechanics chapters covering fundamental physics concepts:
  1. Force of Gravity, Weight and Friction
  2. Speed, Velocity and Acceleration
  3. Newton's Laws of Motion
  4. Work, Energy and Power
  5. Machines
  6. Density and Relative Density

### Technical Implementation
- **Terminal Control:** Direct ANSI escape sequences for cursor positioning
- **Screen Layout:** Fixed 80x24 grid with split-screen design
- **Content Format:** External `CHAPTER#0X` files with embedded flags
- **Diagnostics:** Tracked questions attempted and correct answers per chapter

### Content Flags
The program used special markers in chapter files:
- `{-T}` - Text section
- `{-A}` - Answer prompt
- `{-R}` - Reinforcement/feedback
- `{-E}` - End of chapter
- `{-I}` - Introduction

## Building the Original

To compile and run the original program:

```bash
cd original
fpc mechs_original.pas -o mechs_original
./mechs_original
```

**Note:** The program expects `CHAPTER#01` through `CHAPTER#06` files to exist in the current directory. These content files are not included in this repository.

## Preservation Philosophy

**DO NOT MODIFY** files in this directory. This is a museum piece preserving 1987 computing history. All modern development happens in the `src/` directory.

The original code demonstrates:
- Pascal programming style of the 1980s
- Terminal-based user interface design
- Educational software patterns from the pre-GUI era
- Character-cell display techniques

## For Modern Development

See the `src/` directory for the modern, modular rewrite that builds upon these original concepts while using contemporary software engineering practices.
