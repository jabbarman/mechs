# Changelog

## 2026-02-07 - Usability Improvements

### Fixed: Continue Prompt Behavior
**Issue:** "Press any key to continue..." required Enter key plus another key, and text remained on screen overlapping with subsequent content.

**Changes:**
- Replaced `ReadLn` with `Keyboard.GetKeyEvent()` for true single-keypress input
- Added automatic prompt cleanup - line 22 cleared after keypress
- Updated `DisplayReinforcement` to show prompt on line 23, then clear both lines

**Impact:** Much better user experience - any key works, no Enter needed, clean screen transitions.

### Fixed: Display Issues
**Issue:** Unicode characters (²,×,⋅) displayed as control characters or �

**Changes:**
- Replaced all Unicode with ASCII equivalents in chapter content
- Updated template to guide authors: use `^` for powers, `x` for multiply
- Example: `m/s²` → `m/s^2`

**Impact:** Clean, readable text on all terminals.

### Fixed: Answer Visibility
**Issue:** Answer sections were potentially visible before user input

**Changes:**
- Added explicit case handlers for `ctAnswer` and `ctReinforcement` types
- Answers now strictly internal - used only for comparison
- Reinforcement only shown after user provides answer

**Impact:** Proper quiz flow - no answer spoilers.

## Build System
- Makefile: Standard targets (make, clean, install)
- test.sh: Verification script
- All units compile cleanly with FPC 3.2.2

## Units
- MechsTypes: Core type definitions
- Terminal: ANSI terminal control
- UI: User interface (now with Keyboard unit)
- Content: JSON chapter loading
- Diagnostics: Progress tracking
