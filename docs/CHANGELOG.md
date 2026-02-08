# Changelog

## 2026-02-08 - Multiple-Choice Questions

### Added: Multiple-Choice Question Support
**Feature:** System now supports both free-text and multiple-choice questions, matching the original 1987 implementation.

**Changes:**
- Extended `TChapterSection` with `QuestionType` and `Choices` fields
- Added `TQuestionType` enum: `qtFreeText`, `qtMultipleChoice`
- Updated Content.pas parser to handle `question_type` and `choices` JSON fields
- Modified mechs.lpr to detect and display multiple-choice options automatically
- Backward compatible: existing free-text questions continue to work

**JSON Format:**
```json
{
  "type": "question",
  "question_type": "multiple-choice",
  "text": "Question text?",
  "choices": [
    "A. First option",
    "B. Second option",
    "C. Third option"
  ]
}
```

**Impact:** More authentic to 1987 original, better educational experience with standardized answer format.

### Updated: Chapter 01 - Gravity, Weight and Friction
**Changes:**
- Replaced modern content with original 1987 chapter text
- Converted from 3 free-text questions to 5 multiple-choice questions
- All questions now match original CHAPTER#01 exactly
- Preserved all original reinforcement text

**Topics:**
1. Gravitational force and orbital motion
2. Definition of weight
3. Weight variation in different gravitational fields
4. Static vs. dynamic friction
5. Why friction is reduced in machines

**Impact:** Authentic 1987 educational content, proven pedagogical approach.

### Documentation
- Updated `_template.json` with both question type examples
- Updated CONTRIBUTING.md with multiple-choice format
- Updated ARCHITECTURE.md to explain question type system
- All examples show reinforcement is only for correct answers

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
