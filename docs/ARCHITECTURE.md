# mechs Architecture

## Overview

mechs is a modular, terminal-based Computer-Assisted Learning (CAL) system built with Free Pascal. The architecture emphasizes separation of concerns, extensibility, and maintainability.

## Design Principles

1. **Modularity** - Each unit has a single, well-defined responsibility
2. **Extensibility** - New content and features can be added without modifying core code
3. **Simplicity** - Clear, readable code over clever optimizations
4. **Data-Driven** - Content lives in JSON files, not hardcoded in source

## System Architecture

```
┌─────────────────────────────────────────────────────┐
│                   mechs.lpr                         │
│              (Main Program Loop)                    │
└──────────────────┬──────────────────────────────────┘
                   │
         ┌─────────┴─────────┐
         │                   │
    ┌────▼────┐         ┌───▼────┐
    │   UI    │         │Content │
    │  Unit   │         │  Unit  │
    └────┬────┘         └───┬────┘
         │                  │
    ┌────▼────┐         ┌───▼────────┐
    │Terminal │         │Diagnostics │
    │  Unit   │         │    Unit    │
    └────┬────┘         └───┬────────┘
         │                  │
         └─────────┬────────┘
                   │
              ┌────▼────┐
              │  Types  │
              │  Unit   │
              └─────────┘
```

## Core Units

### Types.pas
**Purpose:** Shared type definitions used across all units

**Key Types:**
- `TChapter` - Complete chapter with metadata and sections
- `TChapterSection` - Individual content section
- `TDiagnosticData` - Performance tracking data
- `TContentType` - Enum for section types

**Dependencies:** None (foundation unit)

### Terminal.pas
**Purpose:** Low-level terminal control using ANSI escape sequences

**Responsibilities:**
- Screen clearing and cursor positioning
- Color output
- Line drawing
- Terminal size detection

**Key Functions:**
- `ClearScreen()` - Clears display
- `GotoXY(x, y)` - Position cursor
- `SetColor(color)` - Change text color
- `DrawLine(y)` - Draw horizontal line

**Dependencies:** Types

### UI.pas
**Purpose:** High-level user interface components

**Responsibilities:**
- Menu display and navigation
- Text presentation
- User input collection
- Screen layout management

**Key Functions:**
- `DisplayMenu(chapters)` - Show chapter selection
- `GetMenuChoice(max)` - Get validated user choice
- `DisplayText(text, y)` - Show formatted text
- `GetAnswer(question)` - Collect answer input

**Dependencies:** Types, Terminal

### Content.pas
**Purpose:** Chapter content loading and parsing

**Responsibilities:**
- JSON chapter file parsing
- Chapter discovery and loading
- Content validation
- Format conversion

**Key Functions:**
- `LoadChapter(filename)` - Load single chapter from JSON
- `LoadChaptersFromDirectory(path)` - Discover and load all chapters
- `SaveChapter(chapter, filename)` - Save chapter to JSON

**Dependencies:** Types, fpjson

**JSON Format:**
```json
{
  "id": "chapter-id",
  "title": "Chapter Title",
  "sections": [
    {
      "type": "text|question|answer|reinforcement|introduction",
      "text": "Content..."
    }
  ]
}
```

### Diagnostics.pas
**Purpose:** User progress tracking and performance analytics

**Responsibilities:**
- Load/save diagnostic data
- Update performance metrics
- Track chapter completion
- Calculate scores

**Key Functions:**
- `LoadDiagnostics()` - Load saved progress
- `SaveDiagnostics(data)` - Persist progress
- `UpdateDiagnostic(data, ...)` - Update metrics
- `GetDiagnostic(list, id)` - Retrieve chapter stats

**Dependencies:** Types, fpjson

**Storage:** `data/diagnostics.json`

## Main Program Flow

```
┌─────────────┐
│   Start     │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ Load Content│
│ & Progress  │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Show      │◄───────┐
│Introduction │        │
└──────┬──────┘        │
       │               │
       ▼               │
┌─────────────┐        │
│   Display   │        │
│    Menu     │        │
└──────┬──────┘        │
       │               │
       ▼               │
┌─────────────┐        │
│  Get User   │        │
│   Choice    │        │
└──────┬──────┘        │
       │               │
       ├─Exit?─────────┤
       │               │
       ▼               │
┌─────────────┐        │
│  Process    │        │
│  Chapter    │        │
└──────┬──────┘        │
       │               │
       ▼               │
┌─────────────┐        │
│   Update    │        │
│ Diagnostics │        │
└──────┬──────┘        │
       │               │
       └───────────────┘
```

## Chapter Processing Flow

1. **Load chapter** - Parse JSON into memory
2. **Display header** - Show chapter title
3. **Iterate sections:**
   - **Text/Introduction** - Display and optionally pause
   - **Question** - Show question, collect answer
   - **Answer** - Validate response
   - **Reinforcement** - Show feedback
4. **Calculate score** - Questions asked vs. correct
5. **Update diagnostics** - Save progress
6. **Return to menu**

## Data Flow

```
JSON Files ──→ Content.LoadChapter() ──→ TChapter record
                                            │
                                            ▼
                                      Main Program
                                            │
                                            ▼
                                    UI.DisplayText()
                                            │
                                            ▼
                                    Terminal.GotoXY()
                                            │
                                            ▼
                                       Screen Output
```

## Extensibility Points

### Adding New Section Types

1. Add enum value to `TContentType` in Types.pas
2. Update JSON parsing in Content.pas
3. Add display logic in main program
4. Update chapter template

### Adding New Question Types

1. Define new content type
2. Extend TChapterSection with optional fields
3. Implement rendering in UI.pas
4. Update answer validation logic

### Adding New Analytics

1. Extend TDiagnosticData in Types.pas
2. Update save/load in Diagnostics.pas
3. Add calculation logic in main program
4. Display in completion screen

## Terminal Compatibility

The system uses standard ANSI escape sequences:
- `ESC[2J` - Clear screen
- `ESC[y;xH` - Position cursor
- `ESC[Nm` - Set color (N = color code)
- `ESC[?25l/h` - Hide/show cursor

Compatible with:
- macOS Terminal
- iTerm2
- Linux terminal emulators (xterm, gnome-terminal, etc.)
- Windows Terminal (Windows 10+)

## Future Enhancements

### Planned Features
- Multiple choice questions
- Fill-in-the-blank questions
- Chapter prerequisites
- Progress visualization
- Answer fuzzy matching
- Configurable themes
- Audio/video content support (URLs)

### Technical Improvements
- Unit tests
- Terminal size auto-detection
- Ncurses integration option
- SQLite diagnostics storage
- Chapter validation tool
- Content editor tool

## Performance Considerations

- Chapters loaded on-demand (lazy loading possible)
- JSON parsing happens at startup
- Diagnostics saved after each chapter
- Terminal updates are immediate (no buffering layer)

The current architecture prioritizes simplicity and maintainability over performance. For typical educational use (< 100 chapters, < 1000 questions), performance is more than adequate.
