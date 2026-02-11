# mechs - Computer-Assisted Learning System

`mechs` is both a preservation project and a working modern educational program.

The original system was written over 40 years ago in Pascal, developed on a VT100 terminal connected to a mini computer. That original machine, operating system, and compiler environment no longer exist in practical use, making the program effectively impossible to run today without emulating the full historical stack.

This repository is the evolution of that original work. Using AI-assisted interpretation of the preserved historical source, the program has been sympathetically modernized into a maintainable Free Pascal codebase that keeps the terminal-first learning style and educational intent intact.

In short: this is not just a mention of an old program—it is a continuation of it.

## What this repository contains

- A **modern, runnable terminal application** for interactive mechanics learning
- A **preserved copy of the historical source** in `original/`
- A **modular Free Pascal implementation** in `src/`
- A **JSON-based chapter/content system** in `data/chapters/`

## What the modern version does

The modern `mechs` program provides:

- Guided, chapter-based instructional flow
- Question-and-answer interaction with immediate reinforcement
- Basic progress and diagnostic tracking
- ANSI terminal UI for a clean text-first experience
- Extensible content via JSON chapter files

## Features

- **Interactive Learning** - Question-and-answer format with immediate feedback
- **Progress Tracking** - Automatic diagnostics and performance metrics
- **Extensible Content** - Easy-to-author JSON chapter format
- **Terminal UI** - Clean ANSI-based interface with color support
- **Modular Design** - Well-organized codebase with reusable units

## Quick Start

### Build and Run

```bash
make
./mechs
```

Or use:

```bash
make run
```

### Requirements

- Free Pascal Compiler (FPC) 3.0+
- Terminal with ANSI escape sequence support
- macOS, Linux, or BSD (Windows support via Windows Terminal)

## Project Structure

```
mechs/
├── src/                    # Modern source code
│   ├── units/              # Reusable Pascal units
│   │   ├── Types.pas       # Type definitions
│   │   ├── Terminal.pas    # Terminal control
│   │   ├── UI.pas          # User interface
│   │   ├── Content.pas     # Chapter loading
│   │   └── Diagnostics.pas # Performance tracking
│   └── mechs.lpr           # Main program
├── data/                   # Chapter content
│   └── chapters/           # JSON chapter files
├── original/               # 1987 original code (preserved)
├── docs/                   # Documentation
└── Makefile                # Build system
```

## Creating Chapters

Chapters are defined in JSON format. See `data/chapters/_template.json` for the structure.

### Example Chapter

```json
{
  "id": "my-chapter",
  "title": "Chapter Title",
  "author": "Your Name",
  "description": "What this chapter teaches",
  "difficulty": 1,
  "sections": [
    {
      "type": "introduction",
      "text": "Introduction text..."
    },
    {
      "type": "text",
      "text": "Educational content..."
    },
    {
      "type": "question",
      "text": "Your question?"
    },
    {
      "type": "answer",
      "text": "expected answer"
    },
    {
      "type": "reinforcement",
      "text": "Feedback and explanation..."
    }
  ]
}
```

### Section Types

- `introduction` - Chapter overview
- `text` - Educational content
- `question` - Interactive question
- `answer` - Expected answer (follows question)
- `reinforcement` - Feedback after answer

## Building

```bash
# Build the project
make

# Clean build artifacts
make clean

# Build and run
make run

# Install system-wide
make install

# Uninstall
make uninstall
```

## Development

### Adding Features

The codebase is organized into units for easy extension:

- **Terminal.pas** - Add new terminal control functions
- **UI.pas** - Add new UI components or layouts
- **Content.pas** - Extend chapter format or add new content types
- **Diagnostics.pas** - Add analytics or tracking features
- **Types.pas** - Define new data structures

### Coding Conventions

- Use Free Pascal's ObjFPC mode
- Follow Pascal naming conventions (PascalCase for types, camelCase for variables)
- Document public functions and procedures
- Keep units focused on single responsibility

## History

This project began as a Computer-Assisted Learning program written in 1987 by S J Jabbar to teach O-level mechanics. It was developed for a VT100 + mini-computer environment that is now effectively inaccessible.

The original source is preserved in `original/`. The modern implementation in this repository is an AI-assisted reinterpretation and rebuild that aims to preserve the character and pedagogy of the original while making it practical to build, run, extend, and maintain on contemporary systems.

## License

See LICENSE file for details.

## Contributing

See `docs/CONTRIBUTING.md` for guidelines on:
- Creating new chapters
- Adding features
- Reporting issues
- Code style
