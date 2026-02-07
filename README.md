# mechs - Computer-Assisted Learning System

A modern, terminal-based educational software for interactive learning. Originally created in 1987 for teaching O-level mechanics, now rebuilt with modular architecture and extensible content system.

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

This project began as a Computer-Assisted Learning program written in 1987 by S J Jabbar to teach O-level mechanics. The original code (preserved in `original/`) has been completely rebuilt with modern Pascal features while maintaining the educational philosophy and terminal-based approach.

## License

See LICENSE file for details.

## Contributing

See `docs/CONTRIBUTING.md` for guidelines on:
- Creating new chapters
- Adding features
- Reporting issues
- Code style
