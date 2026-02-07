# Running mechs

## Quick Start

```bash
make         # Build the program
./mechs      # Run interactively
```

## Requirements

- **Terminal:** Any ANSI-capable terminal (macOS Terminal, iTerm2, Linux terminals)
- **Interaction:** The program requires keyboard input - it's fully interactive
- **Display:** Works best with 80x24 or larger terminal window

## Expected Behavior

1. **Welcome Screen** - Shows introduction, press any key to continue
2. **Main Menu** - Lists available chapters (currently 1)
3. **Chapter Selection** - Enter number (1 for mechanics, 2 to exit)
4. **Chapter Content** - Interactive learning with questions
5. **Score Display** - Shows performance at end
6. **Return to Menu** - Or exit

## Example Session

```
╔═══════════════════════════════════════════════════════════════╗
║          Computer-Assisted Learning - Mechanics               ║
╚═══════════════════════════════════════════════════════════════╝

   Welcome to mechs CAL System
   
   This program helps you learn fundamental physics concepts
   through interactive chapters and questions.
   
   Press any key to continue...
   
   [After pressing key]
   
╔═══════════════════════════════════════════════════════════════╗
║          Computer-Assisted Learning - Mechanics               ║
╚═══════════════════════════════════════════════════════════════╝

   1. Force of Gravity, Weight and Friction
   
   2. Exit
   
   Enter your choice (1-2): _
```

## Troubleshooting

### Program doesn't start
- Run `make` to ensure it's compiled
- Check `./mechs` exists and is executable
- Verify you're in the mechs directory

### No chapters found
```
Error: No chapters found in data/chapters/
```
**Solution:** 
- Verify `data/chapters/mechanics-01-gravity.json` exists
- Check JSON file is valid (not corrupted)

### ANSI escape sequences show as text
Your terminal doesn't support ANSI. Use a modern terminal:
- macOS: Terminal.app or iTerm2
- Linux: gnome-terminal, konsole, xterm
- Windows: Windows Terminal (Windows 10+)

### Input doesn't work
The program requires an interactive terminal. It won't work with:
- Piped input: `echo "1" | ./mechs` ❌
- Redirected input: `./mechs < input.txt` ❌
- Run directly: `./mechs` ✅

## Creating New Chapters

1. Copy the template:
```bash
cp data/chapters/_template.json data/chapters/my-chapter.json
```

2. Edit the JSON file with your content

3. Run `./mechs` - it will auto-discover the new chapter

See `docs/CONTRIBUTING.md` for detailed chapter format guide.

## Automated Testing

```bash
./test.sh    # Verify build and chapter discovery
```

Note: Full interactive testing requires manual keyboard input.

## Performance Data

Progress is automatically saved to `data/diagnostics.json` after each chapter completion. This tracks:
- Questions attempted
- Correct answers
- Time spent
- Last access date

## Color Support

The program uses ANSI colors:
- **Green** - Chapter numbers
- **Red** - Exit option
- **Cyan** - Headers
- **Yellow** - Prompts

If colors don't display, your terminal may need ANSI color support enabled.
