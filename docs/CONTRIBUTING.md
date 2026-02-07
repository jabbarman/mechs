# Contributing to mechs

Thank you for your interest in contributing to the mechs Computer-Assisted Learning System!

## Ways to Contribute

### 1. Creating Educational Content

The easiest way to contribute is by creating new chapters on any topic. Chapters are defined in simple JSON format.

**Steps:**
1. Copy `data/chapters/_template.json` to a new file
2. Fill in your content following the template structure
3. Test your chapter by running `make run`
4. Submit a pull request

**Content Guidelines:**
- Keep sections focused and digestible
- Use clear, simple language
- Provide immediate feedback in reinforcement sections
- Test questions should have unambiguous answers
- Include examples and real-world applications

### 2. Improving the Code

**Before starting:**
- Check existing issues to avoid duplicate work
- For major changes, open an issue first to discuss
- Follow the existing code style and structure

**Development setup:**
```bash
git clone <repository>
cd mechs
make
./mechs
```

**Code areas that need attention:**
- Enhanced terminal detection and compatibility
- More sophisticated answer matching (partial credit, synonyms)
- Better input validation
- Additional question types (multiple choice, fill-in-blank)
- Progress visualization
- Chapter dependencies and prerequisites

### 3. Documentation

Help improve:
- User documentation
- Chapter creation guide
- API documentation for units
- Architecture diagrams
- Tutorial videos or screenshots

### 4. Testing

- Test on different terminal emulators
- Try different chapter content formats
- Find and report edge cases
- Verify ANSI escape sequences work correctly
- Test with various screen sizes

## Code Style

### Pascal Conventions

```pascal
{ Use descriptive comments for procedures }
procedure MyProcedure(const Param: string);
var
  LocalVar: integer;
begin
  { Implementation }
end;

{ Type names use TPascalCase }
type
  TMyRecord = record
    Field: string;
  end;

{ Constants use UPPER_CASE }
const
  MAX_VALUE = 100;
```

### Unit Organization

- Keep units focused on a single responsibility
- Use meaningful names
- Document public interfaces
- Keep implementation details private

### Git Commit Messages

- Use present tense ("Add feature" not "Added feature")
- Keep first line under 50 characters
- Provide context in the body if needed

Example:
```
Add multiple choice question support

Extends the Content unit to support multiple choice questions
with randomized answer order. Updates UI to display choices
and validate selection.
```

## Pull Request Process

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/my-feature`)
3. Make your changes
4. Test thoroughly (`make clean && make test`)
5. Commit with clear messages
6. Push to your fork
7. Open a pull request with description of changes

## Chapter Format Specification

### Metadata

```json
{
  "id": "unique-identifier",
  "title": "Display Title",
  "author": "Your Name",
  "description": "Brief description (1-2 sentences)",
  "difficulty": 1  // 1-5 scale
}
```

### Section Types

**Introduction:**
```json
{
  "type": "introduction",
  "text": "Overview of chapter content"
}
```

**Text:**
```json
{
  "type": "text",
  "text": "Educational content\nCan span multiple lines"
}
```

**Question/Answer:**
```json
{
  "type": "question",
  "text": "Your question here?"
},
{
  "type": "answer",
  "text": "expected answer"
},
{
  "type": "reinforcement",
  "text": "Feedback explaining the answer"
}
```

### Best Practices

- Questions should immediately follow relevant text sections
- Every question must have an answer section
- Reinforcement is optional but highly recommended
- Keep text sections under 15 lines for readability
- Use clear, unambiguous language
- Test your chapter before submitting

## Questions?

Open an issue or reach out to the maintainers. We're happy to help!
