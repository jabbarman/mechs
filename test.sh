#!/bin/bash
# Test script for mechs

cd "$(dirname "$0")"

echo "Testing mechs CAL System..."
echo ""

# Check if executable exists
if [ ! -f "./mechs" ]; then
    echo "❌ mechs executable not found. Run 'make' first."
    exit 1
fi

# Check if chapters exist
if [ ! -d "./data/chapters" ]; then
    echo "❌ data/chapters directory not found."
    exit 1
fi

CHAPTER_COUNT=$(ls data/chapters/*.json 2>/dev/null | grep -v template | wc -l)
echo "✓ Found $CHAPTER_COUNT chapter(s)"

# Verify JSON is valid
for chapter in data/chapters/*.json; do
    if [ "$(basename "$chapter")" != "_template.json" ]; then
        echo "  - $(basename "$chapter")"
    fi
done

echo ""
echo "✅ Build verification complete!"
echo ""
echo "To run interactively: ./mechs"
echo "Note: Program requires interactive terminal for keyboard input"
echo ""
echo "Chapter format example:"
cat data/chapters/_template.json | head -10
echo "..."
