unit UI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Keyboard, MechsTypes, Terminal;

procedure SplitScreen1;
procedure SplitScreen2;
procedure DisplayHeader(const Title: string);
procedure DisplayMenu(const Chapters: TChapterArray);
procedure DisplayContinuePrompt;
function GetMenuChoice(MaxChoice: integer): integer;
function GetYesNo(const Prompt: string): boolean;
procedure DisplayText(const Text: string; StartY: TYCoord);
function CountDisplayLines(const Text: string): integer;
function GetAnswer(const Question: string): string;
procedure DisplayReinforcement(const Text: string);

implementation

procedure SplitScreen1;
begin
  DrawLine(2);
  DrawLine(4);
  DrawLine(21);
end;

procedure SplitScreen2;
begin
  DrawLine(2);
  DrawLine(21);
end;

procedure DisplayHeader(const Title: string);
var
  StartX: integer;
begin
  StartX := (80 - Length(Title)) div 2;
  GotoXY(StartX, 3);
  SetColor(COLOR_CYAN);
  Write(Title);
  ResetColor;
end;

procedure DisplayMenu(const Chapters: TChapterArray);
var
  i: integer;
  YPos: integer;
begin
  ClearScreen;
  SplitScreen1;
  DisplayHeader('Computer-Assisted Learning - Mechanics');
  
  YPos := 6;
  for i := 0 to Length(Chapters) - 1 do
  begin
    GotoXY(10, YPos);
    SetColor(COLOR_GREEN);
    Write(IntToStr(i + 1), '. ');
    ResetColor;
    Write(Chapters[i].Metadata.Title);
    Inc(YPos);
  end;
  
  Inc(YPos);
  GotoXY(10, YPos);
  SetColor(COLOR_RED);
  Write(IntToStr(Length(Chapters) + 1), '. ');
  ResetColor;
  Write('Exit');
end;

procedure DisplayContinuePrompt;
var
  Key: TKeyEvent;
begin
  { Always display on line 22, column 5 }
  GotoXY(5, 22);
  SetColor(COLOR_YELLOW);
  Write('Press any key to continue...');
  ResetColor;
  Flush(Output);  { Force display before waiting for key }
  
  { Initialize keyboard }
  InitKeyboard;
  Key := GetKeyEvent;
  Key := TranslateKeyEvent(Key);
  DoneKeyboard;
  
  { Clear line 22 }
  GotoXY(1, 22);
  Write(StringOfChar(' ', 80));
  Flush(Output);  { Force the clear to display }
end;

function GetMenuChoice(MaxChoice: integer): integer;
var
  Choice: string;
  ValidChoice: boolean;
  ChoiceNum: integer;
begin
  repeat
    GotoXY(1, 22);
    Write(StringOfChar(' ', 80));
    GotoXY(5, 22);
    Write('Enter your choice (1-', MaxChoice, '): ');
    ReadLn(Choice);
    
    ValidChoice := TryStrToInt(Choice, ChoiceNum);
    if ValidChoice then
      ValidChoice := (ChoiceNum >= 1) and (ChoiceNum <= MaxChoice);
      
    if not ValidChoice then
    begin
      GotoXY(5, 23);
      SetColor(COLOR_RED);
      Write('Invalid choice. Please try again.');
      ResetColor;
    end;
  until ValidChoice;
  
  GotoXY(1, 23);
  Write(StringOfChar(' ', 80));
  Result := ChoiceNum;
end;

function GetYesNo(const Prompt: string): boolean;
var
  Response: string;
begin
  GotoXY(5, 22);
  Write(Prompt, ' (y/n): ');
  ReadLn(Response);
  Result := (Length(Response) > 0) and (UpCase(Response[1]) = 'Y');
end;

procedure DisplayText(const Text: string; StartY: TYCoord);
var
  Lines: TStringList;
  i, j: integer;
  CurrentY: TYCoord;
  Line: string;
  WrappedLines: TStringList;
  MaxWidth: integer;
begin
  MaxWidth := 70; { Maximum line width for content area }
  Lines := TStringList.Create;
  WrappedLines := TStringList.Create;
  try
    Lines.Text := Text;
    
    { Wrap each line to MaxWidth }
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      while Length(Line) > MaxWidth do
      begin
        { Find last space before MaxWidth }
        j := MaxWidth;
        while (j > 0) and (Line[j] <> ' ') do
          Dec(j);
        
        if j = 0 then
          j := MaxWidth; { No space found, break at MaxWidth }
        
        WrappedLines.Add(Copy(Line, 1, j));
        Delete(Line, 1, j);
        Line := TrimLeft(Line); { Remove leading spaces from remainder }
      end;
      if Length(Line) > 0 then
        WrappedLines.Add(Line);
    end;
    
    CurrentY := StartY;
    for i := 0 to WrappedLines.Count - 1 do
    begin
      if CurrentY > 20 then
        Break;
      GotoXY(5, CurrentY);
      Write(WrappedLines[i]);
      Inc(CurrentY);
    end;
  finally
    Lines.Free;
    WrappedLines.Free;
  end;
end;

{ Returns how many lines the text will occupy when displayed }
function CountDisplayLines(const Text: string): integer;
var
  Lines: TStringList;
  i, j: integer;
  Line: string;
  MaxWidth: integer;
  Count: integer;
begin
  MaxWidth := 70;
  Count := 0;
  Lines := TStringList.Create;
  try
    Lines.Text := Text;
    
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      if Length(Line) = 0 then
      begin
        Inc(Count);
        Continue;
      end;
      
      while Length(Line) > MaxWidth do
      begin
        j := MaxWidth;
        while (j > 0) and (Line[j] <> ' ') do
          Dec(j);
        if j = 0 then
          j := MaxWidth;
        Inc(Count);
        Delete(Line, 1, j);
        Line := TrimLeft(Line);
      end;
      if Length(Line) > 0 then
        Inc(Count);
    end;
  finally
    Lines.Free;
  end;
  Result := Count;
end;

function GetAnswer(const Question: string): string;
begin
  GotoXY(5, 22);
  Write(Question);
  GotoXY(5, 23);
  Write('Your answer: ');
  ReadLn(Result);
  
  { Clear the lines }
  GotoXY(1, 22);
  Write(StringOfChar(' ', 80));
  GotoXY(1, 23);
  Write(StringOfChar(' ', 80));
end;

procedure DisplayReinforcement(const Text: string);
var
  Lines: TStringList;
  WrappedLines: TStringList;
  i, j: integer;
  CurrentY: TYCoord;
  Line: string;
  MaxWidth: integer;
begin
  { Clear content area (lines 5-20) and prompt area (lines 22-23) }
  for i := 5 to 20 do
  begin
    GotoXY(1, i);
    Write(StringOfChar(' ', 80));
  end;
  GotoXY(1, 22);
  Write(StringOfChar(' ', 80));
  GotoXY(1, 23);
  Write(StringOfChar(' ', 80));
  Flush(Output);
  
  { Display reinforcement text on lines 5-20 with word wrapping }
  MaxWidth := 70;
  Lines := TStringList.Create;
  WrappedLines := TStringList.Create;
  try
    Lines.Text := Text;
    
    { Wrap each line to MaxWidth }
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Lines[i];
      while Length(Line) > MaxWidth do
      begin
        { Find last space before MaxWidth }
        j := MaxWidth;
        while (j > 0) and (Line[j] <> ' ') do
          Dec(j);
        
        if j = 0 then
          j := MaxWidth; { No space found, break at MaxWidth }
        
        WrappedLines.Add(Copy(Line, 1, j));
        Delete(Line, 1, j);
        Line := TrimLeft(Line); { Remove leading spaces from remainder }
      end;
      if Length(Line) > 0 then
        WrappedLines.Add(Line);
    end;
    
    CurrentY := 5;
    SetColor(COLOR_GREEN);
    for i := 0 to WrappedLines.Count - 1 do
    begin
      if CurrentY > 20 then
        Break;
      GotoXY(5, CurrentY);
      Write(WrappedLines[i]);
      Inc(CurrentY);
    end;
    ResetColor;
    Flush(Output);
  finally
    Lines.Free;
    WrappedLines.Free;
  end;
  
  { Display prompt on line 22 }
  DisplayContinuePrompt;
  
  { Clear content and prompt lines after continue }
  for i := 5 to 23 do
  begin
    GotoXY(1, i);
    Write(StringOfChar(' ', 80));
  end;
  Flush(Output);
end;

end.
