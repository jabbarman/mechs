program mechs;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, MechsTypes, Terminal, UI, Content, Diagnostics;

var
  Chapters: TChapterArray;
  DiagnosticsData: TDiagnosticArray;
  CurrentChapter: TChapter;
  CurrentDiagnostic: TDiagnosticData;
  Choice: integer;
  Running: boolean;
  Answer: string;
  QuestionsAsked, CorrectAnswers: integer;
  StartTime: TDateTime;

procedure ProcessChapter(const Chapter: TChapter);
var
  SectionIndex: integer;
  Section: TChapterSection;
  CurrentY: TYCoord;
  i: integer;
begin
  ClearScreen;
  SplitScreen2;
  DisplayHeader(Chapter.Metadata.Title);
  
  CurrentY := 5;
  QuestionsAsked := 0;
  CorrectAnswers := 0;
  StartTime := Now;
  
  SectionIndex := 0;
  while SectionIndex < Length(Chapter.Sections) do
  begin
    Section := Chapter.Sections[SectionIndex];
    
    case Section.ContentType of
      ctIntroduction, ctText:
      begin
        { Check if there's enough space for this text section }
        if CurrentY + CountDisplayLines(Section.Text) > 18 then
        begin
          { Not enough space, show continue prompt and clear screen }
          DisplayContinuePrompt;
          ClearScreen;
          SplitScreen2;
          DisplayHeader(Chapter.Metadata.Title);
          CurrentY := 5;
        end;
        
        DisplayText(Section.Text, CurrentY);
        CurrentY := CurrentY + CountDisplayLines(Section.Text) + 1;
      end;
      
      ctQuestion:
      begin
        { Display question text }
        DisplayText(Section.Text, CurrentY);
        Inc(QuestionsAsked);
        
        { If multiple-choice, display choices }
        if Section.QuestionType = qtMultipleChoice then
        begin
          Inc(CurrentY, 2);
          for i := 0 to Length(Section.Choices) - 1 do
          begin
            GotoXY(5, CurrentY);
            WriteLn(Section.Choices[i]);
            Flush(Output);
            Inc(CurrentY);
          end;
          Inc(CurrentY);
        end;
        
        { Get next section which should be the answer }
        if (SectionIndex + 1 < Length(Chapter.Sections)) and 
           (Chapter.Sections[SectionIndex + 1].ContentType = ctAnswer) then
        begin
          { Get user's answer }
          Answer := GetAnswer('');
          
          { Move to answer section to compare (but don't display it) }
          Inc(SectionIndex);
          
          { Answer checking - case insensitive trim }
          if Trim(LowerCase(Answer)) = Trim(LowerCase(Chapter.Sections[SectionIndex].Text)) then
          begin
            Inc(CorrectAnswers);
            
            { Check for reinforcement - show if answer was correct }
            if (SectionIndex + 1 < Length(Chapter.Sections)) and 
               (Chapter.Sections[SectionIndex + 1].ContentType = ctReinforcement) then
            begin
              Inc(SectionIndex);
              { Add "Excellent!" prefix to reinforcement for correct answers }
              DisplayReinforcement('Excellent!'#10#10 + Chapter.Sections[SectionIndex].Text);
              CurrentY := 5;
              ClearScreen;
              SplitScreen2;
              DisplayHeader(Chapter.Metadata.Title);
            end;
          end
          else
          begin
            { Answer was incorrect - show generic feedback }
            DisplayReinforcement('Not quite right. The correct answer was: ' + 
                                Chapter.Sections[SectionIndex].Text + 
                                #10#10'Let''s continue...');
            
            { Skip the reinforcement section if it exists (it's for correct answers) }
            if (SectionIndex + 1 < Length(Chapter.Sections)) and 
               (Chapter.Sections[SectionIndex + 1].ContentType = ctReinforcement) then
              Inc(SectionIndex);
              
            CurrentY := 5;
            ClearScreen;
            SplitScreen2;
            DisplayHeader(Chapter.Metadata.Title);
          end;
        end;
      end;
      
      ctAnswer, ctReinforcement:
      begin
        { These are handled by ctQuestion, skip if encountered standalone }
      end;
    end;
    Inc(SectionIndex);
  end;
  
  { Update diagnostics }
  CurrentDiagnostic := GetDiagnostic(DiagnosticsData, Chapter.Metadata.ID);
  UpdateDiagnostic(CurrentDiagnostic, QuestionsAsked, CorrectAnswers, 
                   SecondsBetween(Now, StartTime));
  SetDiagnostic(DiagnosticsData, CurrentDiagnostic);
  SaveDiagnostics(DiagnosticsData);
  
  { Show completion message }
  ClearScreen;
  SplitScreen2;
  DisplayHeader('Chapter Complete!');
  GotoXY(10, 10);
  Write('Questions attempted: ', QuestionsAsked);
  GotoXY(10, 11);
  Write('Correct answers: ', CorrectAnswers);
  if QuestionsAsked > 0 then
  begin
    GotoXY(10, 12);
    Write('Score: ', (CorrectAnswers * 100) div QuestionsAsked, '%');
  end;
  DisplayContinuePrompt;
end;

procedure ShowIntroduction;
begin
  ClearScreen;
  SplitScreen1;
  DisplayHeader('Welcome to mechs CAL System');
  
  GotoXY(10, 7);
  Write('Computer-Assisted Learning for Mechanics');
  GotoXY(10, 9);
  Write('This program helps you learn fundamental physics concepts');
  GotoXY(10, 10);
  Write('through interactive chapters and questions.');
  GotoXY(10, 12);
  Write('Select a chapter from the menu to begin.');
  
  DisplayContinuePrompt;
end;

begin
  { Initialize }
  HideCursor;
  try
    { Load chapters and diagnostics }
    Chapters := LoadChaptersFromDirectory('data/chapters/');
    DiagnosticsData := LoadDiagnostics;
    
    if Length(Chapters) = 0 then
    begin
      ClearScreen;
      GotoXY(5, 10);
      SetColor(COLOR_RED);
      WriteLn('Error: No chapters found in data/chapters/');
      WriteLn('Please add chapter JSON files to the data/chapters/ directory.');
      ResetColor;
      ShowCursor;
      Exit;
    end;
    
    ShowIntroduction;
    
    { Main loop }
    Running := True;
    while Running do
    begin
      DisplayMenu(Chapters);
      Choice := GetMenuChoice(Length(Chapters) + 1);
      
      if Choice = Length(Chapters) + 1 then
        Running := False
      else
      begin
        CurrentChapter := Chapters[Choice - 1];
        ProcessChapter(CurrentChapter);
      end;
    end;
    
    { Cleanup }
    ClearScreen;
    GotoXY(30, 12);
    SetColor(COLOR_CYAN);
    WriteLn('Thank you for using mechs!');
    ResetColor;
  finally
    ShowCursor;
  end;
end.
