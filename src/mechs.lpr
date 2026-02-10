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
  SessionScores: TSessionScoreArray;
  PageNumber: integer;

procedure ProcessChapter(const Chapter: TChapter; ChapterIndex: integer);
var
  SectionIndex: integer;
  Section: TChapterSection;
  CurrentY: TYCoord;
  i: integer;
  AnswerLetter: string;
begin
  ClearScreen;
  SplitScreen2;
  DisplayHeader(Chapter.Metadata.Title);
  
  CurrentY := 5;
  PageNumber := 1;
  QuestionsAsked := 0;
  CorrectAnswers := 0;
  StartTime := Now;
  
  { Display page number }
  GotoXY(65, 3);
  Write('Page ', PageNumber);
  
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
          Inc(PageNumber);
          GotoXY(65, 3);
          Write('Page ', PageNumber);
        end;
        
        { Display text with blank line appended at the end }
        DisplayText(Section.Text + #10, CurrentY);
        CurrentY := CurrentY + CountDisplayLines(Section.Text + #10) + 1;
      end;
      
      ctQuestion:
      begin
        { Always start questions on a new page if not already at the top }
        if CurrentY > 5 then
        begin
          DisplayContinuePrompt;
          ClearScreen;
          SplitScreen2;
          DisplayHeader(Chapter.Metadata.Title);
          CurrentY := 5;
          Inc(PageNumber);
          GotoXY(65, 3);
          Write('Page ', PageNumber);
        end;
        
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
          
          { Extract display letter for feedback }
          if Length(Trim(Answer)) > 0 then
            AnswerLetter := UpCase(Trim(Answer)[1])
          else
            AnswerLetter := '?';
          
          { Answer checking - case insensitive trim }
          if Trim(LowerCase(Answer)) = Trim(LowerCase(Chapter.Sections[SectionIndex].Text)) then
          begin
            Inc(CorrectAnswers);
            
            { Correct - use original wording }
            DisplayReinforcement('Yes, ' + AnswerLetter + 
                                ' is the correct answer.');
            
            { Skip the reinforcement section if it exists }
            if (SectionIndex + 1 < Length(Chapter.Sections)) and 
               (Chapter.Sections[SectionIndex + 1].ContentType = ctReinforcement) then
              Inc(SectionIndex);
              
            CurrentY := 5;
            ClearScreen;
            SplitScreen2;
            DisplayHeader(Chapter.Metadata.Title);
            Inc(PageNumber);
            GotoXY(65, 3);
            Write('Page ', PageNumber);
          end
          else
          begin
            { Incorrect - beep and show correct answer with reinforcement }
            Write(#7); { Terminal bell }
            Flush(Output);
            
            if (SectionIndex + 1 < Length(Chapter.Sections)) and 
               (Chapter.Sections[SectionIndex + 1].ContentType = ctReinforcement) then
            begin
              Inc(SectionIndex);
              DisplayReinforcement(AnswerLetter + 
                                  ' is incorrect, the answer is ' + 
                                  Chapter.Sections[SectionIndex - 1].Text + '.' +
                                  #10#10#10 + Chapter.Sections[SectionIndex].Text);
            end
            else
              DisplayReinforcement(AnswerLetter + 
                                  ' is incorrect, the answer is ' + 
                                  Chapter.Sections[SectionIndex].Text + '.');
              
            CurrentY := 5;
            ClearScreen;
            SplitScreen2;
            DisplayHeader(Chapter.Metadata.Title);
            Inc(PageNumber);
            GotoXY(65, 3);
            Write('Page ', PageNumber);
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
  
  { Update session scores }
  SessionScores[ChapterIndex].QuestionsAsked := QuestionsAsked;
  SessionScores[ChapterIndex].CorrectAnswers := CorrectAnswers;
  SessionScores[ChapterIndex].Attempted := True;
  
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
  SplitScreen2;
  DisplayHeader('Introduction');
  
  DisplayText('This program will attempt to teach you some mechanics. ' +
              'Mechanics is a branch of physics which is concerned with ' +
              'the relation of matter to energy.' + #10 +
              #10 +
              'You will be presented with chapters, any of which you may ' +
              'attempt. Each chapter has some pages of text which explain ' +
              'the subject matter.' + #10 +
              #10 +
              'After you have read a chapter you will be asked to try and ' +
              'answer some questions on that chapter. Reinforcement text ' +
              'will be supplied if required.' + #10 +
              #10 +
              'At the end of your session, a breakdown of marks for each ' +
              'chapter attempted will be displayed, together with an ' +
              'overall percentage mark.' + #10 +
              #10 +
              'You may choose to attempt as many or as few chapters as ' +
              'you wish in any one session.', 5);
  
  DisplayContinuePrompt;
  
  ClearScreen;
  SplitScreen2;
  DisplayHeader('Using This Program');
  
  DisplayText('When a chapter is selected from the menu, the text for ' +
              'that chapter will be displayed a page at a time. When you ' +
              'have read each page, you will be prompted to continue.' + #10 +
              #10 +
              'After the text has been presented, you will be asked a ' +
              'number of multiple-choice questions on the chapter. You ' +
              'should answer each question by typing the letter ' +
              'corresponding to your chosen answer.' + #10 +
              #10 +
              'If your answer is correct, you will be told so and the ' +
              'program will continue to the next question. If your answer ' +
              'is incorrect, you will be shown the correct answer and ' +
              'given some reinforcement text explaining it.' + #10 +
              #10 +
              'You may return to the menu and choose another chapter at ' +
              'any time, or you may choose to end the session.', 5);
  
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
    
    { Initialize session scores }
    SetLength(SessionScores, Length(Chapters));
    for Choice := 0 to Length(Chapters) - 1 do
    begin
      SessionScores[Choice].ChapterTitle := Chapters[Choice].Metadata.Title;
      SessionScores[Choice].QuestionsAsked := 0;
      SessionScores[Choice].CorrectAnswers := 0;
      SessionScores[Choice].Attempted := False;
    end;
    
    { Optional introduction }
    ClearScreen;
    SplitScreen2;
    DisplayHeader('mechs - Computer-Assisted Learning');
    GotoXY(10, 10);
    Write('Welcome to the Mechanics CAL System');
    if GetYesNo('Would you like to read the introduction?') then
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
        ProcessChapter(CurrentChapter, Choice - 1);
      end;
    end;
    
    { End-of-session analysis }
    DisplaySessionAnalysis(SessionScores);
    
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
