unit MechsTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { Coordinate types for terminal display }
  TXCoord = 1..80;
  TYCoord = 1..24;
  
  { Content types }
  TContentType = (ctText, ctQuestion, ctAnswer, ctReinforcement, ctIntroduction);
  
  { Question types }
  TQuestionType = (qtFreeText, qtMultipleChoice);
  
  { Chapter section record }
  TChapterSection = record
    ContentType: TContentType;
    Text: string;
    QuestionType: TQuestionType;
    Choices: array of string;  // For multiple-choice questions
  end;
  
  { Chapter metadata }
  TChapterMetadata = record
    ID: string;
    Title: string;
    Author: string;
    Description: string;
    Difficulty: integer;
    Prerequisites: array of string;
  end;
  
  { Complete chapter data }
  TChapter = record
    Metadata: TChapterMetadata;
    Sections: array of TChapterSection;
  end;
  
  { Diagnostic record for tracking user performance }
  TDiagnosticData = record
    ChapterID: string;
    QuestionsAsked: integer;
    CorrectAnswers: integer;
    TimeSpent: integer;
    LastAccessed: TDateTime;
  end;
  
  { List types using dynamic arrays }
  TChapterArray = array of TChapter;
  TDiagnosticArray = array of TDiagnosticData;

implementation

end.
