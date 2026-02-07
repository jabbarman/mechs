unit Diagnostics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, MechsTypes;

const
  DIAGNOSTICS_FILE = 'data/diagnostics.json';

function LoadDiagnostics: TDiagnosticArray;
procedure SaveDiagnostics(var Diagnostics: TDiagnosticArray);
procedure UpdateDiagnostic(var Diagnostic: TDiagnosticData; QuestionsAsked, CorrectAnswers, TimeSpent: integer);
function GetDiagnostic(const Diagnostics: TDiagnosticArray; const ChapterID: string): TDiagnosticData;
procedure SetDiagnostic(var Diagnostics: TDiagnosticArray; const Diagnostic: TDiagnosticData);

implementation

function LoadDiagnostics: TDiagnosticArray;
var
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  i: integer;
  Diagnostic: TDiagnosticData;
  FileContent: string;
  F: TextFile;
  Line: string;
begin
  SetLength(Result, 0);
  
  if not FileExists(DIAGNOSTICS_FILE) then
    Exit;
    
  try
    AssignFile(F, DIAGNOSTICS_FILE);
    Reset(F);
    FileContent := '';
    while not EOF(F) do
    begin
      ReadLn(F, Line);
      FileContent := FileContent + Line;
    end;
    CloseFile(F);
    
    JSONData := GetJSON(FileContent);
    try
      JSONArray := TJSONArray(JSONData);
      SetLength(Result, JSONArray.Count);
      for i := 0 to JSONArray.Count - 1 do
      begin
        JSONObject := JSONArray.Objects[i];
        Diagnostic.ChapterID := JSONObject.Get('chapter_id', '');
        Diagnostic.QuestionsAsked := JSONObject.Get('questions_asked', 0);
        Diagnostic.CorrectAnswers := JSONObject.Get('correct_answers', 0);
        Diagnostic.TimeSpent := JSONObject.Get('time_spent', 0);
        Diagnostic.LastAccessed := Now;
        Result[i] := Diagnostic;
      end;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error loading diagnostics: ', E.Message);
  end;
end;

procedure SaveDiagnostics(var Diagnostics: TDiagnosticArray);
var
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  i: integer;
  F: TextFile;
  Diagnostic: TDiagnosticData;
begin
  JSONArray := TJSONArray.Create;
  try
    for i := 0 to Length(Diagnostics) - 1 do
    begin
      Diagnostic := Diagnostics[i];
      JSONObject := TJSONObject.Create;
      JSONObject.Add('chapter_id', Diagnostic.ChapterID);
      JSONObject.Add('questions_asked', Diagnostic.QuestionsAsked);
      JSONObject.Add('correct_answers', Diagnostic.CorrectAnswers);
      JSONObject.Add('time_spent', Diagnostic.TimeSpent);
      JSONArray.Add(JSONObject);
    end;
    
    AssignFile(F, DIAGNOSTICS_FILE);
    Rewrite(F);
    WriteLn(F, JSONArray.FormatJSON);
    CloseFile(F);
  finally
    JSONArray.Free;
  end;
end;

procedure UpdateDiagnostic(var Diagnostic: TDiagnosticData; QuestionsAsked, CorrectAnswers, TimeSpent: integer);
begin
  Diagnostic.QuestionsAsked := Diagnostic.QuestionsAsked + QuestionsAsked;
  Diagnostic.CorrectAnswers := Diagnostic.CorrectAnswers + CorrectAnswers;
  Diagnostic.TimeSpent := Diagnostic.TimeSpent + TimeSpent;
  Diagnostic.LastAccessed := Now;
end;

function GetDiagnostic(const Diagnostics: TDiagnosticArray; const ChapterID: string): TDiagnosticData;
var
  i: integer;
begin
  for i := 0 to Length(Diagnostics) - 1 do
  begin
    if Diagnostics[i].ChapterID = ChapterID then
    begin
      Result := Diagnostics[i];
      Exit;
    end;
  end;
  
  { Create new diagnostic if not found }
  Result.ChapterID := ChapterID;
  Result.QuestionsAsked := 0;
  Result.CorrectAnswers := 0;
  Result.TimeSpent := 0;
  Result.LastAccessed := Now;
end;

procedure SetDiagnostic(var Diagnostics: TDiagnosticArray; const Diagnostic: TDiagnosticData);
var
  i: integer;
begin
  for i := 0 to Length(Diagnostics) - 1 do
  begin
    if Diagnostics[i].ChapterID = Diagnostic.ChapterID then
    begin
      Diagnostics[i] := Diagnostic;
      Exit;
    end;
  end;
  
  { Add if not found }
  SetLength(Diagnostics, Length(Diagnostics) + 1);
  Diagnostics[Length(Diagnostics) - 1] := Diagnostic;
end;

end.
