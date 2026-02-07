unit Content;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, MechsTypes;

function LoadChapter(const FileName: string): TChapter;
function LoadChaptersFromDirectory(const DirPath: string): TChapterArray;
procedure SaveChapter(const Chapter: TChapter; const FileName: string);

implementation

function LoadChapter(const FileName: string): TChapter;
var
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  SectionsArray: TJSONArray;
  i: integer;
  SectionObj: TJSONObject;
  FileContent: string;
  F: TextFile;
  Line: string;
begin
  AssignFile(F, FileName);
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
    JSONObject := TJSONObject(JSONData);
    
    { Load metadata }
    Result.Metadata.ID := JSONObject.Get('id', '');
    Result.Metadata.Title := JSONObject.Get('title', '');
    Result.Metadata.Author := JSONObject.Get('author', '');
    Result.Metadata.Description := JSONObject.Get('description', '');
    Result.Metadata.Difficulty := JSONObject.Get('difficulty', 1);
    
    { Load sections }
    SectionsArray := JSONObject.Get('sections', TJSONArray(nil)) as TJSONArray;
    if Assigned(SectionsArray) then
    begin
      SetLength(Result.Sections, SectionsArray.Count);
      for i := 0 to SectionsArray.Count - 1 do
      begin
        SectionObj := SectionsArray.Objects[i];
        
        { Parse content type }
        case SectionObj.Get('type', 'text') of
          'text': Result.Sections[i].ContentType := ctText;
          'question': Result.Sections[i].ContentType := ctQuestion;
          'answer': Result.Sections[i].ContentType := ctAnswer;
          'reinforcement': Result.Sections[i].ContentType := ctReinforcement;
          'introduction': Result.Sections[i].ContentType := ctIntroduction;
        else
          Result.Sections[i].ContentType := ctText;
        end;
        
        Result.Sections[i].Text := SectionObj.Get('text', '');
      end;
    end;
  finally
    JSONData.Free;
  end;
end;

function LoadChaptersFromDirectory(const DirPath: string): TChapterArray;
var
  SearchRec: TSearchRec;
  Chapter: TChapter;
  Count: integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  if FindFirst(DirPath + '*.json', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and 
         (Pos('_template', SearchRec.Name) = 0) then
      begin
        try
          Chapter := LoadChapter(DirPath + SearchRec.Name);
          SetLength(Result, Count + 1);
          Result[Count] := Chapter;
          Inc(Count);
        except
          on E: Exception do
            WriteLn('Error loading chapter ', SearchRec.Name, ': ', E.Message);
        end;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

procedure SaveChapter(const Chapter: TChapter; const FileName: string);
var
  JSONObject, SectionObj: TJSONObject;
  SectionsArray: TJSONArray;
  i: integer;
  F: TextFile;
  TypeStr: string;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('id', Chapter.Metadata.ID);
    JSONObject.Add('title', Chapter.Metadata.Title);
    JSONObject.Add('author', Chapter.Metadata.Author);
    JSONObject.Add('description', Chapter.Metadata.Description);
    JSONObject.Add('difficulty', Chapter.Metadata.Difficulty);
    
    SectionsArray := TJSONArray.Create;
    for i := 0 to Length(Chapter.Sections) - 1 do
    begin
      SectionObj := TJSONObject.Create;
      
      case Chapter.Sections[i].ContentType of
        ctText: TypeStr := 'text';
        ctQuestion: TypeStr := 'question';
        ctAnswer: TypeStr := 'answer';
        ctReinforcement: TypeStr := 'reinforcement';
        ctIntroduction: TypeStr := 'introduction';
      end;
      
      SectionObj.Add('type', TypeStr);
      SectionObj.Add('text', Chapter.Sections[i].Text);
      SectionsArray.Add(SectionObj);
    end;
    
    JSONObject.Add('sections', SectionsArray);
    
    AssignFile(F, FileName);
    Rewrite(F);
    WriteLn(F, JSONObject.FormatJSON);
    CloseFile(F);
  finally
    JSONObject.Free;
  end;
end;

end.
