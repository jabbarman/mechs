unit Terminal;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, MechsTypes;

const
  { ANSI Escape Codes }
  ESC = #27;
  CSI = ESC + '[';
  
  { Colors }
  COLOR_RESET = 0;
  COLOR_BLACK = 30;
  COLOR_RED = 31;
  COLOR_GREEN = 32;
  COLOR_YELLOW = 33;
  COLOR_BLUE = 34;
  COLOR_MAGENTA = 35;
  COLOR_CYAN = 36;
  COLOR_WHITE = 37;

procedure ClearScreen;
procedure GotoXY(X: TXCoord; Y: TYCoord);
procedure DrawLine(Y: TYCoord);
procedure SetColor(ForegroundColor: integer);
procedure ResetColor;
procedure HideCursor;
procedure ShowCursor;
function GetTerminalWidth: integer;
function GetTerminalHeight: integer;

implementation

procedure ClearScreen;
begin
  Write(CSI + '2J');
  Write(CSI + 'H');
end;

procedure GotoXY(X: TXCoord; Y: TYCoord);
begin
  Write(CSI + IntToStr(Y) + ';' + IntToStr(X) + 'H');
end;

procedure DrawLine(Y: TYCoord);
var
  Count: integer;
begin
  GotoXY(5, Y);
  for Count := 1 to 70 do
    Write('─');
end;

procedure SetColor(ForegroundColor: integer);
begin
  Write(CSI + IntToStr(ForegroundColor) + 'm');
end;

procedure ResetColor;
begin
  Write(CSI + IntToStr(COLOR_RESET) + 'm');
end;

procedure HideCursor;
begin
  Write(CSI + '?25l');
end;

procedure ShowCursor;
begin
  Write(CSI + '?25h');
end;

function GetTerminalWidth: integer;
begin
  Result := 80;  // Default, could be enhanced with terminal query
end;

function GetTerminalHeight: integer;
begin
  Result := 24;  // Default, could be enhanced with terminal query
end;

end.
