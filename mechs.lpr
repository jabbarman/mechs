program mechs;
//uses Crt;

{The program for the CAL project to teach 'O' level mechanics.
 Written  :  February - April 1987.
 Author   :  S J Jabbar.                                      }

{constants}

const
  null = 0;
  finish = 7;
  text_flag = '{-T}';
  answer_flag = '{-A}';
  reinforcement_flag = '{-R}';
  end_of_chapter_flag = '{-E}';
  introduction_flag = '{-I}';


{declarations}

type
  options = 0..7;
  diagnosis_rec = record
                   option_no : options;
                   no_of_questions : integer;
                   correct_answers : integer
                  end;
  diagnostic_rec = array[1..6] of diagnosis_rec;
  x_coordinates = 1..80;
  y_coordinates = 1..24;

var
  option : options;
  chapter : text;
  header : string;
  diagnosis : diagnosis_rec;
  diagnostics : diagnostic_rec;
  x : x_coordinates;
  y : y_coordinates;



{procedures}

procedure clear_screen;

{This routine clears the screen and leaves the cursor in the top left
 hand corner.}

begin {clear_screen}
  write(chr(12))    {12 is the ASCII code for clear screen}
  // ClrScr;
end;  {clear_screen}


procedure gotoxy(x : x_coordinates;y : y_coordinates);

{This routine takes the cursor to the 'home' posotion (top left)
 and then uses the coordinates x and y and moves the cursor to
 the line y, cahracter x position on the screen.  The maximum value
 of x is 80 and y is 24.}

var
  count : integer;

begin {gotoxy}
  write(chr(27),'H');
  for count := 1 to y do
      write(chr(27),'B');
  for count := 1 to x do
      write(chr(27),'C')
end;  {gotoxy}


procedure draw_line( y : y_coordinates);

{Using the altenative vharacter set, this routine draws a straight line
 at the specified line number.}

var
  count : integer;

begin
  //write(chr(14));
  gotoxy(5,y);
  for count := 1 to 70 do
      write('⎻');
  //write(chr(15))
end;

begin {main body}
  clear_screen;
  gotoxy(5,10);write('test');
  draw_line(2)
end.  {main body}

