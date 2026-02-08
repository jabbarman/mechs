program mechs_original;

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
end;  {clear_screen}


procedure gotoxy(x : x_coordinates;y : y_coordinates);

{This routine takes the cursor to the 'home' posotion (top left)
 and then uses the coordinates x and y and moves the cursor to
 the line y, cahracter x position on the screen.  The maximum value
 of x is 80 and y is 24.}

var
  count : integer;

begin {gotoxy}
  write(chr(27),'H');             {27 is the ASCII code number for ESC, the}
  for count := 1 to y do          {effect of 'ESC H' is the same as 'HOME'.}
      write(chr(27),'B');         {This represents 'ESC B' which is cursor down}
  for count := 1 to x do
      write(chr(27),'C')          {This represents 'ESC C' which is cursor right}
end;  {gotoxy}


procedure draw_line( y : y_coordinates);

{Using the altenative vharacter set, this routine draws a straight line
 at the specified line number.}

var
  count : integer;

begin
  write(chr(14));                  {switches the alternative character set on.}
  gotoxy(5,y);
  for count := 1 to 70 do          {the ' ' is a straight line chracter in    }
      write('⎻');                  {the alt chr set.                          }
  write(chr(15))                   {switches the normal character set on      }
end;

procedure split_screen_1;
{The screen is split at lines 2,4 and 21 in this routine.}

begin {split_screen_1}
  draw_line(2);
  draw_line(4);
  draw_line(21);
end;  {split_screen_1}

procedure split_screen_2;
{The screen is split at lines 3 and 21 in this routine.}

begin {split_screen_2}
  draw_line(2);
  draw_line(21);
end;  {split_screen_2}

procedure continue;
{When this routine is evoked the message in quotes is displayed at line 22.}

var
  ch : char;

begin {continue}
  gotoxy(5,22);write('When you are ready, press any key to continue. ');
  read(ch);
  gotoxy(1,22);writeln(' ':80)
end;  {continue}

procedure display_menu;
{After the screen has been cleared and defined by the lines drawn by
 split_screen_1, this routine displays the chapter titles in this
 project 'syllabus'.}

begin {display_menu}
  clear_screen;
  split_screen_1;
  gotoxy(30,3); writeln('Master Screen');
  gotoxy(1,10);
  writeln('1.  Force of Gravity, Weight and Friction.':59);
  writeln('2.  Speed, Velocity and Acceleration.     ':60);
  writeln('3.  Newtons Laws of Motion.               ':60);
  writeln('4.  Work, Energy and Power                ':60);
  writeln('5.  Machines.                             ':60);
  writeln('6.  Density and relative Density.         ':60);
  writeln;
  writeln('7.  End this session.                     ':60);
end;  {display_menu}

function enumerated(ch : string):integer;
{This function takes the string ch and converts it into an integer.
 It does this by taking the left most character in ch and cinverting
 it to an integer value, then this is multiplied by 10 to the power
 of the number of positions the character is away from the right most
 character.  This process is repeated on all the characters.}

var
  num : integer;
  count : integer;

function power(x,y : integer):integer;
{this function returns the value x to the power of y.}

var
  count : integer;
  dum   : integer;

begin {power}
  dum := 1;
  for count := 1 to y do
      dum : dum * y;
  power := dum
end;  {power}

begin {enumerated}
  num := 0;
  for count := length{ch} downto 1 do
      num := (num + (ord(ch[count])-ord('0'))) * power(10,(count-1));
  enumberated := num;
end;  {enumerated}

procedure choose_option;
{This procedure is called after the menu has been displayed.  It asks
 the user for a number, coresponding to a chapter and validated it.}

var
  number : string;
  valid_option : boolean;
  valid_ans : integer;

begin {choose_option}
  repeat
    gotoxy(1,22);write(' ':80);
    gotoxy(5,22);write('Type in the number of the chapter you require : ');
    readln(number);
    if enumerated(number) in [1..7] then valid_option := true
                                    else valid_option := false;
  until valid_option;
  valid_ans := enumerated(number);
  if valid_ans = 7 then option := finish else option := valid_ans
end  {choose_option};



begin {main body}
  clear_screen;
  gotoxy(5,10);write('test');
  draw_line(2)
end.  {main body}

