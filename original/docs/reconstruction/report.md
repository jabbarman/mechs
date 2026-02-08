# THE REPORT

## 1. Introduction

### 1.1 What is CAL?

CAL is an acronym, short for Computer Assisted (or Aided) Learning. A
similar term, CAI (Computer Assisted Instruction), is also used but, for
the most part, only in the United States. Other related terms are CBL,
CBE, and CBT (Computer Based Learning/Education/Training). For the
purposes of this report the term CAL shall be used.

CAL is based on the formal approach method of teaching. This method was
first discovered by Socrates, but it was not for another two thousand
years that the possibilities of this method were investigated further.
In the 1950s the research of B. F. Skinner at Harvard University yielded
methodical teaching again in the form of Programmed Learning (PL).
Although books were published, methods were devised, and even machines
built based on Skinner's work, by the mid-1960s these had largely
disappeared. PL had not died, however; it had undergone a metamorphosis
and become computerised: CAL.

### 1.2 Development of CAL

In 1958, Rath and Anderson, two IBM personnel, wrote the first CAL
program. Using an IBM 650 mainframe, the program taught binary
arithmetic and interacted with the student through the console
typewriter. Don Bitzer of the University of Illinois made the next
important contribution by using what is now called an authoring
language---allowing teachers to write material without being computer
experts.

Two major systems emerged from work sponsored by a US government grant:
PLATO (Programmed Logic for Automatic Teaching Operations) by Control
Data Corporation, and TICCIT (Timeshared Interactive Computer-Controlled
Information Television) by Mitre Corporation. PLATO used plasma screens
and slides superimposed on computer graphics; TICCIT used normal
television sets. IBM also developed CAL systems, and other manufacturers
included Bell and Howell, Texas Instruments, and Hewlett-Packard.
Specialist CAL companies were founded, such as Computer Curriculum
Corporation (CCC).

Originally these developments were on large machines and out of reach
for most schools. Since the late 1970s, with the widespread use of home
computers (Apple, Tandy, Commodore, etc.) and the launch of the IBM PC
in 1982, CAL became accessible. Many systems now exist, including
MICROTEXT, PASS, MCSD, MUMEDALA, and CYCLOPS.

### 1.3 Using a CAL

A CAL program presents its material to the student using a television
screen or a VDU (visual display unit). The usual method of communication
is via a keyboard. The student is presented with educational material in
a structured way and the computer allows them to respond individually,
actively, and at their own pace. Finally, the computer provides accurate
and immediate feedback.

### 1.4 Varieties of CAL

There are many kinds of educational programs loosely described as CAL:
drill and test programs, educational games, simulations, multi-choice
tests (with or without accompanying text), and others.

#### 1.4.1 Drill and test

These programs are suited for infants and juniors for group drills, but
are generally not recommended for individual use.

#### 1.4.2 Educational games

These are designed for young children and have similarities to arcade
games. They can be used as rewarding interludes between periods of
formal learning, or the whole educational program can be designed as a
game.

#### 1.4.3 Simulations

Simulations can be short examples within a normal CAL or complete
packages. They can be run in real time (e.g., learning to fly an
aircraft) or in fast time (e.g., economic simulations over years).

#### 1.4.5 Multi-choice tests

This project is an example of a CAL in this category. It presents text
after which the pupil is given questions with a choice of answers. Such
programs are useful for revision purposes.

### 1.5 CAL for Physics

Physics is suitable for CAL as questions usually have one correct
answer. On the Open University course "Discovering Physics", programs
written in UCSD Pascal with the OASIS authoring system are used on
microcomputers.

### 1.6 Pascal as a Medium for CAL

No single language is ideally suited for CAL. Factors include price,
availability, tools, utilisation rate, ease of learning, productivity,
and special features. BASIC is common but unsuitable here due to its
unstructured nature. C would be better but required Unix access and
experience. NATAL is specialised but difficult to obtain. Pascal was
chosen for this project due to its structured nature, availability, and
the author's familiarity.

## 2. Objectives

This project aims to produce a multi-choice CAL program with text as a
revision aid for classical mechanics. It is based on an O-level syllabus
but should be usable by those who have completed such a course. The
program consists of six chapters, each with a tutorial followed by
questions. Incorrect answers prompt reinforcement text. At the end of a
session, marks are provided per chapter and overall. The program was
tested during development and refined based on results.

## 3. Implementation Details

### 3.1 Hardware

MECHS was written on the PRIME 750 minicomputer. The terminal used was
the GT100.

### 3.2 Software Used

MECHS was written using the Sheffield screen editor (SED) under the
Primos operating system. The language was Sheffield Pascal.

### 3.3 Software Development

The software was developed using structured techniques.

#### 3.3.1 Functional Specification

The program should: - Present text via frames, - Test the student on the
text presented, - Receive and analyse student responses, - Provide
reinforcement material if required.

The subject matter is classical mechanics aimed at students with
ordinary level physics knowledge.

#### 3.3.2 Data Flow Diagrams

See Appendix.

#### 3.3.3 Structure Charts

See Appendix.

## 4. Testing

The best method for testing a CAL is to give it to potential users and
observe their use. Tests were carried out after the program was written
in skeletal form. Feedback from students (Dennis, Roger, and Ann)
resulted in improvements such as better introductions, page numbering,
screen layout, inclusion of titles, and clearer diagnostics.

## 5. Conclusion

As a revision aid, MECHS proved effective. Students who had not studied
physics for a long time found it useful as a refresher, while
non-physics students found it a useful introduction. Pascal proved to be
a good medium due to its structured nature. User feedback was essential
to meeting the design objectives. There is scope for future development,
such as porting to the IBM PC and adding more chapters.

## Recommendations

MECHS would be well suited to running on the IBM PC to allow graphics
and portability. Making chapter headings and menus subject-independent
would move MECHS toward a CAL revision shell, enabling it to serve as a
front end to a larger system.
