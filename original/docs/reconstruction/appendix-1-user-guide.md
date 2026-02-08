# Appendix I --- User Guide

## 1. Purpose of the Program MECHS

MECHS is a Computer Aided Learning program intended to be used as a
revision aid for an ordinary level physics course on aspects of
classical mechanics. The program presents text followed by questions in
a multiple-choice format. Correct answers allow progression to the next
question; incorrect answers prompt reinforcement text before continuing.

This pattern is repeated for all chapters attempted. At the end of a
session, the program provides a breakdown of marks for each section and
an overall score.

## 2. Description of MECHS' Facilities

### 2.1 Introduction

On request, a few introductory frames of text will be displayed
outlining the objectives and function of the program.

### 2.2 Menu

The menu provides a list of options at any stage:

1.  Force of Gravity, Weight & Friction\
2.  Speed, Velocity & Acceleration\
3.  Newton's Laws of Motion\
4.  Work, Energy & Power\
5.  Machines\
6.  Density and Relative Density\
7.  End this session

Each chapter follows the same pattern described in this guide.

If option 7 is chosen, the program produces diagnostics indicating
whether each chapter was attempted and the mark scored. Chapters not
attempted are indicated as such. Finally, an overall mark is produced
before MECHS ends.

## 3. Resources Required

MECHS runs on a PRIME 750 minicomputer and requires 8 pages of store. It
was written in Sheffield Pascal. The text files require a minimum of 15
pages of store. A typical session will need between twenty and forty
minutes of run time to complete. At the time of writing, the GT100
terminal is the only one that MECHS will run on.

## 4. How to Run MECHS

If you have an account on the PRIME system:

1.  Log in to your UFD (must be via a GT100 terminal).

2.  Type:

        SEG <TPB2>JS1CY4>PROJECT>MECHS.SEG

If you do not have an account:

1.  Find a GT100 terminal and switch it on.

2.  Press **SHIFT** and **BREAK** together and wait for
    `FACILITY REQUIRED ?` to appear.

3.  Type `CO1` and wait for `[CONNECTED]`.

4.  Type `LOGIN JS1CY4` and enter the password `COPPED` (it will not
    appear on screen).

5.  When the `OK` prompt appears, start the session by typing:

        SEG PROJECT>MECHS

6.  When finished, type `LOGOUT` to leave the UFD.

## 5. Example Run Session

Example run sessions could not be included, as there was no easy way to
capture what appears on the GT100 screen to paper. However, you can view
a run session without actually running MECHS:

1.  Log in to the UFD as described above.

2.  Instead of running MECHS, type:

        PRINT EXAMPLE

3.  Log out as before.
