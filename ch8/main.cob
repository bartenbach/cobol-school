      *****************************************************************
       IDENTIFICATION DIVISION.
      *****************************************************************
       PROGRAM-ID. Student-Data-Parser.
      * AUTHOR:    Blake Bartenbach
      * FUNCTION:  Chapter 8 Assignment for COBOL I at MCC


      *****************************************************************
       ENVIRONMENT DIVISION.
      *****************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT STUDENT-DATA-FILE ASSIGN TO"/home/proxa/doc/cob/c0805"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TERMINAL-OUTPUT   ASSIGN TO DISPLAY
           ORGANIZATION IS LINE SEQUENTIAL.


      *****************************************************************
       DATA DIVISION.
      *****************************************************************
       FILE SECTION.

       FD STUDENT-DATA-FILE.
       01 STUDENT-REC.
           05  SOCIAL-SECURITY-IN            PIC X(9).
           05  NAME-IN                       PIC X(21).
           05  CLASS-CODE-IN                 PIC X(1).
           05  SCHOOL-CODE-IN                PIC X(1).
           05  GPA-IN                        PIC 9V99.
           05  CREDITS-EARNED-IN             PIC 9(3).

       WORKING-STORAGE SECTION.
       01  EOF                               PIC 9(1)  VALUE ZERO.
       01  TOTAL-STUDENTS                    PIC 9(3)  VALUE ZERO.
       01  STUDENTS-GPA-LOW                  PIC 9(3)  VALUE ZERO.
       01  STUDENTS-GPA-MID                  PIC 9(3)  VALUE ZERO.
       01  STUDENTS-GPA-HIGH                 PIC 9(3)  VALUE ZERO.
       01  FRESHMEN                          PIC 9(3)  VALUE ZERO.
       01  SOPHOMORES                        PIC 9(3)  VALUE ZERO.
       01  JUNIORS                           PIC 9(3)  VALUE ZERO.
       01  SENIORS                           PIC 9(3)  VALUE ZERO.
       01  HIGH-GPA-BUSINESS                 PIC 9(3)  VALUE ZERO.
       01  HIGH-GPA-LIBERAL-ARTS             PIC 9(3)  VALUE ZERO.
       01  HIGH-GPA-ENGINEERING              PIC 9(3)  VALUE ZERO.
       01  HIGH-CREDIT-LOW-GPA               PIC 9(3)  VALUE ZERO.
       01  PERCENT-GPA-LOW                   PIC 99V99 VALUE ZERO.
       01  PERCENT-GPA-MID                   PIC 99V99 VALUE ZERO.
       01  PERCENT-GPA-HIGH                  PIC 99V99 VALUE ZERO.
       01  PERCENT-HIGH-BUSINESS             PIC 99V99 VALUE ZERO.
       01  PERCENT-HIGH-LIBERAL-ARTS         PIC 99V99 VALUE ZERO.
       01  PERCENT-HIGH-ENGINEERING          PIC 99V99 VALUE ZERO.
       01  PERCENT-HIGH-GPA-FRESHMEN         PIC 99V99 VALUE ZERO.
       01  PERCENT-HIGH-GPA-SOPHOMORES       PIC 99V99 VALUE ZERO.
       01  PERCENT-HIGH-GPA-JUNIORS          PIC 99V99 VALUE ZERO.
       01  PERCENT-HIGH-GPA-SENIORS          PIC 99V99 VALUE ZERO.
       01  PERCENT-GPA-STRING                PIC X(60) VALUE SPACE.
       01  PERCENT-MAJOR-STRING              PIC X(60) VALUE SPACE.
       01  HIGH-CREDIT-LOW-GPA-STRING        PIC X(60) VALUE SPACE.
       01  PERCENT-HIGH-GPA-CLASS-STRING     PIC X(60) VALUE SPACE.
       01  SEPARATOR                         PIC X(80) VALUE SPACE.
       01  COLOR-CODES.
           05  BLUE                          PIC 9(1)  VALUE 1.
           05  RED                           PIC 9(1)  VALUE 4.
           05  WHITE                         PIC 9(1)  VALUE 7.
       SCREEN SECTION.
       01  ERROR-WARNING.
           05  LINE 13 COLUMN 10
                   BEEP
                   FOREGROUND-COLOR WHITE
                       HIGHLIGHT
                   BACKGROUND-COLOR RED
                   VALUE "ERROR!".


      *****************************************************************
       PROCEDURE DIVISION.
      *****************************************************************
       100-MAIN.
           PERFORM 150-INIT
           PERFORM UNTIL EOF = 1
               READ STUDENT-DATA-FILE
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END
                       PERFORM 400-EVAL-GPA
               END-READ
           END-PERFORM
           PERFORM 500-GPA-PERCENT-CALC
           PERFORM 525-CLASS-CODE-CALC
           PERFORM 550-HIGH-GPA-MAJOR-CALC
           PERFORM 600-PRINT-OUTPUT
           PERFORM 200-CLOSE
       STOP RUN.

       150-INIT.
           OPEN INPUT STUDENT-DATA-FILE
           OPEN OUTPUT TERMINAL-OUTPUT
           SET PERCENT-GPA-STRING TO 
           "          PERCENTAGE OF STUDENTS WITH A GPA.."
           SET PERCENT-MAJOR-STRING TO 
           "  PERCENTAGE OF STUDENTS WITH A GPA OVER 3.0 MAJORING IN.."
           SET HIGH-CREDIT-LOW-GPA-STRING TO
           "     STUDENTS UNDER 2.00 GPA AND OVER 100 CREDITS.."
           SET PERCENT-HIGH-GPA-CLASS-STRING TO
           "     DISTRIBUTION OF STUDENTS WITH A GPA OVER 3.0.."
           SET SEPARATOR TO
           "**********************************************************".

       200-CLOSE.
           CLOSE STUDENT-DATA-FILE
           CLOSE TERMINAL-OUTPUT.

       400-EVAL-GPA.
           ADD 1 TO TOTAL-STUDENTS
           EVALUATE TRUE
               WHEN GPA-IN < 2.0 
                   ADD 1 TO STUDENTS-GPA-LOW
                   IF CREDITS-EARNED-IN > 100
                       ADD 1 TO HIGH-CREDIT-LOW-GPA
                   END-IF
               WHEN GPA-IN >= 2.0 AND GPA-IN <= 3.0
                   ADD 1 TO STUDENTS-GPA-MID
               WHEN GPA-IN > 3.0
                   ADD 1 TO STUDENTS-GPA-HIGH
                   EVALUATE SCHOOL-CODE-IN
                       WHEN 1        ADD 1 TO HIGH-GPA-BUSINESS
                       WHEN 2        ADD 1 TO HIGH-GPA-LIBERAL-ARTS
                       WHEN 3        ADD 1 TO HIGH-GPA-ENGINEERING
                       WHEN OTHER    PERFORM 999-ERROR
                   END-EVALUATE
                   EVALUATE CLASS-CODE-IN
                       WHEN 1        ADD 1 TO FRESHMEN 
                       WHEN 2        ADD 1 TO SOPHOMORES
                       WHEN 3        ADD 1 TO JUNIORS
                       WHEN 4        ADD 1 TO SENIORS
                       WHEN OTHER    PERFORM 999-ERROR
                   END-EVALUATE
               WHEN OTHER PERFORM 999-ERROR
           END-EVALUATE.

       500-GPA-PERCENT-CALC.
           COMPUTE PERCENT-GPA-LOW ROUNDED = 
                   STUDENTS-GPA-LOW / TOTAL-STUDENTS * 100
           COMPUTE PERCENT-GPA-MID ROUNDED =
                   STUDENTS-GPA-MID / TOTAL-STUDENTS * 100
           COMPUTE PERCENT-GPA-HIGH ROUNDED =
                   STUDENTS-GPA-HIGH / TOTAL-STUDENTS * 100.

       525-CLASS-CODE-CALC.
           COMPUTE PERCENT-HIGH-GPA-FRESHMEN ROUNDED =
                   FRESHMEN / STUDENTS-GPA-HIGH * 100
           COMPUTE PERCENT-HIGH-GPA-SOPHOMORES ROUNDED =
                   SOPHOMORES / STUDENTS-GPA-HIGH * 100
           COMPUTE PERCENT-HIGH-GPA-JUNIORS ROUNDED =
                   JUNIORS / STUDENTS-GPA-HIGH * 100
           COMPUTE PERCENT-HIGH-GPA-SENIORS ROUNDED =
                   SENIORS / STUDENTS-GPA-HIGH * 100.

       550-HIGH-GPA-MAJOR-CALC.
           COMPUTE PERCENT-HIGH-BUSINESS ROUNDED =
                   HIGH-GPA-BUSINESS / STUDENTS-GPA-HIGH * 100
           COMPUTE PERCENT-HIGH-LIBERAL-ARTS ROUNDED =
                   HIGH-GPA-LIBERAL-ARTS / STUDENTS-GPA-HIGH * 100
           COMPUTE PERCENT-HIGH-ENGINEERING ROUNDED =
                   HIGH-GPA-ENGINEERING / STUDENTS-GPA-HIGH * 100. 

       600-PRINT-OUTPUT.
           DISPLAY SEPARATOR 
           DISPLAY PERCENT-GPA-STRING
           DISPLAY " Less than 2.00                                   " 
           PERCENT-GPA-LOW "%"
           DISPLAY " Over 2.00 - under 3.00                           " 
           PERCENT-GPA-MID "%"
           DISPLAY " Over 3.00                                        " 
           PERCENT-GPA-HIGH "%"
           DISPLAY SEPARATOR
           DISPLAY PERCENT-MAJOR-STRING
           DISPLAY " Business                                         "
           PERCENT-HIGH-BUSINESS "%"
           DISPLAY " Liberal Arts                                     "
           PERCENT-HIGH-LIBERAL-ARTS "%"
           DISPLAY " Engineering                                      "
           PERCENT-HIGH-ENGINEERING  "%"
           DISPLAY SEPARATOR
           DISPLAY HIGH-CREDIT-LOW-GPA-STRING
           DISPLAY "                                                   "
           HIGH-CREDIT-LOW-GPA
           DISPLAY SEPARATOR
           DISPLAY PERCENT-HIGH-GPA-CLASS-STRING
           DISPLAY " Freshmen                                         "
           PERCENT-HIGH-GPA-FRESHMEN "%"
           DISPLAY " Sophomores                                       "
           PERCENT-HIGH-GPA-SOPHOMORES "%"
           DISPLAY " Juniors                                          "
           PERCENT-HIGH-GPA-JUNIORS "%"
           DISPLAY " Seniors                                          "
           PERCENT-HIGH-GPA-SENIORS "%"
           DISPLAY SEPARATOR.

       999-ERROR.
           DISPLAY ERROR-WARNING
           ACCEPT  ERROR-WARNING.
