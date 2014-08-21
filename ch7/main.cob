      ***************************************************************
       IDENTIFICATION DIVISION.
      ***************************************************************
       PROGRAM-ID. Payroll_Assignment.
      * AUTHOR: Blake Bartenbach
      * FUNCTION: Assignment for COBOL I at MCC

      ***************************************************************
       ENVIRONMENT DIVISION.
      ***************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "/home/proxa/doc/cob/c0701"
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TERMINAL-OUT  ASSIGN TO DISPLAY
           ORGANIZATION IS LINE SEQUENTIAL.

      ***************************************************************
       DATA DIVISION.
      ***************************************************************
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01  EMPLOYEE-REC.
           05  NAME-IN     PIC X(15).
           05  HOURS-IN    PIC 9(3).
           05  RATE-IN     PIC 9V99.
       FD TERMINAL-OUT.
       01  PRINT-REC.
           05  NAME-OUT    PIC X(15).
           05  GPAY-OUT    PIC $Z,ZZZ.99.
           05  FICA-OUT    PIC $ZZZ.99.
           05  NPAY-OUT    PIC $Z,ZZZ.99.
       WORKING-STORAGE SECTION.
       01  EOF             PIC 9        VALUE ZERO.
       01  TAX-RATE        PIC V9999    VALUE ZERO.
       01  WS-GPAY         PIC 9999V99  VALUE ZERO.
       01  WS-FICA         PIC 999V99   VALUE ZERO.
       01  WS-NPAY         PIC 9999V99  VALUE ZERO.

      ***************************************************************
       PROCEDURE DIVISION.
      ***************************************************************
       100-MAIN.
           PERFORM 150-INIT
           PERFORM UNTIL EOF = 1
               READ EMPLOYEE-FILE
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END
                       PERFORM 200-CALC
               END-READ
           END-PERFORM.
           PERFORM 300-CLOSE
       STOP RUN. 


       150-INIT.
           MOVE .0765 TO TAX-RATE
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT TERMINAL-OUT.


       200-CALC.
           MOVE NAME-IN TO NAME-OUT
           COMPUTE WS-GPAY ROUNDED = HOURS-IN * RATE-IN
           MOVE WS-GPAY TO GPAY-OUT
           COMPUTE WS-FICA ROUNDED = WS-GPAY * TAX-RATE
           MOVE WS-FICA TO FICA-OUT
           COMPUTE WS-NPAY ROUNDED = WS-GPAY - WS-FICA
           MOVE WS-NPAY TO NPAY-OUT
           WRITE PRINT-REC.


       300-CLOSE.
           CLOSE EMPLOYEE-FILE
           CLOSE TERMINAL-OUT.
