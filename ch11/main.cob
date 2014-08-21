      ******************************************************************
       identification division.
         program-id. chapter-eleven.
      *  author: blake bartenbach
      ******************************************************************

      ******************************************************************
       environment division.
       input-output section.
       file-control.
       select input-file assign to "/home/proxa/doc/cob/c1104"
       organization is line sequential.
       select terminal-out assign to display
       organization is line sequential.
      ******************************************************************

      ******************************************************************
       data division.
       file section.
       fd input-file.
       01  student-rec.
           05  soc-sec-in           pic x(9).
           05  name-in              pic x(21).
           05  class-code-in        pic x(1).
           05  school-code-in       pic x(1).
           05  gpa-in               pic 9v99.
           05  credits-in           pic 9(3).
       fd terminal-out.
       01  print-rec.
           05 rec-line              pic x(30).
       working-storage section.
       01  eof                      pic 9(1)  value zero.
       01  err-msg                  pic x(30) value space.
       01  counter                  pic 9(2)  value zero.
      ******************************************************************

      ******************************************************************
       procedure division.
       100-main.
         perform 100-init
         perform 150-read-file
         perform 999-close
         stop run.

       100-init.
         open input input-file
         open output terminal-out.

       150-read-file.
         perform until eof = 1
           read input-file
             at end
               move 1 to eof
             not at end
               add 1 to counter
               perform 200-parse-rec
               display " "
           end-read
         end-perform.

       200-parse-rec.
         if soc-sec-in not numeric
           move "Invalid SSN" to err-msg
           perform 300-error-rtn
         end-if
         if name-in = space
           move "Invalid name" to err-msg
           perform 300-error-rtn
         end-if
         if class-code-in not=1 and not=2 and not=3 and not=4
           move "Invalid class code" to err-msg
           perform 300-error-rtn
         end-if
         if school-code-in not=1 and not=2 and not=3
           move "Invalid school code" to err-msg
           perform 300-error-rtn
         end-if
         if gpa-in < 0.0 or > 4.0
           move "Invalid GPA" to err-msg
           perform 300-error-rtn
         end-if
         if credits-in = space or > 160 or credits-in not numeric
           move "Invalid number credits" to err-msg
           perform 300-error-rtn
         else
           if class-code-in = 1
             if credits-in < 0 or > 30
               move "Invalid number of credits" to err-msg
               perform 300-error-rtn
             end-if
           end-if
           if class-code-in = 2
             if credits-in < 31 or > 59
               move "Invalid number of creditsd" to err-msg
               perform 300-error-rtn
             end-if
           end-if
           if class-code-in = 3
             if credits-in < 60 or > 92
               move "Invalid number of credits" to err-msg
               perform 300-error-rtn
             end-if
           end-if
           if class-code-in = 4
             if credits-in < 93 or > 160
               move "Invalid number of credits" to err-msg
               perform 300-error-rtn
             end-if
           end-if
         end-if.
         
       300-error-rtn.
         display "Record: " counter "   " err-msg.

       999-close.
         close input-file
         close terminal-out.
      ******************************************************************
