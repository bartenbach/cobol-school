      ******************************************************************
       identification division.
       program-id. chapter-10-problem-4.
      *author.     blake bartenbach
      ******************************************************************

      ******************************************************************
       environment division.
      ******************************************************************
       input-output section.
       file-control.
       select input-file assign to "/home/proxa/doc/cob/c1004"
       organization is line sequential.
       select terminal-out assign to display
       organization is line sequential.

      ******************************************************************
       data division.
      ******************************************************************
       file section.
       fd input-file.
       01  student-rec.
           05  soc-sec-in             pic x(9).
           05  name-in                pic x(20).
           05  class-code-in          pic 9(1).
           05  school-code-in         pic 9(1).
           05  gpa-in                 pic 9v99.
           05  credits-earned-in      pic 9(2).
       fd terminal-out.
       01  print-rec.
           05 record-line             pic x(30).

       working-storage section.
       01  eof                        pic 9(1)  value zero.
       01  current-school             pic 9(1)  value zero.
       01  current-class              pic 9(1)  value zero.
       01  total-students             pic 9(2)  value zero.
       01  school-changed             pic 9(1)  value zero.
       01  total-gpa                  pic 99v99 value zero.
       01  school-hdr.
           05 filler                  pic x(8)  value "SCHOOL: ".
           05 school-name             pic x(12) value spaces.
       01  column-hdr.
           05 filler                  pic x(2)  value spaces.
           05 filler                  pic x(5)  value "CLASS".
           05 filler                  pic x(9)  value spaces.
           05 filler                  pic x(11) value "AVERAGE GPA".
       01  class-entry.
           05 class-name              pic x(19) value spaces.
           05 avg-gpa                 pic 9.99 value zero.

      ******************************************************************
       procedure division.
      ******************************************************************
       100-main.
         perform 150-init
         perform 300-read-file
         perform 250-close
         stop run.

       300-read-file.
         perform until eof = 1 
           read input-file
             at end 
               move 1 to eof
      *        when eof detected, write last parsed class..
               perform 475-write-class
             not at end
               perform 400-parse-rec
           end-read
         end-perform.
         
       400-parse-rec.
          if school-code-in equal current-school
            perform 450-parse-class
          else
      *     school has changed if non-zero.  write last class out!
            if current-school not zero
              perform 475-write-class
              move 1 to school-changed
            end-if
            move school-code-in to current-school
            perform 500-school-name
            perform 425-write-school-hdr
            perform 450-parse-class
          end-if.

       425-write-school-hdr.
          write print-rec from school-hdr after advancing 3 lines
          write print-rec from column-hdr after advancing 2 lines.

       450-parse-class.
      *  this logic can be probably be cleaned up a bit
         if class-code-in equal current-class
           add 1 to total-students
           add gpa-in to total-gpa
         else
           if current-class not zero and school-changed not equal 1
             perform 475-write-class
           end-if
           perform 465-init-class
           add 1 to total-students
           add gpa-in to total-gpa
         end-if.

       465-init-class.
         move class-code-in to current-class
         move 0 to school-changed
         move 0 to total-gpa
         move 0 to total-students
         perform 550-class-name.

       475-write-class.
         compute avg-gpa rounded = total-gpa / total-students
         move avg-gpa to avg-gpa
         write print-rec from class-entry after advancing 1 line.
         
        
       500-school-name.
         evaluate school-code-in
           when 1        move "BUSINESS"      to school-name
           when 2        move "LIBERAL ARTS"  to school-name
           when 3        move "ENGINEERING"   to school-name
           when other    move "UNKNOWN"       to school-name
         end-evaluate.

       550-class-name.
         evaluate current-class
           when 1        move "FRESHMAN"      to class-name
           when 2        move "SOPHOMORE"     to class-name
           when 3        move "JUNIOR"        to class-name
           when 4        move "SENIOR"        to class-name
           when other    move "UNKNOWN"       to class-name
         end-evaluate.

       150-init.
         open input input-file
         open output terminal-out.

       250-close.
         close input-file
         close terminal-out.
           
