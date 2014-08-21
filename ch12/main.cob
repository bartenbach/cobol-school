      ******************************************************************
       identification division.
       program-id. chapter-twelve.
      * author: blake bartenbach
      ******************************************************************

      ******************************************************************
       environment division.
       input-output section.
       file-control.
       select input-file assign to "/home/proxa/doc/cob/c1201"
       organization is line sequential.
       select terminal-out assign to display
       organization is line sequential.
      ******************************************************************

      ******************************************************************
       data division.
       file section.

       fd input-file.
       01  employee-rec.
           05  salesperson-no                 pic 9(2).
           05  salesperson-name               pic x(20).
           05  amt-of-sales                   pic 9(3)v99.

       fd terminal-out.
       01  big-line                           pic x(80).
       01  out-line.
           05  filler                         pic x(6).
           05  salesman-no-out                pic x(2).
           05  filler                         pic x(15).
           05  salesman-name-out              pic x(20).
           05  total-sales-out                pic $zzzz.99.

       working-storage section.
       01  eof                                 pic 9(1)     value zero.
       01  counter                             pic 9(2)     value zero.
       01  sub                                 pic 9(2)     value zero.
       01  total-company-sales                 pic 9(6)v99  value zero.
       01  sales-totals.
           05  total-sales   occurs 20 times   pic 9(4)v99  value zero.
           05  salesman-name occurs 20 times   pic x(20)    value space.
       01  hdr-main                            pic x(60)    value space.
       01  hdr-1.
           05  sls-no                          pic x(16)    value space.
           05  filler                          pic x(4)     value space.
           05  sls-name                        pic x(16)    value space.
           05  filler                          pic x(6)     value space.
           05  sls-total                       pic x(16)    value space.
       01  end-line.
           05  filler                          pic x(30)    value space.
           05  end-line-string                 pic x(20)    value space.
           05  end-line-total-co-sales         pic $zzz,zzz.99.
          
      ******************************************************************

      ******************************************************************
       procedure division.

       100-main.
         perform 111-init
         perform 150-read-file
         perform 999-close
         stop run.

       150-read-file.
         perform until eof = 1
           read input-file
             at end
               move 1 to eof
             not at end
               perform 250-parse-rec
           end-read
         end-perform
         perform 300-print-report.

       111-init.
         open input input-file
         open output terminal-out
         move zeros to sales-totals
         move "           TOTAL SALES FOR EACH SALESPERSON" to big-line
         move "SALESPERSON NO." to sls-no
         move "SALESPERSON NAME" to sls-name
         move "TOTAL SALES" to sls-total
         move "TOTAL COMPANY SALES" to end-line-string.

       250-parse-rec.
         move salesperson-name to salesman-name (salesperson-no)
         add amt-of-sales to total-sales (salesperson-no)
         add amt-of-sales to total-company-sales.
       
       300-print-report.
         write big-line
         move spaces to big-line
         write big-line
         write big-line from hdr-1
         move spaces to big-line
         write big-line
         perform varying sub from 1 by 1 until sub > 20
           if salesman-name (sub) not zeros
             move sub to salesman-no-out
             move salesman-name (sub) to salesman-name-out
             move total-sales (sub) to total-sales-out
             write out-line
           end-if
         end-perform
         move spaces to big-line
         write big-line
         move total-company-sales to end-line-total-co-sales
         write big-line from end-line.

       999-close.
         close input-file
         close terminal-out.
      ******************************************************************
