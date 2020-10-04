      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT ASSIGN TO "student.txt"
           ORGANIZATION  SEQUENTIAL
           ACCESS SEQUENTIAL
           FILE STATUS IS FS.

       DATA DIVISION.
           FILE SECTION.
           FD STUDENT.
           01 STUDENT-FILE.
               05 STUDENT-ID PIC 9(5).
               05 NAME PIC A(25).

           WORKING-STORAGE SECTION.
           01 WS-STUDENT.
               02 TNUM      PIC 9(16).
               02 TPIN      PIC  9(4).

           77 FS   PIC X(2).
           01 WS-EOF   PIC A(1).

       PROCEDURE DIVISION.
           OPEN INPUT STUDENT.
           DISPLAY FS.
      **     GO TO SLEEP.
      *         MOVE 20005 TO STUDENT-ID.
      *         READ STUDENT.
      *         DISPLAY FS.
           READ STUDENT INTO WS-STUDENT
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END DISPLAY WS-STUDENT
               DISPLAY TNUM
           END-READ

      *    END-READ.
           DISPLAY "----------------------"
           DISPLAY TNUM
           CLOSE STUDENT.

       SLEEP.
           GO TO SLEEP.
       STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
